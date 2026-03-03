#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # ============================================================================
  # 1. PYTHON ENVIRONMENT CHECK (MUST BE INSIDE SERVER)
  # ============================================================================

  # Check for Python Environment on startup
  shiny::observe({
    # We check if the environment exists in the list of available conda envs
    env_exists <- tryCatch({
      "icp_conda" %in% reticulate::conda_list()$name
    }, error = function(e) FALSE)

    if (!env_exists) {
      shiny::showModal(shiny::modalDialog(
        title = "Python Environment Required",
        "CloudFlux requires a specific Python environment (icp_conda) for GPU-accelerated alignment.
         Would you like to install it now? This may take several minutes and requires an internet connection.",
        footer = shiny::tagList(
          shiny::actionButton("install_py", "Install Environment", class = "btn-primary"),
          shiny::modalButton("Cancel")
        )
      ))
    }
  })

  # Handle the installation button click
  shiny::observeEvent(input$install_py, {
    shiny::removeModal()
    shiny::showModal(shiny::modalDialog("Installing... please check your R console for progress logs.", footer = NULL))

    tryCatch({
      # This function must be defined in your fct_shiny_helpers.R
      setup_cloudflux_python()
      shiny::removeModal()
      shiny::showNotification("Python setup successful! You can now use ICP Alignment.", type = "message")
    }, error = function(e) {
      shiny::removeModal()
      shiny::showNotification(paste("Installation failed:", e$message), type = "error")
    })
  })

  # ============================================================================
  # 2. REACTIVE VALUES & INITIALIZATION
  # ============================================================================

  rv <- reactiveValues(
    console_output = list(),
    in_dir = NULL,
    out_dir = NULL,
    metadata = NULL,
    sc1 = NULL,
    sc2 = NULL,
    footprints = NULL,
    crs = NULL,
    resolution = NULL,
    processing = NULL,
    union_mask = NULL,
    classified_DTM = NULL,
    classified_nDSM = NULL,
    results = NULL,
    current_legend = NULL,
    out_num = 0,
    dtm_in = "No",
    ndsm_in = "No"
  )

  # Render Intro Photo
  output$photo <- renderImage({
    list(
      src = app_sys("app/www/steps.png"),
      contentType = "image/png"
    )
  }, deleteFile = FALSE)

  # Create necessary workspace directories
  data_default <- paste0(getwd(), "/data/")
  save_drive <- paste0(getwd(), "/saves/")
  tmp_drive <- paste0(getwd(), "/tmp/")
  drive_list <- list(data_default, save_drive, tmp_drive)

  for (drive in drive_list) {
    create_directories(drive)
  }

  # Initialize Console
  rv$console_output <- list(messages = "Welcome to CloudFlux")

  output$console_output <- renderUI({
    total_messages <- length(rv$console_output)
    lapply(seq_along(rv$console_output), function(i) {
      message_number <- total_messages - i + 1
      div(
        style = "border: 1px solid #ccc; padding: 5px; margin: 5px; background-color: #f9f9f9;",
        HTML(paste0("<strong>Message ", message_number, ":</strong><br>", rv$console_output[[i]]))
      )
    })
  })

  # UI State Controller (Toggles buttons based on data availability)
  observe({
    shinyjs::toggleState("PC_confirm", !is.null(input$selected_source) && !is.null(input$selected_target))
    shinyjs::toggleState("run_icp", !is.null(rv$sc1) && !is.null(rv$sc2))
    shinyjs::toggleState("run_mask", !is.null(rv$sc1) && !is.null(rv$sc2))
    shinyjs::toggleState("run_denoise", !is.null(rv$sc1) && !is.null(rv$sc2))
    shinyjs::toggleState("dtm1", !is.null(rv$sc1))
    shinyjs::toggleState("dtm2", !is.null(rv$sc2))
    shinyjs::toggleState("ndsm1", !is.null(rv$sc1))
    shinyjs::toggleState("ndsm2", !is.null(rv$sc2))
  })

  # Base Leaflet Map
  output$leafletmap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = -79.3832, lat = 43.6532, zoom = 11)
  })

  # ============================================================================
  # 3. DATA INPUT & LOADING
  # ============================================================================

  observeEvent(input$upload_file, {
    req(input$upload_file, rv)
    save_path <- file.path(paste0(getwd(), "/data"), input$upload_file$name)
    file.copy(input$upload_file$datapath, save_path)
    add_message(paste0("File uploaded to: ", save_path), rv)
  })

  observeEvent(input$confirm, {
    rv$in_dir <- normalizePath(input$in_dir)
    rv$out_dir <- normalizePath(input$out_dir)
    rv$resolution <- as.integer(input$resolution)
    rv$crs <- as.integer(input$crs)

    req(rv$in_dir)
    mo_dir <- mo$new(rv$in_dir)
    rv$metadata <- mo_dir$metadata

    output$plotmeta <- renderTable({
      req(rv$metadata)
      rv$metadata
    })
  })

  observeEvent(input$confirm, {
    req(rv$metadata)
    laz_names <- rv$metadata %>%
      dplyr::filter(grepl("\\.laz$|\\.las$", file_path)) %>%
      dplyr::pull(file_name)

    updateSelectInput(session, "selected_source", choices = laz_names)
    updateSelectInput(session, "selected_target", choices = laz_names)

    shp_names <- rv$metadata %>%
      dplyr::filter(grepl("\\.shp$", file_path)) %>%
      dplyr::pull(file_name)
    shp_names <- c("No Footprints", shp_names)
    updateSelectInput(session, "selected_buildings", choices = shp_names)
  })

  observeEvent(input$PC_confirm, {
    showModal(modalDialog("Initializing LAS and LAS Index", footer = NULL))
    req(input$selected_source, input$selected_target, input$selected_buildings, rv$metadata)

    if (tools::file_ext(input$selected_source) == "rds") {
      # Handling RDS Load
      r_data <- rv$metadata %>%
        dplyr::filter(grepl("\\.rds", file_path)) %>%
        dplyr::select(file_path, file_name)

      source_path <- r_data %>% dplyr::filter(file_name == input$selected_source) %>% dplyr::pull(file_path)
      target_path <- r_data %>% dplyr::filter(file_name == input$selected_target) %>% dplyr::pull(file_path)

      showModal(modalDialog("Loading spatial container", footer = NULL))
      rv$sc1 <- load(normalizePath(source_path, winslash = "/"))
      rv$sc2 <- load(normalizePath(target_path, winslash = "/"))
      removeModal()
    } else {
      # Handling LAS/LAZ Load
      laz_data <- rv$metadata %>%
        dplyr::filter(grepl("\\.laz$|\\.las$", file_path)) %>%
        dplyr::select(file_path, file_name)

      source_path <- laz_data %>% dplyr::filter(file_name == input$selected_source) %>% dplyr::pull(file_path)
      target_path <- laz_data %>% dplyr::filter(file_name == input$selected_target) %>% dplyr::pull(file_path)

      if (input$selected_buildings == "No Footprints") {
        rv$footprints <- NULL
      } else {
        build_path <- rv$metadata %>% dplyr::filter(file_name == input$selected_buildings) %>% dplyr::pull(file_path)
        rv$footprints <- sf::st_read(normalizePath(build_path, winslash = "/"))
      }

      showModal(modalDialog("Initializing LAS and Index for PC 1", footer = NULL))
      if (!is.null(source_path)) {
        sc1 <- CFCore:::spatial_container$new(as.character(normalizePath(source_path, winslash = "/")))
        sc1$set_crs(rv$crs)
        rv$sc1 <- sc1
      }
      add_message(capture_output(print(rv$sc1$LPC)), rv)

      showModal(modalDialog("Initializing LAS and Index for PC 2", footer = NULL))
      if (!is.null(target_path)) {
        sc2 <- CFCore:::spatial_container$new(as.character(normalizePath(target_path, winslash = "/")))
        sc2$set_crs(rv$crs)
        rv$sc2 <- sc2
      }
      add_message(capture_output(print(rv$sc2$LPC)), rv)

      updateSelectInput(session, "io_obj", choices = setNames(c("sc1", "sc2"), c(input$selected_source, input$selected_target)))
      updateSelectInput(session, "selected_scene", choices = c(input$selected_source, input$selected_target))

      output$leafletmap <- leaflet::renderLeaflet({
        CFCore:::displayIndex(sc1$index)
      })
      removeModal()
    }
  })

  # ============================================================================
  # 4. PROCESSING TASKS
  # ============================================================================

  observeEvent(input$run_mask, {
    req(rv$sc1, rv$sc2)
    add_message("Generating a mask for Source and Target", rv)
    showModal(modalDialog("Creating mask for Source PC", footer = NULL))
    mask_source <- CFCore:::mask_pc(rv$sc1$LPC)
    rv$sc1$mask <- sf::st_transform(mask_source, crs = sf::st_crs(rv$sc1$LPC))

    showModal(modalDialog("Creating mask for Target PC", footer = NULL))
    mask_target <- CFCore:::mask_pc(rv$sc2$LPC)
    rv$sc2$mask <- sf::st_transform(mask_target, crs = sf::st_crs(rv$sc2$LPC))
    removeModal()
  })

  observeEvent(input$run_denoise, {
    req(rv$sc1, rv$sc2)
    showModal(modalDialog("Running Denoising...", footer = NULL))
    tryCatch({
      if (!is.null(rv$footprints)) {
        rv$sc1$LPC <- CFCore:::noise_filter_buildings(rv$sc1$LPC, rv$sc1$mask, rv$footprints)
        rv$sc2$LPC <- CFCore:::noise_filter_buildings(rv$sc2$LPC, rv$sc2$mask, rv$footprints)
      } else {
        rv$sc1$LPC <- CFCore:::noise_filter(rv$sc1$LPC)
        rv$sc2$LPC <- CFCore:::noise_filter(rv$sc2$LPC)
      }
      add_message("Denoising Complete", rv)
    }, error = function(e) add_message(paste0("Denoise error: ", e$message), rv))
    removeModal()
  })

  observeEvent(input$run_icp, {
    req(rv$sc1, rv$sc2)
    showModal(modalDialog("Running ICP Alignment...", footer = NULL))

    source_path <- file.path(getwd(), "tmp", "source.laz")
    target_path <- file.path(getwd(), "tmp", "target.laz")
    lidR::writeLAS(rv$sc1$LPC, source_path)
    lidR::writeLAS(rv$sc2$LPC, target_path)

    icp_module <- app_sys("app/python/icp_hybrid.py")

    tryCatch({
      reticulate::use_condaenv("icp_conda", required = TRUE)
      reticulate::source_python(icp_module)
      icp_aligner <- HybridICP(source_path, target_path, voxel_size = 0.05, icp_method = "point-to-plane")
      icp_result <- icp_aligner$align()

      if (!is.null(icp_result[[1]])) {
        rv$sc2$LPC <- lidR::readLAS(icp_result[[1]])
        sf::st_crs(rv$sc2$LPC) <- sf::st_crs(rv$sc1$LPC)
        add_message(icp_result[[2]], rv)
      }
    }, error = function(e) add_message(paste0("ICP error: ", e$message), rv))
    removeModal()
  })

  # DTM / nDSM Generation
  observeEvent(input$dtm1, {
    showModal(modalDialog("Generating Source DTM...", footer = NULL))
    rv$sc1$to_dtm(rv$resolution)
    add_message("Source DTM Generated", rv)
    removeModal()
  })

  observeEvent(input$dtm2, {
    showModal(modalDialog("Generating Target DTM...", footer = NULL))
    rv$sc2$to_dtm(rv$resolution)
    add_message("Target DTM Generated", rv)
    removeModal()
  })

  observeEvent(input$ndsm1, {
    showModal(modalDialog("Generating Source nDSM...", footer = NULL))
    rv$sc1$to_ndsm(rv$resolution)
    add_message("Source nDSM Generated", rv)
    removeModal()
  })

  observeEvent(input$ndsm2, {
    showModal(modalDialog("Generating Target nDSM...", footer = NULL))
    rv$sc2$to_ndsm(rv$resolution)
    add_message("Target nDSM Generated", rv)
    removeModal()
  })

  # Alignment & Classification
  observeEvent(input$align_rasters, {
    req(rv$processing)
    showModal(modalDialog("Aligning Rasters...", footer = NULL))

    src <- if(rv$processing == "nDSM") rv$sc1$ndsm_raw else rv$sc1$DTM_raw
    tgt <- if(rv$processing == "nDSM") rv$sc2$ndsm_raw else rv$sc2$DTM_raw

    aligned <- CFCore:::process_raster(src, tgt, rv$sc1$mask, rv$sc2$mask)
    rv$source_raster <- aligned[[1]]
    rv$target_raster <- aligned[[2]]
    rv$union_mask <- aligned[[3]]

    add_message("Raster Alignment Complete", rv)
    removeModal()
  })

  observeEvent(input$classify_raster, {
    req(rv$source_raster, rv$target_raster)
    if(rv$processing == "nDSM") {
      rv$classified_nDSM <- terra::mask(diff_classify_ndsm(rv$source_raster, rv$target_raster), rv$union_mask)
    } else {
      rv$classified_DTM <- terra::mask(diff_classify_dtm(rv$source_raster, rv$target_raster), rv$union_mask)
    }
    add_message("Classification Complete", rv)
  })

  # ============================================================================
  # 5. VISUALIZATION & OUTPUT
  # ============================================================================

  observeEvent(input$plot_leaf, {
    output$leafletmap <- leaflet::renderLeaflet({
      CFCore:::displayMap(rv$sc1$DTM, rv$sc1$ndsm, rv$sc2$DTM, rv$sc2$ndsm,
                          dtm_diff = rv$classified_DTM, ndsm_diff = rv$classified_nDSM, rv$union_mask)
    })
  })

  observeEvent(input$which_plot_2d, {
    output$plot2D <- renderPlot({
      if(input$which_plot_2d == "DTM") plot_dtm_stats(rv$classified_DTM) else plot_ndsm_stats(rv$classified_nDSM)
    })
  })

  # Handle Legend Syncing
  observeEvent(input$leafletmap_groups, {
    leaflet::leafletProxy("leafletmap") %>% leaflet::clearControls()
    # Logic to add appropriate legend based on visible group...
  })

  # Save & Screenshot Buttons
  observeEvent(input$screenshot_btn, {
    shinyscreenshot::screenshot(id = "leafletmap", scale = 2)
  })

  # Cleanup on End
  session$onSessionEnded(function() {
    delete_all_files(file.path(getwd(), "tmp"))
  })
}
