#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    shiny::navbarPage(
      title = "CloudFlux (CF)",

      # First tab: Introduction
      shiny::tabPanel(
        title = "Introduction",
        shiny::div(class = "intro-page",
                   shiny::fluidPage(
                     shiny::h2("Welcome to CloudFlux (CF)"),
                     shiny::tags$div(
                       style = "text-align: justify;",
                       "CF is designed to visualize, process, and analyze point cloud data. It can ingest LAS or LAZ formats. CF will create Digital Terrain Models (DTMs) and normalized Digital Surface Models (nDSM) and align point clouds for change detection. User data uploaded to CF is not saved and does not persist in the application. CF is free for use and it was built on the efforts of the open-source and open-access communities.", shiny::tags$br(),
                       shiny::tags$br(),
                       shiny::tags$b("Disclaimer:"), "CF is an educational tool and is not intended to replace professional advice or certified data processing workflows.The outputs are provided 'as is,' with no guarantee of accuracy, completeness, or suitability for any specific purpose. The developers of CF are not liable for any errors, inaccuracies, or decisions made based on its use. Use at your own risk.", shiny::tags$br(),
                       shiny::tags$br(),
                       "Users can uploaded their own point clouds, or they can use the data for Sunnybrook Campus (SB_19.laz and SB_23.laz) which comes preloaded. Below are the steps to use the tool:", shiny::tags$br(),
                       shiny::tags$br()
                     ),
                     shiny::tags$ul(
                       shiny::tags$li("Step 1: Upload the source and target point clouds. Confirm Inputs"),
                       shiny::tags$li("Step 2: Select your source and target point clouds. Confirm Point Clouds"),
                       shiny::tags$li("Step 3: Create Mask for Point Clouds"),
                       shiny::tags$li("Step 4: Denoise point clouds."),
                       shiny::tags$li("Step 5: ICP Alignment."),
                       shiny::tags$li("Step 6: Generate DTMs for source and target."),
                       shiny::tags$li("Step 7: Generate nDSMs for source and target"),
                       shiny::tags$li("Step 8: Select DTM or nDSM for change detection"),
                       shiny::tags$li("Step 9: Align rasters."),
                       shiny::tags$li("Step 10: Classify"),
                       shiny::tags$li("Step 11: Visualize the outputs in maps, 2D statistics, and 3D plots.")
                     ),
                     shiny::div(class = "responsive-img", shiny::imageOutput("photo"))
                   )
        )
      ),

      # Second tab: CloudFlux UI
      shiny::tabPanel(
        title = "CloudFlux",
        shiny::div(class = "sidebar-content",
                   shiny::div(class = "input-container",
                              shiny::h4("Data Input"),
                              shiny::fileInput("upload_file", "Upload Point Cloud (.laz or .las)", accept = c(".laz", ".las"), width = "100%"),
                              shiny::textInput("in_dir", "Input directory:", value = paste0(getwd(), "/data/"), width = "100%"),
                              shiny::textInput("out_dir", "Output directory:", value = paste0(getwd(), "/saves/"), width = "100%"),
                              shiny::numericInput("resolution", "Resolution:", value = 1, width = "100%"),
                              shiny::numericInput("crs", "CRS:", value = 26917, width = "100%"),

                              shiny::div(class = "button-confirm",
                                         shiny::actionButton("confirm", "Confirm Inputs")
                              ),
                              shinyBS::bsTooltip("confirm", "Ensure you are selecting the appropriate CRS and resolution.")
                   ),
                   shiny::h4("Data Processing"),
                   shiny::div(class = "button-container",
                              shiny::selectInput("selected_source", "Select Source", choices = NULL),
                              shiny::selectInput("selected_target", "Select Target",  choices = NULL),
                              shiny::selectInput("selected_buildings", "Select Footprints (.shp)", choices = NULL)
                   ),
                   shiny::div(class = "button-confirm",
                              shiny::actionButton("PC_confirm", "Confirm Point Clouds")
                   ),
                   shinyBS::bsTooltip("PC_confirm", "Confirm selection of source and target point clouds."),

                   shiny::h4("Pre-Processing"),
                   shiny::div(class = "button-container",
                              shiny::actionButton("run_icp", "ICP Alignment"),
                              shinyBS::bsTooltip("run_icp", "Align your source point cloud to your target point cloud"),
                              shiny::actionButton("run_mask", "Generate Mask"),
                              shinyBS::bsTooltip("run_mask", "Create a mask to outline the boundary of the point cloud"),
                              shiny::actionButton("run_denoise", "Remove Noise"),
                              shinyBS::bsTooltip("run_denoise", "Remove outlier points and building footprints")
                   ),

                   shiny::h4("Raster Generation"),
                   shiny::div(class = "button-container",
                              shiny::actionButton("dtm1", "DTM (Source)"),
                              shiny::actionButton("dtm2", "DTM (Target)"),
                              shiny::actionButton("ndsm1", "nDSM (Source)"),
                              shiny::actionButton("ndsm2", "nDSM (Target)")
                   ),

                   shiny::h4("Post Processing"),
                   shiny::selectInput("selected_processing", "Raster Type to Align", choices = c("", "DTM", "nDSM"), width = "100%"),
                   shiny::div(class = "button-container",
                              shiny::actionButton("align_rasters", "Align Rasters", class = "btn"),
                              shinyBS::bsTooltip("align_rasters", "Alignment is required to compare the rasters"),
                              shiny::actionButton("classify_raster", "Classify Rasters", class = "btn"),
                              shinyBS::bsTooltip("classify_raster", "Classify rasters to identify changes in the landscape"),
                              shiny::tags$hr()
                   ),

                   shiny::h4("Plotting"),
                   shiny::div(class = "button-container",
                              shiny::actionButton("plot_leaf", "Plot to Leaflet", class = "btn")
                   ),

                   shiny::h4("Data Saving"),
                   shiny::selectInput("io_obj", "Select PC to Save", choices = NULL, width = "100%"),
                   shiny::div(class = "button-container",
                              shiny::actionButton("screenshot_btn", "Take Screenshot", class = "btn"),
                              shiny::actionButton("save_las", "Save LAS", class = "btn"),
                              shiny::actionButton("save_dtm", "Save DTM", class = "btn"),
                              shiny::actionButton("save_ndsm", "Save nDSM", class = "btn"),
                              shiny::actionButton("save_classified_dtm", "Save Classified DTM", class = "btn"),
                              shiny::actionButton("save_classified_ndsm", "Save Classified nDSM", class = "btn"),
                              shiny::actionButton("save_mask", "Save Mask", class = "btn"),
                              shiny::downloadButton("downloadData", "Save All", class = "btn")
                   )
        ),

        shiny::div(class = "main-panel",
                   shiny::div(class = "main-content",
                              shiny::tabsetPanel(
                                shiny::tabPanel("Leaflet Map", leaflet::leafletOutput("leafletmap", width = "70%", height = "63vh")),
                                shiny::tabPanel("3D Plot",
                                                shiny::div(
                                                  style = "position: relative; width: 100%; height: 63vh;",
                                                  shiny::div(
                                                    style = "
                    position: absolute; top: 2vh; left: 2vw;
                    background-color: rgba(255,255,255,0.95);
                    padding: 0.5em 1em; border-radius: 0.5em; z-index: 10;",
                                                    shiny::selectInput("selected_scene", label = NULL, choices = NULL)
                                                  ),
                                                  rgl::rglwidgetOutput("plot3D", width = "70%", height = "63vh")
                                                )
                                ),
                                shiny::tabPanel("2D Plot",
                                                shiny::div(
                                                  style = "position: relative; width: 100%; height: 60vh;",
                                                  shiny::div(
                                                    style = "
                    position: absolute; top: 2vh; left: 2vw;
                    background-color: rgba(255,255,255,0.95);
                    padding: 0.5em 1em; border-radius: 0.5em; z-index: 10;",
                                                    shiny::selectInput("which_plot_2d", NULL, choices = c("DTM", "nDSM"), selected = "nDSM")
                                                  ),
                                                  shiny::plotOutput("plot2D", width = "70%", height = "60vh")
                                                )
                                ),
                                shiny::tabPanel("Directory Data", shiny::tableOutput("plotmeta"))
                              )
                   ),
                   shiny::div(class = "console-container",
                              shiny::uiOutput("console_output")
                   )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CFCore"
    ),
    shinyjs::useShinyjs(), # Enable JavaScript features
    tags$style(HTML("
        /* ===================== GLOBAL LAYOUT ===================== */
        html, body { height: 100vh; margin: 0; overflow: hidden; }
        /* ===================== INTRODUCTION PAGE ===================== */
        .intro-page { padding: 20px; height: 100vh; overflow-y: auto; }
        /* ===================== SIDEBAR ===================== */
        .sidebar-content {
            padding: 0.5vw; padding-top: 2vh; height: 100vh; width: 30%;
            position: fixed; top: 0; left: 0; overflow-y: auto; background-color: #f9f9f9;
        }
        /* ===================== INPUT CONTAINER ===================== */
        .input-container { display: flex; flex-direction: column; align-items: flex-start; padding-top: 3vh; }
        .input-container .shiny-input-container, .input-container .btn { width: 100%; }
        /* ===================== BUTTON CONTAINER ===================== */
        .button-container { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 0.5vw; }
        .button-container .btn { width: 100%; }
        .button-confirm { display: flex; justify-content: left; width: 100%; margin-top: 0.5vh; }
        /* ===================== MAIN PANEL ===================== */
        .main-panel { margin-left: 30%; width: 100%; height: 100vh; display: flex; flex-direction: column; }
        .main-content { flex-grow: 1; height: 100%; width: 100%; }
        /* ===================== CONSOLE OUTPUT ===================== */
        .console-container {
            position: fixed; bottom: 0; left: 30%; width: 100%; height: 25vh;
            background: #f8f9fa; border-top: 2px solid #ccc; box-sizing: border-box; overflow-y: auto; padding-top: 1vh;
        }
        #console_output { height: 100%; width: 100%; background: #f8f9fa; border: 1px solid #ccc; }
    "))
  )
}
