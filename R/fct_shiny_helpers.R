# ==============================================================================
# UI & DIRECTORY HELPERS
# ==============================================================================

create_directories <- function(in_dir) {
  if (!dir.exists(in_dir)) {
    dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
    message(paste("Created directory:", in_dir))
  } else {
    message(paste("Directory already exists:", in_dir))
  }
}

delete_all_files <- function(dir) {
  if (dir.exists(dir)) {
    files <- list.files(dir, full.names = TRUE)
    lapply(files, function(file) {
      if (file.exists(file)) {
        file.remove(file)
      }
    })
    print(paste("Deleted all files in directory:", dir))
  } else {
    print(paste("Directory does not exist:", dir))
  }
}

add_message <- function(message, rv, session = session) {
  if (!is.character(message)) message <- as.character(message)
  if (length(message) > 1) message <- paste(message, collapse = "<br>")

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_message <- paste0(timestamp, ": ", message)

  shiny::isolate({
    rv$console_output <- c(full_message, rv$console_output)
  })
  flush.console()
}

capture_output <- function(expr) {
  temp <- tempfile()
  sink(temp)
  on.exit(sink())
  on.exit(unlink(temp), add = TRUE)
  eval(expr)
  readLines(temp)
}

extract_info <- function(file_path) {
  path_normal <- normalizePath(file_path, mustWork = FALSE)
  if (!file.exists(path_normal)) stop("Path does not exist: ", path_normal)

  exts <- "\\.(laz|las|xyz|csv|shp|tif|tiff|json|rds)$"
  data_list <- list.files(path_normal, pattern = exts, full.names = TRUE)

  meta_df <- data.frame(
    id = numeric(), file_path = character(), file_name = character(),
    size_mb = numeric(), ext = character(), creation_date = as.POSIXct(character())
  )

  for (i in seq_along(data_list)) {
    current_file_path <- normalizePath(data_list[i], mustWork = FALSE)
    base_name <- basename(current_file_path)
    ext <- tools::file_ext(current_file_path)
    size <- format(round(file.info(current_file_path)$size / (1024^2), 2), nsmall = 2)
    date <- file.info(current_file_path)$mtime
    formatted_date <- format(date, "%Y-%m-%d")

    meta_df <- rbind(meta_df, data.frame(
      id = i, file_path = current_file_path, file_name = base_name,
      size_mb = size, ext = ext, creation_date = formatted_date, stringsAsFactors = FALSE
    ))
  }

  meta_df <- meta_df[order(meta_df$ext), ]
  meta_df$id <- seq_len(nrow(meta_df))
  return(meta_df)
}

# ==============================================================================
# RASTER CLASSIFICATION
# ==============================================================================

diff_classify_ndsm <- function(earlier, later) {
  diff <- later - earlier
  m <- c(-Inf, -10, 1, -10, -0.5, 2, -0.5, 0.5, 3, 0.5, 10, 4, 10, Inf, 5)
  rclmat <- matrix(m, ncol = 3, byrow = TRUE)
  diff_class <- terra::classify(diff, rclmat, include.lowest = TRUE)
  return(diff_class)
}

diff_classify_dtm <- function(earlier, later) {
  diff <- later - earlier
  m <- c(-Inf, -10, 1, -10, -0.5, 2, -0.5, 0.5, 3, 0.5, 10, 4, 10, Inf, 5)
  rclmat <- matrix(m, ncol = 3, byrow = TRUE)
  diff_class <- terra::classify(diff, rclmat, include.lowest = TRUE)
  return(diff_class)
}

# ==============================================================================
# GGPLOT2 CHARTING
# ==============================================================================

plot_dtm_stats <- function(difference_raster) {
  raster_values <- terra::values(difference_raster)
  raster_values <- raster_values[!is.na(raster_values)]

  class_counts <- table(raster_values)
  total_cells <- sum(class_counts)
  class_percentages <- (class_counts / total_cells) * 100

  class_labels <- c("Large Decrease \n(< -10m)", "Decrease \n(-0.5m to -10m)",
                    "Minimal Change \n(-0.5m to 0.5m)", "Increase \n(0.5m to 10m)", "Large Increase \n(> 10m)")

  plot_data <- data.frame(
    class = factor(names(class_counts), levels = c("1", "2", "3", "4", "5")),
    count = as.numeric(class_counts),
    percentage = as.numeric(class_percentages)
  )
  plot_data$class <- factor(plot_data$class, levels = c("1", "2", "3", "4", "5"), labels = class_labels)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = class, y = count, fill = class)) +
    ggplot2::geom_bar(stat = "identity", color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 4) +
    ggplot2::labs(x = "Loss and Gain", y = "Area (m^2)", fill = "Class") +
    ggplot2::ggtitle("Raster Statistics for Change Detection") +
    ggplot2::scale_fill_manual(
      values = c("#5e3c99", "#b2abd2", "#f7f7f7",  "#fdb863", "#e66101"),
      labels = c("Large Decrease (< -10m)", "Decrease (-0.5m to -10m)", "Minimal Change (-0.5m to 0.5m)", "Increase (0.5m to 10m)", "Large Increase (> 10m)"),
      drop = FALSE
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 16, face = "bold"),
      axis.text = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_text(size = 10, angle = 0, hjust = 0.5),
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.size = ggplot2::unit(0.6, "cm"),
      plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "right"
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma)
}

plot_ndsm_stats <- function(difference_raster) {
  raster_values <- terra::values(difference_raster)
  raster_values <- raster_values[!is.na(raster_values)]

  class_counts <- table(raster_values)
  total_cells <- sum(class_counts)
  class_percentages <- (class_counts / total_cells) * 100

  class_labels <- c("Large Decrease \n(< -10m)", "Decrease \n(-0.5m to -10m)",
                    "Minimal Change \n(-0.5m to 0.5m)", "Increase \n(0.5m to 10m)", "Large Increase \n(> 10m)")

  plot_data <- data.frame(
    class = factor(names(class_counts), levels = c("1", "2", "3", "4", "5")),
    count = as.numeric(class_counts),
    percentage = as.numeric(class_percentages)
  )
  plot_data$class <- factor(plot_data$class, levels = c("1", "2", "3", "4", "5"), labels = class_labels)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = class, y = count, fill = class)) +
    ggplot2::geom_bar(stat = "identity", color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 4) +
    ggplot2::labs(x = "Decrease and Increase", y = "Area (m^2)", fill = "Class") +
    ggplot2::ggtitle("Raster Statistics for Change Detection") +
    ggplot2::scale_fill_manual(
      values = c("#555599", "#b2abd2", "#f7f7f7", "#9AE696", "#448F3F"),
      labels = c("Large Decrease (< -10m)", "Decrease (-0.5m to -10m)", "Minimal Change (-0.5m to 0.5m)", "Increase (0.5m to 10m)", "Large Increase (> 10m)"),
      drop = FALSE
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 16, face = "bold"),
      axis.text = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_text(size = 10, angle = 0, hjust = 0.5),
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.size = ggplot2::unit(0.6, "cm"),
      plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "right"
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma)
}

#' Initial Setup for CloudFlux Python Environment
#'
#' This function creates the necessary Miniconda environment and installs
#' the hybrid GPU/CPU dependencies.
#'
#' @export
setup_cloudflux_python <- function() {
  # 1. Create the base environment
  message("Creating conda environment 'icp_conda'...")
  reticulate::conda_create(
    envname = "icp_conda",
    packages = c("python=3.9", "numpy", "open3d")
  )

  # 2. Install pip-specific packages
  # Cupoch, laspy, and pyproj are best handled via pip to ensure
  # CUDA compatibility and latest las format support.
  message("Installing pip dependencies (cupoch, laspy, pyproj)...")
  reticulate::conda_install(
    envname = "icp_conda",
    packages = c("cupoch", "laspy", "pyproj"),
    pip = TRUE
  )

  message("Setup Complete! You can now run the CloudFlux ICP alignment.")
}
