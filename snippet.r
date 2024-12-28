for (i in seq_along(opj_files$datapath)) {
  file <- opj_files$datapath[i]
  original_filename <- tools::file_path_sans_ext(opj_files$name[i])  # Извлечение оригинального имени
  
  opj_data <- tryCatch({
    read.opj(file, encoding = "latin1", tree = FALSE)
  }, error = function(e) {
    skipped_files <<- rbind(skipped_files, data.frame(
      File = basename(file),
      Reason = paste("Error reading file:", e$message),
      stringsAsFactors = FALSE
    ))
    return(NULL)
  })
  
  if (is.null(opj_data)) next
  
  for (obj_name in names(opj_data)) {
    obj <- opj_data[[obj_name]]
    
    if (is.data.frame(obj)) {

      csv_name <- paste0(original_filename, "_", obj_name, ".csv")
      csv_path <- file.path(tempdir(), csv_name)
      write.csv(obj, file = csv_path, row.names = FALSE, fileEncoding = "UTF-8")
      
      if (!(x_col %in% names(obj)) || !(y_col %in% names(obj))) {
        skipped_files <<- rbind(skipped_files, data.frame(
          File = csv_name,
          Reason = paste("Missing required columns:", x_col, "and/or", y_col),
          stringsAsFactors = FALSE
        ))
        next
      }
      
      cleaned_obj <- tryCatch({
        remove_phantom_coords(obj, x_col, y_col, threshold_multiplier)
      }, error = function(e) {
        skipped_files <<- rbind(skipped_files, data.frame(
          File = csv_name,
          Reason = paste("Error cleaning data:", e$message),
          stringsAsFactors = FALSE
        ))
        return(NULL)
      })
      
      if (is.null(cleaned_obj)) next
      
      if (nrow(cleaned_obj) < 2) {
        skipped_files <<- rbind(skipped_files, data.frame(
          File = csv_name,
          Reason = "Insufficient data after cleaning.",
          stringsAsFactors = FALSE
        ))
        next
      }
      
      y_values <- cleaned_obj[[y_col]]
      analysis <- tryCatch({
        count_peaks(y_values, min_peak_height, min_peak_distance, threshold)
      }, error = function(e) {
        skipped_files <<- rbind(skipped_files, data.frame(
          File = csv_name,
          Reason = paste("Error analyzing peaks:", e$message),
          stringsAsFactors = FALSE
        ))
        return(NULL)
      })
      
      if (is.null(analysis)) next
      

      peak_file_name <- paste0(original_filename, "_", obj_name, "_peaks.csv")
      save_dir <- "peak_results"
      if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
      
      peak_file_path <- file.path(save_dir, peak_file_name)
      save_peak_data(cleaned_obj, analysis$peak_indices, peak_file_path, x_col, y_col)
      

      results <- rbind(results, data.frame(
        File = csv_name,
        Number_of_Peaks = analysis$num_peaks,
        stringsAsFactors = FALSE
      ))
      
      p <- ggplot(cleaned_obj, aes_string(x = x_col, y = y_col)) +
        geom_line(color = "blue") +
        geom_point(data = cleaned_obj[analysis$peak_indices, ], aes_string(x = x_col, y = y_col), color = "red", size = 2) +
        ggtitle(csv_name) +
        xlab(x_col) +
        ylab(y_col) +
        theme_minimal()
      
      plots[[csv_name]] <- p
    }
  }
}