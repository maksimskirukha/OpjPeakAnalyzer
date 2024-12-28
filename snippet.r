for (file in opj_files$datapath) {
  # Извлекаем оригинальное имя файла
  original_filename <- tools::file_path_sans_ext(opj_files$name[which(opj_files$datapath == file)])
  
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
      # Используем оригинальное имя файла для сохранения CSV
      csv_name <- paste0(original_filename, "_", obj_name, ".csv")
      peak_file_name <- paste0(original_filename, "_", obj_name, "_peaks.csv")
      
      # Сохраняем файлы
      csv_path <- file.path(tempdir(), csv_name)
      write.csv(obj, file = csv_path, row.names = FALSE, fileEncoding = "UTF-8")
      
      # Сохраняем данные пиков
      save_dir <- "peak_results"
      if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
      
      peak_file_path <- file.path(save_dir, peak_file_name)
      save_peak_data(cleaned_obj, analysis$peak_indices, peak_file_path, x_col, y_col)
    }
  }
}