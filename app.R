library(shiny)
library(Ropj)
library(dplyr)
library(pracma)
library(DT)
library(ggplot2)


count_peaks <- function(y_values, min_peak_height = NULL, min_peak_distance = 5, threshold = 0) {
  peaks <- findpeaks(
    y_values, 
    minpeakheight = min_peak_height, 
    minpeakdistance = min_peak_distance, 
    threshold = threshold
  )
  
  if (!is.null(peaks)) {
    num_peaks <- nrow(peaks)
    peak_indices <- peaks[, 2]
  } else {
    num_peaks <- 0
    peak_indices <- integer(0)
  }
  
  return(list(num_peaks = num_peaks, peak_indices = peak_indices))
}

remove_phantom_coords <- function(df, x_col, y_col, threshold_multiplier = 3) {
  df[[x_col]] <- suppressWarnings(as.numeric(df[[x_col]]))
  df[[y_col]] <- suppressWarnings(as.numeric(df[[y_col]]))
  
  df_clean <- df %>% filter(!is.na(.data[[x_col]]) & !is.na(.data[[y_col]]))
  
  median_x <- median(df_clean[[x_col]], na.rm = TRUE)
  mad_x <- mad(df_clean[[x_col]], constant = 1, na.rm = TRUE)
  
  threshold <- threshold_multiplier * mad_x
  
  df_clean <- df_clean %>%
    filter(abs(.data[[x_col]] - median_x) <= threshold)
  
  return(df_clean)
}

process_files <- function(opj_files, x_col, y_col, min_peak_height, min_peak_distance, threshold, threshold_multiplier = 3) {
  results <- data.frame(
    File = character(),
    Number_of_Peaks = numeric(),
    stringsAsFactors = FALSE
  )
  
  plots <- list()
  
  skipped_files <- data.frame(
    File = character(),
    Reason = character(),
    stringsAsFactors = FALSE
  )
  
  for (file in opj_files$datapath) {
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
        csv_name <- paste0(tools::file_path_sans_ext(basename(file)), "_", obj_name, ".csv")
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
        
        p <- ggplot(cleaned_obj, aes_string(x = x_col, y = y_col)) +
          geom_line(color = "blue") +
          geom_point(data = cleaned_obj[analysis$peak_indices, ], aes_string(x = x_col, y = y_col), color = "red", size = 2) +
          ggtitle(csv_name) +
          xlab(x_col) +
          ylab(y_col) +
          theme_minimal()
        
        plots[[csv_name]] <- p
        
        # save files with peaks
        
        peak_file_name <- paste0(tools::file_path_sans_ext(basename(file)), "_", obj_name, "_peaks.csv")
        save_dir <- "peak_results"
        if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
        
        peak_file_path <- file.path("peak_results", peak_file_name)
        save_peak_data(cleaned_obj, analysis$peak_indices, peak_file_path, x_col, y_col)
        
        results <- rbind(results, data.frame(
          File = csv_name,
          Number_of_Peaks = analysis$num_peaks,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(list(results = results, plots = plots, skipped_files = skipped_files))
}

save_peak_data <- function(cleaned_obj, peak_indices, file_name, x_col, y_col) {
  if (length(peak_indices) == 0) return(NULL)
  
  
  
  peak_data <- cleaned_obj[peak_indices, c(x_col, y_col)]
  print(peak_data)
  write.csv(peak_data, file = file_name, row.names = FALSE, fileEncoding = "UTF-8")
}

ui <- fluidPage(
  titlePanel("Peak Analysis on Cell Graphs"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file_opj", "Select OPJ Files", multiple = TRUE, accept = ".opj"),
      uiOutput("column_select_ui"),
      numericInput("min_peak_height", "Minimum Peak Height", value = 1, min = 0),
      numericInput("min_peak_distance", "Minimum Peak Distance (points)", value = 5, min = 1),
      numericInput("threshold", "Threshold (relative to neighboring points)", value = 0, min = 0),
      actionButton("analyze", "Convert and Analyze"),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Results", DTOutput("results_table")),
        tabPanel("Graphs", 
                 uiOutput("graphs_ui")
        )
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  output$column_select_ui <- renderUI({
    req(input$file_opj)
    
    sample_file <- input$file_opj$datapath[1]
    opj_data <- tryCatch({
      read.opj(sample_file, encoding = "latin1", tree = FALSE)
    }, error = function(e) {
      showNotification(paste("Error reading file:", basename(sample_file), "-", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(opj_data)) return(NULL)
    
    first_obj <- opj_data[[1]]
    if (!is.data.frame(first_obj)) return(NULL)
    
    cols <- names(first_obj)
    
    tagList(
      selectInput("x_col", "Select X Column", choices = cols, selected = cols[1]),
      selectInput("y_col", "Select Y Column", choices = cols, selected = cols[2]),
      helpText("Phantom coordinates will be automatically removed before analysis.")
    )
  })
  
  rv <- reactiveValues(plots = list())
  
  observeEvent(input$analyze, {
    req(input$file_opj)
    req(input$x_col)
    req(input$y_col)
    
    withProgress(message = "Processing files...", value = 0, {
      processed <- process_files(
        opj_files = input$file_opj,
        x_col = input$x_col,
        y_col = input$y_col,
        min_peak_height = input$min_peak_height,
        min_peak_distance = input$min_peak_distance,
        threshold = input$threshold
      )
      incProgress(1)
    })
    
    print(processed$results)
    
    rv$results <- processed$results
    
    output$results_table <- renderDT({
      datatable(processed$results, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    rv$plots <- processed$plots
    
    output$graphs_ui <- renderUI({
      if(length(rv$plots) == 0){
        h4("No graphs available to display.")
      } else {
        plot_output_list <- lapply(names(rv$plots), function(name) {
          plotname <- paste("plot", gsub("[^A-Za-z0-9]", "_", name), sep = "_")
          plotOutput(plotname, height = "300px")
        })
        
        lapply(names(rv$plots), function(name) {
          local({
            plotname <- paste("plot", gsub("[^A-Za-z0-9]", "_", name), sep = "_")
            plot <- rv$plots[[name]]
            output[[plotname]] <- renderPlot({
              plot
            })
          })
        })
        
        do.call(tagList, plot_output_list)
      }
    })
    
    if(nrow(processed$skipped_files) > 0){
      showNotification(
        paste("Skipped", nrow(processed$skipped_files), "file(s). Check console for details."),
        type = "warning",
        duration = NULL
      )
      
      print("Skipped Files:")
      print(processed$skipped_files)
    }
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste("peak_analysis_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(rv$plots)

      print(rv$results)

      
      write.csv(isolate(rv$results), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
