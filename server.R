library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(leaflet)
library(shinyjs)
library(readr)
library(car)
library(nortest)
library(broom)
library(openxlsx)
library(rmarkdown)
library(knitr)
library(tseries)
library(gridExtra)
library(RColorBrewer)
library(classInt)
library(cluster)
library(factoextra)
library(sf)
library(spdep)
library(stringr)


metadata <- data.frame(
  Label = c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", "NOELECTRIC", "LOWEDU",
            "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION"),
  Variable = c("District Code", "Children", "Female", "Elderly", "Female household", "Household members", "Non-electric household", "Low education",
               "Population growth", "Poverty", "Illiteracy", "Training", "Disaster prone", "Homeownership", "Drainage", "Water source", "Population"),
  Description = c("Kode unik untuk setiap kabupaten/kota", "Persentase populasi di bawah lima tahun", "Persentase populasi perempuan",
                  "Persentase populasi berusia 65 tahun ke atas", "Persentase rumah tangga dengan kepala keluarga perempuan",
                  "Rata-rata jumlah anggota rumah tangga", "Persentase rumah tangga tanpa akses listrik",
                  "Persentase populasi 15 tahun ke atas dengan pendidikan rendah", "Persentase perubahan populasi", "Persentase penduduk miskin",
                  "Persentase populasi yang buta huruf", "Persentase rumah tangga yang tidak pernah mendapat pelatihan bencana",
                  "Persentase rumah tangga yang tinggal di area rawan bencana", "Persentase rumah tangga yang menyewa rumah",
                  "Persentase rumah tangga tanpa sistem drainase", "Persentase rumah tangga yang menggunakan air ledeng",
                  "Jumlah populasi")
)

interpret_pval <- function(pval, alpha = 0.05) {
  if (is.na(pval)) return("Hasil tidak tersedia.")
  if (pval <= alpha) {
    paste0("Karena p-value (", format.pval(pval, digits = 3, eps = 0.001), ") \u2264 ", alpha, ", maka H\u2080 ditolak.")
  } else {
    paste0("Karena p-value (", format.pval(pval, digits = 3, eps = 0.001), ") > ", alpha, ", maka H\u2080 gagal ditolak.")
  }
}

shinyServer(function(input, output, session) {
  
  histori_sesi <- reactiveVal(character(0))
  catat_histori <- function(pesan) {
    catatan_baru <- paste(format(Sys.time(), "%H:%M:%S"), "-", pesan)
    histori_lama <- histori_sesi()
    histori_sesi(c(histori_lama, catatan_baru))
  }
  
  values <- reactiveValues(
    data = NULL,
    spatial = NULL,
    kategorisasi_info = NULL,
    hasil_unduhan = list(),
    saved_plots = list()
  )
  
  data_loaded <- FALSE
  
  observe({
    if (data_loaded == FALSE) {
      
      sovi_data_raw <- tryCatch({
        read_csv("sovi_data.csv", show_col_types = FALSE)
      }, error = function(e) {
        showNotification("Gagal membaca file sovi_data.csv.", type = "error", duration = NULL)
        return(NULL)
      })
      
      if (is.null(sovi_data_raw)) return()
      
      names(sovi_data_raw) <- toupper(names(sovi_data_raw))
      values$data <- sovi_data_raw
      
      if (require(sf, quietly = TRUE) && file.exists("indonesia511.geojson")) {
        indonesia_geo <- tryCatch({
          st_read("indonesia511.geojson", quiet = TRUE, stringsAsFactors = FALSE)
        }, error = function(e) { NULL })
        
        if (!is.null(indonesia_geo)) {
          sovi_data_to_join <- values$data %>%
            mutate(key_clean = str_replace_all(as.character(DISTRICTCODE), "\\D", ""))
          
          indonesia_geo_to_join <- indonesia_geo %>%
            mutate(key_clean = str_replace_all(as.character(kodeprkab), "\\D", ""))
          
          common_keys <- intersect(indonesia_geo_to_join$key_clean, sovi_data_to_join$key_clean)
          
          if (length(common_keys) > 0) {
            spatial_data_joined <- indonesia_geo_to_join %>%
              left_join(sovi_data_to_join, by = "key_clean")
            
            numeric_cols <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", "NOELECTRIC", "LOWEDU",
                              "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING", "DPRONE", "RENTED",
                              "NOSEWER", "TAPWATER", "POPULATION")
            cols_to_convert <- intersect(toupper(numeric_cols), toupper(names(spatial_data_joined)))
            
            values$spatial <- spatial_data_joined %>%
              mutate(across(all_of(cols_to_convert), as.numeric))
            
            showNotification("Data spasial berhasil dimuat. Mode spasial aktif.", type = "message", duration = 5)
          } else {
            showNotification("Join GAGAL: Tidak ada kode wilayah yang cocok.", type = "error", duration = 15)
          }
        }
      }
      
      data_loaded <<- TRUE
    }
  })
  
  observe({
    req(values$data) 
    
    numeric_vars_initial <- names(values$data)[sapply(values$data, is.numeric)]
    categorical_vars_initial <- names(values$data)[sapply(values$data, function(x) is.character(x) || is.factor(x) || length(unique(x)) < 15)]
    
    updateSelectInput(session, "kategori_var", choices = numeric_vars_initial)
    updateSelectInput(session, "explore_var", choices = numeric_vars_initial)
    updateSelectInput(session, "normal_var", choices = numeric_vars_initial)
    updateSelectInput(session, "dep_var_homo", choices = numeric_vars_initial)
    updateSelectInput(session, "group_var_homo", choices = categorical_vars_initial)
    updateSelectInput(session, "rata1_var", choices = numeric_vars_initial)
    updateSelectInput(session, "rata2_var", choices = numeric_vars_initial)
    updateSelectInput(session, "rata2_group", choices = categorical_vars_initial)
    updateSelectInput(session, "rata3_var1", choices = numeric_vars_initial)
    updateSelectInput(session, "rata3_var2", choices = numeric_vars_initial)
    updateSelectInput(session, "prop1_var", choices = categorical_vars_initial)
    updateSelectInput(session, "prop2_var_cat", choices = categorical_vars_initial)
    updateSelectInput(session, "var1_var", choices = numeric_vars_initial)
    updateSelectInput(session, "var2_var_num", choices = numeric_vars_initial)
    updateSelectInput(session, "var2_var_cat", choices = categorical_vars_initial)
    updateSelectInput(session, "anova_y", choices = numeric_vars_initial)
    updateSelectInput(session, "anova_group1", choices = categorical_vars_initial)
    updateSelectInput(session, "anova_group2", choices = categorical_vars_initial)
    updateSelectInput(session, "regresi_y", choices = numeric_vars_initial)
    updateSelectInput(session, "regresi_x", choices = numeric_vars_initial)
    
    
    if (!is.null(values$spatial)) {
      spatial_numeric_vars <- names(values$spatial)[sapply(values$spatial, is.numeric)]
      vars_to_exclude <- c("FID", "gid", "kdprov", "kodeprkab.x", "kodeprkab.y", "key_clean.x", "key_clean.y", grep("_mean$", spatial_numeric_vars, value = TRUE))
      final_vars <- setdiff(spatial_numeric_vars, vars_to_exclude)
      
      updateSelectInput(session, "spasial_var", choices = final_vars)
      updateSelectInput(session, "cluster_var", choices = final_vars)
      updateSelectInput(session, "hotspot_var", choices = final_vars)
    }
  })
  
  observe({
    req(input$explore_var)
    current_y_var <- input$explore_var
    x_var_choices <- setdiff(names(values$data)[sapply(values$data, is.numeric)], current_y_var)
    updateSelectInput(session, "x_var", choices = x_var_choices)
  })
  
  observeEvent(input$prop1_var, {
    req(input$prop1_var)
    if (input$prop1_var %in% names(values$data)) {
      levels_prop1 <- unique(values$data[[input$prop1_var]])
      output$prop1_level_ui <- renderUI({
        selectInput("prop1_level", "Pilih Level:", choices = levels_prop1)
      })
    }
  })
  
  get_report_format <- function(input_val) {
    val <- toupper(as.character(input_val))
    if (val == "PDF") return(rmarkdown::pdf_document())
    if (val == "WORD") return(rmarkdown::word_document())
    rmarkdown::pdf_document() 
  }
  
  get_interpretasi_text <- function(ui_output) {
    if (inherits(ui_output, "shiny.tag.list") || inherits(ui_output, "shiny.tag")) {
      paste0(as.character(ui_output))
    } else if (is.character(ui_output)) {
      ui_output
    } else {
      ""
    }
  }
  
# BERANDA
  output$info_summary <- renderUI({ 
    tagList(
      HTML("<b>Judul:</b> Revisiting Social Vulnerability Analysis in Indonesia<br>"),
      HTML("<b>Penulis:</b> Robert Kurniawan, Bahrul Ilmi Nasution, Neli Agustina, Budi Yuniarto<br>"),
      HTML("<b>Sumber Data:</b> Survei Sosial Ekonomi Nasional (SUSENAS) 2017 dan Proyeksi Penduduk 2017 oleh BPS.<br>"),
      HTML("<b>Referensi:</b> "),
      tags$p(
        tags$a(href = "https://doi.org/10.1016/j.dib.2021.107743", 
               target = "_blank", 
               "https://doi.org/10.1016/j.dib.2021.107743")
      ),
      tags$ul(
        tags$li("Terdapat 511 kabupaten/kota dengan 17 indikator kerentanan sosial."),
        tags$li("Indikator mencakup aspek: demografi (anak-anak, lansia, perempuan), ekonomi (kemiskinan, pendidikan), dan ketahanan terhadap bencana."),
        tags$li("Disediakan pula matriks jarak antar wilayah untuk keperluan analisis spasial."),
        tags$li("Data dikalibrasi terhadap peta administratif 2013.")
      )
    ) 
  })
  summary_cards <- reactive({
    req(values$data) 
    
    values$data %>%
      select(where(is.numeric)) %>%
      summarise(across(everything(), list(mean = mean), na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = "Label", values_to = "Mean") %>%
      mutate(Label = gsub("_mean", "", Label)) %>%
      left_join(metadata, by = "Label") %>% 
      filter(!(Label %in% c("DISTRICTCODE", "POPULATION")))
  })
  output$avg_boxes <- renderUI({
    df <- summary_cards()
    req(nrow(df) > 0)
    
    box_ui <- lapply(1:nrow(df), function(i) {
      mean_value <- if (df$Label[i] == "FAMILYSIZE") {
        sprintf("%.2f", df$Mean[i])  
      } else {
        sprintf("%.2f%%", df$Mean[i]) 
      }
      
      div(class = "overview-box",
          HTML(paste0(
            "<b>", df$Label[i], ": ", mean_value, "</b><br/>",
            "<b>", df$Variable[i], "</b><br/>",
            df$Description[i]
          ))
      )
    })
    
    div(class = "box-grid", box_ui)
  })
  
  
  # Manajemen Data
  kategori_results <- eventReactive(input$btn_kategorisasi, {
    req(input$kategori_var, input$kategori_n)
    catat_histori(paste("Kategorisasi variabel:", input$kategori_var, "dengan", input$kategori_n, "kategori"))
    var_to_cat <- input$kategori_var
    num_cat <- as.integer(input$kategori_n)
    
    df <- values$data 
    if (!var_to_cat %in% names(df) || !is.numeric(df[[var_to_cat]])) {
      showNotification("Variabel yang dipilih tidak numerik atau tidak ditemukan.", type = "error")
      return(NULL)
    }
    
    cutpoints <- unique(quantile(df[[var_to_cat]], probs = seq(0, 1, length.out = num_cat + 1), na.rm = TRUE))
    if (length(cutpoints) < 2) {
      showNotification("Tidak cukup variasi dalam data untuk membuat kategori. Coba variabel lain atau jumlah kategori yang berbeda.", type = "error")
      return(NULL)
    }
    labels <- paste0("K", 1:num_cat)
    if (length(cutpoints) - 1 < num_cat) {
      labels <- paste0("K", 1:(length(cutpoints)-1))
    }
    
    new_col_name <- paste0(var_to_cat, "_", num_cat, "cat")
    df[[new_col_name]] <- cut(df[[var_to_cat]], breaks = cutpoints, include.lowest = TRUE, labels = labels)
    
    values$data <- df 
    
    info <- list(var = var_to_cat, n = num_cat, new_col = new_col_name, labels = labels, cutpoints = cutpoints)
    values$kategorisasi_info <- info
    
    updated_numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    updated_factor_vars <- names(values$data)[sapply(values$data, is.factor)]
    updated_categorical_vars <- names(values$data)[sapply(values$data, function(x) is.character(x) || is.factor(x) || length(unique(x)) < 15)]
    
    updateSelectInput(session, "kategori_var", choices = updated_numeric_vars)
    updateSelectInput(session, "explore_var", choices = updated_numeric_vars)
    updateSelectInput(session, "x_var", choices = updated_categorical_vars)
    updateSelectInput(session, "normal_var", choices = updated_numeric_vars)
    updateSelectInput(session, "dep_var_homo", choices = updated_numeric_vars)
    updateSelectInput(session, "rata1_var", choices = updated_numeric_vars)
    updateSelectInput(session, "rata2_var", choices = updated_numeric_vars)
    updateSelectInput(session, "rata2_group", choices = updated_categorical_vars)
    updateSelectInput(session, "rata3_var1", choices = updated_numeric_vars)
    updateSelectInput(session, "rata3_var2", choices = updated_numeric_vars)
    updateSelectInput(session, "prop1_var", choices = updated_categorical_vars)
    updateSelectInput(session, "prop2_var_cat", choices = updated_categorical_vars)
    updateSelectInput(session, "var1_var", choices = updated_numeric_vars)
    updateSelectInput(session, "var2_var_num", choices = updated_numeric_vars)
    updateSelectInput(session, "var2_var_cat", choices = updated_categorical_vars)
    updateSelectInput(session, "anova_y", choices = updated_numeric_vars)
    updateSelectInput(session, "anova_group1", choices = updated_categorical_vars)
    updateSelectInput(session, "anova_group2", choices = updated_categorical_vars)
    updateSelectInput(session, "regresi_y", choices = updated_numeric_vars)
    updateSelectInput(session, "regresi_x", choices = updated_numeric_vars, selected = setdiff(updated_numeric_vars, input$regresi_y)[1:min(4, length(setdiff(updated_numeric_vars, input$regresi_y)))])
    
    updateSelectInput(session, "group_var_homo", choices = updated_factor_vars, selected = new_col_name)
    updateSelectInput(session, "rata2_group", choices = updated_factor_vars[sapply(df[,updated_factor_vars, drop=FALSE], function(x) nlevels(x) == 2)])
    
    showNotification(paste("Variabel", var_to_cat, "berhasil dikategorikan."), type = "message")
    return(info)
  })
  
  output$interpretasi_kategori <- renderUI({
    info <- kategori_results()
    req(info)
    formatted_cutpoints <- sprintf("%.2f", info$cutpoints)
    rentang_list <- lapply(1:(length(info$cutpoints) - 1), function(i) {
      paste0("<li><b>", info$labels[i], "</b>: ", formatted_cutpoints[i], " s.d. ", formatted_cutpoints[i + 1], "</li>")
    })
    HTML(paste0("<p>Variabel <b>'", info$var, "'</b> telah dikategorikan menjadi <b>", info$n, "</b> grup dalam kolom <b>'", info$new_col, "'</b>.</p><b>Rentang nilai:</b><ul>", paste(rentang_list, collapse = ""), "</ul>"))
  })
  
  output$tabel_kategori <- renderDT({ datatable(values$data, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE) })
  
  # --- Unduhan Manajemen Data ---
  output$unduh_kategori_csv <- downloadHandler(
    filename = function() { paste0("data_kategorisasi_", Sys.Date(), ".xlsx") },
    content = function(file) {
      write.xlsx(values$data, file, rowNames = FALSE) 
    }
  )
  output$unduh_kategori_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_kategori_report), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_kategorisasi_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_kategori_report), "PDF" = "pdf_document", "WORD" = "word_document")
      info <- kategori_results()
      req(info, cancelOutput = TRUE)
      
      formatted_cutpoints <- sprintf("%.2f", info$cutpoints)
      rentang_list <- lapply(1:(length(info$cutpoints) - 1), function(i) paste0("- ", info$labels[i], ": ", formatted_cutpoints[i], " s.d. ", formatted_cutpoints[i + 1]))
      interpretasi_text <- paste0("Variabel '", info$var, "' telah dikategorikan menjadi ", info$n, " grup.\n\nRentang nilai:\n", paste(rentang_list, collapse = "\n"))
      
      params <- list(
        var_name = info$var, num_cat = info$n, new_col_name = info$new_col,
        interpretation_text = interpretasi_text, head_data = head(values$data), plot_path = NULL
      )
      
      temp_report_path <- file.path(tempdir(), "kategori_report.Rmd")
      file.copy("kategori_report.Rmd", temp_report_path, overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  # Eksplorasi Data
  explore_results <- eventReactive(input$run_eksplorasi, {
    req(input$explore_var, input$jenis_plot)
    catat_histori(paste("Eksplorasi Data:", input$jenis_plot, "pada variabel Y", input$explore_var, if (!is.null(input$x_var)) paste("dan X", input$x_var) else ""))
    plot_data <- values$data
    
    p <- NULL
    plot_title <- ""
    desc_summary <- NULL
    correlation_text <- NULL
    
    if (input$explore_var %in% names(plot_data)) {
      if (input$jenis_plot == "Histogram") {
        p <- ggplot(plot_data, aes_string(x = input$explore_var)) +
          geom_histogram(bins = 30, fill = "#f78fb3", color = "#fff0f5") +
          labs(title = paste("Histogram", input$explore_var)) +
          theme_minimal()
        plot_title <- paste("Histogram untuk", input$explore_var)
      } else if (input$jenis_plot == "Boxplot") {
        p <- ggplot(plot_data, aes_string(y = input$explore_var)) +
          geom_boxplot(fill = "#fbd6e3", color = "#d65076") +
          labs(title = paste("Boxplot", input$explore_var)) +
          theme_minimal()
        plot_title <- paste("Boxplot untuk", input$explore_var)
      } else if (input$jenis_plot == "Density") {
        p <- ggplot(plot_data, aes_string(x = input$explore_var)) +
          geom_density(fill = "#fbd6e3", color = "#d65076") +
          labs(title = paste("Density Plot", input$explore_var)) +
          theme_minimal()
        plot_title <- paste("Density Plot untuk", input$explore_var)
      } else if (input$jenis_plot == "Scatter Plot") {
        req(input$x_var)
        if (input$x_var %in% names(plot_data) && is.numeric(plot_data[[input$x_var]])) {
          p <- ggplot(plot_data, aes_string(x = input$x_var, y = input$explore_var)) +
            geom_point(alpha = 0.7, color = "#d65076") +
            labs(title = paste("Scatter Plot", input$explore_var, "vs", input$x_var)) +
            theme_minimal()
          plot_title <- paste("Scatter Plot untuk", input$explore_var, "dan", input$x_var)
          correlation_value <- cor(plot_data[[input$explore_var]], plot_data[[input$x_var]], use="complete.obs")
          correlation_text <- paste("Korelasi:", sprintf("%.3f", correlation_value))
        } else {
          showNotification("Variabel X untuk Scatter Plot tidak valid atau tidak numerik.", type = "error")
        }
      }
      
      desc_summary_y <- capture.output(print(summary(plot_data[[input$explore_var]])))
      desc_summary <- paste0("Ringkasan Statistik untuk Variabel Y (", input$explore_var, "):\n",
                             paste(desc_summary_y, collapse = "\n"))
      
      if (input$jenis_plot == "Scatter Plot" && !is.null(input$x_var)) {
        desc_summary_x <- capture.output(print(summary(plot_data[[input$x_var]])))
        desc_summary <- paste0(desc_summary, "\n\nRingkasan Statistik untuk Variabel X (", input$x_var, "):\n",
                               paste(desc_summary_x, collapse = "\n"), "\n\n", correlation_text)
      }
    } else {
      showNotification("Variabel yang dipilih tidak ditemukan.", type = "error")
    }
    
    list(plot = p, explore_var = input$explore_var, x_var = input$x_var,
         jenis_plot = input$jenis_plot, desc_summary = desc_summary,
         plot_title = plot_title)
  })
  
  output$plot_eksplorasi <- renderPlot({ explore_results()$plot })
  output$desc_table <- renderPrint({ cat(explore_results()$desc_summary) })
  output$explore_interpretation <- renderUI({
    res <- explore_results()
    if (is.null(res$desc_summary)) {
      HTML("<p>Tidak ada hasil untuk ditampilkan.</p>")
    } else {
      HTML(paste0("<p>Analisis eksplorasi untuk variabel <b>", res$explore_var, "</b> menunjukkan hal berikut:</p>",
                  "<p>Data visualisasikan menggunakan <b>", res$jenis_plot, "</b>. Interpretasi ringkasan statistik dan bentuk distribusi dapat dilihat dari grafik dan ringkasan di atas.</p>"))
    }
  })
  
  
  # --- Unduhan Eksplorasi Data ---
  output$unduh_eksplorasi_plot <- downloadHandler(
    filename = function() { paste0("plot_eksplorasi_", input$explore_var, "_", Sys.Date(), ".jpg") },
    content = function(file) {
      res <- explore_results()
      if (!is.null(res$plot)) {
        ggsave(file, plot = res$plot, device = "jpeg", width = 8, height = 6, units = "in")
      } else {
        stop("Tidak ada plot untuk diunduh.")
      }
    }
  )
  
  output$unduh_eksplorasi_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_eksplorasi_report), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_eksplorasi_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_eksplorasi_report), "PDF" = "pdf_document", "WORD" = "word_document")
      res <- explore_results()
      req(res, cancelOutput = TRUE)
      
      plot_path <- tempfile(fileext = ".png")
      if (!is.null(res$plot)) ggsave(plot_path, plot = res$plot, device = "png", width = 8, height = 6) else plot_path <- ""
      
      params <- list(
        summary_text = res$desc_summary,
        interpretation_text = get_interpretasi_text(output$explore_interpretation()),
        plot_path = plot_path
      )
      
      temp_report_path <- file.path(tempdir(), "eksplorasi_report.Rmd")
      file.copy("eksplorasi_report.Rmd", temp_report_path, overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  # ===== ANALISIS SPASIAL KERENTANAN SOSIAL =====
  
  # --- Peta Heatmap Kerentanan ---
  
  map_data_reactive <- eventReactive(input$run_spasial_map, {
    req(input$spasial_var, values$spatial)
    withProgress(message = 'Mempersiapkan data peta...', {
      data_map <- values$spatial %>%
        select(DISTRICTCODE, geometry, !!sym(input$spasial_var)) %>%
        filter(!is.na(!!sym(input$spasial_var)))
      
      if (nrow(data_map) == 0) {
        showNotification("Tidak ada data untuk variabel yang dipilih.", type = "error")
        return(NULL)
      }
      
      pal <- colorNumeric(palette = input$spasial_palette, domain = data_map[[input$spasial_var]])
      labels <- sprintf("<strong>%s</strong><br/>%s: %g", data_map$DISTRICTCODE, input$spasial_var, round(data_map[[input$spasial_var]], 2)) %>% lapply(htmltools::HTML)
      
      return(list(data_map = data_map, pal = pal, labels = labels, var = input$spasial_var))
    })
  })
  
  output$peta_kerentanan_leaflet <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = 118, lat = -2, zoom = 5)
  })
  
  observeEvent(input$run_spasial_map, {
    req(values$spatial, input$spasial_var)
    
    data_map <- values$spatial[ !is.na(values$spatial[[input$spasial_var]]), ]
    
    if (nrow(data_map) == 0) {
      showNotification("Tidak ada data untuk variabel yang dipilih.", type = "warning")
      return()
    }
    
    pal <- colorNumeric(palette = input$spasial_palette, domain = data_map[[input$spasial_var]])
    labels <- sprintf("<strong>%s, %s</strong><br/>%s: %g", data_map$nmkab, data_map$nmprov, input$spasial_var, round(data_map[[input$spasial_var]], 2)) %>% lapply(htmltools::HTML)
    
    leafletProxy("peta_kerentanan_leaflet", data = data_map) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(get(input$spasial_var)),
        weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label = labels
      ) %>%
      addLegend(
        pal = pal, 
        values = ~get(input$spasial_var), 
        opacity = 0.7, 
        title = input$spasial_var, 
        position = "bottomright"
      )
  })
  
  output$statistik_spasial <- renderPrint({
    map_inputs <- map_data_reactive()
    req(map_inputs, cancelOutput = TRUE)
    stats <- map_inputs$data_map[[map_inputs$var]]
    cat("=== STATISTIK DESKRIPTIF ===\n")
    cat("Indikator:", map_inputs$var, "\n"); cat("Jumlah Wilayah:", length(stats), "\n")
    cat("Nilai Minimum:", round(min(stats), 3), "\n"); cat("Nilai Maksimum:", round(max(stats), 3), "\n")
    cat("Rata-rata:", round(mean(stats), 3), "\n")
  })
  
  
  render_status_ui <- renderUI({
    if (!is.null(values$spatial)) {
      tags$div(
        style = "background-color: #d4edda; color: #155724; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
        tags$strong("✅ Mode Spasial Aktif:"), paste(nrow(values$spatial), "wilayah dimuat.")
      )
    } else {
      tags$div(
        style = "background-color: #f8d7da; color: #721c24; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
        tags$strong("❌ Mode Spasial Nonaktif:"), "Data peta gagal digabungkan dengan data statistik."
      )
    }
  })
  
  output$status_spasial <- render_status_ui
  output$status_cluster <- render_status_ui
  output$status_hotspot <- render_status_ui
  
  output$interpretasi_spasial <- renderUI({
    map_inputs <- map_data_reactive()
    req(map_inputs, cancelOutput = TRUE)
    HTML(paste0("<p>Peta distribusi untuk indikator <b>", map_inputs$var, "</b> berhasil ditampilkan.</p>"))
  })
  
  # --- Cluster Wilayah Rentan ---
  cluster_results <- eventReactive(input$run_cluster, {
    req(input$cluster_var)
    withProgress(message = 'Menjalankan analisis cluster...', {
      data_source <- if(!is.null(values$spatial)) values$spatial else values$data
      
      if (!input$cluster_var %in% names(data_source)) {
        showNotification(paste("Error: Kolom", input$cluster_var, "tidak ditemukan."), type = "error"); return(NULL)
      }
      
      cluster_data <- data_source %>%
        as.data.frame() %>%
        select(DISTRICTCODE, all_of(input$cluster_var)) %>%
        filter(!is.na(.data[[input$cluster_var]]))
      
      if(nrow(cluster_data) < input$cluster_k || sd(cluster_data[[input$cluster_var]], na.rm = TRUE) == 0) {
        showNotification("Data tidak cukup atau tidak bervariasi untuk clustering.", type = "error"); return(NULL)
      }
      
      set.seed(123)
      if (input$cluster_method == "kmeans") {
        kmeans_res <- kmeans(scale(cluster_data[[input$cluster_var]]), centers = input$cluster_k)
        cluster_data$cluster <- as.factor(kmeans_res$cluster)
      } else {
        dist_matrix <- dist(scale(cluster_data[[input$cluster_var]]))
        hc <- hclust(dist_matrix, method = "ward.D2")
        cluster_data$cluster <- as.factor(cutree(hc, k = input$cluster_k))
      }
      
      map_data <- if(!is.null(values$spatial)) {
        values$spatial %>% left_join(select(cluster_data, DISTRICTCODE, cluster), by = "DISTRICTCODE") %>% filter(!is.na(cluster))
      } else { NULL }
      
      summary_df <- cluster_data %>%
        group_by(cluster) %>%
        summarise(Jumlah_Wilayah = n(), Rata_rata_Indikator = mean(.data[[input$cluster_var]], na.rm = TRUE), .groups = 'drop')
      
      return(list(map_data = map_data, summary_df = summary_df, source_data = cluster_data))
    })
  })
  
  output$peta_cluster <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = 118, lat = -2, zoom = 5)
  })
  
  observeEvent(input$run_cluster, {
    req(values$spatial, input$cluster_var, input$cluster_k)
    
    cluster_data_sf <- values$spatial %>%
      select(DISTRICTCODE, nmkab, nmprov, geometry, all_of(input$cluster_var)) %>%
      filter(!is.na(.data[[input$cluster_var]]))
    
    if(nrow(cluster_data_sf) < input$cluster_k) {
      showNotification("Data tidak cukup untuk clustering.", type="error"); return()
    }
    
    data_for_clustering <- as.data.frame(cluster_data_sf) %>% select(all_of(input$cluster_var))
    
    set.seed(123)
    if (input$cluster_method == "kmeans") {
      kmeans_res <- kmeans(scale(data_for_clustering), centers = input$cluster_k)
      cluster_data_sf$cluster <- as.factor(kmeans_res$cluster)
    } else {
      dist_matrix <- dist(scale(data_for_clustering))
      hc <- hclust(dist_matrix, method = "ward.D2")
      cluster_data_sf$cluster <- as.factor(cutree(hc, k = input$cluster_k))
    }
    
    pal <- colorFactor("Set3", domain = cluster_data_sf$cluster)
    labels <- sprintf("<strong>%s</strong><br/>Cluster: %s", cluster_data_sf$nmkab, cluster_data_sf$nmprov, cluster_data_sf$cluster) %>% lapply(htmltools::HTML)
    
    leafletProxy("peta_cluster", data = cluster_data_sf) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~pal(cluster), weight = 1, color = "white", fillOpacity = 0.8, label = labels) %>%
      addLegend(pal = pal, values = ~cluster, title = "Cluster", position = "bottomright")
  })
  
  output$hasil_cluster <- renderPrint({
    results <- cluster_results(); req(results, cancelOutput = TRUE)
    print(as.data.frame(results$summary_df))
  })
  
  output$interpretasi_cluster <- renderUI({
    results <- cluster_results(); req(results, cancelOutput = TRUE)
    HTML(paste0("<p>Analisis cluster dengan <b>", input$cluster_k, "</b> cluster selesai.</p>"))
  })
  
  
  # === HOTSPOT/OUTLIER ANALYSIS RESTRUCTURED ===
  
  hotspot_results <- eventReactive(input$run_hotspot, {
    req(input$hotspot_var)
    withProgress(message = 'Menjalankan analisis hotspot...', {
      if (is.null(values$spatial)) {
        showNotification("Mode Spasial tidak aktif.", type = "warning"); return(NULL)
      }
      
      if (!input$hotspot_var %in% names(values$spatial)) {
        showNotification(paste("Error: Kolom", input$hotspot_var, "tidak ditemukan."), type = "error"); return(NULL)
      }
      
      hotspot_data <- values$spatial %>%
        select(DISTRICTCODE, geometry, !!sym(input$hotspot_var)) %>%
        filter(!is.na(!!sym(input$hotspot_var)))
      
      nb <- tryCatch(poly2nb(hotspot_data, queen = TRUE), error = function(e) {
        showNotification(paste("Gagal membuat topologi:", e$message), type="error"); NULL
      })
      if(is.null(nb)) return(NULL)
      w <- nb2listw(nb, style = "W", zero.policy = TRUE)
      
      if (input$hotspot_method == "moran") {
        localm <- localmoran(hotspot_data[[input$hotspot_var]], w, zero.policy = TRUE)
        hotspot_data$p_value <- localm[, "Pr(z != E(Ii))"]
        hotspot_data$type <- ifelse(localm[, "Ii"] > 0, "High-High", "Low-Low")
      } else {
        localg <- localG(hotspot_data[[input$hotspot_var]], w, zero.policy = TRUE)
        hotspot_data$p_value <- 2 * pnorm(-abs(as.numeric(localg)))
        hotspot_data$type <- ifelse(as.numeric(localg) > 0, "Hot Spot", "Cold Spot")
      }
      final_data <- hotspot_data %>% mutate(category = ifelse(p_value <= input$hotspot_threshold, type, "Tidak Signifikan"))
      return(list(data = final_data, is_spatial = TRUE))
    })
  })
  
  output$peta_hotspot <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = 118, lat = -2, zoom = 5)
  })
  
  observeEvent(input$run_hotspot, {
    req(values$spatial, input$hotspot_var, input$hotspot_threshold)
    
    hotspot_data <- values$spatial %>%
      select(DISTRICTCODE, nmkab, nmprov, geometry, !!sym(input$hotspot_var)) %>%
      filter(!is.na(!!sym(input$hotspot_var)))
    
    if(nrow(hotspot_data) < 10) { 
      showNotification("Data tidak cukup untuk analisis hotspot.", type="error"); return()
    }
    
    nb <- poly2nb(hotspot_data, queen = TRUE)
    w <- nb2listw(nb, style = "W", zero.policy = TRUE)
    
    if (input$hotspot_method == "moran") {
      localm <- localmoran(hotspot_data[[input$hotspot_var]], w, zero.policy = TRUE)
      hotspot_data$p_value <- localm[, "Pr(z != E(Ii))"]
      hotspot_data$lisa_type <- ifelse(localm[, "Ii"] > 0 & hotspot_data[[input$hotspot_var]] > mean(hotspot_data[[input$hotspot_var]]), "High-High", 
                                       ifelse(localm[, "Ii"] > 0 & hotspot_data[[input$hotspot_var]] < mean(hotspot_data[[input$hotspot_var]]), "Low-Low", 
                                              ifelse(localm[, "Ii"] < 0 & hotspot_data[[input$hotspot_var]] > mean(hotspot_data[[input$hotspot_var]]), "High-Low", "Low-High")))
      hotspot_data$category <- ifelse(hotspot_data$p_value <= input$hotspot_threshold, hotspot_data$lisa_type, "Tidak Signifikan")
      
    } else { # Getis-Ord Gi*
      localg <- as.vector(localG(hotspot_data[[input$hotspot_var]], w, zero.policy = TRUE))
      hotspot_data$p_value <- 2 * pnorm(-abs(localg))
      hotspot_data$type <- ifelse(localg > 0, "Hot Spot", "Cold Spot")
      hotspot_data$category <- ifelse(hotspot_data$p_value <= input$hotspot_threshold, hotspot_data$type, "Tidak Signifikan")
    }
    
    pal <- colorFactor(palette = c("red", "blue", "lightpink", "lightblue", "grey"), 
                       domain = c("High-High", "Low-Low", "Hot Spot", "Cold Spot", "Tidak Signifikan"), 
                       levels = c("High-High", "Low-Low", "Hot Spot", "Cold Spot", "Tidak Signifikan"))
    
    labels <- sprintf("<strong>%s, %s</strong><br/>Kategori: %s<br/>p-value: %.3f", hotspot_data$nmkab, hotspot_data$nmprov, hotspot_data$category, hotspot_data$p_value) %>% lapply(htmltools::HTML)
    
    leafletProxy("peta_hotspot", data = hotspot_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~pal(category), weight = 1, color = "white", fillOpacity = 0.8, label = labels) %>%
      addLegend(pal = pal, values = ~category, title = "Kategori Hotspot", position = "bottomright")
  })
  
  output$hasil_hotspot <- renderPrint({
    results <- hotspot_results(); req(results, cancelOutput = TRUE)
    print(table(results$data$category))
  })
  
  output$interpretasi_hotspot <- renderUI({
    results <- hotspot_results(); req(results, cancelOutput = TRUE)
    HTML("<p>Analisis hotspot selesai.</p>")
  })
  
  output$unduh_spasial_plot <- downloadHandler(
    filename = function() {
      active_tab <- req(input$spasial_tabs)
      var <- ""
      if (active_tab == "Distribusi Kerentanan") var <- input$spasial_var
      else if (active_tab == "Cluster Wilayah") var <- input$cluster_var
      else if (active_tab == "Analisis Outlier") var <- input$hotspot_var
      
      paste0("Peta_", active_tab, "_", var, "_", format(Sys.time(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      req(values$spatial)
      active_tab <- req(input$spasial_tabs)
      
      plot_to_save <- NULL
      
      if (active_tab == "Distribusi Kerentanan") {
        results <- map_data_reactive()
        req(results)
        plot_to_save <- ggplot(results$data_map) +
          geom_sf(aes_string(fill = results$var), color = "white", size = 0.1) +
          scale_fill_distiller(palette = input$spasial_palette, direction = 1) +
          labs(title = paste("Peta Distribusi:", results$var), fill = "Nilai") +
          theme_minimal()
        
      } else if (active_tab == "Cluster Wilayah") {
        results <- cluster_results()
        req(results$map_data)
        plot_to_save <- ggplot(results$map_data) + 
          geom_sf(aes(fill = cluster), color = "white", size = 0.1) +
          scale_fill_brewer(palette = "Set3", name = "Cluster") + 
          labs(title = "Peta Cluster Wilayah") + theme_minimal()
        
      } else if (active_tab == "Analisis Outlier") {
        results <- hotspot_results()
        req(results$data)
        plot_to_save <- ggplot(results$data) + 
          geom_sf(aes(fill = category), color = "white", size = 0.1) +
          scale_fill_manual(values = c("High-High" = "red", "Low-Low" = "blue", "Hot Spot" = "red", "Cold Spot" = "blue", "Tidak Signifikan" = "grey80"), name = "Kategori") +
          labs(title = "Peta Hotspot/Coldspot") + theme_minimal()
      }
      
      if (!is.null(plot_to_save)) {
        ggsave(file, plot = plot_to_save, width = 12, height = 8, dpi = 300)
      } else {
        showNotification("Tidak ada plot yang aktif untuk diunduh.", type = "warning")
      }
    }
  )
  
  output$unduh_spasial_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_spasial_report), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0("Laporan_Analisis_Spasial_", format(Sys.time(), "%Y%m%d"), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_spasial_report), "PDF" = "pdf_document", "WORD" = "word_document")
      req(values$spatial, cancelOutput = TRUE)
      
      params <- list(
        spatial_var = input$spasial_var, cluster_var = input$cluster_var,
        hotspot_var = input$hotspot_var, spatial_data = values$spatial
      )
      
      temp_report_path <- file.path(tempdir(), "spasial_report.Rmd")
      file.copy("spasial_report.Rmd", temp_report_path, overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  # --- Uji Normalitas ---
  normal_test_results <- eventReactive(input$run_normalitas, {
    req(input$normal_var)
    catat_histori(paste("Uji Normalitas:", input$uji_normalitas, "pada variabel", input$normal_var))
    x <- na.omit(values$data[[input$normal_var]])
    if (length(x) < 5) { 
      showNotification("Jumlah data terlalu sedikit untuk uji normalitas.", type = "warning")
      return(NULL)
    }
    
    test <- NULL
    method_name <- input$uji_normalitas
    p_hist <- NULL
    
    tryCatch({
      if (method_name == "Shapiro-Wilk") {
        if (length(x) > 5000) { 
          showNotification("Shapiro-Wilk tidak dapat digunakan untuk lebih dari 5000 data. Menggunakan Kolmogorov-Smirnov.", type = "warning")
          method_name <- "Kolmogorov-Smirnov"
        }
      }
      
      if (method_name == "Shapiro-Wilk") {
        test <- shapiro.test(x)
      } else if (method_name == "Kolmogorov-Smirnov") {
        test <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x)) 
      } else if (method_name == "Jarque-Bera") {
        test <- jarque.bera.test(x)
      } else if (method_name == "Chi-Square Goodness of Fit") {
        breaks <- hist(x, plot = FALSE, breaks = "Sturges")$breaks
        observed_counts <- hist(x, plot = FALSE, breaks = breaks)$counts
        expected_counts <- length(x) * diff(pnorm(breaks, mean = mean(x), sd = sd(x)))
        expected_probs <- expected_counts / sum(expected_counts)
        
        test <- suppressWarnings(chisq.test(x = observed_counts, p = expected_probs, rescale.p = TRUE))
        test$method <- "Chi-Square Goodness-of-Fit Test for Normality"
      }
      
      p_hist <- ggplot(data.frame(x=x), aes(x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill="#f78fb3", color="#fff0f5") +
        stat_function(fun=dnorm, args=list(mean=mean(x), sd=sd(x)), color="#d65076", linetype="dashed", size=1) +
        labs(title = paste("Histogram dengan Kurva Normal untuk", input$normal_var)) +
        theme_minimal()
    }, error = function(e) {
      showNotification(paste("Error saat menjalankan uji normalitas:", e$message), type = "error")
      return(NULL)
    })
    
    list(test = test, var = input$normal_var, plot = p_hist, method = method_name)
  })
  
  output$hasil_normalitas <- renderPrint({
    res <- normal_test_results()
    if (!is.null(res)) {
      print(res$test)
    }
  })
  output$hist_normal <- renderPlot({
    res <- normal_test_results()
    if (!is.null(res)) {
      res$plot
    }
  })
  output$normal_interpretasi <- renderUI({
    res <- normal_test_results()
    if (!is.null(res) && !is.null(res$test)) {
      HTML(paste0("<b>Hipotesis Nol (H\u2080):</b> Data berdistribusi normal.<br>",
                  "<b>Hipotesis Alternatif (H\u2081):</b> Data tidak berdistribusi normal.<br>",
                  interpret_pval(res$test$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })

  output$unduh_normalitas_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_normalitas_report), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_normalitas_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_normalitas_report), "PDF" = "pdf_document", "WORD" = "word_document")
      res <- normal_test_results()
      req(res, cancelOutput = TRUE)
      
      plot_path <- tempfile(fileext = ".png")
      if (!is.null(res$plot)) ggsave(plot_path, plot = res$plot, device = "png")
      
      params <- list(
        var = res$var, method = res$method, test_output = capture.output(print(res$test)),
        interpretation_text = get_interpretasi_text(output$normal_interpretasi()), plot_path = plot_path
      )
      
      temp_report_path <- file.path(tempdir(), "normalitas_report.Rmd")
      file.copy("normalitas_report.Rmd", temp_report_path, overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  # --- Uji Homogenitas ---
  homogen_test_results <- eventReactive(input$run_homogenitas, {
    req(input$dep_var_homo, input$group_var_homo, input$homo_plot_type)
    catat_histori(paste("Uji Homogenitas:", input$metode_homogenitas, "pada", input$dep_var_homo, "oleh", input$group_var_homo))
    df <- values$data
    y <- df[[input$dep_var_homo]]
    grup <- as.factor(df[[input$group_var_homo]])
    
    if (length(levels(grup)) < 2) {
      showNotification("Variabel grup harus memiliki setidaknya dua kategori.", type = "warning")
      return(NULL)
    }
    if (!is.numeric(y)) {
      showNotification("Variabel dependen harus numerik.", type = "warning")
      return(NULL)
    }
    
    test <- NULL
    p_value <- NA
    method_name <- input$metode_homogenitas
    
    tryCatch({
      df <- values$data
      y <- df[[input$dep_var_homo]]
      grup <- as.factor(df[[input$group_var_homo]])
      
      if (method_name == "Levene") {
        test <- leveneTest(y ~ grup, data = df)
        p_value <- test$`Pr(>F)`[1]
      } else if (method_name == "Bartlett") {
        test <- bartlett.test(y ~ grup, data = df)
        p_value <- test$p.value
      } else if (method_name == "F-test (hanya 2 grup)") {
        if (nlevels(grup) != 2) {
          showNotification("F-test hanya berlaku untuk dua grup. Pilih metode lain atau variabel grup dengan 2 kategori.", type = "warning")
          return(NULL)
        }
        test <- var.test(y ~ grup, data = df)
        p_value <- test$p.value
      }
      
      plot_scatter <- NULL
      if (input$homo_plot_type == "Boxplot") {
        plot_scatter <- ggplot(df, aes_string(x=input$group_var_homo, y=input$dep_var_homo, fill=input$group_var_homo)) +
          geom_boxplot(alpha = 0.7, fill = "#fbd6e3", color = "#d65076") +
          labs(title = paste("Boxplot", input$dep_var_homo, "berdasarkan", input$group_var_homo)) +
          theme_minimal() + theme(legend.position = "none")
      } else { # Scatter Plot
        plot_scatter <- ggplot(df, aes_string(x=input$group_var_homo, y=input$dep_var_homo, color=input$group_var_homo)) +
          geom_jitter(width = 0.2, alpha = 0.6, color = "#d65076") +
          labs(title = paste("Scatter Plot", input$dep_var_homo, "berdasarkan", input$group_var_homo)) +
          theme_minimal() + theme(legend.position = "none")
      }
      
    }, error = function(e) {
      showNotification(paste("Error saat menjalankan uji homogenitas:", e$message), type = "error")
      return(NULL)
    })
    
    list(test = test, method = method_name, dep = input$dep_var_homo, grp = input$group_var_homo, plot = plot_scatter, p_value = p_value)
  })
  
  output$hasil_homogenitas <- renderPrint({
    res <- homogen_test_results()
    if (!is.null(res)) {
      print(res$test)
    }
  })
  output$plot_scatter_homo <- renderPlot({
    res <- homogen_test_results()
    if (!is.null(res)) {
      res$plot
    }
  })
  output$interpretasi_homogenitas <- renderUI({
    res <- homogen_test_results()
    if (!is.null(res) && !is.null(res$test)) {
      HTML(paste0("<b>Hipotesis Nol (H\u2080):</b> Varians antar grup adalah homogen.<br>",
                  "<b>Hipotesis Alternatif (H\u2081):</b> Varians antar grup tidak homogen.<br>",
                  interpret_pval(res$p_value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })

  output$unduh_homogenitas_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_homogenitas_report), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_homogenitas_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_homogenitas_report), "PDF" = "pdf_document", "WORD" = "word_document")
      res <- homogen_test_results()
      req(res, cancelOutput = TRUE)
      
      plot_path <- tempfile(fileext = ".png")
      if (!is.null(res$plot)) ggsave(plot_path, plot = res$plot, device = "png")
      
      params <- list(
        test_output = capture.output(print(res$test)),
        interpretation_text = get_interpretasi_text(output$interpretasi_homogenitas()),
        plot_path = plot_path
      )
      
      temp_report_path <- file.path(tempdir(), "homogenitas_report.Rmd")
      file.copy("homogenitas_report.Rmd", temp_report_path, overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  # --- Uji t 1 Sampel ---
  ttest1_res <- eventReactive(input$run_rata1, {
    req(input$rata1_var, input$rata1_mu)
    catat_histori(paste("Uji t 1 Sampel pada variabel:", input$rata1_var, "dengan mu0 =", input$rata1_mu))
    x <- na.omit(values$data[[input$rata1_var]])
    if (length(x) == 0) {
      showNotification("Variabel yang dipilih kosong setelah menghilangkan NA.", type = "warning")
      return(NULL)
    }
    t.test(x, mu = input$rata1_mu)
  })
  output$hasil_uji_rata1 <- renderPrint({
    res <- ttest1_res()
    if (!is.null(res)) print(res)
  })
  output$interpret_rata1 <- renderUI({
    res <- ttest1_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: \u03bc = ", input$rata1_mu, "</b> vs <b>H\u2081: \u03bc \u2260 ", input$rata1_mu, "</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  
  output$unduh_rata1_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_rata1_report), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_t1sampel_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_rata1_report), "PDF" = "pdf_document", "WORD" = "word_document")
      res <- ttest1_res()
      req(res, cancelOutput = TRUE)
      
      plot_path <- tempfile(fileext = ".png")
      ggsave(plot_path, plot = output$plot_rata1(), device = "png")
      
      params <- list(
        var = input$rata1_var, mu = input$rata1_mu, test_output = capture.output(print(res)),
        interpretation_text = get_interpretasi_text(output$interpret_rata1()), plot_path = plot_path
      )
      
      temp_report_path <- file.path(tempdir(), "ratarata_1sampel_report.Rmd")
      file.copy("ratarata_1sampel_report.Rmd", temp_report_path, overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$plot_rata1 <- renderPlot({
    res <- ttest1_res()
    if (!is.null(res)) {
      x <- na.omit(values$data[[input$rata1_var]])
      if (length(x) > 0) {
        df_plot <- data.frame(Value = x)
        ggplot(df_plot, aes(x = Value)) +
          geom_histogram(bins = 20, fill = "#f78fb3", alpha = 0.7, color = "#fff0f5") +
          geom_vline(xintercept = mean(x), color = "#d65076", linetype = "dashed", size = 1) +
          geom_vline(xintercept = input$rata1_mu, color = "#5c2a41", linetype = "solid", size = 1) +
          labs(title = paste("Distribusi", input$rata1_var),
               x = input$rata1_var, y = "Frekuensi",
               subtitle = paste("Garis merah: rata-rata sampel (", round(mean(x), 3), "), Garis ungu: hipotesis μ₀ (", input$rata1_mu, ")")) +
          theme_minimal()
      }
    }
  })
  
  # --- Uji t 2 Sampel Bebas ---
  ttest2_res <- eventReactive(input$run_rata2, {
    req(input$rata2_var, input$rata2_group)
    catat_histori(paste("Uji t 2 Sampel Independen:", input$rata2_var, "berdasarkan grup", input$rata2_group))
    df_clean <- values$data %>% select(all_of(c(input$rata2_var, input$rata2_group))) %>% na.omit()
    if (nrow(df_clean) == 0) {
      showNotification("Data kosong setelah menghilangkan NA.", type = "warning")
      return(NULL)
    }
    group_var <- as.factor(df_clean[[input$rata2_group]])
    if (nlevels(group_var) != 2) {
      showNotification("Variabel grup harus memiliki tepat dua kategori untuk Uji t 2 Sampel Independen.", type = "warning")
      return(NULL)
    }
    formula_str <- paste(input$rata2_var, "~", input$rata2_group)
    t.test(as.formula(formula_str), data = df_clean)
  })
  output$hasil_uji_rata2 <- renderPrint({
    res <- ttest2_res()
    if (!is.null(res)) print(res)
  })
  output$interpret_rata2 <- renderUI({
    res <- ttest2_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: \u03bc\u2081 = \u03bc\u2082</b> vs <b>H\u2081: \u03bc\u2081 \u2260 \u03bc\u2082</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  
  output$unduh_rata2_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_rata2_report), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_t2bebas_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_rata2_report), "PDF" = "pdf_document", "WORD" = "word_document")
      res <- ttest2_res()
      req(res, cancelOutput = TRUE)
      
      plot_path <- tempfile(fileext = ".png")
      ggsave(plot_path, plot = output$plot_rata2(), device = "png")
      
      params <- list(
        test_output = capture.output(print(res)),
        interpretation_text = get_interpretasi_text(output$interpret_rata2()),
        plot_path = plot_path
      )
      
      temp_report_path <- file.path(tempdir(), "ratarata_2bebas_report.Rmd")
      file.copy("ratarata_2bebas_report.Rmd", temp_report_path, overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$plot_rata2 <- renderPlot({
    res <- ttest2_res()
    if (!is.null(res)) {
      df_clean <- values$data %>% select(all_of(c(input$rata2_var, input$rata2_group))) %>% na.omit()
      if (nrow(df_clean) > 0) {
        ggplot(df_clean, aes_string(x = input$rata2_group, y = input$rata2_var)) +
          geom_boxplot(fill = "#fbd6e3", color = "#d65076", alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5, color = "#d65076") +
          labs(title = paste("Perbandingan", input$rata2_var, "berdasarkan", input$rata2_group),
               x = input$rata2_group, y = input$rata2_var) +
          theme_minimal() +
          theme(legend.position = "none")
      }
    }
  })
  
  # --- Uji t 2 Sampel Berpasangan ---
  ttest3_res <- eventReactive(input$run_rata3, {
    req(input$rata3_var1, input$rata3_var2)
    catat_histori(paste("Uji t 2 Sampel Berpasangan:", input$rata3_var1, "vs", input$rata3_var2))

    df_paired <- values$data %>% select(all_of(c(input$rata3_var1, input$rata3_var2))) %>% na.omit()
    if (nrow(df_paired) == 0) {
      showNotification("Data kosong setelah menghilangkan NA atau variabel tidak dapat dipasangkan.", type = "warning")
      return(NULL)
    }
    t.test(df_paired[[input$rata3_var1]], df_paired[[input$rata3_var2]], paired = TRUE)
  })
  output$hasil_uji_rata3 <- renderPrint({
    res <- ttest3_res()
    if (!is.null(res)) print(res)
  })
  output$interpret_rata3 <- renderUI({
    res <- ttest3_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: \u03bc_diff = 0</b> vs <b>H\u2081: \u03bc_diff \u2260 0</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  
  output$unduh_rata3_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_rata3_report), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_t2pasangan_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_rata3_report), "PDF" = "pdf_document", "WORD" = "word_document")
      res <- ttest3_res()
      req(res, cancelOutput = TRUE)
      
      plot_path <- tempfile(fileext = ".png")
      ggsave(plot_path, plot = output$plot_rata3(), device = "png")
      
      params <- list(
        test_output = capture.output(print(res)),
        interpretation_text = get_interpretasi_text(output$interpret_rata3()),
        plot_path = plot_path
      )
      
      temp_report_path <- file.path(tempdir(), "ratarata_2pasangan_report.Rmd")
      file.copy("ratarata_2pasangan_report.Rmd", temp_report_path, overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$plot_rata3 <- renderPlot({
    res <- ttest3_res()
    if (!is.null(res)) {
      df_paired <- values$data %>% select(all_of(c(input$rata3_var1, input$rata3_var2))) %>% na.omit()
      if (nrow(df_paired) > 0) {
        df_long <- tidyr::pivot_longer(df_paired, cols = everything(), names_to = "Variabel", values_to = "Nilai")
        ggplot(df_long, aes(x = Variabel, y = Nilai, fill = Variabel)) +
          geom_boxplot(fill = "#fbd6e3", color = "#d65076", alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5, color = "#d65076") +
          labs(title = paste("Perbandingan Berpasangan", input$rata3_var1, "vs", input$rata3_var2),
               x = "Variabel", y = "Nilai") +
          theme_minimal() +
          theme(legend.position = "none")
      }
    }
  })
  
  
  # Uji Proporsi 1 Sampel
  prop1_res <- eventReactive(input$run_prop1, {
    req(input$prop1_var, input$prop1_level, input$prop1_p)
    catat_histori(paste("Uji Proporsi 1 Sampel:", input$prop1_var, "level", input$prop1_level, "p0 =", input$prop1_p))
    df_clean <- values$data %>% select(all_of(input$prop1_var)) %>% na.omit()
    if (nrow(df_clean) == 0) {
      showNotification("Data kosong setelah menghilangkan NA.", type = "warning")
      return(NULL)
    }
    x_n <- sum(df_clean[[input$prop1_var]] == input$prop1_level, na.rm=TRUE)
    n <- length(df_clean[[input$prop1_var]])
    if (n == 0) {
      showNotification("Ukuran sampel nol.", type = "warning")
      return(NULL)
    }
    prop.test(x=x_n, n=n, p=input$prop1_p)
  })
  output$hasil_prop1 <- renderPrint({ res <- prop1_res(); if(!is.null(res)) print(res) })
  output$interpret_prop1 <- renderUI({
    res <- prop1_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: p = ", input$prop1_p, "</b> vs <b>H\u2081: p \u2260 ", input$prop1_p, "</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  
  prop1_plot <- reactive({
    req(input$prop1_var, input$prop1_level, input$prop1_p)
    
    df_clean <- values$data %>% select(all_of(input$prop1_var)) %>% na.omit()
    x_n <- sum(df_clean[[input$prop1_var]] == input$prop1_level, na.rm=TRUE)
    n <- length(df_clean[[input$prop1_var]])
    
    plot_data <- data.frame(
      Category = c(input$prop1_level, paste0("Bukan ", input$prop1_level)),
      Count = c(x_n, n - x_n),
      Proportion = c(x_n/n, (n-x_n)/n)
    )
    
    ggplot(plot_data, aes(x = Category, y = Proportion, fill = Category)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      geom_hline(yintercept = input$prop1_p, color = "red", linetype = "dashed", size = 1) +
      geom_text(aes(label = paste0(Count, "/", n, "\n(", round(Proportion*100, 1), "%)")), 
                vjust = -0.5) +
      labs(title = paste("Distribusi Proporsi", input$prop1_var),
           subtitle = paste("Garis merah: proporsi hipotesis (", input$prop1_p, ")"),
           x = "Kategori", y = "Proporsi") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  prop1_var_info <- reactive({
    req(input$prop1_var, input$prop1_level, input$prop1_p)
    paste0("**Variabel yang diuji:** ", input$prop1_var, "\n\n",
           "**Level yang dihitung proporsinya:** ", input$prop1_level, "\n\n",
           "**Proporsi hipotesis (p₀):** ", input$prop1_p)
  })
  
  output$unduh_propvar_report1 <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_propvar_report1), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_prop1sampel_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_propvar_report1), "PDF" = "pdf_document", "WORD" = "word_document")
      res <- prop1_res(); req(res, cancelOutput = TRUE)
      plot_path <- tempfile(fileext = ".png"); ggsave(plot_path, plot = prop1_plot(), device = "png")
      params <- list(
        title = "Uji Proporsi 1 Sampel", test_output = capture.output(print(res)),
        interpretation_text = get_interpretasi_text(output$interpret_prop1()),
        plot_path = plot_path, var_info = prop1_var_info()
      )
      temp_report_path <- file.path(tempdir(), "propvar_report.Rmd"); file.copy("propvar_report.Rmd", temp_report_path, overwrite = TRUE)
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$plot_prop1 <- renderPlot({
    req(input$prop1_var, input$prop1_level)
    df <- values$data
    if (input$prop1_var %in% names(df)) {
      tab <- table(df[[input$prop1_var]])
      bar_df <- data.frame(Level = names(tab), Count = as.numeric(tab))
      ggplot(bar_df, aes(x = Level, y = Count, fill = Level)) +
        geom_bar(stat = "identity", fill = "#f78fb3", color = "#d65076") +
        labs(title = paste("Distribusi", input$prop1_var), x = input$prop1_var, y = "Frekuensi") +
        theme_minimal()
    }
  })
  
  # Uji Proporsi 2 Sampel
  observeEvent(input$prop2_var_cat, {
    req(input$prop2_var_cat)
    levels <- unique(na.omit(values$data[[input$prop2_var_cat]]))
    
    output$prop2_level1_ui <- renderUI({
      selectInput("prop2_level1", "Pilih Grup 1:", choices = levels, selected = levels[1])
    })
    output$prop2_level2_ui <- renderUI({
      selectInput("prop2_level2", "Pilih Grup 2:", choices = levels, selected = levels[2])
    })
    output$prop2_success_ui <- renderUI({
      selectInput("prop2_success", "Pilih Level Keberhasilan (Success):", choices = levels, selected = levels[1])
    })
  })
  
  prop2_res <- eventReactive(input$run_prop2, {
    req(input$prop2_var_cat, input$prop2_level1, input$prop2_level2, input$prop2_success)
    catat_histori(paste("Uji Proporsi 2 Sampel:", input$prop2_var_cat, "antara", input$prop2_level1, "vs", input$prop2_level2))
    
    if (input$prop2_level1 == input$prop2_level2) {
      showNotification("Grup 1 dan Grup 2 tidak boleh sama.", type = "error"); return(NULL)
    }
    
    df <- values$data[[input$prop2_var_cat]]
    grup1_data <- df[df == input$prop2_level1]
    grup2_data <- df[df == input$prop2_level2]
    
    x1 <- sum(grup1_data == input$prop2_success, na.rm = TRUE)
    n1 <- length(na.omit(grup1_data))
    x2 <- sum(grup2_data == input$prop2_success, na.rm = TRUE)
    n2 <- length(na.omit(grup2_data))
    
    prop.test(x = c(x1, x2), n = c(n1, n2))
  })
  
  prop2_plot <- reactive({
    req(input$prop2_var_cat, input$prop2_level1, input$prop2_level2, input$prop2_success)
    
    df <- values$data[[input$prop2_var_cat]]
    grup1_data <- df[df == input$prop2_level1]
    grup2_data <- df[df == input$prop2_level2]
    
    x1 <- sum(grup1_data == input$prop2_success, na.rm = TRUE)
    n1 <- length(na.omit(grup1_data))
    x2 <- sum(grup2_data == input$prop2_success, na.rm = TRUE)
    n2 <- length(na.omit(grup2_data))
    
    plot_data <- data.frame(
      Grup = c(input$prop2_level1, input$prop2_level2),
      Sukses = c(x1, x2),
      Total = c(n1, n2),
      Proporsi = c(x1/n1, x2/n2)
    )
    
    ggplot(plot_data, aes(x = Grup, y = Proporsi, fill = Grup)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      geom_text(aes(label = paste0(Sukses, "/", Total, "\n(", round(Proporsi*100, 1), "%)")), 
                vjust = -0.5) +
      labs(title = paste("Perbandingan Proporsi", input$prop2_success, "antara", input$prop2_level1, "dan", input$prop2_level2),
           x = "Grup", y = "Proporsi") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  prop2_var_info <- reactive({
    req(input$prop2_var_cat, input$prop2_level1, input$prop2_level2, input$prop2_success)
    paste0("**Variabel Grup:** ", input$prop2_var_cat, "\n\n",
           "**Grup yang dibandingkan:** ", input$prop2_level1, " vs ", input$prop2_level2, "\n\n",
           "**Level keberhasilan:** ", input$prop2_success)
  })
  
  output$hasil_prop2 <- renderPrint({ res <- prop2_res(); if(!is.null(res)) print(res) })
  output$interpret_prop2 <- renderUI({
    res <- prop2_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: p\u2081 = p\u2082</b> vs <b>H\u2081: p\u2081 \u2260 p\u2082</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  output$unduh_propvar_report2 <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_propvar_report2), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_prop2sampel_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_propvar_report2), "PDF" = "pdf_document", "WORD" = "word_document")
      res <- prop2_res(); req(res, cancelOutput = TRUE)
      plot_path <- tempfile(fileext = ".png"); ggsave(plot_path, plot = prop2_plot(), device = "png")
      params <- list(
        title = "Uji Proporsi 2 Sampel", test_output = capture.output(print(res)),
        interpretation_text = get_interpretasi_text(output$interpret_prop2()),
        plot_path = plot_path, var_info = prop2_var_info()
      )
      temp_report_path <- file.path(tempdir(), "propvar_report.Rmd"); file.copy("propvar_report.Rmd", temp_report_path, overwrite = TRUE)
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$plot_prop2 <- renderPlot({
    req(input$prop2_var_cat)
    df <- values$data
    if (input$prop2_var_cat %in% names(df)) {
      tab <- table(df[[input$prop2_var_cat]])
      bar_df <- data.frame(Level = names(tab), Count = as.numeric(tab))
      ggplot(bar_df, aes(x = Level, y = Count, fill = Level)) +
        geom_bar(stat = "identity", fill = "#f78fb3", color = "#d65076") +
        labs(title = paste("Distribusi", input$prop2_var_cat), x = input$prop2_var_cat, y = "Frekuensi") +
        theme_minimal()
    }
  })
  
  # Uji Varians 1 Sampel
  var1_res <- eventReactive(input$run_var1, {
    req(input$var1_var, input$var1_val)
    catat_histori(paste("Uji Varians 1 Sampel:", input$var1_var, "dengan varians hipotesis", input$var1_val))
    x <- na.omit(values$data[[input$var1_var]])
    if (length(x) < 2) {
      showNotification("Jumlah data terlalu sedikit untuk uji varians.", type = "warning")
      return(NULL)
    }
    if (input$var1_val <= 0) {
      showNotification("Nilai hipotesis varians harus lebih besar dari 0.", type = "warning")
      return(NULL)
    }
    
    n <- length(x)
    sample_var <- var(x)
    chi2_stat <- (n - 1) * sample_var / input$var1_val
    
    p_val <- 2 * min(pchisq(chi2_stat, n - 1), 1 - pchisq(chi2_stat, n - 1))
    
    structure(list(
      statistic = c("X-squared" = chi2_stat),
      parameter = c(df = n - 1),
      p.value = p_val,
      method = "Chi-squared Test for One Sample Variance",
      data.name = input$var1_var,
      alternative = "two.sided",
      estimate = c("sample variance" = sample_var),
      null.value = c("variance" = input$var1_val)
    ), class = "htest")
  })
  output$hasil_var1 <- renderPrint({ res <- var1_res(); if(!is.null(res)) print(res) })
  output$interpret_var1 <- renderUI({
    res <- var1_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: \u03c3\u00b2 = ", input$var1_val, "</b> vs <b>H\u2081: \u03c3\u00b2 \u2260 ", input$var1_val, "</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  
  var1_plot <- reactive({
    req(input$var1_var, input$var1_val)
    
    x <- na.omit(values$data[[input$var1_var]])
    if (length(x) < 2) return(NULL)
    
    df_plot <- data.frame(Value = x)
    sample_var <- var(x)
    
    ggplot(df_plot, aes(x = Value)) +
      geom_histogram(bins = 20, fill = "lightgreen", alpha = 0.7, color = "black") +
      labs(title = paste("Distribusi", input$var1_var),
           subtitle = paste("Varians sampel:", round(sample_var, 3), "| Varians hipotesis:", input$var1_val),
           x = input$var1_var, y = "Frekuensi") +
      theme_minimal()
  })
  
  var1_var_info <- reactive({
    req(input$var1_var, input$var1_val)
    x <- na.omit(values$data[[input$var1_var]])
    sample_var <- var(x)
    paste0("**Variabel yang diuji:** ", input$var1_var, "\n\n",
           "**Varians hipotesis (σ²₀):** ", input$var1_val, "\n\n",
           "**Varians sampel:** ", round(sample_var, 4), "\n\n",
           "**Ukuran sampel:** ", length(x))
  })
  
  output$unduh_propvar_report3 <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_propvar_report3), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_var1sampel_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_propvar_report3), "PDF" = "pdf_document", "WORD" = "word_document")
      res <- var1_res(); req(res, cancelOutput = TRUE)
      plot_path <- tempfile(fileext = ".png"); ggsave(plot_path, plot = var1_plot(), device = "png")
      params <- list(
        title = "Uji Varians 1 Sampel", test_output = capture.output(print(res)),
        interpretation_text = get_interpretasi_text(output$interpret_var1()),
        plot_path = plot_path, var_info = var1_var_info()
      )
      temp_report_path <- file.path(tempdir(), "propvar_report.Rmd"); file.copy("propvar_report.Rmd", temp_report_path, overwrite = TRUE)
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$plot_var1 <- renderPlot({
    req(input$var1_var)
    x <- na.omit(values$data[[input$var1_var]])
    if (length(x) > 0) {
      df_plot <- data.frame(Value = x)
      ggplot(df_plot, aes(x = Value)) +
        geom_histogram(bins = 20, fill = "#f78fb3", alpha = 0.7, color = "#fff0f5") +
        labs(title = paste("Distribusi", input$var1_var), x = input$var1_var, y = "Frekuensi") +
        theme_minimal()
    }
  })
  
  # Uji Varians 2 Sampel
  observeEvent(input$var2_var_cat, {
    req(input$var2_var_cat)
    levels <- unique(na.omit(values$data[[input$var2_var_cat]]))
    
    output$var2_level1_ui <- renderUI({
      selectInput("var2_level1", "Pilih Grup 1:", choices = levels, selected = levels[1])
    })
    output$var2_level2_ui <- renderUI({
      selectInput("var2_level2", "Pilih Grup 2:", choices = levels, selected = levels[2])
    })
  })
  
  var2_res <- eventReactive(input$run_var2, {
    req(input$var2_var_num, input$var2_var_cat, input$var2_level1, input$var2_level2)
    catat_histori(paste("Uji Varians 2 Sampel:", input$var2_var_num, "antara", input$var2_level1, "vs", input$var2_level2))
    
    if (input$var2_level1 == input$var2_level2) {
      showNotification("Grup 1 dan Grup 2 tidak boleh sama.", type = "error"); return(NULL)
    }
    
    df <- values$data
    var_num <- df[[input$var2_var_num]]
    var_cat <- df[[input$var2_var_cat]]
    
    data1 <- var_num[var_cat == input$var2_level1]
    data2 <- var_num[var_cat == input$var2_level2]
    
    var.test(data1, data2)
  })
  
  var2_plot <- reactive({
    req(input$var2_var_num, input$var2_var_cat, input$var2_level1, input$var2_level2)
    
    df <- values$data
    var_num <- df[[input$var2_var_num]]
    var_cat <- df[[input$var2_var_cat]]
    
    data1 <- var_num[var_cat == input$var2_level1]
    data2 <- var_num[var_cat == input$var2_level2]
    
    data1 <- data1[!is.na(data1)]
    data2 <- data2[!is.na(data2)]
    
    combined_data <- data.frame(
      Value = c(data1, data2),
      Group = c(rep(input$var2_level1, length(data1)), 
                rep(input$var2_level2, length(data2)))
    )
    
    stats_data <- data.frame(
      Group = c(input$var2_level1, input$var2_level2),
      Variance = c(var(data1), var(data2)),
      SD = c(sd(data1), sd(data2)),
      N = c(length(data1), length(data2))
    )
    
    p1 <- ggplot(combined_data, aes(x = Group, y = Value, fill = Group)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = paste("Distribusi", input$var2_var_num, "berdasarkan", input$var2_var_cat),
           x = input$var2_var_cat, y = input$var2_var_num) +
      theme_minimal() +
      theme(legend.position = "none")
    
    p2 <- ggplot(stats_data, aes(x = Group, y = Variance, fill = Group)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      geom_text(aes(label = paste0("Var: ", round(Variance, 3), "\nSD: ", round(SD, 3), "\nN: ", N)), 
                vjust = -0.5) +
      labs(title = "Perbandingan Varians antar Grup",
           x = "Grup", y = "Varians") +
      theme_minimal() +
      theme(legend.position = "none")
    
    gridExtra::grid.arrange(p1, p2, ncol = 1)
  })
  
  var2_var_info <- reactive({
    req(input$var2_var_num, input$var2_var_cat, input$var2_level1, input$var2_level2)
    paste0("**Variabel Numerik:** ", input$var2_var_num, "\n\n",
           "**Variabel Grup:** ", input$var2_var_cat, "\n\n",
           "**Grup yang dibandingkan:** ", input$var2_level1, " vs ", input$var2_level2)
  })
  
  output$hasil_var2 <- renderPrint({ res <- var2_res(); if(!is.null(res)) print(res) })
  output$interpret_var2 <- renderUI({
    res <- var2_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: \u03c3\u2081\u00b2 = \u03c3\u2082\u00b2</b> vs <b>H\u2081: \u03c3\u2081\u00b2 \u2260 \u03c3\u2082\u00b2</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  output$unduh_propvar_report4 <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_propvar_report4), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_var2sampel_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_propvar_report4), "PDF" = "pdf_document", "WORD" = "word_document")
      res <- var2_res(); req(res, cancelOutput = TRUE)
      plot_path <- tempfile(fileext = ".png"); ggsave(plot_path, plot = var2_plot(), device = "png")
      params <- list(
        title = "Uji Varians 2 Sampel (F-test)", test_output = capture.output(print(res)),
        interpretation_text = get_interpretasi_text(output$interpret_var2()),
        plot_path = plot_path, var_info = var2_var_info()
      )
      temp_report_path <- file.path(tempdir(), "propvar_report.Rmd"); file.copy("propvar_report.Rmd", temp_report_path, overwrite = TRUE)
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$plot_var2 <- renderPlot({
    req(input$var2_var_num, input$var2_var_cat)
    df <- values$data
    if (input$var2_var_num %in% names(df) && input$var2_var_cat %in% names(df)) {
      ggplot(df, aes_string(x = input$var2_var_cat, y = input$var2_var_num)) +
        geom_boxplot(fill = "#fbd6e3", color = "#d65076", alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5, color = "#d65076") +
        labs(title = paste("Perbandingan Varians", input$var2_var_num, "berdasarkan", input$var2_var_cat),
             x = input$var2_var_cat, y = input$var2_var_num) +
        theme_minimal() +
        theme(legend.position = "none")
    }
  })
  
  # --- ANOVA ---
  anova_results <- eventReactive(input$run_anova, {
    req(input$anova_y, input$anova_group1, input$anova_type)
    catat_histori(paste("Uji ANOVA", input$anova_type, ": Y =", input$anova_y, ", Grup 1 =", input$anova_group1, if (!is.null(input$anova_group2)) paste(", Grup 2 =", input$anova_group2) else ""))
    df_anova <- values$data
    
    formula_str <- NULL
    plot_anova <- NULL
    model <- NULL
    p_value <- NA
    
    tryCatch({
      if (input$anova_type == "Satu Arah (One-Way)") {
        req(is.numeric(df_anova[[input$anova_y]]), is.factor(df_anova[[input$anova_group1]]) || is.character(df_anova[[input$anova_group1]]))
        formula_str <- paste(input$anova_y, "~", input$anova_group1)
        model <- aov(as.formula(formula_str), data = df_anova)
        plot_anova <- ggplot(df_anova, aes_string(x=input$anova_group1, y=input$anova_y, fill=input$anova_group1)) +
          geom_boxplot(alpha = 0.7, fill = "#fbd6e3", color = "#d65076") +
          geom_jitter(width = 0.2, alpha = 0.5, color = "#d65076") +
          labs(title = paste("Boxplot", input$anova_y, "berdasarkan", input$anova_group1)) +
          theme_minimal() +
          theme(legend.position = "none")
        p_value <- summary(model)[[1]]$`Pr(>F)`[1]
        
      } else if (input$anova_type == "Dua Arah (Two-Way)") {
        req(input$anova_group2)
        req(is.numeric(df_anova[[input$anova_y]]),
            is.factor(df_anova[[input$anova_group1]]) || is.character(df_anova[[input$anova_group1]]),
            is.factor(df_anova[[input$anova_group2]]) || is.character(df_anova[[input$anova_group2]]))
        
        df_anova[[input$anova_group1]] <- as.factor(df_anova[[input$anova_group1]])
        df_anova[[input$anova_group2]] <- as.factor(df_anova[[input$anova_group2]])
        
        if (nlevels(df_anova[[input$anova_group1]]) < 2 || nlevels(df_anova[[input$anova_group2]]) < 2) {
          showNotification("Variabel grup untuk ANOVA Dua Arah harus memiliki setidaknya dua kategori.", type = "warning")
          return(NULL)
        }
        formula_str <- paste(input$anova_y, "~", input$anova_group1, "*", input$anova_group2)
        model <- aov(as.formula(formula_str), data = df_anova)
        plot_anova <- ggplot(df_anova, aes_string(x=input$anova_group1, y=input$anova_y, fill=input$anova_group2)) +
          geom_boxplot(alpha = 0.7) +
          facet_wrap(as.formula(paste("~", input$anova_group2))) +
          labs(title = paste("Boxplot", input$anova_y, "berdasarkan", input$anova_group1, "dan", input$anova_group2)) +
          theme_minimal()
        
        anova_summary <- summary(model)[[1]]
        if (nrow(anova_summary) > 2 && !is.na(anova_summary$`Pr(>F)`[3])) { 
          p_value <- anova_summary$`Pr(>F)`[3] 
        } else if (nrow(anova_summary) > 1) {
          p_value <- anova_summary$`Pr(>F)`[1]
        }
      }
    }, error = function(e) {
      showNotification(paste("Error saat menjalankan ANOVA:", e$message), type = "error")
      return(NULL)
    })
    
    list(model=model, plot=plot_anova, formula=formula_str, p_value = p_value, type = input$anova_type,
         y_var = input$anova_y, group1_var = input$anova_group1, group2_var = input$anova_group2)
  })
  output$hasil_anova <- renderPrint({
    res <- anova_results()
    if (!is.null(res) && !is.null(res$model)) summary(res$model)
  })
  output$plot_anova <- renderPlot({
    res <- anova_results()
    if (!is.null(res) && !is.null(res$plot)) res$plot
  })
  output$interpret_anova <- renderUI({
    res <- anova_results()
    if (!is.null(res) && !is.null(res$model)) {
      if (res$type == "Satu Arah (One-Way)") {
        HTML(paste0("<b>Hipotesis Nol (H\u2080):</b> Tidak ada perbedaan rata-rata antara grup.<br>",
                    "<b>Hipotesis Alternatif (H\u2081):</b> Ada perbedaan rata-rata antara setidaknya satu pasang grup.<br>",
                    interpret_pval(res$p_value)))
      } else { # Two-Way ANOVA
        anova_summary <- summary(res$model)[[1]]
        interpretation_text <- ""
        
        if (nrow(anova_summary) > 2 && !is.na(anova_summary$`Pr(>F)`[3])) {
          p_interaksi <- anova_summary$`Pr(>F)`[3]
          interpretation_text <- paste0("<b>Interaksi (", res$group1_var, " * ", res$group2_var, "):</b> ", interpret_pval(p_interaksi))
          if (p_interaksi <= 0.05) {
            interpretation_text <- paste0(interpretation_text, "<br>Pengaruh satu faktor tergantung pada level faktor lainnya.")
          } else {
            interpretation_text <- paste0(interpretation_text, "<br>Tidak ada interaksi signifikan antar faktor.")
          }
        }
        
        if (nrow(anova_summary) > 0 && !is.na(anova_summary$`Pr(>F)`[1])) {
          p_group1 <- anova_summary$`Pr(>F)`[1]
          interpretation_text <- paste0(interpretation_text, "<br><b>Efek Utama (", res$group1_var, "):</b> ", interpret_pval(p_group1))
          if (p_group1 <= 0.05) {
            interpretation_text <- paste0(interpretation_text, "<br>Ada efek utama signifikan dari ", res$group1_var, ".")
          } else {
            interpretation_text <- paste0(interpretation_text, "<br>Tidak ada efek utama signifikan dari ", res$group1_var, ".")
          }
        }
        
        if (nrow(anova_summary) > 1 && !is.na(anova_summary$`Pr(>F)`[2])) {
          p_group2 <- anova_summary$`Pr(>F)`[2]
          interpretation_text <- paste0(interpretation_text, "<br><b>Efek Utama (", res$group2_var, "):</b> ", interpret_pval(p_group2))
          if (p_group2 <= 0.05) {
            interpretation_text <- paste0(interpretation_text, "<br>Ada efek utama signifikan dari ", res$group2_var, ".")
          } else {
            interpretation_text <- paste0(interpretation_text, "<br>Tidak ada efek utama signifikan dari ", res$group2_var, ".")
          }
        }
        HTML(interpretation_text)
      }
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  
  output$unduh_anova_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_anova_report), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_anova_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_anova_report), "PDF" = "pdf_document", "WORD" = "word_document")
      res <- anova_results()
      req(res, cancelOutput = TRUE)
      
      plot_path <- tempfile(fileext = ".png")
      if(!is.null(res$plot)) ggsave(plot_path, plot = res$plot, device = "png")
      
      params <- list(
        formula = res$formula, test_output = capture.output(summary(res$model)),
        interpretation_text = get_interpretasi_text(output$interpret_anova()),
        plot_path = plot_path
      )
      
      temp_report_path <- file.path(tempdir(), "anova_report.Rmd")
      file.copy("anova_report.Rmd", temp_report_path, overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  # --- Regresi Linear Berganda ---
  reg_model <- eventReactive(input$run_regresi, {
    req(input$regresi_y, input$regresi_x)
    catat_histori(paste("Regresi Linear Berganda: Y =", input$regresi_y, ", X =", paste(input$regresi_x, collapse = ", ")))
    if (length(input$regresi_x) == 0) {
      showNotification("Pilih setidaknya satu variabel independen.", type = "warning")
      return(NULL)
    }
  
    selected_cols <- unique(c(input$regresi_y, input$regresi_x))
    df_reg <- values$data %>% select(all_of(selected_cols)) %>% na.omit()
    
    if (nrow(df_reg) == 0) {
      showNotification("Data kosong setelah menghilangkan baris dengan nilai hilang (NA) pada variabel yang dipilih.", type = "warning")
      return(NULL)
    }
    
    formula_str <- paste(input$regresi_y, "~", paste(input$regresi_x, collapse = " + "))
    tryCatch({
      lm(as.formula(formula_str), data = df_reg)
    }, error = function(e) {
      showNotification(paste("Error saat menjalankan model regresi:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$hasil_regresi <- renderPrint({
    model <- reg_model()
    if (!is.null(model)) summary(model)
  })
  
  output$interpret_regresi <- renderUI({
    model_summary <- summary(reg_model())
    if (is.null(model_summary)) return(HTML("<p>Tidak ada hasil interpretasi.</p>"))
    
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    f_statistic <- model_summary$fstatistic
    f_pvalue <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
    
    overall_interpret <- paste0(
      "<p><b>R-squared:</b> ", sprintf("%.3f", r_squared), " (Adjusted R-squared: ", sprintf("%.3f", adj_r_squared), "). Ini menunjukkan proporsi variabilitas variabel dependen (", input$regresi_y, ") yang dapat dijelaskan oleh model.</p>",
      "<p><b>Uji F Model Keseluruhan:</b> ", interpret_pval(f_pvalue),
      " Ini menunjukkan apakah setidaknya satu variabel independen secara signifikan memprediksi variabel dependen.</p>"
    )
    
    coeff_interpret <- "<h4>Interpretasi Koefisien:</h4><ul>"
    coefs <- as.data.frame(model_summary$coefficients)
    names(coefs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    
    for (i in 1:nrow(coefs)) {
      var_name <- rownames(coefs)[i]
      estimate <- sprintf("%.3f", coefs$Estimate[i])
      p_val_coeff <- coefs$`Pr(>|t|)`[i]
      
      if (var_name == "(Intercept)") {
        coeff_interpret <- paste0(coeff_interpret, "<li><b>Intercept:</b> ", estimate, ". Ini adalah nilai rata-rata dari ", input$regresi_y, " ketika semua variabel independen bernilai nol.</li>")
      } else {
        coeff_interpret <- paste0(coeff_interpret, "<li><b>", var_name, ":</b> ", estimate, ". Ini menunjukkan perubahan rata-rata pada ", input$regresi_y, " untuk setiap peningkatan satu unit pada ", var_name, ", dengan variabel lain konstan. ", interpret_pval(p_val_coeff), "</li>")
      }
    }
    coeff_interpret <- paste0(coeff_interpret, "</ul>")
    
    HTML(paste0("<h3>Interpretasi Model Regresi:</h3>", overall_interpret, coeff_interpret))
  })
  
  output$plot_asumsi_regresi <- renderPlot({
    model <- reg_model()
    if (!is.null(model)) {
      par(mfrow = c(2, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 3, 2) + 0.1) 
      plot(model)
      mtext("Plot Diagnostik Regresi", side = 3, line = -1.5, outer = TRUE, cex = 1.5, font = 2)
      par(mfrow = c(1, 1)) 
    }
  })
  
  output$asumsi_regresi_results <- renderUI({
    model <- reg_model()
    if (is.null(model)) return(HTML("<p>Tidak ada hasil uji asumsi.</p>"))
    
    shapiro_res <- NULL
    bp_res <- NULL
    dw_res <- NULL
    
    tryCatch({
      residuals_model <- residuals(model)
      if (length(residuals_model) > 5000) { 
        shapiro_res <- list(p.value = ks.test(scale(residuals_model), "pnorm")$p.value, method = "Kolmogorov-Smirnov")
        shapiro_text <- paste0("<b>Normalitas Residual (Kolmogorov-Smirnov):</b> ", interpret_pval(shapiro_res$p.value))
      } else if (length(residuals_model) >= 3) {
        shapiro_res <- shapiro.test(residuals_model)
        shapiro_text <- paste0("<b>Normalitas Residual (Shapiro-Wilk):</b> ", interpret_pval(shapiro_res$p.value))
      } else {
        shapiro_text <- "<b>Normalitas Residual:</b> Tidak cukup data untuk uji."
      }
      
      # Homoskedastisitas
      bp_res <- ncvTest(model)
      bp_text <- paste0("<b>Homoskedastisitas (Breusch-Pagan):</b> ", interpret_pval(bp_res$p))
      
      # Non-Autokorelasi
      dw_res <- durbinWatsonTest(model)
      dw_text <- paste0("<b>Non-Autokorelasi (Durbin-Watson):</b> ", interpret_pval(dw_res$p))
      
    }, error = function(e) {
      showNotification(paste("Error saat menjalankan uji asumsi:", e$message), type = "error")
      return(NULL)
    })
    
    HTML(paste(shapiro_text, bp_text, dw_text, sep = "<br>"))
  })
  
  
  output$unduh_regresi_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_regresi_report), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0('laporan_regresi_', Sys.Date(), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_regresi_report), "PDF" = "pdf_document", "WORD" = "word_document")
      model <- reg_model()
      req(model, cancelOutput = TRUE)
      
      plot_path <- tempfile(fileext = ".png")
      png(plot_path, width = 800, height = 800); par(mfrow = c(2, 2)); plot(model); dev.off()
      
      params <- list(
        formula = formula(model), summary_output = capture.output(summary(model)),
        assumption_text = get_interpretasi_text(output$asumsi_regresi_results()),
        interpretation_text = get_interpretasi_text(output$interpret_regresi()),
        plot_path = plot_path, y_var = input$regresi_y, x_vars = input$regresi_x
      )
      
      temp_report_path <- file.path(tempdir(), "rlb_report.Rmd")
      file.copy("rlb_report.Rmd", temp_report_path, overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
  # --- Unduh Gabungan ---
  output$tampilan_histori <- renderPrint({
    log_data <- histori_sesi()
    if(length(log_data) == 0) {
      "Belum ada aktivitas analisis yang tercatat dalam sesi ini."
    } else {
      cat(paste(log_data, collapse = "\n"))
    }
  })
  
  output$unduh_gabungan <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_gabungan_report), "PDF" = ".pdf", "WORD" = ".docx", ".pdf")
      paste0("Histori_Sesi_Analisis_", format(Sys.time(), "%Y%m%d"), ext)
    },
    content = function(file) {
      format <- switch(toupper(input$format_gabungan_report), "PDF" = "pdf_document", "WORD" = "word_document")
      log_data <- histori_sesi()
      summary_info <- paste(
        "**Tanggal Sesi:**", format(Sys.Date(), "%d %B %Y"), "\n\n",
        "**Total Aktivitas:**", length(log_data), "aktivitas"
      )
      
      params <- list(
        log_data = log_data, summary_info = summary_info, plots_list = NULL
      )
      
      temp_report_path <- file.path(tempdir(), "gabungan_report.Rmd")
      file.copy("gabungan_report.Rmd", temp_report_path, overwrite = TRUE)
      
      rmarkdown::render(
        input = temp_report_path, output_file = file, output_format = format,
        params = params, envir = new.env(parent = globalenv())
      )
    }
  )
  
})