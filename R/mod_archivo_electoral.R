# ==============================================================================
# MÓDULO: Archivo Electoral (Base de Datos) - REFACTORIZADO
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_archivo_electoral_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Archivo Electoral", collapsible = TRUE, collapsed = FALSE, width = 12, status = "success", solidHeader = TRUE,
        p("Genere tablas con la cantidad de votos por partido. Utilice los filtros para seleccionar las secciones, partidos, elecciones y años de su interés.")
      ),
      # --- Filtros ---
      box(
        title = "Filtros", status = "primary", solidHeader = TRUE, width = 3,
        
        shinyWidgets::pickerInput(
          inputId = ns("pik_secc_archivo"), 
          label = "Seleccione secciones:", 
          options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), 
          choices = NULL, # Se actualiza en server
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("pik_partido_archivo"), 
          label = "Seleccione partidos:", 
          options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), 
          choices = unique(cant_votos_nl$partido), 
          selected = unique(cant_votos_nl$partido), 
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("pik_elecc_archivo"), 
          label = "Seleccione elección:", 
          options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), 
          choices = unique(cant_votos_nl$eleccion), 
          selected = unique(cant_votos_nl$eleccion), 
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("pik_año_archivo"), 
          label = "Seleccione años:", 
          multiple = TRUE, 
          options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), 
          choices = sort(unique(cant_votos_nl$año)), 
          selected = unique(cant_votos_nl$año)
        )
      ),
      # --- Tablas ---
      tabBox(
        width = 9,
        title = "Resultados",
        tabPanel("Tabla Agregada", icon = icon("calculator"),
                 div(style = "display:flex; justify-content: flex-end; padding-bottom:10px;",
                     downloadBttn(outputId = ns("dw_archivo_csv_archivo"), label = "Descargar CSV", size = "xs", icon = icon("file-csv"), color = "success", style = "jelly")
                 ),
                 DT::dataTableOutput(ns("dt_agregada_archivo"))
        ),
        tabPanel("Tabla Desagregada", icon = icon("list"),
                 div(style = "display:flex; justify-content: flex-end; padding-bottom:10px;",
                     downloadBttn(outputId = ns("dw_archivo_csv_2_archivo"), label = "Descargar CSV", size = "xs", icon = icon("file-csv"), color = "success", style = "jelly")
                 ),
                 DT::dataTableOutput(ns("dt_desagregada_archivo"))
        )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_archivo_electoral_server <- function(id, secciones_reactivas, input_filtro_tipo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Actualizar Filtro de Secciones ----------------------------------------
    observeEvent(secciones_reactivas(), {
      req(secciones_reactivas())
      # Detectamos la columna de sección dinámicamente
      shp <- secciones_reactivas()
      col_sec <- intersect(names(shp), c("SECCION", "Seccion", "CLAVE"))[1]
      
      if(!is.na(col_sec)) {
        secciones_disponibles <- shp[[col_sec]]
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "pik_secc_archivo",
          choices = sort(unique(secciones_disponibles)),
          selected = unique(secciones_disponibles)
        )
      }
    })
    
    # 2. Construcción de Datos Desagregados (BLINDADO) -------------------------
    query_archivo <- reactive({
      req(input$pik_secc_archivo, input$pik_partido_archivo, input$pik_elecc_archivo, input$pik_año_archivo, secciones_reactivas())
      
      # A) Preparar Geometría (Metadata)
      # Buscamos nombres de columnas de forma segura
      geo_raw <- sf::st_drop_geometry(secciones_reactivas())
      nombres_cols <- names(geo_raw)
      
      # Detectar Municipio
      col_mun <- intersect(nombres_cols, c("NOMBRE", "MUNICIPIO", "NOM_MUN", "NOMGEO"))[1]
      # Detectar Sección
      col_sec <- intersect(nombres_cols, c("SECCION", "Seccion", "CLAVE"))[1]
      # Detectar Distrito Federal (Opcional)
      col_df <- intersect(nombres_cols, c("DISTRITO", "DTO_FED", "ID_DISTRITO"))[1]
      # Detectar Distrito Local (Opcional)
      col_dl <- intersect(nombres_cols, c("DISTRITO_L", "DTO_LOC", "L_DISTRITO"))[1]
      
      # Crear tabla limpia de referencia geográfica
      data_geo_clean <- geo_raw %>%
        transmute(
          # Si no encuentra columna, rellena con NA o valor por defecto
          seccion_join = as.character(if(!is.na(col_sec)) .data[[col_sec]] else NA),
          Municipio_Ref = if(!is.na(col_mun)) .data[[col_mun]] else "Desconocido",
          Distrito_F_Ref = if(!is.na(col_df)) .data[[col_df]] else NA,
          Distrito_L_Ref = if(!is.na(col_dl)) .data[[col_dl]] else NA
        )
      
      # B) Filtrar Votos (Global)
      votos_filtrados <- cant_votos_nl %>%
        filter(
          partido %in% input$pik_partido_archivo,
          eleccion %in% input$pik_elecc_archivo,
          año %in% input$pik_año_archivo,
          seccion %in% input$pik_secc_archivo
        ) %>%
        mutate(seccion = as.character(seccion))
      
      shiny::validate(need(nrow(votos_filtrados) > 0, "No se encontraron votos con los filtros seleccionados."))
      
      # C) Join Final
      data_final <- votos_filtrados %>%
        left_join(data_geo_clean, by = c("seccion" = "seccion_join")) %>%
        select(
          "Sección" = seccion, 
          "Distrito Federal" = Distrito_F_Ref, 
          "Distrito Local" = Distrito_L_Ref, 
          "Municipio" = Municipio_Ref,
          "Partido" = partido, 
          "Elección" = eleccion, 
          "Votos" = votos, 
          "Año" = año
        )
      
      return(data_final)
    })
    
    # 3. Construcción de Datos Agregados ---------------------------------------
    query_archivo_agg <- reactive({
      req(query_archivo())
      data_raw <- query_archivo()
      
      # Validación extra por seguridad
      if (nrow(data_raw) == 0) return(NULL)
      
      tipo <- input_filtro_tipo() 
      if (is.null(tipo)) tipo <- "Ninguno"
      
      # Agrupación dinámica
      # Nota: Usamos los nombres "bonitos" definidos en el select del paso anterior
      res <- tryCatch({
        if (tipo == "Municipio") {
          data_raw %>% group_by(Municipio, Partido, Elección, Año) %>% summarise(`Votos totales` = sum(Votos, na.rm = T), .groups = 'drop')
        } else if (tipo == "DFederal") {
          data_raw %>% group_by(`Distrito Federal`, Partido, Elección, Año) %>% summarise(`Votos totales` = sum(Votos, na.rm = T), .groups = 'drop')
        } else if (tipo == "DLocal") {
          data_raw %>% group_by(`Distrito Local`, Partido, Elección, Año) %>% summarise(`Votos totales` = sum(Votos, na.rm = T), .groups = 'drop')
        } else {
          data_raw %>% group_by(Partido, Elección, Año) %>% summarise(`Votos totales` = sum(Votos, na.rm = T), .groups = 'drop')
        }
      }, error = function(e) {
        # Fallback si falla la agrupación (ej. columna llena de NAs)
        data_raw %>% group_by(Partido, Elección, Año) %>% summarise(`Votos totales` = sum(Votos, na.rm = T), .groups = 'drop')
      })
      
      return(res)
    })
    
    # 4. Renders de Tablas -----------------------------------------------------
    dt_options <- list(
      searching = TRUE, 
      pageLength = 10,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'excel'),
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
    )
    
    output$dt_agregada_archivo <- DT::renderDataTable({
      dta <- query_archivo_agg()
      shiny::validate(need(!is.null(dta) && nrow(dta) > 0, "No hay datos para mostrar en la tabla agregada."))
      
      datatable(dta, style = 'bootstrap4', class = 'cell-border stripe',
                extensions = c("Buttons"), selection = "single", options = dt_options)
    })
    
    output$dt_desagregada_archivo <- DT::renderDataTable({
      dta <- query_archivo()
      shiny::validate(need(!is.null(dta) && nrow(dta) > 0, "No hay datos para mostrar en la tabla desagregada."))
      
      datatable(dta, style = 'bootstrap4', class = 'cell-border stripe',
                extensions = c("Buttons"), selection = "single", options = dt_options)
    })
    
    # 5. Descargas -------------------------------------------------------------
    output$dw_archivo_csv_archivo <- downloadHandler(
      filename = function() { paste0("electoral_agregado_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(query_archivo_agg(), file, row.names = FALSE) }
    )
    
    output$dw_archivo_csv_2_archivo <- downloadHandler(
      filename = function() { paste0("electoral_desagregado_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(query_archivo(), file, row.names = FALSE) }
    )
    
  })
}