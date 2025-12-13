# ==============================================================================
# MÓDULO: Participación Electoral - REFACTORIZADO
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_participacion_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # --- Panel Lateral ---
      column(
        width = 3,
        box(
          title = "Configuración", status = "primary", solidHeader = TRUE, width = 12,
          p("Visualice los niveles de participación, lista nominal o votos totales."),
          
          shiny::selectInput(ns("tipo_dato"), 
                             "Variable a visualizar:",
                             choices =  c("Lista Nominal", "Tasa de Participación" = "participacion", "Votos Totales" = "votos"),
                             selected = "Lista Nominal"),
          
          hr(),
          
          # Estadísticas rápidas (ValueBox simplificado)
          uiOutput(ns("info_resumen")),
          
          hr(),
          
          downloadBttn(
            outputId = ns("descargar_data"), 
            label = "Descargar Datos", 
            style = "simple", color = "success", size = "sm", block = TRUE, icon = icon("download")
          )
        )
      ),
      
      # --- Mapa ---
      column(
        width = 9,
        box(
          width = 12, title = "Mapa de Calor Electoral", status = "info", solidHeader = TRUE,
          
          div(style = "position: relative;",
              leafletOutput(ns("mapa_participacion"), height = "80vh"),
              
              # Botón flotante pantalla completa
              div(style = "position: absolute; top: 10px; right: 10px; z-index: 1000;",
                  actionButton(ns("btn_fullscreen"), label = NULL, icon = icon("expand"), 
                               class = "btn-light btn-sm", style = "box-shadow: 0 0 5px rgba(0,0,0,0.3);")
              )
          )
        )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_participacion_server <- function(id, secciones_base) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Preparación de Geometría (Optimización) -------------------------------
    # Solo se recalcula si cambian las secciones filtradas, NO la variable
    geo_base <- reactive({
      req(secciones_base())
      shp <- secciones_base()
      
      # Detección dinámica de nombres de columnas
      cols <- names(shp)
      col_sec <- intersect(cols, c("SECCION", "Seccion", "CLAVE", "ID"))[1]
      col_nom <- intersect(cols, c("NOMBRE", "MUNICIPIO", "NOM_MUN", "NOMGEO"))[1]
      
      if (!is.na(col_sec)) {
        shp %>%
          transmute(
            SECCION_JOIN = as.character(.data[[col_sec]]), # Estandarizamos ID
            NOMBRE_SHOW  = if(!is.na(col_nom)) .data[[col_nom]] else "Municipio" # Estandarizamos Nombre
          ) %>%
          st_transform(crs = 4326)
      } else {
        NULL 
      }
    })
    
    # 2. Preparación de Datos (Join + Selección) -------------------------------
    datos_mapa <- reactive({
      req(geo_base(), input$tipo_dato)
      
      geo <- geo_base()
      
      # 'participacion' es Global.
      # Aseguramos que sea dataframe simple y convertimos columna de enlace
      df_part <- participacion %>%
        as.data.frame() %>%
        mutate(SECCION = as.character(SECCION)) # Asumimos que en el CSV/RDS se llama SECCION
      
      # Mapeo del input a la columna real del dataframe
      col_variable <- case_when(
        input$tipo_dato == "Lista Nominal" ~ "lista",
        input$tipo_dato == "participacion" ~ "participacion", 
        input$tipo_dato == "votos" ~ "votos",
        TRUE ~ "lista"
      )
      
      # Verificar que la columna exista en el dataframe global
      if(!col_variable %in% names(df_part)) return(NULL)
      
      # Join y limpieza
      geo %>%
        left_join(df_part, by = c("SECCION_JOIN" = "SECCION")) %>%
        mutate(
          valor_raw = .data[[col_variable]],
          # Limpieza de infinitos o NaNs
          valor_mapa = ifelse(is.finite(valor_raw), valor_raw, 0)
        )
    })
    
    # 3. Renderizado del Mapa --------------------------------------------------
    output$mapa_participacion <- renderLeaflet({
      dta <- datos_mapa()
      shiny::validate(need(!is.null(dta) && nrow(dta) > 0, "No hay datos de participación disponibles para esta zona."))
      # Definición de etiquetas y formatos según el tipo de dato
      es_porcentaje <- input$tipo_dato == "participacion"
      
      # Paleta de colores
      # Usamos 'YlOrRd' (Amarillo-Naranja-Rojo) que es intuitivo para calor/intensidad
      pal <- colorNumeric(palette = "YlOrRd", domain = dta$valor_mapa, na.color = "#808080")
      
      # Texto del Popup formateado
      txt_valor <- if(es_porcentaje) {
        paste0(format(round(dta$valor_mapa * 100, 1), nsmall = 1), "%")
      } else {
        format(dta$valor_mapa, big.mark = ",")
      }
      
      popup_html <- sprintf(
        "<b>Municipio: </b>%s<br>
         <b>Sección: </b>%s<hr style='margin:4px 0;'>
         <b>%s: </b>%s",
        dta$NOMBRE_SHOW,
        dta$SECCION_JOIN,
        input$tipo_dato,
        txt_valor
      )
      
      # Título de leyenda
      titulo_leyenda <- if(es_porcentaje) "Participación (%)" else input$tipo_dato
      
      # Transformación para la leyenda (si es % se muestra 0-100 en vez de 0-1)
      lab_trans <- if(es_porcentaje) function(x) x * 100 else function(x) x
      sufijo    <- if(es_porcentaje) "%" else ""
      
      leaflet(dta) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(valor_mapa),
          color = "#444",      # Borde gris oscuro
          weight = 1,          # Borde delgado
          opacity = 1,
          fillOpacity = 0.8,
          popup = popup_html,
          highlightOptions = highlightOptions(
            weight = 2, color = "#00FFFF", fillOpacity = 0.9, bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = dta$valor_mapa,
          title = titulo_leyenda,
          labFormat = labelFormat(transform = lab_trans, suffix = sufijo)
        )
    })
    
    # 4. Info Resumen (UI Dinámica) --------------------------------------------
    output$info_resumen <- renderUI({
      req(datos_mapa())
      dta <- datos_mapa()
      
      promedio <- mean(dta$valor_mapa, na.rm = TRUE)
      
      if(input$tipo_dato == "participacion") {
        valor_txt <- paste0(round(promedio * 100, 1), "%")
        sub_txt <- "Participación Promedio"
      } else {
        valor_txt <- scales::comma(sum(dta$valor_mapa, na.rm=T))
        sub_txt <- paste("Total", input$tipo_dato)
      }
      
      div(
        h3(valor_txt, style = "color: #0073b7; margin-bottom: 0;"),
        span(sub_txt, style = "color: #666;")
      )
    })
    
    # 5. Descarga --------------------------------------------------------------
    output$descargar_data <- downloadHandler(
      filename = function() { paste0("participacion_", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(st_drop_geometry(datos_mapa()), file, row.names = FALSE)
      }
    )
    
    # 6. Fullscreen ------------------------------------------------------------
    observeEvent(input$btn_fullscreen, {
      shinyjs::runjs(sprintf(
        "var map = document.getElementById('%s');
         if (!document.fullscreenElement) { map.requestFullscreen(); } 
         else { document.exitFullscreen(); }", 
        ns("mapa_participacion")
      ))
    })
  })
}