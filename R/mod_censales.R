# ==============================================================================
# MÓDULO: Datos Poblacionales (Censales) - REFACTORIZADO
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_censales_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = "Datos Poblacionales por Sección", 
      collapsible = TRUE, collapsed = FALSE, 
      width = 12, status = "success", solidHeader = TRUE,
      p("Seleccione una variable demográfica en el panel de control para visualizar su distribución geográfica.")
    ),
    
    fluidRow(
      # --- Panel de Control ---
      column(
        width = 3,
        box(
          title = "Controles", status = "info", solidHeader = TRUE, width = 12,
          
          shiny::selectInput(ns("censo_interes"), 
                             "Elige la variable de interés:", 
                             choices = chs_censales, # Asumiendo variable Global
                             selected = chs_censales[1]),
          hr(),
          downloadBttn(
            outputId = ns("dw_censal_csv_censales"), 
            label = "Descargar datos", 
            size = "sm", icon = icon("download"), color = "primary", style = "simple", block = TRUE
          )
        )
      ),
      
      # --- Visualización ---
      column(
        width = 9,
        tabBox(
          id = ns("censales_tabset"), width = 12,
          
          tabPanel(
            title = "Mapa de Distribución", icon = icon("map-marked-alt"),
            # Contenedor con altura relativa para adaptarse mejor
            leafletOutput(ns("mapa_censales"), height = "80vh"),
            div(style = "position: absolute; bottom: 25px; left: 25px; z-index: 1000;",
                shiny::actionButton(ns("mapFullscreen"), "Pantalla Completa", icon = icon("expand-arrows-alt"), class = "btn-sm btn-light")
            )
          ),
          
          tabPanel(
            title = "Tabla de Datos", icon = icon("table"),
            DT::DTOutput(ns("dt_query_censales"))
          )
        )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_censales_server <- function(id, secciones_reactivas) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Configuración Global ---
    vars_porcentaje <- c("POBFEM", "POBMAS", "PHOG_IND", "POB_AFRO", 
                         "PCON_DISC", "POCUPADA", "PDESOCUP")
    
    # 1. Preparación de Geometría
    geo_base <- reactive({
      req(secciones_reactivas())
      # Transformamos y aseguramos que SECCION sea caracter
      secciones_reactivas() %>%
        mutate(SECCION = as.character(SECCION)) %>%
        st_transform(crs = 4326)
    })
    
    # 2. Preparación de Datos (CORREGIDO)
    datos_mapa_listos <- reactive({
      req(geo_base(), input$censo_interes)
      
      shp <- geo_base()
      
      # CORRECCIÓN: Quitamos 'NOMBRE' del select. 
      # Asumimos que el nombre del municipio ya viene en 'shp' (el shapefile).
      datos_valores <- data_secc_cpv2020 %>%
        mutate(SECCION = as.character(SECCION)) %>%
        filter(SECCION %in% shp$SECCION) %>%
        select(SECCION, POBTOT, any_of("GRAPROES_NIVEL"), 
               VariableInteres = all_of(input$censo_interes))
      
      # Join: Unimos los datos al mapa
      shp_joined <- shp %>%
        left_join(datos_valores, by = "SECCION")
      
      # Cálculos finales
      shp_joined %>%
        mutate(
          POBTOT_safe = ifelse(is.na(POBTOT) | POBTOT == 0, 1, POBTOT),
          raw_value   = as.numeric(VariableInteres),
          
          valor_mapa = case_when(
            input$censo_interes %in% vars_porcentaje ~ (raw_value / POBTOT_safe) * 100,
            TRUE ~ raw_value
          ),
          valor_mapa = coalesce(valor_mapa, 0), 
          
          texto_valor = case_when(
            input$censo_interes %in% vars_porcentaje ~ paste0(format(round(valor_mapa, 2), nsmall = 2), "%"),
            TRUE ~ format(round(valor_mapa, 0), big.mark = ",")
          )
        )
    })
    
    # 3. Renderizado del Mapa (CORREGIDO Y BLINDADO)
    output$mapa_censales <- renderLeaflet({
      # Usamos req() para evitar errores si el reactivo aún no está listo
      req(datos_mapa_listos())
      dta <- datos_mapa_listos()
      
      # VALIDATE SEGURO: Verificamos explícitamente que no sea NULL y tenga filas
      if (is.null(dta) || nrow(dta) == 0) {
        shiny::validate("No hay datos disponibles para la selección actual.")
      }
      
      # Detectar columna de nombre de municipio (Para que no falle el popup)
      # Busca la primera columna que parezca nombre, si no, usa "Desconocido"
      posibles_nombres <- c("NOMBRE", "MUNICIPIO", "NOM_MUN", "NOMGEO")
      col_nombre <- intersect(names(dta), posibles_nombres)[1]
      
      if (is.na(col_nombre)) {
        # Si no encuentra columna de nombre, creamos una dummy
        dta$nombre_mostrar <- "Municipio" 
      } else {
        dta$nombre_mostrar <- dta[[col_nombre]]
      }
      
      # Etiqueta base
      label_base <- names(chs_censales)[which(chs_censales == input$censo_interes)]
      if(length(label_base) == 0) label_base <- input$censo_interes
      
      # Paleta
      pal <- colorNumeric(palette = "viridis", domain = dta$valor_mapa)
      
      # Popup seguro
      txt_escolaridad <- if ("GRAPROES_NIVEL" %in% names(dta)) {
        paste0("<b>Nivel promedio: </b>", dta$GRAPROES_NIVEL, "<br>")
      } else { "" }
      
      popup_content <- sprintf(
        "<b>Municipio: </b>%s<br>
         <b>Sección: </b>%s<hr style='margin:5px 0;'>
         <b>Población Total: </b>%s<br>
         %s
         <b>%s: </b>%s",
        dta$nombre_mostrar, # Usamos la columna detectada
        dta$SECCION,
        format(dta$POBTOT, big.mark = ","),
        ifelse(input$censo_interes == "GRAPROES", txt_escolaridad, ""),
        label_base,
        dta$texto_valor
      )
      
      titulo_leyenda <- if (input$censo_interes %in% vars_porcentaje) paste0(label_base, " (%)") else label_base
      
      leaflet(dta) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(valor_mapa),
          color = "#444444",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.7,
          popup = popup_content,
          highlightOptions = highlightOptions(
            weight = 2, color = "#000", fillOpacity = 0.9, bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~valor_mapa,
          title = titulo_leyenda
        )
    })
    
    # 4. Tabla de Datos
    output$dt_query_censales <- DT::renderDataTable({
      req(datos_mapa_listos())
      
      # Detectar columna nombre igual que arriba
      dta <- datos_mapa_listos()
      col_nombre <- intersect(names(dta), c("NOMBRE", "MUNICIPIO", "NOM_MUN", "NOMGEO"))[1]
      col_nombre_final <- if(is.na(col_nombre)) "SECCION" else col_nombre # Fallback
      
      dta %>%
        st_drop_geometry() %>%
        select(
          any_of(col_nombre_final), # Selecciona el nombre si existe
          Sección = SECCION,
          `Población Total` = POBTOT,
          Valor = raw_value,
          `Valor (Mapa)` = valor_mapa
        ) %>%
        mutate(Indicador = names(chs_censales)[which(chs_censales == input$censo_interes)]) %>%
        relocate(Indicador, .before = Valor) %>%
        DT::datatable(
          style = 'bootstrap4',
          class = 'cell-border stripe',
          extensions = 'Buttons',
          options = list(dom = 'Bfrtip', buttons = c('excel'), scrollX = TRUE)
        )
    })
    
    # 5. Descarga
    output$dw_censal_csv_censales <- downloadHandler(
      filename = function() { paste0("censales_", Sys.Date(), ".csv") },
      content = function(file) { 
        req(datos_mapa_listos())
        write.csv(st_drop_geometry(datos_mapa_listos()), file, row.names=FALSE) 
      }
    )
    
    # 6. Fullscreen
    observeEvent(input$mapFullscreen, {
      shinyjs::runjs(sprintf(
        "var map = document.getElementById('%s');
         if (!document.fullscreenElement) { map.requestFullscreen(); } 
         else { document.exitFullscreen(); }", 
        ns("mapa_censales")
      ))
    })
  })
}