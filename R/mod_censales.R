# ==============================================================================
# MÓDULO: Datos Poblacionales (Censales)
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_censales_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      title = "Datos Poblacionales por Sección", 
      collapsible = TRUE, collapsed = FALSE, 
      width = 12, status = "success", solidHeader = TRUE,
      p("Seleccione una variable demográfica en el panel de control para visualizar su distribución geográfica en el mapa y explorar los datos detallados en la tabla.")
    ),
    
    fluidRow(
      # --- Panel de Control ---
      column(
        width = 3,
        box(
          title = "Controles", status = "info", solidHeader = TRUE, width = 12,
          
          shiny::selectInput(ns("censo_interes"), 
                             "Elige la variable de interés:", 
                             choices = chs_censales, # Variable Global
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
            leafletOutput(ns("mapa_censales"), height = "80vh"),
            shiny::actionButton(ns("mapFullscreen"), "Pantalla Completa", icon = icon("expand-arrows-alt"))
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
    
    # 1. Preparar Datos para Tabla (Formato Largo) -----------------------------
    query_censales <- reactive({
      req(input$censo_interes)
      
      # data_secc_cpv2020 es Global
      # Filtramos primero las columnas de interés y luego pivotamos
      dta_filtrada <- data_secc_cpv2020 %>%
        select(SECCION, any_of(input$censo_interes)) # Seleccionar solo la necesaria + SECCION
      
      # Nota: Para la tabla completa que el usuario quiere ver, 
      # quizás quiera ver SOLO la variable seleccionada filtrada por las secciones activas
      
      secciones_ids <- secciones_reactivas()$SECCION
      
      # Retornamos la fila correspondiente a la variable seleccionada
      # Usamos el dataframe original 'data_secc_cpv2020' completo para pivotar
      data_secc_cpv2020 %>%
        filter(SECCION %in% secciones_ids) %>%
        select(SECCION, Valor = all_of(input$censo_interes)) %>%
        mutate(Indicador = names(chs_censales)[which(chs_censales == input$censo_interes)]) %>%
        relocate(Indicador, .before = Valor)
    })
    
    # 2. Renderizado del Mapa --------------------------------------------------
    output$mapa_censales <- renderLeaflet({
      req(input$censo_interes)
      
      # Variables que deben mostrarse como porcentaje del total de población
      vars_porcentaje <- c("POBFEM", "POBMAS", "PHOG_IND", "POB_AFRO", 
                           "PCON_DISC", "POCUPADA", "PDESOCUP")
      
      # Preparar datos: Unir geometría con datos censales (formato ancho)
      # Convertir SECCION a formato padding (0001) para el join si es necesario, 
      # o asegurarse que coinciden los tipos (character vs numeric)
      
      # data_secc_cpv2020 tiene SECCION como numeric o character, ajustamos:
      datos_censales_procesados <- data_secc_cpv2020 %>%
        mutate(SECCION = as.character(SECCION))
      
      shp_dta <- secciones_reactivas() %>% 
        mutate(SECCION = as.character(SECCION)) %>%
        st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
        left_join(datos_censales_procesados, by = "SECCION")
      
      validate(need(nrow(shp_dta) > 0, "No hay datos para mostrar."))
      
      # Crear etiquetas dinámicas
      label_base <- names(chs_censales)[which(chs_censales == input$censo_interes)]
      if(length(label_base) == 0) label_base <- input$censo_interes
      
      # Procesar valores para el mapa
      shp_dta_map <- shp_dta %>%
        mutate(
          raw_value = .data[[input$censo_interes]],
          
          # Cálculo del valor a mapear (Porcentaje o Absoluto)
          valor_mapa = if (input$censo_interes %in% vars_porcentaje) {
            ifelse(POBTOT > 0, (raw_value / POBTOT) * 100, 0)
          } else {
            raw_value
          },
          
          # Formato de texto para el popup
          texto_valor = if (input$censo_interes %in% vars_porcentaje) {
            paste0(format(round(valor_mapa, 2), nsmall=2), "%")
          } else {
            format(round(valor_mapa, 2), big.mark = ",")
          },
          
          # Línea especial para escolaridad
          linea_escolaridad = if (input$censo_interes == "GRAPROES") {
            paste0("<b>Nivel promedio: </b>", GRAPROES_NIVEL, "<br>")
          } else {
            ""
          },
          
          popup_html = paste0(
            "<b>Municipio: </b>", htmlEscape(NOMBRE), "<br>",
            "<b>Sección: </b>", htmlEscape(SECCION), "<hr style='margin:0.5rem;'>",
            "<b>Población Total: </b>", format(POBTOT, big.mark = ","), "<br>",
            linea_escolaridad,
            "<b>", label_base, ": </b>", texto_valor
          )
        )
      
      # Paleta
      pal <- colorNumeric(palette = "viridis", domain = shp_dta_map$valor_mapa)
      
      titulo_leyenda <- if (input$censo_interes %in% vars_porcentaje) paste0(label_base, " (%)") else label_base
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(
          data = shp_dta_map,
          color = "#596475",
          fillColor = ~pal(valor_mapa), 
          popup = ~popup_html,
          stroke = TRUE,
          fillOpacity = 0.7,
          weight = 1.3,
          highlightOptions = highlightOptions(
            weight = 3, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE
          )
        ) %>%
        addLegend(
          pal = pal, 
          values = shp_dta_map$valor_mapa, 
          title = titulo_leyenda, 
          position = "bottomright"
        )
    })
    
    # 3. Renderizado de Tabla --------------------------------------------------
    output$dt_query_censales <- DT::renderDataTable({
      datatable(
        data = query_censales(),
        style = 'bootstrap5',
        class = 'header stripe',
        extensions = c("Scroller", "Buttons"),
        selection = "single",
        options = list(
          searching = TRUE,
          dom = 'Bfrtip',
          scrollY = 500,
          scroller = TRUE,
          scrollX = TRUE
        )
      )
    })
    
    # 4. Descarga --------------------------------------------------------------
    output$dw_censal_csv_censales <- downloadHandler(
      filename = function() { paste0("censales_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(query_censales(), file, row.names=FALSE) }
    )
    
    # 5. Fullscreen ------------------------------------------------------------
    observeEvent(input$mapFullscreen, {
      shinyjs::runjs(sprintf("fullscreen('%s')", ns("mapa_censales")))
    })
    
  })
}