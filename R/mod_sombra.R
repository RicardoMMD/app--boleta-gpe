# ==============================================================================
# MÓDULO: Voto Sombra
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_sombra_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 3,
        box(
          title = "Visualizador del Voto Sombra", width = 12, collapsible = TRUE, collapsed = TRUE, status = "info",
          p("Esta herramienta permite analizar cuál partido político quedó en segundo lugar en cada sección para una elección específica. Seleccione una elección para generar un mapa interactivo.")
        ),
        box(width = 12, status = "warning",
            shiny::selectInput(ns("eleccion_sombra"),
                               "Seleccione la elección a analizar:",
                               choices = choices_rendimiento_historico) # Variable global
        )
      ),
      column(
        width = 9,
        tabBox(
          width = 12,
          
          # --- Primer Tab: Contiene el mapa ---
          tabPanel(
            title = "Mapa Interactivo", 
            icon = icon("map-location-dot"), 
            
            leafletOutput(ns("mapa_sombra"), height = "80vh", width = "100%"),
            br(),
            shiny::actionButton(ns("mapSombraFullscreen"), "Pantalla Completa", icon = icon("expand"))
          ),
          
          # --- Segundo Tab: Contiene la tabla de datos ---
          tabPanel(
            title = "Tabla de Datos", 
            icon = icon("table"), 
            
            DT::DTOutput(ns("dt_sombra_sombra")),
            
            div(style = "margin-top: 10px;",
                downloadBttn(
                  outputId = ns("dw_csv_sombra"), 
                  label = "Descargar CSV", 
                  size = "sm", 
                  style = "simple",
                  color = "warning"
                )
            )
          )
        )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_sombra_server <- function(id, secciones_reactivas, rank_por_seccion_eleccion) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Procesamiento de Datos ------------------------------------------------
    query_sombra <- reactive({
      req(input$eleccion_sombra)
      
      dta <- rank_por_seccion_eleccion()
      dta_filtrada <- dta %>%
        filter(
          eleccion_año_id == input$eleccion_sombra
        )
      
      return(dta_filtrada)
    })
    
    # 2. Tabla de Datos --------------------------------------------------------
    output$dt_sombra_sombra <- DT::renderDT({
      data <- query_sombra()
      
      datatable(
        data = data,
        style = 'bootstrap5', 
        class = 'header stripe',
        extensions = c("Scroller"), 
        selection = "single",
        options = list(
          searching = FALSE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          scrollX = TRUE
        )
      )
    })
    
    # 3. Mapa Interactivo ------------------------------------------------------
    output$mapa_sombra <- renderLeaflet({
      
      base_mapa <- query_sombra()
      
      # Unir geometría (secciones filtradas desde server principal) con datos
      shp <- secciones_reactivas() %>% st_transform(crs = "+proj=longlat +datum=WGS84")
      
      shp_base_mapa <- left_join(
        shp,
        base_mapa,
        by = c("SECCION" = "seccion")
      ) %>%
        mutate(
          # --- Cálculos para el Popup ---
          
          # Porcentaje de votos ganadores
          porc_votos_ganador = (votos_1 / votos_totales_seccion) * 100,
          
          # Porcentaje de votos segundo lugar
          porc_votos_2do = (votos_2 / votos_totales_seccion) * 100,
          
          # Diferencia de votos
          distancia_votos_2do = votos_1 - votos_2,
          porc_distancia = (distancia_votos_2do / votos_totales_seccion) * 100,
          
          # Participación
          porc_participacion = (votos_totales_seccion / pobtotal) * 100,
          
          # --- Creación del Popup HTML ---
          popup_html = paste0(
            "<b>Sección: </b>", htmlEscape(SECCION), "<br>",
            "<b>Elección: </b>", htmlEscape(eleccion_año_id), "<hr style='margin:0.5rem;'>",
            
            "<b>Población: </b>", format(round(pobtotal, 0), big.mark = ","), "<br>",
            "<b>Votos Totales: </b>", format(round(votos_totales_seccion, 0), big.mark = ","), "<br>",
            "<b>% Participación: </b>", round(porc_participacion, 2), "%<hr style='margin:0.5rem;'>",
            
            "<b>2do Lugar (Sombra): </b>", htmlEscape(partido_2), "<br>",
            "<b>Votos 2do Lugar: </b>", format(round(votos_2, 0), big.mark = ","), "<br>",
            "<b>% Voto 2do Lugar: </b>", round(porc_votos_2do, 2), "%<hr style='margin:0.5rem;'>",
            
            "<b>Ganador (1er Lugar): </b>", format(round(votos_1, 0), big.mark = ","), " (", partido_1, ")", "<br>",
            "<b>Margen victoria: </b>", format(distancia_votos_2do, big.mark = ","), " votos", " (", round(porc_distancia, 2), "%)"
          )
        )
      
      shiny::validate(
        need(nrow(shp_base_mapa) > 0, 
             "No hay datos para esta selección con los filtros geográficos actuales.")
      )
      
      # Definición de Paleta usando la variable global paleta_partidos
      # Si paleta_partidos no tiene todos los colores, usamos un fallback
      
      # Identificar partidos presentes en la columna 'partido_2' (segundo lugar)
      partidos_en_mapa <- unique(na.omit(shp_base_mapa$partido_2))
      
      # Asegurar que tenemos colores para todos
      colores_leyenda <- c()
      for(p in partidos_en_mapa) {
        if(p %in% names(paleta_partidos)) {
          colores_leyenda <- c(colores_leyenda, paleta_partidos[[p]])
        } else {
          colores_leyenda <- c(colores_leyenda, "#333333") # Color default negro/gris
        }
      }
      
      # Agregar color para NA si existe
      colores_leyenda <- c(colores_leyenda, "#dad7cd")
      etiquetas_leyenda <- c(partidos_en_mapa, "Sin datos / No aplica")
      
      # Renderizar mapa
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(
          data = shp_base_mapa,
          label = ~ SECCION,
          color = "#596475", 
          fillColor = ~ifelse(is.na(partido_2), 
                              "#dad7cd", 
                              paleta_partidos[partido_2]), 
          popup = ~ popup_html, 
          stroke = TRUE, 
          fillOpacity = 0.7, 
          weight = 1.3,
          highlightOptions = highlightOptions(
            weight = 3, # Ligeramente más grueso al pasar mouse
            color = "#f72585",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomright",
          colors = colores_leyenda,
          labels = etiquetas_leyenda,
          title = "Partido en 2do Lugar",
          opacity = 0.8
        )
    })
    
    # 4. Descarga CSV ----------------------------------------------------------
    output$dw_csv_sombra <- downloadHandler(
      filename = function() {
        paste0(format(Sys.Date(),"%d_%m_%Y"),"_voto_sombra.csv")
      },
      content = function(file) {
        write.csv(query_sombra(), file, row.names = FALSE)
      }
    )
    
    # 5. Fullscreen JS ---------------------------------------------------------
    observeEvent(input$mapSombraFullscreen, {
      # Llamamos a la función JS pasando el ID con namespace
      shinyjs::runjs(sprintf("fullscreenSombra('%s')", ns("mapa_sombra")))
    })
    
  })
}