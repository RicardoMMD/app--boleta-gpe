# ==============================================================================
# MÓDULO: Comparativo Resultados Históricos
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_ganadores_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Comparativo Resultados Históricos",
        collapsible = TRUE, collapsed = TRUE, 
        solidHeader = FALSE, status = "info",
        p("Explore y compare visualmente los datos electorales de diferentes periodos. Seleccione dos elecciones para generar mapas interactivos donde cada sección se colorea según el partido ganador.")
      )
    ),
    fluidRow( 
      # --- Panel Izquierdo (Elección 1) ---
      box(
        width = 6,
        status = "success",
        shiny::selectInput(
          ns("eleccion_1"), "Elección para el primer panel:",
          choices = choices_rendimiento_historico,
          selected = choices_rendimiento_historico$`Elecciones 2024`[1]
        ),
        
        leafletOutput(ns("mapa_ganador_1"), height = "80vh"),
        
        div(style = "margin-top: 10px; display: flex; justify-content: space-between;",
            shiny::actionButton(ns("mapGanador1Fullscreen"), "Pantalla Completa", icon = icon("expand")),
            downloadButton(ns("downloadganador_elec1"), "Descargar CSV")
        )
      ),
      
      # --- Panel Derecho (Elección 2) ---
      box(
        width = 6,
        status = "success",
        shiny::selectInput(
          ns("eleccion_2"), "Elección para el segundo panel:",
          choices = choices_rendimiento_historico,
          selected = choices_rendimiento_historico$`Elecciones 2021`[1]
        ),
        
        leafletOutput(ns("mapa_ganador_2"), height = "80vh"),
        
        div(style = "margin-top: 10px; display: flex; justify-content: space-between;",
            shiny::actionButton(ns("mapGanador2Fullscreen"), "Pantalla Completa", icon = icon("expand")),
            downloadButton(ns("downloadganador_elec2"), "Descargar CSV")
        )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_ganadores_server <- function(id, secciones_reactivas, rank_por_seccion_eleccion) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- LÓGICA MAPA 1 --------------------------------------------------------
    
    base_ganador_1 <- reactive({
      req(input$eleccion_1)
      rank_por_seccion_eleccion() %>%
        filter(eleccion_año_id == input$eleccion_1)
    })
    
    output$mapa_ganador_1 <- renderLeaflet({
      dta <- base_ganador_1()
      req(nrow(dta) > 0)
      
      # Unir con geometría
      shp_dta_popup <- secciones_reactivas() %>%
        left_join(dta, by = c("SECCION" = "seccion")) %>%
        st_transform(crs = "+proj=longlat +datum=WGS84") %>%
        filter(!is.na(partido_1)) # Filtrar si no hay datos
      
      validate(
        need(nrow(shp_dta_popup) > 0, "No hay datos para la selección actual.")
      )
      
      # Preparar paleta y popups (Logica replicada del original)
      # Asegurar niveles consistente
      niveles_partidos <- c("MC", "PAN", "PRI", "MORENA", "INDEPE", "VERDE", "PT", "otro", "PVEM")
      shp_dta_popup$partido_1 <- factor(shp_dta_popup$partido_1, levels = niveles_partidos)
      
      pal <- colorFactor(c("orange","blue", "red", "brown", "purple", "#6CB655", "#E7DE08", "black", "#6CB655"), 
                         domain = shp_dta_popup$partido_1)
      
      # Cálculos para popup
      shp_dta_popup <- shp_dta_popup %>%
        mutate(
          porc_votos_ganador = (votos_1 / votos_totales_seccion) * 100,
          distancia_votos_2do = votos_1 - votos_2,
          porc_distancia = (distancia_votos_2do / votos_totales_seccion) * 100,
          porc_participacion = (votos_totales_seccion/pobtotal)*100,
          popup_html = paste0(
            "<b>Sección: </b>", htmlEscape(SECCION), "<br>",
            "<b>Elección: </b>", htmlEscape(eleccion_año_id), "<hr style='margin:0.5rem;'>",
            "<b>Ganador: </b>", htmlEscape(partido_1), "<br>",
            "<b>Votos: </b>", format(round(votos_1, 0), big.mark = ","), 
            " (", round(porc_votos_ganador, 2), "%)<br>",
            "<b>Ventaja: </b>", format(distancia_votos_2do, big.mark = ","), 
            " (", round(porc_distancia, 2), "%)"
          )
        )
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addLegend(position = "bottomright", pal = pal, values = shp_dta_popup$partido_1, title = "Ganador") %>% 
        addPolygons(
          data = shp_dta_popup,
          color = "#596475", stroke = T, weight= 1.3,
          fillColor = ~pal(partido_1), fillOpacity = 0.7, 
          popup = ~popup_html, 
          highlightOptions = highlightOptions(weight = 4, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE)
        ) %>% 
        addControl(paste0("Resultados: ", input$eleccion_1), position = "bottomleft", className="map-title") 
    })
    
    # Descarga 1
    output$downloadganador_elec1 <- downloadHandler(
      filename = function() { paste("ganador_1-", Sys.Date(), ".csv", sep="") },
      content = function(file) { write.csv(st_drop_geometry(base_ganador_1()), file) }
    )
    
    # Fullscreen 1 (JS)
    observeEvent(input$mapGanador1Fullscreen, {
      # Invocamos la función JS pasando el ID namespaced
      shinyjs::runjs(sprintf("fullscreenGanador1('%s')", ns("mapa_ganador_1")))
    })
    
    
    # --- LÓGICA MAPA 2 --------------------------------------------------------
    
    base_ganador_2 <- reactive({
      req(input$eleccion_2)
      rank_por_seccion_eleccion() %>%
        filter(eleccion_año_id == input$eleccion_2)
    })
    
    output$mapa_ganador_2 <- renderLeaflet({
      dta <- base_ganador_2()
      req(nrow(dta) > 0)
      
      shp_dta_popup <- secciones_reactivas() %>%
        left_join(dta, by = c("SECCION" = "seccion")) %>%
        st_transform(crs = "+proj=longlat +datum=WGS84") %>%
        filter(!is.na(partido_1))
      
      validate(need(nrow(shp_dta_popup) > 0, "No hay datos para la selección actual."))
      
      niveles_partidos <- c("MC", "PAN", "PRI", "MORENA", "INDEPE", "VERDE", "PT", "otro", "PVEM")
      shp_dta_popup$partido_1 <- factor(shp_dta_popup$partido_1, levels = niveles_partidos)
      pal <- colorFactor(c("orange","blue", "red", "brown", "purple", "#6CB655", "#E7DE08", "black", "#6CB655"), 
                         domain = shp_dta_popup$partido_1)
      
      shp_dta_popup <- shp_dta_popup %>%
        mutate(
          porc_votos_ganador = (votos_1 / votos_totales_seccion) * 100,
          distancia_votos_2do = votos_1 - votos_2,
          porc_distancia = (distancia_votos_2do / votos_totales_seccion) * 100,
          popup_html = paste0(
            "<b>Sección: </b>", htmlEscape(SECCION), "<br>",
            "<b>Elección: </b>", htmlEscape(eleccion_año_id), "<hr style='margin:0.5rem;'>",
            "<b>Ganador: </b>", htmlEscape(partido_1), "<br>",
            "<b>Votos: </b>", format(round(votos_1, 0), big.mark = ","), 
            " (", round(porc_votos_ganador, 2), "%)<br>",
            "<b>Ventaja: </b>", format(distancia_votos_2do, big.mark = ","), 
            " (", round(porc_distancia, 2), "%)"
          )
        )
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addLegend(position = "bottomright", pal = pal, values = shp_dta_popup$partido_1, title = "Ganador") %>% 
        addPolygons(
          data = shp_dta_popup,
          color = "#596475", stroke = T, weight= 1.3,
          fillColor = ~pal(partido_1), fillOpacity = 0.7, 
          popup = ~popup_html, 
          highlightOptions = highlightOptions(weight = 4, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE)
        ) %>% 
        addControl(paste0("Resultados: ", input$eleccion_2), position = "bottomleft", className="map-title") 
    })
    
    # Descarga 2
    output$downloadganador_elec2 <- downloadHandler(
      filename = function() { paste("ganador_2-", Sys.Date(), ".csv", sep="") },
      content = function(file) { write.csv(st_drop_geometry(base_ganador_2()), file) }
    )
    
    # Fullscreen 2 (JS)
    observeEvent(input$mapGanador2Fullscreen, {
      shinyjs::runjs(sprintf("fullscreenGanador2('%s')", ns("mapa_ganador_2")))
    })
    
  })
}