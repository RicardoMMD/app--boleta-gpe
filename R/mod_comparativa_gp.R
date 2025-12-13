# ==============================================================================
# MÓDULO: Comparativa Ganadas/Perdidas
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_comparativa_gp_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Comparativa Ganadas vs Perdidas", 
        collapsible = TRUE, collapsed = TRUE,
        width = 12, solidHeader = FALSE, status = "primary",  
        p("Esta sección te permite realizar un análisis comparativo entre dos elecciones para entender dónde un partido específico ganó, perdió o mantuvo su posición."),
        tags$ul(
          tags$li(strong("Mapa General:"), " Panorama completo (Ganó/Perdió/Igual)."),
          tags$li(strong("Ganadas:"), " Muestra a quién se le arrebató la sección."),
          tags$li(strong("Perdidas:"), " Muestra quién arrebató la sección.")
        )
      )
    ),
    
    fluidRow(
      # --- Panel de Filtros ---
      box(
        width = 3,
        title = "Filtros de Análisis",
        solidHeader = FALSE, status = "success",
        
        selectInput(ns("eleccion1"), "1. Elección Base (Pasado):", choices = choices_rendimiento_historico),
        selectInput(ns("eleccion2"), "2. Elección Comparativa (Presente):", choices = choices_rendimiento_historico),
        selectInput(ns("partido_analisis"), "3. Partido a Analizar:", choices = c("PAN","PRI","VERDE","PT","MC","MORENA","INDEP")),
        
        hr(), 
        downloadButton(ns("download_robados"), "Descargar CSV", class = "btn-block")
      ),
      
      # --- Visualización Tabulada ---
      tabBox(
        width = 9,
        id = ns("map_tabs"),
        title = "Visualización Comparativa",
        
        # Pestaña 1: Mapa General
        tabPanel(
          title = "General", icon = icon("globe-americas"),
          leafletOutput(ns("mapa_gana_o_pierde"), height = "78vh"),
          actionButton(ns("mapGanaOPierdeFullscreen"), "Pantalla Completa", icon = icon("expand"), class = "pull-right")
        ),
        
        # Pestaña 2: Ganadas
        tabPanel(
          title = "Ganadas (Arrebatadas)", icon = icon("plus-circle"),
          leafletOutput(ns("mapa_gana"), height = "78vh"),
          actionButton(ns("mapGanaFullscreen"), "Pantalla Completa", icon = icon("expand"), class = "pull-right")
        ),
        
        # Pestaña 3: Perdidas
        tabPanel(
          title = "Perdidas (Cedidas)", icon = icon("minus-circle"),
          leafletOutput(ns("mapa_pierde"), height = "78vh"),
          actionButton(ns("mapPierdeFullscreen"), "Pantalla Completa", icon = icon("expand"), class = "pull-right")
        )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_comparativa_gp_server <- function(id, secciones_reactivas, rank_por_seccion_eleccion) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Preparación de Datos Base ---------------------------------------------
    base_eleccion_1 <- reactive({
      req(input$eleccion1)
      rank_por_seccion_eleccion() %>% filter(eleccion_año_id == input$eleccion1)
    })
    
    base_eleccion_2 <- reactive({
      req(input$eleccion2)
      rank_por_seccion_eleccion() %>% filter(eleccion_año_id == input$eleccion2)
    })
    
    dta_ganadores_perdedores <- reactive({
      req(input$partido_analisis)
      partido_interes <- input$partido_analisis
      
      d1 <- base_eleccion_1()
      d2 <- base_eleccion_2()
      
      full_join(d1, d2, by = "seccion") %>% 
        mutate(
          gana_o_pierde = case_when(
            partido_1.x != partido_interes & partido_1.y == partido_interes ~ "gana",
            partido_1.x == partido_interes & partido_1.y != partido_interes ~ "pierde",
            partido_1.x != partido_interes & partido_1.y != partido_interes ~ "igual (pierde)",
            partido_1.x == partido_interes & partido_1.y == partido_interes ~ "igual (gana)",
            TRUE ~ "NA"
          )
        )
    })
    
    # 2. Mapa 1: General (Gana/Pierde/Igual) -----------------------------------
    output$mapa_gana_o_pierde <- renderLeaflet({
      dta <- dta_ganadores_perdedores()
      shiny::validate(need(nrow(dta) > 0, "No hay datos para mostrar."))
      
      dta$gana_o_pierde <- factor(dta$gana_o_pierde, levels = c("gana", "pierde", "igual (pierde)", "igual (gana)"))
      pal <- colorFactor(c("#06d6a0","#ef476f", "#e36414", "#219ebc"), domain = dta$gana_o_pierde)
      
      shp_dta <- secciones_reactivas() %>% 
        st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
        left_join(dta, by = c("SECCION" = "seccion")) %>% 
        filter(!is.na(gana_o_pierde)) %>%
        mutate(
          popup_html = paste0(
            "<b>Sección: </b>", SECCION, "<br>",
            "<b>Estado: </b>", gana_o_pierde, "<hr>",
            "<b>", input$eleccion1, ": </b>", votos_1.x," (",partido_1.x,")<br>",
            "<b>", input$eleccion2, ": </b>", votos_1.y," (",partido_1.y,")"
          )
        )
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Voyager)  %>% 
        addLegend(pal = pal, values = shp_dta$gana_o_pierde, title = "Balance") %>% 
        addPolygons(
          data = shp_dta, color = "#596475", fillColor = ~ pal(gana_o_pierde),
          popup = ~ popup_html, stroke = T, fillOpacity = 0.7, weight= 1.3,
          highlightOptions = highlightOptions(weight = 3, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE)
        ) 
    })
    
    # 3. Mapa 2: Ganadas (¿A quién se le ganó?) --------------------------------
    output$mapa_gana <- renderLeaflet({
      dta <- dta_ganadores_perdedores() %>% filter(gana_o_pierde == "gana")
      shiny::validate(need(nrow(dta) > 0, "No se registraron secciones ganadas (flips) en esta comparativa."))
      
      # Join espacial
      shp_dta <- secciones_reactivas() %>%
        st_transform(crs = "+proj=longlat +datum=WGS84") %>%
        left_join(dta, by = c("SECCION" = "seccion"))
      
      # Paleta consistente usando paleta global
      partidos_mapa <- unique(shp_dta$partido_1.x)
      pal <- colorFactor(palette = paleta_partidos, domain = partidos_mapa, na.color = "#808080")
      
      leaflet() %>%  
        addProviderTiles(providers$CartoDB.Voyager)  %>%  
        addLegend(pal = pal, values = shp_dta$partido_1.x, title = "Se le ganó a:") %>%  
        addPolygons(
          data = shp_dta, color = "#596475", weight= 1.3, stroke = T,
          fillColor = ~pal(partido_1.x), fillOpacity = 0.65,
          popup = ~paste0("<b>Sección:</b> ", SECCION, "<br><b>Antes:</b> ", partido_1.x),
          highlightOptions = highlightOptions(weight = 3, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE)
        )
    })
    
    # 4. Mapa 3: Perdidas (¿Quién nos ganó?) -----------------------------------
    output$mapa_pierde <- renderLeaflet({
      dta <- dta_ganadores_perdedores() %>% filter(gana_o_pierde == "pierde")
      shiny::validate(need(nrow(dta) > 0, "No se registraron secciones perdidas en esta comparativa."))
      
      shp_dta <- secciones_reactivas() %>%
        st_transform(crs = "+proj=longlat +datum=WGS84") %>%
        left_join(dta, by = c("SECCION" = "seccion"))
      
      partidos_mapa <- unique(shp_dta$partido_1.y)
      pal <- colorFactor(palette = paleta_partidos, domain = partidos_mapa, na.color = "#808080")
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Voyager)%>% 
        addLegend(pal = pal, values = shp_dta$partido_1.y, title = "Se perdió contra:") %>% 
        addPolygons(
          data = shp_dta, color = "#596475", fillColor = ~pal(partido_1.y), fillOpacity = 0.65,
          stroke = T, weight= 1.3,
          popup = ~paste0("<b>Sección:</b> ", SECCION, "<br><b>Ahora:</b> ", partido_1.y),
          highlightOptions = highlightOptions(weight = 3, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE)
        )
    })
    
    # 5. Descarga --------------------------------------------------------------
    output$download_robados <- downloadHandler(
      filename = function() { paste("comparativa_gp-", Sys.Date(), ".csv", sep="") },
      content = function(file) {
        # Unir info geográfica básica para el reporte
        data_out <- dta_ganadores_perdedores() %>%
          left_join(st_drop_geometry(secciones_reactivas()) %>% select(SECCION, NOMBRE), by = c("seccion"="SECCION"))
        write.csv(data_out, file, row.names=FALSE)
      }
    )
    
    # 6. Fullscreen JS ---------------------------------------------------------
    observeEvent(input$mapGanaOPierdeFullscreen, { shinyjs::runjs(sprintf("fullscreenGanaOPierde('%s')", ns("mapa_gana_o_pierde"))) })
    observeEvent(input$mapGanaFullscreen,        { shinyjs::runjs(sprintf("fullscreenGana('%s')", ns("mapa_gana"))) })
    observeEvent(input$mapPierdeFullscreen,      { shinyjs::runjs(sprintf("fullscreenPierde('%s')", ns("mapa_pierde"))) })
    
  })
}