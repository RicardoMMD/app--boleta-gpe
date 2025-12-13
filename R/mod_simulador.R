# ==============================================================================
# MÓDULO: Simulador de Resultados Electorales
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_simulador_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # --- Panel Izquierdo: Controles e Instrucciones ---
      column(
        width = 3, 
        box(
          width = 12,
          title = "Simulador Electoral", 
          collapsible = TRUE, 
          collapsed = FALSE, # Mejor abierto por defecto para ver controles
          solidHeader = FALSE,
          status = "info",
          p("El simulador electoral te permite generar y visualizar escenarios hipotéticos. Selecciona una elección, elimina un partido y transfiere sus votos a otro."),
          tags$ol(
            tags$li(tags$b("Elección:"), " Proceso base."),
            tags$li(tags$b("Eliminar:"), " Partido que no participa."),
            tags$li(tags$b("Destino:"), " Partido que recibe los votos.")
          )
        ),
        box(
          width = 12, 
          status = "warning",
          shiny::selectInput(ns("eleccion_sim1"), 
                             "Selecciona la elección a simular:",
                             choices = choices_elections_sombra # Variable Global
          ),
          shiny::selectInput(ns("dejar_afuera"), 
                             "Selecciona al partido a eliminar:",
                             choices = c("PAN" = "pan", "PRI" = "pri", "Morena" = "morena", "MC" = "mc", "Verde" = "verde", "PT" = "pt")
          ),
          shiny::selectInput(ns("recibe_votos"), 
                             "Selecciona el destino de los votos:",
                             choices = c("PAN" = "pan", "PRI" = "pri","Morena" = "morena", "MC" = "mc", "Verde" = "verde", "PT" = "pt")
          )
        )
      ),
      
      # --- Panel Derecho: Mapa ---
      column(
        width = 9,
        box(
          width = 12,
          h3("Escenario Simulado: Nuevo Ganador por Sección"),
          leafletOutput(ns("mapa_sim1"), height = "80vh"),
          
          div(style = "margin-top: 10px; display: flex; justify-content: space-between;",
              shiny::actionButton(ns("mapSim1Fullscreen"), "Pantalla Completa", icon = icon("expand")),
              downloadButton(ns("downloadganador_sim"), "Descargar Escenario (CSV)")
          )
        )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_simulador_server <- function(id, secciones_reactivas) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Cálculo del Escenario -------------------------------------------------
    base_sim_1 <- reactive({
      req(input$eleccion_sim1, input$dejar_afuera, input$recibe_votos)
      
      # Filtrar datos base (res_trab debe ser global)
      base_inicial <- res_trab %>% 
        filter(eleccion == input$eleccion_sim1) 
      
      # Identificar votos a transferir
      votos_transferidos <- base_inicial %>% 
        filter(partido == input$dejar_afuera) %>% 
        transmute(seccion, partido = input$recibe_votos, proporcion_extra = proporcion) 
      
      # Aplicar transferencia
      base_calculada <- base_inicial %>% 
        filter(partido != input$dejar_afuera) %>% # Quitar al partido eliminado
        left_join(votos_transferidos, by = c("seccion", "partido")) %>% 
        replace_na(list(proporcion_extra = 0)) %>% 
        mutate(proporcion = proporcion + proporcion_extra) %>% 
        select(seccion, partido, proporcion) %>% 
        pivot_wider(names_from = partido, values_from = proporcion, values_fill = 0)
      
      # Asegurar que existan todas las columnas para la comparación
      cols_necesarias <- c("pan", "pri", "morena", "mc", "verde", "pt")
      for(col in cols_necesarias) {
        if(!col %in% names(base_calculada)) base_calculada[[col]] <- 0
      }
      
      # Determinar Ganador
      # Nota: Usamos max.col para ser más robustos que los if anidados,
      # pero mantenemos la lógica de comparación directa.
      base_ganadores_sim <- base_calculada %>%
        rowwise() %>%
        mutate(
          ganador = case_when(
            pan > pri & pan > mc & pan > morena & pan > verde & pan > pt ~ "PAN",
            pri > pan & pri > mc & pri > morena & pri > verde & pri > pt ~ "PRI",
            morena > pri & morena > mc & morena > pan & morena > verde & morena > pt ~ "morena",
            mc > pri & mc > pan & mc > morena & mc > verde & mc > pt ~ "MC",
            verde > pri & verde > pan & verde > morena & verde > mc & verde > pt ~ "VERDE",
            pt > pri & pt > pan & pt > morena & pt > mc & pt > verde ~ "pt",
            TRUE ~ "Empate/Otro"
          )
        ) %>%
        ungroup()
      
      # Unir con geometría
      secciones_reactivas() %>% 
        left_join(base_ganadores_sim, by = c("SECCION" = "seccion")) %>% 
        filter(!is.na(ganador)) %>%
        st_transform(crs = "+proj=longlat +datum=WGS84")
    })
    
    # 2. Renderizado del Mapa --------------------------------------------------
    output$mapa_sim1 <- renderLeaflet({
      
      mapa_data <- base_sim_1()
      
      shiny::validate(need(nrow(mapa_data) > 0, "No hay datos para la simulación seleccionada."))
      
      # Ajustar niveles para la paleta
      niveles <- c("MC", "PAN", "PRI", "morena", "VERDE", "pt", "Empate/Otro")
      mapa_data$ganador <- factor(mapa_data$ganador, levels = niveles)
      
      pal <- colorFactor(
        palette = c("orange","blue", "red", "maroon", "#6CB655","#E7DE08", "gray"), 
        domain = mapa_data$ganador,
        na.color = "gray"
      )
      
      popup_html <- paste0(
        "<b>Sección: </b>", mapa_data$SECCION, "<br>",
        "<b>Ganador Simulado: </b>", toupper(mapa_data$ganador), "<hr>",
        "<b>PRI: </b>", scales::percent(mapa_data$pri, 0.1), "<br>",
        "<b>PAN: </b>", scales::percent(mapa_data$pan, 0.1), "<br>",
        "<b>Morena: </b>", scales::percent(mapa_data$morena, 0.1), "<br>",
        "<b>MC: </b>", scales::percent(mapa_data$mc, 0.1), "<br>",
        "<b>Verde: </b>", scales::percent(mapa_data$verde, 0.1), "<br>",
        "<b>PT: </b>", scales::percent(mapa_data$pt, 0.1)
      )
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addLegend(pal = pal, values = mapa_data$ganador, title = "Ganador Simulado") %>% 
        addPolygons(
          data = mapa_data, 
          color = "#596475", 
          fillColor = ~pal(ganador), 
          popup = popup_html, 
          stroke = TRUE, 
          fillOpacity = 0.7, 
          weight = 1.5,
          highlightOptions = highlightOptions(
            weight = 4, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE
          )
        )
    })
    
    # 3. Descarga --------------------------------------------------------------
    output$downloadganador_sim <- downloadHandler(
      filename = function() { paste("simulacion_escenario-", Sys.Date(), ".csv", sep="") },
      content = function(file) { write.csv(st_drop_geometry(base_sim_1()), file) }
    )
    
    # 4. Fullscreen ------------------------------------------------------------
    observeEvent(input$mapSim1Fullscreen, {
      shinyjs::runjs(sprintf("fullscreenSim1('%s')", ns("mapa_sim1")))
    })
    
  })
}