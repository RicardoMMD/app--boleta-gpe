# ==============================================================================
# MÓDULO: Evolución Partidista (Cambios Porcentuales)
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_evolucion_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 3,
        box(
          width = 12,
          title = "Evolución Electoral", collapsible = TRUE, collapsed = TRUE, status = "success",
          p("Analice el cambio porcentual en la votación de un partido entre dos elecciones. Seleccione una elección base, una de comparación y el partido de interés.")
        ),
        box(
          width = 12,
          # Inputs con Namespace
          shiny::selectInput(ns("eleccion1_a"), 
                             "Selecciona la elección base (T1):", 
                             choices = lista_elecciones_nombres, # Variable Global
                             selected = "Diputado Local 2015"),
          
          shiny::selectInput(ns("eleccion2_a"), 
                             "Selecciona la segunda elección (T2):", 
                             choices = lista_elecciones_nombres, # Variable Global
                             selected = "Diputado Local 2021"),
          
          shiny::selectInput(ns("partido_analisis_a"), 
                             "Selecciona el partido a analizar:", 
                             choices = c("pan", "pri", "morena", "mc", "verde", "pt"))
        )
      ),
      column(
        width = 9,
        box(
          width = 12,
          h3("Tasa de cambio entre las dos elecciones"),
          leafletOutput(ns("mapa_cambios_perc"), height = "80vh"),
          
          div(style = "margin-top: 10px; display: flex; justify-content: space-between;",
              shiny::actionButton(ns("mapCambiosPercFullscreen"), "Pantalla Completa", icon = icon("expand")),
              downloadButton(ns("download_tasas"), "Descargar Tabla (CSV)")
          )
        )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_evolucion_server <- function(id, secciones_reactivas) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Función auxiliar para mapear nombres de UI a códigos de la base de datos (res_trab)
    # Esto replica el switch gigante del código original
    get_election_code <- function(nombre_eleccion) {
      switch(nombre_eleccion,
             "Diputado Local 2021"   = "dl21",
             "Diputado Federal 2021" = "fed21",
             "Alcalde 2021"          = "ayunt21",
             "Gobernador 2021"       = "gob21",
             "Diputado Federal 2018" = "fed18",
             "Presidente 2018"       = "pres18",
             "Diputado Local 2018"   = "dipl18",
             "Senado 2018"           = "sen18",
             "Alcalde 2018"          = "ayunt18",
             "Diputado Federal 2015" = "fed15",
             "Alcalde 2015"          = "ayunt15",
             "Diputado Local 2015"   = "dipl15",
             "Presidente 2024"       = "pres24", 
             "Senado 2024"           = "sen24",
             "Diputado Federal 2024" = "fed24", 
             "Diputado Local 2024"   = "dipl24", 
             "Alcalde 2024"          = "ayunt24"
      )
    }
    
    # 1. Procesamiento de Datos ------------------------------------------------
    
    # Elección Base (T1)
    base_eleccion_1_a <- reactive({
      req(input$eleccion1_a)
      codigo <- get_election_code(input$eleccion1_a)
      
      res_trab %>% 
        filter(eleccion == codigo) %>% 
        transmute(seccion, prop_1 = proporcion, partido)
    })
    
    # Elección Comparativa (T2)
    base_eleccion_2_a <- reactive({
      req(input$eleccion2_a)
      codigo <- get_election_code(input$eleccion2_a)
      
      res_trab %>% 
        filter(eleccion == codigo) %>% 
        transmute(seccion, prop_2 = proporcion, partido)
    })
    
    # Unión y Cálculo de Tasa
    base_mapa_gp_a <- reactive({
      req(input$partido_analisis_a)
      
      # Unir las dos bases de elecciones
      base_datos <- base_eleccion_1_a() %>% 
        full_join(base_eleccion_2_a(), by = c("seccion", "partido")) %>% 
        filter(partido == input$partido_analisis_a) %>% 
        mutate(
          # Fórmula de Tasa de Cambio: ((Final - Inicial) / Inicial)
          # Se puede multiplicar por 100 aquí o en el label
          cambio_s = (prop_2 - prop_1) / prop_1
        )
      
      # Unir con geometría filtrada
      secciones_reactivas() %>% 
        left_join(base_datos, by = c("SECCION" = "seccion")) %>% 
        filter(!is.na(SECCION)) %>% 
        st_transform(crs = "+proj=longlat +datum=WGS84")
    })
    
    # 2. Renderizado del Mapa --------------------------------------------------
    output$mapa_cambios_perc <- renderLeaflet({
      
      base_mapa_gp <- base_mapa_gp_a()
      
      shiny::validate(
        need(nrow(base_mapa_gp) > 0, "No hay datos suficientes para calcular la evolución con los filtros actuales.")
      )
      
      # Textos para popup
      eleccion_n_1 <- input$eleccion1_a
      eleccion_n_2 <- input$eleccion2_a
      partido_n    <- toupper(input$partido_analisis_a)
      
      popup_text <- sprintf(
        "<strong>Sección: %s</strong><br/>
         Tasa de cambio para %s: <strong>%s</strong><br/>
         <hr>
         Resultado en %s: %s<br/>
         Resultado en %s: %s",
        base_mapa_gp$SECCION,
        partido_n,
        scales::percent(base_mapa_gp$cambio_s, accuracy = 0.1),
        eleccion_n_1,
        scales::percent(base_mapa_gp$prop_1, accuracy = 0.1),
        eleccion_n_2,
        scales::percent(base_mapa_gp$prop_2, accuracy = 0.1)
      ) %>% lapply(htmltools::HTML)
      
      label_text <- sprintf(
        "Sección %s: %s",
        base_mapa_gp$SECCION,
        scales::percent(base_mapa_gp$cambio_s, accuracy = 0.1)
      )
      
      # Mapa
      # Nota: paleta_cambio debe estar definida en global.R (Divergente: Rojo a Verde)
      leaflet(base_mapa_gp) %>% 
        addProviderTiles(providers$CartoDB.Positron, group = "Mapa claro") %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Mapa oscuro") %>%
        
        addLegend(
          pal = paleta_cambio, 
          values = ~cambio_s,  
          title = paste("Tasa de cambio:", partido_n),
          position = "bottomright",
          opacity = 1
        ) %>%
        
        addPolygons(
          color = "#495057", 
          weight = 1.3,
          fillColor = ~paleta_cambio(cambio_s),
          fillOpacity = 0.8,
          label = label_text, 
          popup = popup_text, 
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#e63946",
            bringToFront = TRUE,
            fillOpacity = 0.9
          )
        ) %>%
        
        addLayersControl(
          baseGroups = c("Mapa claro", "Mapa oscuro"),
          options = layersControlOptions(collapsed = TRUE)
        )
    })
    
    # 3. Descarga --------------------------------------------------------------
    output$download_tasas <- downloadHandler(
      filename = function() {
        paste("evolucion_partidista-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(st_drop_geometry(base_mapa_gp_a()), file)
      }
    )
    
    # 4. Fullscreen ------------------------------------------------------------
    observeEvent(input$mapCambiosPercFullscreen, {
      shinyjs::runjs(sprintf("fullscreenCambiosPerc('%s')", ns("mapa_cambios_perc")))
    })
    
  })
}