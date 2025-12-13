# ==============================================================================
# MÓDULO: Gestión de Tiempo de Campaña
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_gestion_tiempo_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Gestión de tiempo de campaña", collapsible = TRUE, collapsed = TRUE,
        width = 12, solidHeader = FALSE, status = "success",
        p("Esta herramienta calcula la distribución óptima del tiempo de campaña. Determine cuántas horas dedicar a cada sección en función de los días disponibles, la cantidad de equipos y la densidad poblacional.")
      ),
      
      # --- Controles ---
      box(width = 3, 
          shiny::sliderInput(ns("dias_campaña"), 
                             "Días de campaña disponibles:", 
                             min = 0, max = 120, step = 10, value = 90),
          
          shiny::sliderInput(ns("equipos"), 
                             "Cantidad de equipos simultáneos:",
                             min = 1, max = 20, step = 1, value = 3)
      ),
      
      # --- Mapa ---
      box(width = 9,
          h3("Distribución de Horas por Sección"),
          leafletOutput(ns("mapa_visitas_secciones"), height = "80vh"),
          
          div(style = "margin-top: 10px; display: flex; justify-content: space-between;",
              shiny::actionButton(ns("mapVisitasFullscreen"), "Pantalla Completa", icon = icon("expand")),
              downloadButton(ns("download_basej"), "Descargar Planificación (CSV)")
          )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_gestion_tiempo_server <- function(id, secciones_reactivas, base_ganadores_pr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Cálculo Logístico -----------------------------------------------------
    base_visitas_secciones <- reactive({
      req(secciones_reactivas())
      
      # Datos de las secciones actuales (filtradas por el usuario)
      shp_actual <- secciones_reactivas()
      
      # 1. Calcular Factor Z
      # Se asume una jornada de 8 horas diarias de trabajo efectivo
      horas_totales_campana <- input$dias_campaña * 8 * input$equipos
      poblacion_total_zona  <- sum(na.omit(shp_actual$pobtotal))
      
      # Evitar división por cero
      if(poblacion_total_zona == 0) return(NULL)
      
      factor_asignacion <- horas_totales_campana / poblacion_total_zona
      
      # 2. Preparar datos de estrategia (basado en ganador previo)
      # Usamos base_ganadores_pr (que ya tiene info histórica)
      # Nota: Asumimos que base_ganadores_pr tiene columnas como 'dip_local_21'
      # Si base_ganadores_pr es reactivo, lo llamamos con ()
      datos_estrategia <- base_ganadores_pr() %>%
        st_drop_geometry() %>%
        select(seccion = SECCION, dip_local_21) %>% 
        mutate(
          PRIPAN = case_when(
            dip_local_21 == "PRI" ~ "PRI",
            TRUE ~ "PAN" # Simplificación estratégica original
          )
        )
      
      # 3. Unir y Calcular Horas por Sección
      base_final <- shp_actual %>%
        left_join(datos_estrategia, by = c("SECCION" = "seccion")) %>%
        mutate(
          tiempoxseccion = round(pobtotal * factor_asignacion, 2)
        ) %>% 
        filter(!is.na(SECCION)) %>%
        st_transform(crs = "+proj=longlat +datum=WGS84")
      
      return(base_final)
    })
    
    # 2. Renderizado del Mapa --------------------------------------------------
    output$mapa_visitas_secciones <- renderLeaflet({
      
      base_mapa <- base_visitas_secciones()
      shiny::validate(need(!is.null(base_mapa) && nrow(base_mapa) > 0, "No hay datos para calcular."))
      
      # Paleta de colores (Blanco a Rojo según intensidad de horas)
      pal <- colorNumeric(palette = c("white", "red"), domain = base_mapa$tiempoxseccion)
      
      popup_html <- paste0(
        "<b>Sección: </b>", base_mapa$SECCION, "<br/>", 
        "<b>Población (Total): </b>", scales::comma(base_mapa$pobtotal), "<br/>",
        "<b>Estrategia sugerida: </b>", base_mapa$PRIPAN, "<hr>",
        "<b>Horas a dedicar: </b>", base_mapa$tiempoxseccion
      )
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addLegend(
          pal = pal, 
          values = base_mapa$tiempoxseccion,  
          title = "Horas sugeridas",
          position = "bottomright"
        ) %>% 
        addPolygons(
          data = base_mapa, 
          color = "black", 
          fillColor = ~pal(tiempoxseccion),
          popup = popup_html, 
          stroke = TRUE, 
          fillOpacity = 0.8, 
          weight = 0.5,
          highlightOptions = highlightOptions(
            weight = 2, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE
          )
        )
    })
    
    # 3. Descarga --------------------------------------------------------------
    output$download_basej <- downloadHandler(
      filename = "planificacion_tiempo_seccion.csv",
      content = function(file) {
        datos_out <- base_visitas_secciones() %>% 
          st_drop_geometry() %>% 
          select(Seccion = SECCION, Municipio = NOMBRE, Poblacion = pobtotal, 
                 Horas_Asignadas = tiempoxseccion, Estrategia = PRIPAN)
        
        write.csv(datos_out, file, row.names = FALSE)
      }
    )
    
    # 4. Fullscreen ------------------------------------------------------------
    observeEvent(input$mapVisitasFullscreen, {
      shinyjs::runjs(sprintf("fullscreenVisitas('%s')", ns("mapa_visitas_secciones")))
    })
    
  })
}