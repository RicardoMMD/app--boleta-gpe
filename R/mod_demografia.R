# ==============================================================================
# MÓDULO: Demografía Espacial
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_demografia_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 3,
        box(
          width = 12,
          title = "Demografía Espacial por Edad y Género", 
          collapsible = TRUE, collapsed = TRUE, status = "info",
          p("Visualice la distribución demográfica en el mapa. Seleccione los grupos de edad y géneros de interés para ver qué secciones tienen mayor concentración de dicha población.")
        ),
        box(
          width = 12,
          # Inputs con Namespace
          shiny::selectInput(ns("edades"),
                             "Seleccione grupos de edad:",
                             choices =  c("18 a 24" = "_18_24", "25 a 29" = "_25_29", "30 a 39" = "_30_39", 
                                          "40 a 49" = "_40_49", "50 a 59" = "_50_59", "Mayores" = "mayores"),
                             selected = c("_18_24", "_25_29", "_30_39", "_40_49", "_50_59", "mayores"),
                             multiple = TRUE),
          shiny::selectInput(ns("genero"),
                             "Seleccione géneros:",
                             choices =  c("Hombre" = "hombres", "Mujer" = "mujeres"),
                             selected = c("hombres", "mujeres"),
                             multiple = TRUE)
        )
      ),
      column(
        width = 9,
        box(
          width = 12,
          leafletOutput(ns("mapa_generos_edad"), height = "80vh"),
          
          div(style = "margin-top: 10px; display: flex; justify-content: space-between;",
              shiny::actionButton(ns("mapGenerosEdadFullscreen"), "Pantalla Completa", icon = icon("expand")),
              downloadButton(ns("download_edades"), "Descargar Información (CSV)")
          )
        )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_demografia_server <- function(id, secciones_reactivas) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Procesamiento de Datos ------------------------------------------------
    base_mapa_edad <- reactive({
      req(input$edades, input$genero)
      
      # Crear patrones regex para filtrar columnas
      pattern_edades <- stringr::str_c(input$edades, collapse = "|") 
      pattern_genero <- stringr::str_c(input$genero, collapse = "|") 
      
      # Nota: 'edades' debe estar disponible en el entorno global (cargado en global.R)
      base_edad <- edades %>% 
        pivot_longer(cols = hombres_18_24:mujeres_mayores, names_to = "categoria", values_to = "cantidad") %>% 
        filter(
          stringr::str_detect(categoria, pattern_edades),
          stringr::str_detect(categoria, pattern_genero)
        ) %>% 
        group_by(seccion)  %>% 
        summarise(
          cantidad = sum(cantidad, na.rm = TRUE), 
          total = mean(padron_total, na.rm = TRUE) # Asumimos que padron_total es constante por sección
        ) %>% 
        mutate(prop = cantidad/total)
      
      # Unir con geometría
      secciones_reactivas() %>% 
        left_join(base_edad, by = c("SECCION" = "seccion")) %>% 
        filter(!is.na(SECCION)) %>% 
        st_transform(crs = "+proj=longlat +datum=WGS84")
    })
    
    # 2. Renderizado del Mapa --------------------------------------------------
    output$mapa_generos_edad <- renderLeaflet({
      
      mapa_data <- base_mapa_edad()
      
      validate(
        need(nrow(mapa_data) > 0, "No hay datos para la selección actual.")
      )
      
      # Paleta numérica (Naranja a Azul)
      pal <- colorNumeric(c("orange", "blue"), domain = mapa_data$prop, na.color = "#ccc")
      
      popup_html <- paste0(
        "<b>Sección: </b>", mapa_data$SECCION, "<br/>", 
        "<b>Población seleccionada: </b>", scales::comma(mapa_data$cantidad), "<br/>",
        "<b>Proporción del padrón: </b>", scales::percent(mapa_data$prop, 0.1)
      )
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addLegend(pal = pal, values = mapa_data$prop, title = "Proporción", position = "bottomright") %>% 
        addPolygons(
          data = mapa_data, 
          color = "#596475", 
          fillColor = ~pal(prop), 
          popup = popup_html, 
          stroke = TRUE, 
          fillOpacity = 0.7, 
          weight = 1.3,
          highlightOptions = highlightOptions(
            weight = 4,
            color = "#f72585",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        )
    })
    
    # 3. Descarga --------------------------------------------------------------
    output$download_edades <- downloadHandler(
      filename = function() {
        paste("demografia_edad_genero-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(st_drop_geometry(base_mapa_edad()), file)
      }
    )
    
    # 4. Fullscreen ------------------------------------------------------------
    observeEvent(input$mapGenerosEdadFullscreen, {
      shinyjs::runjs(sprintf("fullscreenGenerosEdad('%s')", ns("mapa_generos_edad")))
    })
    
  })
}