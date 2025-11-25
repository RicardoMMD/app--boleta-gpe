# ==============================================================================
# MÓDULO: Distancia Entre Primero y Segundo Lugar
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_diferencias_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Distancia Histórica: Primer vs. Segundo Lugar", collapsible = TRUE, collapsed = TRUE, width = 12, status = "info",
        p("Analiza la diferencia de votos entre el primer y segundo lugar en cada sección para la elección seleccionada.")
      )
    ),
    fluidRow(
      column(
        width = 3,
        box(width = 12, status = "warning",
            shiny::selectInput(ns("eleccion_diferencia"),
                               "Seleccione la elección a analizar:",
                               choices = choices_elections_sombra) # Variable Global
        )
      ),
      column(
        width = 9,
        tabBox(
          id = ns("resultados_tabs"), 
          width = 12,
          
          # Primer Tab: Mapa
          tabPanel(
            title = "Mapa", 
            icon = icon("map"),
            leafletOutput(ns("mapa_diferencia"), height = "80vh"),
            div(style = "margin-top: 10px;",
                shiny::actionButton(ns("mapDiferenciaFullscreen"), "Pantalla Completa", icon = icon("expand"))
            )
          ),
          
          # Segundo Tab: Tabla
          tabPanel(
            title = "Tabla", 
            icon = icon("table"),
            div(
              style = "padding-bottom: 10px;",
              downloadBttn(
                outputId = ns("dw_csv_distancia"), 
                label = "Descargar CSV", 
                icon = icon("file-csv"), 
                size = "sm", 
                color = "warning", 
                style = "simple"
              )
            ),
            DT::DTOutput(ns("dt_diferencia_diferencia"))
          )
        )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_diferencias_server <- function(id, secciones_reactivas) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Valores reactivos para almacenar los datos calculados y usarlos en tabla/descarga
    reactive_diferencia <- reactiveValues(base = NULL)
    
    # 1. Procesamiento de Datos ------------------------------------------------
    # Nota: res_trab debe estar cargado en global.R
    query_distancia <- reactive({
      req(input$eleccion_diferencia)
      
      res_trab %>% 
        filter(eleccion %in% input$eleccion_diferencia) %>% 
        arrange(seccion, desc(proporcion)) %>% 
        group_by(seccion) %>%
        mutate(
          diff = (lead(proporcion) - proporcion) * -1,
          proporcion = proporcion
        ) %>%  
        filter(row_number() == 1) # Quedarse con el ganador
    })
    
    # 2. Renderizado del Mapa --------------------------------------------------
    output$mapa_diferencia <- renderLeaflet({
      
      base <- query_distancia()
      shp <- secciones_reactivas() # Argumento del módulo
      
      # Unir datos con geometría
      shp_base <- shp %>% 
        left_join(base, by = c("SECCION" = "seccion")) %>% 
        st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
        filter(!is.na(partido))
      
      validate(
        need(nrow(shp_base) > 0, "No hay datos disponibles para esta selección.")
      )
      
      # Guardar en reactiveValues para la tabla/descarga
      reactive_diferencia$base <- shp_base %>% st_drop_geometry()
      
      # Funciones auxiliares de opacidad (definidas localmente o globales)
      rescale_opacity <- function(x, min_opacity = 0.3, max_opacity = 0.9) {
        ((x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))) * (max_opacity - min_opacity) + min_opacity
      }
      
      apply_opacity_threshold <- function(x, threshold = 0.42) {
        pmin(x, threshold) # Simplificación visual
      }
      
      # Calcular opacidad basada en la competitividad (margen de victoria)
      # Nota: Hay un pequeño bug lógico en el código original con apply_opacity_threshold
      # que normalizaba raro. Usaré una lógica segura aquí:
      # A mayor diferencia, mayor opacidad (más seguro el triunfo)
      shp_base$opacidad <- rescale_opacity(shp_base$diff, min_opacity = 0.4, max_opacity = 0.9)
      
      # Paleta de colores
      niveles_partidos <- c("mc", "pan", "pri", "morena", "indep", "verde", "pt", "otro")
      # Aseguramos que los niveles coincidan con los datos (tolower para seguridad si res_trab viene en minusculas)
      shp_base$partido <- factor(tolower(shp_base$partido), levels = niveles_partidos)
      
      pal <- colorFactor(c("orange","blue", "red", "brown", "purple", "#6CB655", "#E7DE08", "black"), 
                         domain = shp_base$partido, na.color = "#ccc")
      
      # Popup
      shp_base <- shp_base %>% 
        mutate(
          popup_html = paste0(
            "<div style='font-family: sans-serif; padding: 5px; max-width: 250px;'>",
            "<h4 style='margin: 0 0 8px 0; color: #003366;'>Sección: ", SECCION, "</h4>",
            "<hr style='border: none; border-top: 1px solid #ddd; margin: 8px 0;'>",
            "<strong>Ganador:</strong> ", toupper(partido), "<br>",
            "<strong>Votación:</strong> ", scales::percent(proporcion, accuracy = 0.01), "<br>",
            "<strong style='color: #E55934;'>Margen de Victoria:</strong> ", scales::percent(diff, accuracy = 0.01),
            "</div>"
          )
        )
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addLegend(pal = pal, values = shp_base$partido, title = "Ganador", position = "bottomright") %>% 
        addPolygons(
          data = shp_base,
          label = ~ SECCION,
          fillColor = ~pal(partido), 
          popup = ~ popup_html, 
          stroke = TRUE, 
          fillOpacity = ~opacidad, # Opacidad variable
          color = "#596475",
          weight = 1.3,
          highlightOptions = highlightOptions(weight = 3, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE)
        ) %>% 
        addControl(paste0("Resultados: ", input$eleccion_diferencia), position = "bottomleft", className="map-title")
    })
    
    # 3. Renderizado de Tabla --------------------------------------------------
    output$dt_diferencia_diferencia <- DT::renderDT({
      req(reactive_diferencia$base)
      
      data <- reactive_diferencia$base
      
      # Seleccionar y renombrar columnas para visualización
      data_cols <- data %>% 
        select(any_of(c("ENTIDAD", "DISTRITO", "DISTRITO_L", "NOMBRE", "SECCION", "pobtotal", "partido", "eleccion", "diff"))) %>%
        mutate(
          diff = round(diff * 100, 2),
          partido = toupper(partido)
        ) 
      
      colnames(data_cols) <- c("Entidad", "Distrito Federal", "Distrito Local", "Municipio", "Sección", 
                               "Población total", "Partido ganador", "Elección", "Diferencia (%)")
      
      datatable(
        data = data_cols,
        style = 'bootstrap5',
        class = 'header stripe',
        extensions = c("Scroller"),
        selection = "single",
        options = list(
          searching = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip', 
          scrollX = TRUE
        )
      )
    })
    
    # 4. Descarga --------------------------------------------------------------
    output$dw_csv_distancia <- downloadHandler(
      filename = function() {
        paste0("distancia_1_2_", format(Sys.Date(), "%d_%m_%Y"), ".csv")
      },
      content = function(file) {
        req(reactive_diferencia$base)
        write.csv(reactive_diferencia$base, file, row.names = FALSE)
      }
    )
    
    # 5. Fullscreen ------------------------------------------------------------
    observeEvent(input$mapDiferenciaFullscreen, {
      shinyjs::runjs(sprintf("fullscreenDiferencia('%s')", ns("mapa_diferencia")))
    })
    
  })
}