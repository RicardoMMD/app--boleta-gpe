# ==============================================================================
# MÓDULO: Rendimiento Histórico (Voto Diferenciado)
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_rendimiento_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # --- Panel de Control ---
      column(width = 3,
             box(
               title = "Rendimiento Histórico por Partido", collapsible = TRUE, collapsed = TRUE, width = 12, status = "success",
               p("Descubre los bastiones históricos de un partido. Simplemente elige un partido y las elecciones que deseas analizar, y el mapa te mostrará al instante las secciones que ha ganado.")
             ),
             box(
               width = 12, 
               shiny::selectInput(ns("partido_ganador"), 
                                  "Selecciona el partido de interés", 
                                  choices = c("MC","PAN","PRI","VERDE","PT","MORENA","INDEP")),
               
               shiny::selectInput(ns("slt_candidatura_rh"), 
                                  "Selecciona las candidaturas ganadas a visualizar",
                                  multiple = TRUE, 
                                  # Selecciona por defecto el primer elemento de la lista global
                                  selected = choices_rendimiento_historico[[1]][1],
                                  choices = choices_rendimiento_historico
               )
             )
      ),
      
      # --- Visualización ---
      column(width = 9,
             box(width = 12,
                 leafletOutput(ns("mapa_rendimiento_historico"), height = "80vh"),
                 
                 div(style = "margin-top: 10px; display: flex; justify-content: space-between;",
                     shiny::actionButton(ns("mapPriFullscreen"), "Pantalla Completa", icon = icon("expand")),
                     downloadButton(ns("download_quitar"), "Descargar Tabla (CSV)")
                 )
             )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_rendimiento_server <- function(id, secciones_reactivas) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Procesamiento de Datos ------------------------------------------------
    dta_rendimiento_historico <- reactive({
      req(input$partido_ganador, input$slt_candidatura_rh)
      
      # Paso 1: Identificar ganadores por sección y elección
      # Nota: Esto se hace sobre la base completa 'cant_votos_nl' (Global)
      ganadores_votos_nl <- cant_votos_nl %>%
        group_by(seccion, eleccion_año_id) %>%  
        filter(votos == max(votos, na.rm = TRUE)) %>%         
        ungroup()
      
      # Paso 2: Filtrar por selección del usuario
      slt_ganadores_votos_nl <- ganadores_votos_nl %>% 
        filter(
          eleccion_año_id %in% input$slt_candidatura_rh, 
          partido == input$partido_ganador
        )
      
      return(slt_ganadores_votos_nl)
    })
    
    # 2. Renderizado del Mapa --------------------------------------------------
    output$mapa_rendimiento_historico <- renderLeaflet({
      
      # Datos filtrados (solo donde ganó el partido seleccionado)
      dta_ganadores <- dta_rendimiento_historico()
      
      # Agrupar si hay múltiples elecciones seleccionadas
      # (Ej. Si ganó Alcalde 2015 y 2018, colapsamos en una fila por sección)
      dta_ganadores_slt <- dta_ganadores %>% 
        group_by(seccion, partido) %>%
        summarise(
          eleccion_año_ids = paste(eleccion_año_id, collapse = "; "),
          votos = mean(votos, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Unir con la geometría (secciones filtradas por el sidebar global)
      secciones_df <- secciones_reactivas() %>%
        st_transform(crs = "+proj=longlat +datum=WGS84")
      
      shp_ganadores_secciones <- left_join(
        secciones_df,
        dta_ganadores_slt %>% mutate(seccion = as.character(seccion)),
        by = c("SECCION" = "seccion")
      )
      
      # Construcción de Popups Dinámicos
      shp_ganadores_secciones <- shp_ganadores_secciones %>%
        mutate(
          pobtotal_display = ifelse(is.na(pobtotal), "No disponible", format(pobtotal, big.mark=",")),
          votos_display = ifelse(is.na(votos), "No disponible", format(round(votos), big.mark=",")),
          
          proporcion_display = case_when(
            is.na(votos) | is.na(pobtotal) | pobtotal == 0 ~ "No disponible", 
            TRUE ~ paste0(round((votos / pobtotal) * 100, 2), "%") 
          ),
          
          popups_dinamicos = ifelse(
            is.na(partido), # Si es NA, significa que NO ganó el partido seleccionado en las elecciones elegidas
            
            # --- Caso 1: El partido NO ganó ---
            paste0(
              "<h4>Sección: ", SECCION, "</h4>", 
              "<strong>Municipio: </strong>", NOMBRE, "<br>",
              "<strong>Población: </strong>", pobtotal_display, "<hr>", 
              "<em>El partido '", input$partido_ganador, "' no obtuvo victoria en esta sección o no aplica para las elecciones filtradas.</em>"
            ),
            
            # --- Caso 2: El partido SÍ ganó ---
            paste0(
              "<h4>Sección: ", SECCION, "</h4>",
              "<strong>Municipio: </strong>", NOMBRE, "<br>",
              "<strong>Población: </strong>", pobtotal_display, "<hr>", 
              "<strong>Partido Ganador: </strong>", partido, "<br>", 
              "<strong>Elección(es) Ganada(s): </strong>", eleccion_año_ids, "<br>", 
              "<strong>Votos promedio: </strong>", votos_display, "<br>",
              "<strong>Proporción votos/población: </strong>", proporcion_display
            )
          )
        )
      
      # Paleta de colores local (para asegurar que coincida con el input)
      # Nota: Usamos paleta_partidos GLOBAL si está disponible, o definimos fallback
      color_partido_actual <- if(!is.null(paleta_partidos[input$partido_ganador])) {
        paleta_partidos[[input$partido_ganador]]
      } else {
        "#333333" # Color por defecto si no encuentra el partido
      }
      
      leaflet(data = shp_ganadores_secciones) %>%
        addProviderTiles(providers$CartoDB.Voyager) %>%
        
        addPolygons(
          # Si 'eleccion_año_ids' es NA, color gris (no ganó). Si tiene dato, color del partido.
          fillColor = ~ifelse(is.na(eleccion_año_ids), "#dad7cd", color_partido_actual),
          fillOpacity = 0.7,
          color = "#596475",
          weight = 1.5,
          stroke = TRUE,
          popup = ~popups_dinamicos,
          label = ~paste0("Sección: ", SECCION),
          highlightOptions = highlightOptions(
            weight = 4,
            color = "#f72585",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        
        addLegend(
          position = "bottomright",
          colors = c(color_partido_actual, "#dad7cd"),
          labels = c(paste("Victorias de:", input$partido_ganador), "Sin victoria / Sin datos"),
          title = "Referencia"
        )
    })
    
    # 3. Descarga --------------------------------------------------------------
    output$download_quitar <- downloadHandler(
      filename = function() {
        paste("rendimiento_historico-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(st_drop_geometry(dta_rendimiento_historico()), file)
      }
    )
    
    # 4. Fullscreen ------------------------------------------------------------
    observeEvent(input$mapPriFullscreen, {
      shinyjs::runjs(sprintf("fullscreenPri('%s')", ns("mapa_rendimiento_historico")))
    })
    
  })
}