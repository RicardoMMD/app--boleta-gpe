# ==============================================================================
# MÓDULO: Perfil de Colonias
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_colonias_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # --- Guía de Uso ---
    fluidRow(
      box(
        title = "Guía de Uso: Perfiles de Colonias",
        collapsible = TRUE, collapsed = TRUE,
        width = 12, solidHeader = TRUE, status = "primary", 
        p("Esta sección te permite explorar los datos demográficos y electorales a nivel de colonia."),
        tags$ul(
          tags$li(tags$strong("Paso 1:"), " Elige una colonia en el 'Panel de Control'. Su información demográfica aparecerá automáticamente."),
          tags$li(tags$strong("Paso 2:"), " Explora las gráficas de resultados por elección e histórico por partido.")
        )
      )
    ),
    
    # --- Contenido Principal ---
    fluidRow(
      # Columna Izquierda: Controles e Info Texto
      column(width = 4,
             box(
               title = "Panel de Control", status = "primary", solidHeader = TRUE, width = NULL,
               
               # Selector dinámico (se llena desde server)
               shiny::selectInput(ns("colonia_seleccionada"), "Selecciona la colonia de tu interés:", choices = NULL),
               
               hr(),
               
               h4("Información Demográfica"),
               textOutput(ns("texto_colonia")),
               
               hr(),
               
               h4("Filtros para Gráficos"),
               shiny::selectInput(ns("eleccion_colonia"), 
                                  "1. Selecciona una elección:",
                                  choices =  c("Alcalde 2024" = "ayunt24", "Diputado Local 2024" = "dipl24", "Diputado Federal 2024" = "fed24", 
                                               "Alcalde 2021" = "ayunt21", "Gobernador 2021" = "gob21", 
                                               "Alcalde 2018" = "ayunt18", "Presidente 2018" = "pres18",
                                               "Alcalde 2015" = "ayunt15", "Gobernador 2015" = "gob15")
               ),
               shiny::selectInput(ns("partido_colonia"), 
                                  "2. Selecciona un partido:",
                                  choices = c("PAN" = "pan", "PRI" = "pri", "Morena" = "morena", "MC" = "mc", "Verde" = "verde", "PT" = "pt", "Independiente" = "indep")
               )
             )
      ),
      
      # Columna Derecha: Gráficos
      column(width = 8,
             tabBox(
               title = "Visualizaciones", width = NULL,
               
               tabPanel("Resultados por Elección",
                        p("Muestra la proporción de votos que cada partido obtuvo en la elección seleccionada."),
                        plotOutput(ns("grafico_colonia")),
                        downloadButton(ns("download_votos_colonia"), "Descargar CSV")
               ),
               
               tabPanel("Histórico por Partido",
                        p("Muestra el desempeño histórico del partido seleccionado."),
                        plotOutput(ns("grafico_colonia_tiempo")),
                        downloadButton(ns("download_votos_colonia_tiempo"), "Descargar CSV")
               )
             )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_colonias_server <- function(id, secciones_reactivas) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Actualizar Selector de Colonias ---------------------------------------
    observe({
      # Filtrar colonias disponibles según las secciones activas globalmente
      secciones_ids <- secciones_reactivas()$SECCION
      
      # 'colonias' es un dataframe Global (cargado en global.R)
      colonias_filtradas <- colonias %>%
        filter(SECCIÓN %in% secciones_ids)
      
      # Actualizar input
      shiny::updateSelectInput(
        session, 
        "colonia_seleccionada", 
        choices = sort(unique(colonias_filtradas$COLONIA))
      )
    })
    
    # 2. Procesamiento de Datos de la Colonia ----------------------------------
    
    # Identificar secciones de la colonia seleccionada
    secciones_de_colonia <- reactive({
      req(input$colonia_seleccionada)
      colonias %>% 
        filter(COLONIA == input$colonia_seleccionada) %>% 
        pull(SECCIÓN) %>% 
        unique()
    })
    
    # Estadísticas Demográficas (secciones_sd Global)
    base_colonias_sd <- reactive({
      ids <- secciones_de_colonia()
      req(length(ids) > 0)
      
      secciones_sd %>%
        filter(SECCION %in% ids) %>%
        summarise(
          population = sum(POBTOT, na.rm = TRUE),
          pnacent = sum(PNACENT * POBTOT, na.rm = TRUE) / sum(POBTOT, na.rm = TRUE),
          graproes = sum(GRAPROES * POBTOT, na.rm = TRUE) / sum(POBTOT, na.rm = TRUE),
          jovenprop = sum(p_joven * POBTOT, na.rm = TRUE) / sum(POBTOT, na.rm = TRUE),
          viejoprop = sum(p_vieja * POBTOT, na.rm = TRUE) / sum(POBTOT, na.rm = TRUE)
        )
    })
    
    # Votos para Gráfico 1 (Barras por Partido)
    votos_colonia <- reactive({
      req(input$eleccion_colonia)
      ids <- secciones_de_colonia()
      
      # 'cant_votos' es Global
      cant_votos %>% 
        filter(seccion %in% ids) %>% 
        group_by(eleccion, partido) %>% 
        summarise(votos_total = sum(votos, na.rm = TRUE), .groups = "drop") %>% 
        group_by(eleccion) %>% 
        mutate(prop = votos_total / sum(votos_total)) %>% 
        ungroup() %>% 
        filter(eleccion == input$eleccion_colonia, !is.na(partido))
    })
    
    # Votos para Gráfico 2 (Serie de Tiempo)
    votos_colonia_tiempo <- reactive({
      req(input$partido_colonia)
      ids <- secciones_de_colonia()
      
      cant_votos %>% 
        filter(seccion %in% ids) %>% 
        group_by(eleccion, partido) %>% 
        summarise(votos_total = sum(votos, na.rm = TRUE), .groups = "drop") %>% 
        group_by(eleccion) %>% 
        mutate(prop = votos_total / sum(votos_total)) %>% 
        ungroup() %>% 
        filter(partido == input$partido_colonia)
    })
    
    # 3. Renderizado de Texto e Imágenes ---------------------------------------
    
    output$texto_colonia <- renderText({
      stats <- base_colonias_sd()
      ids <- secciones_de_colonia()
      
      paste0(
        "Colonia: ", input$colonia_seleccionada, ".\n",
        "Secciones: ", paste(ids, collapse = ", "), ".\n",
        "Población Total: ", scales::comma(stats$population), " personas.\n",
        "Escolaridad Promedio: ", round(stats$graproes, 2), " años.\n",
        "Nacidos en entidad: ", scales::percent(stats$pnacent, 0.1), ".\n",
        "Población Joven: ", scales::percent(stats$jovenprop, 0.1), ".\n",
        "Población Mayor: ", scales::percent(stats$viejoprop, 0.1), "."
      )
    })
    
    output$grafico_colonia <- renderPlot({
      dta <- votos_colonia()
      validate(need(nrow(dta) > 0, "No hay datos para esta elección."))
      
      ggplot(dta, aes(x = reorder(partido, -prop), y = prop, fill = partido)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent) +
        labs(
          x = "Partido", 
          y = "Porcentaje de Voto", 
          title = paste("Resultados:", input$eleccion_colonia)
        ) +
        theme_minimal() +
        scale_fill_manual(values = c(
          "pri" = "red", "pan" = "blue", "morena" = "maroon", 
          "mc" = "orange", "verde" = "#6CB655", "pt" = "#E7DE08", "indep" = "purple"
        ), na.value = "gray")
    })
    
    output$grafico_colonia_tiempo <- renderPlot({
      dta <- votos_colonia_tiempo()
      validate(need(nrow(dta) > 0, "No hay datos históricos para este partido."))
      
      # Definir orden cronológico de elecciones
      orden_elecciones <- c("ayunt15", "gob15", "dl15", "fed15",
                            "ayunt18", "dipl18", "fed18", "sen18", "pres18", 
                            "ayunt21", "gob21", "dl21", "fed21",
                            "ayunt24", "dipl24", "fed24", "pres24", "sen24")
      
      ggplot(dta, aes(x = factor(eleccion, levels = orden_elecciones), y = prop, group = 1)) +
        geom_line(color = "grey") +
        geom_point(aes(color = prop), size = 3) +
        scale_y_continuous(labels = scales::percent) +
        labs(
          x = "Elección", 
          y = "Porcentaje de Voto", 
          title = paste("Evolución:", toupper(input$partido_colonia))
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    })
    
    # 4. Descargas -------------------------------------------------------------
    output$download_votos_colonia <- downloadHandler(
      filename = function() { paste("colonia_resultados-", Sys.Date(), ".csv", sep="") },
      content = function(file) { write.csv(votos_colonia(), file) }
    )
    
    output$download_votos_colonia_tiempo <- downloadHandler(
      filename = function() { paste("colonia_historico-", Sys.Date(), ".csv", sep="") },
      content = function(file) { write.csv(votos_colonia_tiempo(), file) }
    )
    
  })
}