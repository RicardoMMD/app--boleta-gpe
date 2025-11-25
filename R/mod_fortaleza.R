# ==============================================================================
# MÓDULO: Fortaleza Electoral (Modelo Predictivo)
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_fortaleza_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Fortaleza Electoral (Machine Learning)", collapsible = TRUE, collapsed = TRUE, width = 12, status = "success",
        p("Explore la fortaleza relativa de un partido en cada sección, calculada con modelos de machine learning (Regresión Logística) basados en datos históricos. Seleccione el partido para ver su probabilidad de ganar en el mapa.")
      )
    ),
    fluidRow(
      box(
        width = 3,
        shiny::selectInput(ns("partido_predecir"), 
                           "Selecciona el partido a analizar:", 
                           choices = c("PRI", "morena", "MC", "PAN", "VERDE", "pt"))
      )
    ),
    box(
      width = 12,
      h3("Mapa de Fortaleza Electoral"),
      leafletOutput(ns("mapa_fortaleza"), height = "80vh"),
      
      div(style = "margin-top: 10px; display: flex; justify-content: space-between;",
          shiny::actionButton(ns("mapFortalezaFullscreen"), "Pantalla Completa", icon = icon("expand")),
          downloadButton(ns("download_fortalezas"), "Descargar Tabla (CSV)")
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_fortaleza_server <- function(id, secciones_reactivas) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Cálculo del Modelo ----------------------------------------------------
    base_modelo <- reactive({
      req(input$partido_predecir)
      
      # NOTA: Usamos 'base_ganadores' global para entrenar el modelo con TODOS los datos disponibles
      # para tener mayor robustez estadística, y luego filtramos para el mapa.
      
      # Preparar datos para el modelo (dummy variable para el target)
      base_entrenamiento <- base_ganadores %>% 
        rename(interes_elec = dip_fed_21) %>% # Usamos dip_fed_21 como proxy reciente
        select(!ends_with("21")) %>% # Evitar data leakage de la misma elección
        select(!c(geometry, distrito, poblacion)) %>% 
        mutate(
          resultado = case_when(
            interes_elec %in% input$partido_predecir ~ 1, 
            TRUE ~ 0
          )
        ) %>% 
        mutate(across(contains("_"), as.factor)) # Variables categóricas
      
      # Entrenar Modelo de Regresión Logística Binaria
      # Usamos elecciones pasadas como predictores
      modelo <- glm(
        resultado ~ alcalde_15 + dip_local_15 + dip_local_18 + senado_18 +
          alcalde_18 + dip_fed18 + pres_18, 
        data = base_entrenamiento, 
        family = "binomial"
      )
      
      # Generar predicciones (Probabilidades)
      base_con_predicciones <- cbind(base_entrenamiento, Pred = predict(modelo, base_entrenamiento, type = "response")) 
      
      # Categorizar en Quintiles (Grupos de Fortaleza)
      # Nota: Usamos tryCatch por si no hay varianza suficiente
      tryCatch({
        base_con_predicciones$grupo <- cut(
          base_con_predicciones$Pred, 
          breaks = quantile(base_con_predicciones$Pred, probs = seq(0, 1, 0.2), na.rm = TRUE), 
          include.lowest = TRUE,
          labels = FALSE # Etiquetas 1 a 5
        )
      }, error = function(e) {
        base_con_predicciones$grupo <- 1 # Fallback si falla el corte
      })
      
      # Unir con la geometría seleccionada por el usuario
      secciones_reactivas() %>% 
        left_join(base_con_predicciones, by = c("SECCION" = "seccion")) %>% 
        filter(!is.na(grupo)) %>% # Eliminar secciones sin datos
        st_transform(crs = "+proj=longlat +datum=WGS84")
    })
    
    # 2. Renderizado del Mapa --------------------------------------------------
    output$mapa_fortaleza <- renderLeaflet({
      
      base_mapa <- base_modelo()
      
      validate(need(nrow(base_mapa) > 0, "No hay datos suficientes para generar el modelo."))
      
      # Etiquetas descriptivas para los grupos
      base_mapa <- base_mapa %>% 
        mutate(
          probabilidad_desc = case_when(
            grupo == 1 ~ "Muy baja fortaleza", 
            grupo == 2 ~ "Baja fortaleza",
            grupo == 3 ~ "Mediana fortaleza",
            grupo == 4 ~ "Alta fortaleza",
            grupo >= 5 ~ "Muy alta fortaleza",
            TRUE ~ "Sin datos"
          )
        )
      
      # Definir factor ordenado para la leyenda
      niveles_fortaleza <- c("Muy baja fortaleza", "Baja fortaleza", "Mediana fortaleza", "Alta fortaleza", "Muy alta fortaleza")
      base_mapa$probabilidad_desc <- factor(base_mapa$probabilidad_desc, levels = niveles_fortaleza)
      
      pal <- colorFactor(
        palette = c("#caf0f8","#90e0ef", "#00b4d8", "#0077b6", "#03045e"),
        domain = base_mapa$probabilidad_desc
      )
      
      popup_html <- paste0(
        "<b>Sección: </b>", base_mapa$SECCION, "<br>",
        "<b>Probabilidad estimada: </b>", round(base_mapa$Pred * 100, 1), "%<br>",
        "<b>Nivel: </b>", base_mapa$probabilidad_desc
      )
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addLegend(
          pal = pal, 
          values = base_mapa$probabilidad_desc, 
          title = "Nivel de Fortaleza",
          position = "bottomright"
        )  %>% 
        addPolygons(
          data = base_mapa, 
          color = "#596475", 
          fillColor = ~pal(probabilidad_desc),
          popup = popup_html, 
          stroke = TRUE, 
          fillOpacity = 0.8, 
          weight = 0.5,
          highlightOptions = highlightOptions(
            weight = 3, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE
          )
        )
    })
    
    # 3. Descarga --------------------------------------------------------------
    output$download_fortalezas <- downloadHandler(
      filename = function() { paste("fortaleza_electoral-", Sys.Date(), ".csv", sep="") },
      content = function(file) { write.csv(st_drop_geometry(base_modelo()), file) }
    )
    
    # 4. Fullscreen ------------------------------------------------------------
    observeEvent(input$mapFortalezaFullscreen, {
      shinyjs::runjs(sprintf("fullscreenFortaleza('%s')", ns("mapa_fortaleza")))
    })
    
  })
}