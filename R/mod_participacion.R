# ==============================================================================
# MÓDULO: Participación Electoral
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_participacion_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Participación Electoral", collapsible = TRUE, collapsed = TRUE,
        width = 12, solidHeader = FALSE, status = "success",
        p("Visualice los niveles de participación, lista nominal o cantidad de votos totales por sección.")
      ),
      box(width = 3, 
          shiny::selectInput(ns("participacion"), 
                             "¿Qué quieres observar?",
                             choices =  c("Lista Nominal", "Tasa de participación", "Cantidad de votos"),
                             selected = "Lista Nominal")
      ),
      box(width = 9,
          leafletOutput(ns("mapa_participacion"), height = "80vh")
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_participacion_server <- function(id, inputs_globales, secciones_base) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Preparación de Datos --------------------------------------------------
    datos_participacion <- reactive({
      # 'participacion' es un objeto global (leído en global.R)
      # Filtramos los datos de participación según la geografía seleccionada globalmente
      
      df_part <- participacion # Copia local
      
      # Si participacion es SF, quitamos geometría para el join (si es necesario)
      if(inherits(df_part, "sf")) df_part <- st_drop_geometry(df_part)
      
      # Unir con las secciones activas (que ya tienen el filtro geográfico aplicado)
      # Esto es más seguro que volver a aplicar el switch de inputs_globales
      secciones_ids <- secciones_base()$SECCION
      
      df_part %>% filter(SECCION %in% secciones_ids)
    })
    
    # 2. Renderizado del Mapa --------------------------------------------------
    output$mapa_participacion <- renderLeaflet({
      req(input$participacion)
      
      # Mapeo de input a nombre de columna en dataframe 'participacion'
      # Ajusta estos nombres según tus columnas reales en el RDS 'participacion'
      # Asumo que las columnas se llaman "lista", "participacion", "votos" o similar.
      # Basado en tu código original:
      variable <- case_when(
        input$participacion == "Lista Nominal" ~ "lista",
        input$participacion == "Tasa de participación" ~ "participacion", 
        TRUE ~ "votos"
      )
      
      # Preparar datos para el mapa
      dta_mapa <- datos_participacion() %>% 
        mutate(valor = .[[variable]]) %>%
        mutate(valor = ifelse(is.infinite(valor) | is.nan(valor), NA, valor))
      
      # Unir con geometría
      shp_mapa <- secciones_base() %>% 
        select(SECCION, NOMBRE) %>% # Mantener columnas clave
        left_join(dta_mapa, by = "SECCION") %>% 
        st_transform(crs = "+proj=longlat +datum=WGS84")
      
      validate(need(nrow(shp_mapa) > 0, "No hay datos de participación para esta selección."))
      
      # Configuración de Leyenda y Paleta
      # Bins: limitamos superior si es tasa de participación
      bins <- if (variable == "participacion") {
        vals <- shp_mapa$valor
        vals <- vals[!is.na(vals) & vals <= 1] # ignorar errores >100%
        unique(quantile(vals, probs = seq(0, 1, length.out = 10), na.rm = TRUE))
      } else {
        unique(quantile(shp_mapa$valor, probs = seq(0, 1, length.out = 10), na.rm = TRUE))
      }
      
      pal <- colorBin(palette = "YlOrRd", domain = shp_mapa$valor, bins = bins, na.color = "transparent")
      
      # Formato de etiquetas
      lab_format <- switch(
        variable,
        "participacion" = labelFormat(suffix = "%", transform = function(x) x * 100, digits = 1),
        labelFormat(big.mark = ",", digits = 0)
      )
      
      # Popup
      popup_valor <- case_when(
        is.na(shp_mapa$valor) ~ "No disponible",
        variable == "participacion" & shp_mapa$valor > 1 ~ paste0(round(shp_mapa$valor * 100, 1), "% (supera el 100%)"),
        variable == "participacion" ~ paste0(round(shp_mapa$valor * 100, 1), "%"),
        TRUE ~ formatC(shp_mapa$valor, format = "d", big.mark = ",")
      )
      
      popup_html <- paste0(
        "<b>Sección: </b>", shp_mapa$SECCION, "<br/>",
        "<b>Municipio: </b>", shp_mapa$NOMBRE, "<br/>",
        "<b>", input$participacion, ": </b>", popup_valor
      )
      
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addLegend(
          pal = pal,
          values = if (variable == "participacion") c(0, 1) else shp_mapa$valor,
          title = input$participacion,
          labFormat = lab_format,
          opacity = 1,
          position = "bottomright"
        ) %>%
        addPolygons(
          data = shp_mapa,
          color = "black",
          fillColor = ~pal(valor),
          popup = popup_html,
          stroke = TRUE,
          fillOpacity = 0.7,
          weight = 0.5,
          highlightOptions = highlightOptions(
            weight = 3, color = "#f72585", fillOpacity = 0.8, bringToFront = TRUE
          )
        )
    })
  })
}