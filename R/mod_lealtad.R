# ==============================================================================
# MÓDULO: Lealtad Partidista
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_lealtad_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 12,
          title = "Lealtad Partidista Histórica", status = "primary", solidHeader = TRUE,
          p("El mapa muestra qué partido ha ganado más veces en cada sección considerando todas las elecciones históricas disponibles. El nivel de lealtad indica la consistencia del voto."),
          leafletOutput(ns("mapa_lealtad"), height = "80vh")
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_lealtad_server <- function(id, base_ganadores_pr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$mapa_lealtad <- renderLeaflet({
      
      datos_base <- base_ganadores_pr() # Reactivo pasado desde server principal
      req(nrow(datos_base) > 0)
      
      # 1. Identificar columnas de elecciones
      # Buscamos columnas que coincidan con patrones de elecciones (gober, alcalde, etc.)
      cols_elecciones <- names(datos_base)[grepl("gober|alcalde|dip|senado|pres", names(datos_base))]
      
      # 2. Calcular victorias por partido y sección
      lealtad_partidista <- datos_base %>%
        select(SECCION, all_of(cols_elecciones)) %>%
        st_drop_geometry() %>% 
        pivot_longer(-SECCION, names_to = "eleccion", values_to = "partido") %>%
        # Filtros de limpieza
        filter(!is.na(partido), !partido %in% c("NULL", "0", "otro")) %>%
        group_by(SECCION, partido) %>%
        summarise(victorias = n(), .groups = "drop") %>%
        arrange(SECCION, desc(victorias)) %>%
        group_by(SECCION) %>%
        slice_max(order_by = victorias, n = 1, with_ties = FALSE) %>% # Ganador más frecuente
        ungroup()
      
      # 3. Clasificar nivel de lealtad
      lealtad_partidista <- lealtad_partidista %>%
        mutate(lealtad = case_when(
          victorias <= 3 ~ "Baja lealtad",
          victorias == 4 ~ "Lealtad media",
          victorias == 5 ~ "Alta lealtad",
          victorias >= 6 ~ "Muy alta lealtad"
        )) %>%
        mutate(partido = as.character(partido))
      
      # 4. Unir con geometría
      # Usamos datos_base para recuperar la geometría
      shp_lealtad <- datos_base %>%
        select(SECCION) %>%
        left_join(lealtad_partidista, by = "SECCION") %>%
        filter(!is.na(partido)) %>%
        st_transform(crs = "+proj=longlat +datum=WGS84")
      
      validate(need(nrow(shp_lealtad) > 0, "No hay datos suficientes para calcular la lealtad."))
      
      # 5. Paleta de Colores
      niveles_partido <- c("INDEPE", "MC", "morena", "PAN", "PRI", "PT", "PVEM", "VERDE")
      # Ajustar mayúsculas/minúsculas según tus datos reales. Aquí normalizo a mayúsculas para asegurar.
      shp_lealtad$partido <- toupper(shp_lealtad$partido)
      
      pal_partido <- colorFactor(
        palette = c("gray20", "orange", "#652123", "blue", "#F51F23", "purple", "#73A64E", "#4daf4a"),
        domain = shp_lealtad$partido,
        na.color = "lightgray"
      )
      
      # 6. Popups
      popup_html <- paste0(
        "<b>Sección:</b> ", shp_lealtad$SECCION, 
        "<hr style='border: none; border-top: 1px solid #ddd; margin: 8px 0;'>",
        "<b>Partido dominante:</b> ", shp_lealtad$partido, "<br>",
        "<b>Victorias:</b> ", shp_lealtad$victorias, "<br>",
        "<b>Lealtad:</b> ", shp_lealtad$lealtad
      )
      
      # 7. Mapa
      leaflet(shp_lealtad) %>%
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(
          fillColor = ~pal_partido(partido),
          opacity = 1,
          fillOpacity = 0.65,
          label = ~ SECCION,
          popup = popup_html,
          color = "#596475",
          weight = 1.3,
          highlightOptions = highlightOptions(
            weight = 4, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE
          )
        ) %>%
        addLegend(
          title = "Partido Hegemónico",
          pal = pal_partido,
          values = ~partido,
          opacity = 1,
          position = "bottomright"
        )
    })
  })
}