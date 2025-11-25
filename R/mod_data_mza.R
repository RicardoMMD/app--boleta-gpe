# ==============================================================================
# MÓDULO: Datos por Manzana y Sección
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_data_mza_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 1),
      column(width = 10,
             box(
               width = 12, title = "Instrucciones", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
               h3("Explora poblaciones y votos de secciones electorales a nivel manzana"),
               p("1. Utilice los controles principales en la barra lateral para seleccionar la geografía de su interés."),
               p("2. Utilice el selector de población (ej. 'Población total', 'Población de 18 o más años') ubicado sobre el mapa."),
               p("3. Haga clic en una sección del mapa para visualizar las manzanas que la componen, coloreadas por densidad de población."),
               p("4. A la derecha, se mostrarán opciones para elegir la elección y el año. Debajo, verá un resumen de la población y votos, junto a un gráfico de distribución."),
               p("5. Para regresar, haga clic en la flecha amarilla a la izquierda del título de la sección."),
               h4("Consideraciones:"),
               p("Las poblaciones corresponden al Censo 2020. Únicamente se mapean y contabilizan manzanas urbanas.")
             )
      ),
      column(width = 1)
    ),
    
    fluidRow(
      column(width = 1),
      column(width = 11,
             fluidRow(
               style = "display:flex; align-items:end; gap:1.2rem; font-size:x-large; font-weight:500;",
               shinyjs::hidden(
                 # Nota: ns() también en actionBttn
                 actionBttn(inputId = ns("btt_return_secc_select"), label = NULL, style = "material-circle", color = "warning", icon = icon("chevron-left"))
               ),
               textOutput(ns("text_secc")) 
             )
      )
    ),
    br(),
    
    fluidRow(
      column(width = 1),
      column(width = 6,
             fluidRow(
               style="display:flex; align-items:end; gap:0.8rem;",
               # Contenedor para inputs que se ocultan/muestran
               shinyjs::hidden(
                 pickerInput(inputId = ns("pik_pob_mza"), label = "Seleccione población de interés", options = list(`live-search` = TRUE), choices = chs_poblaciones_manzanas, selected = chs_poblaciones_manzanas[1]),
                 # Este input es crucial, se actualiza desde el server
                 pickerInput(inputId = ns("pik_secc_col_mza"), label = "Búsqueda de sección", multiple = TRUE, options = list(`live-search` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), choices = "", selected = "")
               )
             )
      ),
      column(width = 4,
             fluidRow(
               style="display:flex; align-items:end; gap:0.8rem;",
               shinyjs::hidden(
                 pickerInput(inputId = ns("pik_elect_mza"), label = "Seleccione elección", options = list(`live-search` = TRUE), choices = list_eleccion, selected = list_eleccion[1]),
                 sliderTextInput(inputId = ns("slid_año_mza"), label = "Año de elección:", choices = "", grid = TRUE)
               )
             )
      ),
      column(width = 1)
    ),
    
    fluidRow(
      column(width = 1),
      column(width = 6,
             materialSwitch(inputId = ns("swch_colonias_mzas"), label = "Visualiza colonias", status = "success", value = FALSE),
             leafletOutput(ns("map_mzas"), width = "100%", height = 600)
      ),
      column(width = 4,
             fluidRow(
               valueBoxOutput(ns("vb_pob"), width = 6), # Usamos valueBoxOutput para renderizarlo completo desde server
               valueBoxOutput(ns("vb_vots"), width = 6)
             ),
             plotlyOutput(ns("py_ganadores"), width = "100%")
      ),
      column(width = 1)
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_data_mza_server <- function(id, secciones_reactivas, inputs_globales) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- VALORES REACTIVOS LOCALES ---
    foco_secc_select <- reactiveValues(secc_select = c())
    
    # 1. MAPA BASE -------------------------------------------------------------
    output$map_mzas <- renderLeaflet({
      # Inicializamos con bounds genéricos o basados en la selección global actual
      bounds <- secciones_reactivas() %>% st_transform(4326) %>% sf::st_bbox() %>% as.character()
      
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
        fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
    })
    
    # 2. REACCIÓN A CAMBIOS GLOBALES (Sidebar) ---------------------------------
    # Cuando cambia el municipio/distrito en el sidebar principal:
    observeEvent(c(
      inputs_globales$tipo(),
      inputs_globales$mun(),
      inputs_globales$fed(),
      inputs_globales$loc()
    ), {
      
      # a) Actualizar Picker de Secciones
      updatePickerInput(session = session, inputId = "pik_secc_col_mza",
                        choices = secciones_reactivas()$SECCION, selected = c())
      
      # b) Ocultar controles internos
      shinyjs::hide(id = "btt_return_secc_select", anim = TRUE, animType = 'fade')
      shinyjs::hide(id = "pik_elect_mza",         anim = TRUE, animType = 'fade')
      shinyjs::hide(id = "slid_año_mza",          anim = TRUE, animType = 'fade')
      shinyjs::hide(id = "pik_secc_col_mza",      anim = TRUE, animType = 'fade')
      shinyjs::hide(id = "pik_pob_mza",           anim = TRUE, animType = 'fade')
      
      # c) Resetear selección local
      foco_secc_select$secc_select <- c()
      
      # d) Resetear Mapa via Proxy
      bounds <- secciones_reactivas() %>% st_transform(4326) %>% st_bbox() %>% as.character()
      
      leafletProxy(ns("map_mzas")) %>%
        clearShapes() %>% clearControls() %>%
        fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
        addPolygons(data = secciones_reactivas() %>% st_transform(4326),
                    layerId = ~as.character(SECCION),
                    label = ~ SECCION,
                    fillOpacity = 0.6,
                    fillColor = '#168aad',
                    weight = 0.6, 
                    color = '#596475',
                    opacity = 1,
                    smoothFactor = 1,
                    highlightOptions = highlightOptions(color = '#f72585', fillOpacity = 0.3, weight = 2, bringToFront = TRUE))
    })
    
    # 3. SWITCH COLONIAS -------------------------------------------------------
    observeEvent(input$swch_colonias_mzas, {
      req(is.null(foco_secc_select$secc_select) || length(foco_secc_select$secc_select) == 0)
      
      if(input$swch_colonias_mzas){
        
        # Lógica para filtrar shape de colonias basada en inputs globales
        # Asumimos que shp_cols_x_secc y chs_mun_cols son GLOBALES
        shp_cols_x_secc_selected <- switch(inputs_globales$tipo(),
                                           "Municipio" = shp_cols_x_secc[shp_cols_x_secc$MUNICIPIO %in% as.numeric(chs_mun_cols[[inputs_globales$mun()]]),], 
                                           "DFederal"  = shp_cols_x_secc[shp_cols_x_secc$DISTRITO_F %in% as.numeric(inputs_globales$fed()),], 
                                           "DLocal"    = shp_cols_x_secc[shp_cols_x_secc$DISTRITO_L %in% as.numeric(inputs_globales$loc()),], 
                                           "Ninguno"   = shp_cols_x_secc
        )
        
        leafletProxy(ns("map_mzas")) %>%
          clearShapes() %>% clearControls() %>% 
          addPolygons(data = shp_cols_x_secc_selected %>% st_transform(4326), 
                      label = ~NOMBRE,
                      fillOpacity = 0.6, fillColor = '#f72585', weight = 1, 
                      color = '#596475', opacity = 1, smoothFactor = 1,
                      highlightOptions = highlightOptions(color = '#f72585', fillOpacity = 0.3, weight = 2, bringToFront = TRUE))
      } else {
        # Restaurar secciones
        bounds <- secciones_reactivas() %>% st_transform(4326) %>% st_bbox() %>% as.character()
        leafletProxy(ns("map_mzas")) %>%
          clearShapes() %>% clearControls() %>% 
          fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
          addPolygons(data = secciones_reactivas() %>% st_transform(4326), 
                      layerId = ~as.character(SECCION),
                      label = ~paste("Seccion:", SECCION),
                      fillOpacity = 0.6, fillColor = '#168aad', weight = 0.6, 
                      color = '#596475', opacity = 1, smoothFactor = 1,
                      highlightOptions = highlightOptions(color = '#f72585', fillOpacity = 0.3, weight = 2, bringToFront = TRUE))
      }
    })
    
    # 4. CLICK EN EL MAPA (Selección de Sección) -------------------------------
    observeEvent(input$map_mzas_shape_click, {
      click_id <- input$map_mzas_shape_click$id
      req(!is.null(click_id))
      
      # Lógica de toggle (añadir/quitar selección)
      if(click_id %in% foco_secc_select$secc_select){
        posicion <- which(foco_secc_select$secc_select == click_id)
        foco_secc_select$secc_select <- foco_secc_select$secc_select[-posicion]
      } else {
        foco_secc_select$secc_select <- append(foco_secc_select$secc_select, as.character(click_id))
      }
      
      updatePickerInput(session = session, inputId = "pik_secc_col_mza", selected = foco_secc_select$secc_select)
    })
    
    # 5. BOTÓN REGRESAR --------------------------------------------------------
    observeEvent(input$btt_return_secc_select, {
      
      # Ocultar controles
      shinyjs::hide(id = "btt_return_secc_select", anim = TRUE, animType = 'fade')
      shinyjs::hide(id = "pik_elect_mza",         anim = TRUE, animType = 'fade')
      shinyjs::hide(id = "slid_año_mza",          anim = TRUE, animType = 'fade')
      shinyjs::hide(id = "pik_pob_mza",           anim = TRUE, animType = 'fade')
      shinyjs::hide(id = "pik_secc_col_mza",      anim = TRUE, animType = 'fade')
      
      foco_secc_select$secc_select <- c()
      
      # Restaurar mapa base
      bounds <- secciones_reactivas() %>% st_transform(4326) %>% st_bbox() %>% as.character()
      
      leafletProxy(ns("map_mzas")) %>%
        clearShapes() %>% clearControls() %>% 
        fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
        addPolygons(data = secciones_reactivas() %>% st_transform(4326), 
                    label = ~SECCION, layerId = ~as.character(SECCION),
                    fillOpacity = 0.6, fillColor = '#168aad', weight = 0.7, 
                    color = '#596475', opacity = 1, smoothFactor = 1,
                    highlightOptions = highlightOptions(color = '#f72585', weight = 2, fillOpacity = 0.3, bringToFront = TRUE))
      
      # Limpiar gráfico
      output$py_ganadores <- renderPlotly({
        plot_ly(x=c(), y=c(), type = 'bar', orientation = 'h')
      })
    })
    
    # 6. ACTUALIZAR MAPA AL SELECCIONAR SECCIÓN (DRILL-DOWN) -------------------
    observeEvent(c(input$pik_secc_col_mza, input$pik_pob_mza, input$swch_colonias_mzas), {
      
      req(!is.null(foco_secc_select$secc_select))
      req(length(foco_secc_select$secc_select) > 0)
      
      # Mostrar controles
      shinyjs::show(id = "btt_return_secc_select", anim = TRUE, animType = 'fade')
      shinyjs::show(id = "pik_elect_mza",         anim = TRUE, animType = 'fade')
      shinyjs::show(id = "slid_año_mza",          anim = TRUE, animType = 'fade') 
      shinyjs::show(id = "pik_pob_mza",           anim = TRUE, animType = 'fade')
      shinyjs::show(id = "pik_secc_col_mza",      anim = TRUE, animType = 'fade')
      
      # Filtrar geometrías
      shp_secc_2024_mr = secciones_reactivas() %>% st_transform(4326)
      shp_secc_2024_mr_secc_select <- shp_secc_2024_mr[!(shp_secc_2024_mr$SECCION %in% input$pik_secc_col_mza),]
      
      # Manzanas (variable global: shp_mza2023_secc2024_cpv2020)
      shp_mza_2023_secc <- shp_mza2023_secc2024_cpv2020[shp_mza2023_secc2024_cpv2020$SECCION %in% as.integer(input$pik_secc_col_mza),]
      
      req(nrow(shp_mza_2023_secc) > 0)
      
      bounds <- shp_mza_2023_secc %>% st_bbox() %>% as.character()
      
      # Paleta dinámica
      vals <- as.numeric(shp_mza_2023_secc[[input$pik_pob_mza]])
      percentiles <- quantile(vals, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
      pal <- colorBin("YlOrRd", domain = vals, bins = unique(percentiles))
      
      # Colonias (opcional)
      shp_cols_x_secc_selected <- NULL
      if(input$swch_colonias_mzas){
        shp_cols_x_secc_selected <- switch(inputs_globales$tipo(),
                                           "Municipio" = shp_cols_x_secc[shp_cols_x_secc$MUNICIPIO %in% as.numeric(chs_mun_cols[[inputs_globales$mun()]]),], 
                                           "DFederal"  = shp_cols_x_secc[shp_cols_x_secc$DISTRITO_F %in% as.numeric(inputs_globales$fed()),], 
                                           "DLocal"    = shp_cols_x_secc[shp_cols_x_secc$DISTRITO_L %in% as.numeric(inputs_globales$loc()),], 
                                           "Ninguno"   = shp_cols_x_secc
        )
      }
      
      # Construcción del Proxy
      proxy <- leafletProxy(ns("map_mzas")) %>%
        clearShapes() %>% clearControls() %>% 
        fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
      
      # Capa de fondo (secciones no seleccionadas)
      if(nrow(shp_secc_2024_mr_secc_select) > 0){
        proxy <- proxy %>% addPolygons(data = shp_secc_2024_mr_secc_select, 
                                       label = ~SECCION, layerId = ~as.character(SECCION),
                                       fillOpacity = 0.6, fillColor = '#adb5bd', weight = 0.6, 
                                       color = '#596475', opacity = 1, smoothFactor = 1,
                                       highlightOptions = highlightOptions(color = '#f72585', fillOpacity = 0.3, weight = 2, bringToFront = TRUE))
      }
      
      # Capa de Manzanas
      # Crear labels HTML fuera del addPolygons para limpieza
      labels_html <- lapply(1:nrow(shp_mza_2023_secc), function(i) {
        val <- shp_mza_2023_secc[[input$pik_pob_mza]][i]
        HTML(paste0(
          "<strong>Seccion: </strong>", shp_mza_2023_secc$SECCION[i], "<br>",
          "<strong>Manzana: </strong>", shp_mza_2023_secc$CVEGEO[i], "<br>",
          "<strong>", input$pik_pob_mza, ": </strong>", val
        ))
      })
      
      proxy <- proxy %>%
        addPolygons(data = shp_mza_2023_secc %>% st_transform(4326), 
                    label = labels_html,
                    fillOpacity = 0.6, 
                    fillColor = ~pal(vals),
                    weight = 2, color = '#596475', opacity = 1.0, 
                    highlightOptions = highlightOptions(color = '#f72585', weight = 2, fillOpacity = 0.3, bringToFront = TRUE)) %>%
        addLegend(pal = pal, values = vals, opacity = 0.7, title = "Densidad", position = "bottomright") %>%
        leaflet.extras2::addEasyprint(options = leaflet.extras2::easyprintOptions(exportOnly = TRUE))
      
      # Capa de Colonias (Overlay)
      if(!is.null(shp_cols_x_secc_selected)){
        proxy <- proxy %>%
          addPolygons(data = shp_cols_x_secc_selected %>% st_transform(4326), 
                      label = ~NOMBRE,
                      fillOpacity = 0.6, fillColor = '#f72585', weight = 1, 
                      color = '#596475', opacity = 1,
                      highlightOptions = highlightOptions(color = '#f72585', fillOpacity = 0.3, weight = 2, bringToFront = TRUE))
      }
    })
    
    # 7. TEXTO DESCRIPTIVO -----------------------------------------------------
    output$text_secc <- renderText({
      if(is.null(foco_secc_select$secc_select) || length(foco_secc_select$secc_select) == 0){
        x_1 = "Seleccione una seccion"
      } else {
        x_1 = c("Secciones: ", paste(foco_secc_select$secc_select, collapse = ", "))
      }
      
      x_2 = switch(inputs_globales$tipo(),
                   "Municipio" = paste("Municipio de", inputs_globales$mun()),
                   "DFederal"  = paste("DFederal", inputs_globales$fed()),
                   "DLocal"    = paste("DLocal", inputs_globales$loc()),
                   "Ninguno"   = ""
      )
      paste(x_2, "-", x_1)
    })
    
    # 8. GRÁFICO PLOTLY --------------------------------------------------------
    observeEvent(input$pik_elect_mza, {
      # Actualizar slider de años cuando cambia la elección
      list_año <- sort(unique(cant_votos_nl[cant_votos_nl$eleccion == input$pik_elect_mza,]$año))
      updateSliderTextInput(session = session, inputId = "slid_año_mza", choices = c(list_año), selected = min(list_año))
    })
    
    output$py_ganadores <- renderPlotly({
      req(foco_secc_select$secc_select)
      
      cant_votos_nl_secc <- cant_votos_nl[cant_votos_nl$seccion %in% foco_secc_select$secc_select,]
      cant_votos_select = cant_votos_nl_secc %>% filter(
        eleccion == input$pik_elect_mza,
        año == input$slid_año_mza
      )
      
      validate(need(nrow(cant_votos_select) > 0, "No hay datos para esta selección"))
      
      cant_votos_group <- cant_votos_select %>% 
        aggregate(votos ~ partido + eleccion + año, FUN = sum) %>% 
        arrange(partido)
      
      # Asignación de colores manual
      colores_grafico <- sapply(cant_votos_group$partido, function(p) {
        if(p %in% names(color_partido_2)) color_partido_2[[p]] else "#cccccc"
      })
      
      plot_ly(data = cant_votos_group, x = ~votos, y = ~partido, 
              type = 'bar', orientation = 'h', marker = list(color = colores_grafico)) %>%
        layout(title = "Votos por sección", showlegend = FALSE)
    })
    
    # 9. VALUE BOXES -----------------------------------------------------------
    # Población
    output$vb_pob <- renderValueBox({
      val_texto <- "-"
      titulo <- "Seleccione población"
      
      if(!is.null(foco_secc_select$secc_select) && length(foco_secc_select$secc_select) > 0 && !is.null(input$pik_pob_mza)) {
        data <- st_drop_geometry(shp_mza2023_secc2024_cpv2020)
        data_filtered <- data[data$SECCION %in% foco_secc_select$secc_select, input$pik_pob_mza]
        
        # Lógica: Suma si es conteo (empieza con Población...), Promedio si no
        es_conteo <- chs_poblaciones_manzanas_t[[input$pik_pob_mza]] %in% grep("^Población", names(chs_poblaciones_manzanas), value = TRUE)
        
        val_num <- if(es_conteo) sum(data_filtered, na.rm = TRUE) else mean(data_filtered, na.rm = TRUE)
        val_texto <- if(es_conteo) comma(val_num) else round(val_num, 2)
        
        titulo <- paste0(chs_poblaciones_manzanas_t[[input$pik_pob_mza]], " en sección")
      }
      
      valueBox(val_texto, subtitle = titulo, icon = icon("users"), color = "light-blue")
    })
    
    # Votos
    output$vb_vots <- renderValueBox({
      val_texto <- "-"
      titulo <- "Seleccione elección"
      
      if(!is.null(foco_secc_select$secc_select) && length(foco_secc_select$secc_select) > 0 && !is.null(input$pik_elect_mza)) {
        data_secc <- cant_votos_nl[cant_votos_nl$seccion %in% foco_secc_select$secc_select,]
        data_filter <- data_secc %>% filter(eleccion == input$pik_elect_mza, año == input$slid_año_mza)
        
        val_num <- sum(data_filter$votos, na.rm = TRUE)
        val_texto <- comma(val_num)
        titulo <- paste0("Votos totales: ", input$pik_elect_mza)
      }
      
      valueBox(val_texto, subtitle = titulo, icon = icon("check-to-slot"), color = "teal")
    })
    
  })
}