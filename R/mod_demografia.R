# ==============================================================================
# MÓDULO: Demografía Espacial - REFACTORIZADO
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_demografia_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # --- Panel Lateral de Controles ---
      column(
        width = 3,
        box(
          width = 12,
          title = "Configuración", 
          collapsible = TRUE, collapsed = FALSE, status = "primary", solidHeader = TRUE,
          
          p("Seleccione los criterios para filtrar la población en el mapa."),
          
          shinyWidgets::pickerInput(
            inputId = ns("edades"),
            label = "Grupos de edad:",
            choices =  c("18 a 24" = "_18_24", "25 a 29" = "_25_29", "30 a 39" = "_30_39", 
                         "40 a 49" = "_40_49", "50 a 59" = "_50_59", "60 y más" = "mayores"),
            selected = c("_18_24", "_25_29", "_30_39", "_40_49", "_50_59", "mayores"),
            multiple = TRUE,
            options = list(`actions-box` = TRUE, `select-all-text` = "Todos", `deselect-all-text` = "Ninguno")
          ),
          
          shinyWidgets::pickerInput(
            inputId = ns("genero"),
            label = "Género:",
            choices =  c("Hombres" = "hombres", "Mujeres" = "mujeres"),
            selected = c("hombres", "mujeres"),
            multiple = TRUE
          ),
          
          hr(),
          
          downloadBttn(
            outputId = ns("download_edades"), 
            label = "Descargar Datos", 
            style = "simple", color = "success", size = "sm", block = TRUE, icon = icon("download")
          )
        ),
        
        # Caja de información resumen (Opcional, para dar contexto)
        valueBoxOutput(ns("vbox_poblacion_sel"), width = 12)
      ),
      
      # --- Área del Mapa ---
      column(
        width = 9,
        box(
          width = 12, title = "Distribución Geográfica", status = "info", solidHeader = TRUE,
          
          # Contenedor relativo para posicionar el botón de fullscreen
          div(style = "position: relative;",
              leafletOutput(ns("mapa_generos_edad"), height = "80vh"),
              
              # Botón flotante para fullscreen
              div(style = "position: absolute; top: 10px; right: 10px; z-index: 1000;",
                  actionButton(ns("mapGenerosEdadFullscreen"), label = NULL, icon = icon("expand"), 
                               style = "background: white; border: none; box-shadow: 0 0 5px rgba(0,0,0,0.3);")
              )
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
    
    # 1. Preparación de Geometría (Optimización)
    geo_base <- reactive({
      req(secciones_reactivas())
      shp <- secciones_reactivas()
      
      # Detección segura de columnas
      cols <- names(shp)
      col_sec <- intersect(cols, c("SECCION", "Seccion", "CLAVE", "ID"))[1]
      
      if (!is.na(col_sec)) {
        shp %>%
          transmute(SECCION_JOIN = as.character(.data[[col_sec]]))
      } else {
        NULL
      }
    })
    
    # 2. Cálculo de Datos Demográficos
    datos_calculados <- reactive({
      # Validamos que existan los inputs antes de procesar
      req(input$edades, input$genero)
      
      # Validamos que el dataset global 'edades' exista
      if (!exists("edades")) return(NULL)
      
      # Patrones regex
      pattern_edades <- paste(input$edades, collapse = "|") 
      pattern_genero <- paste(input$genero, collapse = "|") 
      
      # Procesamiento seguro
      tryCatch({
        edades %>% 
          pivot_longer(
            cols = any_of(matches("hombres|mujeres")), 
            names_to = "categoria", 
            values_to = "cantidad"
          ) %>% 
          filter(
            stringr::str_detect(categoria, pattern_edades),
            stringr::str_detect(categoria, pattern_genero)
          ) %>% 
          group_by(seccion)  %>% 
          summarise(
            pob_seleccionada = sum(cantidad, na.rm = TRUE), 
            padron_total = mean(padron_total, na.rm = TRUE), 
            .groups = "drop"
          ) %>% 
          mutate(
            prop = if_else(padron_total > 0, pob_seleccionada / padron_total, 0),
            seccion = as.character(seccion)
          )
      }, error = function(e) {
        return(NULL) # Si falla el cálculo, retornamos NULL limpio
      })
    })
    
    # 3. Join Final
    mapa_final_data <- reactive({
      req(geo_base(), datos_calculados())
      
      geo <- geo_base()
      dta <- datos_calculados()
      
      # Verificación extra
      if(is.null(dta) || nrow(dta) == 0) return(NULL)
      
      left_join(geo, dta, by = c("SECCION_JOIN" = "seccion"))
    })
    
    # 4. Renderizado del Mapa 
    output$mapa_generos_edad <- renderLeaflet({
      
      # Paso 1: Obtener datos
      dta <- mapa_final_data()
      
      # Paso 2: Validación BLINDADA
      # Primero verificamos si es NULL explícitamente
      if (is.null(dta)) {
        shiny::validate("Esperando datos...")
      }
      
      # Luego verificamos si tiene filas (nrow devuelve un número, no NULL)
      if (nrow(dta) == 0) {
        shiny::validate("No hay población que coincida con estos criterios en la zona seleccionada.")
      }
      
      # Paso 3: Renderizado
      pal <- colorNumeric(palette = "plasma", domain = dta$prop, na.color = "transparent")
      
      # Popup seguro
      popup_content <- sprintf(
        "<b>Sección: </b>%s<br/>
         <hr style='margin:3px 0;'>
         <b>Población Seleccionada: </b>%s<br/>
         <b>Total Padrón: </b>%s<br/>
         <b>Concentración: </b>%s",
        dta$SECCION_JOIN,
        scales::comma(dta$pob_seleccionada),
        scales::comma(dta$padron_total),
        scales::percent(dta$prop, 0.1)
      )
      
      leaflet(dta) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(
          color = "#444444", 
          weight = 1,
          smoothFactor = 0.5,
          opacity = 1.0,
          fillOpacity = 0.7,
          fillColor = ~pal(prop),
          popup = popup_content, 
          highlightOptions = highlightOptions(
            weight = 2, color = "cyan", fillOpacity = 0.9, bringToFront = TRUE
          )
        ) %>%
        addLegend(
          pal = pal, 
          values = dta$prop, 
          title = "Concentración (%)", 
          position = "bottomright",
          labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x)
        )
    })
    
    # 5. ValueBox
    output$vbox_poblacion_sel <- renderValueBox({
      # Usamos tryCatch para evitar errores rojos si dta es NULL momentáneamente
      tryCatch({
        req(datos_calculados())
        dta <- datos_calculados()
        if(nrow(dta) == 0) return(NULL)
        
        total_sel <- sum(dta$pob_seleccionada, na.rm = TRUE)
        
        valueBox(
          value = scales::comma(total_sel),
          subtitle = "Población total seleccionada",
          icon = icon("users"),
          color = "purple"
        )
      }, error = function(e) return(NULL))
    })
    
    # 6. Descarga
    output$download_edades <- downloadHandler(
      filename = function() { paste("demografia_edad_genero_", Sys.Date(), ".csv", sep="") },
      content = function(file) {
        req(mapa_final_data())
        write.csv(st_drop_geometry(mapa_final_data()), file, row.names = FALSE)
      }
    )
    
    # 7. Fullscreen
    observeEvent(input$mapGenerosEdadFullscreen, {
      shinyjs::runjs(sprintf(
        "var map = document.getElementById('%s');
         if (!document.fullscreenElement) { map.requestFullscreen(); } 
         else { document.exitFullscreen(); }", 
        ns("mapa_generos_edad")
      ))
    })
  })
}