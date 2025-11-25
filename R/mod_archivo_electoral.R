# ==============================================================================
# MÓDULO: Archivo Electoral (Base de Datos)
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_archivo_electoral_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Archivo Electoral", collapsible = TRUE, collapsed = TRUE, width = 12, status = "success",
        p("Genere tablas con la cantidad de votos por partido. Utilice los filtros para seleccionar las secciones, partidos, elecciones y años de su interés.")
      ),
      # --- Filtros ---
      box(
        width = 3,  
        shinyWidgets::pickerInput(
          inputId = ns("pik_secc_archivo"), 
          label = "Seleccione secciones:", 
          options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), 
          choices = NULL, # Se actualiza en server
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("pik_partido_archivo"), 
          label = "Seleccione partidos:", 
          options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), 
          choices = unique(cant_votos_nl$partido), 
          selected = unique(cant_votos_nl$partido), 
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("pik_elecc_archivo"), 
          label = "Seleccione elección:", 
          options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), 
          choices = unique(cant_votos_nl$eleccion), 
          selected = unique(cant_votos_nl$eleccion), 
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("pik_año_archivo"), 
          label = "Seleccione años:", 
          multiple = TRUE, 
          options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), 
          choices = sort(unique(cant_votos_nl$año)), 
          selected = unique(cant_votos_nl$año)
        )
      ),
      # --- Tablas ---
      tabBox(
        width = 9,
        tabPanel("Tabla Agregada",
                 div(style = "display:flex; gap:5px; padding:5px;",
                     downloadBttn(outputId = ns("dw_archivo_csv_archivo"), label = "CSV", size = "sm", icon = icon("file-csv"), color = "warning", style = "simple")
                 ),
                 DT::dataTableOutput(ns("dt_agregada_archivo"))
        ),
        tabPanel("Tabla Desagregada",
                 div(style = "display:flex; gap:5px; padding:5px;",
                     downloadBttn(outputId = ns("dw_archivo_csv_2_archivo"), label = "CSV", size = "sm", icon = icon("file-csv"), color = "warning", style = "simple")
                 ),
                 DT::dataTableOutput(ns("dt_desagregada_archivo"))
        )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_archivo_electoral_server <- function(id, secciones_reactivas, input_filtro_tipo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Actualizar Filtro de Secciones ----------------------------------------
    # Cuando cambian las secciones disponibles (por filtro global de municipio/distrito)
    # actualizamos el pickerInput local.
    observeEvent(secciones_reactivas(), {
      
      secciones_disponibles <- secciones_reactivas()$SECCION
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "pik_secc_archivo",
        choices = sort(secciones_disponibles),
        selected = secciones_disponibles # Seleccionar todas por defecto para evitar tabla vacía
      )
    })
    
    # 2. Construcción de Datos Desagregados ------------------------------------
    query_archivo <- reactive({
      req(input$pik_secc_archivo, input$pik_partido_archivo, input$pik_elecc_archivo, input$pik_año_archivo)
      
      # Datos espaciales base (sin geometría para ser más rápido)
      data_shp <- sf::st_drop_geometry(secciones_reactivas()) %>% 
        select(SECCION, DISTRITO, DISTRITO_L, NOMBRE)
      
      # Filtrado inicial de cant_votos_nl (Global)
      votos_filtrados <- cant_votos_nl %>%
        filter(
          partido %in% input$pik_partido_archivo,
          eleccion %in% input$pik_elecc_archivo,
          año %in% input$pik_año_archivo,
          seccion %in% input$pik_secc_archivo # Filtro crucial
        )
      
      # Join con datos administrativos
      data_final <- votos_filtrados %>%
        left_join(data_shp, by = c("seccion" = "SECCION")) %>%
        select(
          "Sección" = seccion, 
          "Distrito Federal" = DISTRITO, 
          "Distrito Local" = DISTRITO_L, 
          "Municipio" = NOMBRE,
          "Partido" = partido, 
          "Elección" = eleccion, 
          "Votos" = votos, 
          "Año" = año
        )
      
      return(data_final)
    })
    
    # 3. Construcción de Datos Agregados ---------------------------------------
    query_archivo_agg <- reactive({
      req(query_archivo())
      data_raw <- query_archivo()
      tipo <- input_filtro_tipo() # Reactivo pasado desde el server principal
      
      # Agrupación dinámica según el filtro global seleccionado
      if (tipo == "Municipio") {
        res <- data_raw %>% group_by(Municipio, Partido, Elección, Año) %>% summarise(`Votos totales` = sum(Votos, na.rm = T), .groups = 'drop')
      } else if (tipo == "DFederal") {
        res <- data_raw %>% group_by(`Distrito Federal`, Partido, Elección, Año) %>% summarise(`Votos totales` = sum(Votos, na.rm = T), .groups = 'drop')
      } else if (tipo == "DLocal") {
        res <- data_raw %>% group_by(`Distrito Local`, Partido, Elección, Año) %>% summarise(`Votos totales` = sum(Votos, na.rm = T), .groups = 'drop')
      } else {
        # Ninguno o default
        res <- data_raw %>% group_by(Partido, Elección, Año) %>% summarise(`Votos totales` = sum(Votos, na.rm = T), .groups = 'drop')
      }
      
      return(res)
    })
    
    # 4. Renders de Tablas -----------------------------------------------------
    
    # Opciones comunes de DT
    dt_options <- list(
      searching = TRUE, # Cambiado a TRUE para permitir búsqueda
      deferRender = TRUE,
      scrollY = 500,
      scroller = TRUE,
      autoWidth = TRUE,
      dom = 'Bfrtip',
      scrollX = TRUE
    )
    
    output$dt_agregada_archivo <- DT::renderDataTable({
      datatable(query_archivo_agg(), style = 'bootstrap5', class = 'header stripe',
                extensions = c("Scroller", "Buttons"), selection = "single", options = dt_options)
    })
    
    output$dt_desagregada_archivo <- DT::renderDataTable({
      datatable(query_archivo(), style = 'bootstrap5', class = 'header stripe',
                extensions = c("Scroller", "Buttons"), selection = "single", options = dt_options)
    })
    
    # 5. Descargas -------------------------------------------------------------
    
    output$dw_archivo_csv_archivo <- downloadHandler(
      filename = function() { paste0(format(Sys.Date(),"%d_%m_%Y"),"_archivo_electoral_agregado.csv") },
      content = function(file) { write.csv(query_archivo_agg(), file, row.names = FALSE) }
    )
    
    output$dw_archivo_csv_2_archivo <- downloadHandler(
      filename = function() { paste0(format(Sys.Date(),"%d_%m_%Y"),"_archivo_electoral_desagregado.csv") },
      content = function(file) { write.csv(query_archivo(), file, row.names = FALSE) }
    )
    
  })
}