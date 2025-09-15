# ==============================================================================
# Definición de Opciones Reutilizables
# ==============================================================================
# Se definen las listas de elecciones aquí para no repetirlas en cada `selectInput`.
# Esto facilita el mantenimiento: si se añade una nueva elección, solo se cambia aquí.





# ==============================================================================
# Interfaz de Usuario (UI) de la Aplicación Shiny
# ==============================================================================

# options(shiny.reactlog = TRUE)

ui_code <- dashboardPage(
  
  # 1. Cabecera (Header) ----
  dashboardHeader(titleWidth = "25rem",
    title = # Contenedor del logo y el título
      tags$span(
        tags$img(
          src = "bienvenida_quipu_logo.png", # Ruta relativa desde la carpeta www
          height = "40", # Ajusta la altura del logo
          style = "margin-top:-5px; margin-right:5px;" # Ajuste fino de la posición
        ),
        "QUIPU Electoral" # El texto del título
      )
  ),
  
  # 2. Barra Lateral (Sidebar) ----
  dashboardSidebar(width = "25rem",
    disable = FALSE,
    includeCSS("www/styles.css"),
    includeScript("www/main.js"),
    useShinyjs(),
    
    # --- Filtros Iniciales ---
    shiny::selectInput("tipo_filtro_inicial",
                       "Nivel geográfico:",
                       choices = c("Municipio", "DFederal", "DLocal", "Ninguno"),
                       multiple = FALSE),
    
    hidden(
      shiny::selectInput("municipio_inicial",
                         "Selecciona el municipio:",
                         choices = NULL),
      shiny::selectInput("federal_inicial",
                         "Selecciona el distrito federal:",
                         choices = NULL),
      shiny::selectInput("local_inicial",
                         "Selecciona el distrito local:",
                         choices = NULL)
    ),
    
    # --- Menú Principal ---
    sidebarMenu(
      # Menú de Inicio se mantiene en la parte superior
      menuItem("Inicio", tabName = "home", icon = icon("home")),
      
      # 1. Menú principal "Información Electoral" con sub-elementos
      menuItem("Información Electoral", icon = icon("vote-yea"), startExpanded = FALSE, # Cambia a TRUE si quieres que inicie abierto
               
               # NOTA: Los nombres han sido actualizados, pero los 'tabName' se mantienen
               # para asegurar la compatibilidad con tu código del servidor.
               # menuSubItem("Participación Electoral",           tabName = "participacion"), # Añadido desde el primer código
               menuSubItem("Comparativo Resultados Históricos", tabName = "ganadores"),
               menuSubItem("Distancia entre Primero y Segundo", tabName = "diferencias"),
               menuSubItem("Voto Sombra",                       tabName = "sombra"),
               menuSubItem("Evolución partidista",              tabName = "cambios_porcentuales"),
               #menuSubItem("Voto diferenciado partidista",      tabName = "rendimiento_historico"),
               #menuSubItem("Comparativa ganadas/perdidas",      tabName = "robados"),
               menuSubItem("BD Resultados",                     tabName = "creacion_tablas"),
               # menuSubItem("Lealtad Partidista",                tabName = "lealtad"), # Añadido desde el primer código
               menuSubItem("Simulador de Resultados",           tabName = "simulaciones")
      ),
      
      # 2. Renombramiento de "Datos censales" a "Datos Poblacionales"
      menuItem("Datos Poblacionales",              tabName = "censales",             icon = icon("users-cog")),
      
      # 3. Resto de los menús en el nivel principal
      menuItem("Demografía Espacial",              tabName = "edades",               icon = icon("id-card")),
      menuItem("Población y votos por manzana",    tabName = "data_mza",             icon = icon("map-marker-alt"))
      # menuItem("Perfil de colonias",               tabName = "información_colonias", icon = icon("map-signs")),
      # menuItem("Gestión de tiempo",                tabName = "tiempoxsección",       icon = icon("calendar-alt"))
      
      # La línea de "Fortaleza Electoral" permanece comentada
      # menuItem("Fortaleza Electoral",           tabName = "probabilidades",       icon = icon("shield-alt"))
    )
  ),
  
  # 3. Cuerpo del Dashboard (Body) ----
  dashboardBody(
    extendShinyjs(
      script = "www/main.js", 
      functions = c("fullscreen", "fullscreenFortaleza", "fullscreenVisitas", 
                    "fullscreenGanaOPierde", "fullscreenGana", "fullscreenPierde", 
                    "fullscreenGanador1", "fullscreenGanador2", "fullscreenGenerosEdad", 
                    "fullscreenPri", "fullscreenSim1", "fullscreenCambiosPerc", 
                    "fullscreenManzanas", "fullscreenSombra", "fullscreenDiferencia")
    ),
    
    tabItems(
      
      # Pestaña: Inicio ----
      tabItem(
        tabName = "home",
        
        # --- Fila 1: Banner de Bienvenida ---
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = tagList(icon("dashboard"), "Bienvenido a QUIPU Electoral"),
            h4("Tu centro de comando para el análisis geo-electoral."),
            p(
              "Esta plataforma te permite explorar datos, descubrir patrones y diseñar estrategias ganadoras. ",
              "Para comenzar, selecciona una geografía (municipio o distrito) en el panel de la izquierda y luego navega entre los módulos de análisis."
            ),
            br(),
            textOutput("user_role") # Muestra el rol del usuario, si aplica
          )
        ),
        
        # --- Fila 2: Módulos de Funcionalidades ---
        fluidRow(
          
          # --- Columna 1: Análisis de Resultados Electorales ---
          column(width = 4,
                 box(
                   title = tagList(icon("chart-pie"), "Análisis de Resultados"), 
                   status = "info", solidHeader = TRUE, width = NULL,
                   tags$ul(
                     tags$li(tags$strong("Visualizador Histórico:"), " Compara mapas de ganadores entre dos elecciones."),
                     tags$li(tags$strong("Rendimiento Histórico:"), " Identifica los bastiones históricos de un partido."),
                     tags$li(tags$strong("Voto Sombra:"), " Descubre quién quedó en segundo lugar en cada sección."),
                     tags$li(tags$strong("Distancia Primer-Segundo:"), " Analiza la diferencia de votos entre los punteros.")
                   )
                 )
          ),
          
          # --- Columna 2: Demografía y Territorio ---
          column(width = 4,
                 box(
                   title = tagList(icon("map-location-dot"), "Demografía y Territorio"), 
                   status = "success", solidHeader = TRUE, width = NULL,
                   tags$ul(
                     tags$li(tags$strong("Población por Manzana:"), " Explora la densidad poblacional a nivel micro."),
                     tags$li(tags$strong("Demografía Espacial:"), " Visualiza la distribución de la población por edad y género."),
                     tags$li(tags$strong("Datos Censales:"), " Mapea variables socioeconómicas del censo por sección."),
                     tags$li(tags$strong("Perfil de Colonias:"), " Observa el perfil detallado de las colonias de tu zona.")
                   )
                 )
          ),
          
          # --- Columna 3: Herramientas Estratégicas ---
          column(width = 4,
                 box(
                   title = tagList(icon("cogs"), "Estrategia y Simulación"), 
                   status = "warning", solidHeader = TRUE, width = NULL,
                   tags$ul(
                     tags$li(tags$strong("Simulador Electoral:"), " Mide el impacto de la transferencia de votos entre partidos."),
                     #tags$li(tags$strong("Fortaleza Electoral (ML):"), " Estima la probabilidad de victoria con Machine Learning."),
                     tags$li(tags$strong("Evolución Electoral:"), " Mide la tasa de cambio en la votación para cada partido."),
                     tags$li(tags$strong("Archivo Electoral:"), " Genera tablas personalizadas para análisis profundo.")
                   )
                 )
          )
        )
      ),
      
      # Pestaña: Población y Votos por Manzana ----
      tabItem(
        tabName = "data_mza",
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
                     actionBttn(inputId = "btt_return_secc_select", label = NULL, style = "material-circle", color = "warning", icon = icon("chevron-left"))
                   ),
                   textOutput("text_secc") 
                 )
          )
        ),
        br(),
        
        fluidRow(
          column(width = 1),
          column(width = 6,
                 fluidRow(
                   style="display:flex; align-items:end; gap:0.8rem;",
                   shinyjs::hidden(
                     pickerInput(inputId = "pik_pob_mza", label = "Seleccione población de interés", options = list(`live-search` = TRUE), choices = chs_poblaciones_manzanas, selected = chs_poblaciones_manzanas[1]),
                     pickerInput(inputId = "pik_secc_col_mza", label = "Búsqueda de sección", multiple = TRUE, options = list(`live-search` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), choices = "", selected = "")
                   )
                 )
          ),
          column(width = 4,
                 fluidRow(
                   style="display:flex; align-items:end; gap:0.8rem;",
                   hidden(
                     pickerInput(inputId = "pik_elect_mza", label = "Seleccione elección", options = list(`live-search` = TRUE), choices = list_eleccion, selected = list_eleccion[1]),
                     sliderTextInput(inputId = "slid_año_mza", label = "Año de elección:", choices = "", grid = TRUE)
                   )
                 )
          ),
          column(width = 1)
        ),
        
        fluidRow(
          column(width = 1),
          column(width = 6,
                 materialSwitch(inputId = "swch_colonias_mzas", label = "Visualiza colonias", status = "success", value = FALSE),
                 leafletOutput("map_mzas", width = "100%", height = 600)
          ),
          column(width = 4,
                 fluidRow(
                   valueBox(textOutput("text_vb_pob_map"), strong(textOutput("text_titvb_pob_map")), icon = icon("users"), width = 6, color = "light-blue"),
                   valueBox(textOutput("text_vb_vots_map"), strong(textOutput("text_titvb_vots_map")), icon = icon("check-to-slot"), width = 6, color = "teal")
                 ),
                 plotlyOutput("py_ganadores", width = "100%")
          ),
          column(width = 1)
        )
      ),
      
      # Pestaña: Visualizador Histórico-Electoral ----
      tabItem(
        tabName = "ganadores",
        fluidRow(
          box(
            width = 12,
            title = "Visualizador Histórico-Electoral",
            collapsible = TRUE, collapsed = T, 
            solidHeader = F, status = "info",
            p("Explore y compare visualmente los datos electorales de diferentes periodos. Seleccione dos elecciones para generar mapas interactivos donde cada sección se colorea según el partido ganador.")
          )
        ),
        fluidRow( 
          box(
            status = "success",
            shiny::selectInput(
              "eleccion_1","Elección para el primer panel:",
              choices = lista_elecciones_nombres,selected = lista_elecciones_nombres$`Elecciones 2024`[1]),
            
            leafletOutput("mapa_ganador_1", height = "80vh"),
            shiny::actionButton("mapGanador1Fullscreen", "Pantalla Completa"),
            downloadButton("downloadganador_elec1", "Descargar Tabla (CSV)")
          ),
          box(
            status = "success",
            shiny::selectInput(
              "eleccion_2","Elección para el segundo panel:",
              choices = lista_elecciones_nombres,selected = lista_elecciones_nombres$`Elecciones 2021`[1]),
            
            leafletOutput("mapa_ganador_2", height = "80vh"),
            shiny::actionButton("mapGanador2Fullscreen", "Pantalla Completa"),
            downloadButton("downloadganador_elec2", "Descargar Tabla (CSV)")
          )
        )
      ), 
      
      # Pestaña: Voto Sombra ----
      tabItem(
        tabName = "sombra",
        
        fluidRow(
          column(
            width = 3,
            box(
              title = "Visualizador del Voto Sombra", width = 12, collapsible = TRUE, collapsed = TRUE, status = "info",
              p("Esta herramienta permite analizar cuál partido político quedó en segundo lugar en cada sección para una elección específica. Seleccione una elección para generar un mapa interactivo.")
            ),
            box(width = 12, status = "warning",
                shiny::selectInput("eleccion_sombra",
                                   "Seleccione la elección a analizar:",
                                   choices = choices_rendimiento_historico)
                )
            
          ),
          column(
            width = 9,
            tabBox(
              id = "resultados_tabs", 
              width = 12,
              
              # --- Primer Tab: Contiene el mapa ---
              tabPanel(
                title = "Mapa Interactivo", 
                icon = icon("map-location-dot"), # Icono para la pestaña
                
                # Contenido del primer box original
                leafletOutput("mapa_sombra", height = "80vh", width = "100%"),
                br(), # Un pequeño espacio para separar el mapa del botón
                shiny::actionButton("mapSombraFullscreen", "Pantalla Completa", icon = icon("expand"))
              ),
              
              # --- Segundo Tab: Contiene la tabla de datos ---
              tabPanel(
                title = "Tabla de Datos", 
                icon = icon("table"), # Icono para la pestaña
                
                # Contenido del segundo box original
                downloadBttn(
                  outputId = "dw_csv_sombra", 
                  label = "Descargar CSV", 
                  size = "sm", 
                  style = "simple",
                  color = "warning"
                ),
                hr(), # Una línea horizontal para separar el botón de la tabla
                DT::DTOutput("dt_sombra_sombra")
              )
            )
          )
        )
      ),
      
      # Pestaña: Distancia Primer-Segundo ----
      tabItem(
        tabName = "diferencias",
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
                shiny::selectInput("eleccion_diferencia",
                                   "Seleccione la elección a analizar:",
                                   choices = choices_elections_sombra)
            )
          ),
          column(
            width = 9,
            tabBox(
              id = "resultados_tabs", 
              width = 12, # Este width se aplica a todo el tabBox
              
              # Primer Tab: Mapa
              tabPanel(
                title = "Mapa", 
                icon = icon("map"),
                # Los argumentos 'width' y 'status' se han eliminado de aquí
                leafletOutput("mapa_diferencia", height = "80vh"),
                # Sugerencia: Colocar el botón dentro de un tag para mejor control del estilo
                tags$div(
                  style = "position: absolute; top: 10px; right: 10px; z-index: 1000;",
                  shiny::actionButton("mapDiferenciaFullscreen", "Pantalla Completa")
                )
              ),
              
              # Segundo Tab: Tabla
              tabPanel(
                title = "Tabla", 
                icon = icon("table"),
                # Los argumentos 'width' y 'status' se han eliminado de aquí
                
                # Sugerencia: Agrupar los controles para un mejor layout
                tags$div(
                  style = "padding-bottom: 10px;", # Un poco de espacio
                  downloadBttn(
                    outputId = "dw_csv_distancia", 
                    label = "CSV", 
                    icon = icon("file-csv"), 
                    size = "sm", 
                    color = "warning", 
                    style = "simple"
                  )
                ),
                
                DT::DTOutput("dt_diferencia_diferencia")
              )
            )
          )
        )
      ),
      
      # Pestaña: Demografía Espacial ----
      tabItem(
        tabName = "edades",
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
              shiny::selectInput("edades",
                                 "Seleccione grupos de edad:",
                                 choices =  c("18 a 24" = "_18_24", "25 a 29" = "_25_29", "30 a 39" = "_30_39", 
                                              "40 a 49" = "_40_49", "50 a 59" = "_50_59", "Mayores" = "mayores"),
                                 selected = c("_18_24", "_25_29", "_30_39", "_40_49", "_50_59", "mayores"),
                                 multiple = TRUE),
              shiny::selectInput("genero",
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
              leafletOutput("mapa_generos_edad", height = "80vh"),
              shiny::actionButton("mapGenerosEdadFullscreen", "Pantalla Completa"),
              downloadButton("download_edades", "Descargar Información (CSV)")
            )
          )
          
          
        )
      ),
      
      # Pestaña: Archivo Electoral ----
      tabItem(
        tabName = "creacion_tablas",
        fluidRow(
          box(
            title = "Archivo Electoral", collapsible = TRUE, collapsed = TRUE, width = 12, status = "success",
            p("Genere tablas con la cantidad de votos por partido. Utilice los filtros para seleccionar las secciones, partidos, elecciones y años de su interés.")
          ),
          box(
            width = 3,  
            shinyWidgets::pickerInput(inputId = "pik_secc_archivo", label = "Seleccione secciones:", options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), choices = sort(unique(cant_votos_nl$seccion)), selected = unique(cant_votos_nl$seccion), multiple = TRUE),
            shinyWidgets::pickerInput(inputId = "pik_partido_archivo", label = "Seleccione partidos:", options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), choices = unique(cant_votos_nl$partido), selected = unique(cant_votos_nl$partido), multiple = TRUE),
            shinyWidgets::pickerInput(inputId = "pik_elecc_archivo", label = "Seleccione elección:", options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), choices = unique(cant_votos_nl$eleccion), selected = unique(cant_votos_nl$eleccion), multiple = TRUE),
            shinyWidgets::pickerInput(inputId = "pik_año_archivo", label = "Seleccione años:", multiple = TRUE, options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), choices = sort(unique(cant_votos_nl$año)), selected = unique(cant_votos_nl$año))
          ),
          tabBox(
            width = 9,
            tabPanel("Tabla Agregada",
                     div(style = "display:flex; gap:5px; padding:5px;",
                         downloadBttn(outputId = "dw_archivo_csv_archivo", label = "CSV", size = "sm", icon = icon("file-csv"), color = "warning", style = "simple")
                     ),
                     DT::dataTableOutput("dt_agregada_archivo")
            ),
            tabPanel("Tabla Desagregada",
                     div(style = "display:flex; gap:5px; padding:5px;",
                         downloadBttn(outputId = "dw_archivo_csv_2_archivo", label = "CSV", size = "sm", icon = icon("file-csv"), color = "warning", style = "simple")
                     ),
                     DT::dataTableOutput("dt_desagregada_archivo")
            )
          )
        )
      ),
      
      # Pestaña: Rendimiento Histórico ----
      tabItem(
        tabName = "rendimiento_historico",
        fluidRow(
          column(width = 3,
                 box(
                   title = "Rendimiento Histórico por Partido", collapsible = TRUE, collapsed = TRUE, width = 12, status = "success",
                   p("Descubre los bastiones históricos de un partido. Simplemente elige un partido y las elecciones que deseas analizar, y el mapa te mostrará al instante las secciones que ha ganado.")
                 ),
                 box(
                   width = 12, 
                   shiny::selectInput("partido_ganador", "Selecciona el partido de interés", 
                                      choices = c("MC","PAN","PRI","VERDE","PT","MORENA","INDEP")),
                   shiny::selectInput("slt_candidatura_rh", 
                                      "Selecciona las candidaturas ganadas a visualizar",
                                      multiple = TRUE, 
                                      selected = choices_rendimiento_historico[1][[1]][1],
                                      choices = choices_rendimiento_historico
                                      )
                   )
                 
          ),
          column(width = 9,
                 box(width = 12,
                     leafletOutput("mapa_rendimiento_historico",height = "80vh"), # height = "80vh" mapa_pri   rendimiento_historico
                     shiny::actionButton("mapPriFullscreen", "Pantalla Completa"),
                     downloadButton("download_quitar", "Descargar Tabla (CSV)")
                 )
          )
        )
      ),
      
      # Pestaña: Evolución Electoral ----
      tabItem(
        tabName = "cambios_porcentuales",
        fluidRow(
          column(
            width = 3,
            box(
              width = 12,
              title = "Evolución Electoral", collapsible = TRUE, collapsed = TRUE, status = "success",
              p("Analice el cambio porcentual en la votación de un partido entre dos elecciones. Seleccione una elección base, una de comparación y el partido de interés.")
            ),
            box(
              width = 12,
              shiny::selectInput("eleccion1_a", "Selecciona la elección base", choices = lista_elecciones_nombres, selected = "Diputado Local 2015"),
              shiny::selectInput("eleccion2_a", "Selecciona la segunda elección", choices = lista_elecciones_nombres, selected = "Diputado Local 2021"),
              shiny::selectInput("partido_analisis_a", "Selecciona el partido a analizar", choices =  c("pan", "pri", "morena", "mc", "verde", "pt"))
            )
            ),
          column(
            width = 9,
            box(
              width = 12,
              h3("Tasa de cambio entre las dos elecciones"),
              leafletOutput("mapa_cambios_perc", height = "80vh"),
              shiny::actionButton("mapCambiosPercFullscreen", "Pantalla Completa"),
              downloadButton("download_tasas", "Descargar Tabla (CSV)")
            )
            )
          
          
        )
      ),
      
      # Pestaña: Fortaleza Electoral ----
      tabItem(
        tabName = "probabilidades",
        fluidRow(
          box(
            title = "Fortaleza Electoral (Machine Learning)", collapsible = TRUE, collapsed = TRUE, width = 12, status = "success",
            p("Explore la fortaleza relativa de un partido en cada sección, calculada con modelos de machine learning basados en datos históricos. Seleccione el partido para ver su probabilidad de ganar en el mapa.")
          )
        ),
        fluidRow(
          box(
            width = 3,
            shiny::selectInput("partido_predecir", "Selecciona el partido a analizar:", c("PRI", "morena", "MC", "PAN", "VERDE", "pt"))
          )
        ),
        box(
          width = 12,
          h3("Fortaleza Electoral"),
          leafletOutput("mapa_fortaleza", height = "80vh"),
          shiny::actionButton("mapFortalezaFullscreen", "Pantalla Completa"),
          downloadButton("download_fortalezas", "Descargar Tabla (CSV)")
        )
      ),
      
      # Pestaña: Datos Censales ----
      tabItem(
        tabName = "censales",
        
        # He modificado el título para que coincida con el nuevo nombre del menú y he quitado el 'collapsed'
        box(
          title = tags$h3("Datos Poblacionales por Sección", style = "margin:0;"), 
          collapsible = TRUE, 
          collapsed = FALSE, # Es mejor mostrar la instrucción inicial por defecto
          width = 12, 
          status = "success",
          solidHeader = TRUE,
          p("Seleccione una variable demográfica en el panel de control para visualizar su distribución geográfica en el mapa y explorar los datos detallados en la tabla.")
        ),
        
        fluidRow(
          # --- Columna Izquierda: Panel de Control ---
          # Aquí se quedan todos los inputs y botones de acción que afectan a ambas salidas.
          column(
            width = 3,
            box(
              title = "Controles",
              status = "info",
              solidHeader = TRUE,
              width = 12, # Ancho 12 para llenar la columna de 3
              
              selectInput("censo_interes", 
                          "Elige la variable de interés:", 
                          choices = chs_censales, 
                          selected = chs_censales[1]),
              
              hr(), # Un separador visual
              
              # Botón de descarga con texto más descriptivo
              downloadBttn(
                outputId = "dw_censal_csv_censales", 
                label = "Descargar datos de la tabla", 
                size = "sm", 
                icon = icon("download"), 
                color = "primary", 
                style = "simple",
                block = TRUE # Hace que el botón ocupe todo el ancho
              )
            )
          ),
          
          # --- Columna Derecha: Salidas con Pestañas ---
          # Aquí es donde el mapa y la tabla se agrupan en el tabBox.
          column(
            width = 9,
            tabBox(
              id = "censales_tabset",
              width = 12, # Ancho 12 para llenar la columna de 9
              
              # Pestaña 1: Mapa
              tabPanel(
                title = "Mapa de Distribución", 
                icon = icon("map-marked-alt"),
                leafletOutput("mapa_censales", height = "80vh"),
                # Colocamos el botón de pantalla completa aquí, asociado al mapa
                actionButton("mapFullscreen", "Pantalla Completa", icon = icon("expand-arrows-alt"))
              ),
              
              # Pestaña 2: Tabla de Datos
              tabPanel(
                title = "Tabla de Datos", 
                icon = icon("table"),
                # Movemos la tabla desde el panel de control a su propia pestaña
                DTOutput("dt_query_censales")
              )
            )
          )
        )
      ),
      
      
      # Pestaña: Simulador Electoral ----
      tabItem(tabName = "simulaciones",
              fluidRow(
                column(
                  width = 3, 
                  box(
                    width = 12,
                    title = "Simulador Electoral", 
                    collapsible = TRUE, 
                    collapsed = TRUE,
                    solidHeader = FALSE,
                    status = "info",
                    p("El simulador electoral te permite generar y visualizar escenarios hipotéticos de elecciones. Sigue estos pasos:"),
                    tags$ol(
                      tags$li(
                        tags$b("Selecciona la Elección a Simular:"),
                        " Elige el proceso electoral de interés."
                      ),
                      tags$li(
                        tags$b("Define el Partido a Eliminar:"),
                        " Indica qué partido no participaría en tu simulación."
                      ),
                      tags$li(
                        tags$b("Asigna el Destinatario de los Votos:"),
                        " Selecciona el partido que recibiría los votos del partido eliminado."
                      )
                    ),
                    p("El mapa interactivo resultante mostrará el partido ganador en cada sección bajo tu escenario simulado. Al pasar el cursor, verás detalles de los resultados. Si el partido eliminado es también el destinatario, se simulará su ausencia total sin redistribución de votos."),
                    p("Esta herramienta te ayuda a comprender cómo la redistribución de votos impactaría los resultados electorales.")
                  ),
                  box(width = 12, 
                      shiny::selectInput("eleccion_sim1", 
                                         "Selecciona la elección a simular",
                                         choices = 
                                           list(
                                             "Elecciones 2024" = c("Presidente 2024" = "pres24", "Senado 2024" = "sen24","Diputado Local 2024" = "dipl24", "Diputado Federal 2024" = "fed24", "Alcalde 2024" = "ayunt24"),
                                             "Elecciones 2021" = c("Diputado Local 2021" = "dl21" , "Diputado Federal 2021" = "fed21", "Alcalde 2021" = "ayunt21", "Gobernador 2021" = "gob21"),
                                             "Elecciones 2018" = c("Diputado Local 2018" = "dipl18", "Senado 2018" = "sen18", "Alcalde 2018" = "ayunt18","Presidente 2018" = "pres18", "Diputado Federal 2018" = "fed18"),
                                             "Elecciones 2015" = c("Gobernador 2015" = "gob15", "Alcalde 2015" = "ayunt15", "Diputado Local 2015" = "dipl15","Diputado Federal 2015" = "fed15")
                                           )
                      ),
                      shiny::selectInput("dejar_afuera", 
                                         "Selecciona al partido que quieres dejar afuera. ",
                                         choices =  c("PAN" = "pan", "PRI" = "pri", "Morena" = "morena", "MC" = "mc", "Verde" = "verde", "pt")
                      ),
                      shiny::selectInput("recibe_votos", 
                                         "Selecciona el destino de los votos de este partido",
                                         choices =  c("PAN" = "pan", "PRI" = "pri","Morena" = "morena", "MC" = "mc", "Verde" = "verde", "pt")
                      )
                  ),
                ),
                column(
                  width = 9,
                  box(width = 12,
                      leafletOutput("mapa_sim1", height = "80vh"),
                      shiny::actionButton("mapSim1Fullscreen", "Fullscreen"),
                      downloadButton("downloadganador_sim", "Descargar la tabla como CSV")
                  )
                )
                
                
              )
              
      )
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    ) # Cierre de tabItems
  ) # Cierre de dashboardBody
) # Cierre de dashboardPage






ui_secure <- secure_app(ui_code, background  = "linear-gradient(rgba(13, 59, 102, 0.5), rgba(250, 240, 202, 0.5)),
                        url('https://aledomicom.files.wordpress.com/2024/03/bienvenida_quipu.png')  repeat center fixed;", language = "es")




ui <- ui_secure














