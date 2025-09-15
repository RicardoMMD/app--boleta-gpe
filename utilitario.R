


ui_code <- dashboardPage(
  dashboardHeader(title = 'QUIPU Electoral'), # ,titleWidth = "300px"
  
  dashboardSidebar(disable = F, # width = 250,
                   includeCSS("www/styles.css"),
                   includeScript("www/main.js"),
                   useShinyjs(),
                   shiny::selectInput("tipo_filtro_inicial",
                                      "Elige si quieres filtrar por municipio, distrito federal o local",
                                      choices = c("Municipio", "DFederal", "Ninguno"), multiple = F),#, "DLocal"
                   #uiOutput("dinamico_inicial"),
                   hidden(
                     shiny::selectInput("municipio_inicial",
                                        "Selecciona el municipio del que te interesa obtener información",
                                        choices = opciones_municipio,
                                        selected = opciones_municipio[1]
                     ),
                     shiny::selectInput("federal_inicial",
                                        "Selecciona el distrito federal que te interesa",
                                        choices =  opciones_federal,
                                        selected = opciones_federal[1]
                     ),
                     shiny::selectInput("local_inicial",
                                        "Selecciona el distrito local que te interesa",
                                        choices = opciones_local,
                                        selected = opciones_local[1]
                     )
                   ),
                   ### Sidebar Menu ----
                   sidebarMenu(
                     menuItem("Inicio",                           tabName = "home",                 icon = icon("home")),
                     menuItem("Población y votos por manzana",    tabName = "data_mza",             icon = icon("users")),
                     menuItem("Visualizador Histórico-Electoral", tabName = "ganadores",            icon = icon("history")),
                     menuItem("Voto Sombra",                      tabName = "sombra",               icon = icon("eye")),
                     menuItem("Distancia Primer-Segundo",         tabName = "diferencias",          icon = icon("balance-scale")),
                     menuItem("Demografía Espacial",              tabName = "edades",               icon = icon("id-card")),
                     menuItem("Archivo Electoral",                tabName = "creacion_tablas",      icon = icon("folder-open")),
                     menuItem("Rendimiento Histórico",            tabName = "pri",                  icon = icon("chart-line")),
                     # menuItem("Simulador Electoral",              tabName = "simulaciones",         icon = icon("gamepad")),
                     menuItem("Evolución Electoral",              tabName = "cambios_porcentuales", icon = icon("chart-bar")),
                     # menuItem("Evaluación Comparativa",           tabName = "robados",              icon = icon("balance-scale-right")),
                     # menuItem("Gestión de tiempo",                tabName = "tiempoxsección",       icon = icon("calendar-alt")),
                     # menuItem("Perfil de colonias",               tabName = "información_colonias", icon = icon("map-signs")),
                     menuItem("Fortaleza Electoral",              tabName = "probabilidades",       icon = icon("shield-alt")),
                     menuItem("Datos Censales",                   tabName = "censales",             icon = icon("info-circle"))
                   )
  ),
  dashboardBody(
    extendShinyjs(script = "www/main.js", functions=c("fullscreen",            "fullscreenFortaleza", "fullscreenVisitas", 
                                                      "fullscreenGanaOPierde", "fullscreenGana",      "fullscreenPierde", 
                                                      "fullscreenGanador1",    "fullscreenGanador2",
                                                      "fullscreenGenerosEdad", "fullscreenPri",       "fullscreenSim1", 
                                                      "fullscreenCambiosPerc", "fullscreenManzanas",  "fullscreenSombra", 
                                                      "fullscreenDiferencia")) ,
    tabItems(
      tabItem(tabName = "home",
              
              # 1. Headline & Subheadline
              h1("Bienvenidos a la aplicación de Quipu Analítica"), 
              h3("Una herramienta de análisis GeoElectoral"),
              textOutput("user_role"),
              
              # 4. Features of Application
              h3("Features"),
              tags$ul(
                tags$li("Visualizador Histórico-Electoral. Observa los ganadores por sección en las elecciones"),
                tags$li("Demografía Espacial. Analiza la distribución etaria por sección"),
                tags$li("Archivo electoral. Genera tablas de análisis electoral"),
                tags$li("Rendimiento histórico. Analiza el rendimiento del partido en distintas elecciones"),
                tags$li("Simulador electoral. Observa el voto sombra y cambia la composición de los votos para observar el resultado"),
                tags$li("Evolución electoral. Observa la tasa de cambio de una elección a otra para cada partido"),
                tags$li("Evaluación comparativa. Compara las secciones que ha ganado el partido con las que ha perdido, entre una elección y otra"),
                tags$li("Gestión de tiempo. Estima la cantidad de tiempo que deberán pasar los equipos sin candidato en cada sección"),
                tags$li("Perfil de colonias. Observa el perfil de las distintas colonias de tu distrito o municipio"),
                tags$li("Fortaleza electoral. Usa modelos de machine learning para conocer la fortaleza del partido ante cada elección"),
                tags$li("Datos censales. Conoce los datos censales por sección dentro de tu municipio o distrito"),
              )
      ), 
      
      ### Población y votos por manzana ----
      tabItem(tabName = "data_mza",
              fluidRow(
                column(width = 1),
                column(width = 10,
                       box(width = 12,
                           title = "Instrucciones", status = "info", solidHeader = TRUE,
                           collapsible = T,collapsed = T,
                           h3("Explora poblaciones y votos de secciones electorales a nivel manzana"),
                           p(style = "text-align: justify;",
                             "Aquí podrás observar las manzanas con mayor población de la sección seleccionada. 
                               Además, se presenta un gráfico que ilustra la distribución de votos por partido político de la sección."
                           ),
                           p(style = "text-align: justify;",
                             "1. Primero, utilice los controles principales en el sidebar para seleccionar la geografía de su interés 
                               (puede elegir municipios o distritos locales y federales)."
                           ),
                           p(style = "text-align: justify;",
                             "2. Utilice el selectos de población ubicado justo arriba del mapa. Puede seleccionar como “Población total”,”Población de 18 o mas año”, entre otros."
                           ),
                           p(style = "text-align: justify;",
                             "3. Utilice el mapa para encontrar la sección de su interés y haga clic en ella. Una vez hecho esto, las manzanas comprendidas en dicha sección se visualizarán en el mapa, coloreadas en función de la densidad de población."
                           ),
                           p(style = "text-align: justify;",
                             "4. Además, en el lado derecho de la pantalla se desplegarán opciones adicionales: un selector para elegir la elección gubernamental y el año correspondiente. Debajo de estas opciones, se presentará un resumen que incluirá la suma de la población seleccionada, la cantidad de votos registrados para la elección y el año seleccionados, junto con un gráfico que representará la distribución de votos por partido político."
                           ),
                           p(style = "text-align: justify;",
                             "5. Para volver a la vista anterior y seleccionar otra sección, simplemente haga clic en la flecha amarilla que se encuentra a la izquierda del título descriptivo que aparece después de seleccionar una sección. Tenga en cuenta que puede navegar entre secciones adyacentes haciendo clic en estas."
                           ),
                           h4("Consideraciones: "),
                           p(style = "text-align: justify;",
                             "1. Las poblaciones consideradas para mapear las manzanas pertenecen al Censo de Población y Vivienda 2020. Mientras que la cantidad de votos de la sección se obtiene del conteo del ejercicio electoral del año seleccionado."
                           ),
                           p(style = "text-align: justify;",
                             "2. Unicamente se mapean y contabilizan manzanas urbanas. No se cuenta con información de manzanas rurales"
                           ),
                       ),
                ),
                column(width = 1)
              ),
              fluidRow(
                column(width = 1),
                column(width = 11,
                       fluidRow(style="display:flex;align-items:end; gap:1.2rem; font-size:x-large; font-weight:500;",
                                shinyjs::hidden(
                                  actionBttn(
                                    inputId = "btt_return_secc_select",
                                    label = NULL,
                                    style = "material-circle", 
                                    color = "warning",
                                    icon = icon("chevron-left")
                                  )
                                ),
                                textOutput("text_secc") 
                       )
                ),
              ),
              br(),
              fluidRow(
                column(width = 1),
                column(width = 6,
                       fluidRow(style="display:flex;align-items:end; gap:0.8rem;",
                                shinyjs::hidden(
                                  pickerInput(
                                    inputId = "pik_pob_mza",
                                    label = "Seleccione población de interés",
                                    options = list(
                                      `live-search`=T
                                    ),
                                    choices = chs_poblaciones_manzanas,
                                    selected = chs_poblaciones_manzanas[1]
                                  ),
                                  pickerInput(
                                    inputId = "pik_secc_col_mza",
                                    label = "Búsqueda de sección",
                                    multiple = T,
                                    options = list(
                                      `live-search` = T,
                                      #`actions-box` = TRUE,
                                      `deselect-all-text` = "Deseleccionar Todo",
                                      `select-all-text` = "Seleccionar Todo"),
                                    choices  = "",
                                    selected = "" )
                                )
                       )
                ),
                column(width = 4,
                       fluidRow(style="display:flex;align-items:end; gap:0.8rem;",
                                hidden(
                                  pickerInput(
                                    inputId = "pik_elect_mza",
                                    label = "Seleccione elección",
                                    options = list(
                                      `live-search` = T
                                    ),
                                    choices = list_eleccion,
                                    selected = list_eleccion[1]),
                                  sliderTextInput(
                                    inputId = "slid_año_mza",
                                    label = "Año de elección: ", 
                                    choices = "",
                                    grid = TRUE
                                  )
                                ),
                       )
                ),
                column(width = 1)
              ),
              fluidRow(
                column(width = 1),
                column(width = 6,
                       materialSwitch(
                         inputId = "swch_colonias_mzas",
                         label = "Visualiza colonias",
                         status = "success",
                         value = F
                       ),
                       leafletOutput("map_mzas",width = "100%",height = 600)
                ),
                column(width = 4,
                       fluidRow( #style = "display:flex;",
                         valueBox(textOutput("text_vb_pob_map"), strong(textOutput("text_titvb_pob_map")), icon = icon("users"),width = 6,color = "light-blue"),
                         valueBox(textOutput("text_vb_vots_map"), strong(textOutput("text_titvb_vots_map")), icon = icon("check-to-slot"),width = 6,color = "teal")
                       ),
                       plotlyOutput("py_ganadores",width = "100%") # ,height = 430
                ),
                column(width = 1)
              ),
              fluidRow(
                column(width = 1),
                column(width = 10,),
                column(width = 1)
              )
      ),
      
      ### Visualizador Histórico-Electoral ----
      tabItem(tabName = "ganadores",
              fluidRow(
                box(collapsible = T,
                    title = "Visualizador Histórico-Electoral", 
                    width = 12,
                    solidHeader = T,
                    status = "success",
                    p("Bienvenido a la función 'Visualizador Histórico Electoral', esta sección de la aplicación le permite explorar y hacer comparaciones visuales de los datos electorales de diferentes periodos."),
                    
                    p("Para comenzar, encontrará dos menús desplegables referidos como 'Elección 1' y 'Elección 2'. Aquí puede seleccionar los conjuntos de datos correspondientes a las elecciones que desea analizar. Cada menú generará una base de datos que incluye la sección, el ganador, la población y el distrito."),
                    
                    p("Una vez seleccionadas las elecciones, la aplicación genera automáticamente mapas interactivos para cada uno de ellas. En estos mapas, las diferentes áreas mencionadas en la base de datos son coloreadas de acuerdo al partido ganador. Puede obtener información más detallada al mover el cursor sobre dichas áreas, revelando el número de la sección, el nombre del ganador y la población."),
                    
                    p("El propósito de tener dos mapas es que puede realizar una comparación directa de los cambios electorales entre los dos periodos seleccionados. Fíjese en cómo han cambiado los colores, lo que representa el cambio de preferencias partidistas en distintas zonas."),
                    
                    p("El 'Visualizador Histórico Electoral' es una herramienta poderosa para entender y analizar patrones electorales a lo largo del tiempo. Esperamos que le sea útil en su exploración.")
                    
                )
              ),
              fluidRow(
                box(status = "info",width = 12,
                    column(width = 6,
                           shiny::selectInput("eleccion_1",
                                              "¿Cuál elección te interesa observar en el primer panel?",
                                              choices =  c("Diputado Local 2021", 
                                                           "Diputado Federal 2021", 
                                                           "Alcalde 2021", 
                                                           "Gobernador 2021",
                                                           "Presidencia 2018", 
                                                           "Diputado Federal 2018",
                                                           "Diputado Local 2018", 
                                                           "Senado 2018", "Alcalde 2018", "Alcalde 2015", "Gobernador 2015", 
                                                           "Diputado Federal 2015", "Diputado Local 2015",
                                                           "Presidencia 2024", "Senado 2024", "Diputado Local 2024", "Diputado Federal 2024", "Alcalde 2024"
                                              )),
                    ),
                    column(
                      width = 6,
                      shiny::selectInput("eleccion_2",
                                         "¿Cuál elección te interesa observar en el segundo panel?",
                                         choices =  c("Diputado Local 2021", "Diputado Federal 2021", "Alcalde 2021", "Gobernador 2021",
                                                      "Presidencia 2018", "Diputado Federal 2018",
                                                      "Diputado Local 2018", "Senado 2018", "Alcalde 2018", "Alcalde 2015", "Gobernador 2015",
                                                      "Diputado Federal 2015", "Diputado Local 2015" ,
                                                      "Presidencia 2024", "Senado 2024",  "Diputado Local 2024", "Diputado Federal 2024", "Alcalde 2024"
                                         ))
                    )
                )
                
              ),
              fluidRow( 
                box(status = "success",
                    leafletOutput("mapa_ganador_1", height = "80vh"),
                    shiny::actionButton("mapGanador1Fullscreen", "Fullscreen"),
                    downloadButton("downloadganador_elec1", "Descargar la tabla como CSV")
                    
                ),
                box(status = "success",
                    leafletOutput("mapa_ganador_2", height = "80vh"),
                    shiny::actionButton("mapGanador2Fullscreen", "Fullscreen"),
                    downloadButton("downloadganador_elec2", "Descargar la tabla como CSV")
                    
                )
              )
              
              
      ), 
      ### Voto Sombra ----
      tabItem(tabName = "sombra",
              box(
                title = "Visualizador del voto sombra", 
                width = 12,collapsible = T,collapsed = T,
                
                solidHeader = FALSE,
                status = "info",
                p("La herramienta de Voto Sombra te permite analizar cuál partido político quedó en segundo lugar en la cantidad de votos por secciones en una elección específica. Aquí está cómo funciona:"),
                p("Elegir la Elección: El primer paso es seleccionar una elección para el análisis. Esta elección es la base de los datos que se despliegan en el mapa interactivo."),
                p("	Identificación de Partido en Segundo Lugar: El sistema organiza automáticamente los votos de cada sección y determina cuál partido quedó en segundo lugar. Cada partido queda representado por un color único."),
                p("Generar Mapa: Una vez hecho esto, la herramienta realiza los cálculos necesarios y genera una representación visual en un mapa que muestra qué partido quedó en segundo lugar para cada sección."),
                p("	Interpretar el Mapa Interactivo: Los resultados se visualizan en un mapa interactivo, con cada sección coloreada según el partido que quedó en segundo lugar. Diferentes colores representan diferentes partidos, lo que facilita la identificación."),
                p("	Detalles de la Sección: Al pasar el cursor sobre una sección, aparecerá una pequeña ventana emergente que proporcionará más detalles. Muestra la sección específica y qué partido quedó en segundo lugar."),
                p("Esta herramienta te permite realizar un análisis geográfico detallado del desempeño electoral de los partidos en una elección específica, revelando las áreas donde cada partido fue más o menos fuerte, y obtener una visión valiosa de las tendencias y preferencias de los votantes.")
              ),
              fluidRow(
                shiny::column(width = 3,style = "display:flex;flex-direction: column;",
                              box(width = 12,
                                  status = "warning",
                                  shiny::selectInput("eleccion_sombra",
                                                     "¿Cuál elección te interesa observar en el primer panel?",
                                                     choices =  c("Diputado Local 2021" = "dl21" , "Diputado Federal 2021" = "fed21", "Alcalde 2021" = "ayunt21", "Gobernador 2021" = "gob21",
                                                                  "Diputado Local 2018" = "dipl18", "Senado 2018" = "sen18", "Alcalde 2018" = "ayunt18",
                                                                  "Presidente 2018" = "pres18", "Diputado Federal 2018" = "fed18",
                                                                  "Gobernador 2015" = "gob15", "Alcalde 2015" = "ayunt15", "Diputado Local 2015" = "dl15",
                                                                  "Diputado Federal 2015" = "fed15",
                                                                  "Presidente 2024" = "pres24", 
                                                                  "Senado 2024" = "sen24",
                                                                  "Diputado Federal 2024" = "fed24", 
                                                                  "Alcalde 2024" = "ayunt24",
                                                                  "Diputado Local 2024" = "dipl24"
                                                     ))
                              ),
                              box(width = 12,status = "warning",
                                  downloadBttn(outputId = "dw_csv_sombra",label = "CSV",size = "sm",icon = icon("file-csv"),
                                               color = "warning",style = "simple"),
                                  DT::DTOutput("dt_sombra_sombra")
                              )
                ),
                shiny::column(width = 9,
                              box(width = 12,status = "success",
                                  leafletOutput("mapa_sombra", height = "80vh",width = "100%"),
                                  shiny::actionButton("mapSombraFullscreen", "Fullscreen",)
                              )
                )
              ),
              
              
      ),
      
      ### Distancia Primer-Segundo ----
      tabItem(tabName = "diferencias",
              fluidRow(
                box(
                  title = "Distancia Primer-Segundo Lugar Histórica", collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = FALSE,
                  status = "info",
                  p("")
                )
              ),
              fluidRow(
                column(width = 3,
                       box(width = 12,
                           status = "warning",
                           shiny::selectInput("eleccion_diferencia",
                                              "¿Cuál elección te interesa observar en el panel?",
                                              choices =  c("Diputado Local 2021" = "dl21" , "Diputado Federal 2021" = "fed21", "Alcalde 2021" = "ayunt21", "Gobernador 2021" = "gob21",
                                                           "Diputado Local 2018" = "dipl18", "Senado 2018" = "sen18", "Alcalde 2018" = "ayunt18",
                                                           "Presidente 2018" = "pres18", "Diputado Federal 2018" = "fed18",
                                                           "Gobernador 2015" = "gob15", "Alcalde 2015" = "ayunt15", "Diputado Local 2015" = "dl15",
                                                           "Diputado Federal 2015" = "fed15",
                                                           "Presidente 2024" = "pres24", 
                                                           "Senado 2024" = "sen24",
                                                           "Diputado Federal 2024" = "fed24",
                                                           "Diputado Local 2024" = "dipl24",
                                                           "Alcalde 2024" = "ayunt24"
                                              ))
                       ),
                       box(width = 12,status = "warning",
                           downloadBttn(outputId = "dw_csv_distancia",label = "CSV",size = "sm",icon = icon("file-csv"),color = "warning",style = "simple"),
                           DT::DTOutput("dt_diferencia_diferencia")
                       )
                ),
                box(width = 9,status = "success",
                    leafletOutput("mapa_diferencia", height = "80vh"),
                    shiny::actionButton("mapDiferenciaFullscreen", "Fullscreen") 
                )
              )
      ),
      
      ### Demografía Espacial ----
      tabItem(tabName = "edades",
              fluidRow(
                box(
                  title = "Demografía Espacial", collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = F,
                  status = "success",
                  p("Esta herramienta permite visualizar la demografía espacial basada en grupos de edad y género específicos. A continuación, se detallan los pasos para utilizarla:"),
                  p("1.	Selección de grupos de edad: En la sección 'Selecciona todos los grupos de edad que te interesa tomar en cuenta', usted puede elegir uno o varios grupos de edad que deseas visualizar en el mapa. Los grupos disponibles son:"),
                  p(" ▪	18 a 24"),
                  p(" ▪	25 a 29"),
                  p(" ▪	30 a 39"),
                  p(" ▪	40 a 49"),
                  p(" ▪	50 a 59"),
                  p("▪	Mayores"),
                  p(""),
                  p(" 2.	Selección de género: Similar a la selección de grupos de edad, en la sección 'Selecciona los géneros que te interesa tomar en cuenta', puede elegir visualizar la información de hombres, mujeres, o ambos."),
                  p(" 3.	Visualización de datos: Tras seleccionar los criterios deseados, el mapa se actualizará automáticamente mostrando las diferentes secciones en donde existen más personas que cumplen con las características seleccionadas. Cada sección vendrá con información sobre el porcentaje de la población con estas características.
                           Los colores en el mapa representan la proporción de la población seleccionada en cada sección, con una gama cromática que va del naranja al azul. Además, se indican los diferentes distritos con líneas de distintos colores."),
                  p("4.	Detalle de cada sección: Al posicionar el cursor sobre una sección del mapa, se desplegará un cuadro emergente mostrando detalles de dicha sección, incluyendo el distrito y la proporción (en porcentaje) de la población que coincide con las características seleccionadas."),
                  p("Por favor, utilice esta herramienta para explorar la distribución demográfica basada en grupos de edad y género a nivel local.")
                ),
                box(width = 3,
                    shiny::selectInput("edades",
                                       "Selecciona todos los grupos de edades que te interesa tomar en cuenta",
                                       choices =  c("18 a 24" = "_18_24",
                                                    "25 a 29" = "_25_29",
                                                    "30 a 39" = "_30_39",
                                                    "40 a 49" = "_40_49", 
                                                    "50 a 59" = "_50_59",
                                                    "Mayores" = "mayores"
                                       ), selected = c("18 a 24" = "_18_24",
                                                       "25 a 29" = "_25_29",
                                                       "30 a 39" = "_30_39",
                                                       "40 a 49" = "_40_49", 
                                                       "50 a 59" = "_50_59",
                                                       "Mayores" = "mayores"),
                                       
                                       multiple = T),
                    shiny::selectInput("genero",
                                       "Selecciona los géneros que te interesa tomar en cuenta",
                                       choices =  c("Hombre" = "hombres", "Mujer" = "mujeres"
                                       ), selected = c("Hombre" = "hombres", "Mujer" = "mujeres"), multiple = T)
                ),
                box(width = 9,
                    leafletOutput("mapa_generos_edad", height = "80vh"),
                    shiny::actionButton("mapGenerosEdadFullscreen", "Fullscreen"),
                    downloadButton("download_edades", "Descargar la Información como CSV")
                    
                )
              )
      ),
      
      ### Archivo Electoral ----
      tabItem(tabName = "creacion_tablas",
              
              fluidRow(
                box(
                  title = "Archivo Electoral ", collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = F,
                  status = "success",
                  p("La Herramienta de Archivo Electoral permite visualizar la cantidad de votos obtenidos por los partidos en diferentes elecciones. Los votos pueden ser filtrados por elección y distrito. A continuación se detallan los pasos para utilizar la herramienta:"),
                  p("	1.	Selección de la Elección: En el campo 'De cuál elección te interesa observar la cantidad de votos', usted puede seleccionar la elección de la que desea ver los votos. Las opciones incluyen varias elecciones federales y locales que ocurrieron en diferentes años."),
                  p("	2.	Selección de Tipo de Distrito: En el campo 'Elige si quieres filtrar por distrito local o federal', puede escoger entre un tipo de distrito local o federal, según su interés."),
                  p("3.	Selección de Distrito: Una vez ha elegido el tipo de distrito, un nuevo campo aparecerá donde puede seleccionar el distrito específico de su interés."),
                  p("4.	Visualización de Datos: Una vez ha seleccionado los criterios deseados, la tabla se actualizará automáticamente para mostrar el número de votos obtenidos en la elección y distrito seleccionados, desglosados por partido político."),
                  p("	5.	Descarga de Datos: Adicionalmente, tiene la opción de descargar la tabla generada como un archivo CSV para su uso posterior. Para hacer esto, presione el botón 'Descargar la tabla como CSV'."),
                  p("Esta herramienta es útil para visualizar y analizar los resultados de votación de las elecciones pasadas, permitiendo un análisis detallado basado en ubicaciones y partidos políticos específicos.")
                ),
                box(width = 3,  
                    
                    shinyWidgets::pickerInput(inputId = "pik_secc_archivo",
                                              label = "Seleccione secciones disponibles",
                                              options = list(
                                                size = 10,
                                                `live-search` = TRUE,
                                                `actions-box` = TRUE,
                                                `deselect-all-text` = "Deseleccionar Todo",
                                                `select-all-text` = "Seleccionar Todo"),
                                              choices = sort(unique(cant_votos_nl$seccion)),
                                              selected = unique(cant_votos_nl$seccion),
                                              multiple = T),
                    shinyWidgets::pickerInput(inputId = "pik_partido_archivo",
                                              label = "Seleccione partidos disponibles",
                                              options = list(
                                                size = 10,
                                                `live-search` = TRUE,
                                                `actions-box` = TRUE,
                                                `deselect-all-text` = "Deseleccionar Todo",
                                                `select-all-text` = "Seleccionar Todo"),
                                              choices = unique(cant_votos_nl$partido),
                                              selected = unique(cant_votos_nl$partido),
                                              multiple = T),
                    shinyWidgets::pickerInput(inputId = "pik_elecc_archivo",
                                              label = "Seleccione eleccion disponibles",
                                              options = list(
                                                size = 10,
                                                `live-search` = TRUE,
                                                `actions-box` = TRUE,
                                                `deselect-all-text` = "Deseleccionar Todo",
                                                `select-all-text` = "Seleccionar Todo"),
                                              choices = unique(cant_votos_nl$eleccion),
                                              selected = unique(cant_votos_nl$eleccion),
                                              multiple = T),
                    shinyWidgets::pickerInput(inputId = "pik_año_archivo",multiple = T,
                                              label = "Seleccione años disponibles",
                                              options = list(
                                                size = 10,
                                                `live-search` = TRUE,
                                                `actions-box` = TRUE,
                                                `deselect-all-text` = "Deseleccionar Todo",
                                                `select-all-text` = "Seleccionar Todo"),
                                              choices = sort(unique(cant_votos_nl$año)),
                                              selected = unique(cant_votos_nl$año))
                ),
                
                shinydashboard::tabBox(width = 9,
                                       tabPanel("Tabla agregada",
                                                div(style = "display:flex;gap:5px;padding:5px;",
                                                    downloadBttn(outputId = "dw_archivo_csv_archivo",label = "CSV",size = "sm",icon = icon("file-csv"),
                                                                 color = "warning",style = "simple")
                                                ),
                                                DT::dataTableOutput("dt_agregada_archivo")
                                       ),
                                       tabPanel("Tabla desagregada",
                                                div(style = "display:flex;gap:5px;padding:5px;",
                                                    downloadBttn(outputId = "dw_archivo_csv_2_archivo",label = "CSV",size = "sm",icon = icon("file-csv"),
                                                                 color = "warning",style = "simple")
                                                ),
                                                DT::dataTableOutput("dt_desagregada_archivo")
                                       )
                )
                
              )
      ),
      
      ### Rendimiento Histórico ----
      tabItem(tabName = "pri",
              fluidRow(
                box(
                  title = "Rendimiento Histórico", collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = F,
                  status = "success",
                  p("La Herramienta de Visualización de Rendimiento Histórico de Partidos ofrece un mapa interactivo que muestra el rendimiento del partido seleccionado en diversas elecciones. A continuación los pasos para utilizar esta herramienta:"),
                  p("	1.	Selección del Partido Político: En el campo 'Selecciona el partido de interés', puede seleccionar el partido político de su interés entre las opciones dadas."),
                  p("	2.	Selección de las Candidaturas: En el campo 'Selecciona las candidaturas para ver donde el PRI las ganó', puede seleccionar una o varias elecciones o categorías de cargos. Esta selección permitirá visualizar en qué secciones el partido seleccionado ganó en las candidaturas que eligió."),
                  p("	3.	Visualización del Mapa: Una vez seleccionados los criterios deseados, el mapa se actualizará automáticamente para mostrar las secciones donde el partido político seleccionado ganó las elecciones en las categorías seleccionadas. Si no se selecciona ninguna elección, el mapa mostrará todas las secciones donde el partido no ganó ninguna elección."),
                  p("	4.	Información Detallada de las Secciones: Uno puede hacer clic en una sección específica en el mapa para obtener información más detallada sobre esa sección."),
                  p("Esta herramienta es útil para analizar el rendimiento histórico de los partidos en diferentes elecciones y categorías de cargos. También proporciona una representación visual de las áreas de fortaleza y debilidad de cada partido. Los datos están geográficamente representados, lo que facilita la identificación de patrones espaciales y regionales.")
                )), 
              fluidRow(
                column(width = 12,
                       box(width = 9, 
                           shiny::selectInput("partido_ganador", "Selecciona el partido de interés",
                                              choices = c("PRI", "PAN", "MC", "morena", "pt")),
                           shiny::selectInput("ganadas_pri", 
                                              "Selecciona las candidaturas para ver donde el PRI las gano",
                                              choices =  c("Diputado Local 2021" = "dip_local_21" , "Diputado Federal 2021" = "dip_fed_21", "Alcalde 2021" = "alcalde_21", "Gobernador 2021" = "gober_21",
                                                           "Presidente 2018" = "pres_18", "Diputado Federal 2018" = "dip_fed18",
                                                           "Diputado Local 2018" = "dip_local_18", "Senado 2018" = "senado_18", "Alcalde 2018" = "alcalde_18",
                                                           "Gobernador 2015" = "gober_15", "Alcalde 2015" = "alcalde_15",
                                                           "Diputado Local 2015" = "dip_local_15", "Diputado Federal 2015" = "dip_fed_15",
                                                           "Presidente 2024" = "pres_24", 
                                                           "Senado 2024" = "sen_24", 
                                                           "Diputado Local 2024" = "dip_local_24", 
                                                           "Diputado Federal 2024" = "dip_fed24", 
                                                           "Alcalde 2024" = "alcalde_24"
                                              ), multiple = TRUE, selected = c("Diputado Local 2021" = "dip_local_21")
                           )
                       )),
                column(width = 8,
                       box(width = 9,
                           
                           leafletOutput("mapa_pri", height = "80vh"),
                           shiny::actionButton("mapPriFullscreen", "Fullscreen"),
                           downloadButton("download_quitar", "Descargar la tabla como CSV")
                       )
                ))
              
      ),
      tabItem(tabName = "simulaciones",
              fluidRow(
                box(
                  title = "Simulador electoral", collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = F,
                  status = "success",
                  p("El simulador electoral permite a los usuarios generar y visualizar posibles escenarios electorales basados en las elecciones reales. Aquí se explica cómo funciona y cómo utilizarlo:"),
                  p("	1.	Selección de la Elección a Simular: Este es el primer paso para utilizar la herramienta. Debes seleccionar la elección que deseas simular a partir de una lista predefinida de elecciones."),
                  p("	2.	Selección del Partido a Eliminar: Luego, debes seleccionar el partido político que deseas eliminar del escenario electoral. Esto representa que ese partido no participó en las elecciones."),
                  p("3.	Destinatario de los Votos del Partido Eliminado: En el tercer paso, debes seleccionar el partido que recibiría los votos del partido eliminado."),
                  p("	4.	Visualización de la Simulación: Después de hacer tus selecciones, la simulación se activará. Se generará un nuevo conjunto de datos en los que los votos del partido eliminado se redistribuirán al partido seleccionado. Este nuevo conjunto de datos se visualizará en forma de mapa interactivo."),
                  p("En este mapa, los colores representan el partido que habría ganado en cada sección si los votos se hubieran redistribuido de la manera prevista. Al pasar el cursor sobre una sección, se muestra un cuadro con más detalles sobre los resultados simulados para esa sección."),
                  p("Además, si seleccionas el mismo partido para eliminar y como destinatario de los votos, se simulará el escenario como si ese partido simplemente no hubiera participado en la elección."),
                  p("Esto permite a los usuarios ver cómo diferentes redistribuciones de votos afectarían los resultados de las elecciones y proporciona una herramienta para explorar y comprender mejor el panorama político.")
                ),
                box(width = 3, 
                    shiny::selectInput("eleccion_sim1", 
                                       "Selecciona la elección a simular",
                                       choices =  c("Diputado Local 2021" = "dl21" , "Diputado Federal 2021" = "fed21", "Alcalde 2021" = "ayunt21", "Gobernador 2021" = "gob21",
                                                    "Diputado Local 2018" = "dipl18", "Senado 2018" = "sen18", "Alcalde 2018" = "ayunt18",
                                                    "Presidente 2018" = "pres18", "Diputado Federal 2018" = "fed18",
                                                    "Gobernador 2015" = "gob15", "Alcalde 2015" = "ayunt15", "Diputado Local 2015" = "dipl15",
                                                    "Diputado Federal 2015" = "fed15",
                                                    "Presidente 2024" = "pres24", 
                                                    "Senado 2024" = "sen24",
                                                    "Diputado Local 2024" = "dipl24", 
                                                    "Diputado Federal 2024" = "fed24", 
                                                    "Alcalde 2024" = "ayunt24"
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
                box(width = 9,
                    leafletOutput("mapa_sim1", height = "80vh"),
                    shiny::actionButton("mapSim1Fullscreen", "Fullscreen"),
                    downloadButton("downloadganador_sim", "Descargar la tabla como CSV")
                )
              )
              
      ),
      ### Evolución electoral ----
      tabItem(tabName = "cambios_porcentuales",
              fluidRow(
                box(
                  title = "Evolución Electoral", collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = F,
                  status = "success",
                  p("La herramienta de 'Evolución Electoral' te permite analizar el cambio porcentual en la participación votante de un partido específico entre dos elecciones. Aquí está cómo funciona:"),
                  p("	1.	Elegir la Elección Base y la Segunda Elección: El primer paso es seleccionar una elección base y una segunda elección para la comparación."),
                  p("2.	Seleccionar el Partido Político: A continuación, debes seleccionar el partido político que deseas analizar."),
                  p("	3.	Generar Visualización: Una vez hecho esto, el sistema realizará los cálculos necesarios y generará una visualización que muestra la tasa de cambio en la participación de los votantes del partido seleccionado entre las dos elecciones."),
                  p("4.	Interpretar el Mapa Interactivo: Los resultados se visualizan en un mapa interactivo. Cada zona está coloreada para representar la tasa de cambio en la participación, desde una disminución significativa (rojo oscuro) hasta un aumento significativo (verde oscuro). Las zonas en blanco representan una tasa de cambio neutral."),
                  p("	5.	Detalles de la Zona: Al pasar el cursor sobre una zona, aparecerá una pequeña ventana emergente que proporcionará más detalles. Muestra la tasa de cambio exacta y los porcentajes de votos obtenidos por el partido seleccionado en ambas elecciones."),
                  p("Esta herramienta te permite realizar un análisis geográfico profundo del rendimiento de un partido político a lo largo de las elecciones, identificar zonas de mejora o regresión en la participación y obtener una visión valiosa de los cambios en las tendencias y preferencias de los votantes a través del tiempo.")
                ),
                box(width = 3, 
                    shiny::selectInput("eleccion1_a", 
                                       "Selecciona la elección base",
                                       choices =  c("Diputado Local 2021", "Diputado Federal 2021", "Alcalde 2021", "Gobernador 2021",
                                                    "Diputado Federal 2018", "Presidente 2018", "Diputado Local 2015", "Diputado Federal 2015",
                                                    "Diputado Local 2018", "Senado 2018", "Alcalde 2018", "Alcalde 2015", "Presidente 2024", "Senado 2024",
                                                    "Diputado Federal 2024", "Diputado Local 2024", "Alcalde 2024"
                                       ), selected = "Diputado Local 2015"),
                    shiny::selectInput("eleccion2_a", 
                                       "Selecciona la segunda elección ",
                                       choices =  c("Diputado Federal 2021", "Alcalde 2021", "Diputado Local 2021",  "Gobernador 2021",
                                                    "Diputado Federal 2018", "Presidente 2018", "Diputado Local 2015", "Diputado Federal 2015", 
                                                    "Diputado Local 2018", "Senado 2018", "Alcalde 2018", "Alcalde 2015", "Presidente 2024", "Senado 2024",
                                                    "Diputado Federal 2024", "Diputado Local 2024", "Alcalde 2024"
                                       ),  selected = "Diputado Local 2021"),
                    shiny::selectInput("partido_analisis_a", 
                                       "Selecciona al partido que quieres analizar ",
                                       choices =  c("pan", "pri", "morena", "mc", "verde", "pt"))
                ),
                box(width = 9,
                    h3("Tasa de cambio entre las dos elecciones "),
                    leafletOutput("mapa_cambios_perc", height = "80vh"),
                    shiny::actionButton("mapCambiosPercFullscreen", "Fullscreen"),
                    downloadButton("download_tasas", "Descargar la tabla como CSV")
                )
              )
              
      ),
      ### Evaluación Comparativa ----
      tabItem(tabName = "robados",
              fluidRow(
                box(
                  title = "Evaluación Comparativa", collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = F,
                  status = "success",
                  p("La pestaña 'Evaluación comparativa' presenta un análisis comparativo entre diferentes elecciones y 
                      destaca las áreas donde un partido político específico ha ganado o perdido en 
                      comparación con la elección anterior."),
                  p("En esta pestaña, puedes seleccionar dos elecciones diferentes para comparar 
                      y especificar el partido político de interés. La comparación analiza los ganadores 
                      en las secciones electorales para cada elección y determina si el partido de 
                      interés ha ganado, perdido o mantenido la misma posición en la segunda elección 
                      en comparación con la primera."),
                  p("El resultado se visualiza en tres mapas interactivos:"),
                  p("	1.	El primer mapa muestra en qué secciones el partido seleccionado ganó o perdió. 
                      Las secciones donde el partido ganó se resaltan en verde, las secciones donde perdió 
                      en rojo, y las secciones donde se mantuvo igual son de color gris si el partido perdió 
                      en ambas elecciones o de color blanco si ganó."),
                  p("	2.	El segundo mapa se centra en las secciones donde el partido ganó en la segunda elección. 
                      Muestra a quién le ganó el partido la sección."),
                  p("	3.	El tercer mapa se centra en las secciones donde el partido perdió en la segunda 
                      elección. Muestra frente a quién perdió el partido la sección."),
                  p("Estas visualizaciones te permiten analizar y entender mejor la dinámica de 
                      los votantes entre las diferentes elecciones y cómo diversos 
                      partidos han ganado o perdido terreno. Estos insights pueden ser útiles 
                      para desarrollar estrategias para futuras elecciones o comprender mejor la 
                      percepción y comportamiento de los votantes de un distrito.")
                ),
                box(width = 3, 
                    shiny::selectInput("eleccion1", 
                                       "Selecciona la elección base",
                                       choices =  c("Diputado Local 2021", "Diputado Federal 2021", "Alcalde 2021", "Gobernador 2021",
                                                    "Presidencia 2018", "Diputado Federal 2018",
                                                    "Diputado Local 2018", "Senado 2018", "Alcalde 2018", "Alcalde 2015", "Gobernador 2015",
                                                    "Presidencia 2024", "Senado 2024", "Diputado Local 2024", "Alcalde 2024", "Diputado Federal 2024"
                                       )),
                    shiny::selectInput("eleccion2", 
                                       "Selecciona la segunda elección ",
                                       choices =  c("Diputado Local 2021", "Diputado Federal 2021", "Alcalde 2021", "Gobernador 2021",
                                                    "Presidencia 2018", "Diputado Federal 2018",
                                                    "Diputado Local 2018", "Senado 2018", "Alcalde 2018", "Alcalde 2015", "Gobernador 2015",
                                                    "Presidencia 2024", "Senado 2024", "Diputado Local 2024", "Alcalde 2024", "Diputado Federal 2024"
                                       )),
                    shiny::selectInput("partido_analisis", 
                                       "Selecciona al partido que quieres analizar ",
                                       choices =  c("PAN", "PRI", "morena", "MC", "INDEPE", "VERDE", "pt"))
                ),
                box(width = 9,
                    h3("Aspecto general "),
                    leafletOutput("mapa_gana_o_pierde", height = "80vh"),
                    shiny::actionButton("mapGanaOPierdeFullscreen", "Fullscreen"),
                    downloadButton("download_robados", "Descargar la tabla como CSV"),
                    h3("Ganadas por el partido en la segunda elección"),
                    leafletOutput("mapa_gana", height = "80vh"),
                    shiny::actionButton("mapGanaFullscreen", "Fullscreen"),
                    h3("Perdidas por el partido en la segunda elección"),
                    leafletOutput("mapa_pierde", height = "80vh"),
                    shiny::actionButton("mapPierdeFullscreen", "Fullscreen")
                )
              )
      ),
      ### Gestión de tiempo ---- 
      tabItem(tabName = "tiempoxsección",
              fluidRow(
                box(
                  title = "Gestión de tiempo de campaña", collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = F,
                  status = "success",
                  p("En esta pestaña podrás observar cuantas horas deben dedicarse en total a cada sección en los días de campaña.
                       El tiempo dedicado a cada sección depende de la cantidad de días de campaña y de la cantidad de equipos que existen 
                       para recorrer el municipio."),
                  p("Además, si das click a una sección te dirá si es recomendable promocionarte como PAN o como PRI en ese espacio.")
                ),
                box(width = 3, 
                    shiny::sliderInput("dias_campaña", "Selecciona la cantidad de días de campaña", min = 0, max = 120, step = 10, value = 90),
                    shiny::sliderInput("equipos", 
                                       "Selecciona la cantidad de equipos que van a recorrer el municipio",
                                       min = 1, max = 20, step = 1, value = 3)
                ),
                box(width = 9,
                    h3("Aspecto general "),
                    leafletOutput("mapa_visitas_secciones", height = "80vh"),
                    shiny::actionButton("mapVisitasFullscreen", "Fullscreen"),
                    downloadButton("download_basej", "Descargar la base informativa")
                )
              )
      ),
      ### Perfil de colonias ----
      tabItem(tabName = "información_colonias",
              fluidRow(
                box(
                  title = "Perfiles de colonias", collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = F,
                  status = "success",
                  p("En la pestaña 'Información de Colonias', encontrarás una serie de opciones que te permitirán filtrar y visualizar información específica relacionada con los datos demográficos y los resultados de votación de diferentes colonias."),
                  p("Primero, puedes seleccionar una colonia específica de interés a través del cuadro de selección 'Selecciona la colonia de interés'. Una vez que hayas seleccionado la colonia, tendrás otras dos opciones de filtrado: "),
                  p("En 'Selecciona una elección para ver los resultados en la colonia', puedes seleccionar una elección específica para ver los resultados de votación de la colonia seleccionada en esa elección. Las elecciones van desde 2021 hasta 2015 e incluyen diferentes niveles de cargo, como diputado local, diputado federal, alcalde y gobernador."),
                  p("En 'Selecciona el partido para ver los resultados en la colonia', puedes especificar un partido político para ver cuántos votos consiguió en la colonia seleccionada. Las opciones van desde partidos populares como PAN, PRI, Morena, hasta MC e Independientes."),
                  p("Después del filtrado, verás un texto que resume la información demográfica básica de la colonia seleccionada, como la población total, el grado medio de escolaridad, el porcentaje de personas nacidas en la colonia, y la proporción de jóvenes y mayores. "),
                  p("Justo debajo de este texto, hay dos gráficos:"),
                  p("El primero, muestra la proporción de votos que cada partido obtuvo en la colonia seleccionada para la elección específica que seleccionaste."),
                  p("El segundo gráfico muestra cómo se ha desempeñado un partido específico a lo largo del tiempo, en términos de la proporción de votos recibidos en la colonia seleccionada. Este gráfico es útil para visualizar las tendencias de votación para un partido específico a lo largo de diferentes elecciones.")
                ),
                box(width = 3, 
                    uiOutput("colonias_seleccionadas")
                ),
                box(width = 3,
                    textOutput("texto_colonia")
                ),
                box(width = 3,
                    shiny::selectInput("eleccion_colonia", 
                                       "Selecciona una elección para ver los resultados de esa elección en la colonia",
                                       choices =  c("Diputado Local 2021" = "dl21" , "Diputado Federal 2021" = "fed21", "Alcalde 2021" = "ayunt21", "Gobernador 2021" = "gob21",
                                                    "Diputado Local 2018" = "dipl18", "Senado 2018" = "sen18", "Alcalde 2018" = "ayunt18",
                                                    "Presidente 2018" = "pres18", "Diputado Federal 2018" = "fed18",
                                                    "Gobernador 2015" = "gob15", "Alcalde 2015" = "ayunt15", "Senado 2024" = "sen24",
                                                    "Alcalde 2024" = "ayunt24", "Diputado Federal 2024" = "fed24", "Diputado Local 2024" = "dipl24"
                                                    
                                       )
                                       
                    )),
                box(width = 3,
                    h3("Resultados de la elección"),
                    plotOutput("grafico_colonia"),
                    downloadButton("download_votos_colonia", "Descargar la tabla como CSV")
                ),
                box(width = 3,
                    shiny::selectInput("partido_colonia", 
                                       "Selecciona el partido para ver los resultados históricos de ese partido en la colonia",
                                       choices =   c("PAN" = "pan", "PRI" = "pri", "Morena" = "morena", "MC" = "mc", "Independiente" = "indep" , "Verde" = "verde", "pt")
                    )
                ),
                box(width = 9,
                    h3("Resultados históricos del partido en la colonia"),
                    plotOutput("grafico_colonia_tiempo"),
                    downloadButton("download_votos_colonia_tiempo", "Descargar la tabla como CSV")
                )
              )
              
      ),
      ### Fortaleza Electoral ----
      tabItem(tabName = "probabilidades",
              fluidRow(
                box(
                  title = "Fortaleza electoral", collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = F,
                  status = "success",
                  p("En la pestaña 'Fortalezas', encontrarás funcionalidades que te ayudarán a explorar la fortaleza relativa de los diferentes partidos políticos sección por sección en diversas elecciones. Esta información se obtiene utilizando métodos de aprendizaje de máquina con base en los datos de elecciones pasadas y se muestra en un formato fácil de entender."),
                  p("Primero, utilizarás la opción 'Selecciona la elección a analizar' para elegir la elección de la que estás interesado, puedes seleccionar entre Diputado Federal, Diputado Local, Alcalde y Presidencia."),
                  p("Segundo, en 'Selecciona el partido del cual te interesa conocer la fortaleza', definirás el partido político cuya fortaleza relativa te interesa explorar. Las opciones son PRI, morena, MC y PAN."),
                  p("Una vez definidos estos dos filtros, la aplicación te mostrará un mapa dividido en secciones, donde cada sección representa una región electoral. Cada sección estará coloreada de acuerdo a la fortaleza del partido seleccionado en esa sección. Las fortalezas varían desde Muy Baja hasta Muy Alta. Además, las secciones están delimitadas con diferentes colores y patrones de línea en función del distrito al que pertenecen."),
                  p("Si pasas el cursor sobre una sección específica en el mapa, verás un cuadro emergente que dará más detalles sobre esa sección y la fortaleza del partido seleccionado en esa sección en particular."),
                  p("En resumen, esta pestaña 'Fortalezas' proporciona una forma interactiva y visual de explorar cómo se desempeñan los diferentes partidos políticos en diversas zonas utilizando métodos de aprendizaje de máquina basándose en los datos de elecciones pasadas.")
                  
                ),
                fluidRow(
                  box(width = 3,
                      shiny::selectInput("partido_predecir", "Selecciona al partido del que te interesa conocer la fortaleza de ganar",
                                         c("PRI", "morena", "MC", "PAN", "VERDE", "pt"))
                  )),
                box(width = 12,
                    h3("Fortaleza"),
                    leafletOutput("mapa_fortaleza", height = "80vh"),
                    shiny::actionButton("mapFortalezaFullscreen", "Fullscreen"),
                    downloadButton("download_fortalezas", "Descargar la tabla como CSV",)
                )
              )
              
      ),
      
      ### Datos censales ----
      tabItem(tabName = "censales",
              box(
                title = "Datos censales",collapsible = T,collapsed = T,
                width = 12,
                solidHeader = F,
                status = "success",
                p("En la pestaña 'Fortalezas', encontrarás funcionalidades que te ayudarán a explorar la fortaleza relativa de los diferentes partidos políticos sección por sección en diversas elecciones. Esta información se obtiene utilizando métodos de aprendizaje de máquina con base en los datos de elecciones pasadas y se muestra en un formato fácil de entender."),
                p("Primero, utilizarás la opción 'Selecciona la elección a analizar' para elegir la elección de la que estás interesado, puedes seleccionar entre Diputado Federal, Diputado Local, Alcalde y Presidencia."),
                p("Segundo, en 'Selecciona el partido del cual te interesa conocer la fortaleza', definirás el partido político cuya fortaleza relativa te interesa explorar. Las opciones son PRI, morena, MC y PAN."),
                p("Una vez definidos estos dos filtros, la aplicación te mostrará un mapa dividido en secciones, donde cada sección representa una región electoral. Cada sección estará coloreada de acuerdo a la fortaleza del partido seleccionado en esa sección. Las fortalezas varían desde Muy Baja hasta Muy Alta. Además, las secciones están delimitadas con diferentes colores y patrones de línea en función del distrito al que pertenecen."),
                p("Si pasas el cursor sobre una sección específica en el mapa, verás un cuadro emergente que dará más detalles sobre esa sección y la fortaleza del partido seleccionado en esa sección en particular."),
                p("En resumen, esta pestaña 'Fortalezas' proporciona una forma interactiva y visual de explorar cómo se desempeñan los diferentes partidos políticos en diversas zonas utilizando métodos de aprendizaje de máquina basándose en los datos de elecciones pasadas.")
              ),
              fluidRow(
                column(width = 4,
                       box(width = 12,
                           shiny::selectInput("censo_interes", "Elige la variable de la que te interesa obtener más información",
                                              choices = chs_censales, selected = chs_censales[1]),
                           downloadBttn(outputId = "dw_censal_csv_censales",label = "CSV",size = "sm",icon = icon("file-csv"),color = "warning",style = "simple"),
                           DTOutput("dt_query_censales")
                       )
                ),
                column(width = 8,
                       box(
                         width = 12,
                         leafletOutput("mapa_censales", height = "80vh"),
                         shiny::actionButton("mapFullscreen", "Fullscreen")
                       )
                ),
                
              )
      ),
      tabItem(tabName = "censales_mza",
              fluidRow(
                box(
                  title = "Datos censales por Manzana",collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = F,
                  status = "success"),
                fluidRow(
                  column(width = 4,
                         box(width = 12,
                             shiny::selectInput("censo_interes_mza", "Elige la variable de la que te interesa obtener más información",
                                                choices = c("Población total" = "POBTOT",
                                                            "Población Femenina" = "POBFEM",
                                                            "Población Masculina" = "POBMAS",
                                                            "Población mayor a 60 años" = "POB60MA",
                                                            "Población Indígena" = "INDIGEN",
                                                            "Población Afromexicana" = "AFROMEX",
                                                            "Población desempleada" = "DESEMPL",
                                                            "Viviendas deshabitadas" = "VIV_DES",
                                                            "Población Analfabeta" = "ANALFAB"
                                                            
                                                ), selected = c("Población total" = "POBTOT")
                             ),
                             
                             
                         )
                         
                  )
                ),
                box(
                  width = 12,
                  leafletOutput("mapa_manzanas", height = "80vh"),
                  shiny::actionButton("mapManzanasFullscreen", "Fullscreen")
                )
              )
      )
      
      
    )
  )
)




# ui_secure <- secure_app(ui_code, background  = "linear-gradient(rgba(13, 59, 102, 0.5), rgba(250, 240, 202, 0.5)),
#                         url('https://aledomicom.files.wordpress.com/2024/03/bienvenida_quipu.png')  repeat center fixed;", language = "es")




ui <- ui_code



