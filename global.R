# ==============================================================================
# Archivo Global para la Aplicación Shiny
# ------------------------------------------------------------------------------
# Este script se ejecuta una sola vez cuando la aplicación se inicia.
# Aquí se cargan las librerías, se leen los datos, se procesan y se
# definen los objetos globales que estarán disponibles en ui.R y server.R.
# ==============================================================================

## 1. Carga de Librerías ----
# ------------------------------------------------------------------------------
# Agrupamos todas las librerías necesarias al inicio del script.

# Core de Shiny y UI
library(shiny)
library(shinydashboard)
library(shinymanager)
library(shinyWidgets)
library(shinyjs)

# Visualización y Tablas
library(DT)
library(plotly)
library(leaflet)

# Manipulación de Datos y Spatial
library(sf)
library(tidyverse)
library(scales)
library(colorRamps)
library(readxl)


## 2. Constantes, Paletas de Colores y Opciones Fijas ----
# ------------------------------------------------------------------------------
# Objetos que no cambian y son usados a lo largo de la app, como colores
# o mapeos de nombres.

# Paletas de colores para partidos políticos
color_partido_2 <- c("MORENA" = "#B41A1A",  
                     "PAN"    = "#0F58A8",  
                     "PRI"    = "#FF0000",  
                     "MC"     = "#FD7A13",  
                     "VERDE"  = "#228B22",  
                     "PT"     = "#FFF200",  
                     "INDEP"  = "#A000E5")

paleta_partidos <- c(
  "PRI" = "#e41a1c",      # Rojo
  "PAN" = "#377eb8",      # Azul
  "MC" = "#ff7f00",       # Naranja
  "MORENA" = "#a65628",   # Marrón/Guinda
  "PT" = "#b30000",       # Rojo oscuro
  "VERDE" = "#4daf4a",    # Verde
  "INDEPE" = "#7678ed"     # Gris
  # Añade más partidos y colores según sea necesario
)

# Opciones para selects de variables censales (manzanas y secciones)
chs_censales <- c("Población total" = "POBTOT", 
                  "Población Femenina" = "POBFEM",
                  "Población Masculina" = "POBMAS", 
                  "Población Indígena" = "PHOG_IND",
                  "Población Afromexicana" = "POB_AFRO",
                  "Población con discapacidad" = "PCON_DISC",
                  "Población de 12 años y más ocupada" = "POCUPADA",
                  "Población de 12 años y más desocupada" = "PDESOCUP",
                  "Viviendas particulares habitadas" = "VIVPAR_HAB",
                  "Viviendas particulares deshabitadas" = "VIVPAR_DES",
                  "Grado promedio de escolaridad" = "GRAPROES",
                  "Promedio de ocupantes en viviendas particulares habitadas" = "PROM_OCUP")

# Mapeo de nombres de elecciones para la UI
choice_mapping <- c("Diputado Local 2021"   = "dip_local_21",
                    "Diputado Federal 2021" = "dip_fed_21",
                    "Alcalde 2021"          = "alcalde_21",
                    "Gobernador 2021"       = "gober_21",
                    "Presidencia 2018"      = "pres_18",
                    "Diputado Federal 2018" = "dip_fed18", 
                    "Diputado Local 2018"   = "dip_local_18",
                    "Senado 2018"           = "senado_18",
                    "Alcalde 2018"          = "alcalde_18",
                    "Alcalde 2015"          = "alcalde_15",
                    "Gobernador 2015"       = "gober_15",
                    "Diputado Federal 2015" = "dip_fed_15",
                    "Diputado Local 2015"   = "dip_local_15",
                    "Presidencia 2024"      = "pres_24", 
                    "Senado 2024"           = "sen_24",
                    "Diputado Federal 2024" = "dip_fed24", 
                    "Diputado Local 2024"   = "dip_local_24", 
                    "Alcalde 2024"          = "alcalde_24")

# Lista para selectores que solo necesitan los nombres
lista_elecciones_nombres <- list(
  "Elecciones 2024" = c("Alcalde 2024", "Diputado Federal 2024", "Diputado Local 2024", "Presidencia 2024", "Senado 2024"),
  "Elecciones 2021" = c("Alcalde 2021", "Diputado Federal 2021", "Diputado Local 2021", "Gobernador 2021"),
  "Elecciones 2018" = c("Alcalde 2018", "Diputado Federal 2018", "Diputado Local 2018", "Presidencia 2018", "Senado 2018"),
  "Elecciones 2015" = c("Alcalde 2015", "Diputado Federal 2015", "Diputado Local 2015", "Gobernador 2015")
)


choices_elections_sombra <- list(
  "Elecciones 2024" = c("Presidente 2024" = "pres24", "Senado 2024" = "sen24", "Diputado Federal 2024" = "fed24", "Diputado Local 2024" = "dipl24", "Alcalde 2024" = "ayunt24"),
  "Elecciones 2021" = c("Gobernador 2021" = "gob21", "Diputado Federal 2021" = "fed21", "Diputado Local 2021" = "dl21", "Alcalde 2021" = "ayunt21"),
  "Elecciones 2018" = c("Presidente 2018" = "pres18", "Senado 2018" = "sen18", "Diputado Federal 2018" = "fed18", "Diputado Local 2018" = "dipl18", "Alcalde 2018" = "ayunt18"),
  "Elecciones 2015" = c("Diputado Federal 2015" = "fed15", "Diputado Local 2015" = "dipl15", "Alcalde 2015" = "ayunt15")
)

# Paleta de colores divergente para representar cambios positivos y negativos
colores_cambio <- c(
  # Rojos (disminución)
  "#800f2f", "#a4133c", "#c9184a", "#ff4d6d", "#ff758f", 
  "#ffb3c1", "#ffccd5", "#fff0f3",
  # Neutro
  "#f8f9fa",
  # Verdes (aumento)
  "#d8f3dc", "#b7e4c7", "#95d5b2", "#74c69d", "#52b788", 
  "#40916c", "#2d6a4f", "#1b4332", "#081c15"
)

# Bines para la paleta. Centrados en 0.
bins_cambio <- c(-Inf, -0.8, -0.6, -0.4, -0.2, -0.1, -0.05, -0.01, 0.01, 
                 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, Inf)

# Crear la función de paleta una sola vez
paleta_cambio <- colorBin(
  palette = colores_cambio, 
  domain = NULL, # El dominio se ajustará automáticamente
  bins = bins_cambio, 
  na.color = "#ced4da" # Un color neutro para NA
)


## 3. Carga de Datos Crudos ----
# ------------------------------------------------------------------------------
# Lectura de todos los archivos fuente (SHP, CSV, Excel).
# RECOMENDACIÓN: Para optimizar, considera pre-procesar estos datos
# y guardarlos como archivos .rds, que se cargan mucho más rápido.

# Datos espaciales y geográficos
municipios_lista <- st_read("www/shps/MUNICIPIO.shp") %>% 
  st_drop_geometry() %>% 
  select(MUNICIPIO, NOMBRE)

secciones_prev_shp <- st_read("www/shps/SECCION_2.shp")
manzanas_nl_shp <- st_read("www/shps/censo_manzanas_nl.shp")
d_local_shp <- st_read("www/shps/DISTRITO_LOCAL.shp")
shp_cols_x_secc_shp <- st_read("www/shps/shp_cols_x_secc_simplify/shp_cols_x_secc.shp")
shp_mza_2023_shp <- st_read('www/shps/19m.shp')

# Datos tabulares (estadísticas, votos, etc.)
secciones_sd <- read_csv("www/data/datos_por_seccion_NUEVO_LEON.csv")
censales <- read_csv("www/data/estadisticas_censales_nuevoleon.csv")
cant_votos_nl <- read_csv("www/data/cant_votos_nl.csv")
base_ganadores <- read_csv("www/data/datos_ganadores_NUEVO_LEON.csv")
res_trab <- read_csv("www/data/resultados_trabajado_NUEVO_LEON.csv")
cant_votos <- read_csv("www/data/resultados_trabajado_cant_votos_NUEVO_LEON.csv")
colonias <- read_excel("www/data/colonias.xlsx")
edades <- read_csv("www/data/edades_arreglado.csv")

# Datos de manzanas (CPV 2020)
data_mza_urbana_cpv2020 <- read_csv("www/mzas/conjunto_de_datos_ageb_urbana_19_cpv2020.csv")
data_nl_mza2023_secc2024 <- read_csv("www/mzas/data_nl_mza2023_secc2024.csv")
select_data_cpv <- read.csv("www/mzas/diccionario_datos_ageb_urbana_19_cpv2020.csv")


## 4. Procesamiento y Transformación de Datos ----
# ------------------------------------------------------------------------------
# En esta sección se realizan las uniones (joins), mutaciones y cálculos
# para preparar los datos para la aplicación.

# Procesamiento de secciones
secciones_prev <- secciones_prev_shp %>% 
  left_join(municipios_lista, by = "MUNICIPIO") %>% 
  left_join(select(secciones_sd, SECCION, pobtotal = POBTOT), by = "SECCION") %>% 
  mutate(SECCION = as.character(SECCION)) %>% 
  select(-GEOMETRY1_)

# Transformación de proyecciones espaciales
d_local <- st_transform(d_local_shp, crs = "+proj=longlat +datum=WGS84")
shp_cols_x_secc <- st_transform(shp_cols_x_secc_shp, 4326)
shp_mza_2023 <- st_transform(shp_mza_2023_shp, 4326)

# Limpieza y joins de datos electorales
cant_votos_nl$partido <- toupper(cant_votos_nl$partido)

# base_ganadores <- base_ganadores %>% 
#   mutate(seccion = as.character(seccion)) %>% 
#   left_join(select(secciones_prev, seccion = SECCION, distrito = DISTRITO_L), by = "seccion")

# res_trab <- res_trab %>% 
#   mutate(seccion = as.character(seccion)) %>% 
#   left_join(select(secciones_prev, seccion = SECCION, distrito = DISTRITO_L), by = "seccion")

cant_votos <- cant_votos %>% 
  mutate(seccion = as.character(seccion)) %>% 
  left_join(select(secciones_prev, seccion = SECCION, distrito = DISTRITO_L), by = "seccion")

edades <- edades %>% 
  mutate(seccion = as.character(seccion))

# Procesamiento y agregación de datos censales por manzana y sección
data_mza_secc <- merge(data_nl_mza2023_secc2024, data_mza_urbana_cpv2020, by="CVEGEO", all.x=TRUE)

data_secc_cpv2020 <- data_mza_secc %>%
  filter(!is.na(NOM_ENT)) %>%
  group_by(SECCION, NOM_ENT, NOM_MUN) %>%
  summarise(
    POBTOT = sum(POBTOT, na.rm = TRUE),
    POBFEM = sum(POBFEM, na.rm = TRUE),
    POBMAS = sum(POBMAS, na.rm = TRUE),
    PHOG_IND = sum(PHOG_IND, na.rm = TRUE),
    POB_AFRO = sum(POB_AFRO, na.rm = TRUE),
    PCON_DISC = sum(PCON_DISC, na.rm = TRUE),
    POCUPADA = sum(POCUPADA, na.rm = TRUE),
    PDESOCUP = sum(PDESOCUP, na.rm = TRUE),
    VIVPAR_HAB = sum(VIVPAR_HAB, na.rm = TRUE),
    VIVPAR_DES = sum(VIVPAR_DES, na.rm = TRUE),
    GRAPROES = round(mean(GRAPROES, na.rm = TRUE), 2),
    PROM_OCUP = round(mean(PROM_OCUP, na.rm = TRUE), 2)
  ) %>% 
  ungroup() %>%
  mutate(SECCION = as.character(SECCION))

# Unión de datos espaciales de manzanas con datos censales
seccion_data_para_unir <- data_nl_mza2023_secc2024 %>%
  select(CVEGEO, SECCION, area_intersect)

# Unimos el objeto espacial (izquierda) con el data frame preparado (derecha).
# left_join es el equivalente en dplyr a merge(..., all.x = TRUE)
shp_mza2023_secc2024 <- shp_mza_2023 %>%
  left_join(seccion_data_para_unir, by = "CVEGEO")

columnas_censales_a_unir <- data_mza_urbana_cpv2020 %>%
  select(
    CVEGEO,
    NOM_ENT,
    NOM_MUN,
    NOM_LOC,
    POBTOT:VPH_SINTIC
  )

# Realizamos la unión final. Usaremos left_join de dplyr, es más explícito.
shp_mza2023_secc2024_cpv2020 <- shp_mza2023_secc2024 %>%
  left_join(columnas_censales_a_unir, by = "CVEGEO")


## 5. Creación de Listas para Inputs (Selectores) ----
# ------------------------------------------------------------------------------
# Creamos aquí los vectores que poblarán los `selectInput` y otros
# controles en la interfaz de usuario (UI).



choices_rendimiento_historico = list(
  "Elecciones 2024" = c(
    "Presidencia 2024" = "pres_24",
    "Senado 2024" = "sen_24",
    "Diputado federal 2024" = "dip_fed24",
    "Diputado local 2024" = "dip_local_24",
    "Alcalde 2024" = "alcalde_24"
  ),
  "Elecciones 2021" = c(
    "Gobernador 2021" = "gober_21",
    "Diputado federal 2021" = "dip_fed_21",
    "Diputado local 2021" = "dip_local_21",
    "Alcalde 2021" = "alcalde_21"
  ),
  "Elecciones 2018" = c(
    "Presidencia 2018" = "pres_18",
    "Senado 2018" = "senado_18",
    "Diputado federal 2018" = "dip_fed18",
    "Diputado local 2018" = "dip_local_18",
    "Alcalde 2018" = "alcalde_18"
  ),
  "Elecciones 2015" = c(
    "Gobernador 2015" = "gober_15",
    "Diputado federal 2015" = "dip_fed_15",
    "Diputado local 2015" = "dip_local_15",
    "Alcalde 2015" = "alcalde_15"
  )
)

lookup_table <- tibble(
  display_name = unlist(lapply(choices_rendimiento_historico, names)),
  filter_value = unlist(choices_rendimiento_historico) 
) %>%
  mutate(
    año = as.numeric(str_extract(display_name, "\\d{4}$")), 
    eleccion_type = str_trim(str_remove(display_name, "\\s\\d{4}$")) 
  ) %>%
  select(eleccion_type, año, filter_value) 

cant_votos_nl <- cant_votos_nl %>%
  left_join(
    lookup_table,
    by = c("eleccion" = "eleccion_type", "año" = "año") 
  ) %>%
  rename(eleccion_año_id = filter_value) 

# Opciones geográficas
opciones_municipio <- sort(unique(secciones_prev$NOMBRE))
opciones_local <- sort(unique(secciones_prev$DISTRITO_L))
opciones_federal <- sort(unique(secciones_prev$DISTRITO))

# Opciones de elecciones
list_eleccion <- unique(cant_votos_nl$eleccion)

# Relaciones y mapeos de municipios y colonias
relacion_cve_mun_colonias <- secciones_prev %>%
  filter(!is.na(NOMBRE)) %>%
  group_by(MUNICIPIO, NOMBRE) %>%
  summarise(count = n(), .groups = 'drop')

chs_mun_cols <- setNames(relacion_cve_mun_colonias$MUNICIPIO, relacion_cve_mun_colonias$NOMBRE)
chs_mun_cols_t <- setNames(relacion_cve_mun_colonias$NOMBRE, relacion_cve_mun_colonias$MUNICIPIO)

# Mapeos de indicadores censales para manzanas
chs_poblaciones_manzanas <- setNames(select_data_cpv$Mnemónico, select_data_cpv$Indicador)
chs_poblaciones_manzanas_t <- setNames(select_data_cpv$Indicador, select_data_cpv$Mnemónico)

# Relaciones CVE_MUN y NOM_MUN
relacion_cve_mun <- shp_mza2023_secc2024_cpv2020 %>%
  st_drop_geometry() %>%
  filter(!is.na(NOM_MUN)) %>%
  group_by(CVE_MUN, NOM_MUN) %>%
  summarise(count = n(), .groups = 'drop')

chs_mun <- setNames(relacion_cve_mun$CVE_MUN, relacion_cve_mun$NOM_MUN)
chs_mun_t <- setNames(relacion_cve_mun$NOM_MUN, relacion_cve_mun$CVE_MUN)