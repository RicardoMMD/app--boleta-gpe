# ==============================================================================
# GENERAR_JSON_ESTRUCTURA.R
# ==============================================================================
# Este script lee los RDS definidos en tu global.R, extrae su metadata
# (nombres de columnas, tipos, dimensiones) y guarda un JSON.

library(tidyverse)
library(sf)
library(jsonlite)

# 1. FUNCIÓN AUXILIAR PARA EXTRAER METADATA ------------------------------------
# Esta función actúa como un "glimpse" pero retorna una lista estructurada
inspect_object <- function(obj) {
  
  # Si es un objeto espacial (SF), quitamos la geometría para la "muestra"
  # para no llenar el JSON de coordenadas inlegibles.
  is_sf <- inherits(obj, "sf")
  
  info <- list(
    class = class(obj),
    rows = tryCatch(nrow(obj), error = function(e) length(obj)), # Si es vector usa length
    cols = tryCatch(ncol(obj), error = function(e) NULL),
    size_mb = format(object.size(obj), units = "Mb")
  )
  
  # Si es Dataframe o Tibble (incluyendo SF)
  if (inherits(obj, "data.frame")) {
    
    # Preparamos una muestra de datos (primeras 3 filas)
    sample_data <- head(obj, 3)
    
    # Si es SF, convertimos a tibble normal y borramos la columna geometry para el preview
    if (is_sf) {
      sample_data <- sample_data %>% st_drop_geometry() %>% as_tibble()
      info$crs <- st_crs(obj)$input # Guardamos info del sistema de coordenadas
    }
    
    # Estructura de columnas
    info$columns <- lapply(names(obj), function(col_name) {
      # Evitar error si es la columna geometria
      if (is_sf && inherits(obj[[col_name]], "sfc")) {
        return(list(name = col_name, type = "geometry", sample = "POLYGON/POINT..."))
      }
      
      list(
        name = col_name,
        type = class(obj[[col_name]])[1],
        sample = as.character(head(obj[[col_name]], 3)) # Convertir a char para asegurar compatibilidad JSON
      )
    })
  } else if (inherits(obj, "list") && !is.data.frame(obj)) {
    # Si es una lista normal (no DF)
    info$keys <- names(obj)
    info$sample <- "List object (details omitted)"
  } else {
    # Si es un vector simple
    info$sample <- head(obj, 5)
  }
  
  return(info)
}

# Lista maestra donde guardaremos todo
reporte_global <- list()

message("--- INICIANDO ANÁLISIS DE DATOS ---")

# 2. PROCESAMIENTO DE LISTAS AUXILIARES ----------------------------------------
if (file.exists("data_optimizada/listas_auxiliares.rds")) {
  message("Procesando: listas_auxiliares.rds")
  listas_aux <- readRDS("data_optimizada/listas_auxiliares.rds")
  
  # Desglosamos tal como lo haces en el global
  reporte_global$choices_rendimiento_historico <- inspect_object(listas_aux$choices_rendimiento_historico)
  reporte_global$chs_mun_cols                  <- inspect_object(listas_aux$chs_mun_cols)
  reporte_global$chs_poblaciones_manzanas      <- inspect_object(listas_aux$chs_poblaciones_manzanas)
  reporte_global$chs_poblaciones_manzanas_t    <- inspect_object(listas_aux$chs_poblaciones_manzanas_t)
  reporte_global$lista_elecciones_nombres      <- inspect_object(listas_aux$lista_elecciones_nombres)
  reporte_global$choices_elections_sombra      <- inspect_object(listas_aux$choices_elections_sombra)
  
  rm(listas_aux)
} else {
  warning("No se encontró data_optimizada/listas_auxiliares.rds")
}

# 3. PROCESAMIENTO DE ARCHIVOS INDIVIDUALES ------------------------------------

# Mapeo de archivos a nombres de variable (basado en tu global.R)
files_to_process <- list(
  "secciones_prev"               = "data_optimizada/secciones_mapa.rds",
  "shp_cols_x_secc"              = "data_optimizada/colonias_mapa.rds",
  "shp_mza2023_secc2024_cpv2020" = "data_optimizada/manzanas_censo_mapa.rds",
  "municipios_lista"             = "data_optimizada/municipios_lista.rds",
  "cant_votos_nl"                = "data_optimizada/cant_votos_nl_procesado.rds",
  "cant_votos"                   = "data_optimizada/cant_votos_simple.rds",
  "base_ganadores"               = "data_optimizada/base_ganadores.rds",
  "res_trab"                     = "data_optimizada/res_trab.rds",
  "edades"                       = "data_optimizada/edades.rds",
  "colonias"                     = "data_optimizada/colonias.rds",
  "participacion"                = "data_optimizada/participacion_full.rds",
  "data_secc_cpv2020"            = "data_optimizada/data_secc_cpv2020_procesado.rds"
)

for (var_name in names(files_to_process)) {
  path <- files_to_process[[var_name]]
  
  if (file.exists(path)) {
    message(paste("Procesando:", var_name))
    data_temp <- readRDS(path)
    reporte_global[[var_name]] <- inspect_object(data_temp)
    rm(data_temp) # Liberar memoria
    gc() # Garbage collection forzada para archivos grandes
  } else {
    warning(paste("Archivo no encontrado:", path))
    reporte_global[[var_name]] <- list(error = "File not found")
  }
}

# 4. GUARDAR JSON --------------------------------------------------------------
message("--- GUARDANDO JSON ---")
write_json(reporte_global, "estructura_datos.json", pretty = TRUE, auto_unbox = TRUE)

message("Listo! Archivo generado: estructura_datos.json")