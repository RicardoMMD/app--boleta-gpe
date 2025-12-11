# ==============================================================================
# R/branding_config.R
# Definición de identidades visuales
# ==============================================================================

obtener_configuracion_marca <- function(marca = "QUIPU") {
  
  if (marca == "QUIPU") {
    list(
      app_title = "QUIPU Electoral",
      logo_path = "bienvenida_quipu_logo.png", # Debe estar en www/
      browser_title = "QUIPU - Análisis",      # Título de la pestaña del navegador
      
      # Textos específicos
      welcome_title = "Bienvenido a QUIPU Electoral",
      welcome_subtitle = "Tu centro de comando para el análisis geo-electoral.",
      
      # Estilos CSS / Fondos
      login_background = "linear-gradient(rgba(13, 59, 102, 0.5), rgba(250, 240, 202, 0.5)),
                          url('https://aledomicom.files.wordpress.com/2024/03/bienvenida_quipu.png')  repeat center fixed;",
      
      # Colores principales (Opcional, si quieres cambiar colores de cajas)
      color_primario = "primary",
      color_secundario = "info"
    )
    
  } else if (marca == "BOLETA") {
    list(
      app_title = "BOLETA Electoral",
      logo_path = "boleta_logo.png",           # Debe estar en www/
      browser_title = "BOLETA - Dashboard",
      
      welcome_title = "Bienvenidos a BOLETA Electoral",
      welcome_subtitle = "Una herramienta de análisis GeoElectoral interactiva.",
      
      login_background = "linear-gradient(rgba(255, 246, 246, 1), rgba(250, 240, 202, 0.5)),
                          url('https://aleadomi.wordpress.com/wp-content/uploads/2025/06/boletia.png')  repeat center fixed;",
      
      color_primario = "danger", # Rojo/Guinda
      color_secundario = "warning"
    )
  }
}