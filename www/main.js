// =============================================================================
// MAIN.JS - Lógica de Pantalla Completa Modularizada
// =============================================================================

// 1. Función Helper Genérica (Evita repetir código)
// Recibe el ID dinámico (namespaced) desde R
function toggleFullscreen(id) {
  var elem = document.getElementById(id);
  
  // Verificación de seguridad por si el elemento no existe aún
  if (!elem) {
    console.error("Elemento no encontrado para fullscreen: " + id);
    return;
  }

  // Comprobar si ya estamos en fullscreen para salir, o si no para entrar
  if (!document.fullscreenElement &&    // Standard
      !document.mozFullScreenElement && // Firefox
      !document.webkitFullscreenElement && // Chrome/Safari
      !document.msFullscreenElement) {  // IE/Edge
      
    // --- ENTRAR A PANTALLA COMPLETA ---
    if (elem.requestFullscreen) {
      elem.requestFullscreen();
    } else if (elem.mozRequestFullScreen) {
      elem.mozRequestFullScreen();
    } else if (elem.webkitRequestFullscreen) {
      elem.webkitRequestFullscreen();
    } else if (elem.msRequestFullscreen) {
      elem.msRequestFullscreen();
    }
    
  } else {
    
    // --- SALIR DE PANTALLA COMPLETA ---
    if (document.exitFullscreen) {
      document.exitFullscreen();
    } else if (document.webkitExitFullscreen) {
      document.webkitExitFullscreen();
    } else if (document.mozCancelFullScreen) {
      document.mozCancelFullScreen();
    } else if (document.msExitFullscreen) {
      document.msExitFullscreen();
    }
  }
}

// 2. Definición de funciones ShinyJS
// Todas redirigen a la helper pasando el ID que reciben desde R

shinyjs.fullscreen = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenFortaleza = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenVisitas = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenGanaOPierde = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenGana = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenPierde = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenGanador1 = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenGanador2 = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenGenerosEdad = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenPri = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenSim1 = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenCambiosPerc = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenManzanas = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenSombra = function(id) { toggleFullscreen(id); };

shinyjs.fullscreenDiferencia = function(id) { toggleFullscreen(id); };