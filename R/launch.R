#' Run the app in a web browser
#' 
#' @description 
#' This the function used to launch the shiny app.
#' 
#' @param inbrowser Launch the app inside the system's default browser or not.
#' @return No return value, called for side effects.
#' 
#' @example 
#' launchFossilSimShiny()
#' 
#' @import shiny
#' @import FossilSim
#' @export
launchFossilSimShiny <- function(inbrowser = TRUE) {
  appDir <- system.file("shinyApp", package = "FossilSimShiny")
  if (appDir == "") {
    stop("Could not find shinyApp. Try re-installing `FossilSimShiny`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = inbrowser)
}
