#' Run the Shiny app in a web browser
#' 
#' @import shiny
#' @import FossilSim
#' @export
launchFossilSimShiny <- function() {
  appDir <- system.file("shinyApp", package = "FossilSimShiny")
  if (appDir == "") {
    stop("Could not find shinyApp. Try re-installing `FossilSimShiny`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
