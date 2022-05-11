#' Run the Shiny app in a web browser
#' 
#' @description 
#' This the function used to launch the FossilSim shiny app.
#' If want a look at the inner workings of the app, take a look at the ./inst/shinyApp/ folder.
#' 
#' @param inbrowser Launch the app inside the system's default browser or not.
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
