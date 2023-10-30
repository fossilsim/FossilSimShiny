library(shiny)

#todo -- return error if user tries to simulate fossils first
#todo -- non-uniform sometimes makes the app crash - is this still an issue? -> don't think so
#todo -- choose restrictions on parameter values more systematically, turnover, tip number, psi, meanlog, sdlog
#todo -- add additional checks for newick trees, e.g. branch lengths
#todo -- need an error for when you try to draw a reconstructed tree with only one sample
#todo -- how about a help button that says learn more about this model?
#todo -- option to plot complete and reconstructed tree alongside each other?
#todo -- what about an option to output some of the simulated data? or specify the seed

# Main UI --- ---
ui = fluidPage(
  shinyjs::useShinyjs(),
  
  # Import CSS for formatting ---
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  
  # App title for head ---
  tags$head(HTML("<title>Simulate fossils with FossilSim</title>")),
  
  # Small white space, purely aesthetic ---
  HTML("</br> "),
  
  # Sidebar layout with input and output definitions ---
  sidebarLayout(
    
    # Input panel for inputs ---
    # ./R/inputSidebar.R
    inputSidebarUI("inputSidebar"),
    
    # Main panel for displaying outputs ---
    # ./R/outputSidebar.R
    outputSidebar("outputSidebar"),
    
  ),
  
  # App title ---
  fixedPanel(
    tags$div(class = "app-title", HTML(
      "<h1 id = \"app-title\">F<img id = \"logo\" src = \"logo.png\" style=\"width : 20px;\">ssilSimShiny</h1>"
      )), left = 0, top = 0, width="100%",
  ),
  
  # Small white space, purely aesthetic ---
  HTML("</br> </br> </br> </br> </br> </br>"),
  
  # Tooltip : info bar at the bottom of the application ---
  # ./R/tooltip.R
  tooltip(id = "tooltip"),
  
  # Themebar : buttons to change theme (background color, text color, etc...)
  # ./R/themebar.R
  themebar(id = "themebar")
  
)


# Main Server Function --- ---
server <- function(input, output) {
  shinyjs::showLog()
  
  # Here we create the different keys for our tabs ---
  # Each tab has a key specific to it, this is what allows us to manage our data between tabs
  # If you would like to modify the tab names, you also need to need to modify the initial tab in ./R/outputSidebar.R and currTab in www/saveas.js
  # The "current" key corresponds to the current active tab; do not modify
  # This does not need to be an active variable as keys shouldn't be changed during run time
  keys = c("current", "i", "ii", "iii", "iv", "v")
  
  # Creation of our tab manager ---
  # ./R/toad.R
  tabManager <- toadCreate(keys)

  # InputSidebarServer : Handles all of the inputs and generates data ---
  # ./R/inputSidebar.R
  inputSidebar = inputSidebarServer("inputSidebar", tabManager)
  
  # OutputSidebarServer : Mostly handles output, like generating plots, and some tab managing ---
  # ./R/outputSidebar.R
  outputSidebar = outputSidebarServer("outputSidebar", tabManager, keys)
}

# Run the app --- outdated
shinyApp(ui = ui, server = server)
