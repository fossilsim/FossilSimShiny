library(shiny)

#todo -- return error if user tries to simulate fossils first
#todo -- non-uniform sometimes makes the app crash - is this still an issue?
#todo -- choose restrictions on parameter values more systematically, turnover, tip number, psi, meanlog, sdlog
#todo -- add additional checks for newick trees, e..g branch lengths
#todo -- how about a help button that says learn more about this model?

# Input ----
ui = fluidPage(
  
  # App title ----
  titlePanel("Simulate fossils"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
      
      h3("Tree"),
      
      # reaction inputs
      numericInput(inputId = "lambda", # input name
                   label = "Speciation rate", # user instructions
                   value = 0.1,
                   min = 0, # input specific arguments
                   max = 10 ),
      
      numericInput(inputId = "mu",
                   label = "Extinction rate",
                   value = 0.05,
                   min = 0,
                   max = 10 ),
      
      numericInput(inputId = "tips",
                   label = "Tip number",
                   value = 10,
                   min = 0,
                   max = 100 ),
    
      actionButton("newtree", "Simulate tree"),
      
      # "user tree" option --- 
      checkboxInput(inputId = "usertree", "User tree", value = FALSE),
      
      textInput("newick", "User tree",
                value = "Enter newick string..."),
      
      h3("Fossils"),
      
      tabsetPanel(id = "tabset", 
                  tabPanel("Uniform",
                           numericInput(inputId = "psi",
                                        label = "Sampling rate",
                                        value = 1,
                                        min = 0,
                                        max = 10 )
                           ), 
                  tabPanel("Non-uniform",
                           sliderInput(inputId = "int",
                                        label = "Number of intervals",
                                        value = 1,
                                        min = 0,
                                        max = 10 ),
                           numericInput(inputId = "meanrate",
                                        label = "Mean sampling rate",
                                        value = 1,
                                        min = 0,
                                        max = 10 ),
                           numericInput(inputId = "variance",
                                        label = "Variance",
                                        value = 1,
                                        min = 0,
                                        max = 10 )
                  )
                  ),
      
      actionButton("newfossils", "Simulate fossils"),
      
      h3("Appearance"),
      
      checkboxInput(inputId = "showtree", "Show tree", value = TRUE),
      checkboxInput(inputId = "showfossils", "Show all occurrences", value = TRUE),
      checkboxInput(inputId = "showranges", "Show ranges", value = FALSE),
      checkboxInput(inputId = "showstrata", "Show strata", value = FALSE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # reaction outputs
      plotOutput(outputId = "tree") # has matching render function
    )
    
  )
)




# contains plot instructions
server <- function(input, output){
  v <- reactiveValues(tree = NULL, fossils = FossilSim::fossils()) # decide how you want to inititalise the process, you could turn the tree stuff into a function
  
  observeEvent(input$newtree,{
    if(input$usertree){
      v$tree = ape::read.tree(text = input$newick)
    } else {
      v$tree = TreeSim::sim.bd.taxa(input$tips, 1, input$lambda, input$mu)[[1]]
    }
    v$fossils = FossilSim::fossils()
  })
  
 
  observeEvent(input$newfossils, {
    if(is.null(v$tree)) return() # ideally this would return an error message
    if(input$tabset == "Uniform")
      v$fossils = FossilSim::sim.fossils.poisson(tree = v$tree, rate = input$psi)
    else if (input$tabset == "Non-uniform") {
      max.age = FossilSim::tree.max(v$tree)
      times = c(0, sort(runif(input$int-1, min = 0, max = max.age)), max.age)
      rates = rlnorm(input$int, log(input$meanrate), sqrt(input$variance))
      v$fossils = FossilSim::sim.fossils.intervals(v$tree, interval.ages = times, rates = rates)
    }
  })
  
  output$tree <- renderPlot( {
    
    if(input$usertree) 
      validate(need( !is.null(ape::read.tree(text = input$newick)) , "Specify newick string"))
    else validate(need( ((input$mu/input$lambda) < 0.9), "Turnover a bit too high! Be kind to the server - try a lower extinction rate!"))
    
    if(is.null(v$tree)) return()
    
    plot(v$fossils, v$tree, show.tree = input$showtree, show.ranges = input$showranges, 
         show.fossils = input$showfossils, show.strata = input$showstrata, strata = input$int)
  } )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
