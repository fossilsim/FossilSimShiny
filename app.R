library(shiny)

#todo -- return error if user tries to simulate fossils first
#todo -- non-uniform sometimes makes the app crash - is this still an issue? -> don't think so
#todo -- choose restrictions on parameter values more systematically, turnover, tip number, psi, meanlog, sdlog
#todo -- add additional checks for newick trees, e.g. branch lengths
#todo -- need an error for when you try to draw a reconstructed tree with only one sample
#todo -- how about a help button that says learn more about this model?
#todo -- option to plot complete and reconstructed tree alongside each other?
#todo -- what about an option to output some of the simulated data? or specify the seed

# Input ----
ui = fluidPage(
  
  # Import CSS for formatting ----
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  
  # App title for head ----
  tags$head(HTML("<title>Simulate fossils with FossilSim</title>")),
  
  # App title ----
  titlePanel(
    h1("Simulate fossils with FossilSim")
  ),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
      
      # tree simulation inputs ---
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
      
      actionButton("simtree", "Simulate tree"),
    
      
      
      # "user tree" option --- 
      checkboxInput(inputId = "usertree", "User tree", value = FALSE),
      
      textInput(inputId = "newick", "User tree",
                value = "Enter newick string..."),
     
      # taxonomy inputs ---            
      h3("Taxonomy"),
      
      numericInput(inputId = "taxonomybeta", # input name
                  label = "Probability of symmetric speciation", # user instructions
                  value = 0.1,
                  min = 0, # input specific arguments
                  max = 10 ),
      numericInput(inputId = "taxonomylambda", # input name
                  label = "Rate of anagenesis", # user instructions
                  value = 0.1,
                  min = 0, # input specific arguments
                  max = 10 ),
      
      actionButton("simtax", "Simulate taxonomy"),
      
      # fossil inputs ---
      h3("Fossils"),
      
      tabsetPanel(id = "tabset", 
                  tabPanel(p("Uniform", id = "uniform"),
                           numericInput(inputId = "psi",
                                        label = "Sampling rate",
                                        value = 1,
                                        min = 0,
                                        max = 10 )
                           ), 
                  tabPanel(p("Non-uniform", id = "non-uniform"),
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
     
      
      # Tree appearance toggles ---
      h3("Appearance"),
      
      checkboxInput(inputId = "showtree", "Show tree", value = TRUE),
      checkboxInput(inputId = "showtaxonomy", "Show taxonomy", value = FALSE),
      checkboxInput(inputId = "showfossils", "Show all occurrences", value = TRUE),
      checkboxInput(inputId = "showranges", "Show ranges", value = FALSE),
      checkboxInput(inputId = "showstrata", "Show strata", value = FALSE),
      checkboxInput(inputId = "showtips", "Show tips", value = FALSE),
      checkboxInput(inputId = "reconstructed", "Show reconstructed tree", value = FALSE),
      
     
      # Save Tree ---
      h3("Save your Tree"),
      
      actionButton("saveas", "Save tree as image..."),
      # Import Javascript for saveas functionality
      tags$script(src = "saveas.js")
      
    ),
    
    # Main panel for displaying outputs ---
    fixedPanel(id="outputDisplay",
      tabsetPanel(id = "tabset",
        
        # First panel : displays the tree
        tabPanel(
                p("Tree"),
                
                # reaction outputs
                plotOutput(outputId = "tree", width = "100%", height = "85vh"), # has matching render function
                
                ),

        # Second panel : displays the taxonomy
        tabPanel(
                p("Tax"),
                
                # reaction outputs
                plotOutput(outputId = "tax", width = "100%", height = "85vh"), # has matching render function

                )
      ),
      # Set size of output panel, todo: make it react to different screen resolution
      left = "33%",
      width = "67%"
    )
    
  ),
  
  # Tooltip : info bar at the bottom of the application ----
  fixedPanel(
    p(id = "tooltip", "i : "),
    
    left = 0,
    bottom = 0,
    width="100%",
    style = "background-color: #4AA4DE; color:white"
  ),
  
  # Import Javascript for tooltip, you can add/modify tooltip descriptions in www/tooltip.js
  tags$script(src = "tooltip.js")
)




# contains plot instructions
server <- function(input, output){
  v <- reactiveValues(tree = NULL, fossils = FossilSim::fossils()) # decide how you want to inititalise the process, you could turn the tree stuff into a function
  
  observeEvent(input$simtree,{
    v$tree = TreeSim::sim.bd.taxa(input$tips, 1, input$lambda, input$mu)[[1]]
    v$fossils = FossilSim::fossils()
  })
  
  observeEvent( c(input$usertree, input$newick),{
    v$tree = ape::read.tree(text = input$newick)
    v$fossils = FossilSim::fossils()
  })
  
  # taxonomy output ---
  observeEvent(input$simtax,{
    if(is.null(v$tree)) return() # check if there is tree
    v$tax = FossilSim::sim.taxonomy(tree = v$tree, input$taxonomybeta, input$taxonomylambda) 
    v$fossils = FossilSim::fossils()
  })
 
  observeEvent(input$newfossils, {
    if(is.null(v$tree)) return() # ideally this would return an error message
    if(input$tabset == "<p id=\"uniform\">Uniform</p>")
      v$fossils = FossilSim::sim.fossils.poisson(tree = v$tree, rate = input$psi)
    else if (input$tabset == "<p id=\"non-uniform\">Non-uniform</p>") {
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
    
    if(input$showtaxonomy) {
      validate(need(!is.null(v$tax), "No taxomony..."))
      # Synchronize fossils and taxonomy
      # todo : move this to generate fossil or taxonomy
      if(!attr(v$fossils,"from.taxonomy"))
        v$fossils = FossilSim::reconcile.fossils.taxonomy(v$fossils, v$tax)
    }
    
    
    plot(v$fossils, v$tree,
         taxonomy = v$tax,
         show.tree = input$showtree,
         show.ranges = input$showranges, 
         show.fossils = input$showfossils, 
         show.strata = input$showstrata, 
         show.taxonomy = input$showtaxonomy,
         strata = input$int,
         reconstructed = input$reconstructed, 
         show.tip.label = input$showtips, 
         align.tip.label = TRUE)
    
     if (input$showtaxonomy && FALSE) {
     if(is.null(v$tax)) return()
     if(!all(v$tree$edge %in% v$tax$edge)) return()
     plot(v$tax, v$tree, legend.position = "bottomleft")
    }
  
    } )
  
  # Temporary ??? ---
  output$tax <- renderPlot( {
    validate(need(!is.null(v$tax), "Please simulate taxomony..."))
    if(!all(v$tree$edge %in% v$tax$edge)) return()
    plot(v$tax, v$tree, legend.position = "bottomleft")
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
