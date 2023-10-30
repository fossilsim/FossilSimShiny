# Input Sidebar : Sidebar on the left side of the app where you make all your inputs ----

# Sidebar UI --- ---
inputSidebarUI <- function(id) {
  
  # Wrapping our ids ---
  # The NS function allows us to wrap the ids of our ui components
  # This is necessary for nested R modules like this one
  # ns stands for namespace, more info : https://shiny.rstudio.com/articles/modules.html
  ns <- NS(id)
  
  # Start of our UI ---
  # tagList is the nested version of fluidPage
  tagList(
    
    # Loading ---
    fixedPanel(id = "loader-wrapper",
               tags$div(id = "loader", "")
    ),
    
    sidebarPanel(
      
      HTML("</br>"),
      
      # I. Tree Simulation Inputs ---
      
      actionButton(ns("treetab-button"), h3("Tree"), class = "protobutton"),
      
      tags$div(id = ns("treetab-content"), class = "prototab prototabhidden",
               
               HTML("</br>"),
               
               # "Simulate tree" option --- 
               numericInput(inputId = ns("lambda"), # input name
                            label = "Speciation rate",  # user instructions
                            value = 0.1,
                            min = 0,                    # input specific arguments
                            max = 10 ),
               
               numericInput(inputId = ns("mu"),
                            label = "Extinction rate",
                            value = 0.05,
                            min = 0,
                            max = 10 ),
               
               numericInput(inputId = ns("tips"),
                            label = "Tip number",
                            value = 10,
                            min = 0,
                            max = 100 ),
               
               actionButton(ns("simtree"), "Simulate tree"),
               
               HTML("</br> </br>"),
               
               # "user tree" option --- 
               textInput(inputId = ns("newick"), "User tree",
                         value = "Enter newick string..."),
               
               actionButton(ns("usertree"), "Import tree"),
               
      ),
      # ---<
      
      HTML("</br>"),
      
      # II. Taxonomy Simulation Inputs ---     
      
      actionButton(ns("taxtab-button"), h3("Taxonomy"), class = "protobutton"),
      
      tags$div(id = ns("taxtab-content"), class = "prototab prototabhidden",
               
               HTML("</br>"),
               
               numericInput(inputId = ns("taxonomybeta"), 
                            label = "Probability of symmetric speciation", 
                            value = 0.1,
                            min = 0,
                            max = 10 ),
               
               numericInput(inputId = ns("taxonomylambda"),
                            label = "Rate of anagenesis",
                            value = 0.1,
                            min = 0,
                            max = 10 ),
               
               actionButton(ns("simtax"), "Simulate taxonomy"),
               
      ),
      # ---<
      
      HTML("</br>"),
      
      # III. Fossil Sim Inputs ---
      
      actionButton(ns("fossiltab-button"), h3("Fossils"), class = "protobutton"),
      
      tags$div(id = ns("fossiltab-content"), class = "prototab prototabhidden",
               
               HTML("</br>"),
               
               # Multiple panel : every type of fossil simulation has its panel
               #todo -- add more type of fossil simulations
               #warning -- subject to change
               tabsetPanel(id = ns("tabset"), 
                           
                           # i. Uniform Distribution --
                           tabPanel(p("Uniform", id = ns("uniform")),
                                    HTML("</br>"),
                                    numericInput(inputId = ns("uniform-psi"),
                                                 label = "Sampling rate",
                                                 value = 1,
                                                 min = 0,
                                                 max = 10 )
                           ), 
                           # --<
                           
                           # ii. Non-uniform Distribution --
                           tabPanel(p("Time-dependent", id = ns("non-uniform")),
                                    HTML("</br>"),
                                    sliderInput(inputId = ns("non-uniform-int"),
                                                label = "Number of intervals",
                                                value = 1,
                                                min = 0,
                                                max = 10 ),
                                    
                                    numericInput(inputId = ns("non-uniform-meanrate"),
                                                 label = "Mean sampling rate",
                                                 value = 1,
                                                 min = 0,
                                                 max = 10 ),
                                    
                                    numericInput(inputId = ns("non-uniform-variance"),
                                                 label = "Variance",
                                                 value = 1,
                                                 min = 0,
                                                 max = 10 )
                           ),
                           # --<
                           
                           # iii. Environment Model (Holland, 1995) --
                           tabPanel(p("Environment-dependent", id = ns("enviro-dep")),
                                    HTML("</br>"),
                                    numericInput(inputId = ns("enviro-dep-strata"),
                                                 label = "Strata",
                                                 value = 7,
                                                 min = 1,
                                                 max = 10 ),
                                    
                                    numericInput(inputId = ns("enviro-dep-pd"),
                                                 label = "Preferred depth (PD)",
                                                 value = 0.5,
                                                 min = 0,
                                                 max = 10 ),
                                    
                                    numericInput(inputId = ns("enviro-dep-dt"),
                                                 label = "Depth tolerance (DT)",
                                                 value = 1,
                                                 min = 0,
                                                 max = 10 ),
                                    
                                    numericInput(inputId = ns("enviro-dep-pa"),
                                                 label = "Peak abundance (PA)",
                                                 value = 1,
                                                 min = 0,
                                                 max = 10 ),
                           ),
                           # --<
                           
                           # iv. Lineage Dependent Model
                           tabPanel(p("Lineage-dependent", id = ns("lineage-dep")),
                                    HTML("</br>"),
                                    numericInput(inputId = ns("lineage-dep-LNrate"),
                                                 label = "Mean rate",
                                                 value = 2,
                                                 min = 1,
                                                 max = 5 ),
                                    numericInput(inputId = ns("lineage-dep-LNsd"),
                                                 label = "Standard deviation",
                                                 value = 1,
                                                 min = 0,
                                                 max = 5 ),
                           ),
                           
                           # --<
                           
               ),
               
               actionButton(ns("simfossils"), "Simulate fossils"),
               
      ),
      # ---<
      HTML("</br> </br> </br> </br> </br> </br>"),
      # IV. Appearance Toggles ---
      
      actionButton(ns("appearancetab-button"), h3("Appearance"), class = "protobutton"),
      
      tags$div(id = ns("appearancetab-content"), class = "prototab prototabhidden",
               
               HTML("</br>"),
               
               checkboxInput(inputId = ns("showtree"), "Show tree", value = TRUE),
               checkboxInput(inputId = ns("showtaxonomy"), "Show taxonomy", value = FALSE),
               checkboxInput(inputId = ns("showfossils"), "Show all occurrences", value = TRUE),
               checkboxInput(inputId = ns("showranges"), "Show ranges", value = FALSE),
               checkboxInput(inputId = ns("showstrata"), "Show strata", value = FALSE),
               checkboxInput(inputId = ns("showtips"), "Show tip labels", value = FALSE),
               checkboxInput(inputId = ns("reconstructed"), "Show reconstructed tree", value = FALSE),
               checkboxInput(inputId = ns("enviro-dep-showsamplingproxy"), "Show depth used in sampling", value = TRUE),
               
      ),
      # ---<
      
      # V. Save Tree ---
      h3("Save your Tree"),
      
      actionButton(ns("saveas"), "Save tree as image..."),
      
      # Import Javascript for saveas functionality
      tags$script(src = "saveas.js"),
      
      tags$script(src = "protodashboard.js"),
      # ---<
      
      # Import Js for loading animation
      tags$script(src = "loading.js")
      
    )
  )
}


# Server code --- ---
inputSidebarServer <- function(id, v) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      # List of all of our different inputs, incredibly useful to perform mass actions ---
      # Does not need to be reactive as we avoid creating new inputs during run time
      allNumericInputs = c("lambda", "mu", "tips", "taxonomylambda", "taxonomybeta", "psi", "meanrate", "variance", "strata", "pd", "dt", "pa", "rate")
      allCheckboxInputs = c("showtree", "showtaxonomy", "showfossils", "showranges", "showstrata", "showtips", "reconstructed", "showsamplingproxy")
      allTextInputs = c("newick")
      
      listOfTabs = list(unif = "<p id=\"inputSidebar-uniform\">Uniform</p>", timedep = "<p id=\"inputSidebar-non-uniform\">Time-dependent</p>",
                        envdep = "<p id=\"inputSidebar-enviro-dep\">Environment-dependent</p>", lindep = "<p id=\"inputSidebar-lineage-dep\">Lineage-dependent</p>")
      
      # Simulate tree function ---
      observeEvent(input$simtree, {
        validate(need( ((v$current$mu/v$current$lambda) < 0.9), "Turnover a bit too high! Be kind to the server - try a lower extinction rate!"))
        validate(need( (v$current$tips < 101), "Please keep the number of tips under one hundred ~ thank you."))
        
        v$current$tree = TreeSim::sim.bd.taxa(input$tips, 1, input$lambda, input$mu)[[1]]
        v$current$fossils = FossilSim::fossils()
        v$current$tax = NULL # set taxonomy to NULL to avoid issues as old taxonomy will very likely not line up with a new tree
        
        session$sendCustomMessage("loading", FALSE)
      })
      
      # Newick function ---
      observeEvent(input$usertree, {
        validate(need(!is.null(input$newick), "No tree found, please input the tree as Newick"))
        v$current$tree = ape::read.tree(text = input$newick)
        
        v$current$fossils = FossilSim::fossils()
        
        session$sendCustomMessage("loading", FALSE)
      })
      
      # Simulate taxonomy function ---
      observeEvent(input$simtax, {
        if(is.null(v$current$tree)) return() # check if there is tree #todo -- error message
        validate(need( (input$taxonomybeta >= 0 && input$taxonomybeta <= 1 && input$taxonomylambda >= 0 && input$taxonomylambda <= 1), "Rates need to be between one and zero."))
        
        v$current$tax = FossilSim::sim.taxonomy(tree = v$current$tree, input$taxonomybeta, input$taxonomylambda)
        v$current$fossils = FossilSim::fossils()
        
        session$sendCustomMessage("loading", FALSE)
      })
      
      # Simulate fossils function ---
      observeEvent(input$simfossils, {
        if(is.null(v$current$tree)) return() # todo -- error message
        
        # Here we check which fossil sim tab is selected and simulate fossils
        #todo -- better way to check selected tab
        
        # i. Uniform Distribution --
        if(input$tabset == listOfTabs$unif) {
          v$current$fossilModelName = "Uniform"
          v$current$fossils = FossilSim::sim.fossils.poisson(tree = v$current$tree, rate = input$psi)
        }
        # --<
        
        # ii. Non-Uniform Distribution --
        else if (input$tabset == listOfTabs$timedep) {
          max.age = FossilSim::tree.max(v$current$tree)
          times = c(0, sort(runif(input$int-1, min = 0, max = max.age)), max.age)
          rates = rlnorm(input$int, log(input$meanrate), sqrt(input$variance))
          v$current$fossilModelName = "Non-Uniform"
          v$current$fossils = FossilSim::sim.fossils.intervals(v$current$tree, interval.ages = times, rates = rates)
        }
        # --<
        
        # iii. Environment Model (Holland, 1995) --
        else if (input$tabset == listOfTabs$envdep) {
          wd = FossilSim::sim.gradient(input$strata)
          v$current$fossilModelName = "Holland"
          v$current$fossils = FossilSim::sim.fossils.environment(tree = v$current$tree,
                                                                 max.age = FossilSim::tree.max(v$current$tree),
                                                                 strata = input$strata,
                                                                 proxy.data = wd,
                                                                 PD = input$pd,
                                                                 DT = input$dt,PA = input$pa)
        }
        # --<
        
        # iv. Lineage Model --
        else if (input$tabset == listOfTabs$lindep) {
          dist = function() { rlnorm(1, log(v$current$rate), 1) }
          
          # Check if taxonomy has been generated and if it corresponds to the current tree
          if (is.null(v$current$tax) || !(v$current$tree$edge %in% v$current$tax$edge)) {
            # If not then stop loading animation and do nothing
            session$sendCustomMessage("loading", FALSE)
            return()
          }
          
          rates = FossilSim::sim.trait.values(v$current$rate, taxonomy = v$current$tax, model = "independent", dist = dist)
          v$current$fossilModelName = "Lineage"
          v$current$fossils = FossilSim::sim.fossils.poisson(rates, taxonomy = v$current$tax)
        }
        # --<
        
        session$sendCustomMessage("loading", FALSE)
        
      })
      
      # Input synchronization with data ---
      # When an input is changed run through all of the inputs and grabs the data
      #todo -- implement a more efficient method ie : only grab what was changed
      observe({
        for (inpId in c(allNumericInputs, allCheckboxInputs, allTextInputs)) {
          v$current[[inpId]] = input[[inpId]]
        }
        # TODO conditional appearance buttons
        #if (input$tabset )
      })
      
      # On tab change, update all inputs to stored data for that tab ---
      observeEvent(v$currentTab, {
        for (ni in allNumericInputs) {
          updateNumericInput(session, ni, value=v$current[[ni]])
        }
        for (ci in allCheckboxInputs) {
          updateCheckboxInput(session, ci, value=v$current[[ci]]) 
        }
      })
    }
  )
}
