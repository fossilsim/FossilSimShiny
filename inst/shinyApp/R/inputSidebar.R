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
    
    sidebarPanel(
      
      # I. Tree Simulation Inputs ---
      h3("Tree"),
      
      # reactive inputs
      numericInput(inputId = NS(id, "lambda"), # input name
                   label = "Speciation rate",  # user instructions
                   value = 0.1,
                   min = 0,                    # input specific arguments
                   max = 10 ),
      
      numericInput(inputId = NS(id, "mu"),
                   label = "Extinction rate",
                   value = 0.05,
                   min = 0,
                   max = 10 ),
      
      numericInput(inputId = NS(id, "tips"),
                   label = "Tip number",
                   value = 10,
                   min = 0,
                   max = 100 ),
      
      actionButton(NS(id, "simtree"), "Simulate tree"),
      
      
      # "user tree" option --- 
      checkboxInput(inputId = NS(id, "usertree"), "User tree", value = FALSE),
      
      textInput(inputId = NS(id, "newick"), "User tree",
                value = "Enter newick string..."),
      
      # ---<
      
      # II. Taxonomy Simulation Inputs ---            
      h3("Taxonomy"),
      
      numericInput(inputId = NS(id, "taxonomybeta"), 
                   label = "Probability of symmetric speciation", 
                   value = 0.1,
                   min = 0,
                   max = 10 ),
      
      numericInput(inputId = NS(id, "taxonomylambda"),
                   label = "Rate of anagenesis",
                   value = 0.1,
                   min = 0,
                   max = 10 ),
      
      actionButton(NS(id, "simtax"), "Simulate taxonomy"),
      
      # ---<
      
      # III. Fossil Sim Inputs ---
      h3("Fossils"),
      
      # Multiple panel : every type of fossil simulation has it's panel
      #todo -- add more type of fossil simulations
      #warning -- subject to change
      tabsetPanel(id = NS(id, "tabset"), 
                  
                  # i. Uniform Distribution --
                  tabPanel(p("Uniform", id = "uniform"),
                           numericInput(inputId = NS(id, "psi"),
                                        label = "Sampling rate",
                                        value = 1,
                                        min = 0,
                                        max = 10 )
                  ), 
                  # --<
                  
                  # ii. Non-uniform Distribution --
                  tabPanel(p("Non-uniform", id = "non-uniform"),
                           sliderInput(inputId = NS(id, "int"),
                                       label = "Number of intervals",
                                       value = 1,
                                       min = 0,
                                       max = 10 ),
                           
                           numericInput(inputId = NS(id, "meanrate"),
                                        label = "Mean sampling rate",
                                        value = 1,
                                        min = 0,
                                        max = 10 ),
                           
                           numericInput(inputId = NS(id, "variance"),
                                        label = "Variance",
                                        value = 1,
                                        min = 0,
                                        max = 10 )
                  ),
                  # --<
                  
                  # iii. Environment Model (Holland, 1995) --
                  tabPanel(p("Environment-dependent", id = "enviro-dep"),
                           numericInput(inputId = NS(id, "strata"),
                                        label = "Strata",
                                        value = 7,
                                        min = 1,
                                        max = 10 ),
                           
                           numericInput(inputId = NS(id, "pd"),
                                        label = "PD",
                                        value = 0.5,
                                        min = 0,
                                        max = 10 ),
                           
                           numericInput(inputId = NS(id, "dt"),
                                        label = "DT",
                                        value = 1,
                                        min = 0,
                                        max = 10 ),
                           
                           numericInput(inputId = NS(id, "pa"),
                                        label = "PA",
                                        value = 1,
                                        min = 0,
                                        max = 10 )
                  )
                  # --<
                  
      ),
      
      actionButton(NS(id, "newfossils"), "Simulate fossils"),
      
      # ---<
      
      # IV. Appearance Toggles ---
      h3("Appearance"),
      
      checkboxInput(inputId = NS(id, "showtree"), "Show tree", value = TRUE),
      checkboxInput(inputId = NS(id, "showtaxonomy"), "Show taxonomy", value = FALSE),
      checkboxInput(inputId = NS(id, "showfossils"), "Show all occurrences", value = TRUE),
      checkboxInput(inputId = NS(id, "showranges"), "Show ranges", value = FALSE),
      checkboxInput(inputId = NS(id, "showstrata"), "Show strata", value = FALSE),
      checkboxInput(inputId = NS(id, "showtips"), "Show tips", value = FALSE),
      checkboxInput(inputId = NS(id, "reconstructed"), "Show reconstructed tree", value = FALSE),
      
      # ---<
      
      # V. Save Tree ---
      h3("Save your Tree"),
      
      actionButton(NS(id, "saveas"), "Save tree as image..."),
      
      # Import Javascript for saveas functionality
      tags$script(src = "saveas.js")
      
      # ---<
      
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
      allNumericInputs = c("lambda", "mu", "tips", "taxonomylambda", "taxonomybeta", "psi", "meanrate", "variance", "strata", "pd", "dt", "pa")
      allCheckboxInputs = c("usertree", "showtree", "showtaxonomy", "showfossils", "showranges", "showstrata", "showtips", "reconstructed")
      
      # Simulate tree function ---
      observeEvent(input$simtree, {
        v$current$tree = TreeSim::sim.bd.taxa(input$tips, 1, input$lambda, input$mu)[[1]]
        v$current$fossils = FossilSim::fossils()
        v$current$tax = NULL # set taxonomy to NULL to avoid issues as old taxonomy will very likely not line up with a new tree
        }
      )

      # Newick function ---
      observeEvent( c(input$usertree, input$newick), {
        v$current$tree = ape::read.tree(text = input$newick)
        v$current$fossils = FossilSim::fossils()
        }
      )

      # Simulate taxonomy function ---
      observeEvent(input$simtax, {
        if(is.null(v$current$tree)) return() # check if there is tree #todo -- error message
        v$current$tax = FossilSim::sim.taxonomy(tree = v$current$tree, input$taxonomybeta, input$taxonomylambda)
        v$current$fossils = FossilSim::fossils()
        }
      )

      # Simulate fossils function ---
      observeEvent(input$newfossils, {
        if(is.null(v$current$tree)) return() # todo -- error message
        
        # Here we check which fossil sim tab is selected and simulate fossils
        #todo -- better way to check selected tab
        
        # i. Uniform Distribution --
        if(input$tabset == "<p id=\"uniform\">Uniform</p>")
          v$current$fossils = FossilSim::sim.fossils.poisson(tree = v$current$tree, rate = input$psi)
        # --<
        
        # ii. Non-Uniform Distribution --
        else if (input$tabset == "<p id=\"non-uniform\">Non-uniform</p>") {
          max.age = FossilSim::tree.max(v$current$tree)
          times = c(0, sort(runif(input$int-1, min = 0, max = max.age)), max.age)
          rates = rlnorm(input$int, log(input$meanrate), sqrt(input$variance))
          v$current$fossils = FossilSim::sim.fossils.intervals(v$current$tree, interval.ages = times, rates = rates)
        }
        # --<

        # iii. Environment Model (Holland, 1995) --
        else if (input$tabset == "<p id=\"enviro-dep\">Environment-dependent</p>") {
          wd = FossilSim::sim.gradient(input$strata)
          v$current$fossils = FossilSim::sim.fossils.environment(tree = v$current$tree,
                                                                 max.age = FossilSim::tree.max(v$current$tree),
                                                                 strata = input$strata,
                                                                 proxy.data = wd,
                                                                 PD = input$pd,
                                                                 DT = input$dt,PA = input$pa)
        }
        # --<
        }
      )


      # Input synchronization with data ---
      # When an input is changed run through all of the inputs and grabs the data
      #todo -- implement a more efficient method ie : only grab what was changed
      observe( {
        for (inpId in c(allNumericInputs, allCheckboxInputs)) {
            v$current[[inpId]] = input[[inpId]]
        }
        }
      )
      
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