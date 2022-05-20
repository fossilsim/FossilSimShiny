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
      
      actionButton(NS(id, "treetab-button"), h3("Tree"), class = "protobutton"),
      
      tags$div(id = NS(id, "treetab-content"), class = "prototab prototabhidden",
      
      HTML("</br>"),
      
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
      
      ),
      # ---<
      
      HTML("</br>"),
      
      # II. Taxonomy Simulation Inputs ---     
      
      actionButton(NS(id, "taxtab-button"), h3("Taxonomy"), class = "protobutton"),
      
      tags$div(id = NS(id, "taxtab-content"), class = "prototab prototabhidden",
      
      HTML("</br>"),
      
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
      
      ),
      # ---<
      
      HTML("</br>"),
      
      # III. Fossil Sim Inputs ---
      
      actionButton(NS(id, "fossiltab-button"), h3("Fossils"), class = "protobutton"),
      
      tags$div(id = NS(id, "fossiltab-content"), class = "prototab prototabhidden",
      
      HTML("</br>"),
      
      # Multiple panel : every type of fossil simulation has it's panel
      #todo -- add more type of fossil simulations
      #warning -- subject to change
      tabsetPanel(id = NS(id, "tabset"), 
                  
                  # i. Uniform Distribution --
                  tabPanel(p("Uniform", id = "uniform"),
                           HTML("</br>"),
                           numericInput(inputId = NS(id, "psi"),
                                        label = "Sampling rate",
                                        value = 1,
                                        min = 0,
                                        max = 10 )
                  ), 
                  # --<
                  
                  # ii. Non-uniform Distribution --
                  tabPanel(p("Non-uniform", id = "non-uniform"),
                           HTML("</br>"),
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
                           HTML("</br>"),
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
                                        max = 10 ),
                           
                           checkboxInput(inputId = NS(id, "showsamplingproxy"), "Show Sampling Proxy", value = TRUE)
                  ),
                  # --<
                  
                  # iv. Lineage Dependent Model
                  tabPanel(p("Lineage-dependent", id = "lineage-dep"),
                           HTML("</br>"),
                           numericInput(inputId = NS(id, "rate"),
                                        label = "Rate",
                                        value = 2,
                                        min = 1,
                                        max = 5 ),
                           
                  ),
                  
                  # --<
                  
      ),
      
      actionButton(NS(id, "newfossils"), "Simulate fossils"),
      
      ),
      # ---<
      HTML("</br> </br> </br> </br> </br> </br>"),
      # IV. Appearance Toggles ---
      
      actionButton(NS(id, "appearancetab-button"), h3("Appearance"), class = "protobutton"),
      
      tags$div(id = NS(id, "appearancetab-content"), class = "prototab prototabhidden",
      
      HTML("</br>"),
      
      checkboxInput(inputId = NS(id, "showtree"), "Show tree", value = TRUE),
      checkboxInput(inputId = NS(id, "showtaxonomy"), "Show taxonomy", value = FALSE),
      checkboxInput(inputId = NS(id, "showfossils"), "Show all occurrences", value = TRUE),
      checkboxInput(inputId = NS(id, "showranges"), "Show ranges", value = FALSE),
      checkboxInput(inputId = NS(id, "showstrata"), "Show strata", value = FALSE),
      checkboxInput(inputId = NS(id, "showtips"), "Show tips", value = FALSE),
      checkboxInput(inputId = NS(id, "reconstructed"), "Show reconstructed tree", value = FALSE),
      
      ),
      # ---<
      
      # V. Save Tree ---
      h3("Save your Tree"),
      
      actionButton(NS(id, "saveas"), "Save tree as image..."),
      
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
      allCheckboxInputs = c("usertree", "showtree", "showtaxonomy", "showfossils", "showranges", "showstrata", "showtips", "reconstructed", "showsamplingproxy")
      allTextInputs = c("newick")
      
      # Simulate tree function ---
      observeEvent(input$simtree, {
        validate(need( ((v$current$mu/v$current$lambda) < 0.9), "Turnover a bit too high! Be kind to the server - try a lower extinction rate!"))
        validate(need( (v$current$tips < 101), "Please keep the number of tips under one hundred ~ thank you."))
        
        v$current$tree = TreeSim::sim.bd.taxa(input$tips, 1, input$lambda, input$mu)[[1]]
        v$current$fossils = FossilSim::fossils()
        v$current$tax = NULL # set taxonomy to NULL to avoid issues as old taxonomy will very likely not line up with a new tree

        session$sendCustomMessage("loading", FALSE)
        }
      )

      # Newick function ---
      observeEvent( c(input$usertree, input$newick), {
        v$current$tree = ape::read.tree(text = input$newick)
        
        v$current$fossils = FossilSim::fossils()
        
        session$sendCustomMessage("loading", FALSE)
        }
      )

      # Simulate taxonomy function ---
      observeEvent(input$simtax, {
        if(is.null(v$current$tree)) return() # check if there is tree #todo -- error message
        validate(need( (input$taxonomybeta >= 0 && input$taxonomybeta <= 1 && input$taxonomylambda >= 0 && input$taxonomylambda <= 1), "Rates need to be between one and zero."))
      
        v$current$tax = FossilSim::sim.taxonomy(tree = v$current$tree, input$taxonomybeta, input$taxonomylambda)
        v$current$fossils = FossilSim::fossils()
        
        session$sendCustomMessage("loading", FALSE)
        }
      )

      # Simulate fossils function ---
      observeEvent(input$newfossils, {
        if(is.null(v$current$tree)) return() # todo -- error message
        
        # Here we check which fossil sim tab is selected and simulate fossils
        #todo -- better way to check selected tab
        
        # i. Uniform Distribution --
        if(input$tabset == "<p id=\"uniform\">Uniform</p>") {
          v$current$fossilModelName = "Uniform"
          v$current$fossils = FossilSim::sim.fossils.poisson(tree = v$current$tree, rate = input$psi)
        }
        # --<
        
        # ii. Non-Uniform Distribution --
        else if (input$tabset == "<p id=\"non-uniform\">Non-uniform</p>") {
          max.age = FossilSim::tree.max(v$current$tree)
          times = c(0, sort(runif(input$int-1, min = 0, max = max.age)), max.age)
          rates = rlnorm(input$int, log(input$meanrate), sqrt(input$variance))
          v$current$fossilModelName = "Non-Uniform"
          v$current$fossils = FossilSim::sim.fossils.intervals(v$current$tree, interval.ages = times, rates = rates)
        }
        # --<

        # iii. Environment Model (Holland, 1995) --
        else if (input$tabset == "<p id=\"enviro-dep\">Environment-dependent</p>") {
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
        else if (input$tabset == "<p id=\"lineage-dep\">Lineage-dependent</p>") {
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
        
        }
      )

      # Input synchronization with data ---
      # When an input is changed run through all of the inputs and grabs the data
      #todo -- implement a more efficient method ie : only grab what was changed
      observe( {
        for (inpId in c(allNumericInputs, allCheckboxInputs, allTextInputs)) {
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
