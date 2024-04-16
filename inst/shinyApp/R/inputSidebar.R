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
               actionButton(ns("cleartax"), "Clear taxonomy"),
               
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
                                    selectInput(ns("nonUniformType"), label = "Interval type", choices = c("random", "even", "user-defined")),
                                    
                                    conditionalPanel("input.nonUniformType == 'random' || input.nonUniformType == 'even'",
                                                     sliderInput(inputId = ns("non-uniform-int"), label = "Number of intervals", value = 1, min = 0, max = 10 ), 
                                                     ns = ns),
                                    conditionalPanel("input.nonUniformType == 'user-defined'",
                                                     textInput(inputId = ns("non-uniform-bounds"), label = "Interval bounds (delimited by /)"), 
                                                     ns = ns),
                                    
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
                           tabPanel(p("Depth-dependent", id = ns("enviro-dep")),
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
                                                 value = 0.5,
                                                 min = -10,
                                                 max = 3 ),
                                    numericInput(inputId = ns("lineage-dep-LNsd"),
                                                 label = "Standard deviation",
                                                 value = 0.7,
                                                 min = 0,
                                                 max = 5 ),
                           ),
                           
                           # --<
                           
               ),
               
               actionButton(ns("simfossils"), "Simulate fossils"),
               actionButton(ns("clearfossils"), "Clear fossils"),
      ),
      # ---<
      HTML("</br> </br> </br> </br> </br> </br>"),
      # IV. Appearance Toggles ---
      
      actionButton(ns("appearancetab-button"), h3("Appearance"), class = "protobutton"),
      
      tags$div(id = ns("appearancetab-content"), class = "prototab prototabhidden",
               
               HTML("</br>"),
               
               checkboxInput(inputId = ns("showtree"), "Show tree", value = TRUE),
               checkboxInput(inputId = ns("showtaxonomy"), "Show fossil taxonomy - (requires simulated taxonomy and fossils)", value = FALSE),
               checkboxInput(inputId = ns("showfossils"), "Show all occurrences - (requires simulated fossils)", value = TRUE),
               checkboxInput(inputId = ns("showranges"), "Show ranges - (requires simulated fossils)", value = FALSE),
               checkboxInput(inputId = ns("showstrata"), "Show strata used in simulating fossils", value = FALSE),
               checkboxInput(inputId = ns("showtips"), "Show tip labels", value = FALSE),
               checkboxInput(inputId = ns("reconstructed"), "Show reconstructed tree", value = FALSE),
               checkboxInput(inputId = ns("enviro-dep-showsamplingproxy"), "Show depth (requires fossils under a depth-dependent model)", value = FALSE),
               
      ),
      # ---<
      
      # V. Save Tree ---
      h3("Save your simulation"),
      
      downloadButton(ns("dldata"), "Download the data"),
      
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
      allNumericInputs = c("lambda", "mu", "tips", "taxonomylambda", "taxonomybeta", "uniform-psi", "non-uniform-int", "non-uniform-meanrate", "non-uniform-variance", "enviro-dep-strata", 
                           "enviro-dep-pd", "enviro-dep-dt", "enviro-dep-pa", "lineage-dep-LNrate", "lineage-dep-LNsd")
      allCheckboxInputs = c("showtree", "showtaxonomy", "showfossils", "showranges", "showstrata", "showtips", "reconstructed", "enviro-dep-showsamplingproxy")
      allTextInputs = c("newick")
      
      listOfTabs = list(unif = "<p id=\"inputSidebar-uniform\">Uniform</p>", timedep = "<p id=\"inputSidebar-non-uniform\">Time-dependent</p>",
                        envdep = "<p id=\"inputSidebar-enviro-dep\">Depth-dependent</p>", lindep = "<p id=\"inputSidebar-lineage-dep\">Lineage-dependent</p>")
      
      # Simulate tree function ---
      observeEvent(input$simtree, {
        v$current$error = FALSE
        
        if (v$current$mu/v$current$lambda >= 0.9) {
          v$current$error = TRUE
          v$current$errorMsg = "To avoid overloading the server, please keep the turnover under 0.9."
        }
        if(v$current$tips > 100) {
          v$current$error = TRUE
          v$current$errorMsg = "To avoid overloading the server, please keep the number of tips under 100."
        }
        validate(need(!v$current$error, v$current$errorMsg))
        
        initial.time = Sys.time()
        repeat {
          v$current$tree = try(TreeSim::sim.bd.taxa(input$tips, 1, input$lambda, input$mu)[[1]])
          if(class(v$current$tree) != "try-error") break
        }
        v$current$status = list(timing = Sys.time() - initial.time, msg = "")
        
        v$current$fossils = FossilSim::fossils()
        v$current$tax = NULL # set taxonomy to NULL to avoid issues as old taxonomy will very likely not line up with a new tree
        
        session$sendCustomMessage("loading", FALSE)
      })
      
      # Newick function ---
      observeEvent(input$usertree, {
        v$current$error = FALSE
        if (input$newick == "" || input$newick == "Enter newick string...") {
          v$current$error = TRUE
          v$current$errorMsg = "No tree found, please input the tree as Newick."
        }
        validate(need(!v$current$error, v$current$errorMsg))
        
        v$current$tree = try(ape::read.tree(text = input$newick))
        if(is.null(v$current$tree)) v$current$tree = try(ape::read.tree(text = paste0(input$newick, ";"))) #ape requires the final ';' for some reason
        
        if(class(v$current$tree) == "try-error") v$current$tree = NULL
        if(is.null(v$current$tree)) {
          v$current$error = TRUE
          v$current$errorMsg = "Could not read tree, please check the Newick string."
        }
        v$current$status = list(timing = -1, msg = "")
        
        v$current$fossils = FossilSim::fossils()
        v$current$tax = NULL # set taxonomy to NULL to avoid issues as old taxonomy will very likely not line up with a new tree
        validate(need(!v$current$error, v$current$errorMsg))
        
        session$sendCustomMessage("loading", FALSE)
      })
      
      # Simulate taxonomy function ---
      observeEvent(input$simtax, {
        v$current$error = FALSE
        if (is.null(v$current$tree)) {
          v$current$error = TRUE
          v$current$errorMsg = "No tree found, please simulate or input a tree first."
        }
        validate(need(!v$current$error, v$current$errorMsg))
        
        initial.time = Sys.time()
        v$current$tax = FossilSim::sim.taxonomy(tree = v$current$tree, input$taxonomybeta, input$taxonomylambda)
        if(!attr(v$current$fossils, "from.taxonomy")) v$current$fossils = FossilSim::reconcile.fossils.taxonomy(v$current$fossils, v$current$tax)
        else v$current$fossils = FossilSim::fossils()
        
        tmp = v$current$tax[, c("sp", "mode")]
        event_counts = table(tmp[!duplicated(tmp),]$mode)
        v$current$status = list(timing = Sys.time() - initial.time, 
                                msg = paste("Simulated:", event_counts["a"], "anagenetic events,", event_counts["b"], "budding events and", event_counts["s"]/2, "bifurcating events"))
        
        session$sendCustomMessage("loading", FALSE)
      })
      
      # Clear taxonomy
      observeEvent(input$cleartax, {
        v$current$tax = NULL
      })
      
      # Check that fossilization rates are not too high
      validateRates = function(rates) {
        if(any(rates > 10)) {
          v$current$error = TRUE
          v$current$errorMsg = "Current parameters lead to high fossil sampling rates. Please adjust or try again."
        }
        validate(need(!v$current$error, v$current$errorMsg))
      }
      
      # Simulate fossils function ---
      observeEvent(input$simfossils, {
        v$current$error = FALSE
        if (is.null(v$current$tree)) {
          v$current$error = TRUE
          v$current$errorMsg = "No tree found, please simulate or input a tree first."
        }
        validate(need(!v$current$error, v$current$errorMsg))
        
        # Here we check which fossil sim tab is selected and simulate fossils
        initial.time = Sys.time()
        
        # i. Uniform Distribution --
        if(input$tabset == listOfTabs$unif) {
          v$current$fossilModelName = "Uniform"
          v$current$fossils = FossilSim::sim.fossils.poisson(tree = v$current$tree, rate = input$`uniform-psi`)
        }
        # --<
        
        # ii. Non-Uniform Distribution --
        else if (input$tabset == listOfTabs$timedep) {
          max.age = FossilSim::tree.max(v$current$tree)
          
          if(input$nonUniformType == "random") {
            v$current$int.ages = c(0, sort(runif(input$`non-uniform-int` - 1, min = 0, max = max.age)), max.age)
          } else if(input$nonUniformType == "even") {
            v$current$int.ages = seq(0, max.age, length.out = input$`non-uniform-int` + 1)
          } else if(input$nonUniformType == "user-defined") {
            v$current$int.ages = sort(as.numeric(strsplit(input$`non-uniform-bounds`, "/")[[1]]))
            if(v$current$int.ages[1] > 0) v$current$int.ages = c(0, v$current$int.ages)
            if(v$current$int.ages[length(v$current$int.ages)] < max.age) v$current$int.ages = c(v$current$int.ages, max.age)
          }
            
          rates = rlnorm(length(v$current$int.ages) - 1, log(input$`non-uniform-meanrate`), sqrt(input$`non-uniform-variance`))
          validateRates(rates)
          v$current$fossilModelName = "Non-Uniform"
          v$current$fossils = FossilSim::sim.fossils.intervals(v$current$tree, interval.ages = v$current$int.ages, rates = rates)
        }
        # --<
        
        # iii. Environment Model (Holland, 1995) --
        else if (input$tabset == listOfTabs$envdep) {
          v$current$strata = input$`enviro-dep-strata`
          v$current$wd = FossilSim::sim.gradient(input$`enviro-dep-strata`)
          v$current$fossilModelName = "Holland"
          v$current$fossils = FossilSim::sim.fossils.environment(tree = v$current$tree,
                                                                 max.age = FossilSim::tree.max(v$current$tree),
                                                                 strata = input$`enviro-dep-strata`,
                                                                 proxy.data = v$current$wd,
                                                                 PD = input$`enviro-dep-pd`,
                                                                 DT = input$`enviro-dep-dt`, PA = input$`enviro-dep-pa`)
        }
        # --<
        
        # iv. Lineage Model --
        else if (input$tabset == listOfTabs$lindep) {
          dist = function() { rlnorm(1, log(input$`lineage-dep-LNrate`), input$`lineage-dep-LNsd`) }
          rates = FossilSim::sim.trait.values(dist(), tree = v$current$tree, taxonomy = v$current$tax, model = "independent", dist = dist)
          validateRates(rates)
          v$current$fossilModelName = "Lineage"
          v$current$fossils = FossilSim::sim.fossils.poisson(rates, tree = v$current$tree, taxonomy = v$current$tax)
        }
        # --<
        
        v$current$status = list(timing = Sys.time() - initial.time, 
                                msg = paste("Simulated:", nrow(v$current$fossils), "fossil samples"))
        
        session$sendCustomMessage("loading", FALSE)
        
      })
      
      # Clear fossils
      observeEvent(input$clearfossils, {
        v$current$fossils = FossilSim::fossils()
      })
      
      # download simulated data as RData
      output$dldata <- downloadHandler(
        filename = function() {paste0("data-", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), ".RData")},
        content = function(file) {
          data = list(tree = v$current$tree, taxonomy = v$current$tax, fossils = v$current$fossils)
          save(data, file = file)
        })
      
      # Input synchronization with data ---
      # When an input is changed run through all of the inputs and grabs the data
      #todo -- implement a more efficient method ie : only grab what was changed
      observe({
        for (inpId in c(allNumericInputs, allCheckboxInputs, allTextInputs)) {
          v$current[[inpId]] = input[[inpId]]
        }
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
