# Output Sidebar : Fixed sidebar on the left of the app where everything is plotted ----
# Handles changing tabs and switching between graphs (tree, taxonomy, fossils + tree)

# UI/Frontend --- ---
outputSidebar <- function(id) {
  
  # Wrapping our ids ---
  # The NS function allows us to wrap the ids of our ui components
  # This is necessary for nested R modules like this one
  # ns stands for namespace, more info : https://shiny.rstudio.com/articles/modules.html
  ns <- NS(id)
  
  # This is the name of the first tab ---
  # needs to match main.R/keys[2]
  # this is the value to modify if you would like to rename the tabs
  initialTabName = "i"
  
  # Start of our UI ---
  # tagList is the nested version of fluidPage
  tagList(
    fixedPanel(id=ns("saveButtons"),
               downloadButton(ns("saveas"), "Save image as...", icon = shiny::icon("file-export")),
               radioButtons(ns("imgformat"), label = NULL, c("PNG", "PDF"), inline = TRUE),
               top = "60px", left = "33%", width = "67%"),
    
    fixedPanel(id=ns("outputDisplay"),
               
               tabsetPanel(id = ns("outputTabset"),
                           
                           # Initial tab ---
                           tabPanel(
                             
                             initialTabName,
                             
                             # This is the view selector : tree, taxonomy, tree+fossils ---
                             fixedPanel(id = ns(paste0(initialTabName, "fixedview")),
                                        
                                        selectInput(inputId = ns(paste0(initialTabName, "dropview")),
                                                    label="", choices = c("tree", "taxonomy", "tree+fossils")),
                                        top = "60px", right = "5%", width = "15%", height="5vh"),
                             
                             # Main plot output ---
                             plotOutput(outputId = ns(paste0(initialTabName, "tree")),
                                        width = "100%", height = "85vh"),
                           ),
                           
                           # New tab button ---
                           tabPanel("+", "")
               ),
               
               # Set size of output panel 
               #todo -- make it react to different screen resolution
               left = "33%", top= "120px", bottom = "100px", width = "67%"
    )
  )
}

# Server code --- ---
outputSidebarServer <- function(id, v, k) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      # Setup ---
      # Getting our namespace
      ns <- NS(id)
      
      # Grabing our keys
      keys = k
      
      # tabManager$createdKeys stores which tab have been created
      # At the start, only "current" and "i" have been created. They correspond to keys[1] and keys[2]
      v$createdKeys = c(keys[1], keys[2])
      
      makePlots = reactive({
        session$sendCustomMessage("loading", FALSE)
        
        validate(need( (v$current$taxonomybeta >= 0 && v$current$taxonomybeta <= 1), "Probability of bifurcation needs to be between 0 and 1"))
        validate(need(is.null(v$current$error) || !v$current$error, v$current$errorMsg))
        
        # Check if there is a tree
        if(is.null(v$current$tree)) return()
        
        # Show fossil taxonomy
        if(v$current$showtaxonomy) {
          validate(need(!is.null(v$current$tax), "Show taxonomy is selected but no taxonomy was found"))
        }
        
        par(oma = c(8, 0, 0, 0))
        # View 1) tree : display tree with empty fossils
        if (input[[paste0(v$currentTab, "dropview")]] == "tree"){
          plot(FossilSim::fossils(),
               v$current$tree,
               taxonomy = v$current$tax,
               show.tree = v$current$showtree,
               show.ranges = v$current$showranges,
               show.fossils = v$current$showfossils,
               show.strata = v$current$showstrata,
               show.taxonomy = v$current$showtaxonomy,
               reconstructed = v$current$reconstructed,
               show.tip.label = v$current$showtips,
               align.tip.label = TRUE)
        }
        
        # View 2) taxonomy : displays the generated taxonomy
        if (input[[paste0(v$currentTab, "dropview")]] == "taxonomy"){
          validate(need(!is.null(v$current$tax), "No taxonomy found, please simulate a taxomony."))
          validate(need(all(v$current$tree$edge %in% v$current$tax$edge), "Taxonomy incompatible with tree, please resimulate taxonomy."))
          
          plot(v$current$tax, v$current$tree, legend.position = "bottomleft",
               show.tip.label = v$current$showtips,
               align.tip.label = TRUE)
        }
        
        # View 3) tree+fossils : displays the tree with the fossils on top
        if (input[[paste0(v$currentTab, "dropview")]] == "tree+fossils") {
          validate(need(!is.null(v$current$fossils), "No fossils found, please simulate fossils."))
          
          is.enviro.model = !is.null(v$current$fossilModelName) && v$current$fossilModelName == "Holland"
          show.depth = (v$current$`enviro-dep-showsamplingproxy` && is.enviro.model)
          strata = if((v$current$showstrata || v$current$`enviro-dep-showsamplingproxy`) && is.enviro.model) v$current$strata else 1
          int.ages = if(v$current$showstrata && (!is.null(v$current$fossilModelName) && v$current$fossilModelName == "Non-Uniform")) v$current$int.ages else NULL
          
          plot(v$current$fossils,
               v$current$tree,
               taxonomy = v$current$tax,
               show.tree = v$current$showtree,
               show.ranges = v$current$showranges,
               show.fossils = v$current$showfossils,
               show.strata = v$current$showstrata,
               show.taxonomy = v$current$showtaxonomy,
               
               # Only for Holland 95
               show.proxy = show.depth,
               proxy.data = v$current$wd,
               strata = strata,
               
               # Only for time-dependent
               interval.ages = int.ages,
               
               reconstructed = v$current$reconstructed,
               show.tip.label = v$current$showtips,
               align.tip.label = TRUE)
        }
        recordPlot()
      })
      
      # Displaying plots ---
      
      # for every tab
      for (k in keys) {
        # update the tab's plot if input values have changed
        # in reality only $current plot will ever be changed
        output[[paste0(k,"tree")]] <- renderPlot({ makePlots() })
      }
      
      # Dropdown/View selector ----
      # Priority is very important (last to first)
      # First is the tree view which gets triggered when the tree data is changed, ie : when the user simulates a tree 
      observeEvent(v$current$tree, {
        updateSelectInput(session, paste0(v$currentTab, "dropview"), selected = "tree")
      })
      # Second is the taxonomy view, same mechanism
      observeEvent(v$current$tax, {
        if (!is.null(v$current$tax) && all(v$current$tree$edge %in% v$current$tax$edge)) {
          updateSelectInput(session, paste0(v$currentTab, "dropview"), selected = "taxonomy")
        }
      })
      # Last is the fossil+tree view
      observeEvent(v$current$fossils, {
        if (length(v$current$fossils$sp) != 0) {
          updateSelectInput(session, paste0(v$currentTab, "dropview"), selected = "tree+fossils")
        }
      })
      
      
      # Change/create new tabs ---
      observeEvent(input$outputTabset, {
        
        # Create new tab when the "+" tab is clicked --
        if (input$outputTabset == "+") {
          
          # The following code can be a bit confusing, so, I have decided to comment it extensively --
          
          # First, save the current tab
          v[[v$currentTab]] = v$current
          
          # Then, get a new key from the available keys for the new tab
          v$currentTab = toadNewTabKey(keys, v$createdKeys)
          
          # Add this new key to the list of used keys, so it doesn't get used twice
          v$createdKeys = append(v$createdKeys, v$currentTab)
          
          # Switch the current tab value to the newly created one
          v$current = v[[v$currentTab]]
          
          # --<
          
          # Create tab
          # same layout as the initial tab
          # each component has an id consisting of it's tab id and it's component name
          # ex : for the tab "ii", the component "dropview" is named "iidropview"
          insertTab(inputId = "outputTabset",
                    
                    tabPanel(title = v$currentTab,
                             
                             fixedPanel(id = ns(paste0(v$currentTab, "fixedview")),
                                        
                                        selectInput(inputId = ns(paste0(v$currentTab, "dropview")),
                                                    label="", choices = c("tree", "taxonomy", "tree+fossils")),
                                        top = "60px", right = "5%", width = "15%", height="5vh"),
                             
                             plotOutput(outputId = ns(paste0(v$currentTab, "tree")),
                                        width = "100%", height = "85vh")), target = "+", position="before")
          
          # If all of the tab keys have been used, we delete the "+" tab
          if (v$createdKeys[length(v$createdKeys)] == keys[length(keys)]) {
            removeTab(inputId = "outputTabset", target = "+")
          }
          
          # Set the new tab as selected in the UI
          updateTabsetPanel(session, "outputTabset", v$currentTab)
        }
        
        # Change tab --
        else {
          v[[v$currentTab]] = v$current
          v$currentTab = input$outputTabset
          v$current = v[[v$currentTab]]
        }
      })
      
      output$saveas <- downloadHandler(
        filename = function() {
          if(input$imgformat == "PNG") paste0("plot-", Sys.Date(), ".png")
          else paste0("plot-", Sys.Date(), ".pdf")
        },
        content = function(file) {
          if(input$imgformat == "PNG") png(file, width = 2500, height = 1500)
          else pdf(file, width = 20, height = 15)
          replayPlot(makePlots())
          dev.off()
        })
    }
  )
}