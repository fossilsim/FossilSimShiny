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
    
    fixedPanel(id=NS(id, "outputDisplay"),
      
      tabsetPanel(id = NS(id, "outputTabset"),
      
      # Initial tab ---
      tabPanel(
        
        initialTabName,
        
        # This is the view selector : tree, taxonomy, tree+fossils ---
        fixedPanel(id = NS(id, paste(initialTabName, "ifixedview", sep="")),
          
                   selectInput(inputId = NS(id, paste(initialTabName, "dropview", sep = "")),
                               label="", choices = c("tree", "taxonomy", "tree+fossils")),
                               top = "40px", right = "5%", width = "15%", height="5vh"),
        
        # Main plot output ---
        plotOutput(outputId = NS(id, paste(initialTabName, "tree", sep="")),
                   width = "100%", height = "85vh"),
        
      ),
      
      # New tab button ---
      tabPanel(
        "+",
        ""
      )
    ),
      
    # Set size of output panel 
    #todo -- make it react to different screen resolution
    left = "33%",
    top= "60px",
    width = "67%"
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
      
      
      # Diplaying plots ---
      
      # for every tab
      for (k in keys) {
        
        # update the tab's plot if input values have changed
        # in reality only $current plot will ever be changed
        
        output[[paste(k,"tree",sep="")]] <- renderPlot( {
          
          # Newick tree import
          if(v$current$usertree)
            validate(need(!is.null(ape::read.tree(text = v$current$newick)) , "Specify newick string"))
          else {
            validate(need( ((v$current$mu/v$current$lambda) < 0.9), "Turnover a bit too high! Be kind to the server - try a lower extinction rate!"))
            validate(need( (v$current$tips < 101), "Please keep the number of tips under one hundred ~ thank you."))
          }
          
          validate(need( (v$current$taxonomybeta >= 0 && v$current$taxonomybeta <= 1 && v$current$taxonomylambda >= 0 && v$current$taxonomylambda <= 1), "Rates need to be between one and zero."))
          
          
          # Check if there is a tree
          if(is.null(v$current$tree)) return()
    
          # Show fossil taxonomy
          if(v$current$showtaxonomy) {
            validate(need(!is.null(v$current$tax), "No taxomony..."))
            
            # Synchronize fossils and taxonomy
            #todo -- move this to generate fossil or taxonomy
            if(!attr(v$current$fossils,"from.taxonomy"))
              v$current$fossils = FossilSim::reconcile.fossils.taxonomy(v$current$fossils, v$current$tax)
          }
          
          # Temporary
          #todo -- Allow user to tweak this
          wd = FossilSim::sim.gradient(v$current$strata)
          
          # View 1) tree : display tree with empty fossils
          if (input[[paste(v$currentTab, "dropview", sep="")]] == "tree"){
            plot(FossilSim::fossils(),
                 v$current$tree,
                 taxonomy = v$current$tax,
                 show.tree = v$current$showtree,
                 show.ranges = v$current$showranges,
                 show.fossils = v$current$showfossils,
                 show.strata = v$current$showstrata,
                 show.taxonomy = v$current$showtaxonomy,
                 # show.proxy = FALSE,
                 # proxy.data = wd,
                 # strata = v$current$current$strata,
                 reconstructed = v$current$reconstructed,
                 show.tip.label = v$current$showtips,
                 align.tip.label = TRUE)
          
          
          }
          
          # View 2) taxonomy : displays the generated taxonomy
          if (input[[paste(v$currentTab, "dropview", sep="")]] == "taxonomy"){
            validate(need(!is.null(v$current$tax), "Please simulate taxomony..."))
            if(!all(v$current$tree$edge %in% v$current$tax$edge)) return()
            plot(v$current$tax, v$current$tree, legend.position = "bottomleft")
          }
          
          # View 3) tree+fossils : displays the tree with the fossils on top
          if (input[[paste(v$currentTab, "dropview", sep="")]] == "tree+fossils"){
          plot(v$current$fossils,
               v$current$tree,
               taxonomy = v$current$tax,
               show.tree = v$current$showtree,
               show.ranges = v$current$showranges,
               show.fossils = v$current$showfossils,
               show.strata = v$current$showstrata,
               show.taxonomy = v$current$showtaxonomy,
               
               # Only for Holland 95
               show.proxy = (v$current$showsamplingproxy && v$current$fossilModelName == "Holland"),
               proxy.data = wd,
               strata = v$current$strata,
               
               reconstructed = v$current$reconstructed,
               show.tip.label = v$current$showtips,
               align.tip.label = TRUE)
          }
        })
        }
      
      
      
      # Dropdown/View selector ----
      # Priority is very important (last to first)
      # First is the tree view which gets triggered when the tree data is changed, ie : when the user simulates a tree 
      observeEvent(v$current$tree, {
        updateSelectInput(session, paste(v$currentTab, "dropview", sep=""), selected = "tree")
      })
      # Second is the taxonomy view, same mechanism
      observeEvent(v$current$tax, {
        if (!(is.null(v$current$tax))) {
          if(all(v$current$tree$edge %in% v$current$tax$edge)){
            updateSelectInput(session, paste(v$currentTab, "dropview", sep=""), selected = "taxonomy")}}
      })
      # Last is the fossil+tree view
      observeEvent(v$current$fossils, {
        if (length(v$current$fossils$sp) != 0) {
        updateSelectInput(session, paste(v$currentTab, "dropview", sep=""), selected = "tree+fossils")}
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
              
              fixedPanel(id = NS(id, paste(v$currentTab, "fixedview", sep="")),

                         selectInput(inputId = NS(id, paste(v$currentTab, "dropview", sep="")),
                                     label="", choices = c("tree", "taxonomy", "tree+fossils")),
                                     top = "40px", right = "5%", width = "15%", height="5vh"),
                                                               
                                                       
              plotOutput(outputId = NS(id, paste(v$currentTab, "tree", sep="")),
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
          # Debug : print(v$currentTab)
          v$current = v[[v$currentTab]]
        }
        
        # Send our current tab to saveas
        session$sendCustomMessage("tab-for-saveas", v$currentTab)
      })
    }
  )
}