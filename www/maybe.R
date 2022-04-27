tabsetPanel(id = "tabset2", 
    tabPanel(p("Tree", id = "test1"),
    
    mainPanel(
        # reaction outputs
        plotOutput(outputId = "tree"), # has matching render function
      )
    ),
    
    tabPanel(p("Tax", id = "test2"),
             
    mainPanel(
        # reaction outputs
        plotOutput(outputId = "tax"), # has matching render function
      )
    )
    )