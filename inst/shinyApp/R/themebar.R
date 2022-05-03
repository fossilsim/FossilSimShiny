# Themebar : Lets you change UI theme using a bar with buttons at the bottom right of the application ----
themebar <- function(id) {
  ns <- NS(id)
  
  tagList(
    fixedPanel(
      id = NS(id, "fixed"),
      right = 5,
      bottom = 5,
      width="25%",
      style="text-align : right;"
    ),
    
    # Import Javascript for themebar, you can create/modify themes using www/themeManger.js and www/style.css
    tags$script(src = "themeManager.js")
  )
}