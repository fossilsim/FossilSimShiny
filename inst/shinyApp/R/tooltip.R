# Tooltip : info bar at the bottom of the application ----
tooltip <- function(id) {
  tagList(
    fixedPanel(
      p(id = "tooltip", "i : "),
      left = 0,
      bottom = 0,
      height = "40px",
      width="100%",
      style = "background-color: #54aeff; font-size: 16px;",
    ),
    
    # Import Javascript for tooltip, you can add/modify tooltip descriptions in www/tooltip.js
    tags$script(src = "tooltip.js")
  )
}