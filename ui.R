
page_sidebar(
  title = "EDI SFE Fish Data",
  sidebar = sidebar(
    width = 300,
    pickerInput(inputId = "sources", label = "Data Sources", multiple = TRUE, 
                choices = sources, selected = sources,
                options = list(`actions-box` = TRUE, liveSearch = TRUE, size = 5)),
    sliderInput(inputId = "years", label = "Years", min = yr_min, max = yr_max, 
                value = c(yr_min, yr_max), sep = "", step = 1),
    actionButton(inputId = "get_species", "Get Species Data")
  ),
  leafletOutput("map")
)

