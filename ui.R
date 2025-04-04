
page_sidebar(
  title = "EDI SFE Fish Abundance",
  sidebar = sidebar(
    width = 300,
    pickerInput(inputId = "sources", label = "Data Sources", multiple = TRUE, 
                choices = sources, selected = sources,
                options = list(`actions-box` = TRUE, liveSearch = TRUE, size = 5)),
    sliderInput(inputId = "years", label = "Years", min = yr_min, max = yr_max, 
                value = c(yr_min, yr_max), sep = "", step = 1),
    pickerInput(inputId = "group_by", label = "Group by", multiple = TRUE, 
                choices = c("Source", "Year", "Month", "Date", "Taxa"), 
                selected = c("Source", "Year", "Taxa")),
    uiOutput("drawMessage"),
    actionButton("tally_fish", label = "Tally Fish Abundance", disabled = TRUE)
  ),
  leafletOutput("map")
)

