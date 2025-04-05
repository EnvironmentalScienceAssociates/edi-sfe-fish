
page_sidebar(
  title = "EDI SFE Fish Abundance",
  sidebar = sidebar(
    width = 300,
    conditionalPanel(
      condition = 'input.nav == "Map"',
      sliderInput(inputId = "years", label = "Years", min = yr_min, max = yr_max, 
                  value = c(yr_min, yr_max), sep = "", step = 1),
      pickerInput(inputId = "sources", label = "Sources", multiple = TRUE, 
                  choices = sources, selected = sources,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5)),
      pickerInput(inputId = "group_by", label = "Group by", multiple = TRUE, 
                  choices = c("Taxa", "Source", "Year", "Month", "Date"), 
                  selected = c("Taxa", "Source", "Year")),
      uiOutput("messageButton"),
      uiOutput("sourceMessage")
    ),
    conditionalPanel(
      condition = 'input.nav == "Table"',
      uiOutput("taxa"),
      uiOutput("months"),
      uiOutput("dateRange"),
      downloadButton("download", "Download Table", icon = icon("download"))
    )

  ),
  navset_card_underline(
    id = "nav",
    nav_panel(
      title = "Map",
      leafletOutput("map")
      ),
    nav_panel(
      title = "Table",
      DT::DTOutput("table")
    )
  )
)

