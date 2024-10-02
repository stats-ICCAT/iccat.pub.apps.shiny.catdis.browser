ui = function() {
  TITLE = paste0("ICCAT / Task 2 / CATDIS / browser / ", META$LAST_UPDATE)
  return(
    fluidPage(
      shinyjs::useShinyjs(),
      title = TITLE,
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tags$div(
        class = "main-container",
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          tags$div(id = "glasspane",
                   tags$div(class = "loading", "Filtering data and preparing output...")
          )
        ),
        tags$div(
          fluidRow(
            column(
              width = 8,
              h2(
                style = "margin-top: 5px !important",
                img(src = "iccat-logo.jpg", height = "48px"),
                span(TITLE)
              )
            )
          ),
          fluidRow(
            column(
              width = 2,
              fluidRow(
                column(
                  width = 12,
                  sliderInput("years", "Year range",
                              width = "100%",
                              min = MIN_YEAR, max = MAX_YEAR,
                              value = c(max(MIN_YEAR, MAX_YEAR - 30 + 1), MAX_YEAR),
                              sep = "",
                              step  = 1
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  UI_select_input("species", "Species", ALL_SPECIES)
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  UI_select_input("stocks", "Stock(s)", ALL_STOCKS)
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  UI_select_input("flags", "Flag(s)", ALL_FLAGS)
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  UI_select_input("fleets", "Fleet(s)", ALL_FLEETS)
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  UI_select_input("gearGroups", "Gear group(s)", ALL_GEAR_GROUPS)
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  UI_select_input("schoolTypes", "School type(s)", ALL_FISHING_MODES)
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  UI_select_input("samplingAreas", "Sampling area(s)", ALL_SAMPLING_AREAS)
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  actionButton("resetFilters", "Reset all filters", icon = icon("filter-circle-xmark"), style = "width: 100%")
                ),
                column(
                  width = 6,
                  downloadButton("downloadFiltered", "Download", style = "width: 100%")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  hr(),
                  span("Data last updated on:"),
                  strong(META$LAST_UPDATE)
                )
              )
            ),
            column(
              width = 10,
              tabsetPanel(
                id = "output",
                tabPanel(TAB_PIEMAP,
                  div(style = "padding-top: .5em",
                    fluidRow(
                      column(
                        width = 2,
                        fluidRow(
                          column(
                            width = 12,
                            selectInput("piemapCategory", label = "Categorise by:",
                                        choices =
                                          setNames(
                                            c("GEAR_GROUP", "SPECIES", "SCHOOL_TYPE"),
                                            c("Gear group", "Species", "School type")
                                          )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            selectInput("metricPie", label = "Metric:",
                                        choices =
                                          setNames(
                                            c("AC",           "AV",             "AVC"),
                                            c("Accumulation", "Annual average", "Annual average (corrected)")
                                          )
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            sliderInput("radius", "Maximum pie radius (units)",
                                        width = "100%",
                                        min = 0.5, max = 5,
                                        value = 3,
                                        sep = "",
                                        step  = .1
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            selectInput("catchScale", label = "Reference catch scale",
                                        choices = c("Calculated", "Fixed")
                            ),
                            conditionalPanel(
                              condition = "input.catchType == 'Fixed'",
                              sliderInput("catch", "Reference catch (log10(t))",
                                          width = "100%",
                                          min = 3, max = 6,
                                          value = 5,
                                          sep = "",
                                          step  = .1
                              )
                            )
                          )
                        )
                      ),
                      column(
                        width = 10,
                        plotOutput("piemap", width = "1000px", height = "800px")
                      )
                    )
                  )
                ),
                tabPanel(TAB_HEATMAP,
                  div(style = "padding-top: .5em",
                    fluidRow(
                      column(
                        width = 2,
                        fluidRow(
                          column(
                            width = 12,
                            selectInput("scale", label = "Scale:",
                                        choices =
                                          setNames(
                                            c("LI",     "LN"),
                                            c("Linear", "Log10")
                                          )
                            )
                          ),
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            selectInput("metricHeat", label = "Metric:",
                                        choices =
                                          setNames(
                                            c("AC",           "AV",             "AVC"),
                                            c("Accumulation", "Annual average", "Annual average (corrected)")
                                          )
                            )
                          )
                        )
                      ),
                      column(
                        width = 10,
                        plotOutput("heatmap", width = "1000px", height = "800px")
                      )
                    )
                  )
                ),
                tabPanel(TAB_RAW_DATA,
                  fluidRow(
                    column(
                      width = 12,
                      tags$div(id = "filtered_data_container", style = "padding-top: .5em",
                        dataTableOutput("rawData")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
