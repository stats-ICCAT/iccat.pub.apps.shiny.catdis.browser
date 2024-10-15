ui = function() {
  TITLE = "ICCAT / Task 2 / CATDIS / browser"
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
            column(width = 8,
              h2(
                style = "margin-top: 5px !important",
                img(src = "iccat-logo.jpg", height = "48px"),
                span(TITLE)
              )
            )
          ),
          fluidRow(
            column(width = 2,
              fluidRow(
                column(width = 12,
                  sliderInput("years",
                    label = "Year range:",
                    width = "100%",
                    min = MIN_YEAR, max = MAX_YEAR,
                    value = c(max(MIN_YEAR, MAX_YEAR - 30 + 1), MAX_YEAR),
                    sep = "",
                    step  = 1
                  )
                )
              ),
              fluidRow(
                column(width = 12,
                  UI_select_input("quarters",
                    label = "Quarter(s):",
                    placeholder = "All quarters",
                    choices= ALL_QUARTERS)
                )
              ),
              fluidRow(
                column(width = 12,
                  UI_select_input("species",
                    label = "Species:",
                    placeholder = "All species",
                    choices = ALL_SPECIES)
                )
              ),
              #fluidRow(
              #  column(
              #    width = 12,
              #    UI_select_input("stocks", "Stock(s):", "All stocks", ALL_STOCKS)
              #  )
              #),
              fluidRow(
                column(width = 12,
                  UI_select_input("flags",
                    label = "Flag(s):",
                    placeholder = "All flags",
                    choices = ALL_FLAGS)
                )
              ),
              fluidRow(
                column(width = 12,
                  UI_select_input("fleets",
                    label = "Fleet(s):",
                    placeholder = "All fleets",
                    choices = ALL_FLEETS)
                )
              ),
              fluidRow(
                column(width = 12,
                  UI_select_input("gearGroups",
                    label = "Gear group(s):",
                    placeholder = "All gear groups",
                    choices = ALL_GEAR_GROUPS)
                )
              ),
              fluidRow(
                column(width = 12,
                  UI_select_input("schoolTypes",
                    label = "School type(s):",
                    placeholder = "All school types",
                    choices = ALL_FISHING_MODES)
                )
              ),
              fluidRow(
                column(width = 12,
                  UI_select_input("samplingAreas",
                    label = "Sampling area(s):",
                    placeholder = "All sampling areas",
                    choices = ALL_SAMPLING_AREAS)
                )
              ),
              fluidRow(
                column(width = 6,
                  actionButton("resetFilters",
                    label = "Reset all filters",
                    icon = icon("filter-circle-xmark"),
                    style = "width: 100%")
                ),
                column(width = 6,
                  downloadButton("downloadFiltered",
                    label = "Download",
                    style = "width: 100%")
                )
              ),
              fluidRow(
                column(width = 12,
                  hr(),
                  span("Data last updated on:"),
                  strong(META$LAST_UPDATE)
                )
              )
            ),
            column(width = 10,
              tabsetPanel(
                id = "dataset",
                tabPanel(TAB_PIEMAP,
                  icon = icon("chart-pie"),
                  div(style = "padding-top: .5em",
                    fluidRow(
                      column(width = 3,
                        style = "padding-top: 3.65em",
                        fluidRow(
                          column(width = 12,
                            UI_select_input_single("piemapArea",
                              label = "Zoom on:",
                              choices = ALL_ATLANTIC_AREAS
                            )
                          )
                        ),
                        fluidRow(
                          column(width = 12,
                            UI_select_input("piemapAreaOverlay",
                              label = "Overlay(s):",
                              placeholder = "Select overlays",
                              choices = ALL_AREAS_OVERLAYS
                            )
                          )
                        ),
                        fluidRow(
                          column(width = 12,
                            UI_select_input_single("piemapCategory",
                              label = "Categorise by:",
                              choices = setNames(
                                c("GEAR_GROUP", "SPECIES", "SCHOOL_TYPE", "QUARTER"),
                                c("Gear group", "Species", "School type", "Quarter")
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(width = 12,
                            UI_select_input_single("piemapMetric",
                              label = "Metric:",
                              choices = setNames(
                                c("AC",           "AV",             "AVC"),
                                c("Accumulation", "Annual average", "Annual average (corrected)")
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(width = 12,
                            UI_select_input_single("pieCenter",
                              label = "Pie position:",
                              choices = setNames(
                                c("O",             "G"),
                                c("Grid centroid", "Grid center")
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(width = 12,
                            UI_select_input_single("catchScale",
                              label = "Reference catch scale",
                              choices = c("Calculated", "Fixed")
                            )
                          )
                        ),
                        fluidRow(
                          column(width = 12,
                            sliderInput("radius",
                              label = "Maximum pie radius (units)",
                              width = "100%",
                              min = 0.5, max = 5,
                              value = 3,
                              sep = "",
                              step  = .1
                            ),
                            conditionalPanel(
                              condition = "input.catchScale == 'Fixed'",
                              sliderInput("catch",
                                label = "Reference catch [ log10(t) ]",
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
                      column(width = 9,
                        plotOutput("piemap", width = "1000px", height = "800px")
                      )
                    )
                  )
                ),
                tabPanel(
                  TAB_HEATMAP,
                  icon = icon("th"),
                  div(style = "padding-top: .5em",
                    fluidRow(
                      column(width = 3,
                        style = "padding-top: 3.65em",
                        fluidRow(
                          column(width = 12,
                            UI_select_input_single("heatmapArea",
                              label = "Zoom on:",
                              choices = ALL_ATLANTIC_AREAS
                            )
                          )
                        ),
                        fluidRow(
                          column(width = 12,
                            UI_select_input("heatmapAreaOverlay",
                              label = "Overlay(s):",
                              placeholder = "Select overlays",
                              choices = ALL_AREAS_OVERLAYS
                            )
                          )
                        ),
                        fluidRow(
                          column(width = 12,
                            UI_select_input_single(
                              "scale",
                              label = "Scale:",
                              choices =
                                setNames(
                                  c("LI",     "LN"),
                                  c("Linear", "Logarithmic")
                                )
                            )
                          ),
                        ),
                        fluidRow(
                          column(width = 12,
                            UI_select_input_single(
                              "heatmapMetric",
                              label = "Metric:",
                              choices = setNames(
                                c("AC",           "AV",             "AVC"),
                                c("Accumulation", "Annual average", "Annual average (corrected)")
                              )
                            )
                          )
                        )
                      ),
                      column(width = 9,
                        plotOutput("heatmap", width = "1000px", height = "800px")
                      )
                    )
                  )
                ),
                tabPanel(
                  TAB_RAW_DATA,
                  icon = icon("list-alt"),
                  fluidRow(
                    column(width = 12,
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
