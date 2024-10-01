server = function(input, output, session) {
  EMPTY_FILTER =
    list(years = c(),
         radius = 3,
         flags = c(),
         fleets = c(),
         gearGroups = c(),
         schoolTypes = c(),
         species = c(),
         stocks = c(),
         sampling_areas = c()
    )

  observeEvent(input$resetFilters, { session$reload() })

  default_filter_data = function(data, input = EMPTY_FILTER) {
    INFO(paste0("Years          : ", paste0(input$years,          collapse = "-")))
    INFO(paste0("Flags          : ", paste0(input$flags,          collapse = ", ")))
    INFO(paste0("Fleets         : ", paste0(input$fleets,         collapse = ", ")))
    INFO(paste0("Gear groups    : ", paste0(input$gearGroups,     collapse = ", ")))
    INFO(paste0("Fishing modes  : ", paste0(input$schoolTypes,   collapse = ", ")))
    INFO(paste0("Species        : ", paste0(input$species,        collapse = ", ")))
    INFO(paste0("Stocks         : ", paste0(input$stocks,         collapse = ", ")))
    INFO(paste0("Sampling areas : ", paste0(input$sampling_areas, collapse = ", ")))

    start = Sys.time()

    filtered = data

    has_years = length(input$years) == 2

    if(has_years) {
      first_year = input$years[1]
      last_year  = input$years[2]

      filtered = filtered[YEAR >= first_year & YEAR <= last_year]
    } else {
      first_year = min(data$YEAR)
      last_year  = max(data$YEAR)
    }

    if(!is.null(input$flags)) {
      filtered = filtered[FLAG %in% input$flags]
    }

    if(!is.null(input$fleets)) {
      filtered = filtered[FLEET %in% input$fleets]
    }

    if(!is.null(input$gearGroups)) {
      filtered = filtered[GEAR_GROUP %in% input$gearGroups]
    }

    if(!is.null(input$schoolTypes)) {
      filtered = filtered[SCHOOL_TYPE %in% input$schoolTypes]
    }

    if(!is.null(input$species)) {
      filtered = filtered[SPECIES %in% input$species]
    }

    if(!is.null(input$stocks)) {
      filtered = filtered[STOCK %in% input$stocks]
    }

    if(!is.null(input$samplingAreas)) {
      filtered = filtered[SAMPLING_AREA %in% input$samplingAreas]
    }

    end = Sys.time()

    INFO(paste0("Filtering data: ", end - start))

    INFO(paste0("Filtered data size: ", nrow(filtered)))

    return(filtered)
  }

  filter_catdis_data = reactive({
    return(
      filter_catdis_data_(input)
    )
  })

  filter_catdis_data_ = function(input = EMPTY_FILTER) {
    filtered = default_filter_data(CATDIS_Y, input)

    return(filtered)
  }

  validate_filtering = function(filtered_data) {
    filtered_rows = nrow(filtered_data)

    if(filtered_rows == 0) {
      shinyjs::disable("downloadFiltered")

      showModal(modalDialog(title  = "No matching records",
                            footer = NULL,
                            easyClose = TRUE,
                            fade = FALSE,
                            "Please refine your current filtering criteria!"))
    } else {
      shinyjs::enable("downloadFiltered")
    }

    #validate(need(filtered_rows > 0, "Current filtering criteria do not identify any valid record!"))

    return(filtered_data)
  }

  piemap = function() {
    CATDIS_data = validate_filtering(filter_catdis_data())

    if(nrow(CATDIS_data) == 0) return()

    category = input$piemapCategory
    metric   = input$metricPie

    colnames(CATDIS_data)[which(colnames(CATDIS_data) == category)] = "CATEGORY"

    if     (metric == "AC")  { CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE)), keyby = .(GRID, CATEGORY)] }
    else if(metric == "AV")  { CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE) / (input$years[2] - input$years[1] + 1)), keyby = .(GRID, CATEGORY)] }
    else if(metric == "AVC") { CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE) / length(unique(YEAR))), keyby = .(GRID, CATEGORY)] }

    all_categories = unique(CATDIS_data$CATEGORY)

    if(category == "GEAR_GROUP") {
      legend_label = "Gear group"
      fill_colors = REF_GEAR_GROUPS_COLORS[GEAR_GROUP_CODE %in% unique(CATDIS_data$CATEGORY)][order(GEAR_GROUP_CODE)]$FILL
    } else if(category == "SPECIES") {
      legend_label = "Species"
      fill_colors = SPECIES_COLORS[SPECIES %in% unique(CATDIS_data$CATEGORY)]$FILL
    } else if(category == "SCHOOL_TYPE") {
      legend_label = "School type"
      fill_colors = SCHOOL_TYPE_COLORS[SCHOOL_TYPE %in% unique(CATDIS_data$CATEGORY)]$FILL
    }

    if(length(all_categories) == 1) {
      # Otherwise, for datasets with only one gear, the geom_scatterpie function will yield an error...
      CATDIS_data = rbind(CATDIS_data, data.table(GRID = "6100000", CATEGORY = "foo", CATCH = 0))
    }

    CATDIS_data =
      merge(
        CATDIS_data, as.data.table(GRIDS_SF)[, .(CODE, LON = CENTER_LON, LAT = CENTER_LAT)],
        by.x = "GRID", by.y = "CODE"
      )

    CATDIS_data_w =
      dcast.data.table(
        CATDIS_data,
        LON + LAT ~ CATEGORY,
        fun.aggregate = sum,
        value.var = "CATCH"
      )

    CATDIS_data_w[, RADIUS     := rowSums(CATDIS_data_w[, 3:ncol(CATDIS_data_w)])]
    CATDIS_data_w[, RADIUS_REL := input$radius * sqrt(RADIUS / max(RADIUS))]

    return(
      map.atlantic() +
        geom_sf(
          data = st_as_sf(ATLANTIC_OCEAN_RAW_GEOMETRY, crs = 4326, wkt = "GEOMETRY_WKT"),
          fill = "transparent",
          color = "#00000044"
        ) +

        geom_scatterpie(
          data = CATDIS_data_w,
          aes(x = LON,
              y = LAT,
              r = RADIUS_REL
          ),
          linewidth = .3,
          alpha = .7,
          cols = as.character(sort(unique(CATDIS_data$CATEGORY))),

          long_format = FALSE
        ) +

        geom_scatterpie_legend(
          CATDIS_data_w$RADIUS_REL,
          x = -90,
          y = -25,
          labeller = function(x) {
            paste(prettyNum(round((x / input$radius) ^ 2 * max(CATDIS_data_w$RADIUS)), big.mark = ","), " t")
          },
          breaks = c(0, input$radius / sqrt(2), input$radius),
          size = 2.5
        ) +

        scale_fill_manual(legend_label, values = fill_colors) +
        guides(
          fill = guide_legend(
            position = "right"
          )
        )
    )
  }

  output$piemap =
    renderPlot(res = 120, {
      piemap()
    })

  heatmap = function() {
    CATDIS_data = validate_filtering(filter_catdis_data())

    if(nrow(CATDIS_data) == 0) return()

    metric   = input$metricHeat
    scale    = input$scale

    label = "Catches / year"
    label_unit = "t"

    if (metric == "AC")  {
      label = "Catches"
      CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE)), keyby = .(GRID)]
    } else if(metric == "AV")  {
      CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE) / (input$years[2] - input$years[1] + 1)), keyby = .(GRID)]
    } else if(metric == "AVC") {
      CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE) / length(unique(YEAR))), keyby = .(GRID)]
    }

    if(scale == "LN") {
      label_unit = "log10(t)"
      CATDIS_data[, CATCH := log10(CATCH)]
    }

    CATDIS_data =
      merge(
        CATDIS_data, as.data.table(GRIDS_SF)[, .(CODE, GEOMETRY_WKT)],
        by.x = "GRID", by.y = "CODE"
      )

    return(
      map.atlantic() +
        geom_sf(
          data = st_as_sf(ATLANTIC_OCEAN_RAW_GEOMETRY, crs = 4326, wkt = "GEOMETRY_WKT"),
          fill = "transparent",
          color = "#00000044"
        ) +

        geom_sf(
          data = CATDIS_data,
          aes(
            geometry = GEOMETRY_WKT,
            fill = CATCH
          ),
          color = "#00000022",
          alpha = .6
        ) +

        scale_fill_gradient(
          low   = "#FFFFFF",
          high  = "#0000AA",
          guide = "legend",
          labels = scales::comma
        ) +

        guides(
          fill = guide_legend(
            title = paste0(label, " (", label_unit, ")"),
            position = "bottom",
            nrow = 1
          )
        )
    )
  }

  output$heatmap =
    renderPlot(res = 120, {
      heatmap()
    })

  output$rawData =
    renderDataTable({
      filtered_data = validate_filtering(filter_catdis_data())

      return(
        DT::datatable(
          filtered_data,
          options = list(
            pageLength = INITIAL_NUM_ENTRIES,
            autoWidth = TRUE,
            scrollX = FALSE,
            dom = "ltipr" # To remove the 'search box' - see: https://rstudio.github.io/DT/options.html and https://datatables.net/reference/option/dom
          ),
          filter    = "none",
          selection = "none",
          rownames = FALSE,
          colnames = c("Year",
                       "Flag name", "Fleet code",
                       "Gear group", "School type",
                       "Species", "Stock",
                       "Sampling area", "Grid",
                       "Catch (t)")
        )
        %>% DT::formatCurrency(columns = c("CATCH"), currency = "")
      )
    })

  get_filename_components = function(input) {
    components = c(paste0(input$years,         collapse = "-"),
                   paste0(input$species,       collapse = "+"),
                   paste0(input$stocks,        collapse = "+"),
                   paste0(input$flags,         collapse = "+"),
                   paste0(input$fleets,        collapse = "+"),
                   paste0(input$gearGroups,    collapse = "+"),
                   paste0(input$gears,         collapse = "+"),
                   paste0(input$schoolTypes,   collapse = "+"),
                   paste0(input$samplingAreas, collapse = "+"))

    components = components[which(components != "")]

    return(paste0(components, collapse = "_"))
  }

  get_filename_components_piemap = function(input) {
    components = get_filename_components(input)

    return(
      paste0(components, "_", input$piemapCategory, "_", input$metricPie)
    )
  }

  get_filename_components_heatmap = function(input) {
    components = get_filename_components(input)

    return(
      paste0(components, "_", input$scale, "_", input$metricHeat)
    )
  }

  serialize_last_update_date = function() {
    return(
      str_replace_all(META$LAST_UPDATE, "\\-", "")
    )
  }

  output$downloadFiltered = downloadHandler(
    filename = function() {
      output = input$output

      filename_prefix = paste0("ICCAT_CATDIS_", serialize_last_update_date())

      if(output == TAB_PIEMAP)
        return(paste0(filename_prefix, "_piemap_",  get_filename_components_piemap (input), ".png"))
      else if(output == TAB_HEATMAP)
        return(paste0(filename_prefix, "_heatmap_", get_filename_components_heatmap(input), ".png"))
      else
        return(paste0(filename_prefix, "_data_",    get_filename_components        (input), ".csv.gz"))
    },
    content = function(file) {
      output = input$output

      if(output == TAB_PIEMAP)
        ggsave(filename = file, piemap(), width = 13, height = 12)
      else if(output == TAB_HEATMAP)
        ggsave(filename = file, heatmap(), width = 13, height = 12)
      else
        write.csv(filter_catdis_data(), gzfile(file), row.names = FALSE, na = "")
    }
  )
}
