server = function(input, output, session) {
  EMPTY_FILTER =
    list(years = c(),
         quarters = c(),
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
    INFO(paste0("Quarters       : ", paste0(input$quarters,       collapse = ", ")))
    INFO(paste0("Flags          : ", paste0(input$flags,          collapse = ", ")))
    INFO(paste0("Fleets         : ", paste0(input$fleets,         collapse = ", ")))
    INFO(paste0("Gear groups    : ", paste0(input$gearGroups,     collapse = ", ")))
    INFO(paste0("Fishing modes  : ", paste0(input$schoolTypes,    collapse = ", ")))
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

    if(!is.null(input$quarters)) {
      filtered = filtered[QUARTER %in% names(ALL_QUARTERS[as.integer(input$quarters)])]
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
    filtered = default_filter_data(CATDIS_Q, input)

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

    area     = input$piemapArea
    category = input$piemapCategory
    metric   = input$piemapMetric

    overlays = input$piemapAreaOverlay

    # Selected area boundaries
    area_xlim = ATLANTIC_AREAS_LIMITS[area][[1]]$xlim
    area_ylim = ATLANTIC_AREAS_LIMITS[area][[1]]$ylim

    # Calculates the pie chart legend position in order to show it always
    # in the same place on the map (regardless of the selected area)

    x_width_default  = DEFAULT_XLIM[2] - DEFAULT_XLIM[1]
    x_width          = area_xlim[2]    - area_xlim[1]

    y_height_default = DEFAULT_YLIM[2] - DEFAULT_YLIM[1]
    y_height         = area_ylim[2]    - area_ylim[1]

    default_legend_x_displacement = DEFAULT_PIECHART_LEGEND_X - DEFAULT_XLIM[1]
    default_legend_y_displacement = DEFAULT_PIECHART_LEGEND_Y - DEFAULT_YLIM[1]

    default_legend_x_displacement_rel = default_legend_x_displacement / x_width_default
    default_legend_y_displacement_rel = default_legend_y_displacement / y_height_default

    calc_legend_x = area_xlim[1] + default_legend_x_displacement_rel * x_width
    calc_legend_y = area_ylim[1] + default_legend_y_displacement_rel * y_height

    # Revert on using the manually-set legend coordinates...

    legend_x = ATLANTIC_AREAS_LIMITS[area][[1]]$legend_x
    legend_y = ATLANTIC_AREAS_LIMITS[area][[1]]$legend_y

    colnames(CATDIS_data)[which(colnames(CATDIS_data) == category)] = "CATEGORY"

    # Computes the selected metric for each pie:

    # Total accumulation
    if     (metric == "AC")  { CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE)),                                         keyby = .(GRID, GRID_LON, GRID_LAT, CATEGORY)] }
    # Annual average
    else if(metric == "AV")  { CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE) / (input$years[2] - input$years[1] + 1)), keyby = .(GRID, GRID_LON, GRID_LAT, CATEGORY)] }
    # Annual average (corrected)
    else if(metric == "AVC") { CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE) / length(unique(YEAR))),                  keyby = .(GRID, GRID_LON, GRID_LAT, CATEGORY)] }

    all_categories = unique(CATDIS_data$CATEGORY)

    # Computes the pie slices colors based on the selected category:
    if(category == "GEAR_GROUP") {
      legend_label = "Gear group"
      fill_colors = REF_GEAR_GROUPS_COLORS[GEAR_GROUP_CODE %in% unique(CATDIS_data$CATEGORY)][order(GEAR_GROUP_CODE)]$FILL
    } else if(category == "SPECIES") {
      legend_label = "Species"
      fill_colors = SPECIES_COLORS[SPECIES %in% unique(CATDIS_data$CATEGORY)]$FILL
    } else if(category == "SCHOOL_TYPE") {
      legend_label = "School type"
      fill_colors = SCHOOL_TYPE_COLORS[SCHOOL_TYPE %in% unique(CATDIS_data$CATEGORY)]$FILL
    } else if(category == "QUARTER") {
      legend_label = "Quarter"
      fill_colors = QUARTER_COLORS[QUARTER %in% unique(CATDIS_data$CATEGORY)]$FILL
    }

    if(length(all_categories) == 1) {
      # Otherwise, for datasets with only one gear, the geom_scatterpie function will yield an error...
      CATDIS_data = rbind(CATDIS_data, data.table(GRID = "6100000", GRID_LON = 0, GRID_LAT = 0, CATEGORY = "foo", CATCH = 0))
    }

    if(input$pieCenter == "O") { # Pies should be centered in the grid ocean area (as stored in the 5x5 grid reference data)
      CATDIS_data =
        merge(
          CATDIS_data, as.data.table(GRIDS_SF)[, .(CODE, LON = CENTER_LON, LAT = CENTER_LAT)],
          by.x = "GRID", by.y = "CODE"
        )
    } else {                     # Pies should be centered in the theoretical grid center (as stored in the CATDIS data)
      CATDIS_data =
        merge(
          CATDIS_data[, .(GRID, LON = GRID_LON, LAT = GRID_LAT, CATEGORY, CATCH)], as.data.table(GRIDS_SF)[, .(CODE)],
          by.x = "GRID", by.y = "CODE"
        )
    }

    CATDIS_data_w =
      dcast.data.table(
        CATDIS_data,
        LON + LAT ~ CATEGORY,
        fun.aggregate = sum,
        value.var = "CATCH"
      )

    # Calculates the radius as the sum of catches of all categories for  a given pie
    CATDIS_data_w[, RADIUS     := rowSums(CATDIS_data_w[, 3:ncol(CATDIS_data_w)])]

    # Calculates the masimum radius either as either fixed value, or as a function of the maximum RADIUS value detected in the aggregate data
    max_radius = ifelse(input$catchScale == "Fixed", 10^input$catch, max(CATDIS_data_w$RADIUS))

    #CATDIS_data_w[, RADIUS_REL := input$radius * sqrt(RADIUS / max(RADIUS))]
    CATDIS_data_w[, RADIUS_REL := input$radius * sqrt(RADIUS / max_radius)]

    if(input$radius <= 1)     # Defines one pie legend break only if radius is <= 1
      pie_legend_breaks = c(input$radius)
    else if(input$radius < 3) # Defines two pie legend breaks if 1 < radius < 3
      pie_legend_breaks = c(0, input$radius)
    else                      # Otherwise defines three pie legend breaks if radius >= 3
      pie_legend_breaks = c(0, input$radius / sqrt(2), input$radius)

    # The coordinate system to enforce on the final plot
    c_sf = coord_sf(xlim = area_xlim,
                    ylim = area_ylim,
                    crs         = iccat.pub.maps::CRS_EQUIDISTANT,       # The Coordinate Reference System we want to use for display purposes...
                    default_crs = sf::st_crs(iccat.pub.maps::CRS_WGS84), # The Coordinate Reference System the simple features are set to by default
                    label_axes = "--EN", )

    c_sf$default = TRUE

    base_geometries = geometries_for(ATLANTIC_OCEAN_RAW_GEOMETRY,
                                     target_crs = iccat.pub.maps::CRS_EQUIDISTANT)

    if(!is.null(overlays))
      base_geometries = geometries_for(AREA_OVERLAYS[CODE %in% overlays], target_crs = iccat.pub.maps::CRS_EQUIDISTANT)

    piemap =
      map.atlantic(crs = iccat.pub.maps::CRS_EQUIDISTANT) +
        geom_sf( # Adds the outline of the selected overlays (ICCAT area by default)
          data = base_geometries,
          fill = "transparent",
          color = ifelse(is.null(overlays), "#00000044", "#000000CC")
        ) +

        geom_sf( # Fixed layer with all 5x5 grids shown in very light, transparent gray
          data = geometries_for(GRIDS_5x5_RAW_GEOMETRIES,
                                target_crs = iccat.pub.maps::CRS_EQUIDISTANT),
          fill = "transparent",
          color = "#88888822"
        ) +

        geom_scatterpie( # The actual scatter pie elements...
          data = CATDIS_data_w[order(+RADIUS_REL)],
          aes(x = LON,
              y = LAT,
              r = RADIUS_REL
          ),
          linewidth = .3,
          alpha     = .7,
          cols      = as.character(sort(unique(CATDIS_data$CATEGORY))),

          long_format = FALSE
        ) +

        geom_scatterpie_legend( # ...and their legend
          CATDIS_data_w$RADIUS_REL,
          x = legend_x,
          y = legend_y,
          labeller = function(x) {
            paste(prettyNum(round((x / input$radius) ^ 2 * max_radius), big.mark = ","), " t")
          },
          breaks = pie_legend_breaks,
          size = 2.5
        ) +

        scale_fill_manual(legend_label, values = fill_colors) +
        guides(
          fill = guide_legend(
            position = "right"
          )
        ) +

        theme(
          legend.justification = "top",
          legend.margin = margin(t = 1, unit = "cm"),
          legend.title  = element_text(size = 9),
          legend.text   = element_text(size = 9)
        ) +

        c_sf # Adds the coordinate system, including the xlim / ylim to ensure everything is shown properly

    # Extracts the legend (as a GROB) from the piemap...
    piemap_legend = get_legend(piemap)

    # .. then removes the legend from the piemap itself...
    piemap = piemap + theme(legend.position = "none")

    return(
      plot_grid(               # Creates a 2x1 grid to consistently show the map and its legend
        piemap, piemap_legend,
        rel_widths = c(5, 1.3) # Proportions: Map:legend = 5:1.3
      ) +

      theme(plot.background = element_rect(fill = "white", colour = NA))
    )
  }

  output$piemap =
    renderPlot(res = 120, {
      piemap()
    })

  heatmap = function() {
    CATDIS_data = validate_filtering(filter_catdis_data())

    if(nrow(CATDIS_data) == 0) return()

    area     = input$heatmapArea
    scale    = input$scale
    metric   = input$heatmapMetric

    overlays = input$heatmapAreaOverlay

    area_xlim = ATLANTIC_AREAS_LIMITS[area][[1]]$xlim
    area_ylim = ATLANTIC_AREAS_LIMITS[area][[1]]$ylim

    label = "Catches / year"
    label_unit = "t"

    # Computes the selected metric for each pie:

    if (metric == "AC")  {
      label = "Catches"
      # Total accumulation
      CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE)), keyby = .(GRID)]
    } else if(metric == "AV")  {
      # Annual average
      CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE) / (input$years[2] - input$years[1] + 1)), keyby = .(GRID)]
    } else if(metric == "AVC") {
      # Annual average (corrected)
      CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE) / length(unique(YEAR))), keyby = .(GRID)]
    }

    if(scale == "LN") { # Updates catch values to their corresponding log10
      label_unit = "log10(t)"
      CATDIS_data[, CATCH := log10(CATCH)]
    }

    CATDIS_data =
      merge(
        CATDIS_data, as.data.table(GRIDS_SF)[, .(CODE, GEOMETRY_WKT)],
        by.x = "GRID", by.y = "CODE"
      )

    # The coordinate system to enforce on the final plot
    c_sf = coord_sf(xlim = area_xlim,
                    ylim = area_ylim,
                    crs         = iccat.pub.maps::CRS_EQUIDISTANT,       # The Coordinate Reference System we want to use for display purposes...
                    default_crs = sf::st_crs(iccat.pub.maps::CRS_WGS84), # The Coordinate Reference System the simple features are set to by default
                    label_axes = "--EN", )

    c_sf$default = TRUE

    base_geometries = geometries_for(ATLANTIC_OCEAN_RAW_GEOMETRY,
                                     target_crs = iccat.pub.maps::CRS_EQUIDISTANT)

    if(!is.null(overlays))
      base_geometries = geometries_for(AREA_OVERLAYS[CODE %in% overlays], target_crs = iccat.pub.maps::CRS_EQUIDISTANT)

    heatmap =
      map.atlantic(crs = iccat.pub.maps::CRS_EQUIDISTANT) +
        geom_sf( # Adds the outline of the selected overlays (ICCAT area by default)
          data = base_geometries,
          fill = "transparent",
          color = ifelse(is.null(overlays), "#00000044", "#000000CC")
        ) +

        geom_sf( # Fixed layer with all 5x5 grids shown in very light, transparent gray
          data = geometries_for(GRIDS_5x5_RAW_GEOMETRIES, target_crs = CRS_EQUIDISTANT),
          fill = "transparent",
          color = "#88888822"
        ) +

        geom_sf( # The actual heatmap elements, as a series of 5x5 grids with different color intensity (proportional to their catch)
          data = CATDIS_data,
          aes(
            geometry = GEOMETRY_WKT,
            fill = CATCH
          ),
          color = "#00000022",
          alpha = .6
        ) +

        scale_fill_gradient( # The gradient scale
          low   = "#FFFFFF",
          high  = "#0000AA",
          guide = "legend",
          labels = scales::comma
        ) +

        guides(
          fill = guide_legend(
            title = paste0(label, " [ ", label_unit, " ]"),
            position = "right"
          )
        ) +

        theme(
          legend.justification = "top",
          legend.margin = margin(t = 1, unit = "cm"),
          legend.title  = element_text(size = 9),
          legend.text   = element_text(size = 9)
        ) +

        c_sf # Adds the coordinate system, including the xlim / ylim to ensure everything is shown properly

    # Extracts the legend (as a GROB) from the heatmap...
    heatmap_legend = get_legend(heatmap)

    # .. then removes the legend from the heatmap itself...
    heatmap = heatmap + theme(legend.position = "none")

    return(
      plot_grid(               # Creates a 2x1 grid to consistently show the map and its legend
        heatmap, heatmap_legend,
        rel_widths = c(5, 1.3) # Proportions: Map:legend = 5:1.3
      ) +

      theme(plot.background = element_rect(fill = "white", colour = NA))
    )
  }

  output$heatmap =
    renderPlot(res = 120, {
      heatmap()
    })

  output$rawData =
    renderDataTable({
      filtered_data = validate_filtering(filter_catdis_data())

      filtered_data = filtered_data[, .(YEAR,
                                        QUARTER,
                                        FLAG,
                                        FLEET,
                                        GEAR_GROUP,
                                        SCHOOL_TYPE,
                                        SPECIES,
                                        STOCK,
                                        SAMPLING_AREA,
                                        GRID,
                                        GRID_LON,
                                        GRID_LAT,
                                        GRID_CENTROID_LON,
                                        GRID_CENTROID_LAT,
                                        CATCH)]

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
          colnames = c("Year", "Quarter",
                       "Flag name", "Fleet code",
                       "Gear group", "School type",
                       "Species", "Stock",
                       "Sampling area",
                       "Grid",
                       "Center lon.", "Center lat.", "Centroid lon.", "Centroid lat.",
                       "Catch (t)")
        )
        %>% DT::formatCurrency(columns = c("GRID_LON", "GRID_LAT", "GRID_CENTROID_LON", "GRID_CENTROID_LAT"), currency = "")
        %>% DT::formatCurrency(columns = c("CATCH"), currency = "")
      )
    })

  get_filename_components = function(input) { # Common part of the filename, built using the selected filters
    quarters_labels = paste0("Q", sort(input$quarters))

    components = c(paste0(input$years,         collapse = "-"),
                   paste0(quarters_labels,     collapse = "+"),
                   paste0(input$species,       collapse = "+"),
                   paste0(input$stocks,        collapse = "+"),
                   paste0(input$flags,         collapse = "+"),
                   paste0(input$fleets,        collapse = "+"),
                   paste0(input$gearGroups,    collapse = "+"),
                   paste0(input$gears,         collapse = "+"),
                   paste0(input$schoolTypes,   collapse = "+"),
                   paste0(input$samplingAreas, collapse = "+"))

    if(input$dataset == TAB_PIEMAP)       components = append(components, input$piemapArea)  # Additional filename components for the piemap
    else if(input$dataset == TAB_HEATMAP) components = append(components, input$heatmapArea) # Additional filename components for the heatmap
    # Nothing specific shall be added in case the selected tab is the one for the data table

    # Removes all unset filename components...
    components = components[which(components != "")]

    # Returns all valid filename components separated by an underscore ("_")
    return(paste0(components, collapse = "_"))
  }

  get_filename_components_piemap = function(input) {
    components = get_filename_components(input)

    return( # Adds the category and metrics as specific components of the piemap filename
      paste0(components, "_", input$piemapCategory, "_", input$metricPie)
    )
  }

  get_filename_components_heatmap = function(input) {
    components = get_filename_components(input)

    return( # Adds the scale and metrics as specific components of the heatmap filename
      paste0(components, "_", input$scale, "_", input$metricHeat)
    )
  }

  serialize_last_update_date = function() {
    return(
      str_replace_all(META$LAST_UPDATE, "\\-", "")
    )
  }

  output$downloadFiltered = downloadHandler(
    filename = function() { # Builds the downloaded file name on the basis of the selected tab and the various components (filters, etc.)
      dataset = input$dataset

      filename_prefix = paste0("ICCAT_CATDIS_", serialize_last_update_date())

      if(dataset == TAB_PIEMAP)       # If the selected tab is the one for the piemap, downloads the piemap as an image
        return(paste0(filename_prefix, "_piemap_",  get_filename_components_piemap (input), ".png"))
      else if(dataset == TAB_HEATMAP) # If the selected tab is the one for the heatmap, downloads the heatmap as an image
        return(paste0(filename_prefix, "_heatmap_", get_filename_components_heatmap(input), ".png"))
      else                            # Otherwise, the selected tab is the one for the data table, and therefore downloads the dataset as a zipped CSV file
        return(paste0(filename_prefix, "_data_",    get_filename_components        (input), ".csv.gz"))
    },
    content = function(file) {
      dataset = input$dataset

      if(dataset == TAB_PIEMAP)       # If the selected tab is the one for the piemap, downloads the piemap as an image
        ggsave(filename = file, piemap()  + annotation_custom(grob = ICCAT_LOGO_RASTER), width = 10, height = 8)
      else if(dataset == TAB_HEATMAP) # If the selected tab is the one for the heatmap, downloads the heatmap as an image
        ggsave(filename = file, heatmap() + annotation_custom(grob = ICCAT_LOGO_RASTER), width = 10, height = 8)
      else                            # Otherwise, the selected tab is the one for the data table, and therefore downloads the dataset as a zipped CSV file
        write.csv(filter_catdis_data(), gzfile(file), row.names = FALSE, na = "")
    }
  )
}
