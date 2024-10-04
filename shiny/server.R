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

    area_xlim = ATLANTIC_AREAS_LIMITS[area][[1]]$xlim
    area_ylim = ATLANTIC_AREAS_LIMITS[area][[1]]$ylim

    colnames(CATDIS_data)[which(colnames(CATDIS_data) == category)] = "CATEGORY"

    if     (metric == "AC")  { CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE)),                                         keyby = .(GRID, GRID_LON, GRID_LAT, CATEGORY)] }
    else if(metric == "AV")  { CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE) / (input$years[2] - input$years[1] + 1)), keyby = .(GRID, GRID_LON, GRID_LAT, CATEGORY)] }
    else if(metric == "AVC") { CATDIS_data = CATDIS_data[, .(CATCH = sum(CATCH, na.rm = TRUE) / length(unique(YEAR))),                  keyby = .(GRID, GRID_LON, GRID_LAT, CATEGORY)] }

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

    CATDIS_data_w[, RADIUS     := rowSums(CATDIS_data_w[, 3:ncol(CATDIS_data_w)])]

    max_radius = ifelse(input$catchScale == "Fixed", 10^input$catch, max(CATDIS_data_w$RADIUS))

    #CATDIS_data_w[, RADIUS_REL := input$radius * sqrt(RADIUS / max(RADIUS))]
    CATDIS_data_w[, RADIUS_REL := input$radius * sqrt(RADIUS / max_radius)]

    if(input$radius <= 1)
      pie_legend_breaks = c(input$radius)
    else if(input$radius < 3)
      pie_legend_breaks = c(0, input$radius)
    else
      pie_legend_breaks = c(0, input$radius / sqrt(2), input$radius)

    c_sf = coord_sf(xlim = area_xlim,
                    ylim = area_ylim,
                    crs = iccat.pub.maps::CRS_EQUIDISTANT,
                    default_crs = sf::st_crs(iccat.pub.maps::CRS_WGS84),
                    label_axes = "--EN", )

    c_sf$default = TRUE

    pie =
      map.atlantic(crs = iccat.pub.maps::CRS_EQUIDISTANT) +

        geom_sf( # Adds the outline of the ICCAT area
          data = geometries_for(ATLANTIC_OCEAN_RAW_GEOMETRY,
                                target_crs = iccat.pub.maps::CRS_EQUIDISTANT),
          fill = "transparent",
          color = "#00000044"
        ) +

        geom_sf(
          data = geometries_for(GRIDS_5x5_RAW_GEOMETRIES,
                                target_crs = iccat.pub.maps::CRS_EQUIDISTANT),
          fill = "transparent",
          color = "#88888822"
        ) +

        geom_scatterpie(
          data = CATDIS_data_w,
          aes(x = LON,
              y = LAT,
              r = RADIUS_REL
          ),
          linewidth = .3,
          alpha     = .7,
          cols      = as.character(sort(unique(CATDIS_data$CATEGORY))),

          long_format = FALSE
        ) +

        geom_scatterpie_legend(
          CATDIS_data_w$RADIUS_REL,
          x = -90,
          y = -25,
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

        c_sf

    pie_legend = get_legend(pie)

    pie = pie + theme(legend.position = "none")

    return(
      plot_grid(
        pie, pie_legend,
        rel_widths = c(5, 1.3)
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

    area_xlim = ATLANTIC_AREAS_LIMITS[area][[1]]$xlim
    area_ylim = ATLANTIC_AREAS_LIMITS[area][[1]]$ylim

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

    c_sf = coord_sf(xlim = area_xlim,
                    ylim = area_ylim,
                    crs = iccat.pub.maps::CRS_EQUIDISTANT,
                    default_crs = sf::st_crs(iccat.pub.maps::CRS_WGS84),
                    label_axes = "--EN", )

    c_sf$default = TRUE

    heat =
      map.atlantic(crs = iccat.pub.maps::CRS_EQUIDISTANT) +
        #geom_sf(
        #  data = geometries_for(ATLANTIC_OCEAN_RAW_GEOMETRY, target_crs = CRS_EQUIDISTANT),
        #  fill = "transparent",
        #  color = "#00000044"
        #) +

        geom_sf(
          data = geometries_for(GRIDS_5x5_RAW_GEOMETRIES, target_crs = CRS_EQUIDISTANT),
          fill = "transparent",
          color = "#88888822"
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

        c_sf

    heat_legend = get_legend(heat)

    heat = heat + theme(legend.position = "none")

    return(
      plot_grid(
        heat, heat_legend,
        rel_widths = c(5, 1.3)
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

  get_filename_components = function(input) {
    components = c(paste0(input$years,         collapse = "-"),
                   paste0(paste0("Q", sort(input$quarters)), collapse = "+"),
                   paste0(input$species,       collapse = "+"),
                   paste0(input$stocks,        collapse = "+"),
                   paste0(input$flags,         collapse = "+"),
                   paste0(input$fleets,        collapse = "+"),
                   paste0(input$gearGroups,    collapse = "+"),
                   paste0(input$gears,         collapse = "+"),
                   paste0(input$schoolTypes,   collapse = "+"),
                   paste0(input$samplingAreas, collapse = "+"))

    if(input$dataset == TAB_PIEMAP) components = append(components, input$piemapArea)
    else if(input$dataset == TAB_HEATMAP) components = append(components, input$heatmapArea)

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
      dataset = input$dataset

      filename_prefix = paste0("ICCAT_CATDIS_", serialize_last_update_date())

      if(dataset == TAB_PIEMAP)
        return(paste0(filename_prefix, "_piemap_",  get_filename_components_piemap (input), ".png"))
      else if(dataset == TAB_HEATMAP)
        return(paste0(filename_prefix, "_heatmap_", get_filename_components_heatmap(input), ".png"))
      else
        return(paste0(filename_prefix, "_data_",    get_filename_components        (input), ".csv.gz"))
    },
    content = function(file) {
      dataset = input$dataset

      if(dataset == TAB_PIEMAP)
        ggsave(filename = file, piemap()  + annotation_custom(grob = ICCAT_LOGO_RASTER), width = 10, height = 8)
      else if(dataset == TAB_HEATMAP)
        ggsave(filename = file, heatmap() + annotation_custom(grob = ICCAT_LOGO_RASTER), width = 10, height = 8)
      else
        write.csv(filter_catdis_data(), gzfile(file), row.names = FALSE, na = "")
    }
  )
}
