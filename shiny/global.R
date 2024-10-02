library(iccat.pub.base)
library(iccat.pub.data)
library(iccat.pub.maps)
library(iccat.pub.aes)

library(scatterpie)
library(cowplot)

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(htmltools)

library(stringr)

options(scipen = 9999)

# THIS IS ***FUNDAMENTAL*** TO HAVE THE DOCKER CONTAINER CORRECTLY LOAD THE .RData FILE WITH THE ORIGINAL UTF-8 ENCODING
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

TAB_PIEMAP   = "Piemap"
TAB_HEATMAP  = "Heatmap"
TAB_RAW_DATA = "Tabular data"

INITIAL_NUM_ENTRIES  = 50

load("./META.RData")
load("./CATDIS_Y.RData")

CATDIS_flags       = REF_FLAGS[NAME_EN %in% unique(CATDIS_Y$FLAG)]
CATDIS_fleets      = REF_FLEETS[CODE %in% unique(CATDIS_Y$FLEET)]
CATDIS_gear_groups = REF_GEAR_GROUPS[CODE %in% unique(CATDIS_Y$GEAR_GROUP)]

ALL_SPECIES_CODES = c("ALB", "BFT",
                      "BET", "SKJ", "YFT",
                      "SWO",
                      "BUM", "SAI", "WHM")

ALL_SPECIES_DATA = REF_SPECIES[CODE %in% ALL_SPECIES_CODES]
ALL_SPECIES_DATA$CODE =
  factor(
    ALL_SPECIES_DATA$CODE,
    labels = ALL_SPECIES_CODES,
    levels = ALL_SPECIES_CODES,
    ordered = TRUE
  )

ALL_SPECIES_DATA = ALL_SPECIES_DATA[order(CODE)]

ALL_FLAGS          = setNames(as.character(CATDIS_flags$NAME_EN),    paste0(CATDIS_flags$CODE,       " - ", CATDIS_flags$NAME_EN))
ALL_FLEETS         = setNames(as.character(CATDIS_fleets$CODE),      paste0(CATDIS_fleets$CODE,      " - ", CATDIS_fleets$NAME_EN))
ALL_GEAR_GROUPS    = setNames(as.character(CATDIS_gear_groups$CODE), paste0(CATDIS_gear_groups$CODE, " - ", CATDIS_gear_groups$NAME_EN))

ALL_FISHING_MODES  = setNames(c("FSC", "FAD", "n/a"),
                              c("FSC - Free-swimming schools", "FAD - Log-associated schools", "n/a - Unknown / Unavailable"))

ALL_SPECIES        = setNames(ALL_SPECIES_DATA$CODE, paste0(ALL_SPECIES_DATA$CODE, " - ", ALL_SPECIES_DATA$NAME_EN))
ALL_STOCKS         = sort(unique(CATDIS_Y$STOCK))
ALL_SAMPLING_AREAS = sort(unique(CATDIS_Y$SAMPLING_AREA))

SPECIES_COLORS =
  data.table(
    SPECIES = ALL_SPECIES_CODES,
    FILL    = colorRampPalette(brewer.pal(n = length(ALL_SPECIES_CODES), name = "Paired"))(length(ALL_SPECIES_CODES))
  )

SCHOOL_TYPE_COLORS =
  data.table(
    SCHOOL_TYPE = c("FAD",    "FSC", "n/a"),
    FILL        = c("yellow", "red", "gray")
  )

GRIDS_SF = geometries_for(GRIDS_5x5_RAW_GEOMETRIES)

PIEMAP_CATEGORIES = setNames(c("Gear group", "Species"), c("GearGroup", "Species"))

DEFAULT_RADIUS = pi

UI_select_input = function(id, label, choices) {
  return(
    virtualSelectInput(
      inputId = id,
      label = label,
      width = "100%",
      multiple = TRUE,
      autoSelectFirstOption = FALSE,
      choices = choices,
      search = TRUE,
      showValueAsTags = FALSE,
      updateOn = "close"
    )
  )
}

set_log_level(LOG_INFO)

MIN_YEAR = 1950 #min(CA_ALL$Year)
MAX_YEAR = max(CATDIS_Y$YEAR)

INFO(paste0(nrow(CATDIS_Y), " rows loaded from CATDIS_Y"))
