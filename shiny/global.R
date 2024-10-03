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

### INITIALIZATION

options(scipen = 9999)

# THIS IS ***FUNDAMENTAL*** TO HAVE THE DOCKER CONTAINER CORRECTLY LOAD THE .RData FILE WITH THE ORIGINAL UTF-8 ENCODING
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

set_log_level(LOG_INFO)

load("./META.RData")
load("./CATDIS_Y.RData")

MIN_YEAR = 1950 #min(CA_ALL$Year)
MAX_YEAR = max(CATDIS_Y$YEAR)

INFO(paste0(nrow(CATDIS_Y), " rows loaded from CATDIS_Y"))

### BUILDING REFERENCE DATA FOR UI COMPONENTS

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

SP_TEMPERATE = ALL_SPECIES_DATA[CODE %in% c("ALB", "BFT")]
SP_TROPICAL  = ALL_SPECIES_DATA[CODE %in% c("BET", "SKJ", "YFT")]
SP_BILLFISH  = ALL_SPECIES_DATA[CODE %in% c("SWO", "BUM", "SAI", "WHM")]

ALL_SPECIES = list(
  "Temperate tunas" = setNames(as.character(SP_TEMPERATE$CODE),    paste0(SP_TEMPERATE$CODE,    " - ", SP_TEMPERATE$NAME_EN)),
  "Tropical tunas"  = setNames(as.character(SP_TROPICAL$CODE),     paste0(SP_TROPICAL$CODE,     " - ", SP_TROPICAL$NAME_EN)),
  "Billfish"        = setNames(as.character(SP_BILLFISH$CODE),     paste0(SP_BILLFISH$CODE,     " - ", SP_BILLFISH$NAME_EN))
)

#ALL_SPECIES        = setNames(ALL_SPECIES_DATA$CODE, paste0(ALL_SPECIES_DATA$CODE, " - ", ALL_SPECIES_DATA$NAME_EN))
ALL_STOCKS         = sort(unique(CATDIS_Y$STOCK))

SAMPLING_AREAS = sort(unique(CATDIS_Y$SAMPLING_AREA))

SA_ALB = SAMPLING_AREAS[which(str_sub(SAMPLING_AREAS, 1, 2) == "AL")]
SA_BFT = SAMPLING_AREAS[which(str_sub(SAMPLING_AREAS, 1, 2) == "BF")]
SA_BET = SAMPLING_AREAS[which(str_sub(SAMPLING_AREAS, 1, 2) == "BE")]
SA_SKJ = SAMPLING_AREAS[which(str_sub(SAMPLING_AREAS, 1, 2) == "SJ")]
SA_YFT = SAMPLING_AREAS[which(str_sub(SAMPLING_AREAS, 1, 2) == "YF")]
SA_BIL = SAMPLING_AREAS[which(str_sub(SAMPLING_AREAS, 1, 3) == "BIL")]

ALL_SAMPLING_AREAS = list(
  "Albacore tuna"         = SA_ALB,
  "Northern bluefin tuna" = SA_BFT,
  "Bigeye tuna"           = SA_BET,
  "Skipjack tuna"         = SA_SKJ,
  "Yellowfin tuna"        = SA_YFT,
  "Billfish"              = SA_BIL
)

PIEMAP_CATEGORIES = setNames(c("Gear group", "Species"),
                             c("GearGroup", "Species"))

### BUILDING CUSTOM REFERENCE COLORS

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

### UI COMPONENTS CONFIGURATION

DEFAULT_RADIUS = pi

TAB_PIEMAP   = "Piemap"
TAB_HEATMAP  = "Heatmap"
TAB_RAW_DATA = "Tabular data"

INITIAL_NUM_ENTRIES  = 50

GRIDS_SF = geometries_for(GRIDS_5x5_RAW_GEOMETRIES)

UI_select_input = function(id, label, placeholder = "Select", choices) {
  return(
    virtualSelectInput(
      inputId = id,
      label = label,
      placeholder = placeholder,
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

### ICCAT LOGO

ICCAT_LOGO = png::readPNG("./www/iccat-logo.png")
ICCAT_LOGO_RASTER = grid::rasterGrob(ICCAT_LOGO, interpolate = TRUE,
                                     #width = unit(75, "points"),
                                     width = unit(0.07, "npc"),
                                     #x = unit(.09, "npc"), y = unit(.98, "npc"),
                                     x = unit(0.08, "npc"), y = unit(.98, "npc"),
                                     hjust = -9, vjust = 12.4)
