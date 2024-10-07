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
load("./CATDIS_Q.RData")
load("./FAO_MAJOR_AREAS.RData")
load("./ICCAT_SUBAREAS.RData")

AREA_OVERLAYS = as.data.table(FAO_MAJOR_AREAS)
AREA_OVERLAYS = rbind(AREA_OVERLAYS, as.data.table(ICCAT_SUBAREAS))

INFO(paste0(nrow(CATDIS_Q), " rows loaded from CATDIS_Q"))

DEFAULT_PIECHART_LEGEND_X = -90
DEFAULT_PIECHART_LEGEND_Y = -25

CATDIS_Q$QUARTER =
  factor(
    CATDIS_Q$QUARTER,
    levels = c( 1,              2,               3,              4),
    labels = c("Q1 (Jan-Mar)", "Q2 (Apr-Jun)", "Q3 (Jul-Sep)", "Q4 (Oct-Dec)"),
    ordered = TRUE
  )

MIN_YEAR = min(CATDIS_Q$YEAR)
MAX_YEAR = max(CATDIS_Q$YEAR)

### BUILDING REFERENCE DATA FOR UI COMPONENTS

ALL_QUARTERS = setNames(c(1, 2, 3, 4),
                        as.character(sort(unique(CATDIS_Q$QUARTER))))

CATDIS_flags       = REF_FLAGS[NAME_EN %in% unique(CATDIS_Q$FLAG)]
CATDIS_fleets      = REF_FLEETS[CODE %in% unique(CATDIS_Q$FLEET)]
CATDIS_gear_groups = REF_GEAR_GROUPS[CODE %in% unique(CATDIS_Q$GEAR_GROUP)]

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

ALL_STOCKS = sort(unique(CATDIS_Q$STOCK))

SAMPLING_AREAS = sort(unique(CATDIS_Q$SAMPLING_AREA))

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
                             c("GearGroup",  "Species"))

ALL_ATLANTIC_AREAS = setNames(
  c("AT",
    "AT-NW",
    "AT-NE",
    "MD",
    "AT-WT",
    "AT-ET",
    "AT-SW",
    "AT-SE"),
  c("AT - All Atlantic ocean",
    "AT-NW - Northwest Atlantic",
    "AT-NE - Northeast Atlantic",
    "MD - Mediterranean sea",
    "AT-WT - Western tropical area",
    "AT-ET - Eastern tropical area",
    "AT-SW - Southwest Atlantic",
    "AT-SE - Southeast Atlantic")
)

ATLANTIC_AREAS_LIMITS =
  list(
    "AT"    = list(xlim = DEFAULT_XLIM,  ylim = DEFAULT_YLIM,  legend_x = -90, legend_y = -50),
    "AT-NW" = list(xlim = c(-100,  -30), ylim = c(   5,   75), legend_x = -95, legend_y =  10),
    "AT-NE" = list(xlim = c( -30,   40), ylim = c(   5,   75), legend_x =  20, legend_y =  10),
    "MD"    = list(xlim = c( -10,   45), ylim = c(  20,   75), legend_x =  -5, legend_y =  25),
    "AT-WT" = list(xlim = c( -75,  -30), ylim = c( -20,   25), legend_x = -70, legend_y = -15),
    "AT-ET" = list(xlim = c( -30,   15), ylim = c( -20,   25), legend_x =   5, legend_y =  20),
    "AT-SW" = list(xlim = c( -95,  -30), ylim = c(   5,  -60), legend_x = -90, legend_y = -55),
    "AT-SE" = list(xlim = c( -30,   35), ylim = c(   5,  -60), legend_x = -25, legend_y = -55)
  )

ALL_FAO_MAJOR_AREAS_OVERLAYS =
  setNames(
    FAO_MAJOR_AREAS$CODE, paste0(str_replace_all(FAO_MAJOR_AREAS$CODE, "_", "-"), " - ", FAO_MAJOR_AREAS$NAME_EN)
  )

ALL_ICCAT_SUBAREAS_OVERLAYS =
  setNames(
    ICCAT_SUBAREAS$CODE, paste0(ICCAT_SUBAREAS$CODE, " - ", ICCAT_SUBAREAS$NAME_EN)
  )

ALL_AREAS_OVERLAYS =
  list(
    "FAO major areas" = ALL_FAO_MAJOR_AREAS_OVERLAYS,
    "ICCAT subareas"  = ALL_ICCAT_SUBAREAS_OVERLAYS
  )

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

QUARTER_COLORS =
  data.table(
    QUARTER  = c("Q1 (Jan-Mar)", "Q2 (Apr-Jun)", "Q3 (Jul-Sep)", "Q4 (Oct-Dec)"),
    FILL     = c("purple", "green", "orange", "blue")
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
                                     width = unit(0.07, "npc"),
                                     x = unit(0.08, "npc"), y = unit(.98, "npc"),
                                     hjust = -9, vjust = 12.4)
