library(iccat.dev.data)
library(iccat.pub.maps)

# A connection to the database holding the CATDIS data
DB_CATDIS_CURRENT = DB_CATDIS()

# A connection to the database holding the GIS data
DB_GIS_CURRENT = DB_GIS(server = "ATENEA\\SQL22")

# The list of species codes for which CATDIS data is produced

CATDIS_SPECIES = c("ALB", "BFT",
                   "BET", "SKJ", "YFT",
                   "SWO",
                   "BUM", "SAI", "WHM")

# Retrieves the latest CATDIS data

CATDIS =
  catdis(CATDIS_SPECIES, db_connection = DB_CATDIS_CURRENT)

# Factorises the quarter

CATDIS$Trimester =
  factor(
    CATDIS$Trimester,
    levels = c( 1,              2,              3,              4),
    labels = c("Q1 (Jan-Mar)", "Q2 (Apr-Jun)", "Q3 (Jul-Sep)", "Q4 (Oct-Dec)"),
    ordered = TRUE
  )

# Factorises the species code

CATDIS$SpeciesCode =
  factor(
    CATDIS$SpeciesCode,
    levels = CATDIS_SPECIES,
    labels = CATDIS_SPECIES,
    ordered = TRUE
  )

# Factorises the school type code

CATDIS$SchoolType =
  factor(
    CATDIS$SchoolType,
    levels = c("FAD", "FSC", "n/a"),
    labels = c("FAD", "FSC", "n/a"),
    ordered = TRUE
  )

# Merges the CATDIS data with the georeferenced grid data

CATDIS =
  merge(
    CATDIS, GRIDS_5x5_RAW_GEOMETRIES[, .(CODE, GRID_CENTROID_LON = CENTER_LON, GRID_CENTROID_LAT = CENTER_LAT)],
    by.x = "CWPCode", by.y = "CODE"
  )

# Creates a standardised quarterly version of the CATDIS data

CATDIS_Q =
  CATDIS[, .(YEAR = YearC,
             QUARTER = Trimester,
             FLAG = FlagName,
             FLEET = FleetCode,
             GEAR_GROUP = GearGrp,
             SCHOOL_TYPE = SchoolType,
             SPECIES = SpeciesCode,
             STOCK = Stock,
             SAMPLING_AREA = SampAreaCode,
             GRID = CWPCode,
             GRID_LON = xLon5ctoid,
             GRID_LAT = yLat5ctoid,
             GRID_CENTROID_LON,
             GRID_CENTROID_LAT,
             CATCH = round(Catch_t, 2))]

# Creates a standardised annual version of the CATDIS data

CATDIS_Y =
  CATDIS_Q[, .(CATCH = sum(CATCH, na.rm = TRUE)),
           keyby = .(YEAR,
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
                     GRID_CENTROID_LAT)]

# Creates an updated version of the metadata

META = list(LAST_UPDATE = "2024-10-15",
            FILENAME_Y = "ICCAT_CATDIS_20241015_Y.csv.gz",
            FILENAME_Q = "ICCAT_CATDIS_20241015_Q.csv.gz")

# Saves the two CATDIS datasets (quarterly and annual) and the metadata themselves as R data frames

save("META",     file = "./shiny/META.RData", compress = "gzip")
save("CATDIS_Y", file = "./shiny/CATDIS_Y.RData",   compress = "gzip")
save("CATDIS_Q", file = "./shiny/CATDIS_Q.RData",   compress = "gzip")

# Writes the two CATDIS datasets (quarterly and annual) as compressed CSV files

write.table(CATDIS_Y, file = gzfile(paste0("./shiny/www/", META$FILENAME_Y)), sep = ",", na = "", row.names = FALSE)
write.table(CATDIS_Q, file = gzfile(paste0("./shiny/www/", META$FILENAME_Q)), sep = ",", na = "", row.names = FALSE)

# To retrieve raw geometries (by code) from the GIS database...
raw_geometries_for = function(area_codes, db_connection = DB_GIS_CURRENT) {
  return(
    tabular_query(
      db_connection,
      paste0("
        SELECT
          CODE,
          TYPE_CODE,
          NAME_EN,
          NAME_ES,
          NAME_FR,
          SURFACE_IN_ICCAT_AREA,
         (GEOMETRY_CUT.MakeValid()).STAsText() AS GEOMETRY_WKT
        FROM
          [AREAS]
        WHERE
          CODE IN (", paste(shQuote(area_codes, type = "sh"), collapse=", "), ")"
      )
    )
  )
}

# To retrieve raw geometries (by area type code) from the GIS database...
raw_geometries_by_type = function(type_code, db_connection = DB_GIS_CURRENT) {
  return(
    tabular_query(
      db_connection,
      paste0("
        SELECT
          CODE,
          TYPE_CODE,
          NAME_EN,
          NAME_ES,
          NAME_FR,
          SURFACE_IN_ICCAT_AREA,
         (GEOMETRY_CUT.MakeValid()).STAsText() AS GEOMETRY_WKT
        FROM
          [AREAS]
        WHERE
          TYPE_CODE = '", type_code, "'"
      )
    )
  )
}

# Retrieves all geometries for the FAO major areas
FAO_MAJOR_AREAS =
  raw_geometries_by_type("FAO_MAJOR")

# Retrieves all geometries for the ICCAT subareas
ICCAT_SUBAREAS =
  raw_geometries_for(
    c("MD",
      "AT",
      "AT-N",
      "AT-S",
      "AT-W",
      "AT-E",
      "AT-NW",
      "AT-NE",
      "AT-SW",
      "AT-SE")
  )

# Saves the FAO major areas and ICCAT subareas as R data frames
save("FAO_MAJOR_AREAS", file = "./shiny/FAO_MAJOR_AREAS.RData", compress = "gzip")
save("ICCAT_SUBAREAS",  file = "./shiny/ICCAT_SUBAREAS.RData",  compress = "gzip")

# Retrieves the mapping from the 5x5 grids to the sampling areas
GRID_TO_SAMPLING_AREA_MAPPINGS =
  tabular_query(
    connection = DB_GIS_CURRENT,
    statement  = "
      SELECT
      	S.CODE AS GRID_CODE,
      	T.CODE AS SAMPLING_AREA_CODE,
      	AI.SOURCE_AREA_OVERLAP_PERCENTAGE AS OVERLAPPING_PERCENTAGE
      FROM
        AREAS S
      INNER JOIN
        AREA_INTERSECTIONS AI
      ON
        S.CODE = AI.SOURCE_AREA_CODE
      INNER JOIN
        AREAS T
      ON
        T.CODE = AI.TARGET_AREA_CODE
      WHERE
      	S.TYPE_CODE = 'GRID05x05' AND
      	T.TYPE_CODE = 'SAMPLING_AREA'
      "
  )

# Saves the grid - sampling area mappings as a R data frame
save("GRID_TO_SAMPLING_AREA_MAPPINGS", file = "./shiny/GRID_TO_SAMPLING_AREA_MAPPINGS.RData", compress = "gzip")
