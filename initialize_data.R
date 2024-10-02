library(iccat.dev.data)
library(iccat.pub.maps)

CATDIS_SPECIES = c("ALB", "BFT",
                   "BET", "SKJ", "YFT",
                   "SWO",
                   "BUM", "SAI", "WHM")

CATDIS =
  catdis(CATDIS_SPECIES)

CATDIS$SpeciesCode =
  factor(
    CATDIS$SpeciesCode,
    levels = CATDIS_SPECIES,
    labels = CATDIS_SPECIES,
    ordered = TRUE
  )

CATDIS$SchoolType =
  factor(
    CATDIS$SchoolType,
    levels = c("FAD", "FSC", "n/a"),
    labels = c("FAD", "FSC", "n/a"),
    ordered = TRUE
  )

CATDIS =
  merge(
    CATDIS, GRIDS_5x5_RAW_GEOMETRIES[, .(CODE, GRID_CENTROID_LON = CENTER_LON, GRID_CENTROID_LAT = CENTER_LAT)],
    by.x = "CWPCode", by.y = "CODE"
  )

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

META = list(LAST_UPDATE = "2024-09-30",
            FILENAME_Y = "ICCAT_CATDIS_20240930_Y.csv.gz",
            FILENAME_Q = "ICCAT_CATDIS_20240930_Q.csv.gz")

save("META",     file = "./shiny/META.RData", compress = "gzip")
save("CATDIS_Y", file = "./shiny/CATDIS_Y.RData",   compress = "gzip")
save("CATDIS_Q", file = "./shiny/CATDIS_Q.RData",   compress = "gzip")

write.table(CATDIS_Y, file = gzfile(paste0("./shiny/www/", META$FILENAME_Y)), sep = ",", na = "", row.names = FALSE)
write.table(CATDIS_Q, file = gzfile(paste0("./shiny/www/", META$FILENAME_Q)), sep = ",", na = "", row.names = FALSE)
