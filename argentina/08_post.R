source("00_globalVariables.R")
#-----------------------------
# (8) Post-estimation

net      <- readRDS("network/net.rds")                       #
roads    <- st_transform(readRDS("network/roads.rds"), crs)  # I already included OSM
osm_name <- "fclass"                                         # OSM column for type of road (motorway, trunk...)
grid     <- st_transform(eixport::wrf_grid(paste0(system.file("extdata", package = "eixport"), "/wrfinput_d01")), crs)


# df
df   <- rbind(fread("emi/exhaust.csv"), fread("emi/evaporatives.csv"))
df$emissions <- units::set_units(df$emissions, "g")
df$t <- units::set_units(df$emissions, "t")

#---
# streets  ####
cat("Distributing emissions on streets\n")
dt <- df[pollutant != "NMHC", sum(emissions, na.rm = T), by = .(month, pollutant)]
dt$fuel <- "all"
dt$type_emi <- "Exhaust"
dtev <- df[pollutant == "NMHC", sum(emissions, na.rm = T), by = .(month, pollutant, fuel, type_emi)]

#DT
DT <- data.table(
	month = c(dt$month, dtev$month),
        pollutant = c(dt$pollutant, dtev$pollutant),
        fuel = c(dt$fuel, dtev$fuel),
        type_emi = c(dt$type_emi, dtev$type_emi),
        g = c(dt$V1, dtev$V1))

DT       <- DT[month %in% months_subset]
DT$month <- as.character(ifelse(nchar(DT$month) < 2, paste0("0", DT$month), DT$month))
DT$TY    <- ifelse(DT$type_emi == "Exhaust", "EXH", "EVA")
DT$PFTM  <- paste(DT$pollutant, DT$fuel, DT$TY, DT$month, sep = "_")

roads$length <- st_length(roads)
roads$lengthHDV <- ifelse(roads[[osm_name]] %in% c( "tertiary", "secondary", "primary"), 0, roads$length)

for (i in seq_along(DT$PFTM)) {
    roads[[DT$PFTM[i]]] <- DT[PFTM == DT$PFTM[i]]$g * roads$length / sum(roads$length)
}

# By default distributing NO,NO2, NOx only one trunks and motorway
names_nox <- c(  grep("NO_", DT$PFTM, value = T),  grep("NO2_", DT$PFTM, value = T),  grep("NOx_", DT$PFTM, value = T) )
for (i in seq_along(names_nox)) {
    roads[[names_nox[i]]] <- DT[PFTM == names_nox[i]]$g * roads$lengthHDV / sum(roads$lengthHDV)
}

# grids ####
x <- roads[, DT$PFTM]

#
cat("Cropping streets for grid extent\n")
x <- st_crop(x, st_as_sfc(st_bbox(g)))

#
cat("Gridding emissions\n")
gx <- emis_grid(spobj = x, g = grid)

#
cat("Saving emissions\n")
suppressWarnings(file.remove("post/emi_grid.rds" ))
suppressWarnings(file.remove("post/emi_table.rds"))

saveRDS(roads,               "post/emi_street.rds")
saveRDS(gx,                  "post/emi_grid.rds"  )
saveRDS(df,                  "post/emi_table.rds" )

#dt0 <- df[, round(sum(t), 2), by = .(pollutant)]
#print(dt0)
suppressWarnings( rm("df1", "df2", "df3", "dt", "dt0", "dt1", "dt2", "dt3", "factor_emi","g", "gx", "i", "lf", "na", "net", "pol", "pols", "x", "crs" ))
