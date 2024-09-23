source("00_globalVariables.R")
#-----------------------------
# Wear Emis. ####

# Tyres, Breaks and Road
language <- "english" #portuguese english spanish
metadata <- readRDS("config/metadata.rds")
mileage  <- readRDS("config/mileage.rds")
tfs      <- readRDS("config/tfs.rds")
fleet    <- readRDS("config/fleet.rds")
pmonth <- readRDS("config/pmonth.rds")
# net      <- readRDS("network/net.rds")
# pol <- c("TSP", "PM10", "PM2.5", "PM1",  "PM0.1")
pol      <- c("PM2.5", "PM10")

verbose  <- FALSE
maxage <- 40
fuel <- readRDS("config/fuel.rds")

reg <- unique(fuel$region)
setDT(pmonth)

suppressWarnings(file.remove("emi/wear.csv"))

# Escapamento ####
cat("Estimating emissions Wear\n")
metadata_original <- metadata

metadata$v_eea_old <- ifelse(metadata$v_eea_old %in% c("PC", "LCV", "Motorcycle"),
                             metadata$v_eea_old,
                             "HDV")

metadata$v_eea_old <- ifelse(metadata$v_eea_old %in% c("Motorcycle"),
                             "2W",
                             metadata$v_eea_old)
# monthly profile
metadata$fuel <- gsub("ELEC", "G", metadata$fuel)


# wear ####
wear <- c("tyre", "break", "road")


for(k in seq_along(reg)) {

  cat("\n\n", reg[k],  "\n")
  for (i in seq_along(metadata$vehicles)) {

    cat("\n", metadata$vehicles[i]) #, rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])) )
    x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
    x[is.na(x)] <- 0
    x <- x[region == reg[k], ]
    x$region <- NULL
    setDF(x)

    dm <- pmonth[region == reg[k] & fuel == metadata$fuel[i]]$consumption_t

    for (j in seq_along(pol)) {
      cat(" ", pol[j], " ")

      for (m in seq_along(wear)) {

        ef <- ef_wear(wear= wear[m],
                      type = metadata$v_eea_old[i],
                      pol = pol[j],
                      speed = metadata$speed[i])

        ef <- rep(ef[[1]], ncol(x))

        array_x <- emis_hot_td(
          veh = x,
          lkm = mileage[[metadata$vehicles[i]]],
          ef = ef[1:maxage],
          fortran = TRUE,
          nt = check_nt()*0.9,
          pro_month = dm,
          verbose = verbose,
          params = list(
            veh = metadata$vehicles[i],
            size = metadata$size[i],
            fuel = metadata$fuel[i],
            pollutant = pol[j],
            type_emi = "Wear",
            subtype_emi = wear[m],
            baseyear = YEAR
          )
        )
        array_x$region <- reg[k]

        fwrite(array_x, "emi/wear.csv", append = TRUE)
      }
    }
  }

}
cat("\nEmissions in: /emi/wear.csv")
