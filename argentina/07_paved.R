source("00_globalVariables.R")
#-----------------------------
# Paved road Emis. ####

# 4) paved roads ####
metadata <- readRDS("config/metadata.rds")
mileage  <- readRDS("config/mileage.rds")
tfs      <- readRDS("config/tfs.rds"    )
fleet    <- readRDS("config/fleet.rds"  )
pmonth   <- readRDS("config/pmonth.rds" )
met      <- readRDS("config/met.rds"    )       #ra , lo usa?
fuel     <- readRDS("config/fuel.rds"   )
# net      <- readRDS("network/net.rds")

suppressWarnings(file.remove("emi/resuspension.csv"))
reg    <- unique(fuel$region)
ra = met[,c("Month","region","precip_mm")]     # esta bien esto?

maxage <- 40
wear   <- c("tyre", "break", "road")
pol    <- c("PM2.5", "PM10")
#k      <- c(0.62, 0.15)
kpm10 = 4.6
kpm25 = 1.1


sL1 <- 2.4 # silt [g/m^2] se ADT < 500 (CENMA CHILE)          # tertiary
sL2 <- 0.7 # silt [g/m^2] se 500 < ADT < 5000 (CENMA CHILE)   # secondary
sL3 <- 0.6 # silt [g/m^2] se 5000 < ADT < 10000 (CENMA CHILE) # primary
sL4 <- 0.3 # silt [g/m^2] se ADT > 10000 (CENMA CHILE)        # motorway trunk


nr <- data.table(
  highway =  c("primary", "tertiary", "secondary", "trunk","motorway"),
  V1 = c(96950,45101,128350,24288,19643)
)

nr[, perc :=  V1/sum(V1)]

nr[, sL := ifelse(
  highway == "motorway", "sL4",
  ifelse(
    highway == "trunk", "sL3",
    ifelse(
      highway == "primary", "sL2",
      "sL1" ) ) )]

sl <- nr[, sum(perc), by = sL]

# Wear ####

# ef
ef_paved <- function(k, sL, W) {
  efx <- k * (sL/2)^0.65 * (W/3)^1.5
  EmissionFactors(efx)
}

vx <- list.files("veh", full.names = T)
nv <- list.files("veh", full.names = F)
nv <- gsub(".rds", "", nv)

rbindlist(lapply(seq_along(vx), function(i){
  xx <- readRDS(vx[i])
  dt <- data.table(x = rowSums(xx[, 1:(ncol(xx) - 1), with = F]),
                   region = xx[, ncol(xx), with = F][[1]],
                   veh = nv[i])
  dt
})) -> dveh

dveh[grepl(pattern = "PC",     x = veh), w := 1  ]
dveh[grepl(pattern = "LCV",    x = veh), w := 3.5]
dveh[grepl(pattern = "TRUCKS", x = veh), w := 20 ]
dveh[grepl(pattern = "BUS",    x = veh), w := 20 ]
dveh[grepl(pattern = "MC",     x = veh), w := 1  ]

dveh[, xw := x*w]
dveh[, fleet :=sum(x), by = region]
awf <- unique(dveh[, sum(xw)/fleet, by = region])
#pmonth[, pro := m3/sum(m3), by = .(fuel, region)]
pmonth[, pro := FUEL_M3/sum(FUEL_M3), by = .(fuel, region)]

# implementation ####

for(k in seq_along(reg)) {

  cat("\n\n", reg[k])
  for (i in seq_along(metadata$vehicles)) {

    cat("\n", metadata$vehicles[i], rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])) )

    x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
    x <- as.data.frame(x)
    x[is.na(x)] <- 0
    xv <- x[x$region == reg[k], 1:maxage]
    for (j in seq_along(pol)) {

      cat(" ", pol[j], " ")
      ef <- ef_paved(k = if(pol[j] == "PM10" ) kpm10 else kpm25,
                     sL = (2.4*sl[sL == "sL1"]$V1 +
                             0.7*sl[sL == "sL2"]$V1 +
                             0.6*sl[sL == "sL3"]$V1 +
                             0.5*sl[sL == "sL4"]$V1),
                     W = awf[region == reg[k]]$V1)
      # print(ef)
      for (mo in 1:12) {
        dm <- pmonth[region == reg[k] & fuel == metadata$fuel[i] & month(date) == mo]$pro
        lkm = mileage[[metadata$vehicles[i]]][1:maxage]
        xe <- unlist(xv)*lkm*ef*(1 - ra[region == unique(region)[k] & Month == mo]$PN)

        array_x <- data.table(
          emissions = xe,
          rows = k,
          age = 1:maxage,
          month = mo,
          veh = metadata$vehicles[i],
          size = metadata$size[i],
          fuel = metadata$fuel[i],
          pollutant = if(pol[j] == "PM10" ) "PM10" else "PM2.5",
          type_emi = "Resupension",
          subtype_emi = "",
          baseyear = YEAR,
          region = reg[k]
        )

        fwrite(array_x, "emi/resuspension.csv", append = TRUE)

        rm(array_x)
        gc()
      }
    }
  }
}

