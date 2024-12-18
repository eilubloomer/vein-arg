source("00_globalVariables.R")
#-----------------------------
#4) Post-estimation ####
# roads <- readRDS("network/net.rds")

#*crs <- 3857
#*osm_name <- "highway"
#*source("scripts/post2.R", encoding = "UTF-8")

#post2.R----------------------------------------------------------------
UF_select <- basename(getwd())
# g <- readRDS(paste0("../../../rds/g_", UF_select, ".rds"))

# df
df <- rbind(  fread("emi/exhaust.csv"),  fread("emi/wear.csv"),  fread("emi/evaporatives.csv"))
df$pollutant <- ifelse(df$pollutant == "PM2.5", "PM", df$pollutant)
df$emissions <- units::set_units(df$emissions, "g")
df$t <- units::set_units(df$emissions, "t")

# datatable ####
suppressWarnings(file.remove("post/emi_table.rds"))
cat("Saving emissions\n")
saveRDS(df, "post/emi_table.rds")
fwrite (df, "post/emi_table.csv")

dt0 <- df[, round(sum(t), 2), by = .(pollutant)]
#print(dt0)

#post2.R----------------------------------------------------------------

#--------
# 4) Post-estimation roads ####
roads_path <- "./sp/roads/" 
osm_name <- "highway"
source("scripts/post_roads.R", encoding = "UTF-8")

#--------
# 5) Post-estimation grids ####

dom   <- "d02_"   #select domain
month <- 7        #select month
mech  <- "CBMZ"   #select chemical mechanism

g <- eixport::wrf_grid("./wrf/wrfinput_d01")
saveRDS(g, "./wrf/gd01.rds")

g <- st_as_sf(g)                           # grid to spatial features
st_crs(g) <- 4326                          # set grid crs
crs <- 3857                                # 
pols <- c("CO","HC","NOx", "CO2", "PM", "NO2", "NO", "SO2", "CH4", "ETOH", "NH3", "N2O", "PM10", "NMHC_G_EXHAUST", "NMHC_E_EXHAUST", "NMHC_D_EXHAUST", "NMHC_G_EVAPORATIVES_HISTORIC", "NMHC_E_EVAPORATIVES_HISTORIC")

#source("scripts/post_grid.R", encoding = "UTF-8")
cat("Gridding lengths\n")

 f <- list.files(path = "post/streets/",  pattern = "emis_street", full.names = T)
fa <- list.files(path = "post/streets/",  pattern = "emis_street",  full.names = F)
x <- readRDS(f[month])[pols]
x <- st_crop(x, st_as_sfc(st_bbox(g)))
gx <- emis_grid(spobj = x, g = g, sr = crs)

print(paste0("post/grids/", dom, "emis_grid_", sprintf("%02d", month), ".rds"))
saveRDS(gx, paste0("post/grids/", dom,   "emis_grid_", sprintf("%02d", month),".rds"))

##--------
## plots
#metadata <- readRDS("config/metadata.rds")
#veh      <- readRDS("config/fleet_age.rds")
#pol      <- c("CO", "HC", "NOx", "CO2", "PM", "NMHC")
#pal      <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
#gpols <- c("CO", "HC", "NOx", "CO2", "PM", "NO2", "NO", "ETOH", "SO2", "CH4", "NH3", "N2O", "PM10", "NMHC_G", "NMHC_E", "NMHC_D")
#
#source("scripts/plots.R", encoding = "UTF-8")
#----------------------------------
# # MECH spatial ####
# g <- read_stars("../../cams/CAMS-GLOB-ANT_v4.2_carbon-monoxide_2017.nc", 
#                 sub = "tro")
# g <- eixport::wrf_grid("../../wrf/wrfinput_d01")
g <- readRDS("../../wrf/gd02.rds")
month <- 7
g <- st_as_sf(g)
st_crs(g) <- 4326

source("scripts/mech3_spatial_v2.R",  encoding = "UTF-8",  echo = F)

#------------------------------
# # MECH spatial GAS PM  ####
# g <- read_stars("../../cams/CAMS-GLOB-ANT_v4.2_carbon-monoxide_2017.nc", 
#                 sub = "tro")
# g <- eixport::wrf_grid("../../wrf/wrfinput_d01")
d <- 1
for(kk in 1:2){
 g <- readRDS(paste0("../../wrf/gd0", kk, ".rds"))
 month <- 7
 g <- st_as_sf(g)
 st_crs(g) <- 4326
 mech <- "CBMZ"
 dom <- paste0("d0", kk, "_")
 gas <- c("CO", "CO2", "NO2", "NO", "SO2", "CH4", "NH3", "N2O") 
 mw <- c(28.01, 44.01, 46.0055, 30.01, 64.066, 16.04, 17.031, 44.013) 
 pms <- c("PM", "PM10")
 aer <- "pm2023"
 source("scripts/mech3_spatial_GAS_PM.R", encoding = "UTF-8", echo = F)
}

# WRF CHEM
co  <- 2
o3  <- 1
no2 <- 0.9
no  <- 1.5
pm  <- 0.1

# type only grids
k <- 1
for(kk in 1:2){
  mech <- "CBMZ"
  dir.create("wrf")
  dir.create(paste0("wrf/", mech))
  g1 <- readRDS("../../wrf/gd01.rds")
  dim(g1)
  g2 <- readRDS("../../wrf/gd02.rds")
  dim(g2)
  
  for( k in seq_along(mech)) {
    # g <- eixport::wrf_grid("../../wrf/wrfinput_d01")
    # Number of lat points 179
    # Number of lon points 179
    
    # g <- eixport::wrf_grid("../../wrf/wrfinput_d02")
    # Number of lat points 198
    # Number of lon points 219
    
    language       <- "portuguese" # english spanish
    # net            <- readRDS("network/net.rds")
    domain         <- kk
    cols           <- ifelse(domain == 2, 198, 179)
    rows           <- ifelse(domain == 2, 219, 179)
    n_aero         <- 15
    wrf_times      <- 24 # 
    pasta_wrfinput <- "../../wrf/"
    pasta_wrfchemi <- "wrf"
    # dir.create(paste0(pasta_wrfchemi, "/", mech[k]))
    dom            <- paste0("d0", domain,"_")
    wrfi           <- paste0("../../wrf/wrfinput_d0", domain)
    hours          <- 0
    tz             <- "America/Sao_Paulo"
    month          <- 7
    lf             <- paste0("post/spec_grid/", dom, "emis_grid_07_mol.rds")
    
    source("scripts/wrf_country.R", encoding = "UTF-8")
  }
}
# 
fn <- function(x) {
  i <- magick::image_read(x)
  i <- magick::image_trim(i)
  magick::image_write(i, x)
}

fn("images/WRF_d01_CO.png")
fn("images/WRF_d02_CO.png")
fn("images/WRF_d01_NO.png")
fn("images/WRF_d02_NO.png")

#------------------------------------------------------------------------
#OLD STUF:
# 0 Configuration

#*readxl::excel_sheets(path)
#*tfs      <- readxl::read_xlsx(path = inventory:path, sheet = "tfs")
#*metadata <- readxl::read_xlsx(path = inventory_path, sheet = "metadata")
#*s        <- readxl::read_xlsx(path = inventory_path, sheet = "s")
#*standard <- readxl::read_xlsx(path = inventory_path, sheet = "standard")
#*factors  <- readxl::read_xlsx(path = inventory_path, sheet = "factors")
#*factors$factor <- 1
#*
#*mileage <- readRDS("../../config/mileage.rds")
#*veh    <- readRDS("./config/fleet_age.rds")
#*fuel   <- readRDS("./config/fuel.rds")
#*pmonth <- readRDS("./config/fuel_month.rds")
#*met    <- readRDS("./config/met.rds")
#*
#*scale  <- "tunnel2018"
#*theme  <- "black" # dark clean ing
#*delete_directories <- TRUE
#*#maxage in metadata = 40

# 1) Network ####
# crs <- 3857
# source("scripts/net.R", encoding = "UTF-8")
# rm(list = ls())
# gc()

#*# 2) Traffic ####
#*language <- "english" # spanish portuguese
#*metadata <- readRDS("config/metadata.rds")
#*categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
#*veh <- readRDS("config/fleet_age.rds")
#*verbose <- TRUE
#*theme <- "black" # dark clean ink
#*survival   <- TRUE
#*fuel <- readRDS("config/fuel.rds")
#*source("scripts/traffic.R", encoding = "UTF-8")
#*rm(list = ls())
#*gc()

#*# 3) Estimation ####
#*language <- "portuguese" # english chinese spanish portuguese
#*metadata <- readRDS("config/metadata.rds")
#*mileage <- readRDS("config/mileage.rds")
#*veh <- readRDS("config/fleet_age.rds")
#*# net <- readRDS("network/net.rds")
#*pmonth <- readRDS("config/pmonth.rds")
#*met <- readRDS("config/met.rds")
#*verbose <- FALSE
#*
#*# fuel calibration with fuel consumption data
#*fuel <- readRDS("config/fuel.rds")
#*pol <- "FC"
#*source("scripts/fuel_eval.R", encoding = "UTF-8")
#*rm(list = ls())
#*gc()
#*
#*
#*# Exhaust
#*language <- "portuguese" # english chinese spanish portuguese
#*metadata <- readRDS("config/metadata.rds")
#*mileage <- readRDS("config/mileage.rds")
#*veh <- readRDS("config/fleet_age.rds")
#*# net <- readRDS("network/net.rds")
#*pmonth <- readRDS("config/pmonth.rds")
#*met <- readRDS("config/met.rds")
#*verbose <- FALSE
#*pol <- c(
#*  "CO", "HC", "NMHC", "NOx", "CO2",
#*  "PM", "NO2", "NO", "SO2", "CH4", 
#*  "NH3", "CH4", "N2O", "ETOH"
#*)
#*scale <- "tunnel2018"
#*plot_ef <- FALSE
#*
#*s <- readRDS("config/s.rds")
#*factors <- readRDS("config/factors.rds")
#*
#*source("scripts/exhaust_sulfur.R", encoding = "UTF-8")
#*rm(list = ls())
#*gc()
#*
#*# Evaporative
#*language <- "portuguese" # english chinese spanish portuguese
#*metadata <- readRDS("config/metadata.rds")
#*mileage <- readRDS("config/mileage.rds")
#*veh <- readRDS("config/fleet_age.rds")
#*#net <- readRDS("network/net.rds")
#*pmonth <- readRDS("config/pmonth.rds")
#*meto <- readRDS("config/met.rds")
#*verbose <- FALSE
#*maxage <- 40
#*scale <- "tunnel2018"
#*plot_ef <- F
#*
#*metadata[family == "PC", maxage := 40]
#*metadata[family == "LCV", maxage := 40]
#*metadata[family == "TRUCKS", maxage := 40]
#*metadata[family == "BUS", maxage := 40]
#*metadata[family == "MC", maxage := 40]
#*source("scripts/evaporatives.R", encoding = "UTF-8", echo = F)
#*rm(list = ls())
#*gc()
#*
#*
#*# Tyres, Breaks and Road
#*language <- "english" #portuguese english spanish
#*metadata <- readRDS("config/metadata.rds")
#*mileage  <- readRDS("config/mileage.rds")
#*tfs      <- readRDS("config/tfs.rds")
#*# net      <- readRDS("network/net.rds")
#*veh      <- readRDS("config/fleet_age.rds")
#*pmonth <- readRDS("config/pmonth.rds")
#*pol      <- c("PM2.5", "PM10")
#*verbose  <- FALSE
#*maxage <- 40
#*source("scripts/wear.R", encoding = "UTF-8")
#*rm(list = ls())
#*gc()


