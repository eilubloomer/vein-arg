options(encoding = "UTF-8")
#------------------------------------
# VEIN - Implementación ARGENTINA
#------------------------------------
require(vein)       # vein
require(sf)         # spatial
require(ggplot2)    # plots
require(data.table) # faster data.frames
require(readxl)     # read .xls files
require(units)
require(stars)
require(eixport)
require(cptcity)    # 7120 colour palettes

wdir="/home/usuario/github/eilubloomer/vein/argentina"
setwd(wdir)

## global vars:
YEAR=2019                          # Año elegido para inventario

## Input files:
inventory_path="inventory.xlsx"    # Path a archivo de entrada

sp_zones_path="sp/provincias.gpkg" # archivo espacial de regiones/provincias/departamentos/localidades
sp_roads_path="sp/rutas.gpkg"      # archivo espacial de calles/caminos/avenidas/rutas/autopistas


#--------------------------------------------------------------------
#
# (0) Leo y proceso "inventory.xlsx"

## Leo sheets:
metadata   <- read_xlsx(path = inventory_path, sheet = "metadata")      # definicion de categorias de vehiculos y características
geocode    <- read_xlsx(path = inventory_path, sheet = "geocode")       # definicion de regiones y sus id (geocode)
fuel_month <- read_xlsx(path = inventory_path, sheet = "fuel_month")    # consumo de combustible por mes y region
mileage    <- read_xlsx(path = inventory_path, sheet = "mileage")       # kilometraje de cada categoria
fleet_age  <- read_xlsx(path = inventory_path, sheet = "fleet")         # flota anual (numero de autos de cada categoría)
tfs        <- read_xlsx(path = inventory_path, sheet = "tfs")           # ciclo diurno
met        <- read_xlsx(path = inventory_path, sheet = "met")           # temperaturas medias y dias con precip.
euro       <- read_xlsx(path = inventory_path, sheet = "euro")          # ??
tech       <- read_xlsx(path = inventory_path, sheet = "tech")          # ??
im_ok      <- read_xlsx(path = inventory_path, sheet = "im_ok")         # num. vehiculos que fallaron verif. tecnica total
im_co      <- read_xlsx(path = inventory_path, sheet = "im_co")         # num. vehiculos que fallaron verif. tecnica por CO
im_hc      <- read_xlsx(path = inventory_path, sheet = "im_hc")         # num. vehiculos que fallaron verif. tecnica por HC
im_nox     <- read_xlsx(path = inventory_path, sheet = "im_nox")        # num. vehiculos que fallaron verif. tecnica por NOx
im_pm      <- read_xlsx(path = inventory_path, sheet = "im_pm25")       # num. vehiculos que fallaron verif. tecnica por PM25
s          <- read_xlsx(path = inventory_path, sheet = "s")             # ??
fuel_spec  <- read_xlsx(path = inventory_path, sheet = "fuel_spec")     # características de los combustibles
pmonth     <- read_xlsx(path = inventory_path, sheet = "pmonth")        # ???


#--------------------------------------------------------------------
#
# (1) Network

crs=32721 #UTM faja 21.   
zones=read_sf(sp_zones_path)   # leo regiones
roads=read_sf(sp_roads_path)   # leo rutas

zones=st_transform(zones,crs)  # transf. coords a sist. proyectado (generalmente vienen en epsg:4326 (latlon))
roads=st_transform(roads,crs)  # transf. coords a sist. proyectado (generalmente vienen en epsg:4326 (latlon))

#--------------------------------------------------------------------
#
# (2) Traffic

categories <- c("pc", "lcv", "trucks", "bus", "mc") # autos, traffics, camiones, bondis, motos
k_D        <- 1/2.482039   # ???    
k_E        <- 1/5.708199   # ???
k_G        <- 1/5.866790   # ???
survival   <- TRUE

#source("scripts/traffic.R", encoding = "UTF-8")
# fleet age
fleet_age[is.na(fleet_age)] <- 0

# apply survival functions
if(survival){
  for (i in seq_along(metadata$vehicles)) {
    fleet_age[[metadata$vehicles[i]]] <- age( x = fleet_age[[metadata$vehicles[i]]], type = metadata$survival[i], a = metadata$survival_param_a[i], b = metadata$survival_param_b[i] )
  }
}

# extraigo subcategorias de vehiculos
subcategories <- names(fleet_age)
c_PC     <- subcategories[grep(pattern = "PC",     x = subcategories)]
c_LCV    <- subcategories[grep(pattern = "LCV",    x = subcategories)]
c_TRUCKS <- subcategories[grep(pattern = "TRUCKS", x = subcategories)]
c_BUS    <- subcategories[grep(pattern = "BUS",    x = subcategories)]
c_MC     <- subcategories[grep(pattern = "MC",     x = subcategories)]


# calculate proportion in PC
PC_total=sum(fleet_age[, c_PC])
kPC_G  <- sum(fleet_age$PC_G)  / PC_total
kPC_E  <- sum(fleet_age$PC_E)  / PC_total
kPC_FG <- sum(fleet_age$PC_FG) / PC_total
kPC_FE <- sum(fleet_age$PC_FE) / PC_total
kPC <- c(kPC_G, kPC_E, kPC_FG, kPC_FE)
l_PC <- list()

# fuel
kf <- c(k_G, k_E, k_G, k_E)

for (i in seq_along(n_PC)) {
  x <- veh[[n_PC[i]]] * kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_PC[i], ".rds"))
  l_PC[[i]] <- unlist(x)
}
dfpc <- as.data.frame(do.call("cbind", l_PC))
names(dfpc) <- n_PC













#--------------------------------------------------------------------
#
# (3) Emissions

## (3.1) Exhaust

## (3.2) Evaporatives

## (3.2) Paved roads







