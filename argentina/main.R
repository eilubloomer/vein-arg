options(encoding = "UTF-8")
#------------------------------------
# VEIN - Implementación ARGENTINA
#------------------------------------
require(vein)       # vein
require(sf)         # spatial
require(data.table) # faster data.frames
require(readxl)     # read .xls files
require(units)      # units conversions
#require(eixport)   #netcdf?
#require(stars)
#require(ggplot2)    # plots
#require(cptcity)    # 7120 colour palettes

wdir="/home/usuario/github/eilubloomer/vein/argentina"
setwd(wdir)

## global vars:
YEAR=2019                          # Año elegido para inventario

categories= c("PC", "LCV", "TRUCKS", "BUS", "MC")                           # categorias vehiculos: autos, traffics/pick-up/kangoo, camiones, bondis, motos
pollutants= c("CO", "HC", "NMHC", "NOx", "CO2", "PM", "NO2", "NO", "SO2")   # contaminantes a inventariar

## Input files:
inventory_path="inventory.xlsx"    # Path a archivo de entrada
sp_zones_path="sp/provincias.gpkg" # gpkg de regiones/provincias/departamentos/localidades
sp_roads_path="sp/rutas.gpkg"      # gpkg de calles/caminos/avenidas/rutas/autopistas

#--------------------------------------------------------------------
#
# (0) Leo y proceso "inventory.xlsx"

## Leo sheets:
metadata   <- read_xlsx(path = inventory_path, sheet = "metadata")      # def. de categorias de vehiculos y características
geocode    <- read_xlsx(path = inventory_path, sheet = "geocode")       # def. de regiones e ids (geocode)
fuel       <- read_xlsx(path = inventory_path, sheet = "fuel")          # consumo de combustible por mes y region
fleet      <- read_xlsx(path = inventory_path, sheet = "fleet")         # flota anual (numero de autos de cada categoría)
#mileage    <- read_xlsx(path = inventory_path, sheet = "mileage")       # kilometraje de cada categoria
tfs        <- read_xlsx(path = inventory_path, sheet = "tfs")           # ciclo diurno
met        <- read_xlsx(path = inventory_path, sheet = "met")           # temperaturas medias y dias con precip.
euro       <- read_xlsx(path = inventory_path, sheet = "euro")          # ??
tech       <- read_xlsx(path = inventory_path, sheet = "tech")          # ??
#im_ok      <- read_xlsx(path = inventory_path, sheet = "im_ok")         # num. vehic. que fallaron verif. tecnica total
#im_co      <- read_xlsx(path = inventory_path, sheet = "im_co")         # num. vehic. que fallaron verif. tecnica por CO
#im_hc      <- read_xlsx(path = inventory_path, sheet = "im_hc")         # num. vehic. que fallaron verif. tecnica por HC
#im_nox     <- read_xlsx(path = inventory_path, sheet = "im_nox")        # num. vehic. que fallaron verif. tecnica por NOx
#im_pm      <- read_xlsx(path = inventory_path, sheet = "im_pm25")       # num. vehic. que fallaron verif. tecnica por PM25
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

k_D        <- 1/2.482039   # ???    
k_E        <- 1/5.708199   # ???
k_G        <- 1/5.866790   # ???
survival   <- TRUE

#source("scripts/traffic.R", encoding = "UTF-8")
# fleet age
fleet[is.na(fleet)] <- 0

# apply survival functions
if(survival){
  for (i in seq_along(metadata$vehicles)) {
    fleet[[metadata$vehicles[i]]] <- age( x = fleet[[metadata$vehicles[i]]], type = metadata$survival[i], a = metadata$survival_param_a[i], b = metadata$survival_param_b[i] )
  }
}

# extraigo subcategorias de vehiculos
subcategories <- names(fleet)
c_PC     <- subcategories[grep(pattern = "PC",     x = subcategories)]
c_LCV    <- subcategories[grep(pattern = "LCV",    x = subcategories)]
c_TRUCKS <- subcategories[grep(pattern = "TRUCKS", x = subcategories)]
c_BUS    <- subcategories[grep(pattern = "BUS",    x = subcategories)]
c_MC     <- subcategories[grep(pattern = "MC",     x = subcategories)]


# calculate proportion in PC
PC_total=sum(fleet[, c_PC])
kPC_G  <- sum(fleet$PC_G)  / PC_total
kPC_E  <- sum(fleet$PC_E)  / PC_total
kPC_FG <- sum(fleet$PC_FG) / PC_total
kPC_FE <- sum(fleet$PC_FE) / PC_total
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







