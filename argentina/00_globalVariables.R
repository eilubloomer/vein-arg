options(encoding = "UTF-8")

#Packages to use:
require(vein)       # vein
require(sf)         # spatial
require(data.table) # faster data.frames
require(readxl)     # read .xls files
require(units)      # units conversions
require(ggplot2)

library(cptcity) # 7120 colour palettes
library(stars)
library(eixport)

#Base directory
dir="/home/ram/github/eilubloomer/vein-arg/argentina"
setwd(dir)

#Input files:
inventory_path="inventory.xlsx"     # Path a archivo de entrada
sp_region_path="sp/provincias.gpkg" # gpkg de regiones/provincias/departamentos/localidades
sp_roads_path="sp/rutas.gpkg"       # gpkg de calles/caminos/avenidas/rutas/autopistas

#Projection
crs          <- 31983               #Coordinate reference system:

#Run parameters:
YEAR=2019                          # Año elegido para inventario
months_subset<-c(1:12)             #10:11 for instance

pollutants= c("CO", "HC", "NMHC", "NOx", "CO2", "PM", "NO2", "NO", "SO2")   # contaminantes a inventariar

col_region         <- "region"     # esta columna debe estar presente en fuel y met
scale              <- "none"
delete_directories <- TRUE
verbose            <- FALSE
do_plots           <- FALSE
theme              <- "black"      # plot theme

#01-config    variables
add_reg_veh        <- TRUE

#02-traffic   variables
survival=TRUE                                        # aplico curva de supervivencia?
categories= c("PC", "LCV", "TRUCKS", "BUS", "MC")    # categorias vehiculos: autos, traffics/pick-up/kangoo, camiones, bondis, motos

# Preguntas:
#	- que significado tiene "lv", "v" y "vv"


#03-fuel_eval variables


#04-exhaust   variables
IM=FALSE


#05-evaporative variables

#06- wear

#07-paved roads
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
