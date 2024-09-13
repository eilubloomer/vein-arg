options(encoding = "UTF-8")

#Packages to use:
require(vein)       # vein
require(sf)         # spatial
require(data.table) # faster data.frames
require(readxl)     # read .xls files
require(units)      # units conversions
require(ggplot2)

#Base directory
dir="/home/ram/github/eilubloomer/vein-arg/argentina"
setwd(dir)

#Input files:
inventory_path="inventory.xlsx"     # Path a archivo de entrada
sp_region_path="sp/provincias.gpkg" # gpkg de regiones/provincias/departamentos/localidades
sp_roads_path="sp/rutas.gpkg"       # gpkg de calles/caminos/avenidas/rutas/autopistas


#Run parameters:
YEAR=2019                          # Año elegido para inventario

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


