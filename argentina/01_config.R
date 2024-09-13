source("00_globalVariables.R")
#-----------------------------
#
# (1) Leo y proceso "inventory.xlsx"

## Leo sheets:
metadata <- read_xlsx(path = inventory_path, sheet = "metadata" ); setDT(metadata ) # def. de categorias de vehiculos y características
geocode  <- read_xlsx(path = inventory_path, sheet = "geocode"  ); setDT(geocode  ) # def. de regiones e ids (geocode)
fuel     <- read_xlsx(path = inventory_path, sheet = "fuel"     ); setDT(fuel     ) # consumo de combustible por mes y region
fleet    <- read_xlsx(path = inventory_path, sheet = "fleet"    ); setDT(fleet    ) # flota anual (numero de autos de cada categoría)
mileage  <- read_xlsx(path = inventory_path, sheet = "mileage"  ); setDT(mileage  ) # kilometraje de cada categoria
tfs      <- read_xlsx(path = inventory_path, sheet = "tfs"      ); setDT(tfs      ) # ciclo diurno
met      <- read_xlsx(path = inventory_path, sheet = "met"      ); setDT(met      ) # temperaturas medias y dias con precip.
euro     <- read_xlsx(path = inventory_path, sheet = "euro"     ); setDT(euro     ) # ??
tech     <- read_xlsx(path = inventory_path, sheet = "tech"     ); setDT(tech     ) # ??
s        <- read_xlsx(path = inventory_path, sheet = "s"        ); setDT(s        ) # ??
fuel_spec<- read_xlsx(path = inventory_path, sheet = "fuel_spec"); setDT(fuel_spec) # características de los combustibles
#pmonth   <- read_xlsx(path = inventory_path, sheet = "pmonth"   ); setDT(pmonth   ) # ???
im_ok    <- read_xlsx(path = inventory_path, sheet = "im_ok"    ); setDT(im_ok    ) # num. vehic. que fallaron verif. tecnica total
im_co    <- read_xlsx(path = inventory_path, sheet = "im_co"    ); setDT(im_co    ) # num. vehic. que fallaron verif. tecnica por CO
im_hc    <- read_xlsx(path = inventory_path, sheet = "im_hc"    ); setDT(im_hc    ) # num. vehic. que fallaron verif. tecnica por HC
im_nox   <- read_xlsx(path = inventory_path, sheet = "im_nox"   ); setDT(im_nox   ) # num. vehic. que fallaron verif. tecnica por NOx
im_pm    <- read_xlsx(path = inventory_path, sheet = "im_pm25"  ); setDT(im_pm    ) # num. vehic. que fallaron verif. tecnica por PM25

# assuming HY and GLP G in the meantime
metadata$fuel <- gsub("HY" , "G", metadata$fuel)
metadata$fuel <- gsub("GLP", "G", metadata$fuel)

#transforma milage a un objeto "units" que tiene unidades de km.
#mileage[, metadata$vehicles] <- add_lkm(mileage[, metadata$vehicles])  #ES NECESARIO? 

#Agregar columna de region a fleet (veh)
region <- toupper(unique(geocode$region))
if(!any(grepl("region", names(fleet)))) {
  if(add_reg_veh) {
    fleet <- rbindlist(lapply(seq_along(region), function(i){
    fleet$region <- region[i]
    fleet
     }))
  }
}

# fuel ####
# necesita columnas Year, Month, FUEL_M3 *density_tm3
fuel <- fuel[Year ==  YEAR]
fuel[, date          := ISOdate(Year, Month, 1, 0,0,0)]
fuel[, consumption_t := FUEL_M3 *density_tm3          ]

# manual
fuel[, type := "data"]

pmonth <- fuel

fuel <- fuel[, sum(consumption_t),  by = .(region, Year, fuel, type, density_tm3) ] #-> fuel
names(fuel)[ncol(fuel)] <- "consumption_t"
fuel$consumption_t <- units::set_units(fuel$consumption_t,   "t")
fuel$density_tm3   <- units::set_units(fuel$density_tm3,  "t/m3")
fuel$kinitial <- 1   


#Filter by inventory selected year:
fuel   <-   fuel[Year ==  YEAR]
pmonth <- pmonth[Year ==  YEAR]
met    <-    met[Year ==  YEAR]

#Save all to .rds files on the config directory:
if (!dir.exists("config")){dir.create("config")}; # Create the "config" dir if not exists

saveRDS(metadata ,"config/metadata.rds")
saveRDS(geocode  ,"config/geocode.rds") 
saveRDS(fuel     ,"config/fuel.rds")     
saveRDS(fleet    ,"config/fleet.rds")    
saveRDS(mileage  ,"config/mileage.rds")  
saveRDS(tfs      ,"config/tfs.rds")      
saveRDS(met      ,"config/met.rds")      
saveRDS(euro     ,"config/euro.rds")     
saveRDS(tech     ,"config/tech.rds")     
saveRDS(s        ,"config/s.rds")        
saveRDS(fuel_spec,"config/fuel_spec.rds")
saveRDS(pmonth   ,"config/pmonth.rds")   
saveRDS(im_ok    ,"config/im_ok.rds")    
saveRDS(im_co    ,"config/im_co.rds")    
saveRDS(im_hc    ,"config/im_hc.rds")    
saveRDS(im_nox   ,"config/im_nox.rds")   
saveRDS(im_pm    ,"config/im_pm.rds")    
