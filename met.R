setwd("C:/Users/pc/Downloads/OneDrive_1_26-7-2024")
met= read.csv("Exp. Nro. 203783.txt")
met$mes = strftime(as.POSIXlt(met$Fecha,"%Y-%m-%d"),"%Y-%m")#imprimir con el año
met$Temp..Media...C.= as.numeric(met$Temp..Media...C.)
met <- met[!is.na(met$Temp..Media...C.), ]#saco las TempMedias N/A con: is.na(columna)
metMedia = aggregate(met$Temp..Media...C., by =list(mes=met$mes, id=met$Estacion), FUN=mean, na.action=na.rm)#agregar columna año y dps en by agregar año
estMet= read.csv("EstacionesSMN.csv", sep= ";")
tablafinal = merge(estMet, metMedia, by.x="NRO", by.y = "id")
#filtrar por estaciones que quiero subset() subset ( tabla$Estacion %in% estaciones)
tablafinal <- subset(tablafinal, NRO %in% c(87576, 87222, 87155, 87828, 87355, 87166, 87374, 87162, 87078, 87623, 87217, 87418, 87178, 87715, 87791, 87047, 87311, 87436, 87925, 87371, 87129, 87938, 87121))
tablafinal <- tablafinal[order(tablafinal$NOMBRE, tablafinal$mes), ]
write.csv(tablafinal, "Met_estacionesSMN", row.names=F)
