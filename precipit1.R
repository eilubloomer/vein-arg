setwd("/home/eileen/Downloads/Precipit")
precipit= read.csv("Exp. Nro. 203783-Precip.csv")
precipit$Fecha = strftime(as.POSIXlt(precipit$Fecha,"%Y-%m-%d"),"%Y-%m")
precipit$Precipitacion..mm.= as.numeric(precipit$Precipitacion..mm.)
precipit <- precipit[!is.na(precipit$Precipitacion..mm.), ]
dlluvia = aggregate(precipit$Precipitacion..mm., by = list(fecha = precipit$Fecha, id = precipit$Estacion), FUN = function(x) c(numDias = sum(x > 0.254), cantidadPrecip= sum(x[x > 0.254])))
estMet= read.csv("estaciones SMN PAIS.csv", sep= ",")
estMet= estMet[,c("PROVINCIA", "NRO")]
precipitacion = merge(estMet, dlluvia, by.x="NRO", by.y = "id")
precipitacion <- subset(precipitacion, NRO %in% c(87576, 87222, 87155, 87828, 87344, 87166, 87374, 87162, 87046, 87623, 87217, 87418, 87178, 87715, 87791, 87047, 87311, 87436, 87925, 87371, 87129, 87938, 87121))
precipitacion <- tablafinal[order(precipitacion$NRO, precipitacion$fecha), ]
write.csv(tablafinal, "Precipitación", row.names=F)
