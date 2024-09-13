#wdir="/home/eileen/github/vein/prep"  # eilee
wdir="/home/ram/github/eilubloomer/vein/prep"  # ram
setwd(wdir)

# Rutas a archivos:
path_tmp_file="data/SMN/Exp. Nro. 203783.txt"
path_ppt_file="data/SMN/Exp. Nro. 203783-Precip.txt"
path_est_file="data/SMN/SMN_estaciones.csv"

#Leo archivos de entrada:
tmp= read.csv(path_tmp_file,sep=",",dec=".", header=T, na.strings = "\\N")  #Leo archivo c/ datos horarios de temperaturas
ppt= read.table(path_ppt_file, sep=",",dec=".",header=T,na.strings = "\\N") #Leo archivo c/ datos diarios  de precipitaciones
est= read.csv(path_est_file,sep=",",dec=".",header=T)                       #Leo archivo estaciones SMN
est=est[c("NRO","PROVINCIA","NOMBRE")]
#Estaciones a usar:
estaciones_id=c(87582,87576, 87222, 87155, 87828, 87355, 87166, 87374, 87162, 87078, 87623, 87217, 87418, 87178, 87715, 87791, 87047, 87311, 87436, 87925, 87371, 87129, 87938, 87121)

#----------------
# Temperaturas: calculo de temperaturas medias mensuales representativas de cada provincia.

tmp$mes = strftime(as.POSIXlt(tmp$Fecha,"%Y-%m-%d"),"%Y-%m-01")                                                  #imprimir con el año
tmp <- tmp[!is.na(tmp$Temp..Media...C.), ]                                                                    #saco las Temp Medias == N/A con: is.na(columna)
tmpMedia = aggregate(tmp$Temp..Media...C., by =list(mes=tmp$mes, id=tmp$Estacion), FUN=mean, na.action=na.rm) #calculo media mensual de cada estación

tablaTemp  = merge(est, tmpMedia, by.x="NRO", by.y = "id")

tablaTemp <- subset(tablaTemp, NRO %in% estaciones_id )               #filtrar por estaciones que quiero subset() subset ( tabla$Estacion %in% estaciones)
tablaTemp <- tablaTemp[order(tablaTemp$PROVINCIA, tablaTemp$mes), ]

colnames(tablaTemp)=c("estId","Provincia","estName","yr_mo","temp[ºC]")
#write.csv(format(tablaFinal, digits=3), "met.csv", row.names=F)

#----------------
# Precipitaciones: calculo precipitación acumulada mensual y dias con precipitaciones para cada mes de cada provincia

ppt$mes = strftime(as.POSIXlt(ppt$Fecha,"%Y-%m-%d"),"%Y-%m-01")   #Creo columna yr_mo
ppt <- ppt[!is.na(ppt$Precipitacion..mm.), ]                   #elimino filas con na values

lluvia = aggregate(ppt$Precipitacion..mm., by = list(mes = ppt$mes, id = ppt$Estacion), FUN = function(x) c(diasPrecip = sum(x > 0.254), acumPrecip= sum(x[x > 0.254]))) # calculo ppt acum mensual y dias con ppt por mes

tablaPpt = merge(est, lluvia, by.x="NRO", by.y = "id")
   
tablaPpt = subset(tablaPpt, NRO %in% estaciones_id )
tablaPpt = tablaPpt[order(tablaPpt$PROVINCIA, tablaPpt$mes), ]

colnames(tablaPpt)=c("estId","region","estName","yr_mo","ppt")
#write.csv(format(tablaFinal, digits=3), "Precipitación.csv", row.names=F)

#----------------
# Merge all

tablaMet = merge(tablaPpt,tablaTemp[c("yr_mo","estId","temp_C")], by=c("yr_mo","estId"))

tablaMet$Year =strftime(tablaMet$yr_mo,"%Y")
tablaMet$Month=strftime(tablaMet$yr_mo,"%m")


write.csv(format(tablaMet, digits=3), "met.csv", row.names=F)
