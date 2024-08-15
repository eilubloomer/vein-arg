wdir="/home/usuario/github/eilubloomer/vein/prep"     #defino directorio de trabajo
setwd(wdir)                                           #voy a directorio de trabajo

combustibles=c("G","D","GNC")
#Leo archivo de ventas:
df = read.csv("data/res_1104_04/precios-eess-2023.csv", sep = ",", dec = ".", stringsAsFactors = F) # leo archivo de entrada

df=df[which(!df$canal_de_comercializacion %in% c("Agro","Reventa a otras estaciones de servicio")),]  # no contabilizamos combustible al agro ni reventas
df = df[c("mes", "producto","volumen", "provincia")]                                # me quedo solo con las columnas que me interesan

df$producto[grep("Gas Oil*", df$producto)]="D"                                      # renombro productos Gas Oil* -> "D" (diesel)
df$producto[grep("Nafta*", df$producto)]="G"                                        # renombro productos Nafta*   -> "G" (gasolina)
df=df[which(df$producto %in% combustibles),]                                        # me quedo solo con las filas con producto "D" o "G".

vol= aggregate(df[c("volumen")], by=list(combustible=df$producto, provincia=df$provincia, mes=df$mes), FUN = "sum")
vol=vol[order(vol$combustible, vol$provincia,vol$mes),]                             # re-ordeno las filas

colnames(vol)=c("fuel","region","month","volumen_m3")
#Escribo resultado
write.csv(vol, "fuel.csv",row.names = FALSE)                       # escribo archivo de salida
