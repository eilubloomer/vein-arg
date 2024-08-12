setwd("~/vein_test/argen/sp/res-1104-04")                                           #voy a directorio de trabajo

df <- read.csv("precios-eess-2023.csv", sep = ",", dec = ".", stringsAsFactors = F) # leo archivo de entrada
df = df[c("mes", "producto","volumen", "provincia")]                                # me quedo solo con las columnas que me interesan
df$producto[grep("Gas Oil*", df$producto)]="D"                                      # renombro productos Gas Oil* -> "D" (diesel)
df$producto[grep("Nafta*", df$producto)]="G"                                        # renombro productos Nafta*   -> "G" (gasolina)
df=df[which(df$producto %in% c("G","D")),]                                          # me quedo solo con las filas con producto "D" o "G".

vol = aggregate(df[c("volumen")], by=list(combustible=df$producto, provincia=df$provincia, mes=df$mes), FUN = "sum")

vol=vol[order(vol$combustible, vol$provincia,vol$mes),]                             # re-ordeno las filas

write.csv(vol, "volumenesPorProvincia.csv",row.names = FALSE)                       # escribo archivo de salida