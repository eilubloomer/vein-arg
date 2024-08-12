setwd("/home/eileen/Downloads/vein_res1104/res1104-04")
df <- read.csv("precios-eess-2023.csv", sep = ",", dec = ".", stringsAsFactors = F) 
tablaChica = df[c("mes", "producto","volumen", "provincia")]
tablaChica$producto[which(tablaChica$producto=="Gas Oil Grado 2")]="D" #cambiar id de producto
tablaChica$producto[which(tablaChica$producto=="Gas Oil Grado 3")]="D" 
tablaChica$producto[which(tablaChica$producto=="Nafta (súper) entre 92 y 95 Ron")]="G" 
tablaChica$producto[which(tablaChica$producto=="Nafta (premium) de más de 95 Ron")]="G" 
tablaChica$producto[which(tablaChica$producto=="Nafta (común) hasta 92 Ron")]="G" 
tablaChica=tablaChica[-which(tablaChica$producto=="GNC"),] #sacar filas que son GNC tabla
tablaChica=tablaChica[-which(tablaChica$producto=="N/D"),]
tablaChica=tablaChica[-which(tablaChica$producto=="Kerosene"),]
tablaChica=tablaChica[-which(tablaChica$producto=="GLPA"),]
volVendido = aggregate(tablaChica[c("volumen")], by=list(combustible=tablaChica$producto, provincia=tablaChica$provincia, mes=tablaChica$mes), FUN = "sum")
tablaOrdenada = volVendido[order( volVendido$combustible),]
tablaOrdenada = volVendido[order(volVendido$mes),]
tablaOrdenada = volVendido[order(volVendido$provincia),]
library(openxlsx)
write.xlsx(tablaOrdenada, "tablaOrdenada.xlsx", rowNames = FALSE)
