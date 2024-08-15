wdir="/home/usuario/github/eilubloomer/vein/prep"     #defino directorio de trabajo
setwd(wdir)                                           #voy a directorio de trabajo

categorias=c("PC", "LCV", "TRUCKS", "BUS","MC")

subcategorias=list(                                                                          #[1] "PICK-UP"                  "TRACTOR C/CABINA DORMITORIO"  "SEMIRREMOLQUE"                "PICK-UP CABINA DOBLE"        
  PC=c("COUPE","SEDAN*","CONVERTIBLE","DESCAPOTABLE","FAMILIAR","PHAETON","TODO TERRENO"),   #[5] "FURGONETA"                "SEDAN 4 PUERTAS"              "CHASIS C/CABINA"              "SEDAN 5 PUERTAS"             
  LCV=c("PICK-UP*","FURGON*","CHASIS*","RURAL*","UTILITARIO"),                               #[9] "FURGON"                   "RURAL 5 PUERTAS"              "TODO TERRENO"                 "ACOPLADO"                    
  TRUCKS=c("REMOLQUE","CAMION","ARENERO","ACOPLADO"),                                        #[13] "CAMION"                  "PICK-UP CABINA SIMPLE"        "CHASIS C/CABINA DORMITORIO"   "CHASIS CON CABINA DORMITORIO"
  BUS=c("BUS","PASAJEROS"),                                                                  #[17] "CHASIS CON CABINA"       "SEDAN 3 PUERTAS CON PORTON"   "TRACTOR C/ CABINA DORMITORIO" "CHASIS CON CABINA DOBLE"     
  MC=c("MOTO"))                                                                              #[21] "SEDAN 2 PUERTAS"         "CHASIS S/CABINA"              "SEMIRREMOLQUE BITREN T"       "SEMIRREMOLQUE BITREN D"      
                                                                                             #[25] "COUPE"                   "MIDIBUS"                      "ARENERO"                      "CONVERTIBLE"                 
                                                                                             #[29] "SEDAN 3 PUERTAS"         "UTILITARIO"                   "TRANS.DE PASAJEROS"           "DESCAPOTABLE"                
                                                                                             #[33] "FAMILIAR"                "CHASIS SIN CABINA"            "MINIBUS (O MICROOMNIBUS)"     "CAMION AUTOBOMBA"            
                                                                                             #[37] "PICK-UP CABINA Y MEDIA"  "TRACTOR DE CARRETERA"         "PICK UP"                      "CARRETON"                    
                                                                                             #[41] "CASA RODANTE"            "PHAETON"                      "CASA RODANTE C/MOTOR"   
#Leo archivo de inscripciones:
df = read.csv("data/dnrpa_inscr_iniciales/dnrpa-inscripciones-iniciales-autos-all.csv", sep = ",", dec = ".", stringsAsFactors = F) # leo archivo de entrada
df = df[c("tramite_fecha", "automotor_tipo_descripcion","titular_domicilio_provincia")]                                # me quedo solo con las columnas que me interesan
#df$mes = strftime(as.POSIXlt(df$tramite_fecha,"%Y-%m-%d"),"%Y-%m")   #C
df$year = strftime(as.POSIXlt(df$tramite_fecha,"%Y-%m-%d"),"%Y")   #C

df$category="UNKNOWN"                
for (cat in categorias) {     
  for (subcat in subcategorias[[cat]] ){
     df$category[grep(subcat,df$automotor_tipo_descripcion)]=cat                             
  }
}                             

df$vehiculos=1
veh= aggregate(df[c("vehiculos")], by=list(category=df$category, region=df$titular_domicilio_provincia, year=df$year), FUN = "sum")
veh=veh[order(veh$category, veh$region,veh$year),]                             # re-ordeno las filas

colnames(veh)=c("category","region","year","vehicles")

#Escribo resultado
write.csv(veh, "fleet.csv", row.names = FALSE)                       # escribo archivo de salida
