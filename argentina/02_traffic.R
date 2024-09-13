source("00_globalVariables.R")
#-----------------------------
# 2) Traffic ####
#
# Preguntas:
#       - que significado tiene "lv", "v" y "vv"
# 	- la parte donde se calculan las flotas aplicando survival funcs. parece repetirse dos veces, por que?
#-----------------------------

#Load .rds tables:
metadata <- readRDS("config/metadata.rds"); setDT(metadata)
fleet    <- readRDS("config/fleet.rds"   ); setDT(fleet)     #veh
fuel     <- readRDS("config/fuel.rds"    ); setDT(fuel)

# NAs to 0 on fleet
fleet[is.na(fleet)] <- 0

#Create "veh" & "images" directories if not exist:
if (! dir.exists("veh")   ){dir.create("veh"   )};
if (! dir.exists("images")){dir.create("images")};

# Delete any existing file into "veh/"
arquivos <- list.files(path = "veh", pattern = ".rds", full.names = TRUE)
file.remove(arquivos)


#Get vehicle names categories:
n_PC     <- metadata$vehicles[grep(pattern = "PC",     x = metadata$vehicles)]  #names of categories with "PC_" 
n_LCV    <- metadata$vehicles[grep(pattern = "LCV",    x = metadata$vehicles)]
n_TRUCKS <- metadata$vehicles[grep(pattern = "TRUCKS", x = metadata$vehicles)]
n_BUS    <- metadata$vehicles[grep(pattern = "BUS",    x = metadata$vehicles)]
n_MC     <- metadata$vehicles[grep(pattern = "MC",     x = metadata$vehicles)]

n_veh    <- list(PC = n_PC, LCV = n_LCV, TRUCKS = n_TRUCKS, BUS = n_BUS, MC = n_MC)

inte <- intersect(metadata$vehicles, names(fleet))
if(length(inte) != length(metadata$vehicles)){
  cat( "ERROR: fleet needs the same vehicles categories as metadata$vehicles:\n")
  stop()
}

# identicar nomes de grupos                                #(!) NO ES REDUNDANTE?
nveh    <- names(fleet)
n_PC    <- nveh[grep(pattern = "PC",     x = nveh)]
n_LCV   <- nveh[grep(pattern = "LCV",    x = nveh)]
n_TRUCKS<- nveh[grep(pattern = "TRUCKS", x = nveh)]
n_BUS   <- nveh[grep(pattern = "BUS",    x = nveh)]
n_MC    <- nveh[grep(pattern = "MC",     x = nveh)]


setorderv(fleet, cols = c( "Year"), order = c( -1))

if(survival) {

  if(any(grepl("region", names(fleet)))) {
    cat("Identified `region` column in `fleet`\n")
    lv <- split(fleet, fleet[["region"]])

    for(j in seq_along(lv)) {
      for(i in seq_along(metadata$vehicles)) {
        lv[[j]][[metadata$vehicles[i]]] <-   age(x = lv[[j]][[metadata$vehicles[i]]],
                                                 type = metadata$survival[i],
                                                 a = metadata$survival_param_a[i],
                                                 b = metadata$survival_param_b[i])
      }
    }
    fleet <- rbindlist(lv)

  } else {                                          # esto entiendo que no pasaría nunca por que en 01_config forzamos a que haya col de regiones.
    cat("No `region` in `veh`\n")
    for(i in seq_along(metadata$vehicles)) {
        fleet[[metadata$vehicles[i]]] <-   age(x = fleet[[metadata$vehicles[i]]],
                                             type = metadata$survival[i],
                                             a = metadata$survival_param_a[i],
                                             b = metadata$survival_param_b[i])
    }
  }
}


# fleet ####
#

v <- metadata$vehicles

# columna region en hoja fuel ####
reg <- unique(fuel[["region"]])

if(any(grepl("region", names(fleet)))) {

  cat("Identified `region` in `veh`\n")

  rbindlist(lapply(seq_along(v), function(i) {            #loop en vehiculos
    rbindlist(lapply(seq_along(reg), function(j) {        #loop en regiones
      
      x <- fleet[[v[i]]]*fuel[region == reg[j] & fuel == metadata$fuel[i]]$kinitial
      x <- remove_units(x)[1:metadata$maxage[i]]
      x <- Vehicles(matrix(x, ncol = metadata$maxage[i]))
      x$"region" <- reg[j]
      x
    })) -> dt
    saveRDS(dt, paste0("veh/", v[i], ".rds"))

    df <- melt.data.table(dt,
                          id.vars = "region",
                          measure.vars = paste0("V", 1:metadata$maxage[i]),
                          variable.name = "age",
                          value.name = "fleet")
                          #value.name = "veh")
    df$vehicles <- v[i]
    df
  })) -> vv

} else {
  cat("No `region` in `veh`\n")

  rbindlist(lapply(seq_along(v), function(i) {

    if(verbose){
      cat("\n", metadata$vehicles[i],
          rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
      )}

    # cat(reg[j], " " )

    x <- fleet[[v[i]]]*fuel[fuel == metadata$fuel[i]]$kinitial
    x <- remove_units(x)[1:metadata$maxage[i]]
    x <- Vehicles(matrix(x, ncol = metadata$maxage[i]))
    x

    saveRDS(x, paste0("veh/", v[i], ".rds"))

    df <- melt.data.table(x,
                          # id.vars = "region",
                          measure.vars = paste0("V", 1:metadata$maxage[i]),
                          variable.name = "age",
                          value.name = "fleet")
                          #value.name = "veh")
    df$vehicles <- v[i]
    df
  })) -> vv
}


# plots ####
if (do_plots){
  cat("Plotting fleet \n")
  
  names(vv)
  
  vv <- merge(vv, metadata, by = "vehicles", all.x = TRUE)
  
  vv$age <- as.numeric(gsub("V", "", vv$age))
  vv$Year <- YEAR - vv$age + 1
  vv$sf <- paste(vv$size, vv$fuel)
  fam <- unique(metadata$family)
  #vv <- remove_units(vv)
  
  cat("    Plotting FLEET CIRCULATING\n")
  for(i in seq_along(fam)) {
  
     ggplot(vv[family == fam[i] &  as.numeric(fleet)> 0], aes(x = Year, y = fleet, colour = vehicles)) +
        geom_line() +
        facet_wrap(~ region, scales = "free_y") +
        theme_bw(base_size = 10)+
        theme(axis.text.x = element_text(angle = 90)) -> p
  
    png(paste0("images/FLEET_CIRCULATING_", fam[i], ".png"),
        width = 3000, height = 2500, "px", res = 300)
    print(p)
    dev.off()
  }
  
  cat("    Plotting VEHICLES INITIAL\n")
  dx <- vv[, sum(fleet, na.rm = T), by = .(vehicles, family, region)]
  names(dx)[4] <- "fleet"
  
  p <- ggplot(dx, aes(x = vehicles, y = fleet, fill = family)) +
        	geom_bar(stat = "identity", col = "black")+
  	      labs(y = "fleet", title = "Vehicles") +
          facet_wrap(~ region, scales = "free_x", nrow = 2) +
          theme_bw() +
          #scale_y_sqrt() +
          coord_flip()+
          scale_x_discrete(limits = rev(metadata$vehicles)) +
          theme(axis.text.x = element_text(angle=90, hjust=1))
  
  p
  
  png(filename =  paste0("images/VEHICLES_INITIAL.png"),
      width = 2500, height = 2500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()
  
}
  
rm(list = ls())
gc()
