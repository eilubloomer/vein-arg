
# n_veh
n_PC    <- metadata$vehicles[grep(pattern = "PC", x = metadata$vehicles)]
n_LCV   <- metadata$vehicles[grep(pattern = "LCV", x = metadata$vehicles)]
n_TRUCKS<- metadata$vehicles[grep(pattern = "TRUCKS", x = metadata$vehicles)]
n_BUS   <- metadata$vehicles[grep(pattern = "BUS", x = metadata$vehicles)]
n_MC    <- metadata$vehicles[grep(pattern = "MC", x = metadata$vehicles)]

n_veh   <- list(PC = n_PC, LCV = n_LCV, TRUCKS = n_TRUCKS, BUS = n_BUS, MC = n_MC)

inte <- intersect(metadata$vehicles, names(fleet))

if(length(inte) != length(metadata$vehicles)){
  cat( "veh needs the same vehicles as metadata$vehicles:\n")
  stop()
}

# apagando arquivos
message("Deleting veh/*.rds\n")
system("cp -rf veh vehold")
# arquivos <- list.files(path = "veh", pattern = ".rds", full.names = TRUE)
# file.remove(arquivos)

# fleet age
fleet[is.na(fleet)] <- 0

# plotting
cat("Plotting traffic flows\n")

# identicar nomes de grupos
nveh    <- names(fleet)
n_PC    <- nveh[grep(pattern = "PC", x = nveh)]
n_LCV   <- nveh[grep(pattern = "LCV", x = nveh)]
n_TRUCKS<- nveh[grep(pattern = "TRUCKS", x = nveh)]
n_BUS   <- nveh[grep(pattern = "BUS", x = nveh)]
n_MC    <- nveh[grep(pattern = "MC", x = nveh)]

setDT(fleet)
setorderv(fleet, cols = c( "Year"), order = c( -1))

if(survival) {

  if(any(grepl("region", names(fleet)))) {
    cat("Identified `region` in `veh`\n")

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

  } else {
    cat("No `region` in `veh`\n")

    for(i in seq_along(metadata$vehicles)) {
      fleet[[metadata$vehicles[i]]] <-   age(x = fleet[[metadata$vehicles[i]]],
                                           type = metadata$survival[i],
                                           a = metadata$survival_param_a[i],
                                           b = metadata$survival_param_b[i])
    }}
}


# veh ####
#
v <- metadata$vehicles

# columna region en hoja fuel ####
reg <- unique(fuel[["region"]])

if(any(grepl("region", names(fleet)))) {

  cat("Identified `region` in `veh`\n")

  rbindlist(lapply(seq_along(v), function(i) {

    if(verbose){
      cat("\n", metadata$vehicles[i],
          rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
      )}

    rbindlist(lapply(seq_along(reg), function(j) {

      # cat(reg[j], " " )

      x <- fleet[[v[i]]]*fuel[region == reg[j] &
                              fuel == metadata$fuel[i]]$kfinal
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
                          value.name = "veh")
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
                          value.name = "veh")
    df$vehicles <- v[i]
    df
  })) -> vv
}


# plots ####
if (do_plots){
   cat("Plotting fleet \n")

   names(vv)

   vv <- merge(vv,
            metadata,
            by = "vehicles",
            all.x = TRUE)

   vv$age <- as.numeric(gsub("V", "", vv$age))
   vv$Year <- YEAR - vv$age + 1
   vv$sf <- paste(vv$size, vv$fuel)
   fam <- unique(metadata$family)

   vv <- remove_units(vv)

   if(any("region" %in% names(vv))) {
   
     for(i in seq_along(fam)) {
   
       ggplot(vv[family == fam[i] &
                   as.numeric(fleet)> 0],
              aes(x = Year,
                  y = fleet,
                  colour = vehicles)) +
         geom_line() +
         facet_wrap(~ region) +
         scale_y_sqrt() +
         theme_bw(base_size = 10) -> p
   
       png(paste0("images/FLEET_CIRCULATING_", fam[i], "_FINAL.png"),
           width = 3000, height = 2500, "px", res = 300)
       print(p)
       dev.off()
     }
   
   } else {
     for(i in seq_along(fam)) {
   
       ggplot(vv[family == fam[i] &
                   as.numeric(fleet)> 0],
              aes(x = Year,
                  y = fleet,
                  colour = vehicles)) +
         geom_line() +
         theme_bw(base_size = 10)+
         theme(axis.text.x = element_text(angle = 90)) -> p
   
       png(paste0("images/FLEET_CIRCULATING_", fam[i], "_FINAL.png"),
           width = 3000,
           height = 2500,
           "px",
           res = 300)
       print(p)
       dev.off()
     }
   
   
   }

# ggplot2
  dx <- vv[,
         sum(fleet, na.rm = T),
         by = .(vehicles,
                family,
                region
         )]
  names(dx)[4] <- "fleet"

  p <- ggplot(dx,
            aes(x = vehicles,
                y = fleet,
                fill = family)) +
  geom_bar(stat = "identity",
           col = "black")+
  labs(y = "fleet",
       title = "Vehicles") +
  facet_wrap(~ region,
             # scales = "free_x",
             nrow = 1) +
  theme_bw() +
  scale_y_sqrt() +
  coord_flip()+
  scale_x_discrete(limits = rev(metadata$vehicles)) +
  theme(axis.text.x = element_text(angle=90,
                                   hjust=1))

  p
  
  png(filename =  paste0("images/VEHICLES_FINAL.png"),
      width = 2500, height = 2500, units = "px", pointsize = 12,
      bg = "white",  res = 300)
  print(p)
  dev.off()

}
