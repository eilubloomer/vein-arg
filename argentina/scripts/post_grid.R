cat("Gridding lengths\n")

 f <- list.files(path = "post/streets/",  pattern = "emis_street", full.names = T)
fa <- list.files(path = "post/streets/",  pattern = "emis_street",  full.names = F)
x <- readRDS(f[month])[pols]
x <- st_crop(x, st_as_sfc(st_bbox(g)))
gx <- emis_grid(spobj = x, g = g, sr = crs)
  
print(paste0("post/grids/", dom, "emis_grid_", sprintf("%02d", month), ".rds"))
saveRDS(gx, paste0("post/grids/", dom,   "emis_grid_", sprintf("%02d", month),".rds"))
          
