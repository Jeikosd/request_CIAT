load(file = "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_cocoa/_data_R/image_make_random_points.RData")

bra_df  ## data frame con el numero de presencias
bra_adm1 ## shapefile correspondiente al anterior data frame
glc_bra_adm1_rcl_resample ## raster areas donde deben ir las presencias


library('spdplyr')
library('rgeos')
## Nota recordar que el objeto points_filter debe ser generado anteriormente en algun codigo

## shapefile_all shapefile que contiene todos los departamentos 
## points_filter es un objeto data frame que contiene los departamentos para generar los puntos aleatorios (debe tener el mismo nombre 
## que el anterior shapefile_all) 
## raster_areas es cualquier raster que coincida con la resolucion que el investigador quiere y al mismo extent del shapefile_all
## name_features
## parallel = T True, F False
## ncpus = 2 default

values <- Rpoints_by_Shapefile(bra_adm1, bra_df, glc_bra_adm1_rcl_resample)


Rpoints_by_Shapefile <- function(shapefile_all, points_filter, raster_areas){
  
  # Proof
  # shapefile_all <- bra_adm1
  # points_filter <- bra_df
  # raster_areas <- glc_bra_adm1_rcl_resample
  

  random_points <- function(x, y, z, name_filter, n) {
    
    
    # Proof
    # x <- shapefile_all
    # y <- points_filter
    # z <- raster_areas
    # name_filter <- points_filter$Name[2]
    # n <- points_filter$number[2]
    
    x <- x %>%
      filter(name %in% name_filter)
    
    w <- raster::crop(z, x)
    coord_value <- tbl_df(rasterToPoints(w))
    
    
    if(n <= dim(coord_value)[1]){
      
      points_random <- sample_n(coord_value, n, replace = FALSE) %>%
        mutate(name = rep(name_filter, length(x)))
      
    }else{
      points_random <- coord_value %>%
        mutate(name = rep(name_filter, length(x)))
      
    }

    
    return(points_random)
  }
  
  # points_random <- random_points(shapefile_all, points_filter, raster_areas, points_filter$Name[i], points_filter$number[i])
  
  points_random <- lapply(1:dim(points_filter)[1], function(i) random_points(shapefile_all, points_filter, raster_areas, points_filter$Name[i], points_filter$number[i]))
  points_random <- bind_rows(points_random)
  
  
  # library(snowfall)
  # sfInit(parallel = T, ncpus)
  # sfLibrary(raster)
  # sfLibrary(spdplyr)
  # 
  # sfExport('shapefile_all')
  # sfExport('points_filter')
  # sfExport('random_points')
  # sfExport('raster_areas')
  # 
  # points_random <- sfLapply(1:length(points_filter$Name), function(i) random_points(shapefile_all, points_filter, points_filter$Name[i], points_filter$number[i]))

    return(points_random)
  }
 