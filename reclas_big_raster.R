library(raster)
# library(tiff)
library(rgdal)
library(foreign)
library(dplyr)
# library(data.table)



# library(foreach)
# library(doSNOW)
# 
# library(doMC)

# cpus <- 3  ## Number the trheads in your computer
#
# cl <- makeCluster(cpus)   
#
# registerDoSNOW(cl)  ## For Windows


# install.packages("RGtk2", depen=T)
# install.packages("tiff", depen=T)
# install.packages("doMC", depen=T)

# path <- '//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_cocoa/_raster/_glc/_2015/World_Ecological_2015/globalelus_query/'
path <- '//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_cocoa/_raster/_glc/_2015/World_Ecological_2015/_co/'

###
x <- raster(paste0(path, 'World_Ecological_2015.tif'))
y <- read.dbf(paste0(path, 'World_Ecological_2015.tif.vat.dbf')) %>%
  tbl_df()


select_clumn <- select(y, ELU_GLC_De, Value) 

unique_values <- unique(select(select_clumn, ELU_GLC_De)) %>%
  mutate(ID = 1:length(ELU_GLC_De))
  
joint_var <- inner_join(select_clumn, unique_values, by = 'ELU_GLC_De')

# select_clnum <- mutate(select_clumn, ELU_GLC_De)

config_variable <- function(x){
  
  initial_df <- filter(joint_var, Value == x) 
  
  if(dim(initial_df)[1] !=0){
    
    y <-  initial_df$ID
  }else{
    
    y <- x
  }
  
  return(y)
  
}

# proof <- lapply(v[1:100], config_variable)

filename = 'test'

f3 <- function(x, filename) {
   out <- raster(x)
   out <- writeStart(out, filename, overwrite=TRUE)
   for (r in 1:nrow(out)) {
     
     
     v <- getValues(x, r)
     
     proof <- lapply(v, config_variable) 
     proof <- as.vector(do.call('rbind', proof))
     
    ## Probar Foreach 
     
     
    # bind_rows(proof)   
    #  rbindlist(proof)
    #  
    #  library (plyr)
    #  df <- ldply (proof, c)
    #  proof <- ldply(v, config_variable, .parallel = TRUE)
    #  llply(proof, c, .parallel = TRUE)
     
     # for(j in 1:length(v)){
     #   
     #   initial_df <- filter(joint_var, Value == v[j]) 
     #   if(dim(initial_df)[1] !=0){ 
     #     
     #     v[j] = initial_df$ID
     #   }else{
     #     
     #     v[j] = v[j]
     #   }
     #   
     # 
     #   }
     
     out <- writeValues(out, v, r)
     }
   out <- writeStop(out)
   return(out)
}

## Function to Big raster process

reclassify_bigRaster <- function(x, filename = ''){
  
  out <- raster(x)
  small <- canProcessInMemory(out, 3)
  filename <- trim(filename)
  
  
  ## Condition to determine if the raster needs to write in disk and make the name
  ## for the file raster
  
  if(!small & filename == ''){
    
      filename <- rasterTmpFile()  
    
  }
  
  todisk <- FALSE
  
  if(filename != ''){
    
    out <- writeStart(out, filename, overwrite = TRUE)
    todisk <- TRUE
    
  } else {
    
    vv <- matrix(ncol = nrow(out), nrow = ncol(out))
  }
  
  
  for(r in 1:nrow(out)){
    print(paste(r, nrow(out)))
    v <- getValues(x, r)
    v <- lapply(v, config_variable) 
    v <- as.vector(do.call('rbind', v))
    
         if(todisk){
      
                out <- writeValues(out, v, r)
      
      
               } else{
      
                        vv[, r] <- v
               }
  }
    
  if(todisk) {
    
    out <- writeStop(out)
    
  } else {
    
    out <- setValues(out, as.vector(vv))
    
  }     
  
  return(out)
  
}
