#===============================================================================

# restoration scenarios using an optimizing algorithm

#===============================================================================


#==== packages =================================================================

library(raster)
library(sf)
library(prioritizr)

#==== opening rasters ==========================================================


# IMPORTANT! 
# all rasters must have same projection, extent and resolution

path <- "raster files path"

#--- property limits -----------------------------------------------------------

lim <- raster(paste0(path,"lim.tif"))

#---- cost raster --------------------------------------------------------------

co <- raster(paste0(path,"co.tif"))

#---- APP ----------------------------------------------------------------------

# areas of permanent protection -- as defined by landowners - were used as a 
# locked-in constraint

app <- raster(paste0(path,"app.tif"))

#---- Legal reserves -----------------------------------------------------------

# Legal reserves

RL <- raster(paste0(path,"RL.tif"))

#==== inserting data into a list ===============================================

data <- list(lim,co,app,RL)

#==== opening property boundaries ==============================================

path2 <- "property limits file"

properties <- st_read(file.path(path2,"file name.shp"))



CAR_list <- properties$CAR # all properties must have a unique ID, here named 
                           # CAR

#---- preliminary analysis -----------------------------------------------------

total_area <- sum(properties$Area_Propriedade_ha) # total farm's area
area_veg <- sum(properties$Veg_area_ha) # total area vegetated
area_app <- sum(properties$APPRast_ha) #total area of APP

prop.veg <- area_veg/area_total # proportion of vegetation
prop.app <- area_app/area_total # proportion of app

prop.total <- prop.veg + prop.app # total area covered by current veg and 
                                  # apps to be be restored

# the region already had 14% forest cover so we only considered 20-40% targets
# restoring apps already achieve 20% target, so there is no need to define a
# target

#==== defining targets for the remaining scenarios -============================

t30 <- 0.3 - prop.total 

# target 40 
t40 <- 0.4 - prop.total

#==== function to formulate problems  ==========================================

problem_formulation <- function(relative_target,raster,pen){
  
  x <- raster[[2]]
  feat <- raster[[1]]
  lin <- raster[[3]]
  problem(x =x,features = feat)%>%
    add_min_set_objective()%>% # 
    add_relative_targets(relative_target) %>%  
    add_locked_in_constraints(lin) %>% 
    add_binary_decisions()%>% 
    add_gurobi_solver(gap = 0) %>% 
    add_pool_portfolio(method = 2, number_solutions = 1)%>% 
    add_boundary_penalties(pen) 
}

#==== masking the rasters using properties boundaries to reduce processing time =

f <- function(x)crop(x,extent(properties))
data_crop <- lapply(data,f)
f2 <- function(x)mask(x,propriedades)
data_mask <- lapply(data_crop,f2)


#==== formulating the problems =================================================

# 20% target
p20 <- problem_formulation(relative_target = 0.2,raster = data_mask,pen = 0)
# 30% target
p30 <- problem_formulation(relative_target = 0.3,raster = data_mask,pen = 0)
# 40% target
p40 <- problem_formulation(relative_target = 0.4,raster = data_mask,pen = 0)


#==== problem solving ==========================================================

#====  S20 =================

s20 <- solve(p20)

#====  S30 =================

s30 <- solve(p30)

#====  S40 =================

s40 <- solve(p40)

#==== calculating area restored and costs========================================

# function for area

area_restored <- function(solution){
  lista_pixel <- list()
  for (i in seq(1,length(solucao),1)) {
    n.pixel <- cellStats(solucao[[i]][[2]], "sum")
    lista_pixel[[i]] <- n.pixel
  }
  
  restored_pixels <-  as.data.frame(do.call(rbind, list_pixel))
  restored_pixels$CAR <- CAR_list
  return(pixels_restaurados)
}

# function for costs

costs_property <- function(solution){
  costs_list <- list()
  for (i in seq(1,length(solution),1)) {
    cost <- cellStats(solution[[i]][[1]] * solution[[i]][[2]], "sum")
    costs_list[[i]]<- custo
  }
  
  pixels_custo <-  as.data.frame(do.call(rbind, lista_custos))
  pixels_custo$CAR <- CAR_list
  return(pixels_custo)
  
}

#---- adding cost and solution data into lists ---------------------------------

costs_solutions_s20 <- list(data_mask[[2]],s20[[1]])
costs_solutions_s30 <- list(data_mask[[2]],s30[[1]])
costs_solutions_s40 <- list(data_mask[[2]],s40[[1]])


## 20

list_rasters_s20 <- list()

c=1

for (i in CAR_listsub) {
  ## subset de cada propriedade ###########################
  prop <- propriedades[propriedades$CAR==i,]
  ## funcao que cropa os raster ###########################
  f <- function(x)crop(x,extent(prop))
  costs_crop <- lapply(costs_solutions_s20,f)
  ## extraindo raster de interesse ########################
  f2 <- function(x)mask(x,prop)
  costs_mask <- lapply(costs_crop,f2)
  ## inserindo na lista de rasters ########################
  list_rasters_s20[[c]] <- costs_mask 
  c=c+1
}

## 30

list_rasters_s30 <- list()

c=1

for (i in CAR_listsub) {
  ## subset de cada propriedade ###########################
  prop <- propriedades[propriedades$CAR==i,]
  ## funcao que cropa os raster ###########################
  f <- function(x)crop(x,extent(prop))
  costs_crop <- lapply(costs_solutions_s30,f)
  ## extraindo raster de interesse ########################
  f2 <- function(x)mask(x,prop)
  costs_mask <- lapply(costs_crop,f2)
  ## inserindo na lista de rasters ########################
  list_rasters_s30[[c]] <- costs_mask 
  c=c+1
}


## 40

list_rasters_s40 <- list()

c=1

for (i in CAR_listsub) {
  ## subset de cada propriedade ###########################
  prop <- propriedades[propriedades$CAR==i,]
  ## funcao que cropa os raster ###########################
  f <- function(x)crop(x,extent(prop))
  costs_crop <- lapply(costs_solutions_s40,f)
  ## extraindo raster de interesse ########################
  f2 <- function(x)mask(x,prop)
  costs_mask <- lapply(costs_crop,f2)
  ## inserindo na lista de rasters ########################
  list_rasters_s40[[c]] <- costs_mask 
  c=c+1
}


#--- calculating the values ---------------------------------------------------

## 20

area_s20 <- area_restored(list_rasters_s20)
costs_s20 <- costs_property(solution = list_rasters_s20)

##30
area_s30 <- area_restored(list_rasters_s30)
costs_s30 <- costs_property(solution = list_rasters_s30)

##40
area_s40 <- area_restored(list_rasters_s40)
costs_s40 <- costs_property(solution = list_rasters_s40)

#---- combining in a data frame-------------------------------------------------

df_rl20 <- merge(area_s20,costs_s20,by="CAR")
df_rl30 <- merge(area_s30,costs_s30,by="CAR")
df_rl40 <- merge(area_s40,costs_s40,by="CAR")


#---- converting area to ha ----------------------------------------------------

f4 <- function(x)x*900/10000

df_rl20[,c(2:3)] <- apply(df_rl20[,c(2:3)],MARGIN = 2,FUN =f4 )

names(df_rl20) <- c("CAR","area_ha","cost")

df_rl30[,c(2:3)] <- apply(df_rl30[,c(2:3)],MARGIN = 2,FUN =f4 )

names(df_rl30) <- c("CAR","area_ha","cost")

df_rl40[,c(2:3)] <- apply(df_rl40[,c(2:3)],MARGIN = 2,FUN =f4 )

names(df_rl40) <- c("CAR","area_ha","cost")

#==== saving results ======================================================

#---- saving data frames--------------------------------------------------------

write.csv(df_rl20,"rl20.csv")
write.csv(df_rl30,"rl30.csv")
write.csv(df_rl30,"rl40.csv")


#---- saving raster files -----------------------------------------------------


writeRaster(s20[[1]],"regional_level_20.tif")
writeRaster(s30[[1]],"regional_level_30.tif")
writeRaster(s40[[1]],"regional_leve_40.tif")


