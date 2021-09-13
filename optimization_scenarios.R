#===============================================================================

# restoration scenarios allocating restoration pixels

#===============================================================================


#==== packages =================================================================

library(raster)
library(sf)
library(prioritizr)

#==== opening rasters ==========================================================

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

# Legal reserves, conferir uso!

RL <- raster(paste0(path,"RL.tif"))

#==== inserting data into a list ===============================================

data <- list(lim,co,app,RL)

#==== opening property boundaries ==============================================

path2 <- "property limits file"

properties <- st_read(file.path(path2,"file name.shp"))

CAR_list <- properties$CAR

# This shapefile contains columns with:

## proportion of APP
## proportion of vegetation
## A unique ID named CAR for each property


#==== clipping rasters =========================================================

# calculating (lembrar qq to calculandoa aqui!!)

#---- empty lists to store rasters masked at the property level ----------------

list_rasters <- list()
list_propveg <- list() # listing vegetation cover proportion
list_proAPP <- list()  # listing APP proportion
#lista_proAPP <- list()

c=1 # counter
for (i in properties) {
  ## subset each property ######################################################
  prop <- propriedades[propriedades$CAR==i,]
  veg <- prop$PropVeg
  papp <- prop$PropAPP
  ## function to crop rasters with property boundaries #########################
  f <- function(x)crop(x,extent(prop))
  data_crop <- lapply(data,f)
  ## masking rasters  ##########################################################
  f2 <- function(x)mask(x,prop)
  data_mask <- lapply(data_crop,f2)
  ## inserting on the empty lists  #############################################
  list_rasters[[c]] <- dados_mask
  list_propveg[[c]] <- veg
  list_proAPP[[c]] <- papp
  c=c+1
}

#===== moving targets ==========================================================

# defining moving targets to achieve the desired final forest cover (10,20,30
# 40)

# this considers baseline vegetation cover when defining the objective



#---- target 10 ----------------------------------------------------------------

target_moving_10 <- list()

for (i in seq(1,length(list_propveg),1)) {
  relative_target <- 0.1
  if (lista_propveg[[i]]> relative_target ){target_moving_10[[i]] <- 0}
  else {
    target_moving_10[[i]] <- relative_target - list_propveg[[i]] 
  }
}

#---- target 20 ----------------------------------------------------------------

target_moving_20 <- list()

for (i in seq(1,length(lista_propveg),1)) {
  relative_target <- 0.2
  if (list_propveg[[i]]> relative_target ){target_moving_20[[i]] <- 0}
  else {
    target_moving_20[[i]] <- relative_target - list_propveg[[i]] 
  }
}

target_moving_202 <- do.call(rbind,target_moving_20)[,1]

#--- target 30 -----------------------------------------------------------------

target_moving_30 <- list()

for (i in seq(1,length(lista_propveg),1)) {
  relative_target <- 0.3
  if (list_propveg[[i]]> relative_target ){target_moving_30[[i]] <- 0}
  else {
    target_moving_30[[i]] <- relative_target - list_propveg[[i]] 
  }
  
  
  
}

target_moving_302 <- do.call(rbind,target_moving_30)[,1]

#--- target 40 -----------------------------------------------------------------

target_moving_40 <- list()

for (i in seq(1,length(lista_propveg),1)) {
  relative_target <- 0.4
  if (lista_propveg[[i]]> relative_target ){target_moving_40[[i]] <- 0}
  else {
    target_moving_40[[i]] <- relative_target - list_propveg[[i]] 
  }
  
}

target_moving_402 <- do.call(rbind,target_moving_40)[,1]

#==== function to formulate problems  ==========================================

problem_formulation <- function(relative_target,raster,pen,ef, solutions){
  
  list_p2 <- list()
  for (i in seq(1,length(list_rasters),1)) {
    #### costs data ############################################################
    planning_unit <- raster[[i]][[2]]
    #### property limits  ## ###################################################
    features <- raster[[i]][[1]]
    #### locked-in app #########################################################
    lockedin <- raster[[i]][[3]]
    ### check if there are any app (at least 3 pixels) #########################
    ## if not, problem is formulated without locked-in constraints
    if(max(raster::cellStats(lockedin, "sum")) <=12){ 
      p <- problem(x = planning_unit,features = features)%>%
        add_min_set_objective()%>% # minimum costs
        add_relative_targets(relative_target[[i]]) %>%  # % restoration target
        add_binary_decisions()%>% 
        add_gurobi_solver(gap = 0.01,time_limit = 900 ) %>% 
        add_pool_portfolio(method = 2, number_solutions = solutions) %>% 
        add_boundary_penalties(penalty = pen, edge_factor = ef) 
      list_p2[[i]] <- p
      
    }
    else{
      #### problem fwith locked_in ############################################# 
      p <- problem(x = planning_unit,features = features)%>%
        add_min_set_objective()%>% 
        add_relative_targets(relative_target[[i]]) %>%  # % restauracao
        add_locked_in_constraints(lockedin) %>% # APPs
        add_binary_decisions()%>% 
        add_gurobi_solver(gap = 0.01,time_limit = 900) %>% 
        add_pool_portfolio(method = 2, number_solutions = solutions)%>% 
        add_boundary_penalties(penalty = pen, edge_factor = ef)
      lista_p2[[i]] <- p
      
    }
    
  }
  return(lista_p2)
  
}

#==== formulating the problems =================================================

#---- 10% target ---------------------------------------------------------------
list_p10 <- problem_formulation(relative_target = target_moving_10,
                                raster = lista_rasters,
                                pen = 0,
                                ef = 0,
                                solutions = 1)

#---- 20% target ---------------------------------------------------------------
list_p20 <- problem_formulation(relative_target = target_moving_20,
                                raster = lista_rasters,
                                pen = 0,
                                ef = 0,
                                solutions = 1)
#---- 30% target ---------------------------------------------------------------
list_p30 <- problem_formulation(relative_target = target_moving_30,
                                raster = lista_rasters,
                                pen = 0,
                                ef = 0,
                                solutions = 1)

#---- 40% target ---------------------------------------------------------------
list_p40 <- problem_formulation(relative_target = target_moving_40,
                                raster = lista_rasters,
                                pen = 0,
                                ef = 0,
                                solutions = 1)

#==== problem solving ==========================================================

f3 <- function(x)solve(x)
#====  S10 =================
s10 <- lapply(lista_p10,f3)

#====  S20 =================
s20 <- lapply(lista_p20,f3)

#====  S30 =================
s30 <- lapply(lista_p30,f3)

#====  S40 =================
s40 <- lapply(lista_p40,f3)

#==== calculating n. pixels restored ===========================================

area_restored <- function(solution){
  list_pixel <- list()
  for (i in seq(1,length(solucao),1)) {
    n.pixel <- cellStats(solution[[i]][[1]], "sum")
    list_pixel[[i]] <- n.pixel
  }
  
  pixels_restored <-  as.data.frame(do.call(rbind, lista_pixel))
  pixels_restored$CAR <- CAR_list
  return(pixels_restored)
}

#### 10 ###############################
area <- area_restoreda(solution = s10)
#### 20 ###############################
area <- area_restoreda(solution = s20)
#### 30 ###############################
area <- area_restored(solution = s30)
#### 40 ###############################
area <- area_restored(solution = s40)

#==== restoration costs ========================================================

costs <- function(soluction,costs){
  lista_custos <- list()
  
  for (i in seq(1,length(soluction),1)) {
    cost <- cellStats(costs[[i]][[2]] * solution[[i]][[1]], "sum")
    list_costs[[i]]<- cost
  }
  
  pixels_cost <-  as.data.frame(do.call(rbind, list_costs))
  pixels_cost$CAR <- CAR_list
  return(pixels_cost)
  
}

#---- 10 -----------------------------------------------------------
cost <- costs_propriedade(solucao = s10,costs =lista_rasters )
#---- 20 -----------------------------------------------------------
cost <- costs_propriedade(solucao = s20,costs =lista_rasters )
#---- 30 -----------------------------------------------------------
cost <- costs_propriedade(solucao = s30,costs =lista_rasters )
#---- 40 -----------------------------------------------------------
cost <- costs_propriedade(solucao = s40,costs =lista_rasters )

#---- combine into a single data.frame -------------------------------------------------------

# repeat for each target

df_scenarios <- merge(area,custo,by="CAR")
names(df_cenarios) <- c("CAR","area_rest_ha","cost_rest")

#---- adjusting values to hectares ---------------------------------------------

df_scenarios2 <- df_scenarios
f4 <- function(x)x*9/100
df_scenarios2[,c(2:3)] <- apply(df_scenarios2[,c(2:3)],MARGIN = 2,FUN =f4 )

#==== saving results ===========================================================

path.save <- "output file path"

#### target 10 ##################################################################
write.csv(df_scenarios2,paste0(path.save2,"area_costs_farm_level_t10_nopen.csv"))
#### target 20 ##################################################################
write.csv(df_scenarios2,paste0(path.save,"area_costs_farm_level_t20_nopen.csv"))
#### target 30 ##################################################################
write.csv(df_scenarios2,paste0(path.save,"area_costs_farm_level_t30_nopen.csv"))
#### target 40 ##################################################################
write.csv(df_scenarios2,paste0(path.save,"area_costs_farm_level_t40_nopen.csv"))

#==== mosaicing scenarios into one scene =======================================

mosaic_f <- function(list_solutions,x,y){
  
  solutions_raster <- list() 
  c = 1
  for (i in seq(x,y,1)) {
    
    solutions_raster[[c]] <- list_solutions[[i]][[1]]
    c = c + 1
  }
  
  solutions_raster$fun <- mean
  solutions_raster$na.rm <- TRUE
  mosaic_scen <- do.call(mosaic,solutions_raster)
  return(mosaic_scen)
}

#---- s10 ----------------------------------------------------------------------
mosaics10 <- mosaic_f(list_solutions = s10,x = 1,y = lengh(s10))
#---- s20 ----------------------------------------------------------------------
mosaics20 <- mosaic_f(list_solutions = s20,x = 1,y = lengh(s20))
#---- s10 ----------------------------------------------------------------------
mosaics30 <- mosaic_f(list_solutions = s30,x = 1,y = lengh(s30))
#---- s40 ----------------------------------------------------------------------
mosaics40 <- mosaic_f(list_solutions = s40,x = 1,y = lengh(s40))

#---- saving raster files ------------------------------------------------------

#### target 10 #################################################################
writeRaster(mosaics10,paste0(path.save,"mosaic_farm_level_10.tif"),
            overwrite=TRUE)
#### target 20 #################################################################
writeRaster(mosaics20,paste0(path.save,"mosaic_farm_level_20.tif"),
            overwrite=TRUE)
#### target 30 #################################################################
writeRaster(mosaics30,paste0(path.save,"mosaic_farm_level_30.tif"),
            overwrite=TRUE)
#### target 40 #################################################################
writeRaster(mosaics40,paste0(path.save,"mosaic_farm_level_40.tif"),
            overwrite=TRUE)
