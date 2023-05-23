#---------------------------------------------------------

# map with study site

#---------------------------------------------------------

library(sf)
library(geobr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggspatial)

#---- pasta com o geodatabase -----------------------------------

# replace with a shapefile with the study site(!!)
# g <- file.path(dirname(getwd()),"capitulo2.gdb")
# g1 <- "C:/Users/falbe/OneDrive/Doutorado/Cap1/Cap1.gdb" apagar!!

#### BR ###############

BR <- read_country()

#### MA ##############

Biom <- read_biomes(simplified = T)%>%filter(name_biome=="Mata Atlântica")

#### list municipalities from Gonzales-Chaves et al. 2021 (insert study name)

df_Adrian <- read.csv("tables/metricas_studysite_Gonzalez_Chaves_2021.csv")

#### municipios BR ######################

Mun <- read_municipality()

MunAdrian <- filter(Mun,code_muni %in% df_Adrian$codigo_ibg)

# check here which variables were needed

#Mun_Adrian2 <- merge(MunAdrian,df_Adrian[,c(1,4:16)],by.y="cod",by.x="CD_GEOCMU")

MunAdrian2 <- left_join(MunAdrian,df_Adrian,by=join_by(code_muni==codigo_ibg))


#### AE ##########################

AE <- st_read("shapefiles/study_site.shp")

# get the bounding box:
Lim = st_as_sfc(st_bbox(AE))
# Lim_a <- st_bbox(c(xmin = -47.62647, xmax = -44.7, ymax = -23.1, ymin = -20.5), crs = st_crs(AE))
# Lim <-  st_as_sf(Lim_a)
#### propriedades ##################

p <- st_read("shapefiles/properties_limits.shp")

#---- plotando municipios sobre MA -----------

Fig1A <- ggplot()+
  # plot biome AF
  geom_sf(data = Biom,
          colour=NA)+
  # plot municipalities with coffe within the AF
  geom_sf(data = MunAdrian2,
          aes(fill = ID),colour="white", size=0.1)+
  # plot BR limits
  geom_sf(data = BR,fill = NA)+
  # bounding box
  geom_sf(data = Lim, fill = NA, color = "black", size = 1.2)+
  coord_sf(xlim = c(-60, -30), 
           ylim = c(-35, -10), 
           expand = FALSE)+
  theme_void()+
  theme(legend.position = c(0.2, 0.9),
        legend.title = element_blank())+
  theme(plot.margin=unit(c(0.5,-1,0.5,0.5), "cm"))
  


#---- plotando limite das fazendas ----------------

# isso define a margem dos plots

# plot.margin=unit(c(1.60,1,0.29,-1), "cm")
# ordem:
# t, r, b, l

farms <- ggplot()+
  geom_sf(data = Biom,colour=NA)+
  geom_sf(data=p,size=0.1)+
  coord_sf(xlim = c(-47.62647, -44.7), 
         ylim = c(-23.1, -20.5), 
         expand = FALSE)+
  annotation_scale(location = "br", width_hint = 0.5)+
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
   theme_void()+
  theme(plot.margin=unit(c(0.5,0.5,0.5,-1), "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))


fig_a_b <- ggarrange(Fig1A,farms,ncol = 2,labels = c("A","B"))

# save <- "D:\\Doutorado\\cap2\\manuscrito\\figuras\\"
# 
# ggsave(filename = paste0(save,"studysite.jpeg"),plot = fig_a_b,dpi = 300,width = 19,height = 14,units = "cm" )


#---- metricas -----------------------------------------------------------------

a <- ggboxplot(data =df_Adrian,x='ID',y = 'FC_m',fill ='ID',
               ylab="Forest cover (%)",xlab="",
               outlier.shape=NA)+
  #stat_summary(fun.y=mean, geom="point", shape=23, size=2)+
  #ylim(0,0.3)+
  theme(legend.position = "none")+
  guides(x = "none")+
  theme(plot.margin=unit(c(-1,0.5,0.5,0.5), "cm"))+
  coord_cartesian(ylim = c(0,0.3))

a <- ggpar(a,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"),
           font.xtickslab = c(8),font.ytickslab = c(8))+
  theme(legend.position = "none")

b <- ggboxplot(data =df_Adrian,x='ID',y = 'FC2km',fill ='ID',ylab="mean forest cover (% in 2km) ",
               xlab="",outlier.shape=NA)+
  #stat_summary(fun.y=mean, geom="point", shape=23, size=2)+
  #ylim(0,0.3)+
  theme(legend.position = "none")+
  guides(x = "none")+
  theme(plot.margin=unit(c(-1,0.5,0.5,0.5), "cm"))+
 coord_cartesian(ylim = c(0,0.3))

b <- ggpar(b,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"),
           font.xtickslab = c(8),font.ytickslab = c(8))

c <- ggboxplot(data =df_Adrian,x='ID',y = 'EUdist',fill ='ID',
               ylab="mean distance to forests (m)",xlab="",outlier.shape=NA)+
#  stat_summary(fun.y=mean, geom="point", shape=23, size=2)+
  #ylim(0,500)+
  theme(legend.position = "none")+
  guides(x = "none")+
  theme(plot.margin=unit(c(-1,0.5,0.5,0.5), "cm"))+
  coord_cartesian(ylim = c(0,500))

c <- ggpar(c,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"),
           font.xtickslab = c(8),font.ytickslab = c(8))


df_Adrian2 <- subset(df_Adrian, !is.na(df_Adrian$proporcao_Arabica))

df_Adrian2$Area_p <- df_Adrian2$area_total/df_Adrian2$area_total

# d <- ggstripchart(data =df_Adrian2,
#                   x = "ID",
#                   y = "proporcao_Canephora",
#                   ylab="Proportion of C. canephora",
#                   color =  'Principal',xlab="",
#                   palette = c("black", "gray"))+
#   theme(legend.title = element_blank())  +
#   guides(x = "none")+
#   theme(plot.margin=unit(c(-1,0.5,0.5,0.5), "cm"))



d <- ggstripchart(data =df_Adrian2,
             x = "ID",
             y = "proporcao_Canephora",
             ylab="Proportion of C. canephora",
             color ='ID',xlab="")+
  theme(legend.position = "none")  +
  guides(x = "none")+
  theme(plot.margin=unit(c(-1,0.5,0.5,0.5), "cm"))



d <- ggpar(d,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"),
           font.xtickslab = c(8),font.ytickslab = c(8),legend = NULL)

plots <- ggarrange(a,b,c, labels=c("C","D","E"),ncol=3,legend = NULL)

# plots <- ggarrange(a,b,c, labels=c("C","D","E"),ncol=3,legend = NULL,margin = list(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5))[[1]]

up <- ggarrange(Fig1A,farms)
down <- ggarrange(a,b,c,ncol=3)

#completa <- ggarrange(fig_a_b,plots,nrow = 2,heights = c(3, 1),legend = NULL)

# assim os labels ficam certos

completa <-ggarrange(up,down,nrow = 2,heights = c(3, 1),legend = NULL)+ 
  annotate("text", x = 0.03, y = 0.95, label = "A", fontface = "bold")+
  annotate("text", x = 0.03, y = 0.30, label = "C", fontface = "bold")+
  annotate("text", x = 0.45, y = 0.95, label = "B", fontface = "bold")+
  annotate("text", x = 0.365, y = 0.30, label = "D", fontface = "bold")+
  annotate("text", x = 0.70, y = 0.30, label = "E", fontface = "bold")



# ggarrange(fig_a_b,plots,nrow = 2,heights = c(3.5, 1),legend = NULL,
#           label.y = c(1,1,1.5,1.5,1.5,1.5))

# save <- "D:\\Doutorado\\cap2\\manuscrito\\figuras_novas\\"

ggsave(filename = "figures/fig1.jpeg",plot = completa,
       dpi = 300,width = 19,height = 18,units = "cm" )#18

# ggsave(filename = paste0("studysite_compl.jpeg"),plot = completa,
#        dpi = 300,width = 19,height = 18,units = "cm" )

#---- comparing median values ---------------------------------------------------

# ?ggboxplot
# 
# md_mun_other <- median(df_Adrian$FC_m[df_Adrian$ID=="other municipalities"])
# 
# md_mun_SA <- median(df_Adrian$FC_m[df_Adrian$ID=="study area"])
# 
# 
# md_fc_other <- median(df_Adrian$FC2km[df_Adrian$ID=="other municipalities"])
# 
# md_fc_SA <-median(df_Adrian$FC2km[df_Adrian$ID=="study area"])
# 
# md_fc_SA/md_fc_other
# 
# (md_fc_SA-md_fc_other)/md_fc_other
# 
# 
# 
# 
# md_dist_other <- median(df_Adrian$EUdist[df_Adrian$ID=="other municipalities"])
# md_dist_SA <- median(df_Adrian$EUdist[df_Adrian$ID=="study area"])
# 
# md_dist_SA
# 
# 0.1/0.4
# 
# 0.4/0.1
# 
# md_dist_other/md_dist_SA
# 
# (md_dist_other-md_dist_SA)/md_dist_SA
# 
# table(df_Adrian$ID,df_Adrian$Principal)
# 
# # proportion that have Canephora
# 
# 100/(510+100)
# 
# table(df_Adrian$ID,df_Adrian$proporcao_Canephora!=0)
# 
# # proportion that have predominance of Canephora
# 72/(72+538)
