#===============================================================================

# Figure 4. cashflow 

#===============================================================================

#==== pacote ===================================================================

library(dplyr)
library(doBy)
library(tidyr)
library(ggallin)
library(scales)
library(ggpubr)
library(ggrepel)
library(doBy)
library(scales)
library(sf)
library(rcartocolor)
library(ggpubr)
library(RColorBrewer)

#============================================================================
#"D:\\Doutorado\\cap2\\analise_fragmentacao"

pforest <- "C:/Users/falbe/OneDrive/Doutorado/cap2/analise_fragmentacao"

l <- list.files(path = pforest,pattern = ".shp")


l <- grep(list.files(pforest,pattern = "forest_property.shp",
     full.names = T),pattern = ".xml",invert = T,value = T)


prop <- st_read("shapefiles/properties_limits.shp")

nomes <- c("fl10","fl20","fl30","fl40","rl20","rl30","rl40","present")


crs <- st_crs(prop)

df <- list()

c <- 0
for(i in 3:10){
  c <- c+1
  p <- st_read(l[i])
  p <- st_transform(p,crs = crs)
  p_c <- st_intersection(p,prop) # cortando so pra dentro das propriedades
  p_c$area <- st_area(p_c)
  p_c$area_log <- log10(p_c$area)
  attributes(p_c$area) <- NULL
  attributes(p_c$area_log) <- NULL
  st_geometry(p_c) <- NULL
  p_c$scen <- nomes[c]
  df[[c]] <- p_c

}

df <- do.call(rbind,df)

summary(as.factor(df$scen))

df$area_ha <- df$area/10000
n.frag <- data.frame(n.frag=table(df$scen))

head(df)

write.csv(df,"tables/forest_configuration_properties.csv",row.names = F)

#---- classes de tamanho -------------------------------------------------------

df$area_ha_cl <- cut(x =df$area_ha,breaks = c(0,10,100,500,1000,4000) )


#---- summary por area ---------------------------------------------------------

area <- summaryBy(area~scen,FUN = "sum",df)

df2 <- cbind(n.frag,area[,2])
names(df2) <- c("scen","n.frag","area_m2")

df2$area_ha <- df2$area_m2/10000


#---- plotando -------------------------


# aqui eh area total dos fragmentos que intersectam as propriedades
# e n. total desses fragmentos!

#### plotando stacked barplot com classses stacked por scen ####################


df4 <- summaryBy(area_ha~scen+area_ha_cl,df,FUN = sum) 


totals <- summaryBy(area_ha~scen,df,FUN = sum) 

# ta ok toodo mundo da 100! pq o grafico sai distorcido?? nao sei!

df4$prop <- NA
for(i in 1:8){
  
  df4$prop[df4$scen==nomes[i]] <- df4$area_ha.sum[df4$scen==nomes[i]]/totals$area_ha.sum[totals$scen==nomes[i]]
  
}


df4$scen <- as.character(df4$scen)

#---- plotando numero fragmentos ----------------------------------------------------

# seria interessante um grafico da area acumulada pelo numero de fragmentos!
# nao ficou mto legal, nao da pra ver nenhum padrao!

# ordenando o dataframe

df_acum <-df[order(df$area_ha, decreasing = T),]

df_acum$n.frag <- NA

nomes <- c("present","fl10","fl20","fl30","fl40","rl20","rl30","rl40")

for(i in seq(1:7)){

df_acum$n.frag[df_acum$scen==nomes[i]] <- seq(1,n.frag[n.frag$n.frag.Var1==nomes[i],2],1)

}
# calculando area acumulada

df_acum = mutate(group_by(df_acum), cumsum=cumsum(area_ha))

df5 <- data.frame(table(df$area_ha_cl,df$scen))

df5$Freqlog10 <- log10(df5$Freq)


n.frag$n.frag.Var1 <- as.character(n.frag$n.frag.Var1)
n.frag$n.frag.Var1[n.frag$n.frag.Var1=="APPLR"] <- "farmers"

head(n.frag)

#-------- sugestoes Jean -------------------------------------------------------


##### re-ordenano scenarios

df6 <- df4

summary(df6$area_ha_cl)

str(df6$scen)

df6$scen[df6$scen=="present"] <- "baseline"

df6$area_ha_cl <- factor(df6$area_ha_cl,levels = rev(levels(df6$area_ha_cl)))

df6$area_ha_cl <- factor(df6$area_ha_cl,levels = (levels(df6$area_ha_cl)))

df6$scen <- factor(df6$scen,levels = c("baseline","fl10","fl20","rl20","fl30","rl30","fl40","rl40"))





labels <-  c("<10 (ha)", "10-100", "100-500","500-1000")

rev(labels)

library(matlib)
as.numeric(df6$area_ha_cl)

p2 <- ggplot(df6, aes(x = scen, y = round(x = area_ha.sum,digits = 2), 
                      label = round(prop,2),fill = area_ha_cl,
                      order = as.numeric(area_ha_cl))) +
  #ggtitle("fragments area (ha)")+
  geom_bar(stat = "identity") +
  # scale_fill_viridis_d(option = "D",
  #                      name = "",
  #                      labels = c("<10", "10-100", "100-500","500-1000",">1000"))+
  scale_fill_brewer(palette = "Paired",
name = "",
labels = rev(labels))+
  geom_text(size = 2.5,colour="black", position = position_stack(vjust = 0.5), fontface = "bold")+
  coord_flip()+
  theme_pubclean()+
  theme(#axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank())+
  #scale_fill_carto_d(palette = 'ArmyRose')
  scale_y_continuous(labels = scales::comma)+
  ylab("(a) total forest area (ha)")+
  guides(fill = guide_legend(reverse = TRUE))

cf2 <- ggpar(p2,font.ytickslab = c(8,"bold", "black"),font.xtickslab = c(8, "gray"),
    font.legend=c(6,"black"),font.main=c(8,"gray"),font.x = c(8,'bold','black'))+
  theme(axis.line.x = element_line(size = 1, colour = "gray", linetype=1))


l <- get_legend(cf2)


#### sem legenda ############3


p3 <- ggplot(df6, aes(x = scen, y = round(x = area_ha.sum,digits = 2), label = round(prop,2),fill = area_ha_cl,
                      order = -as.numeric(area_ha_cl))) +
  #ggtitle("fragments area (ha)")+
  geom_bar(stat = "identity") +
  # scale_fill_viridis_d(option = "D",
  #                      name = "",
  #                      labels = c("<10", "10-100", "100-500","500-1000",">1000"))+
  scale_fill_brewer(palette = "Paired",
                    name = "",
                    labels = labels)+
  geom_text(size = 2.5,colour="black", position = position_stack(vjust = 0.5), fontface = "bold")+
  coord_flip()+
  theme_pubclean()+
  theme(#axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.margin=unit(c(0.25,0.1,0.29,0.8), "cm"))+
  #scale_fill_carto_d(palette = 'ArmyRose')
  scale_y_continuous(labels = scales::comma)+
  ylab("(a) total forest area (ha)")+
  guides(fill = guide_legend(reverse = TRUE))+ theme(legend.position = "none")


cf3 <- ggpar(p3,font.ytickslab = c(8,"bold", "black"),font.xtickslab = c(8, "gray"),
      font.main=c(8,"gray"),font.x = c(8,'bold','black'))+
  theme(axis.line.x = element_line(size = 1, colour = "gray", linetype=1))


n.frag2 <- n.frag


n.frag2$n.frag.Var1 <- factor(n.frag2$n.frag.Var1,levels = c("APP", "farmers","fl10","fl20","rl20","fl30","rl30","fl40","rl40"))

p4 <- ggplot(n.frag2, aes(x = n.frag.Var1, y = n.frag.Freq)) +
  #ggtitle("number of fragments")+
  geom_bar(stat = "identity",fill="cornsilk2") +
  # scale_fill_viridis_d(option = "D",
  #                      name = "",
  #                      labels = c("<10", "10-100", "100-500","500-1000",">1000"))+
  #geom_text(size = 2,colour="black", position = position_stack(vjust = 0.5), fontface = "bold")+
  coord_flip()+
  theme_pubclean()+
  theme(#axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.margin=unit(c(0.25,1,0.35,-1), "cm"))+
  ylab("(b) number of fragments")+
  scale_y_continuous(labels = scales::comma)
#scale_fill_carto_d(palette = 'ArmyRose')

# order margin: t, r, b, l

p5 <- ggpar(p4,font.xtickslab = c(8, "gray"),font.main=c(8,"gray"),font.ytickslab = FALSE,
        font.x = c(8,'bold','black'))+
  theme(axis.line.x = element_line(size = 1, colour = "gray", linetype=1) )


library(egg)

p6 <- ggarrange(cf3,p5,widths = c(2, 1))#+
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank(),
  #       axis.title.y=element_blank(),
  #       axis.text.y=element_blank(),
  #       axis.ticks.y=element_blank(),
  #       )
#+ 
  #theme(plot.margin = unit(c(0.5,0.25,0.25,1), "cm")) #aqui ta lines, em cima, cm!

#theme(plot.margin = unit(c(0.5,0.25,0.25,0.25)

detach("package:egg",unload = T)

p7 <- ggarrange(l,p6, nrow = 2,heights = c(1,30),widths = c(1,1))


ggsave(filename =paste0(path2,"/fig_areafrag_nfragv_new.jpg"),plot = p7,units = "cm",width = 14,height =9,"jpeg")
