#===============================================================================

# Figure 2. Pannel plot

#===============================================================================

#==== pacote ===================================================================

library(dplyr)
library(doBy)

#==============================================================================


# scenarios data frame 

cb <- read.csv("tables/results_df_noAPP_LRscen_scale_renamed_sen_renamed.csv")

# # ordering targets
# 
cb$target <- factor(cb$target, levels=c("present",
                                       "fl10",
                                       "fl20",
                                       "rl20",
                                       "fl30",
                                       "rl30",
                                       "fl40",
                                        "rl40"))

##### calculating costs and benefits #########################################

# total costs (o.c. restoration costs and compensation costs)
cb$total_cost <- cb$NPV_restoration+cb$restor_cost_f+cb$compensation_costs
# discounting o.c. of coffee areas, to avoid double counting then, since we are considering production as the response variable as well.
cb$total_cost_minusCoffee <- cb$total_cost - cb$oc_coffee_doll

# present target should have 0 value
cb$total_cost_minusCoffee[cb$target=='present'] <- 0

# scenarios with no opportunity cost must also have zero value

cb$total_cost_minusCoffee[cb$NPV_restoration==0] <- 0

summary(cb$total_cost_minusCoffee) # no negative costs - OK

# calculating benefits (comp. gains and differences in yield)

cb$total_benefits <- cb$Potential_comp_gain+cb$NPV_yield_d_n.progr
cb$c_b <- cb$total_benefits - cb$total_cost_minusCoffee

#### data on coffee field and yield ###########################################

# talhoes = coffee parcels

talhoes <- read.csv("tables/coffee_parcels_yield.csv")


#------------- yield  ---------------------------------------------------------

yield <- talhoes %>%
  group_by(target,scale)%>%
  summarise_at("Yield",funs(mean,sd,median))

# calculating se

yield$se <- NA

for(i in seq(1,nrow(yield),1)){
  
  yield$se[i] <- yield$sd[i]/sqrt(nrow(talhoes[talhoes$target==yield[[i,1]],]))
  
}

yield$max <- yield$mean+yield$se
yield$min <- yield$mean-yield$se


#### plotting ##################################################################

library(ggpubr)
library(ggrepel)
library(RColorBrewer)

RColorBrewer::brewer.pal(n = 3,name = "Spectral")

# paired
fill <- c("#A6CEE3" ,"#B2DF8A")
# spectral
# "#FC8D59" "#FFFFBF" "#99D594"

yl <- expression(atop("yield", paste("(60 kg bags", ~ha^-1,~y^-1,')')))

yield$target <- factor(yield$target, levels=c("present",
                                             "fl10",
                                             "fl20",
                                             "rl20",
                                             "fl30",
                                             "rl30",
                                             "fl40",
                                             "rl40"))


yield$scale <- factor(yield$scale, levels=c("farm-level","regional-level"))


yield_p <- ggplot(yield[yield$target!='present',], aes(x=target, y=mean, color=scale
  ,group = target)) + 
  geom_pointrange(aes(ymin=min, ymax=max),size=0.3,fatten = 2)+
  #geom_point(aes(x=target, y=yield_ha.median, color=scale, group = target), shape=23,size=4,show.legend = F)+
  #geom_text_repel(aes(label=target),hjust=0, vjust=1,angle=90,size = 2.5)+ 
  #scale_colour_viridis_d( option = "D",alpha = 0.7)+
  #scale_color_brewer(palette = "Paired")+
  # manually choosing paired pallete with colors easier to differentiate
  scale_colour_manual(values = fill)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  #theme(legend.position = "top",)+
  #theme(legend.position = "none")+
  labs(y= yl, x = "")+
  theme(axis.title = element_text(size = 14))+
  scale_y_continuous()+
  #scale_x_continuous(labels = scales::comma)+
  geom_hline(yintercept=yield$mean[yield$target=='present'], 
             linetype="dashed", color = "red",size=0.7)+ 
  annotate("rect", xmin = -Inf, xmax =Inf, 
           ymin =yield$min[yield$target=='present'], 
           ymax =yield$max[yield$target=='present'] ,
           alpha = .1)+
  rotate_x_text(45)+ 
  theme(legend.position = "none")


yield_p <- ggpar(yield_p,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"),
               font.xtickslab = c(8),font.ytickslab = c(8))



#------------- total production -----------------------------------------------

# converting to dollars/ha

cb$production <- NA
# convertion rate
conv_rate <- 3.21414

# value bag in MG

cb$production[grep(pattern = "MG",x = cb$ID)] <- (cb$total_bags[grep(pattern = "MG",x = cb$ID)]*7.127986*60)/conv_rate

# value bag in SP

cb$production[grep(pattern = "SP",x = cb$ID)] <- (cb$total_bags[grep(pattern = "SP",x = cb$ID)]*7.29*60)/conv_rate

# scalling production (when using farm area, its not necessary anymore)

#cb$production_1000 <- cb$production/1000

# scaling by property size

cb$production_1000 <- cb$production/cb$Area_Propriedade_ha


# average total production per scenario

avg_yield_total <- summaryBy(production_1000~target+scale,cb,FUN = c(mean,sd,median)) 
avg_yield_total$se <- NA

for(i in seq(1,nrow(avg_yield_total),1)){
  
  avg_yield_total$se[i] <- avg_yield_total$production_1000.sd[i]/sqrt(507)
  
}

avg_yield_total$max <- avg_yield_total$production_1000.mean+avg_yield_total$se
avg_yield_total$min <- avg_yield_total$production_1000.mean-avg_yield_total$se


#### plotting #################################################################

yl_prod <- expression(atop("total production", paste('(',"USD", ~y^-1,~ha^-1,')')))

avg_yield_total$target <- factor(avg_yield_total$targe, levels=c("present", 
                                                                 "fl10",
                                                                 "fl20",
                                                                 "rl20",
                                                                 "fl30",
                                                                 "rl30",
                                                                 "fl40",
                                                                 "rl40"))


avg_yield_total$scale <- factor(avg_yield_total$scale, 
                                levels=c("farm-level","regional-level"))

total_prod <- ggplot(avg_yield_total[avg_yield_total$target!="present",], 
        aes(x=target, y=production_1000.mean, color=scale, group = target)) + 
  geom_pointrange(aes(ymin=min, ymax=max),size=0.3,fatten = 2)+
  #geom_point(aes(x=target, y=total_bags.median, color=scale, group = target), shape=23,size=4,show.legend = F)+
  #geom_text_repel(aes(label=target),hjust=0, vjust=1,angle=90,size = 2.5)+ 
  #scale_colour_viridis_d( option = "D",alpha = 0.7)+
  #scale_color_brewer(palette = "Paired")+
  scale_color_manual(values=fill)+
  theme_bw()+
  #theme(legend.position = "top",legend.title = element_blank())+
  #theme(legend.position = "none")+
  labs(y= yl_prod, x = "")+
  theme(axis.title = element_text(size = 14))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_y_continuous(labels = scales::comma)+
  #scale_x_continuous(labels = scales::comma)+
  geom_hline(yintercept=mean(cb$production_1000[cb$target=="present"]), linetype="dashed", color = "red",size=0.7)+ 
  annotate("rect", xmin = -Inf, xmax =Inf, 
           ymin =avg_yield_total$min[avg_yield_total$target=='present'], 
           ymax =avg_yield_total$max[avg_yield_total$target=='present'] ,
           alpha = .1)+
  rotate_x_text(45)+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+ 
  theme(legend.position = "none")
  #theme(legend.title = element_blank())


total_prod <- ggpar(total_prod,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"),font.xtickslab = c(8),font.ytickslab = c(8))

#---- opportunity cost ---------------------------------------------------------

#### opportunity cost/ha ############################

# weighted by property area since this is a more usefull information. However, it's important to make it clear that evevery individual farm opportunity cost was divided by farm area so a farmer can have a better idea deppending on its farms on how much it would cost to undergo the scenarios

# original
cb$NPV_restoration_area <- cb$NPV_restoration/cb$Area_Propriedade_ha

# teste
#cb$NPV_restoration_area <- cb$NPV_restoration


avg_co <- summaryBy(NPV_restoration_area~target+scale,cb,FUN = c(mean,sd,median)) 
avg_co$se <- NA

for(i in seq(1,nrow(avg_co),1)){
  
  avg_co$se[i] <- avg_co$NPV_restoration_area.sd[i]/sqrt(507)
  
}

avg_co$max <- avg_co$NPV_restoration_area.mean+avg_co$se
avg_co$min <- avg_co$NPV_restoration_area.mean-avg_co$se

yl_co <- expression(atop("opportunity cost", paste('(',"USD", ~ha^-1,')')))

avg_co$target <- factor(avg_co$target, levels=c("present",
                                                "fl10",
                                                "fl20",
                                                "rl20",
                                                 "fl30",
                                                 "rl30",
                                                 "fl40",
                                                 "rl40" ))
                        


co <- ggplot(avg_co[avg_co$target!="present",], aes(x=target, y=NPV_restoration_area.mean, color=scale, group = target)) + 
  geom_pointrange(aes(ymin=min, ymax=max),size=0.3,fatten = 2)+
  #scale_color_brewer(palette = "Paired")+
  scale_color_manual(values = fill)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  #theme(legend.position = "top",legend.title = element_blank())+
  #theme(legend.position = "none")+
  labs(y= yl_co, x = "")+
  theme(axis.title = element_text(size = 14))+
  scale_y_continuous()+
  #scale_x_continuous(labels = scales::comma)+
  rotate_x_text(45)+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+ 
  theme(legend.position = "none")


co <- ggpar(co,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"),
                        font.xtickslab = c(8))



#---- restoration  cost --------------------------------------------------------

# tb testar aqui sem dividir por area (?)

# original
cb$restor_cost_f_area <- cb$restor_cost_f/cb$Area_Propriedade_ha
# without dividing by property area
#cb$restor_cost_f_area <- cb$restor_cost_f

avg_rest$restor_cost_f_area.sd[i]/sqrt(507)


avg_rest <- summaryBy(restor_cost_f_area~target+scale,cb,FUN = c(mean,sd,median)) 
avg_rest$se <- NA

for(i in seq(1,nrow(avg_rest),1)){
  
  avg_rest$se[i] <- avg_rest$restor_cost_f_area.sd[i]/sqrt(507)
  
}

#### plotting ##################################################################

avg_rest$max <- avg_rest$restor_cost_f_area.mean+avg_rest$se
avg_rest$min <- avg_rest$restor_cost_f_area.mean-avg_rest$se


yl_r <- expression(atop("restoration cost", paste('(',"USD", ~ha^-1,')')))


rest <- ggplot(avg_rest[avg_rest$target!="present",], aes(x=target, y=restor_cost_f_area.mean, color=scale, group = target)) + 
  geom_pointrange(aes(ymin=min, ymax=max),size=0.3,fatten = 2)+
  #scale_color_brewer(palette = "Paired")+
  scale_color_manual(values = fill)+
  theme_bw()+
  #theme(legend.position = "top",legend.title = element_blank())+
  #theme(legend.position = "none")+
  labs(y= yl_r, x = "")+
  theme(axis.title = element_text(size = 14))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_y_continuous()+
  #scale_x_continuous(labels = scales::comma)+
  rotate_x_text(45)+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+ 
  theme(legend.position = "none")


rest <- ggpar(rest,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"),
                          font.xtickslab = c(8),font.ytickslab = c(8))


#---- ccompensation gains ---- -------------------------------------------------


cb$compensation_costs_area <- cb$compensation_costs/cb$Area_Propriedade_ha

custos_comp <- summaryBy(compensation_costs_area~target+scale,cb,FUN = c(mean,sd,median)) 
custos_comp$se <- NA

for(i in seq(1,nrow(custos_comp),1)){
  
  custos_comp$se[i] <- custos_comp$compensation_costs_area.sd[i]/sqrt(507)
  
}

#### plotting ##################################################################

custos_comp$max <- custos_comp$compensation_costs_area.mean+custos_comp$se
custos_comp$min <- custos_comp$compensation_costs_area.mean-custos_comp$se

yl_c_comp <- expression(atop("compensation cost", paste('(',"USD", ~ha^-1,')')))


custos_comp_plot <- ggplot(custos_comp[avg_rest$target!="present",], aes(x=target, y=compensation_costs_area.mean, color=scale, group = target)) + 
  geom_pointrange(aes(ymin=min, ymax=max),size=0.3,fatten = 2)+
  #scale_color_brewer(palette = "Paired")+
  scale_color_manual(values = fill)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  #theme(legend.position = "top",legend.title = element_blank())+
  #theme(legend.position = "none")+
  labs(y= yl_c_comp, x = "")+
  theme(axis.title = element_text(size = 14))+
  scale_y_continuous()+
  #scale_x_continuous(labels = scales::comma)+
  rotate_x_text(45)+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+ 
  theme(legend.position = "none")


custos_comp_area <- ggpar(custos_comp_plot,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"),
                          font.xtickslab = c(8),font.ytickslab = c(8))



#---- compensation gains --------------------------------------------------------


cb$Potential_comp_gain_area <- cb$Potential_comp_gain/cb$Area_Propriedade_ha


ganhos_comp <- summaryBy(Potential_comp_gain_area~target+scale,cb,FUN = c(mean,sd,median)) 
ganhos_comp$se <- NA

for(i in seq(1,nrow(ganhos_comp),1)){
  
  ganhos_comp$se[i] <- ganhos_comp$Potential_comp_gain_area.sd[i]/sqrt(507)
  
}

ganhos_comp$max <- ganhos_comp$Potential_comp_gain_area.mean+ganhos_comp$se
ganhos_comp$min <- ganhos_comp$Potential_comp_gain_area.mean-ganhos_comp$se


yl_g_comp <- expression(atop("compensation gain", paste('(',"USD", ~ha^-1,')')))


ganhos_comp_plot <- ggplot(ganhos_comp[avg_rest$target!="present",], aes(x=target, y=Potential_comp_gain_area.mean, color=scale, group = target)) + 
  geom_pointrange(aes(ymin=min, ymax=max),size=0.3,fatten = 2)+
  #scale_color_brewer(palette = "Paired")+
  scale_color_manual(values = fill)+
  theme_bw()+
  #theme(legend.position = "top",legend.title = element_blank())+
  #theme(legend.position = "none")+
  labs(y= yl_g_comp, x = "")+
  theme(axis.title = element_text(size = 14))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_y_continuous()+
  #scale_x_continuous(labels = scales::comma)+
  rotate_x_text(45)+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+ 
  theme(legend.position = "none")


beneficos_comp_area <- ggpar(ganhos_comp_plot,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"),
                             font.xtickslab = c(8),font.ytickslab = c(8))


#---- changes in coffee area --------------------------------------------------

# total coffee area per scenario

coff_total <- summaryBy(area_coffe_field_ha~target+scale,cb,FUN = c(sum,mean,sd,median)) 

coff_total$perc <- coff_total$area_coffe_field_ha.sum/
  coff_total$area_coffe_field_ha.sum[coff_total$target=="present"]

coff_total$perc_loss <- (1-coff_total$perc )*100

coff_total$target <- factor(coff_total$target, levels=c("present",
                                                        "fl10",
                                                        "fl20",
                                                        "rl20",
                                                        "fl30",
                                                        "rl30",
                                                        "fl40",
                                                        "rl40"))


yl <- "coffee area loss (%)"

coffee <- ggbarplot(data = coff_total[coff_total$target!='present',],
                    x = 'target',y = 'perc_loss',fill='scale',color = "transparent")+
  scale_fill_manual(values = fill)+
  #scale_fill_brewer(palette = "Paired")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  #theme(legend.position = "top",)+
  #theme(legend.position = "none")+
  labs(y= yl, x = "")+
  theme(axis.title = element_text(size = 14))+
  scale_y_continuous()+
  #scale_x_continuous(labels = scales::comma)+
  #geom_hline(yintercept=34.40393, linetype="dashed", color = "red",size=0.7)+ 
  rotate_x_text(45)+ 
  theme(legend.position = "none")
  



coffee_plot <- ggpar(coffee,font.x = c(8,  "black"),font.y = c(8, "black"),
                  font.xtickslab = c(8),font.ytickslab = c(8),font.legend=c(6,"black"))


#---- creating legend--------------------------------------------------------


avg_co$target <- factor(avg_co$targe, levels=c("present","fl10","fl20","rl20","fl30","rl30","fl40","rl40"))

avg_co$scale <- factor(avg_co$scale, levels=c("farm-level","regional-level"))


p <- ggplot() + geom_point(data=avg_co[avg_co$target!="present",], 
                           aes(x=target, y=NPV_restoration_area.mean, color=scale), shape=15, size=5)+
  scale_color_manual(values = fill)
  #scale_color_brewer(palette = "Paired")


p <- p + guides(color=guide_legend(title=NULL)) 
p <- p + theme(legend.key = element_blank())

p <- ggpar(p,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"),
           font.xtickslab = c(8),font.ytickslab = c(8),font.legend=c(8,"black"))


l <- get_legend(p)

plot(l)


#---- pannel plot -------------------------------------------------------------


panels_area <- ggarrange(co,rest,coffee_plot,yield_p,total_prod,custos_comp_area,beneficos_comp_area,l,
                          labels = c("A","B","C","D","E","F","G",""))


ggsave(filename ="figures/fi2.jpg",
       plot = panels_area,units = "cm",width = 19,height =16,"jpeg")
