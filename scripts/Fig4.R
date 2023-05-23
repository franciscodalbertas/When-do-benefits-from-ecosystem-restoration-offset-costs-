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

#==============================================================================

# scenarios data frame 

cb <- read.csv("tables/results_df_noAPP_LRscen_scale_renamed_sen_renamed_final.csv")



avg_cost <- summaryBy(c_b~target+scale,cb,FUN = c(mean,sd)) 
avg_cost$se <- NA
for(i in seq(1,nrow(avg_cost),1)){
  
  avg_cost$se[i] <- avg_cost$c_b.sd[i]/sqrt(507)
  
}

avg_cost$max <- avg_cost$c_b.mean+avg_cost$se
avg_cost$min <- avg_cost$c_b.mean-avg_cost$se

restored_area <- summaryBy(area_rest_ha~target,cb,FUN = sum) 

df <- cbind(avg_cost,restored_area[,2])

names(df)[8] <- "restored_area_ha"

mean_pres <- mean(cb$NPV_yield_d_pres)

df$prop <- df$c_b.mean/mean_pres

df$max_prop <- df$max/mean_pres

df$min_prop <- df$min/mean_pres

#---- plot cashflow geral -----------------------------------------------------

yl_cash <- "total cashflow (USD) x 1000"
xl_cash <- expression("area restored ("~ha^-1~')')


df$target <- factor(df$target, levels=c("present", "APP", "farmers","fl10","fl20","rl20","fl30","rl30","fl40","rl40"))


cost_benefit <- df %>%
  mutate(c_b.mean1000=c_b.mean/1000)%>%
  mutate(min=min/1000)%>%
  mutate(max=max/1000)%>%
  filter(target!="present")%>%
  ggplot(aes(x=restored_area_ha, 
             y=c_b.mean1000,
             color=target, group = target)) + 
  geom_pointrange(aes(ymin=min, ymax=max), position=position_dodge(width = 800))+
  scale_color_brewer(palette = "Paired")+
  theme_bw()+
  theme(legend.position = "none",legend.title = element_blank())+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(legend.position = "right")+
  labs(y= yl_cash, x = xl_cash)+
  theme(axis.title = element_text(size = 14))+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  geom_hline(yintercept=0, linetype="dashed", color = "red",size=0.7)+
  ggtitle("")
  
cost_benefit <- ggpar(cost_benefit,
                      font.x = c(8, "bold", "black"),
                      font.y = c(8, "bold", "black"),
            font.xtickslab = c(8),font.ytickslab = c(8),
            font.legend=c(6,"black"),
            legend.title ="",
            axis.title.x=element_text(face = "bold"))


#---- adicionando vegetacao em % --------------------------------------------

xl_cash2 <- expression("area restored ("~ha^-1~')')

veg_area <- summaryBy(Veg_posrest_ha~target,cb,FUN = sum)


veg_area[,2]-restored_area[,2]


present <- cb[cb$scale=="present",]

a_total <- sum(present$Area_Propriedade_ha)

cost_benefit_veg <- cost_benefit + 
  scale_x_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ ./a_total+(17687.13/a_total),
                     name = "proportion of the area forested"))+
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~ ./mean_pres,
                      name = "",
                      breaks = seq(0, -0.15, by = -0.01)))



cost_benefit_veg <- ggpar(cost_benefit_veg,
                          font.x = c(8, "bold", "black"),
                          font.y = c(8, "bold", "black"),
              font.xtickslab = c(8),
              font.ytickslab = c(8),
              font.legend=c(6,"black"),
              legend.title ="")

# salvar figura como potencial supp. decorrente da figura 4

ggsave(filename ="figures/fig4_A_abscash.jpg",
       plot = cost_benefit_veg,
       units = "cm",
       width = 8, #10
       height =7,  #8
       "jpeg")

#---- substituir y absoluto por y relativo ------------------------------------

yl_cash2 <- "cash flow variation"

mean_pres <- mean(cb$NPV_yield_d_pres)

df$prop_var <- df$c_b.mean/mean_pres
df$max <- df$max/mean_pres
df$min <- df$min/mean_pres


cost_benefit_prop <- df %>%
  filter(target!="present")%>%
  ggplot(aes(x=restored_area_ha, 
             y=prop_var,
             color=target, group = target)) + 
  geom_pointrange(aes(ymin=min, ymax=max), position=position_dodge(width = 800))+
  scale_color_brewer(palette = "Paired")+
  theme_bw()+
  theme(legend.position = "none",legend.title = element_blank())+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(legend.position = "right")+
  labs(y= yl_cash2, x = xl_cash)+
  theme(axis.title = element_text(size = 14))+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  geom_hline(yintercept=0, linetype="dashed", color = "red",size=0.7)+
  ggtitle("")

cost_benefit_prop <- ggpar(cost_benefit_prop,
                      font.x = c(8, "bold", "black"),
                      font.y = c(8, "bold", "black"),
                      font.xtickslab = c(8),font.ytickslab = c(8),
                      font.legend=c(6,"black"),
                      legend.title ="",
                      axis.title.x=element_text(face = "bold"))

cost_benefit_prop_veg <- cost_benefit_prop + 
  scale_x_continuous(labels = scales::comma,
  sec.axis = sec_axis(~ ./a_total+(17687.13/a_total),
  name = "proportion of the area forested"))+
  scale_y_continuous(labels = scales::comma,
  sec.axis = sec_axis(~ ./mean_pres,
  name = "",
  breaks = seq(0, -0.15, by = -0.01)))

cost_benefit_prop_veg <- ggpar(cost_benefit_prop_veg,
                          font.x = c(8, "bold", "black"),
                          font.y = c(8, "bold", "black"),
                          font.xtickslab = c(8),
                          font.ytickslab = c(8),
                          font.legend=c(6,"black"),
                          legend.title ="")


ggsave(filename ="figures/fig4_A_propcash.jpg",
       plot = cost_benefit_prop_veg,
       units = "cm",
       width = 7, #10
       height =6,  # 6
       "jpeg")
#---- separando cash flow em outras variaveis, como vegetação ------------------

q1 <-  summary(cb$PropVeg)[2]
m <- median(cb$PropVeg)
q3 <- summary(cb$PropVeg)[5]

cb$VegCat <- NA

cb$VegCat[cb$PropVeg<=m] <- 'b'
#cb$VegCat[cb$PropVeg>m&cb$PropVeg<=q3] <- 'media'
cb$VegCat[cb$PropVeg>m] <- 'a'

nrow(cb[cb$PropVeg<=m&cb$target=='present',])
nrow(cb[cb$PropVeg>m&cb$target=='present',])

avg_cost2 <- summaryBy(c_b~target+scale+VegCat,cb,FUN = c(mean,sd)) 
avg_cost2$se <- NA

for(i in seq(1,nrow(avg_cost2),1)){
  
  avg_cost2$se[i] <- avg_cost2$c_b.sd[i]/sqrt(507)
  
}

avg_cost2$max <- avg_cost2$c_b.mean+avg_cost2$se
avg_cost2$min <- avg_cost2$c_b.mean-avg_cost2$se


restored_area2 <- summaryBy(area_rest_ha~target+VegCat,cb,FUN = sum) 

df2 <- cbind(avg_cost2,restored_area2[,3])

names(df2)[9] <- "restored_area_ha"

mean_pres_a <- mean(cb$NPV_yield_d_pres[cb$VegCat=="high"])
mean_pres_m <- mean(cb$NPV_yield_d_pres[cb$VegCat=="medium"])
mean_pres_b <- mean(cb$NPV_yield_d_pres[cb$VegCat=="low"])

df2$prop[df2$VegCat=='high'] <- df2$c_b.mean[df2$VegCat=='high']/mean_pres_a
df2$prop[df2$VegCat=='medium'] <- df2$c_b.mean[df2$VegCat=='medium']/mean_pres_b
df2$prop[df2$VegCat=='low'] <- df2$c_b.mean[df2$VegCat=='low']/mean_pres_b

df2$max_prop[df2$VegCat=='low'] <- df2$max[df2$VegCat=='low']/mean_pres_b

df2$min_prop[df2$VegCat=='high'] <- df2$min[df2$VegCat=='high']/mean_pres_a

df2$min_prop[df2$VegCat=='medium'] <- df2$min[df2$VegCat=='medium']/mean_pres_a


df2$prop_var <- df2$c_b.mean/mean_pres

df2$target <- factor(df2$target, levels=c("present", "APP", "farmers","fl10","fl20","rl20","fl30","rl30","fl40","rl40"))

plot_low_veg <- df2%>%
  mutate(max=max/mean_pres)%>%
  mutate(min=min/mean_pres)%>%
  filter(target!="present"&VegCat=="b")%>%
  ggplot(aes(x=target,
             y=prop_var,
             color=target, 
             group = target)) + 
  geom_pointrange(aes(ymin=min, ymax=max), 
                  #position=position_dodge(width = 800), 
                  show.legend = FALSE)+
  scale_color_brewer(palette = "Paired")+
  theme_bw()+
  theme(legend.position = "none",legend.title = element_blank())+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  #theme(legend.position = "none")+
  labs(y= yl_cash2, x = "")+
  theme(axis.title = element_text(size = 14))+
  scale_y_continuous(breaks = seq(-0.2,0.07,0.04),limits = c(-0.2,0.07))+
  #scale_x_continuous(labels = scales::comma)+
  geom_hline(yintercept=0, linetype="dashed", color = "red",size=0.7)+
  ggtitle("<10% forest cover")


plot_low_veg <- ggpar(plot_low_veg,font.y = c(8, "bold", "black"),font.ytickslab = c(8),font.main = c(9,"bold"))


plot_high_veg <- df2%>%
  mutate(max=max/mean_pres)%>%
  mutate(min=min/mean_pres)%>%
  filter(target!="present"&VegCat=="a")%>%
  ggplot(aes(x=target,
             y=prop_var,
             color=target, 
             group = target)) + 
  geom_pointrange(aes(ymin=min, ymax=max), 
                  #position=position_dodge(width = 800), 
                  show.legend = FALSE)+
  scale_color_brewer(palette = "Paired")+
  theme_bw()+
  theme(legend.position = "none",legend.title = element_blank())+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  #theme(legend.position = "none")+
  labs(y= yl_cash2, x = "")+
  theme(axis.title = element_text(size = 14))+
  scale_y_continuous(breaks = seq(-0.2,0.07,0.04),limits = c(-0.2,0.07))+
  #scale_x_continuous(labels = scales::comma)+
  geom_hline(yintercept=0, linetype="dashed", color = "red",size=0.7)+
  ggtitle(">10% forest cover")

plot_high_veg <- ggpar(plot_high_veg,font.y = c(8, "bold", "black"),font.ytickslab = c(8),font.main = c(9,"bold"))


# (>10% forest cover)
#(>10% forest cover
combined <- ggarrange(plot_low_veg,plot_high_veg,labels = c("A","B"))

#---- adicionar carbono --------------------------------------------------------

# convertendo pra CO2 

cb$TonCO2 <- cb$TonC*(44/12)

# calcular retorno do C

valores <- c(5,20,25,30,35,80)

f <- function(x)x*cb[,which(names(cb)=="TonCO2")]

df_c <- cbind(cb,do.call(cbind,lapply(valores,f)))

names(df_c)[55:60] <- c("5_USD","20_USD","25_USD","30_USD","35_USD","80_USD")

df_c$c_b_sum_5 <- df_c$c_b +df_c$`5_USD`
df_c$c_b_sum_20 <- df_c$c_b +df_c$`20_USD`
df_c$c_b_sum_25 <- df_c$c_b +df_c$`25_USD`
df_c$c_b_sum_30 <- df_c$c_b +df_c$`30_USD`
df_c$c_b_sum_35 <- df_c$c_b +df_c$`35_USD`
df_c$c_b_sum_80 <- df_c$c_b +df_c$`80_USD`

which(names(df_c)=="c_b")
which(names(df_c)=="c_b_sum_5")

custos <- names(df_c)[c(52,61:66)]

avg_cost_mean<- df_c %>%
  group_by(target,scale)%>%
  summarise_at(noquote(custos),c(mean))

avg_cost_mean_l <- pivot_longer(data = avg_cost_mean,cols = 3:9)

avg_cost_sd<- df_c  %>%
  group_by(target,scale)%>%
  summarise_at(noquote(custos),c(sd))


avg_cost_sd_l <- pivot_longer(data = avg_cost_sd,cols = 3:9)


avg_cost <- left_join(avg_cost_mean_l,avg_cost_sd_l,by=c("target","scale","name"))

names(avg_cost)[4:5] <- c("mean","sd")

avg_cost$se <- NA

for(i in seq(1,nrow(avg_cost),1)){
  
  avg_cost$se[i] <- avg_cost$sd[i]/sqrt(507)
  
}

avg_cost$max <- avg_cost$mean+avg_cost$se
avg_cost$min <- avg_cost$mean-avg_cost$se


#---- plotando -----------------------------------------------------------------


avg_cost$target <- factor(avg_cost$target, levels=c("present","fl10","fl20","rl20","fl30","rl30","fl40","rl40"))

avg_cost$name <- factor(avg_cost$name, levels=c("c_b","c_b_sum_5","c_b_sum_20",
                                                "c_b_sum_25","c_b_sum_30","c_b_sum_35","c_b_sum_80"))


yl_cash <- "total cashflow (USD)x1000"
xl_cash <- expression("")

carbon_names <- list(
  'c_b'="no carbon",
  'c_b_sum_5'="5 USD carbon",
  'c_b_sum_20'="20 USD carbon",
  'c_b_sum_25'="25 USD carbon",
  'c_b_sum_30'="30 USD carbon",
  'c_b_sum_35'="35 USD carbon",
  'c_b_sum_80'="80 USD carbon"
)

carbon_labeller <- function(variable,value){
  return(carbon_names[value])
}

# ordering the plots

avg_cost$target <- factor(avg_cost$target, levels=c("present","fl10","fl20","rl20","fl30","rl30","fl40","rl40"))

carbon_pannel <- avg_cost %>%
  filter(target!="present")%>%
  ggplot(aes(x=target, y=mean,color=target, group = target)) + 
  geom_pointrange(aes(ymin=min, ymax=max),  
                  show.legend = F)+
  #geom_text_repel(aes(label=target),hjust=0, vjust=1,angle=90,size = 2.5)+ 
  #scale_colour_viridis_d( option = "B",alpha=0.7)+
  #scale_color_npg()+
  scale_color_brewer(palette = "Paired")+
  theme_bw()+
  theme(legend.position = "top",legend.title = element_blank())+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  #theme(legend.position = "none")+
  labs(y= yl_cash, x = xl_cash)+
  theme(axis.title = element_text(size = 14))+
  scale_y_continuous(labels =  function(y) y / 1000)+
  #scale_x_continuous(labels = scales::comma)+
  geom_hline(yintercept=0, linetype="dashed", color = "red",size=0.7)+
  facet_wrap(~name,labeller=carbon_labeller  )+ 
  theme(strip.background = element_blank())

carbon_panel <- ggpar(carbon_pannel,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"),
               font.xtickslab = c(8),font.ytickslab = c(8),axis.title.x=element_text(face = "bold"))

ggsave(filename = "figures/figS7.jpg",plot = carbon_panel,width = 13,height = 15,units = "cm")


# filtrar so carbon 20

carbon_20 <- avg_cost %>%
  filter(target!="present")%>%
  filter(name=="c_b_sum_20") %>%
  mutate(mean=mean/mean_pres*1000)%>%
  mutate(max=max/mean_pres*1000)%>%
  mutate(min=min/mean_pres*1000)%>%  
ggplot(aes(x=target, y=mean,color=target, group = target)) + 
  geom_pointrange(aes(ymin=min, ymax=max),  
                  show.legend = F)+
  #geom_text_repel(aes(label=target),hjust=0, vjust=1,angle=90,size = 2.5)+ 
  #scale_colour_viridis_d( option = "B",alpha=0.7)+
  #scale_color_npg()+
  scale_color_brewer(palette = "Paired")+
  theme_bw()+
  theme(legend.position = "top",legend.title = element_blank())+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  #theme(legend.position = "none")+
  labs(y= yl_cash2, x = xl_cash)+
  theme(axis.title = element_text(size = 14))+
  scale_y_continuous(labels =  function(y) y / 1000,limits =c(-200,70),breaks = seq(-200,70,40))+
  #scale_x_continuous(labels = scales::comma)+
  geom_hline(yintercept=0, linetype="dashed", color = "red",size=0.7)+
  #facet_wrap(~name,labeller=carbon_labeller  )+ 
  theme(strip.background = element_blank())+
  ggtitle("carbon payment (20 USD/ton)")


carbon_20 <- ggpar(carbon_20,font.y = c(8, "bold", "black"),font.ytickslab = c(8),font.main = c(9,"bold"))

#combined <- ggarrange(plot_low_veg,plot_high_veg,labels = c("A","B"))
panel_results <- ggarrange(plot_low_veg,plot_high_veg,carbon_20,labels = c("A","B","C"))

panel_results2 <- ggarrange(plot_low_veg,plot_high_veg,carbon_20,labels = c("A","B","C"),ncol = 3, heights = c(1, 1, 1))

# nao ta alinhado

library(egg)

# Align the plots horizontally

panel_results2 <- ggarrange(plot_low_veg,plot_high_veg,carbon_20,labels = c("A","B","C"),ncol = 3)


ggsave(filename = "figures/fig4_C.jpg",plot = panel_results2,width = 19,height = 7,units = "cm")
