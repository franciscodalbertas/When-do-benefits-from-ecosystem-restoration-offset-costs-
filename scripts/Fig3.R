#===============================================================================

# Figure 3. cashflow balance

#===============================================================================

#==== pacote ===================================================================

library(dplyr)
library(doBy)
library(tidyr)
library(ggallin)
library(scales)
library(ggpubr)

#==============================================================================

# scenarios data frame 

cb <- read.csv("tables/results_df_noAPP_LRscen_scale_renamed_sen_renamed.csv")


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

head(cb)

# saving final df

write.csv(cb,"tables/results_df_noAPP_LRscen_scale_renamed_sen_renamed_final.csv",row.names = F)

#----  divergent barchart ------------------------------------------------------

# categorizing costs and benefits

cb$NetChange_cat <- ifelse(cb$NPV_yield_d_n.progr<0,yes = "n",no = "p") 

# aggregating data x scen

cb_agg <- cb%>%
  group_by(target,scale,NetChange_cat)%>%
  summarise(changes_production=sum(NPV_yield_d_n.progr),
            res_costs=sum(restor_cost_f),oc=sum(NPV_restoration),
              comp_cost=sum(compensation_costs),comp_gains=sum(Potential_comp_gain))


# Reshape data from wide to long format
cb_long <- cb_agg %>%
  pivot_longer(cols = 4:8)%>%
  mutate(cat=if_else(name=="changes_production"& NetChange_cat=="n","negative net changes",if_else(name=="changes_production"& NetChange_cat=="p","positive net changes",name)))%>%
  filter(target!="present")%>%
# colocando custos como valores negativos
 mutate(value=if_else(cat=="comp_gains"|cat=="positive net changes",false = value*-1,true=value))%>% mutate(value=if_else(cat=="negative net changes",false = value,true=value*-1))%>%
  mutate(value_1000=value/1000)
  
cb_long2 <- cb_long %>% 
  group_by(target,cat)%>%
  summarise(value_1000=sum(value_1000),value=sum(value))
  
# aggregating again

# defining orders!

cb_long2$target <- factor(cb_long2$target, levels=c("fl10",
                                       "fl20",
                                       "rl20",
                                       "fl30",
                                       "rl30",
                                       "fl40",
                                       "rl40"))


cb_long2$cat <- factor(cb_long2$cat, levels=c("negative net changes",
                                            "oc",
                                            "res_costs",
                                            "comp_cost",
                                            "positive net changes",
                                            "comp_gains" ))

library(RColorBrewer)

palette <- brewer.pal(9, "RdBu")[c(1,2,3,4,7,8)]


#"#B2182B" "#D6604D" "#F4A582" "#FDDBC7" "#92C5DE" "#4393C3"
# Create the chart

y_l <-"cashflow (USD) x 1000" 

# df with net value

netvalue <- cb_long2 %>%
  group_by_at(1)%>%
  summarise(net=sum(value_1000))

library(scales)


barplot <- cb_long2%>%
  #filter(cat!="negative net changes")%>%
  #filter(cat!="positive net changes")%>%
  ggplot( aes(x = target, y = value_1000, fill = cat)) +
  geom_bar(stat = "identity",col="white",position="stack") +
  geom_point(data = netvalue,mapping = aes(x=target,y=net),fill=NA,show.legend = FALSE)+
  scale_fill_manual(values = c("#B2182B", "#D6604D","#F4A582", "#FDDBC7", "#92C5DE","#2166AC"),labels=c("production negative net changes","opportunity cost","restoration cost","compensation cost","production positive net changes","compensation gain")) +
  coord_flip() +
  labs(title = "", x = "", y = y_l) +
    #scale_y_log10(labels = comma) +
  scale_y_continuous(labels = comma) +
  # adding net value
  theme_classic() +
  labs(title = "", x = "", y = y_l, fill = "")+
  # Add a line at the zero point
  geom_hline(yintercept = 0, color = "black", size = 1, linetype = "dotdash") +
  theme(legend.position = "top")
  


barplot <- ggpar(barplot,font.x = c(8, "bold", "black"),font.y = c(8, "bold", "black"), font.xtickslab = c(8),font.ytickslab = c(8), legend.size = 8)

ggsave(filename ="figures/fi3.jpg",
       plot = barplot,
       units = "cm",
       width = 16,
       height =8,
       "jpeg")

