################################################################################
# R Code related to                                                            #
# Tracking societal concerns on pesticides – A Google Trends analysis          #
# -----------------------------------------------------------------------------#
# r script for downloading and first preparation of data                       #
################################################################################

# table of content:
# 0. settings
# 1. identification of search terms with available google trends data
# 2. downloading and first preparation of data  

################################################################################
#------------------------------- 0. settings
################################################################################


# clear workspace:
rm(list = ls()) 

# install packages
# install.packages(c("tidyverse", "gtrendsR","strucchange","patchwork"))

# load packages
require(tidyverse)
require(gtrendsR)
require(strucchange)
require(ggpubr)
require(patchwork)


# define main working directory 
cd_main <- "H:/google trends/trinkwasserinitiative"        #### !!! Needs to be specified !!!

# set directory
setwd(cd_main) 



################################################################################
#----------- 1. identification of search terms with available google trends data
################################################################################

## group pesticides
### german
plot(gtrends(c("Pflanzenschutzmittel"), geo = "CH",time='2011-01-01 2019-12-31'))   # yes 
plot(gtrends(c("Pestizid"), geo = "CH",time='2011-01-01 2019-12-31'))               # yes 
plot(gtrends(c("Pestizide"), geo = "CH",time='2011-01-01 2019-12-31'))              # yes 
### french
plot(gtrends(c("Produit phytosanitaire"), geo = "CH",time='2011-01-01 2019-12-31')) # yes 
plot(gtrends(c("Pesticide"), geo = "CH",time='2011-01-01 2019-12-31'))              # yes 
### italian
plot(gtrends(c("Prodotto fitosanitario"), geo = "CH",time='2011-01-01 2019-12-31')) # no
plot(gtrends(c("Pesticida"), geo = "CH",time='2011-01-01 2019-12-31'))              # no


## group initiative
### german
plot(gtrends(c("Trinkwasserinitiative"), geo = "CH",time='2011-01-01 2019-12-31'))                         # yes
plot(gtrends(c("Trinkwasser initiative"), geo = "CH",time='2011-01-01 2019-12-31'))                        # yes 
plot(gtrends(c("'Trinkwasser-initiative'"), geo = "CH",time='2011-01-01 2019-12-31'))                      # no 
plot(gtrends(c("Für sauberes Trinkwasser und gesunde Nahrung"), geo = "CH",time='2011-01-01 2019-12-31'))  # no
plot(gtrends(c("Pestizid initiative"), geo = "CH",time='2011-01-01 2019-12-31'))                           # yes
plot(gtrends(c("Für eine Schweiz ohne synthetische Pestizide"), geo = "CH",time='2011-01-01 2019-12-31'))  # yes
### french
plot(gtrends(c("Pour une eau potable propre et une alimentation saine"), geo = "CH",time='2011-01-01 2019-12-31'))  # no
plot(gtrends(c("Initiative pour l’eau potable"), geo = "CH",time='2011-01-01 2019-12-31'))                          # no
plot(gtrends(c("Initiative eau propre"), geo = "CH",time='2011-01-01 2019-12-31'))                                  # no
plot(gtrends(c("'Pour une Suisse libre de pesticides de synthese"), geo = "CH",time='2011-01-01 2019-12-31'))       # no
plot(gtrends(c("Pour une Suisse sans pesticides de synthese"), geo = "CH",time='2011-01-01 2019-12-31'))            # no
plot(gtrends(c("initiative sur les pesticides de synthèse"), geo = "CH",time='2011-01-01 2019-12-31'))              # no
plot(gtrends(c("Zero pesticide"), geo = "CH",time='2011-01-01 2019-12-31'))                                         # no
plot(gtrends(c("Initiative pesticide "), geo = "CH",time='2011-01-01 2019-12-31'))                                  # no
### italian
plot(gtrends(c("Acqua potabile pulita e cibo sano"), geo = "CH",time='2011-01-01 2019-12-31'))           # no
plot(gtrends(c("iniziativa per l’acqua potabile"), geo = "CH",time='2011-01-01 2019-12-31'))             # no
plot(gtrends(c("Iniziativa sull'acqua potabile"), geo = "CH",time='2011-01-01 2019-12-31'))              # no
plot(gtrends(c("Per una Svizzera senza pesticidi sintetici"), geo = "CH",time='2011-01-01 2019-12-31'))  # no
plot(gtrends(c("Iniziativa contro i pesticidi"), geo = "CH",time='2011-01-01 2019-12-31'))               # no
plot(gtrends(c("Iniziativa contro pesticidi"), geo = "CH",time='2011-01-01 2019-12-31'))                 # no
plot(gtrends(c("Iniziativa sui pesticidi"), geo = "CH",time='2011-01-01 2019-12-31'))                    # no


################################################################################
#---------- 2. downloading and first preparation of data  
################################################################################

# download google trends available
p1 <- gtrends(c("Pflanzenschutzmittel","Pestizid","Pestizide","Produit phytosanitaire","Pesticide"), geo = "CH",time='2011-01-01 2019-12-31') 
i1 <- gtrends(c("Trinkwasserinitiative","Trinkwasser initiative","Pestizid initiative","Für eine Schweiz ohne synthetische Pestizide"), geo = "CH",time='2011-01-01 2019-12-31') 

# indicate group (either pesticide or initiative)
p2 <- p1$interest_over_time %>% 
  mutate(group = "pesticide",
         subgroup = ifelse(keyword =="Pflanzenschutzmittel"|keyword =="Produit phytosanitaire","plant protection product","pesticide")) # create sub-group for pesticide main group
i2 <- i1$interest_over_time %>% 
  mutate(group = "initiative",
         subgroup= "initiative")


# use the search term that has the highest value within a search categories. This is used to obtain the rescale values. 
key_pesticides <- (p2 %>% filter(hits==100))[5] # search term of the category pesticides with the most hits. 
key_initiative <- (i2 %>% filter(hits==100))[5] # search term of the category initiative with the most hits. 


# download most popular search term per category 
d1 <- gtrends(c(paste(key_initiative),paste(key_pesticides)), geo = "CH",time='2011-01-01 2019-12-31') 

# obtain the rescale values
rescales <- d1$interest_over_time %>% group_by(keyword) %>% summarise(max_hits = max(hits)) 
rescales_pesticide <- as.numeric(rescales[1,2])
rescales_initiatve <- as.numeric(rescales[2,2])

# add rescale value to original google trends data
p2$rescale <- rescales_pesticide
i2$rescale <- rescales_initiatve

# merge data and compute group variables (see appendix 2 for details)
d2 <- bind_rows(p2,i2) %>% 
  mutate(hits_rescaled = hits/100*rescale) %>%   
  group_by(group,date) %>% 
  mutate(sum_group  = sum(hits_rescaled)) %>% 
  group_by(subgroup,date) %>% 
  mutate(sum_subgroup  = sum(hits_rescaled)) %>%  
  group_by(group) %>% 
  mutate(max_sum_group = max(sum_group)) %>%  
  group_by(subgroup) %>% 
  mutate(max_sum_subgroup = max(sum_subgroup)) %>%  
  ungroup() %>% 
  mutate(max_sum_group2 = max(sum_group),
         max_sum_subgroup2 = max(sum_subgroup),
         group_hits = sum_group/max_sum_group2*100, 
         subgroup_hits = sum_subgroup/max_sum_subgroup2*100, 
         hits_rescaled2 = hits_rescaled/max_sum_group2*100) 
