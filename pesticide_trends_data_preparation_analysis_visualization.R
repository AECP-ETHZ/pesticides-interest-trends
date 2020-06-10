################################################################################
# R Code related to                                                            #
# Tracking societal concerns on pesticides – A Google Trends analysis          #
# by Sergei Schaub, Robert Huber & Robert Finger                               # 
# 2020                                                                         #
# Environmental Research Letters                                               #
# -----------------------------------------------------------------------------#
# r script for data preparation, analysis and visualization                    #
# 2020, ETH Zurich                                                             #
# developed by Sergei Schaub                                                   #  
################################################################################

# table of content:

# 0. settings
# 1. load and prepare data
# 2. testing for structural breaks
# 2.1 create time series
# 2.2 test for structural breaks
# 3. visualization of interest and structural breaks
# 3.1 figure 1
# 3.2 figure A1
# 3.3 figure A2



################################################################################
#------------------------------- 0. settings
################################################################################


# clear workspace:
rm(list = ls()) 

# install packages
# install.packages(c("tidyverse", "gtrendsR","strucchange","patchwork","ggrepel"))

# load packages
require(tidyverse)
require(gtrendsR)
require(strucchange)
require(ggpubr)
require(patchwork)
require(ggrepel)

# define main working directory 
cd_main <- "H:/google trends/trinkwasserinitiative"        #### !!! Needs to be specified !!!

# set directory
setwd(cd_main) 



################################################################################
#------------------------------- 1. load and prepare data
################################################################################

# load data (the data needs to be downloaded in advance using the r script 'pesticide_trends_data_download.R'):
list_csv <- list.files(pattern = "google_trend_data_") # create a list of all downloaded data files
n1 <- length(list_csv)                                 # number of downloaded data files
s1 <- NULL                                             # create container for merged data, main
s1aux3 <- NULL                                         # create container for merged data, to identify identical samples

## combine data files:
for (i in 1:n1) {
  name_csv <- list_csv[i]                                                                                            # current file name
  s1aux <- read.csv(name_csv, stringsAsFactors = F) %>% mutate(down_date = str_sub(name_csv, start = -12, end = -5)) # read current csv file
  s1 <- bind_rows(s1,s1aux)                                                                                          # merge data
  
  s1aux2 <- s1aux %>% dplyr::select(hits)                       # create data for identifying identical samples
  colnames(s1aux2) <- str_sub(name_csv, start = -12, end = -5)  # name column
  s1aux3 <- bind_cols(s1aux3, s1aux2)                           # merge data
}

duplicated(t(s1aux3)) # identification of identical samples


# create subset of data:
## focus on main group
s2 <- s1 %>% group_by(group, date) %>% 
  mutate(group_hits_mean = mean(group_hits , na.rm = T),
         dev_mean = group_hits-group_hits_mean) %>% 
  dplyr::select(group, date, down_date, dev_mean, group_hits_mean) %>% distinct() %>% ungroup()


s3 <- s1 %>% group_by(group, date) %>% 
  mutate(group_hits_mean = mean(group_hits , na.rm = T),
         group_hits_sd = mad(group_hits , na.rm = T)) %>% 
  dplyr::select(group, date, down_date, group_hits) %>% distinct() %>% ungroup()

## focus on subgroup
s4 <- s1 %>% group_by(subgroup, date) %>% 
  mutate(hits_rescaled2_mean = mean(hits_rescaled2 , na.rm = T)) %>% 
  dplyr::select(group, subgroup,date, down_date, hits_rescaled2_mean) %>% distinct() %>% ungroup()

s5 <- s1 %>% group_by(subgroup, date) %>% 
  mutate(hits_rescaled2_mean = mean(subgroup_hits , na.rm = T)) %>% 
  dplyr::select(group, subgroup,date, down_date, hits_rescaled2_mean) %>% distinct() %>% ungroup()




################################################################################
#--------------------- 2. testing for structural breaks
################################################################################
# this section follows a blog post by Anirudh (2018). 'Endogenously Detecting Structural Breaks in a Time Series: Implementation in R',
# https://www.r-bloggers.com/endogenously-detecting-structural-breaks-in-a-time-series-implementation-in-r/

#-------------------------------------------------------------------------------------------------
# 2.1 create time series
#------------------------------------------------------------------------------------------------- 

# create time series of specific terms
## pesticides and plant protection products (main group)
d2_pest_aux <- s2 %>% dplyr::select(date,group,group_hits_mean) %>% filter(group == "pesticide") %>% distinct()
d2_pest <-     s2 %>% dplyr::select(date,group,group_hits_mean) %>% filter(group == "pesticide") %>% distinct() %>% dplyr::select(group_hits_mean) 
d2_pest_ts <- ts(d2_pest)
# plot(d2_pest_ts)

## initiatives (main group)
d2_init_aux <- s2 %>% dplyr::select(date,group,group_hits_mean) %>% filter(group == "initiative") %>% distinct() 
d2_init <-     s2 %>% dplyr::select(date,group,group_hits_mean) %>% filter(group == "initiative") %>% distinct() %>% dplyr::select(group_hits_mean) 
d2_init_ts <- ts(d2_init)
# plot(d2_init_ts)

## pesticides (subgroup)
d2_pest_defr_aux <- s5 %>% filter(subgroup == "pesticide") %>% dplyr::select(date,hits_rescaled2_mean) %>% distinct()
d2_pest_defr <-     s5 %>% filter(subgroup == "pesticide") %>% dplyr::select(date,hits_rescaled2_mean) %>% distinct() %>% dplyr::select(hits_rescaled2_mean)
d2_pest_defr_ts <- ts(d2_pest_defr)
# plot(d2_pest_defr_ts)

## plant protection products (subgroup)
d2_plan_defr_aux <- s5 %>% filter(subgroup == "plant protection product") %>% dplyr::select(date,hits_rescaled2_mean) %>% distinct()
d2_plan_defr <-     s5 %>% filter(subgroup == "plant protection product") %>% dplyr::select(date,hits_rescaled2_mean) %>% distinct() %>%  dplyr::select(hits_rescaled2_mean)
d2_plan_defr_ts <- ts(d2_plan_defr)
# plot(d2_plan_defr_ts)



#-------------------------------------------------------------------------------------------------
# 2.2 test for structural breaks
#------------------------------------------------------------------------------------------------- 

# pesticides and plant protection products (main group)
break_pest <- breakpoints(d2_pest_ts ~ 1)
summary(break_pest)
plot(break_pest)
pest_bre <- break_pest$breakpoints # extract break point information 
## get date of breaks
d2_pest_aux$time <- seq(1:length(d2_pest_aux$date)) # first, add time to data
pest_bre_date <- as.data.frame(d2_pest_aux %>% filter(time == as.numeric(pest_bre[1]) | time ==as.numeric(pest_bre[2])))[,1] # second, save date


# initiative (main group)
break_init <- breakpoints(d2_init_ts ~ 1)
summary(break_init)
plot(break_init)
init_bre <- break_init$breakpoints # extract break point information 
## get date of breaks
d2_init_aux$time <- seq(1:length(d2_init_aux$date)) # first, add time to data
inti_bre_date <- as.data.frame(d2_init_aux %>% filter(time == as.numeric(init_bre[1]) | time ==as.numeric(init_bre[2])))[,1] # second, save date

# pesticides (subgroup)
break_pest_defr <- breakpoints(d2_pest_defr_ts ~ 1)
summary(break_pest_defr)
plot(break_pest_defr)
pest_defr_bre <- break_pest_defr$breakpoints # extract break point information 
## get date of breaks
d2_pest_defr_aux$time <- seq(1:length(d2_pest_defr_aux$date)) # first, add time to data
pest_defr_bre_date <- as.data.frame(d2_pest_defr_aux %>% 
                                      filter(time == as.numeric(pest_defr_bre[1]) |
                                               time ==as.numeric(pest_defr_bre[2]) | 
                                               time ==as.numeric(pest_defr_bre[3])))[,1] # second, save date

# plant protection products (subgroup)
break_plan_defr <- breakpoints(d2_plan_defr_ts ~ 1)
summary(break_plan_defr)
plot(break_plan_defr)
plan_defr_bre <- break_plan_defr$breakpoints # extract break point information 
## get date of breaks
d2_plan_defr_aux$time <- seq(1:length(d2_plan_defr_aux$date)) # first, add time to data
plan_defr_bre_date <- as.data.frame(d2_plan_defr_aux %>% filter(time == as.numeric(plan_defr_bre[1])))[,1] # second, save date





################################################################################
#--------------------- 3. visualization of interest and structural breaks
################################################################################

#-------------------------------------------------------------------------------------------------
# 3.1 figure 1 - main interest over time for search terms related to pesticides and the popular 
#     initiatives in switzerland 
#------------------------------------------------------------------------------------------------- 

d2 <- s2 %>% mutate(date = as.Date(date)) # transform date variable

# figure 1, panel A, step 1 from 2:
fig1a_a <- ggplot() + 
  geom_vline(xintercept = as.numeric(as.Date(c(pest_bre_date,inti_bre_date))), 
             color=c("#01665e","#01665e","#8c510a"),linetype="dashed", alpha = 0.6,
             size=0.6)+
  geom_line(data =d2, aes(date, group_hits_mean, color = group),size=0.75)+
  geom_hline(yintercept = 0, 
             color=c("#000000"),
             size=0.75)+
  theme(axis.title.x = element_text( size=14), axis.text.x  = element_text( size=12), 
        axis.title.y = element_text( size=14), axis.text.y  = element_text( size=12),legend.text  = element_text( size=12) ,
        legend.title = element_text( size=14),title= element_text( size=14),
        legend.position=c(.17, .7),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_blank(),strip.text = element_text(size=14),
        legend.margin = margin(6, 6, 6, 6),
        plot.title = element_text(size=14),
        plot.tag = element_text(size=14))+
  scale_y_continuous(name = "Google Search Volume", limits = c(-7,115),breaks = c(0,25,50,75,100))+
  scale_x_date(name = "Date",date_breaks = "1 year", date_labels = "%Y")+
  scale_colour_manual(values=c("#8c510a",
                               "#01665e"))

fig1a_a

# retrive mean search volume per year and main group
d2 %>% dplyr::select(-down_date) %>% distinct() %>% mutate(year = substr(date,1,4)) %>% group_by(year, group) %>% 
  summarise(Mhits = mean(group_hits_mean))


# create data frame of important political activities and report releases
events <- data.frame(
  as.Date(c("2016-11-29","2018-5-25","2019-02-27", # 1-3
            "2017-3-21" ,"2018-1-18","2018-12-14", # 4-6
            "2015-3-20",   # 7
            "2016-5-16",   # 8
            "2017-4-01",   # 9
            "2019-4-01",   # 10
            "2019-9-06")), # 11;   dates
  as.Date(c("2017-4-01" ,"2018-2-01","2018-12-01", # 1-3
            "2016-12-01","2018-6-01","2019-03-01", # 4-6
            "2015-4-01",   # 7
            "2016-6-01",   # 8
            "2017-4-01",   # 9
            "2019-4-01",   # 10
            "2019-9-01")), # 11;   proxy dates, i.e. closest data for which we also have google trend data (only for matching)
  c("Start of collecting signatures (Save Switzerland from Synthetic Pesticides popular initiative)",
    "Initiative submission (Save Switzerland from Synthetic Pesticides popular initiative)",
    "Publication of the official response by the Swiss Federal Council (Save Switzerland from Synthetic Pesticides popular initiative)",
    "Start of collecting signatures (Clean Drinking Water and Healthy Food popular initiative)",
    "Initiative submission (Clean Drinking Water and Healthy Food popular initiative)",
    "Publication of the official response by the Swiss Federal Council (Clean Drinking Water and Healthy Food popular initiative)",
    "WHO on glyphosate, probably carcinogenic to humans",
    "WHO on glyphosate, glyphosate is unlikely to pose a carcinogenic risk to humans from exposure through the diet",
    "Hohe ökotoxikologische Risiken in Bächen",
    "Anhaltend hohe PSM-Belastung in Bächen",
    "Kampagne der Schweizer Kantonschemiker im Jahre 2019"), # description
  c("PI1a","PI1b","PI1c", 
    "PI2a","PI2b","PI2c", 
    "IR1","IR2",
    "NR1","NR2","NR3"),     # abbreviation 
  c(rep("Political Activities",6),rep("International Reports",2),rep("National Reports",3)), # group (initiative related = 1, international topics = 2, swiss topics = 3)
  c(rep("Political Activities (IT)",3),rep("Political Activities (PI)",3),
    rep("International Reports",2),rep("National Reports",3)), # group 2 (initiative related = 1, international topics = 2, swiss topics = 3)
  c(rep(-2,11))) # y-axis


colnames(events) <- c("date","date_proxy","description","abbreviation","group","group2","y_axis") # rename columns 

# create data for variable to plot linear trends
## for pesticides and plant protection products (main group)
if(length(pest_bre)==1){
  d2_pest_aux2 <- d2_pest_aux %>% 
    mutate(kind_word = "pesticide", breakpoint = ifelse(time<pest_bre[1],0,1)) %>% mutate(date = as.Date(date)) } else if (length(pest_bre)==2){
      
      d2_pest_aux2 <- d2_pest_aux %>% mutate(kind_word = "pesticide",  breakpoint = ifelse(time<pest_bre[1],0,
                                                                                           ifelse(time<pest_bre[2],1,2)))%>% mutate(date = as.Date(date))
    }

## for initiatives (main group)
if(length(init_bre)==1){
  d2_init_aux2 <- d2_init_aux %>% 
    mutate(kind_word = "initiative", breakpoint = ifelse(time<init_bre[1],0,1)) %>% mutate(date = as.Date(date)) } else if (length(init_bre)==2){
      
      d2_init_aux2 <- d2_init_aux %>% mutate(kind_word = "initiative",  breakpoint = ifelse(time<init_bre[1],0,
                                                                                            ifelse(time<init_bre[2],1,2)))%>% mutate(date = as.Date(date))
    }
## merge data of containing variables for linear trends
d2_mix_aux2 <- bind_rows(d2_pest_aux2,d2_init_aux2)


# figure 1, panel A, step 2 from 2:
set.seed(42)
fig1a_b <- 
  fig1a_a + 
  geom_point(data = events, aes(date, y_axis, color = group), size = 1.5)+

  geom_text_repel(data = events, aes(date, y_axis,label=abbreviation, color = group), 
                  angle = 90,
                  size = 3.5,box.padding = 0.5, max.overlaps = Inf,min.segment.length = 0,
                  ylim  = c(NA,-3))+
  geom_smooth(data = d2_mix_aux2,
              aes(date, group_hits_mean, color = paste0("keyword-trend:",kind_word,breakpoint)),size=0.6,method = "lm", formula = y ~ x,se = FALSE)+
  annotate("text", x = as.Date("2013-12-01"),y = 99,  label = "Strucutral break",size = 4,angle = 90)+
  annotate("text", x = as.Date("2017-07-01"),y = 99,  label = "Strucutral break",size = 4,angle = 90)+
  annotate("text", x = as.Date("2016-12-01"),y = 99,  label = "Strucutral break",size = 4,angle = 90)+
  scale_colour_manual(values=c("#8c510a", # initiative
                               "#737373",
                               "#8c510a", # keyword−trend:initiative0
                               "#8c510a", # keyword−trend:initiative1
                               "#01665e", # keyword−trend:pesticide0
                               "#01665e", # keyword−trend:pesticide1
                               "#01665e", # keyword−trend:pesticide2
                               "#bdbdbd",
                               "#01665e", # pesticide (group variable)
                               "black"), 
                      name = "Main Group:", labels = c("Initiative","abbr1","Initiative-Trend1","Initiative-Trend2",
                                                  "Pesticides and Plant\n Protection Products","Pesticide-Trend1","Pesticide-Trend2","abbr2"
                                                  ,"Pesticide-Trend3","abbr3"))+
  labs(tag = "A")


fig1a_b






# create data for variable to plot linear trends
## for pesticides (subgroup)
if(length(pest_defr_bre)==1){
  d2_pest_defr_aux2 <- d2_pest_defr_aux %>% 
    mutate(kind_word = "pesticide", breakpoint = ifelse(time<pest_defr_bre[1],0,1)) %>% mutate(date = as.Date(date)) } else if (length(pest_defr_bre)==2){
      
      d2_pest_defr_aux2 <- d2_pest_defr_aux %>% mutate(kind_word = "pesticide",  breakpoint = ifelse(time<pest_defr_bre[1],0,
                                                                                                     ifelse(time<pest_defr_bre[2],1,2)))%>% mutate(date = as.Date(date))
    }else if (length(pest_defr_bre)==3){
      
      d2_pest_defr_aux2 <- d2_pest_defr_aux %>% mutate(kind_word = "pesticide",  breakpoint = ifelse(time<pest_defr_bre[1],0,
                                                                                                     ifelse(time<pest_defr_bre[2],1,
                                                                                                            ifelse(time<pest_defr_bre[3],2,3))))%>% mutate(date = as.Date(date))
    }

## for plant protection products (subgroup) 
if(length(plan_defr_bre)==1){
  d2_plan_defr_aux2 <- d2_plan_defr_aux %>% 
    mutate(kind_word = "plant protection product", breakpoint = ifelse(time<plan_defr_bre[1],0,1)) %>% mutate(date = as.Date(date)) } else if (length(plan_defr_bre)==2){
      
      d2_plan_defr_aux2 <- d2_plan_defr_aux %>% mutate(kind_word = "plant protection product",  breakpoint = ifelse(time<plan_defr_bre[1],0,
                                                                                                                    ifelse(time<plan_defr_bre[2],1,2)))%>% mutate(date = as.Date(date))
    }

## merge data of containing variables for linear trends
d2_mix2_aux2 <- bind_rows(d2_pest_defr_aux2,d2_plan_defr_aux2)

d2sub <- s5 %>% filter(!subgroup == "initiative") %>% dplyr::select(-down_date) %>% distinct() # we do not want to plot political activities

# figure 1, panel B, step 1 from 1:
fig1b <-
  ggplot() +
  geom_vline(xintercept = as.numeric(as.Date(c(pest_defr_bre_date,plan_defr_bre_date))), 
             color=c("#9970ab","#9970ab","#9970ab","#5aae61"),
             size=0.6,linetype="dashed", alpha = 0.6,)+
  geom_line(data=d2sub ,aes(as.Date(date), hits_rescaled2_mean, color = subgroup),size=0.75)+
  geom_hline(yintercept = 0, color=c("#000000"),size=0.75)+
  geom_smooth(data = d2_mix2_aux2,size=0.6,method = "lm", formula = y ~ x,se = FALSE, 
              aes(date, hits_rescaled2_mean, color = paste0("keyword-trend:",kind_word,breakpoint)),
              show.legend = FALSE)+
  theme(axis.title.x = element_text( size=14), axis.text.x  = element_text( size=12), 
        axis.title.y = element_text( size=14), axis.text.y  = element_text( size=12),legend.text  = element_text( size=12) ,
        legend.title = element_text( size=14),title= element_text( size=14),
        legend.position=c(.17, .7))+  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_blank(),strip.text = element_text(size=14),
        legend.margin = margin(6, 6, 6, 6),
        plot.title = element_text(size=14),
        plot.tag = element_text(size=14))+
  scale_y_continuous(name = "Google Search Volume", limits = c(-2,115),breaks = c(0,25,50,75,100))+
  scale_x_date(name = "Date",date_breaks = "1 year", date_labels = "%Y")+
  scale_colour_manual(values=c("#9970ab","#9970ab","#9970ab","#9970ab","#5aae61","#5aae61","#9970ab","#5aae61"), 
                      name = "Subgroup:", labels = c("Pesticides","Pesticides","Pesticides","Pesticides", "Plant Protection Products",
                                                     "Plant Protection Products","Pesticides", "Plant Protection Products"))+
  labs( 
    # title= "Subgroup trends", 
    tag = "B")+

  annotate("text", x = as.Date("2014-01-01"),y = 99,  label = "Strucutral break",size = 4,angle = 90)+
  annotate("text", x = as.Date("2014-12-01"),y = 99,  label = "Strucutral break",size = 4,angle = 90)+
  annotate("text", x = as.Date("2016-12-01"),y = 99,  label = "Strucutral break",size = 4,angle = 90)+
  annotate("text", x = as.Date("2018-06-01"),y = 99,  label = "Strucutral break",size = 4,angle = 90)

fig1b


# figure 1, combine panels
fig1 <- ggarrange(fig1a_b, fig1b, ncol = 1, nrow = 2, heights = c(1,0.7))
fig1


# save figure
#ggsave("figure_1.pdf",plot=fig1 , width = 8.50, height = 11) 


#-------------------------------------------------------------------------------------------------
# 3.2 figure A1 -  sampling noise of google trends data.
#------------------------------------------------------------------------------------------------- 

# figure A1, panel A
figa1a <- ggplot(s3, aes(as.Date(date), group_hits, color = group)) + 
  stat_summary(fun.data = 'mean_sdl',
               fun.args = list(mult = 1),
               geom = 'smooth', se = TRUE)+
  geom_hline(yintercept = 0, 
             color=c("#000000"),
             size=0.75)+
  theme(axis.title.x = element_text( size=14), axis.text.x  = element_text( size=12), 
        axis.title.y = element_text( size=14), axis.text.y  = element_text( size=12),legend.text  = element_text( size=12) ,
        legend.title = element_text( size=14),title= element_text( size=14),
        legend.position=c(.17, .7))+  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_blank(),strip.text = element_text(size=14),
        legend.margin = margin(6, 6, 6, 6),
        plot.title = element_text(size=14),
        plot.tag = element_text(size=14))+
  scale_y_continuous(name = "Google Search Volume", limits = c(-2,115),breaks = c(0,25,50,75,100))+
  scale_x_date(name = "Date",date_breaks = "1 year", date_labels = "%Y")+
  scale_colour_manual(values=c("#8c510a",
                               "#01665e"
  ))+ 
  labs(tag = "A")

# figure A1, panel B
figa1b <- ggplot(data=s2 %>% filter(dev_mean != 0) ,aes(dev_mean, fill = group)) + 
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 2)+
  geom_hline(yintercept = 0, 
             color=c("#000000"),
             size=0.75)+
  theme(axis.title.x = element_text( size=14), axis.text.x  = element_text( size=12), 
        axis.title.y = element_text( size=14), axis.text.y  = element_text( size=12),legend.text  = element_text( size=12) ,
        legend.title = element_text( size=14),title= element_text( size=14),
        legend.position=c(.17, .7))+  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_blank(),strip.text = element_text(size=14),
        legend.margin = margin(6, 6, 6, 6),
        plot.title = element_text(size=14),
        plot.tag = element_text(size=14))+
  labs(x ="Deviation from Mean", y= "Frequency", tag = "B")+
  scale_fill_manual(values=c("#8c510a",
                             "#01665e"
  ))



# figure 2, combine panels
figa1 <- figa1a+figa1b+ plot_layout(ncol = 1, heights = c(3, 1.5))
figa1

# save figure
#ggsave("figure_a1.pdf",plot=figa1 , width = 8.50, height = 11) 



#-------------------------------------------------------------------------------------------------
# 3.3 figure A2 -  interest over time for search terms related to pesticides and plant protection 
#     products in austria, france, germany and italy
#------------------------------------------------------------------------------------------------- 

# download data (we downloaded data on the 3.3.2020)
a1 <- gtrends(c("Pflanzenschutzmittel","Pestizide"), geo = "AT",time='2011-01-01 2019-12-31')   # austria
f1 <- gtrends(c("Produit phytosanitaire","Pesticide"), geo = "FR",time='2011-01-01 2019-12-31') # france
d1 <- gtrends(c("Pflanzenschutzmittel","Pestizide"), geo = "DE",time='2011-01-01 2019-12-31')   # germany
i1 <- gtrends(c("Prodotto fitosanitario","Pesticida"), geo = "IT",time='2011-01-01 2019-12-31') # italy

# retrive interest over time
a2 <- a1$interest_over_time %>% mutate(country = "Austria")
f2 <- f1$interest_over_time %>% mutate(country = "France")
d2 <- d1$interest_over_time %>% mutate(country = "Germany")
i2 <- i1$interest_over_time %>% mutate(country = "Italy")

# merge national data
q1 <- bind_rows(a2,f2,d2,i2) %>% mutate(Subgroup = ifelse(keyword == "Pestizide", "Pesticides",
                                                          ifelse(keyword == "Pesticide", "Pesticides",
                                                                 ifelse(keyword == "Pestizide", "Pesticides",
                                                                        ifelse(keyword == "Pesticida", "Pesticides","Plant Protection Products")))))
# write.csv(q1, file = "pesticide_at_fr_de_it_03032020.csv")


# figure A2
figa2 <-
  ggplot() +
  geom_line(data=q1 ,aes(as.Date(date), hits, color = Subgroup),size=0.75)+
  geom_hline(yintercept = 0, color=c("#000000"),size=0.75)+
  facet_grid(country ~ .)+
  theme(axis.title.x = element_text( size=14), axis.text.x  = element_text( size=12), 
        axis.title.y = element_text( size=14), axis.text.y  = element_text( size=12),legend.text  = element_text( size=12) ,
        legend.title = element_text( size=14),title= element_text( size=14),
        legend.position=c("bottom"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_blank(),strip.text = element_text(size=14),
        legend.margin = margin(6, 6, 6, 6),
        plot.title = element_text(size=14),
        plot.tag = element_text(size=14))+
  scale_y_continuous(name = "Google Search Volume", limits = c(-2,115),breaks = c(0,25,50,75,100))+
  scale_x_date(name = "Date",date_breaks = "1 year", date_labels = "%Y")+
  scale_colour_manual(values=c("#9970ab","#5aae61"), 
                      name = "Subgroup:")


figa2

# save figure
#ggsave("figure_a2.pdf",plot=figa2 , width = 8.50, height = 11) 
