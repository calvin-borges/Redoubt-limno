library(tidyverse)
library(janitor)
library (readr)
library(readxl)
library(writexl)
#set wd
#migrated this over from F604 class file. Now housed in filepath below
#set wd
setwd("C:/Users/Calvinborges/Desktop/Git upload/Redoubt-limno")
  #

#read back in
nut.dat<- read_csv("data/processed/nutrients.1980.2025.clean.csv")

# add factor for lab and "era". also fix depth as factor.
  nut.dat<-nut.dat %>% 
    mutate(lab= case_when( Year>2018 ~"KILL", 
                           between(Year, 1999,2017)~"CCAL",
                           Year<1998~"ADFG?"),
            era= case_when( 
                            between( Year,1980,2013)~"pre_ls",
                            between(Year, 2014, 2023)~"post_ls",
                            Year>2023~"post_fert"),
           Depth_m= as.factor(Depth_m))
  
  # order factor for plottin purposes
  nut.dat <- nut.dat %>%
    mutate( era= fct_relevel(era, "pre_ls", "post_ls", "post_fert"))
  
  #add DIN as a variable
  nut.dat<-nut.dat %>% 
    mutate(DIN_mg_l= NO3_N_plus_n02_mg_l+NH3_N_mg_l)
  
  #wuick plot for spatial ammonia
  nut.dat%>% 
    filter(Depth_m== 1) %>% 
    ggplot(aes( Julian_Day, NH3_N_mg_l))+
    geom_point()+
    facet_grid(~Station)
#
# filter out depths to be either 1 or 66, and months to be 4:10: doing this just to simplify dataset for plotting
  nut.filt<-nut.dat %>% 
    filter(Depth_m %in% c( 1, 66),
           Station %in% c(2,3),
           Month %in% c(4:10)) %>% 
    mutate(Depth_m= as.factor(Depth_m))
  
#begin plotting

##Phosphourous #### Parameters are broken down into  Unfiltered Total Phosphourous, Total Dissolved Phosphourous ,and Orthophospahte (biologically ready)
 
  ####Unfiltered Total Phosphorous
  nut.filt %>% 
    ggplot(aes( Julian_Day, log(UTP_mgl+0.0001),color=Depth_m))+
    geom_point()+
    facet_wrap(~era)
  ### UTP summarised
  nut.filt%>% 
    group_by(Year, Station, Depth_m) %>% 
    summarise(mean_UTP= mean(UTP_mgl, na.rm =TRUE), n=n()) %>% 
    ggplot(aes(Year, mean_UTP, color=Depth_m))+
    geom_point()
  
  ###Total Dissolved Phosphorous (TDP)
  nut.filt %>% 
    ggplot(aes( Julian_Day, TDP_mgl,color=Depth_m))+
    geom_point()
  ### TDP summarized
  nut.filt %>% 
    group_by(Year, Station, Depth_m) %>% 
    summarise(mean_TDP= mean(TDP_mgl, na.rm =TRUE), n=n()) %>% 
    ggplot(aes(Year, mean_TDP, color=Depth_m))+
    geom_point()
  #### orthophosphate
  nut.filt %>% 
    ggplot(aes( Julian_Day,PO4_P_mgl,color=Depth_m))+
    geom_point()
  ###orthophosphate summarised
  nut.filt %>% 
    group_by(Year, Station, Depth_m) %>% 
    summarise(mean_orthophospahte= mean(PO4_P_mgl, na.rm =TRUE), n=n()) %>% 
    ggplot(aes(Year, mean_orthophospahte, color=Depth_m))+
    geom_point()
  
  #COMBINED PHosphourous CYCLE
  
  #seasonal patterns
  nut.filt %>% 
    pivot_longer(cols = c(UTP_mgl, TDP_mgl, PO4_P_mgl), names_to = "parameter", values_to = "mg_ml") %>% 
    ggplot(aes(Julian_Day, mg_ml, color=parameter))+
    geom_point()+
    facet_wrap(~era)
  
  #long term patterns
  
  nut.filt %>% 
    group_by(Year, Depth_m) %>% 
    summarise(n_months=n_distinct(Month),mean_UTP = mean(UTP_mgl, na.rm = TRUE), mean_TDP=mean(TDP_mgl, na.rm = TRUE), mean_ortho= mean(PO4_P_mgl, na.rm = TRUE)) %>% 
    pivot_longer(cols = c(mean_UTP, mean_TDP, mean_ortho), names_to = "parameter", values_to = "mg_ml") %>% 
  ggplot(aes(Year, mg_ml, color=parameter))+
    geom_point()+
    facet_wrap(~Depth_m, ncol=1)
  
  
  ####Nitrogen######################### Split into Unfiltered Total Nitrogen (UTN), DIN, 

  #plot UTN seasonal
  nut.filt %>% 
    filter(UTN_mgl<0.75) %>% #filter to remove outlier
    ggplot(aes( Julian_Day, UTN_mgl,color=Depth_m))+
    geom_point()+
    facet_wrap(~era)
  ###UTN annual
  nut.filt %>% 
    filter(UTN_mgl<0.75) %>% #filter to remove outlier
    group_by(Year, era, Depth_m) %>% 
    summarise(mean_UTN_mg_L= mean(UTN_mgl, na.rm =TRUE), n=n()) %>% 
    ggplot(aes(Year, mean_UTN_mg_L, color=Depth_m))+
    geom_line()
  
  ###NH3#####
  nut.filt %>% 
    ggplot(aes( Julian_Day, NH3_N_mg_l,color=Depth_m))+
    geom_point()+
    facet_grid(~era)
  
  #plot DIN
  nut.filt %>% 
    ggplot(aes( Julian_Day, DIN_mg_l,color=Depth_m))+
    geom_point()+
    facet_grid(~era)
  ###DIN summarized by year
  nut.filt %>% 
    group_by(Year, Station, Depth_m) %>% 
    summarise(mean_Din= mean(DIN_mg_l, na.rm =TRUE), n=n()) %>% 
    ggplot(aes(Year, mean_Din, color=Depth_m))+
    geom_point()


  #
  #
  #
  # NITROGEN CYCLE COMBINED
  #seasonal patterns
  nut.filt %>% 
    pivot_longer(cols = c(UTN_mgl, DIN_mg_l), names_to = "parameter", values_to = "mg_ml") %>% 
    ggplot(aes(Julian_Day, mg_ml, color=parameter))+
    geom_point()+
    geom_smooth(se=FALSE)+
    facet_grid(~era)
  
  #long term patterns
  
  nut.filt %>% 
    group_by(Year, Depth_m) %>% 
    summarise(n_months=n_distinct(Month),mean_UTN = mean(UTN_mgl, na.rm = TRUE), mean_N_N= mean(NO3_N_plus_n02_mg_l, na.rm = TRUE), mean_Ammonia= mean(NH3_N_mg_l, na.rm=TRUE)) %>% 
    pivot_longer(cols = c(mean_UTN, mean_N_N, mean_Ammonia), names_to = "parameter", values_to = "mg_ml") %>% 
    ggplot(aes(Year, mg_ml, color=parameter))+
    geom_line()+
    facet_wrap(~Depth_m, ncol=1)
  
# TRACE MINERALS/ MICRONUTRIENTS
  
  ##plot magnesium
  nut.filt %>% 
    ggplot(aes( Julian_Day, Mg_mgl,color=Depth_m))+
    geom_point()+
    facet_wrap(~era)
  ###summarised
  nut.filt %>% 
    group_by(Year, Station, Depth_m) %>% 
    summarise(mean_magnesium_mgl= mean(Mg_mgl, na.rm =TRUE), n=n()) %>% 
    ggplot(aes(Year, mean_magnesium_mgl, color=Depth_m))+
    geom_point()
  #Iron
  nut.filt %>% 
    ggplot(aes( Julian_Day, Iron_mgl, color=Depth_m))+
    geom_point()+
    theme_bw()
  #summarise
  nut.filt %>% 
    group_by(Year, Depth_m) %>% 
    summarise(mean_fe_mgl= mean(Iron_mgl, na.rm =TRUE), n=n()) %>% 
    ggplot(aes(Year, mean_fe_mgl, color=Depth_m))+
    geom_point()
  
  ##plot Silica
  nut.filt %>% 
    ggplot(aes( Julian_Day, SiO2_Si_mgl,color=Depth_m))+
    geom_point()+
    facet_grid(~era)+
    geom_smooth(se=FALSE)
  
  ##silica summarized by year
  nut.filt %>% 
    group_by(Year, Station, Depth_m) %>% 
    summarise(mean_silica_mgl= mean(SiO2_Si_mgl, na.rm =TRUE), n=n()) %>% 
    ggplot(aes(Year, mean_silica_mgl, color=Depth_m))+
    geom_point()+
    geom_smooth(se=FALSE)
  
  
  ###calcium
  nut.filt%>% 
    ggplot(aes( Julian_Day, Ca_mgl,color=Depth_m))+
    geom_point()+
    facet_wrap(~era)
  ####calcium summarised by year
  nut.filt %>% 
    group_by(Year, Station, Depth_m) %>% 
    summarise(mean_calcium= mean(Ca_mgl, na.rm =TRUE), n=n()) %>% 
    ggplot(aes(Year, mean_calcium, color=Depth_m))+
    geom_point()
  
  
  #ALkalinity and PH
  ########################
  
  ##Alkalinkty
  nut.filt %>% 
    ggplot(aes( Julian_Day, Alkalinity_mg_ca03,color=Depth_m))+
    geom_point()+
    facet_wrap(~era)
  ##alkalinity summarised by year
  nut.filt %>% 
    group_by(Year, Station, Depth_m) %>% 
    summarise(mean_alkalinity= mean(Alkalinity_mg_ca03, na.rm =TRUE), n=n()) %>% 
    ggplot(aes(Year, mean_alkalinity, color=factor(Depth_m)))+
    geom_point()
  #strange jump
  #reached out to CCAl and likely do to a switch in methodology around 2010
  ###ph
  nut.filt %>% 
    ggplot(aes( Julian_Day, pH))+
    geom_point()+
    facet_wrap(~Depth_m)
  ####ph summarized by year
  nut.filt%>% 
    group_by(Year,lab, Depth_m) %>% 
    summarise(mean_pH= mean(pH, na.rm =TRUE), n=n()) %>% 
    ggplot(aes(Year, mean_pH, color=Depth_m))+
    geom_point()
    
  ####
  #######

    
  
  