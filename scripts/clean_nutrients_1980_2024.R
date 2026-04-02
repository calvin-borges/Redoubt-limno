library(tidyverse)
library(janitor)
library (readr)
library(readxl)
library(writexl)
# Calvin Borges calvin.borges@usda.gov
#April 1 2026
#
## This script takes 3 different .csv files and converts and combines them to common variables and units
#set wd
setwd("C:/Users/Calvinborges/Desktop/Git upload")
#
#
# READ IN DATA + col types
## read in functional .csv from years 1999-2017
#
nutrients.99.17<- read_csv("data/processed/nutrients/nutrients_1999_2017_clean.csv")

#read in .csv for years 2023 and 2024
nut.23.24<- read_csv("data/processed/nutrients/Redoubt Nutrients 2023_2024_complete.csv")
#
# read in csv 1980-1996
nut.80.96<-read_csv("data/processed/nutrients/80_96.nutrient.clean.csv")

#different labs (KILL vs CCAL) were used in these time periods so slightly different nomenclature/ anaylses/ Units/. 
#1) CCAL reported units in mg/l while kill reports in ug/L. Transformation is x1000.
#2) Phosphorous and Nitrogen are reported differently across labs. After correspondence with Michael Brett I believe the following transformations are appropriate
#
#  phosphorus:
#  #CCAL    #KILL
#  PO-4 =  FRP
#  TDP  =  TFP
#  UTP  =  TP
#
#  NITROGEN:
#  #CCAL    #KILL
#  NO3N+NO2 = nitrite/nitrate
#  NH3-N == TA
#  TDN  = can't be calculated from KILL metrics (calc= TA+ nitrite+nitrate+ "DON" (Dissolved Organic Nitrogen. DON not present in data))
#  UTN  = TKN + Nitrite/ Nitrate
#
# transform 1999_2017. Reached out to CCAL and from 1999- 2005 (specifically to april of 05) the lab was reporting TKN. went into the original text files and UTKN analyses was requested til this date while UTN requested afterwards. Therefore logical that 1999-2005 UTN needs to be converted from TKN to UTN (by adding nitrate/nitrite). A plot of the non-converted data supports this as UTN in this period dramatically lower. CCAL recommends doing so. Cb

#convert TKN to UTN in appropriate year/months
nutrients.99.17<-nutrients.99.17 %>% 
  mutate( UTN_mgl= case_when(Year<2005 ~ UTN_mgl+NO3_N_plus_n02_mg_l, 
                             Year==2005& Month==4~UTN_mgl+NO3_N_plus_n02_mg_l,
                             Year>=2005~UTN_mgl
  ))

# 23 and 24 
str(nut.23.24)

#Transform 23+24 + Rename to match 1999.2017+ add time#######################
nut.23.24<-nut.23.24 %>% 
  mutate( PO4_P_mgl= FRP_ug_L/1000,#phosphorous cycle first
          TDP_mgl=  TFP_ug_L/1000,
          UTP_mgl= TP_ug_L/1000,
          NO3_N_plus_n02_mg_l=`N+N_ug_L`/1000, #then nitrogen cycle
          NH3_N_mg_l= TA_ug_L/1000,
          UTN_mgl= (TKN_ug_L+ `N+N_ug_L`)/1000,
          SiO2_Si_mgl= Rsi_ug_L/1000)  #Silica

#add Day/Month/Year/Depth cols
nut.23.24.transformed<- nut.23.24 %>%
  mutate( Date= as.Date(Sample_date, format= "%m/%d/%Y"), #confirm as date
          Year=  year(Date), #extract
          Month= month(Date),
          Julian_Day=yday(Date),
          Depth_m= `Depth(m)`
  )
str(nut.23.24.transformed)
str(nutrients.99.17) #looks good to join 

#combine
nutrients<- bind_rows(nutrients.99.17, nut.23.24.transformed)

#Transform + Rename 80.96
#
nut.80.96<-
  nut.80.96 %>% 
  mutate( PO4_P_mgl= `Filterable Reactive Phosphorus (mg·L-1 as P)`/1000,#phosphorous cycle first
          TDP_mgl=  `Total Filterable Phosphorus (mg·L-1 as P)`/1000,
          UTP_mgl= `Total Phosphorus (mg·L-1 as P)`/1000,
          NO3_N_plus_n02_mg_l=`Nitrate + Nitrite (mg·L-1 as N)`/1000, #then nitrogen cycle
          NH3_N_mg_l= `Ammonia (mg·L-1 as N)`/1000,
          UTN_mgl= (`Total Kjeldahl Nitrogen (mg·L-1 as N)`+ `Nitrate + Nitrite (mg·L-1 as N)`)/1000,
          SiO2_Si_mgl= `Reactive Silicon (mg·L-1 as Si)`/1000,#Silica
          Iron_mgl= `Iron (mg·L-1)`/1000,
          Particulate_Carbon_mgl=`Particulate Carbon (mg·L-1 as C)`/1000,
          Mg_mgl= `Magnesium (mg·L-1)`, #note not/1000, datasheet is written as mgL
          Ca_mgl= `Calcium (mg·L-1)`, #note not /1000
          Alkalinity_mg_ca03= `Alkalinity (mg·L-1 as CaCO3)`,#note not /1000
          pH = `pH (units)`)
#
# There is also total Mg and Total Ca present in the 1999-2017 dataset that could potentially be the appropriate var to join with ca and MG with.


# add Day/Month/Year/Depth cols and verify
nut.80.96.transformed<-nut.80.96 %>% 
  mutate( Date= make_date(Year, Month, Day), 
          Julian_Day=yday(Date),
          Depth_m= Depth,
          Station= as.factor(`Sampling Site`))
# check str

str(nutrients)
str(nut.80.96.transformed)

#change station to factor
nutrients<-nutrients %>% 
  mutate(Station=as.factor(Station))
#combine
nutrients.dat<- bind_rows(nutrients, nut.80.96.transformed)
# select variables to de clutter data set
nutrients.dat<-nutrients.dat %>% 
  select(Date, Year, Month, Julian_Day, pH, Station,Depth_m, PO4_P_mgl, TDP_mgl, UTP_mgl, NO3_N_plus_n02_mg_l, NH3_N_mg_l, UTN_mgl, SiO2_Si_mgl, Iron_mgl, Particulate_Carbon_mgl,Mg_mgl, Ca_mgl, Alkalinity_mg_ca03)
#
str(nutrients.dat)

## okay writing this to a .csv. Caution that I made transformations/ joins/ decisions that could effect the integrity of this data. Recommend at least reading through code above to make sure you agree with what I did- Safer practice to start with "80_96.clean', "nutrients_1999-2017_clean", and Redoubt Nutrients 2023_2024_complete.csv and perform your own operations.

write.csv( nutrients.dat,"C:/Users/Calvinborges/Desktop/Git upload/data/processed/nutrients.1980.2024.clean.csv")
