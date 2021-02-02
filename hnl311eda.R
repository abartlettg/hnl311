# Adriana Bartlett Gray
# EDA - Honolulu 311 Data

# Load libraries & read in files

library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

hnl311 <- read.csv("Data/Honolulu_311_archive.csv")
rgsample10_complete <- read_csv("Data/rgsample10_complete.csv")
HIMedHomePrices <- read_csv("Data/HIMedHomePrices.csv")

# Adding columns w/dates in proper date type using lubridate
hnl311 = hnl311 %>%
  mutate(crecondate = mdy_hms(DateCreate_Converted), 
         clocondate = mdy_hms(DateClosed_Converted))


######## GROUPING REPORTTYPES FOR BETTER VISUALIZATION ########


RoadSafety = c("Broken / Vandalized signs", "Sign", "Light", "Pothole", 
               "Roadway")
Water = c("Stream", "Stormwater", "Flooding")
Trash = c("Trash")
Homeless = c("Homeless")
Tree = c("Tree")
Vehicle = c("Vehicle","Taxi","Parking")

hnl311 = hnl311 %>%
  mutate(HLReportType=case_when(
    ReportType %in% RoadSafety == TRUE ~ 'RoadSafety',
    ReportType %in% Water == TRUE ~ 'Water',
    ReportType %in% Trash == TRUE ~ 'Trash',
    ReportType %in% Homeless == TRUE ~ 'Homeless',
    ReportType %in% Tree == TRUE ~ 'Tree',
    ReportType %in% Vehicle == TRUE ~ 'Vehicle',))

hnl311$HLReportType = 
  factor(hnl311$HLReportType, 
         levels=c('Vehicle','Water','Tree','RoadSafety','Trash','Homeless'), 
         ordered=TRUE)




############# REMOVING RECORDS W/NULL REPORTTYPE ##################

# Filtering out records w/no HLReportType
hnl311 = hnl311 %>%
  filter(HLReportType != 'NA')
# Reduces records by 10k out of 24k... the problem of nulls





############# SEPARATING DATA W/NULL DATES VS W/NO NULL DATES #############
###### RECORDS W/NO NULL DATES TO BE USED FOR TIME TO CLOSE ANALYSIS ######
### WHILE RETAINING FULL SET OF RECORDS FOR ANALYSIS NOT INVOLVING TIME ###

# Filter out data w/nulls
hnl311_nonulldates = hnl311 %>%
  filter(DateCreate_Converted != '' & DateClosed_Converted != '')

hnl311_wnulldates = hnl311 %>%
  filter(DateCreate_Converted == '' | DateClosed_Converted == '')
# 17404+6971 = 24375 --- Note that there are no records that have a
# closed/conv date that do not have a create/conv date

# Adding timespan (represented in seconds)
hnl311_nonulldates = hnl311_nonulldates %>%
  mutate(timespan = as.duration(clocondate-crecondate))

# Adding WeeksToClose (represented in weeks for better visualization)
hnl311_nonulldates = hnl311_nonulldates %>%
  mutate(WeeksToClose=time_length(timespan,unit="weeks"))




#################### DO NOT RUN AGAIN - PROCESSING COMPLETE #################
########### ONE TIME RUN TO PREPARE SAMPLE FOR REVERSE GEOCODING ############
########## COMMENTING & SAVING IN CASE FUNDING RECEIVED FOR FULL RG #########

# Preparing sample of 10% of data to reverse geocode using Texas A&M free
# batch geocoding (limited to 2500 records for free)

#rg_sample10=sample_n(hnl311_nonulldates, 1741)

#rg_sample10=rg_sample10 %>%
#  select(id, location) %>%
#  mutate(state='HI')

#rg_sample10$location=str_replace_all(rg_sample10$location, "[)()]", "")
#rg_sample10$location=strsplit(rg_sample10$location, ', ')

#rg_sample10=rg_sample10 %>% unnest_wider(location, names_sep = "_")
#rg_sample10=rg_sample10 %>% 
#            rename(latitude = location_1, longitude = location_2)

#write.csv(rg_sample10, file='rgsample10.csv')
#rgsample10_complete


######## THIS DATA ONLY TO BE USED WHEN SEGMENTING BY CITY/AREA/MHP ##########
##############################################################################
############ JOINING REVERSE GEOCODE PROCESSED W/SAMPLE 10% DF ###############

# Join processed reverse geocoding file w/ComputedCity
hnl311_wrg = hnl311_nonulldates %>%
  left_join(rgsample10_complete, by='id')

# Remove records where reverse geocoding did not result in a ComputedCity
hnl311_wrg = hnl311_wrg %>%
  filter(ComputedCity != '')

######## GROUPING CITIES INTO AREAS FOR BETTER VISUALIZATION ########

UrbanHNL = c('Urban Honolulu') 
EastHNL = c('East Honolulu')
Windward = c('Kaneohe', 'Kailua', 'Hauula', 'Waimanalo')
Central = c('Mililani','Mililani Town', 'Wahiawa','Aiea','Pearl City',
            'Waipahu','Waimalu', 'Wheeler AFB')
Leeward = c('Waianae','Ewa Beach','Kapolei','Ewa Gentry', 'Barbers Point N A S')
NorthShore = c('Waialua','Haleiwa','Kahuku','Laie','Kaaawa')

hnl311_wrg = hnl311_wrg %>%
  mutate(Area=case_when(
    ComputedCity %in% NorthShore == TRUE ~ 'NorthShore',
    ComputedCity %in% Leeward == TRUE ~ 'Leeward',
    ComputedCity %in% UrbanHNL == TRUE ~ 'UrbanHNL',
    ComputedCity %in% EastHNL == TRUE ~ 'EastHNL',
    ComputedCity %in% Windward == TRUE ~ 'Windward',
    ComputedCity %in% Central == TRUE ~ 'Central',))

hnl311_wrg$Area = 
  factor(hnl311_wrg$Area, 
         levels=c('NorthShore','EastHNL','Windward','Leeward','UrbanHNL'), 
         ordered=TRUE)

############ FILTERING OUT AREA == NA ###########
hnl311_wrg = hnl311_wrg %>%
  filter(Area != 'NA')

###### JOINING CENSUS DATA ON MEDIAN HOME PRICES BY CITY ######
hnl311_wrg_mhp = inner_join(hnl311_wrg, HIMedHomePrices)


#### PLACING MEDIUM HOME PRICES IN RANGES FOR BETTER VISUALIZATION ####
hnl311_wrg_mhp = hnl311_wrg_mhp %>%
  mutate(MedianHomePrice=case_when(
    MHP>=300000 & MHP<=400000 ~ '300ks',
    MHP>=400000 & MHP<=500000 ~ '400ks',
    MHP>=500000 & MHP<=600000 ~ '500ks',
    MHP>=600000 & MHP<=700000 ~ '600ks',
    MHP>=700000 & MHP<=800000 ~ '700ks',
    MHP>=800000 & MHP<=900000 ~ '800ks',
    MHP>=900000 & MHP<=1000000 ~ '900ks',))

# Make MedianHomePrice a Factor
hnl311_wrg_mhp$MedianHomePrice = 
  factor(hnl311_wrg_mhp$MedianHomePrice, 
         levels=c('300ks','400ks','500ks','600ks','700ks','800ks','900ks'), 
         ordered=TRUE)




###################### OVERVIEW PLOTS #########################

################## DON'T USE REVERSE GEOCODING ################

# Number by Type (all)

g = ggplot(data = hnl311, aes(HLReportType))
g + 
  geom_bar()

# Timespan by Type (all) 

g = ggplot(data = hnl311_nonulldates, aes(HLReportType, WeeksToClose))
g + 
  geom_boxplot()

# Problem of Nulls - piechart to show how big null problem is

############### HLTYPE BY AREA/CITY/MHP PLOTS ################

################## MUST USE REVERSE GEOCODING ################
############## TYPE BY AREA/CITY/MHP PLOTS ###############
# User selects an Area, City, MPH Range or All
# Graph is returned showing amounts of reports in each HLType

hnl311_wrg %>%
  filter(Area=='Windward') %>%
  ggplot(aes(HLReportType)) +
  geom_bar()

hnl311_wrg %>%
  filter(ComputedCity=='Kaneohe') %>%
  ggplot(aes(HLReportType)) +
  geom_bar()

hnl311_wrg_mhp %>%
  filter(MedianHomePrice=='900k+') %>%
  ggplot(aes(HLReportType)) +
  geom_bar()

############## TIMESPAN BY AREA/CITY/MHP PLOTS ###############
# User selects an Area, City, MPH Range or All
# Graph is returned showing timespan by HLType
hnl311_wrg %>%
  filter(Area=='Windward') %>%
  ggplot(aes(HLReportType,WeeksToClose)) +
  geom_boxplot()

hnl311_wrg %>%
  filter(ComputedCity=='Kaneohe') %>%
  ggplot(aes(HLReportType,WeeksToClose)) +
  geom_boxplot()

hnl311_wrg_mhp %>%
  filter(MedianHomePrice=='900k+') %>%
  ggplot(aes(HLReportType,WeeksToClose)) +
  geom_boxplot()



  




