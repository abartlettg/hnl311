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

# Saving unique values of categorical data for easy access later
ReportTypeValues=unique(hnl311$ReportType)
CurrentStatusValues=unique(hnl311$CurrentStatus)

######## GROUPING REPORTTYPES FOR BETTER VISUALIZATION ########
# This should be moved up to original file read in. #

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



######################## REFINE OR DELETE #######################


# Shows close to half the data is classified as Type OTHER
g = ggplot(data = hnl311, aes(ReportType,))
g + 
  geom_bar() 

# Shows most in Closed status, <5000 Referred, <2000 Null
# Probably as expected for an archive file
g = ggplot(data = hnl311, aes(CurrentStatus,))
g + 
  geom_bar() 

# Records with no Date Closed
DateClosed_ConvertedEmpty = hnl311 %>%
  filter(DateClosed_Converted == '')
# 6971/24375 --- approx 28%

# Percentage of those that are Referred
g = ggplot(data = DateClosed_ConvertedEmpty, aes(CurrentStatus,))
g + 
  geom_bar()
# 4000/6971 --- approx 57% of empties are Referred

# Records with no Date Created
DateCreate_ConvertedEmpty = hnl311 %>%
  filter(DateCreate_Converted == '') 
# 790/24375 --- approx 3%

######################## END OF SECTION TO BE DELETED ######################

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



############################# REFINE OR DELETE###############################
###################### CALCULATIONS & PLOTTING FOR EDA ###################### 

avg_durations=hnl311_nonulldates %>%
  group_by(ReportType) %>%
  summarize(avg=as.duration(mean(as.duration(timespan))))
# Averages here span from 20.22 weeks to 2.87 years by ReportType

as.duration(mean(as.duration(hnl311_nonulldates$clocondate-
                             hnl311_nonulldates$crecondate)))
# Returns [1] "71647924.6291083s (~2.27 years)"

mean(hnl311_nonulldates$WeeksToClose)
# 103

as.duration(median(as.duration(hnl311_nonulldates$clocondate-
                               hnl311_nonulldates$crecondate)))
# Returns [1] "35945243s (~1.14 years)"

median(hnl311_nonulldates$WeeksToClose)
# 59

min(hnl311_nonulldates$clocondate)
max(hnl311_nonulldates$clocondate)
min(hnl311_nonulldates$crecondate)
max(hnl311_nonulldates$crecondate)
# Archive file includes records from 2011-11-05 TO 2019-02-12

min(hnl311_nonulldates$WeeksToClose)
max(hnl311_nonulldates$WeeksToClose)
# From 0.0001240079 to 300.1307

# Boxpolot of Weeks to Close by HL Report Type
g = ggplot(data = hnl311_nonulldates, aes(HLReportType,WeeksToClose))
g + 
  geom_boxplot()

# How many in each category
g = ggplot(data = hnl311_nonulldates, aes(HLReportType,))
g + 
  geom_bar()

# CurrentStatus of Closed vs Referred To Dept relating
# to ReportType and timespan

# Timespan by type in violin shape is interesting, but is this a graph that
# the audience will be able to relate to
g = ggplot(data = hnl311_nonulldates, aes(HLReportType, WeeksToClose))
g + 
  geom_violin()

######## INSIGHT INTO RELATIONSHIPS BETWEEN TIMESTAMP AND CURRENTSTATUS ######
unique(hnl311_nonulldates$CurrentStatus)
# [1] "Closed" "" 

# This shows that by selecting only records w/no nulls in closed/converted,
# there are no records w/a status of 'Referred to Dept'... showing that 
# once a report is Referred, closure is no longer tracked by this system.
# This explains the volume of null dates... probably, the vast majority of
# those are 'Referred to Dept'

unique(hnl311_wnulldates$CurrentStatus)
# [1] "Referred To Dept" "Closed"           ""     

# Visualizing CurrentStatus by clocondate null
g = ggplot(data = hnl311_wnulldates, aes(CurrentStatus,))
g + 
  geom_bar()
# If it does not have a closed date, it may be Referred, Closed, or Null

g = ggplot(data = hnl311_nonulldates, aes(CurrentStatus,))
g + 
  geom_bar()
# If it has a closed date, it is closed

###################### END OF REFINE OR DELETE ##############################





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
#rg_sample10=rg_sample10 %>% rename(latitude = location_1, longitude = location_2)

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


#################### TO BE REFINED/DELETED #############################  

# Visualizing Data by Area

g = ggplot(data = hnl311_wrg, aes(Area,))
g + 
  geom_bar()
# Shows vast majority of reports come from Urban HNL

g = ggplot(data = hnl311_wrg, aes(Area,WeeksToClose))
g + 
  geom_boxplot()

unique(hnl311_wrg$Area)   
    

g = ggplot(data = hnl311_wrg, aes(Area,WeeksToClose))
g + 
  geom_violin()

g = ggplot(data = hnl311_wrg, aes(Area,WeeksToClose))
g + 
  geom_boxplot()
  
g = ggplot(data = hnl311_wrg, aes(Area, fill=HLReportType))
g + 
  geom_bar(position='dodge')



g = ggplot(data = hnl311_wrg, aes(Area, fill=HLReportType))
g + 
  geom_bar(position='fill')



g = ggplot(data = hnl311, aes(HLReportType))
g + 
  geom_bar()

g = ggplot(data = hnl311_wrg, aes(Area, timespan))
g + 
  geom_boxplot()

g = ggplot(data = hnl311_wrg, aes(HLReportType, timespan))
g + 
  geom_boxplot()
######################### END OF REFINE OR DELETE #######################


##### Output Files to be used by Shiny App #####

write.csv(hnl311, file='Output/hnl311.csv')
write.csv(hnl311_wrg, file='Output/hnl311_wrg.csv')
write.csv(hnl311_wrg_mhp, file='Output/hnl311_wrg_mhp.csv')
write.csv(hnl311_nonulldates, file='Output/hnl311_nonulldates.csv')


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



  




