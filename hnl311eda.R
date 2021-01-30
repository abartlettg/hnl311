# Adriana Bartlett Gray
# EDA - Honolulu 311 Data

################################################################################################
# APPROACH TO EDA:
# ================
#   1 - Explore simple parts of data set (parts that do not require much clean-up or conversion)
#   2 - Reverse Geocoding of Location (coordinates) to create a column for City
#   3 - Explore relationship between City and variables other than Description
#   4 - Text Analysis of Description field
#         - If text analysis becomes too problematic as to threaten on-time completion, STOP
#         - Otherwise, see how far I can go with this
################################################################################################


library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(hms)

hnl311 <- read.csv("C:/Users/adria/NYCDSA/Bootcamp/Project1/DataSets/Honolulu_311_archive.csv")

# Adding columns w/dates in proper date type using lubridate
# Retaining original data in case I need it later
hnl311 = hnl311 %>%
  mutate(crecondate = mdy_hms(DateCreate_Converted), clocondate = mdy_hms(DateClosed_Converted))

# Viewing unique values of categorical data
unique(hnl311$ReportType)
unique(hnl311$CurrentStatus)

# Saving unique values of categorical data for easy access later
ReportTypeValues=unique(hnl311$ReportType)
CurrentStatusValues=unique(hnl311$CurrentStatus)

# Exploratory use of ggplot to visualize parts of data that are already clean

# Shows close to half the data is classified as Type OTHER
#g = ggplot(data = hnl311, aes(ReportType,))
#g + 
#  geom_bar() 

# Shows most in Closed status, <5000 Referred, <2000 Null
# Probably as expected for an archive file
#g = ggplot(data = hnl311, aes(CurrentStatus,))
#g + 
#  geom_bar() 

# But, I recall seeing a lot of NAs for DateClosed_Converted (clocondate)
# Let's see how many nulls
DateClosed_ConvertedEmpty = hnl311 %>%
  filter(DateClosed_Converted == '')
# 6971/24375 --- approx 28%
# This is very high considering this should be automatically time stamped
# when the status is changed.

# This could indicate a data integrity issue, but first, let's see if there
# is a relationship between Status and these empty dates... perhaps, it 
# is related to the 'Referred To Dept' status.

g = ggplot(data = DateClosed_ConvertedEmpty, aes(CurrentStatus,))
g + 
  geom_bar()
# 4000/6971 --- approx 57% of empties are Referred
# But, there are empty dates in all categories still indicating this is is
# not an auto timestamp regardless of change in status OR the timestamp
# can be deleted

# Checking create/conv as well
DateCreate_ConvertedEmpty = hnl311 %>%
  filter(DateCreate_Converted == '') 
# 790/24375 --- approx 3%
# Not a large percentage, but shows this is not an auto timestamp OR the
# timestamp can be deleted

# I'm concerned that empty dates may not be random... that these dates
# could be deleted or hidden for the purpose of reporting better statistics.

# Coming from a background of QA and Data Modeling, it's impossible for me
# not to want to interview/ask questions of the IT bus analysts and users to
# determine if there is a valid reason for empty dates... it's possible that
# this is the result of a 'work-around' from the users' standpoint or that
# this is not a field that should be used at all for calculating anything
# meaningful in terms of response time from the IT bus analysts' standpoint.
#
# But, given that I can not interview/question anyone, I will move forward
# with the disclaimer that the results coming from dates should not be used 
# to make ANY conclusions unless/until we can get a better understanding of
# how these dates are actually modeled/used.

############# CAVEAT - QUESTIONBLE ASSUMPTIONS ############# 

# Analysis of clocondate - crecondate

# Separate out data w/nulls
hnl311_nonulldates = hnl311 %>%
  filter(DateCreate_Converted != '' & DateClosed_Converted != '')

hnl311_wnulldates = hnl311 %>%
  filter(DateCreate_Converted == '' | DateClosed_Converted == '')
# 17404+6971 = 24375 --- Note that there are no records that have a
# closed/conv date that do not have a create/conv date

hnl311_nonulldates = hnl311_nonulldates %>%
  mutate(timespan = as.duration(clocondate-crecondate))

avg_durations=hnl311_nonulldates %>%
 # select (ReportType, timespan) %>%
  group_by(ReportType) %>%
  summarize(avg=as.duration(mean(as.duration(timespan))))
# Averages here span from 20.22 weeks to 2.87 years by ReportType

as.duration(mean(as.duration(hnl311_nonulldates$clocondate-
                             hnl311_nonulldates$crecondate)))
# Returns [1] "71647924.6291083s (~2.27 years)"
  
as.duration(mean(hnl311_nonulldates$clocondate-
                 hnl311_nonulldates$crecondate))
# Also Returns [1] "71647924.6291083s (~2.27 years)"

as.duration(median(hnl311_nonulldates$clocondate-
                   hnl311_nonulldates$crecondate))
# Median is not that different [1] "74782937s (~2.37 years)"

min(hnl311_nonulldates$clocondate)
max(hnl311_nonulldates$clocondate)
min(hnl311_nonulldates$crecondate)
max(hnl311_nonulldates$crecondate)

as.duration(min(as.duration(hnl311_nonulldates$timespan)))
as.duration(max(as.duration(hnl311_nonulldates$timespan)))

# I need to view the data in ggplot to visualize what's going on
# Boxpolot of mean timespan from create to close for different Report Types
g = ggplot(data = hnl311_nonulldates, aes(ReportType,as.duration(timespan)))
g + 
  geom_boxplot()
# Shows that timespan is pretty wide for most categories.
# Parking is the only category that has a limited span, but this could be 
# due to not that many in the category

hnl311_nonulldates %>%
  filter(ReportType=='Parking')
# Yes, only one entry for 'Parking' category

# How many in each category
g = ggplot(data = hnl311_nonulldates, aes(ReportType,))
g + 
  geom_bar()
# Other than 'Other', Vehicle is the highest which makes sense because the vast
# majority of SFHs in Hawaii are not in HOAs so there in no middleman to 
# monitor and remedy the issue of abandoned vehicles.  These have to be 
# reported to the City & County of Honolulu to address and it does take a long,
# long time for them to deal with it all.


# CurrentStatus of Closed vs Referred To Dept relating
# to ReportType and timespan

# Timespan by type in violin shape is interesting, but is this a graph that
# the audience will be able to relate to
g = ggplot(data = hnl311_nonulldates, aes(ReportType, timespan))
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
# Good amount in all statuses

g = ggplot(data = hnl311_nonulldates, aes(CurrentStatus,))
g + 
  geom_bar()
# Virtually all Closed... tiny amount empty

##############################################################################

# Try to change timespan unit from seconds to days for final graphs since this
# will be easier for audience to wrap their minds around.

##################### Attempt at reverse geocoding ########################## 
library(revgeo)
revgeo(longitude=-158.00, latitude=21.39, 
       provider = 'photon')  
#photon supposed to be free service, but doesn't work

#21.396820276, -158.009726946 -- sample in Hawaii

# Try sample file for Texas A&M reverse geocoding batch processing service
# Signed up and have 2,500 record credit
rg_sample=head(hnl311_nonulldates)
rg_sample=rg_sample %>%
  select(id, location) %>%
  mutate(state='HI')

# Need to split coordinates into separate fields to process file
library(stringr)

rg_sample$location=str_replace_all(rg_sample$location, "[)()]", "")
rg_sample$location=strsplit(rg_sample$location, ', ')

#rg_sample=rg_sample %>%
#  mutate(latitude=location[:1], longitude=location[:2])

#rg_sample=rg_sample %>% pivot_wider(location)
rg_sample=rg_sample %>% unnest_wider(location, names_sep = "_")
rg_sample=rg_sample %>% rename(latitude = location_1, longitude = location_2)

write.csv(rg_sample, file='rgsample.csv')

rgsample_complete  #Processing by Texas A&M looks good
# To use this, I need to extract a sample of the records within my free limit
# Then, join on id to add ComputedCity back to the sample
# Then, analysis of anything having to do w/ComputedCity has to be done
# against the sample.
#################### DO NOT RUN AGAIN - PROCESSING COMPLETE ################# 
# Preparing sample of 10% of data to reverse geocode
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

##############################################################################

#rg_sample$latitude=rg_sample$...1
#rg_sample$longitude=rg_sample$...2

#rg_sample %>% pivot_wider(location)

# Stop here for now... may be going down a rabbit hole w/this
# All services I find cost money for the volume of data I'm processing
# If I use this at all, I could sample 10% of the records since I have
# a free 2,500 records I can process

# IMPORTANT... I CAN STILL USE LOCATION TO POSSIBLY DISPLAY DATA IN A MAP
# FORMAT, BUT ONLY IF IT SEEMS RELEVANT AND CAN BE DONE IN SUCH A WAY AS
# TO CONVEY MEANINGFUL INFORMATION

########################################################################



  