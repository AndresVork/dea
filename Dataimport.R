#Libraries

library(dplyr)
library(ggplot2)
library(tidyr)
library(rsdmx)
library(DT)
library(purrr)
library(readr)
library(stringr)
library(tibble)


# THE per capita	
# CHE per capita	? - OECD
# Health & Social Workers	- OECD
# Physicians	- OECD
# Nurses	- OECD
# Hospital beds	- OECD
# HALE	
# DALY	
# IMR	- OECD
# MMR	 - OECD, not good indicator, as zero already
# Avoidable mortality - OECD
# Avoidable hospitalizations - OECD, TODO! How to aggregate


#1. Data from OECD
#https://data-explorer.oecd.org

#Variables 

#Input - Per capita current health expenditure  ---------

#US Dollars per person, PPP converted, constant prices
#Total finacing scheme
#Total health function
#Total mode of provision  - not relevant
#All health care providers
#Price base - 2020
#Time period 2015- latest available

#url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_SHA@DF_SHA,1.1/.A.EXP_HEALTH.USD_PPP_PS._T.._T.._T...?startPeriod=2015"
url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_SHA@DF_SHA,1.1/.A.EXP_HEALTH.USD_PPP_PS._T.._T.._T...Q?startPeriod=2015"

tempcheppp <- readSDMX(url)
dfcheppp <- as.data.frame(tempcheppp)
dfcheppp <- dfcheppp |> select(REF_AREA, obsTime, obsValue, MEASURE) |> 
  mutate(type = "input")
save(dfcheppp, file = "Data/dfcheppp.RData")

head(dfcheppp)

#Output - Avoidable mortality = Preventable mortality + Treatable mortality ---------

#Measure = all 3
#Death per 100 000 inhab
#Sex - both
#Calculation methodology default (should be standardised)

url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_STAT@DF_AM,1.1/.A.PREVM+TRTM+AVM.DT_10P5HB.._T.......?startPeriod=2015"
tempavoid <- readSDMX(url)
dfavoid <- as.data.frame(tempavoid)
dfavoid <- dfavoid |> select(REF_AREA, obsTime, obsValue, MEASURE) |> 
  mutate(type = "output") |> 
  #use inverse values, because we want these to be as small as possible
  mutate(obsValue = 1/obsValue)

save(dfavoid, file = "Data/dfavoid.RData")
head(dfavoid)
table(dfavoid$MEASURE)
#TRTM - treatale mortality
#PREVM - preventable mortality
#AVM - avoidable mortality

# Input - Health workers ----------
#Measure - Health and social employment; Physisians; Nurses
#Unit - Per 1000 inhabitant
#Age - default, total
#Sex - default, total
#Profession - default, total
# Worker Status - default, total
# Activity status - default, total

# We take maximum of activity statuses

url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_EMP_REAC@DF_REAC,1.1/.HSE.10P3HB...VQ+PHYS+MINU...?startPeriod=2015"
tempworker <- readSDMX(url)
dfworker <- as.data.frame(tempworker)
dfworker <- dfworker |> select(REF_AREA, obsTime, obsValue, HEALTH_PROF, 
                               HEALTH_PROF_ACTIVITY_STATUS) 

table(dfworker$HEALTH_PROF_ACTIVITY_STATUS)
#_Z - not applicable : TODO! To check
#LP - licenced to practice
#P - practicing
#PA - professionally active

# We take maximum of activity statuses

dfworker <- dfworker %>%
  group_by(REF_AREA, obsTime, HEALTH_PROF) %>%
  summarise(
    #If all are missing then also max is missing
    maxvalue = if (all(is.na(obsValue))) {
      NA_real_
    } else {
      #otherwise use maximum
      max(obsValue, na.rm = TRUE)
    },
    .groups = "drop"
  )

dfworker <- dfworker |>
  #rename back to have same names
  rename(obsValue = maxvalue) |> 
  #and add cateory
  mutate(type = "input")

#Rename to have the same variable name
dfworker <- dfworker |> rename(MEASURE = HEALTH_PROF)

head(dfworker)
table(dfworker$MEASURE)
#MINU - Nurses
#PHYS - Physicians
#VQ -  Human health and social work activities

#TODO! 

save(dfworker, file = "Data/dfworker.RData")

# Output - LE - life expectancy --------

url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_STAT@DF_HEALTH_STATUS,1.1/.A.LFEXP.Y.Y0._T.......?startPeriod=2015"
temple <- readSDMX(url)
dfle <- as.data.frame(temple)
dfle <- dfle |> select(REF_AREA, MEASURE, obsTime, obsValue) |> 
  mutate(type = "output")
save(dfle, file = "Data/dfle.RData")
head(dfle)
table(dfle$MEASURE)


#TODO! Add other outputs
# Output - HALE - health-adjustede life expectancy

#Input - hospital beds --------
#per 1000

url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_REAC_HOSP@DF_BEDS_SECT,1.1/.HB.10P3HB.._T....?startPeriod=2015"

tempbeds <- readSDMX(url)
dfbeds <- as.data.frame(tempbeds)
dfbeds <- dfbeds |> select(REF_AREA, MEASURE, obsTime, obsValue) |> 
  mutate(type = "input")
save(dfbeds, file = "Data/dfbeds.RData")
head(dfbeds)
table(dfbeds$MEASURE)


#Output - infant mortality
#No minimum gestation period

url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_STAT@DF_HEALTH_STATUS,1.1/.A.INM.DT_10P3BR_L......NONE...?startPeriod=2015"
#url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_STAT@DF_HEALTH_STATUS,1.1/.A.INM.DT_10P3BR_L.........?startPeriod=2015"

tempinm <- readSDMX(url)
dfinm <- as.data.frame(tempinm)
dfinm <- dfinm |> select(REF_AREA, MEASURE, obsTime, obsValue) |> 
  mutate(type = "output") |> 
  #use inverse values, because we want these to be as small as possible
  mutate(obsValue = 1/obsValue)

save(dfinm, file = "Data/dfinm.RData")
#head(dfinm)
#table(dfinm$MEASURE, dfinm$REF_AREA)


#Output - maternal mortality rate
#Note - for many countries it is zero, hence we cannot use it

url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_STAT@DF_HEALTH_STATUS,1.1/.A.MATM..........?startPeriod=2015"
tempmatm <- readSDMX(url)
dfmatm <- as.data.frame(tempmatm)
dfmatm <- dfmatm |> select(REF_AREA, MEASURE, obsTime, obsValue) |> 
  mutate(type = "output") |> 
  #use inverse values, because we want these to be as small as possible
  mutate(obsValue = 1/obsValue)

save(dfmatm, file = "Data/dfmatm.RData")
head(dfmatm)
table(dfmatm$MEASURE, dfmatm$REF_AREA)

#Output - Avoidable hospital admissions ---------s
#TODO! How to aggregate?

url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HCQO@DF_PC,2.1/.A...._T.OBS..?startPeriod=2015"
tempaha <- readSDMX(url)
dfaha <- as.data.frame(tempaha)
dfaha <- dfaha |> select(REF_AREA, MEASURE, obsTime, obsValue) |> 
  mutate(type = "output") |> 
  #use inverse values, because we want these to be as small as possible
  mutate(obsValue = 1/obsValue)

save(dfaha, file = "Data/dfaha.RData")
head(dfaha)


#Append files

dfdata <- bind_rows(dfle, dfavoid, dfworker, dfcheppp, dfbeds, dfinm, dfmatm) 
#head(dfdata)
colnames(dfdata) <- tolower(colnames(dfdata))

#Select necessary countries
#List of countries to keep

mycountriesabbr <- c(
  "AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE",
  "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL",
  "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX",
  "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN",
  "ESP", "SWE", "CHE", "TUR", "GBR", "USA",
  "BGR", "HRV", "CYP","ROU")

dfdata <- dfdata |> 
  filter(ref_area %in% mycountriesabbr)

save(dfdata, file = "Data/dfdata.RData")
save(dfdata, file = "dfdata.RData")
table(dfdata$type, dfdata$measure, useNA = "ifany")

#

