library(data.table)
library(tidyverse)
library(dplyr)
library(sf)
library(magrittr)
library(ggplot2)
library(scales)
library(lubridate)
library(BBmisc)
library(emuR)

country <- 'Rwanda'

# Import the reflectance df
refl_l8 <- data.table(read_csv(paste0('output/gee/', country, '_refl_l8_pol.csv')))
refl_s1 <- data.table(read_csv(paste0('output/gee/', country, '_refl_s1.csv')))

#### L8
refl_l8 <- refl_l8[,c('date', 'SR_B4', 'SR_B5', 'SR_B6','QA_PIXEL','plotID')]
# CLEAN AND PROCESS LANDSAT 8 DATA
# Remove rows with NA in red or NIR
refl_l8 <- refl_l8[!is.na(refl_l8$SR_B4)| !is.na(refl_l8$SR_B5),]

# Remove time from datetime
refl_l8$date <- as.Date(refl_l8$date, tryFormats = "%d/%m/%Y")
setorder(refl_l8, cols='date')
refl_l8$yearmon <- as.Date(ISOdate(year(refl_l8$date), month(refl_l8$date), 15))

# Scale DN values to reflectance values
scale_fac <- 0.0000275
offset <- -0.2

refl_l8[,c('SR_B4', 'SR_B5', 'SR_B6')] <- 
  (refl_l8[,c('SR_B4', 'SR_B5', 'SR_B6')] * scale_fac) + offset
# Remove negative reflectance
refl_l8 <- refl_l8[refl_l8$SR_B4 >= 0,]
refl_l8 <- refl_l8[refl_l8$SR_B5 >= 0,]

### Remove clouds
refl_l8$clouds <- 0
# Create function to untangle the bits and return 0 when no cloud
cloud_landsat <- function(x){
  bits <- as.numeric(intToBits(x))[1:16]
  if(bits[7] == 1 && bits[5] == 0 && bits[3] == 0){
    return(0)
  }
  else(return(1))
}
# Apply function to all pixels 
refl_l8$clouds <- lapply(refl_l8$QA_PIXEL, cloud_landsat)
# Filter out pixels with clouds
refl_l8 <- refl_l8[refl_l8$clouds==0,]

# Compute some VIs
refl_l8$ndvi <- (refl_l8$SR_B5 - refl_l8$SR_B4) / (refl_l8$SR_B5 + refl_l8$SR_B4)
refl_l8$osavi <- ((1+0.16)*(refl_l8$SR_B5 - refl_l8$SR_B4)) / (refl_l8$SR_B5 + refl_l8$SR_B4 + 0.16)
refl_l8$ndmi <- (refl_l8$SR_B5 - refl_l8$SR_B6) / (refl_l8$SR_B5 + refl_l8$SR_B5)

# Aggregate values per plot
refl_aggr_l8 <- refl_l8[,.(ndvi=mean(ndvi,na.rm=T), osavi=mean(osavi,na.rm=T), ndmi=mean(ndmi,na.rm=T)), 
                        by=list(plotID, date)]
refl_aggr_l8$yearmon <- as.Date(ISOdate(year(refl_aggr_l8$date), month(refl_aggr_l8$date), 15))


#### S1
# CLEAN AND PRCCESS SENTINEL 1 DATA
# Remove where or VV or VH is NA
refl_s1 <- refl_s1[!is.na(refl_s1$VV) & !is.na(refl_s1$VH),]

# Remove outliers
refl_s1 <- refl_s1[refl_s1$VV > -30 & refl_s1$VV < 0 & refl_s1$VH > -30 & refl_s1$VH < 0,]

# Remove time from datetime
refl_s1$date <- as.Date(refl_s1$date, tryFormats = "%d/%m/%Y")
setorder(refl_s1, cols='date')
refl_s1$yearmon <- as.Date(ISOdate(year(refl_s1$date), month(refl_s1$date), 15))

# Convert from db to power unit
refl_s1$VVl <- 10^(refl_s1$VV/10)
refl_s1$VHl <- 10^(refl_s1$VH/10)
refl_s1$dop <- refl_s1$VVl/(refl_s1$VVl+refl_s1$VHl)

# Compute SAR VI 
refl_s1$rfdi <- (refl_s1$VVl - refl_s1$VHl) / (refl_s1$VVl + refl_s1$VHl)
refl_s1$dbR <- (refl_s1$VHl / refl_s1$VVl)
refl_s1$rvi <- refl_s1$dop * (4*refl_s1$VHl/(refl_s1$VVl+refl_s1$VHl))

# Write VI dt
fwrite(paste0(refl_aggr_l8, 'output/time_series/', country, '_aggr_vi_l8.csv'))
fwrite(refl_s1, paste0('output/time_series/', country, '_aggr_vi_s1.csv'))

refl_s1 <- data.table(read_csv(paste0('output/time_series/', country, '_aggr_vi_s1.csv')))

# Remove not needed variables
rm(scale_fac, 
   dupl_l8, dupl_s2, dupl_l9, dupl_s1, offset, cloud_landsat)
