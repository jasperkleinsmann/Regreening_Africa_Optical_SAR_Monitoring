library(terra)
library(sf)
library(lubridate)
library(data.table)
library(tsibble)
library(zoo)
library(tidyverse)
library(scales)

country <- 'Kenya'

load(paste0('output/models/', country, '_l8_fc_plt.RDS'))
load(paste0('output/models/', country, '_l8_armax_plt.RDS'))
l8_ts_plt <- data.table(read_csv(paste0('output/time_series/', country, '_l8_plt.csv')))

load("output/models/Ghana_s1_fc_plt.RDS")
s1_ts_plt <- data.table(read_csv(paste0('output/time_series/', country, '_s1_plt.csv')))

plots <- st_read(paste0('output/plot_data/', country, '/', country, '_plots_final.GeoJSON'))

###### L8
# Clean dates
l8_fc_plt_dt <- l8_fc_plt[,c('.mean', 'yearmonth', 'plotID', 'upper_95', 'lower_95')]
l8_fc_plt_dt$yearmon <- as.Date(ISOdate(year(l8_fc_plt_dt$yearmonth), month(l8_fc_plt_dt$yearmonth), 15))

# Merge actual ndvi with the forecast
l8_fc_plt_dt <- merge(l8_fc_plt_dt, l8_ts_plt[,c('ndvi', 'ndvi_int', 'plotID', 'yearmon')], 
                   by=c('yearmon', 'plotID'), all.x=T)
l8_fc_plt_dt <- data.table(l8_fc_plt_dt)

# Check whether montly observation is within 95% CI
l8_fc_plt_dt$above_95 <- l8_fc_plt_dt$ndvi_int > l8_fc_plt_dt$upper_95
l8_fc_plt_dt <- l8_fc_plt_dt[order(l8_fc_plt_dt$yearmon),]
l8_fc_plt_dt <- l8_fc_plt_dt[order(l8_fc_plt_dt$plotID),]

# Check consequetive months 
l8_fc_plt_dt$fit <- 0
l8_fc_plt_dt$regreening <- NA
l8_fc_plt_dt$sgnf_outlier <- NA

for (plt in unique(l8_fc_plt_dt$plotID)){
  print(plt)
  plot_obs <- l8_fc_plt_dt[l8_fc_plt_dt$plotID==plt,]
  
  index_outlier <- which(plot_obs$above_95==T)
  consq_outlier <- which(diff(index_outlier) == 1)
    
  if (length(consq_outlier) > 0){
    l8_fc_plt_dt$regreening[l8_fc_plt_dt$plotID==plt] <- 1
    l8_fc_plt_dt[l8_fc_plt_dt$plotID==plt,][index_outlier,][consq_outlier+1,]$sgnf_outlier <- 1
  }
}
l8_fc_plt_dt$fit[!is.na(l8_fc_plt_dt$.mean)] <- 1
fwrite(l8_fc_plt_dt, paste0('output/models/l8_greening_', country, '.csv'))
# Regreening yes/no
l8_green <- l8_fc_plt_dt[,.(l8_green=mean(regreening), l8_green_sum=sum(sgnf_outlier, na.rm=T),fit=mean(fit,na.rm=T)), by='plotID']
# Merge with plot data
plots <- merge(plots, l8_green, by='plotID', all.x=T, all.y=F)
plots$l8_green[is.na(plots$l8_green)] <- 0

###### S1
# Clean dates
s1_fc_plt_dt <- s1_fc_plt[,c('.mean', 'yearmonth', 'plotID', 'upper_95', 'lower_95')]
s1_fc_plt_dt$yearmon <- as.Date(ISOdate(year(s1_fc_plt$yearmonth), month(s1_fc_plt$yearmonth), 15))

# Merge actual ndvi with the forecast
s1_fc_plt_dt <- merge(s1_fc_plt_dt, s1_ts_plt[,c('VV', 'VV_int', 'plotID', 'yearmon')], 
                   by=c('yearmon', 'plotID'), all.x=T)
s1_fc_plt_dt <- data.table(s1_fc_plt_dt)

# Check whether montly observation is within 95% CI
s1_fc_plt_dt$above_95 <- s1_fc_plt_dt$VV_int > s1_fc_plt_dt$upper_95
s1_fc_plt_dt <- s1_fc_plt_dt[order(s1_fc_plt_dt$yearmon),]
s1_fc_plt_dt <- s1_fc_plt_dt[order(s1_fc_plt_dt$plotID),]

# Check consequetive months 
s1_fc_plt_dt$regreening <- NA
s1_fc_plt_dt$sgnf_outlier <- NA

for (plt in unique(s1_fc_plt_dt$plotID)){
  print(plt)
  plot_obs <- s1_fc_plt_dt[s1_fc_plt_dt$plotID==plt,]
  
  index_outlier <- which(plot_obs$above_95==T)
  consq_outlier <- which(diff(index_outlier) == 1)
  
  if (length(consq_outlier) > 0){
    s1_fc_plt_dt$regreening[s1_fc_plt_dt$plotID==plt] <- 1
    s1_fc_plt_dt[s1_fc_plt_dt$plotID==plt,][index_outlier,][consq_outlier+1,]$sgnf_outlier <- 1
  }
}
fwrite(s1_fc_plt_dt, paste0('output/models/s1_greening_', country, '.csv'))
# Regreening yes/no
s1_fc_plt_dt[,.(s1_green=mean(regreening)), by='plotID']
# Merge with plot data
plots <- merge(plots, s1_fc_plt_dt[,c('s1_green', 'plotID')], by='plotID', all.x=T, all.y=F)
plots$s1_green[is.na(plots$s1_green)] <- 0


# Merge the TP and FMNR plant date column
plots$plant_date <- fifelse(is.na(plots$plant_date), plots$plant_date_fmnr, plots$plant_date)

#### Save the new plots dataset including greening information
st_write(plots, dsn=paste0('output/plot_data/', country, '/', country, '_plots_green.GeoJSON'), driver='GeoJSON')
plots <- st_read(dsn=paste0('output/plot_data/', country, '/', country, '_plots_green.GeoJSON'))

# Get county level regreening 
plots_dt <- data.table(plots)

county_green <- plots_dt[,.(l8_green=sum(l8_green),
                         number_sites=length(l8_green),
                         perc=sum(l8_green)/length(l8_green),
                         l8_ha=sum(Hectare),
                         l8_ha_green=sum(Hectare[l8_green==1]),
                         l8_ha_perc=sum(Hectare[l8_green==1])/sum(Hectare),
                         avg_size=mean(Hectare,na.rm=T)),
                      by=list(county_region)]
county_green
county_green[county_green$l8_ha>10,]
sum(county_green$l8_ha_green)/sum(county_green$l8_ha)
sum(county_green$l8_ha)

type_green <- plots_dt[,.(l8_green=sum(l8_green),
                         number_sites=length(l8_green),
                         perc=sum(l8_green)/length(l8_green),
                         l8_ha=sum(Hectare),
                         l8_ha_green=sum(Hectare[l8_green==1]),
                         l8_ha_perc=sum(Hectare[l8_green==1])/sum(Hectare)),
                      by=list(type)]
type_green

# Exclude plots where no model was fit
cnt_green_fit <- plots_dt[,.(l8_green=sum(l8_green),
                         number_sites=length(l8_green[fit==1]),
                         perc=sum(l8_green)/length(l8_green[fit==1]),
                         l8_ha=sum(Hectare[fit==1], na.rm=T),
                         l8_ha_green=sum(Hectare[l8_green==1]),
                         l8_ha_perc=sum(Hectare[fit==1], na.rm=T)/sum(Hectare)),
                      by=list(county)]
cnt_green_fit
