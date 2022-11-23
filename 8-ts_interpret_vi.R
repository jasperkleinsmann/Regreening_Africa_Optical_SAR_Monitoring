library(terra)
library(sf)
library(lubridate)
library(data.table)
library(tsibble)
library(zoo)
library(tidyverse)
library(scales)

country <- 'Rwanda'

load(paste0('output/models/', country, '_l8_fc_plt_ndvi.RDS'))
load(paste0('output/models/', country, '_l8_fc_plt_osavi.RDS'))
load(paste0('output/models/', country, '_l8_fc_plt_ndmi.RDS'))
l8_ts_plt <- data.table(read_csv(paste0('output/time_series/', country, '_l8_plt.csv')))
load(paste0('output/models/', country, '_l8_armax_plt_ndvi.RDS'))

load(paste0('output/models/', country, '_s1_fc_plt_rvi.RDS'))
s1_ts_plt <- data.table(read_csv(paste0('output/time_series/', country, '_s1_plt.csv')))
load(paste0('output/models/', country, '_s1_armax_plt_rvi.RDS'))

plots <- st_read(paste0('output/plot_data/', country, '/', country, '_plots_final.GeoJSON'))

###### L8
# Clean dates
l8_fc_plt_dt <- l8_fc_plt[,c('.mean', 'yearmonth', 'plotID', 'upper_95', 'lower_95')]
l8_fc_plt_dt$yearmon <- as.Date(ISOdate(year(l8_fc_plt_dt$yearmonth), month(l8_fc_plt_dt$yearmonth), 15))

######### VI SPECIFIC #######
# Clean dates -- VI specific
l8_fc_plt_dt_ndvi <- l8_fc_plt[,c('.mean', 'yearmonth', 'plotID', 'upper_95', 'lower_95')]
names(l8_fc_plt_dt_ndvi) <- c('ndvi_fc', 'yearmonth', 'plotID', 'ndvi_upper', 'ndvi_lower')

l8_fc_plt_dt_osavi <- l8_fc_plt_osavi[,c('.mean', 'yearmonth', 'plotID', 'upper_95', 'lower_95')]
names(l8_fc_plt_dt_osavi) <- c('osavi_fc', 'yearmonth', 'plotID', 'osavi_upper', 'osavi_lower')

l8_fc_plt_dt_ndmi <- l8_fc_plt_ndmi[,c('.mean', 'yearmonth', 'plotID', 'upper_95', 'lower_95')]
names(l8_fc_plt_dt_ndmi) <- c('ndmi_fc', 'yearmonth', 'plotID', 'ndmi_upper', 'ndmi_lower')

l8_fc_plt_dt_vi <- merge(l8_fc_plt_dt_ndvi, l8_fc_plt_dt_osavi, by=c('yearmonth', 'plotID'), all.x=T)
l8_fc_plt_dt_vi <- merge(l8_fc_plt_dt_vi, l8_fc_plt_dt_ndmi, by=c('yearmonth', 'plotID'), all.x=T)
l8_fc_plt_dt_vi$yearmon <- as.Date(ISOdate(year(l8_fc_plt_dt_vi$yearmonth), month(l8_fc_plt_dt_vi$yearmonth), 15))

# Merge actual ndvi with the forecast -- VI specific
l8_fc_plt_dt_vi <- merge(l8_fc_plt_dt_vi, l8_ts_plt[,c('ndvi_int', 'osavi_int', 'ndmi_int', 'plotID', 'yearmon')], 
                         by=c('yearmon', 'plotID'), all.x=T)
l8_fc_plt_dt_vi <- data.table(l8_fc_plt_dt_vi)

# Check whether montly observation is within 95% CI -- VI specific
l8_fc_plt_dt_vi$above_95_ndvi <- l8_fc_plt_dt_vi$ndvi_int > l8_fc_plt_dt_vi$ndvi_upper
l8_fc_plt_dt_vi$above_95_osavi <- l8_fc_plt_dt_vi$osavi_int > l8_fc_plt_dt_vi$osavi_upper
l8_fc_plt_dt_vi$above_95_ndmi <- l8_fc_plt_dt_vi$ndmi_int > l8_fc_plt_dt_vi$ndmi_upper

l8_fc_plt_dt_vi <- l8_fc_plt_dt_vi[order(l8_fc_plt_dt_vi$yearmon),]
l8_fc_plt_dt_vi <- l8_fc_plt_dt_vi[order(l8_fc_plt_dt_vi$plotID),]

# Check consequetive months 
l8_fc_plt_dt_vi$fit_ndvi <- 0
l8_fc_plt_dt_vi$regreening_ndvi <- NA
l8_fc_plt_dt_vi$sgnf_outlier_ndvi <- NA

l8_fc_plt_dt_vi$fit_osavi <- 0
l8_fc_plt_dt_vi$regreening_osavi <- NA
l8_fc_plt_dt_vi$sgnf_outlier_osavi <- NA

l8_fc_plt_dt_vi$fit_ndmi <- 0
l8_fc_plt_dt_vi$regreening_ndmi <- NA
l8_fc_plt_dt_vi$sgnf_outlier_ndmi <- NA

for (plt in unique(l8_fc_plt_dt_vi$plotID)){
  print(plt)
  plot_obs <- l8_fc_plt_dt_vi[l8_fc_plt_dt_vi$plotID==plt,]
  
  index_outlier_ndvi <- which(plot_obs$above_95_ndvi==T)
  consq_outlier_ndvi <- which(diff(index_outlier_ndvi) == 1)
  if (length(consq_outlier_ndvi) > 0){
    l8_fc_plt_dt_vi$regreening_ndvi[l8_fc_plt_dt_vi$plotID==plt] <- 1
    l8_fc_plt_dt_vi[l8_fc_plt_dt_vi$plotID==plt,][index_outlier_ndvi,][consq_outlier_ndvi+1,]$sgnf_outlier_ndvi <- 1
  }
  
  index_outlier_osavi <- which(plot_obs$above_95_osavi==T)
  consq_outlier_osavi <- which(diff(index_outlier_osavi) == 1)
  if (length(consq_outlier_osavi) > 0){
    l8_fc_plt_dt_vi$regreening_osavi[l8_fc_plt_dt_vi$plotID==plt] <- 1
    l8_fc_plt_dt_vi[l8_fc_plt_dt_vi$plotID==plt,][index_outlier_osavi,][consq_outlier_osavi+1,]$sgnf_outlier_osavi <- 1
  }
  
  index_outlier_ndmi <- which(plot_obs$above_95_ndmi==T)
  consq_outlier_ndmi <- which(diff(index_outlier_ndmi) == 1)
  if (length(consq_outlier_ndmi) > 0){
    l8_fc_plt_dt_vi$regreening_ndmi[l8_fc_plt_dt_vi$plotID==plt] <- 1
    l8_fc_plt_dt_vi[l8_fc_plt_dt_vi$plotID==plt,][index_outlier_ndmi,][consq_outlier_ndmi+1,]$sgnf_outlier_ndmi <- 1
  }
}
l8_fc_plt_dt_vi$fit_ndvi[!is.na(l8_fc_plt_dt_vi$ndvi_fc)] <- 1
l8_fc_plt_dt_vi$fit_osavi[!is.na(l8_fc_plt_dt_vi$osavi_fc)] <- 1
l8_fc_plt_dt_vi$fit_ndmi[!is.na(l8_fc_plt_dt_vi$ndmi_fc)] <- 1
fwrite(l8_fc_plt_dt_vi, paste0('output/models/l8_greening_vi_', country, '.csv'))
# Regreening yes/no
l8_vi_green <- l8_fc_plt_dt_vi[,.(green_ndvi=mean(regreening_ndvi), green_ndvi_sum=sum(sgnf_outlier_ndvi, na.rm=T),
                               fit_ndvi=mean(fit_ndvi,na.rm=T),
                               green_osavi=mean(regreening_osavi), green_osavi_sum=sum(sgnf_outlier_osavi, na.rm=T),
                               fit_osavi=mean(fit_osavi,na.rm=T),
                               green_ndmi=mean(regreening_ndmi), green_ndmi_sum=sum(sgnf_outlier_ndmi, na.rm=T),
                               fit_ndmi=mean(fit_ndmi,na.rm=T)), by='plotID']
l8_vi_green$green_ndvi[is.na(l8_vi_green$green_ndvi)] <- 0
l8_vi_green$green_osavi[is.na(l8_vi_green$green_osavi)] <- 0
l8_vi_green$green_ndmi[is.na(l8_vi_green$green_ndmi)] <- 0
# Merge with plot data
plots <- merge(plots, l8_vi_green, by='plotID', all.x=T, all.y=F)
st_write(plots, dsn=paste0('output/plot_data/', country, '/', country, '_plots_green_vi.GeoJSON'), driver='GeoJSON')
######### VI SPECIFIC #######
plots$greening <- 0
plots$greening[plots$green_rvi==1] <- 1
plots$greening[plots$green_ndvi==1] <- 2
plots$greening[plots$green_rvi==1 & plots$green_ndvi==1] <- 3


###### S1
# Clean dates
s1_fc_plt_dt <- s1_fc_plt[,c('.mean', 'yearmonth', 'plotID', 'upper_95', 'lower_95')]
s1_fc_plt_dt$yearmon <- as.Date(ISOdate(year(s1_fc_plt$yearmonth), month(s1_fc_plt$yearmonth), 15))

# Merge actual ndvi with the forecast
s1_fc_plt_dt <- merge(s1_fc_plt_dt, s1_ts_plt[,c('rvi_int', 'plotID', 'yearmon')], 
                   by=c('yearmon', 'plotID'), all.x=T)
s1_fc_plt_dt <- data.table(s1_fc_plt_dt)

# Check whether montly observation is within 95% CI
s1_fc_plt_dt$above_95 <- s1_fc_plt_dt$rvi_int > s1_fc_plt_dt$upper_95
s1_fc_plt_dt <- s1_fc_plt_dt[order(s1_fc_plt_dt$yearmon),]
s1_fc_plt_dt <- s1_fc_plt_dt[order(s1_fc_plt_dt$plotID),]

# Check consequetive months
s1_fc_plt_dt$fit <- 0
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
s1_fc_plt_dt$fit[!is.na(s1_fc_plt_dt$.mean)] <- 1
fwrite(s1_fc_plt_dt, paste0('output/models/s1_greening_', country, '.csv'))
# Regreening yes/no
s1_green <- s1_fc_plt_dt[,.(green_rvi=mean(regreening), green_rvi_sum=sum(sgnf_outlier, na.rm=T),
                fit_rvi=mean(fit,na.rm=T)), by='plotID']
s1_green$green_rvi[is.na(s1_green$green_rvi)] <- 0

# Merge with plot data
plots <- merge(plots, s1_green[,c('green_rvi','green_rvi_sum','fit_rvi','plotID')], by='plotID', all.x=T, all.y=F)

##### Combine NDVI and RVI result in single column
plots$greening <- 0
plots$greening[plots$green_ndvi==1] <- 1
plots$greening[plots$green_rvi==1] <- 2
plots$greening[plots$green_ndvi==1 & plots$green_rvi==1] <- 3

#### Save the new plots dataset including greening information
st_write(plots, dsn=paste0('output/plot_data/', country, '/', country, '_plots_green_vi.GeoJSON'), driver='GeoJSON')
plots <- st_read(dsn=paste0('output/plot_data/', country, '/', country, '_plots_green_vi.GeoJSON'))

# Get county level regreening 
plots_dt <- data.table(plots)

cnt_green <- plots_dt[,.(l8_green=sum(green_ndvi),
                         number_sites=length(green_ndvi),
                         perc=sum(green_ndvi)/length(green_ndvi),
                         l8_ha=sum(Hectare),
                         l8_ha_green=sum(Hectare[green_ndvi==1]),
                         l8_ha_perc=sum(Hectare[green_ndvi==1])/sum(Hectare)),
                      by=list(county)]
cnt_green

cnt_green_rvi <- plots_dt[,.(s1_green=sum(green_rvi,na.rm=T),
                         number_sites=length(green_rvi),
                         perc=sum(green_rvi,na.rm=T)/length(green_rvi),
                         s1_ha=sum(Hectare),
                         s1_ha_green=sum(Hectare[green_rvi==1],na.rm=T),
                         s1_ha_perc=sum(Hectare[green_rvi==1],na.rm=T)/sum(Hectare)),
                      by=list(county)]
cnt_green_rvi


plots_dt$land_use[plots_dt$garden=='yes'] <- 'Garden'
plots_dt$land_use[plots_dt$crop_field=='yes'] <- 'Crop field'
plots_dt$land_use[plots_dt$pasture_grassland=='yes'] <- 'Pasture/grassland'
plots_dt$land_use[plots_dt$fallow_bushland=='yes'] <- 'Fallow/bushland'
plots_dt$land_use[plots_dt$other_sites=='yes'] <- 'Other sites'


lu_green <- plots_dt[,.(l8_green=sum(green_ndvi),
                         number_sites=length(green_ndvi),
                         perc=sum(green_ndvi)/length(green_ndvi),
                         l8_ha=sum(Hectare),
                         l8_ha_green=sum(Hectare[green_ndvi==1]),
                         l8_ha_perc=sum(Hectare[green_ndvi==1])/sum(Hectare)),
                      by=list(land_use)]
lu_green <- lu_green[!is.na(lu_green$land_use)]
lu_green


lu_green_rvi <- plots_dt[,.(s1_green=sum(green_rvi,na.rm=T),
                             number_sites=length(green_rvi),
                             perc=sum(green_rvi,na.rm=T)/length(green_rvi),
                             s1_ha=sum(Hectare),
                             s1_ha_green=sum(Hectare[green_rvi==1],na.rm=T),
                             s1_ha_perc=sum(Hectare[green_rvi==1],na.rm=T)/sum(Hectare)),
                          by=list(land_use)]
lu_green_rvi <- lu_green_rvi[!is.na(lu_green_rvi$land_use)]
lu_green_rvi


type_green <- plots_dt[,.(l8_green=sum(green_ndvi),
                         number_sites=length(green_ndvi),
                         perc=sum(green_ndvi)/length(green_ndvi),
                         l8_ha=sum(Hectare),
                         l8_ha_green=sum(Hectare[green_ndvi==1]),
                         l8_ha_perc=sum(Hectare[green_ndvi==1])/sum(Hectare)),
                      by=list(type)]
type_green
View(type_green)

sum(plots_dt$Hectare[plots_dt$green_rvi==1],na.rm=T)/sum(plots_dt$Hectare)

# Exclude plots where no model was fit
cnt_green_fit <- plots_dt[,.(l8_green=sum(l8_green),
                         number_sites=length(l8_green[fit==1]),
                         perc=sum(l8_green)/length(l8_green[fit==1]),
                         l8_ha=sum(Hectare[fit==1], na.rm=T),
                         l8_ha_green=sum(Hectare[l8_green==1]),
                         l8_ha_perc=sum(Hectare[fit==1], na.rm=T)/sum(Hectare)),
                      by=list(county)]
cnt_green_fit



# Assess the model fit of optical and sar
l8_report <- report(l8_armax_plt)
mean(l8_report$AIC)
hist(l8_report$AIC)

s1_report <- report(s1_armax_plt)
mean(s1_report$AIC)
residuals(s1_armax_plt)
hist(s1_report$AIC)


