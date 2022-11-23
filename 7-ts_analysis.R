
library(feasts)
library(tsibble)
library(forecast)
library(stats)
library(zoo)
library(astsa)
library(urca) # for kpss test
library(TSA)
library(fable)
library(data.table)
library(tidyverse)
library(dplyr)
library(sf)
library(magrittr)
library(ggplot2)
library(scales)
library(lubridate)
library(BBmisc)
library(readr)

country <- 'Kenya'

# Import the l8 and s1 timeseries 
l8_ts <- data.table(read_csv(paste0('output/time_series/', country, '_l8_ts.csv')))
s1_ts <- data.table(read_csv(paste0('output/time_series/', country, '_s1_ts.csv')))

# Import the plots data in include the plant date in the graph
plots <- data.table(read_csv(paste0('output/plot_data/', country, '/',country, '_plots_filtered.csv')))

# Filter out time stamps before the observation
l8_ts <- l8_ts[l8_ts$yearmon > as.Date('2013-01-01') & l8_ts$yearmon < as.Date('2022-11-01')] # start L8 observation
s1_ts <- s1_ts[s1_ts$yearmon > as.Date('2014-01-01') & s1_ts$yearmon < as.Date('2022-11-01')] # start S1 observation

############################# Create tsibble with the lagged variables 
#### L8
l8_ts_lag <- l8_ts %>% 
  group_by(yearmon) %>% 
  summarize(ndvi=mean(ndvi,na.rm=T),
            prcp=mean(prcp_month))

l8_ts_lag <- l8_ts_lag %>% 
  filter(yearmon >= min(l8_ts$date, na.rm=T)) %>% # Remove rows before first observation
  mutate(yearmonth=yearmonth(as.character(yearmon)),
         prcp_lag1=lag(prcp,1),
         prcp_lag2=lag(prcp,2),
         ndvi_int=na.approx(ndvi)) %>% # Fil gaps in time series
  as_tsibble(index=yearmonth)

l8_ref_lag <- l8_ts_lag %>% filter(year(yearmonth) < 2018)
l8_val_lag <- l8_ts_lag %>% filter(year(yearmonth) >= 2018)

#### S1
s1_ts_lag <- s1_ts %>% 
  group_by(yearmon) %>% 
  summarize(rvi=mean(VV,na.rm=T),
            prcp=mean(prcp_month))

s1_ts_lag <- s1_ts_lag %>% 
  filter(yearmon >= min(s1_ts$date, na.rm=T)) %>% # Remove rows before first observation
  mutate(yearmonth=yearmonth(as.character(yearmon)),
         prcp_lag1=shift(prcp,n=1,type='lag'),
         prcp_lag2=shift(prcp,n=2,type='lag'),
         rvi_int=na.approx(rvi)) %>% # Fil gaps in time series 
  as_tsibble(index=yearmonth)

s1_ref_lag <- s1_ts_lag %>% filter(year(yearmonth) < 2018)
s1_val_lag <- s1_ts_lag %>% filter(year(yearmonth) >= 2018)
########################### End creating tsibble


##################################### Fit and test full model
#### l8
# Fit arimax model and look at coeficients
l8_armax_mod <- l8_ref_lag %>% 
  model(arima = ARIMA(ndvi ~ prcp + prcp_lag1 + prcp_lag2, stepwise=T, ic='aic')) %>% 
  report()
gg_tsresiduals(l8_armax_mod)

l8_fc_ts <- fabletools::forecast(l8_armax_mod, new_data = l8_val_lag[,c('yearmonth', 'prcp', 'prcp_lag1', 'prcp_lag2')])

l8_ts_ci <- l8_fc_ts$ndvi %>% 
  hilo(level = 95) 
l8_fc_ts <- l8_fc_ts %>% 
  mutate(upper_95=l8_ts_ci$upper,
         lower_95=l8_ts_ci$lower)

# Plot full model
ggplot()+
  geom_line(data=l8_ref_lag, 
            aes(x = yearmonth, y = ndvi_int - residuals(l8_armax_mod)$.resid), 
            col='darkred',lwd=0.9, alpha=0.8) +
  geom_line(data=l8_ts_lag, aes(x=yearmonth, y=ndvi_int), col='darkgreen',lwd=1)+
  geom_line(data=l8_fc_ts, aes(x = yearmonth, y = .mean), 
            col='darkred',lwd=0.9, lty=1)+
  #geom_line(data=l8_ts_lag, aes(x=yearmon, y=prcp/600), col='blue')
  geom_ribbon(data=l8_fc_ts, aes(x=yearmonth, ymin=lower_95, ymax=upper_95),
              alpha=0.1, fill='red', lwd=0.8)+
  labs(y='NDVI', x='Time', title=paste('Actual (green) vs predicted (red) vegetation including 95% CI'))

#### S1
# Fit arimax model and look at coeficients
s1_armax_mod <- s1_ref_lag %>% 
  model(arima = ARIMA(rvi_int ~ prcp + prcp_lag1 + prcp_lag2, stepwise=T, ic='aic')) %>% 
  report()
gg_tsresiduals(s1_armax_mod)

s1_fc_ts <- fabletools::forecast(s1_armax_mod, new_data = s1_val_lag[,c('yearmonth', 'prcp', 'prcp_lag1', 'prcp_lag2')])

s1_ts_ci <- s1_fc_ts$rvi_int %>% 
  hilo(level = 95) 
s1_fc_ts <- s1_fc_ts %>% 
  mutate(upper_95=s1_ts_ci$upper,
         lower_95=s1_ts_ci$lower)

# Plot full model
ggplot()+
  geom_line(data=s1_ref_lag, 
            aes(x = yearmonth, y = rvi_int - residuals(s1_armax_mod)$.resid), 
            col='darkred',lwd=0.9, alpha=0.8) +
  geom_line(data=s1_ts_lag, aes(x=yearmonth, y=rvi_int), col='darkgreen',lwd=1)+
  geom_line(data=s1_fc_ts, aes(x = yearmonth, y = .mean), 
            col='darkred',lwd=0.9, lty=1)+
  geom_ribbon(data=s1_fc_ts, aes(x=yearmonth, ymin=lower_95, ymax=upper_95),
              alpha=0.1, fill='red', lwd=0.8)+
  labs(y='RVI', x='Time', title=paste('Actual (green) vs predicted (red) vegetation including 95% CI'))



#################################################################### PER COUNTY
################### Aggregate per county and yearmon
#### L8
# Take mean ndvi and prcp
l8_ts_by_cnt <- l8_ts %>% 
  group_by(county_region, yearmon) %>%
  summarize(ndvi=mean(ndvi, na.rm=T),
            prcp=mean(prcp_month)) 

l8_ts_by_cnt <- l8_ts_by_cnt %>% rename(county=county_region)

# Lag the prcp
l8_ts_by_cnt <- l8_ts_by_cnt %>% 
  mutate(prcp_lag1=lag(prcp, 1),
         prcp_lag2=lag(prcp, 2))

# Identify the first observation date
l8_first_obs <- l8_ts_by_cnt %>%
  filter(!is.na(ndvi)) %>% 
  summarize(first_obs=min(yearmon),
            last_obs=max(yearmon))

# Exclude rows before 1st observation for each county
l8_ts_cnt <- l8_ts_by_cnt[l8_ts_by_cnt$county==l8_first_obs$county[1] & l8_ts_by_cnt$yearmon >= 
                            l8_first_obs$first_obs[1] & l8_ts_by_cnt$yearmon <= l8_first_obs$last_obs[1],]
for (i in 2:nrow(l8_first_obs)){
  print(i)
  plot_tsibble <- l8_ts_by_cnt[l8_ts_by_cnt$county==l8_first_obs$county[i] & l8_ts_by_cnt$yearmon >= 
                                 l8_first_obs$first_obs[i] & l8_ts_by_cnt$yearmon <= l8_first_obs$last_obs[i],]
  l8_ts_cnt <- rbind(l8_ts_cnt, plot_tsibble)
}

# Make it a tsibble and fill with lagged variables
l8_ts_cnt <- l8_ts_cnt %>% 
  mutate(yearmonth=yearmonth(as.character(yearmon)),
         ndvi_int=na.approx(ndvi)) %>% 
  as_tsibble(index=yearmonth, key=county)

# Create reference and validation datasets
l8_ref_cnt <- l8_ts_cnt %>% filter(year(yearmonth) < 2018)
l8_val_cnt <- l8_ts_cnt %>% filter(year(yearmonth) >= 2018)


#### S1
s1_ts_by_cnt <- s1_ts %>% 
  group_by(county, yearmon) %>% 
  summarize(rvi=mean(rvi, na.rm=T),
            prcp=mean(prcp_month)) 

s1_ts_by_cnt <- s1_ts_by_cnt %>% 
  mutate(prcp_lag1=shift(prcp, n=1, type='lag'),
         prcp_lag2=shift(prcp, n=2, type='lag'))

# Identify the first observation date
s1_first_obs <- s1_ts_by_cnt %>%
  filter(!is.na(rvi)) %>% 
  summarize(first_obs=min(yearmon),
            last_obs=max(yearmon))

# Exclude rows before 1st observation for each county
s1_ts_cnt <- s1_ts_by_cnt[s1_ts_by_cnt$county==s1_first_obs$county[1] & s1_ts_by_cnt$yearmon >= 
                            s1_first_obs$first_obs[1] & s1_ts_by_cnt$yearmon <= s1_first_obs$last_obs[1],]
for (i in 2:nrow(l8_first_obs)){
  print(i)
  plot_tsibble <- s1_ts_by_cnt[s1_ts_by_cnt$county==s1_first_obs$county[i] & s1_ts_by_cnt$yearmon >= 
                                 s1_first_obs$first_obs[i] & s1_ts_by_cnt$yearmon <= s1_first_obs$last_obs[i],]
  s1_ts_cnt <- rbind(s1_ts_cnt, plot_tsibble)
}

# Make it a tsibble and fill with lagged variables
s1_ts_cnt <- s1_ts_cnt %>% 
  mutate(yearmonth=yearmonth(as.character(yearmon)),
         rvi_int=na.approx(rvi)) %>% 
  as_tsibble(index=yearmonth, key=county)

# Create reference and validation datasets
s1_ref_cnt <- s1_ts_cnt %>% filter(year(yearmonth) < 2018)
s1_val_cnt <- s1_ts_cnt %>% filter(year(yearmonth) >= 2018)

##################################### End making COUNTY tsibbles


##################################### Forecast vegetation signal per COUNTY
#### L8
l8_armax_cnt <- l8_ref_cnt %>%
  model(ARIMA(ndvi_int ~ prcp + prcp_lag1 + prcp_lag2, stepwise=T))
l8_fc_cnt <- fabletools::forecast(l8_armax_cnt, new_data = l8_val_cnt[,c('yearmonth', 'prcp', 'prcp_lag1', 'prcp_lag2', 'county')])

fabletools::forecast(l8_armax_cnt, new_data = l8_val_cnt[,c('yearmonth', 'prcp','prcp_lag1', 'prcp_lag2', 'county')]) %>%
  autoplot(l8_ts_cnt)+
  labs(x='Time', y='NDVI')
accuracy(l8_fc_cnt, l8_ts_cnt)

# Extract confidence levels
l8_cnt_ci <- l8_fc_cnt$ndvi_int %>% 
  hilo(level = 95) 
l8_fc_cnt <- l8_fc_cnt %>% 
  mutate(upper_95=l8_cnt_ci$upper,
         lower_95=l8_cnt_ci$lower)
# Plot
cnt <- 'Nyatike'
ggplot()+
  geom_line(data=l8_ref_cnt[l8_ref_cnt$county==cnt,], 
            aes(x = yearmonth, y = ndvi - residuals(l8_armax_cnt[l8_armax_cnt$county==cnt,])$.resid), 
            col='darkred',lwd=0.65, alpha=0.6) +
  geom_line(data=l8_ts_cnt[l8_ts_cnt$county==cnt,], aes(x=yearmonth, y=ndvi_int), col='darkgreen',lwd=0.7)+
  geom_line(data=l8_ts_cnt[l8_ts_cnt$county==cnt,], 
            aes(x=yearmonth, y=normalize(prcp, method='range',c(0.1,0.2))), col='blue')+
  geom_line(data=l8_fc_cnt[l8_fc_cnt$county==cnt,], aes(x = yearmonth, y = .mean), 
            col='darkred',lwd=0.65, lty=2)+
  geom_ribbon(data=l8_fc_cnt[l8_fc_cnt$county==cnt,], aes(x=yearmonth, ymin=lower_95, ymax=upper_95),
              alpha=0.1, fill='red')+
  labs(y='NDVI', x='Time', title=paste('Actual (green) vs predicted (red) NDVI in',cnt,'including 95% CI'))

#### S1
s1_armax_cnt <- s1_ref_cnt %>%
  model(ARIMA(VV_int ~ prcp + prcp_lag1 + prcp_lag2 + pdq(1, 0, 1) + PDQ(0, 0, 0)))
s1_fc_cnt <- forecast(s1_armax_cnt, new_data = s1_val_cnt[,c('yearmonth', 'prcp', 'prcp_lag1', 'prcp_lag2', 'county')])

forecast(s1_armax_cnt, new_data = s1_val_cnt[,c('yearmonth', 'prcp', 'prcp_lag1', 'prcp_lag2', 'county')]) %>%
  autoplot(s1_ts_cnt)+
  labs(x='Time', y='NDVI')
accuracy(s1_fc_cnt, s1_ts_cnt)

# Extract confidence levels
s1_cnt_ci <- s1_fc_cnt$VV_int %>% 
  hilo(level = 95) 
s1_fc_cnt <- s1_fc_cnt %>% 
  mutate(upper_95=s1_cnt_ci$upper,
         lower_95=s1_cnt_ci$lower)

# Plot
cnt <- 'Bwaku West'
ggplot()+
  geom_line(data=s1_ref_cnt[s1_ref_cnt$county==cnt,], 
            aes(x = yearmonth, y = VV_int - residuals(s1_armax_cnt[s1_armax_cnt$county==cnt,])$.resid), 
            col='darkred',lwd=0.65, alpha=0.6) +
  geom_line(data=s1_ts_cnt[s1_ts_cnt$county==cnt,], aes(x=yearmonth, y=VV_int), col='darkgreen',lwd=0.7)+
  geom_line(data=s1_ts_cnt[s1_ts_cnt$county==cnt,], 
            aes(x=yearmonth, y=normalize(prcp, method='range',c(-15,-14))), col='blue')+
  geom_line(data=s1_fc_cnt[s1_fc_cnt$county==cnt,], aes(x = yearmonth, y = .mean),
            col='darkred',lwd=0.65,lty=2)+
  geom_ribbon(data=s1_fc_cnt[s1_fc_cnt$county==cnt,], aes(x=yearmonth, ymin=lower_95, ymax=upper_95),
              alpha=0.1, fill='red')+
  labs(y='VVn', x='Time', title=paste('Actual (green) vs predicted (red) VV backscatter in',cnt,'including 95% CI'))

############################################################### END PER COUNTY



############################################################### PER PLOT
################### Aggregate per plot and yearmonth
##### L8
l8_ts_by_plt <- l8_ts %>% 
  group_by(plotID, yearmon) %>% 
  summarize(ndvi=mean(ndvi, na.rm=T),
            prcp=mean(prcp)) 
# Make lagged variables
l8_ts_by_plt <- l8_ts_by_plt %>% 
  mutate(prcp_lag1=data.table::shift(prcp, n=1, type='lag'),
         prcp_lag2=data.table::shift(prcp, n=2, type='lag')) 

# Identify the first observation date 
l8_first_obs_plt <- l8_ts_by_plt %>%
  filter(!is.na(ndvi)) %>% 
  summarize(first_obs=min(yearmon),
            last_obs=max(yearmon))
# Exclude rows before 1st observation for each county
l8_ts_plt <- l8_ts_by_plt[l8_ts_by_plt$plotID==l8_first_obs_plt$plotID[1] & l8_ts_by_plt$yearmon >= 
                            l8_first_obs_plt$first_obs[1] & l8_ts_by_plt$yearmon <= l8_first_obs_plt$last_obs[1],]
for (i in 2:nrow(l8_first_obs_plt)){
  print(i)
  plot_tsibble <- l8_ts_by_plt[l8_ts_by_plt$plotID==l8_first_obs_plt$plotID[i] & l8_ts_by_plt$yearmon >= 
                                 l8_first_obs_plt$first_obs[i] & l8_ts_by_plt$yearmon <= l8_first_obs_plt$last_obs[i],]
  l8_ts_plt <- rbind(l8_ts_plt, plot_tsibble)
}

# Make it a tsibble and interpolate the NAs
l8_ts_plt <- l8_ts_plt %>% 
  mutate(ndvi_int=na.approx(ndvi))

fwrite(l8_ts_plt, paste0('output/time_series/', country, '_l8_plt.csv'))
l8_ts_plt <- data.table(read_csv(paste0('output/time_series/', country, '_l8_plt.csv')))

l8_ts_plt <- l8_ts_plt %>% 
  mutate(yearmonth=yearmonth(as.character(yearmon))) %>% 
  as_tsibble(index=yearmonth, key=plotID)

# Create reference and validation datasets
l8_ref_plt <- l8_ts_plt %>% filter(year(yearmonth) < 2017)
l8_val_plt <- l8_ts_plt %>% filter(year(yearmonth) >= 2017)




##### S1
s1_ts_by_plt <- s1_ts %>% 
  group_by(plotID, yearmon) %>% 
  summarize(VV=mean(VV, na.rm=T),
            prcp=mean(prcp_month)) 
# Make lagged variables
s1_ts_by_plt <- s1_ts_by_plt %>% 
  mutate(prcp_lag1=shift(prcp, n=1, type='lag'),
         prcp_lag2=shift(prcp, n=2, type='lag')) 

# Identify the first observation date 
s1_first_obs_plt <- s1_ts_by_plt %>%
  filter(!is.na(VV)) %>% 
  summarize(first_obs=min(yearmon),
            last_obs=max(yearmon))
# Exclude rows before 1st observation for each county
s1_ts_plt <- s1_ts_by_plt[s1_ts_by_plt$plotID==s1_ts_by_plt$plotID[1] & s1_ts_by_plt$yearmon >= 
                            s1_first_obs_plt$first_obs[1] & s1_ts_by_plt$yearmon <= s1_first_obs_plt$last_obs[1],]
for (i in 2:nrow(s1_first_obs_plt)){
  print(i)
  plot_tsibble <- s1_ts_by_plt[s1_ts_by_plt$plotID==s1_first_obs_plt$plotID[i] & s1_ts_by_plt$yearmon >= 
                                 s1_first_obs_plt$first_obs[i] & s1_ts_by_plt$yearmon <= s1_first_obs_plt$last_obs[i],]
  s1_ts_plt <- rbind(s1_ts_plt, plot_tsibble)
}

# Make it a tsibble and interpolate the NAs
s1_ts_plt <- s1_ts_plt %>% 
  mutate(yearmonth=yearmonth(as.character(yearmon)),
         VV_int=na.approx(VV)) %>% 
  as_tsibble(index=yearmonth, key=plotID)

# Create reference and validation datasets
s1_ref_plt <- s1_ts_plt %>% filter(year(yearmonth) < 2018)
s1_val_plt <- s1_ts_plt %>% filter(year(yearmonth) >= 2018)

fwrite(s1_ts_plt, paste0('Rdata/output/time_series/', country, '_s1_plt.csv'))
s1_ts_plt <- data.table(read_csv(paste0('Rdata/output/time_series/', country, '_s1_plt.csv')))


##################################### Forecast vegetation signal per PLOT
#### L8
l8_armax_plt <- l8_ref_plt %>%
  model(ARIMA(ndvi_int ~ prcp_lag1 + prcp_lag2 + pdq(1, 0, 1) + PDQ(1, 0, 0)))
l8_fc_plt <- fabletools::forecast(l8_armax_plt, new_data = l8_val_plt[,c('yearmonth', 'prcp_lag1', 'prcp_lag2', 'plotID')])

components(l8_fc_plt)
accuracy(l8_fc_plt, l8_ts_plt)

# Extract confidence levels
l8_plt_ci <- l8_fc_plt$ndvi_int %>% 
  hilo(level = 95) 
l8_fc_plt <- l8_fc_plt %>% 
  mutate(upper_95=l8_plt_ci$upper,
         lower_95=l8_plt_ci$lower)

save(l8_fc_plt, file = paste0('output/models/', country,'_l8_fc_plt.RDS'))
save(l8_armax_plt, file = paste0('output/models/', country,'_l8_armax_plt.RDS'))
load(paste0('output/models/', country, '_l8_fc_plt.RDS'))
load(paste0('output/models/', country, '_l8_armax_plt.RDS'))

# Plot
plt <- 5602
ggplot()+
  geom_line(data=l8_ref_plt[l8_ref_plt$plotID==plt,], 
            aes(x = yearmonth, y = ndvi_int - residuals(l8_armax_plt[l8_armax_plt$plotID==plt,])$.resid), 
            col='darkred',lwd=0.65, alpha=0.6) +
  geom_line(data=l8_ts_plt[l8_ts_plt$plotID==plt,], aes(x=yearmonth, y=ndvi_int), col='darkgreen',lwd=0.7)+
  geom_line(data=l8_ts_plt[l8_ts_plt$plotID==plt,], 
            aes(x=yearmonth, y=normalize(prcp, method='range',c(0.2,0.3))), col='blue')+
  geom_line(data=l8_fc_plt[l8_fc_plt$plotID==plt,], aes(x = yearmonth, y = .mean), 
            col='darkred',lwd=0.65, lty=2)+
  geom_ribbon(data=l8_fc_plt[l8_fc_plt$plotID==plt,], aes(x=yearmonth, ymin=lower_95, ymax=upper_95),
              alpha=0.2, fill='red')+
  geom_point(data = l8_ts[l8_ts$plotID==plt,], aes(x = yearmon, y = ndvi), alpha=0.5)+
  geom_vline(xintercept=plots[plots$plotID==plt,]$plant_date,lty=3,alpha=0.75)+
  labs(y='NDVI', x='Time', title=paste('Actual (green) vs predicted (red) NDVI in including 95% CI'))


#### S1
s1_armax_plt <- s1_ref_plt %>%
  model(ARIMA(VV_int ~ prcp + prcp_lag1 + prcp_lag2 + pdq(1, 0, 1) + PDQ(1, 0, 0)))
s1_fc_plt <- forecast(s1_armax_plt, new_data = s1_val_plt[,c('yearmonth', 'prcp', 'prcp_lag1', 'prcp_lag2', 'plotID')])

accuracy(s1_fc_plt, s1_ts_plt)

# Extract confidence levels
s1_plt_ci <- s1_fc_plt$VV_int %>% 
  hilo(level = 95) 
s1_fc_plt <- s1_fc_plt %>% 
  mutate(upper_95=s1_plt_ci$upper,
         lower_95=s1_plt_ci$lower)

save(s1_fc_plt, file = "output/models/Rwanda_s1_fc_plt.RDS")
load("output/models/Rwanda_s1_plt_fc.RDS")

# Plot
plt <- 15035
ggplot()+
  geom_line(data=s1_ref_plt[s1_ref_plt$plotID==plt,], 
            aes(x = yearmonth, y = VV_int - residuals(s1_armax_plt[s1_armax_plt$plotID==plt,])$.resid), 
            col='darkred',lwd=0.65, alpha=0.6) +
  geom_line(data=s1_ts_plt[s1_ts_plt$plotID==plt,], aes(x=yearmonth, y=VV_int), col='darkgreen',lwd=0.7)+
  geom_line(data=s1_ts_plt[s1_ts_plt$plotID==plt,], 
            aes(x=yearmonth, y=normalize(prcp, method='range',c(-15,-13))), col='blue')+
  geom_line(data=s1_fc_plt[s1_fc_plt$plotID==plt,], aes(x = yearmonth, y = .mean), 
            col='darkred',lwd=0.65, lty=2)+
  geom_ribbon(data=s1_fc_plt[s1_fc_plt$plotID==plt,], aes(x=yearmonth, ymin=lower_95, ymax=upper_95),
              alpha=0.1, fill='red')+
  geom_point(data = s1_ts[s1_ts$plotID==plt,], aes(x = yearmon, y = VV), alpha=0.5)+
  geom_vline(xintercept=plots[plots$plotID==plt,]$plant_date,lty=3,alpha=0.75)+
  labs(y='VV', x='Time', title=paste('Actual (green) vs predicted (red) VV in including 95% CI'))
