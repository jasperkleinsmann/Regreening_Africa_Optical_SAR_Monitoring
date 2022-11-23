
library(terra)
library(sf)
library(lubridate)
library(data.table)

country <- 'Rwanda'
gpm <- rast(paste0('output/gee/gpm/', country, '_GPM_stack.tif'))
plots <- st_read(dsn=paste0('output/plot_data/', country, '/', country, '_plot_centroid.GeoJSON'))
plots <- vect(plots)

#### Create dates
start_ts <- as.Date('2013-01-01')
end_ts <- as.Date('2022-08-30')

months_dif <- (year(end_ts) - year(start_ts)) * 12 + (month(end_ts) - month(start_ts))
months_ts <- seq(ymd(start_ts), by = "month", length.out=months_dif)
# Assign dates to gpm_stack layers
names(gpm) <- months_ts


#### Extract values for each layer
gpm_plots <- data.table(extract(gpm, plots, cells=F))
gpm_plots <- gpm_plots[,-c('ID')] # rempve one column
rownames(gpm_plots) <- plots$plotID

# Transpose dt
gpm_ts <- cbind(row = rownames(gpm_plots), stack(gpm_plots))

# Clean dt
names(gpm_ts) <- c('plotID', 'prcp', 'date')
gpm_ts$yearmon <- as.Date(ISOdate(year(gpm_ts$date), month(gpm_ts$date), 15))

#### Write as csv
fwrite(gpm_ts, "output/time_series/gpm/Ethiopia_gpm_monthly.csv")
