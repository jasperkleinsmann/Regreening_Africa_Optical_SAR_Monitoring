
#### Import
country <- 'Rwanda'
# Plot data
plots <- st_read(dsn=paste0('output/plot_data/', country, '/', country, '_plots_all.GeoJSON'))
plots_pnt <- st_read(dsn=paste0('output/plot_data/', country, '/', country, '_plot_centroid.GeoJSON'))
gpm_month <- data.table(read_csv(paste0('output/time_series/gpm/', country, '_gpm_monthly.csv')))
refl_l8 <- data.table(read_csv(paste0('output/time_series/', country, '_aggr_vi_l8.csv')))
refl_s1 <- data.table(read_csv(paste0('output/time_series/', country, '_aggr_vi_s1.csv')))

# Read spatial data
admin <- st_read(paste0('data/admin_boundaries/', country,'/gha_admbnda_adm2_gss_20210308.shp'))

# Clean plot data
names(plots)[names(plots) == 'district_commune_woreda'] <- 'county'

## GENERAL
admin <- st_transform(admin, crs=st_crs(plots))

sf::sf_use_s2(FALSE) 
county_index <- st_within(plots_pnt, admin, sparse=T)
null_counties <- which(sapply(county_index,length)==0)
county_index[null_counties] <- NA
county_index <- unlist(county_index)
plots$county <- admin$ADM2_EN[unlist(county_index)]

plots <- plots[!is.na(plots$county),]

st_write(plots, dsn=paste0('output/plot_data/', country, '/', country, '_plots_final.GeoJSON'))
plots <- st_read(dsn=paste0('output/plot_data/', country, '/', country, '_plots_final.GeoJSON'))

# Add information gpm
gpm_month <- merge(gpm_month, plots[,c('plotID', 'county')], 
                   by='plotID')
gpm_month <- gpm_month[,-c('geometry')]

# clean and merge refl datasets
l8_ts <- merge(refl_l8, gpm_month[,c('plotID', 'yearmon','prcp_month')],
               by=c('plotID', 'yearmon'), all.y=T)
l8_ts <- merge(l8_ts, plots[,c('plotID', 'county', 'county_region', 'plant_date')], 
               by=c('plotID'),all.y=F)
l8_ts <- l8_ts[,-c('geometry')]
l8_ts <- l8_ts[order(l8_ts$yearmon),]

s1_ts <- merge(refl_s1, gpm_month[,c('plotID', 'yearmon','prcp_month')],
               by=c('plotID', 'yearmon'), all.y=T)
s1_ts <- merge(s1_ts, plots[,c('plotID', 'county')], 
               by=c('plotID'),all.y=F)
s1_ts <- s1_ts[,-c('geometry')]
s1_ts <- s1_ts[order(s1_ts$yearmon),]

# Write cleaned L8 and S1 ts
fwrite(l8_ts, paste0('output/time_series/', country, '_l8_ts.csv'))
fwrite(s1_ts, paste0('output/time_series/', country, '_s1_ts.csv'))

rm(refl_l8, refl_s1)



