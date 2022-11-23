
library(data.table)
library(tidyverse)
library(dplyr)
library(sf)
library(magrittr)
library(ggplot2)
library(lwgeom) 

country <- 'Ethiopia'

#### Import data 
plots.csv <- data.table(read_csv(paste0('Rdata/Rwanda_survey/', country, '_householdsurvey_poly.csv')))

cohort.tree.tp <- data.table(read_csv(paste0('data/plot_data/', country, '_tp_species.csv')))
farmer.plot.info.tp <- data.table(read_csv(paste0('data/plot_data/', country, '_tp_profile.csv')))
plots.csv.tp <- data.table(read_csv(paste0('data/plot_data/', country, '_tp_polygon.csv')))
cohort.tree.fmnr <- data.table(read_csv(paste0('data/plot_data/', country, '_fmnr_species.csv')))
farmer.plot.info.fmnr <- data.table(read_csv(paste0('data/plot_data/', country, '_fmnr_profile.csv')))
plots.csv.fmnr <- data.table(read_csv(paste0('data/plot_data/', country, '_fmnr_polygon.csv')))

# Only select regreening projects
farmer.plot.info.tp <- farmer.plot.info.tp[farmer.plot.info.tp$survey_name=='Regreening Africa',]
farmer.plot.info.fmnr <- farmer.plot.info.fmnr[farmer.plot.info.fmnr$survey_name=='Regreening Africa',]
cohort.tree.fmnr <- cohort.tree.fmnr[cohort.tree.fmnr$survey_name=='Regreening Africa',]

cohort.tree.tp$type <- 'tp'
farmer.plot.info.tp$type <- 'tp'
plots.csv.tp$type <- 'tp'
cohort.tree.fmnr$type <- 'fmnr'
farmer.plot.info.fmnr$type <- 'fmnr'
plots.csv.fmnr$type <- 'fmnr'

# Make tp and fmrn datasets consistent
farmer.plot.info.fmnr$fmnr_start <- farmer.plot.info.fmnr$fmnr_start_date
farmer.plot.info.tp$fmnr_start <- NA

names(cohort.tree.tp)[names(cohort.tree.tp) == 'tp_latitude'] <- 'latitude'
names(cohort.tree.tp)[names(cohort.tree.tp) == 'tp_longitude'] <- 'longitude'
names(cohort.tree.tp)[names(cohort.tree.tp) == 'tp_altitude'] <- 'altitude'
names(cohort.tree.tp)[names(cohort.tree.tp) == 'tp_accuracy'] <- 'accuracy'
                    
names(cohort.tree.fmnr)[names(cohort.tree.fmnr) == c('tree_latitude', 'tree_longitude',
                                                     'tree_altitude','tree_accuracy')] <- c(
                                                       'latitude','longitude','altitude','accuracy')

# Combine the tp and fmnr datasets
cohort.tree <- rbind(cohort.tree.tp, cohort.tree.fmnr, fill=T)
farmer.plot.info <- rbind(farmer.plot.info.tp, 
                          farmer.plot.info.fmnr[,-c('id', 'land_fenced', 'species_number_start', 'fmnr_start_date')])
plots.csv <- rbind(plots.csv.tp, plots.csv.fmnr[,-c('id')])

rm(cohort.tree.tp, farmer.plot.info.tp, plots.csv.tp,
   cohort.tree.fmnr, farmer.plot.info.fmnr, plots.csv.fmnr)

#cohort.tree <- data.table(read_csv('Rdata/Plot data/Rwanda_cohort.csv'))
#farmer.plot.info <- data.table(read_csv('Rdata/Plot data/Rwanda_farmer_plot_info.csv'))
#plots.csv <- data.table(read_csv('Rdata/Plot data/Rwanda_polygons.csv'))

#### End import


# Re-order plot.info data
farmer.plot.info <- farmer.plot.info[order(farmer.plot.info$farmerID),]

#### MAKE PLOT DATA SPATIAL AND ASSIGN ID
# Make sf from plot.geom
plots.attr <- plots.csv[,-c("area_polygon")] # select all columns except for 'Plot"
geometry <- st_as_sfc(plots.csv$area_polygon, crs = 4326)
plots.spat <- st_sf(plots.attr, geometry)

# Clean plot.spat file
names(plots.spat)[names(plots.spat) == 'plotID'] <- 'Plot_id'
plots.spat <- plots.spat[plots.spat$area_msq!=0,] # Remove where area is 0

# Assign plotID to datasets
plotID <- seq(1, nrow(plots.spat))
plots.spat$plotID <- seq(1, nrow(plots.spat))

# Convert multipolygon into single polygons
plots.spat <- st_cast(plots.spat, "POLYGON")
#Calculate size of (potential) new polygons
plots.spat$area <- as.numeric(st_area(plots.spat)/10000) # in ha
plots.spat <- plots.spat[plots.spat$area>0,] # remove plots with area = 0
# Order on polygon size and then remove duplicates (smallest)
plots.spat <- plots.spat[order(plots.spat$plotID),]
plots.spat <- plots.spat[-c(which(duplicated(plots.spat$plotID, fromLast=F))),]
# Make geometries valid
plots.spat <- st_make_valid(plots.spat)

#### EXCLUDE/MERGE OVERLAPPING GEOMETRIES
# Set buffer of 0m to remove intersection issues
sf::sf_use_s2(FALSE)
plots.spat <- st_buffer(plots.spat, 0)


# Create empty df to fill
for (type in unique(plots.spat$type)){
  print(type)
  plots_type <- plots.spat[plots.spat$type==type,]
  
  for (pl in unique(plots_type$plotID)){
    
    intersect_rows <- st_intersection(plots_type[plots_type$plotID==pl,], plots_type)
    print(pl)
    # Check whether plot has not yet been excluded yet
    if (nrow(intersect_rows)!=0){
      
      intersect_rows$OverlapArea <- st_area(intersect_rows)
      other_plots <- intersect_rows[intersect_rows$plotID.1!=pl,]
      sum_overlap <- sum(other_plots$OverlapArea)
      area_plot_interest <- intersect_rows$OverlapArea[intersect_rows$plotID.1==pl]
      perc_overlap <- (sum_overlap/area_plot_interest)*100
      # Set threshold for when percentage overlap is too high (now: 60%)
      if (as.integer(perc_overlap)>as.integer(60)){
        # Find plot with largest overlap
        largest_overlap_plotID <- other_plots$plotID.1[other_plots$OverlapArea==max(other_plots$OverlapArea)]
        # Select row of largest overlapping plot
        largest_overlap_row <- plots_type[plots_type$plotID==largest_overlap_plotID,]
        # Join the plot of interest and the largest overlapping plot
        join_interest_largest <- rbind(plots_type[plots_type$plotID==pl,], largest_overlap_row)
        # Select the biggest of the two
        largest_plot_row <- join_interest_largest[join_interest_largest$area_msq==
                                                    max(join_interest_largest$area_msq),]
        # Join the geometries with the largest-overlap plot
        joined_geom <- st_union(plots_type[plots_type$plotID==pl,], largest_plot_row, is_coverage=T)
        plots_type$geometry[plots_type$plotID==largest_plot_row$plotID] <- joined_geom$geometry
        # Exclude overlapping plot from df
        smallest_plot_ID <- join_interest_largest$plotID[join_interest_largest$area_msq==
                                                           min(join_interest_largest$area_msq)]
        plots.spat <- plots.spat[plots.spat$plotID!=smallest_plot_ID,]
        
        print(paste(type, 'polygon excluded'))
      }
    }
    else{print('Plot already exlcuded')}
  }
}
st_write(obj=plots.spat, dsn=paste0('output/plot_data/', country, '/', country, '_plots_filtered.GeoJSON'), driver='GeoJSON')
plots.spat <- st_read(dsn=paste0('output/plot_data/', country, '/', country, '_plots_filtered.GeoJSON'))
plots.backup <- plots.spat



#### INDICATE INDIVIDUAL PLOTS AND GIVE ID
# for plots.spat dt
dupl.index <- which(duplicated(plots.spat$farmerID) | duplicated(plots.spat$farmerID, fromLast=TRUE)==T)
dupl.unique <- unique(plots.spat$farmerID[dupl.index])

plots.spat$MultiplePlots <- 0
for (i in dupl.unique){
  num.plots.farm <- nrow(plots.spat[plots.spat$farmerID==i,])
  index.plots.farm <- which(plots.spat$farmerID==i)
  for (pl in 1:length(index.plots.farm)){
    plots.spat$MultiplePlots[index.plots.farm[pl]] <- pl
  }
}
plots.spat$FarmerPlotID <- NA
plots.spat$FarmerPlotID <- paste0(plots.spat$farmerID, '_', plots.spat$MultiplePlots)

# for farmer.plot.info dt
dupl.index <- which(duplicated(farmer.plot.info$farmerID) | duplicated(farmer.plot.info$farmerID, fromLast=TRUE)==T)
dupl.unique <- unique(farmer.plot.info$farmerID[dupl.index])

farmer.plot.info$MultiplePlots <- 0
for (i in dupl.unique){
  num.plots.farm <- nrow(farmer.plot.info[farmer.plot.info$farmerID==i,])
  index.plots.farm <- which(farmer.plot.info$farmerID==i)
  for (pl in 1:length(index.plots.farm)){
    farmer.plot.info$MultiplePlots[index.plots.farm[pl]] <- pl
  }
}
farmer.plot.info$FarmerPlotID <- NA
farmer.plot.info$FarmerPlotID <- paste0(farmer.plot.info$farmerID, '_', farmer.plot.info$MultiplePlots)
####


#### CLEAN TYPOS IN UNITS
units <- unique(plot.info$unit)
units.m2 <- c(units[1],units[4],units[5],units[6],units[7],units[8],units[9],units[10],units[11],units[13],units[14],
              units[15],units[16],units[17],units[19],units[20],units[21])
units.ha <- c(units[3])
units.ac <- c(units[2], units[12])

plot.info$Units[plot.info$unit %in% units.m2] <- as.character('m2')
plot.info$Units[plot.info$unit %in% units.ha] <- as.character('ha')
plot.info$Units[plot.info$unit %in% units.ac] <- as.character('acres')

plot.info$landsize <- as.numeric(plot.info$landsize)
plot.info$Est.Hectares[which(plot.info$Units=='m2')] <- plot.info$landsize[which(plot.info$Units=='m2')]/10000
plot.info$Est.Hectares[which(plot.info$Units=='ha')] <- plot.info$landsize[which(plot.info$Units=='ha')]
plot.info$Est.Hectares[which(plot.info$Units=='acres')] <- plot.info$landsize[which(plot.info$Units=='acres')]/2.47105381
####


# COMBINE COHORT AND PLOT DATA (BASED ON SPATIAL OVERLAP)
# Remove the rows with NAs in the longitude, latitude or farmerID
cohort.tree <- na.omit(cohort.tree, cols=c('longitude', 'latitude','farmerID'))
# Make the cohort.tree observations spatial
cohort.spat <- st_as_sf(cohort.tree, coords = c("longitude", "latitude"), crs = 4326)

# Assign plotID to tree data based on location tree
sf::sf_use_s2(FALSE) # turn of s2 to avoid error in st_intersection

# 1 lat = ~ 111111 m at the equator --> 5m = 0.000045 (7m = 0.000063)
plots.buf <- st_buffer(plots.spat, 0.000063)

cohort.spat$plotID <- NA
for (pl in 1:nrow(plots.buf)){
  print(pl)
  trees.in.plot.index <- which(st_within(cohort.spat, plots.buf[pl,], sparse=F)==T)
  trees.in.plot <- cohort.spat[c(trees.in.plot.index),]
  
  if (nrow(trees.in.plot) >= 1) {
    for (t in 1:nrow(trees.in.plot)){
      if (trees.in.plot$farmerID[t]==plots.buf$farmerID[pl]){
        cohort.spat$plotID[trees.in.plot.index[t]] <- plots.buf$plotID[pl]
      }
    }
  }
}
####


#### ASSIGN DATE 
# TO COHORT DATA
months <- c('Jan'=1, 'Feb'=2, 'Mar'=3, 'Apr'=4,'May'=5,'Jun'=6,'Jul'=7,'Aug'=8,'Sep'=9,
            'Oct'=10,'Nov'=11,'Dec'=12)
cohort.spat$plant_date <- as.Date(ISOdate(2000,1,1))

for (i in 1:nrow(cohort.spat)){
  print(i)
  
  date_string <- cohort.spat$date_planted[i]
  split_list <- strsplit(date_string, '/')
  
  cohort.spat$plant_date[i] <- as.Date(ISOdate(as.integer(split_list[[1]][2]), 
                                               as.integer(split_list[[1]][1]), 
                                               1))
}
# TO FARMER DATA
farmer.plot.info$plant_date_fmnr <- as.Date(ISOdate(2000,1,1))
for (i in 1:nrow(farmer.plot.info)){
  print(i)
  
  date_string <- farmer.plot.info$fmnr_start[i]
  split_list <- strsplit(date_string, '/')
  
  farmer.plot.info$plant_date_fmnr[i] <- as.Date(ISOdate(as.integer(split_list[[1]][2]), 
                                               as.integer(split_list[[1]][1]), 
                                               1))
}
####


#### INDICATE THE FIRST COHORT AT EACH PLOT (for merging purposes)
cohort.info <- data.table(st_drop_geometry(cohort.spat)) # remove geometry
cohort.info$FirstCohortPlotID <- 0

# Write down plotID only for the first cohort in plot
for (i in unique(cohort.info$plotID)){
  print(i)
  
  first_plant <- which(cohort.info[cohort.info$plotID==i,]$plant_date==min(cohort.info[cohort.info$plotID==i,]$plant_date))[1]
  cohort.info[cohort.info$plotID==i,]$FirstCohortPlotID[first_plant] <- i
}
####



#### MERGE DATASETS ON A PLOT LEVEL
farmer.plot.info.geom <- merge(farmer.plot.info[,c('county_region', 'district_commune_woreda', 
                                               'own_land', 'community_land', 'government_land', 'mosque_church_land', 
                                               'school_land', 'other_land', 'crops', 'croplist', 'unit', 
                                               'landsize', 'FarmerPlotID', 'plant_date_fmnr')], 
                               plots.spat[,c('farmerID', 'Plot_id', 'area_msq', 'plotID', 'MultiplePlots', 'FarmerPlotID',
                                             'type')], 
                               by='FarmerPlotID', all.x=F, all.y=T)
farmer.plot.info.spat <- st_as_sf(farmer.plot.info.geom) # Make spatial

# Add cohort info
plots.all <- merge(farmer.plot.info.spat, cohort.info[,c('species_name', 'plant_date', 'number_planted', 
                                                         'number_survived', 'woodlot', 'internal_boundary', 
                                                         'external_boundary', 'garden', 'crop_field', 'pasture_grassland',
                                                         'fallow_bushland', 'other_sites', 'mgt_prunning', 'mgt_fencing', 
                                                         'mgt_weeding', 'mgt_watering','mgt_organic_fertilizer', 
                                                         'FirstCohortPlotID','use_firewood', 'local_name','mgt_thinning',
                                                         'mgt_pollarding_lopping','mgt_coppicing','mgt_others')], 
                   by.x='plotID', by.y = 'FirstCohortPlotID', all.x=T, all.y=F)
####


#### CLEAN COMBINED DF
sf::sf_use_s2(FALSE)
plots.all <- st_buffer(plots.all, 0)
# Convert multipolygon into single polygons
plots.all <- st_cast(plots.all, "POLYGON")
# Recalculate the area after the spatial merges
plots.all$Hectare <- as.numeric(st_area(plots.all)/10000) # in ha
plots.all <- plots.all[plots.all$Hectare>0,] # remove plots with area = 0
# Order on polygon size and then remove duplicates (smallest)
plots.all <- plots.all[order(plots.all$Hectare),]
plots.all <- plots.all[-c(which(duplicated(plots.all$plotID, fromLast=F))),]
# Exclude two very large polygons (plodID: "9977", "19871")
#plots.all <- plots.all[-c(which(plots.all$plotID==9977 | plots.all$plotID==19871)),]
####
View(plots.all)
plots.all


#### CHANGE THE GEOMETRIES OF THE PLOT POLYGONS
# Centroid
plot.centroid <- st_centroid(plots.all, of_largest_polygon=F)

# Buffer (7m)
#plots.buf <- st_buffer(plots.all, 0.000063)

# Multipolygon
centroid_multi <- plot.centroid %>% 
  st_combine()
####



#### WRITE THE FILES AS GEOJSON AND CSV
st_write(obj=plots.all, dsn=paste0('output/plot_data/', country, '/', country, '_plots_all.GeoJSON'), driver='GeoJSON')
st_write(obj=plots.all, dsn=paste0('output/plot_data/', country, '/', country, '_plots_all.csv'), driver='CSV')
st_write(obj=plot.centroid, dsn=paste0('output/plot_data/', country, '/', country, '_plot_centroid.GeoJSON'), driver='GeoJSON')
st_write(obj=plot.centroid, dsn=paste0('output/plot_data/', country, '/', country, '_plot_centroid.csv'), driver='CSV')
#st_write(obj=plot.centroid, dsn='output/plot_data/Niger_plot_centroid.shp', driver='ESRI Shapefile')
st_write(obj=centroid_multi, dsn=paste0('output/plot_data/', country, '/', country, '_centroid_multi.GeoJSON'), driver='GeoJSON')
st_write(obj=centroid_multi, dsn=paste0('output/plot_data/', country, '/', country, '_centroid_multi.csv'), driver='CSV')
#st_write(obj=plots.buf, dsn='Rdata/Shapefiles/Rwanda_plots_buf.GeoJSON', driver='GeoJSON')
#st_write(obj=plots.buf, dsn='Rdata/Shapefiles/Rwanda_plots_buf.csv', driver='CSV')

# Read the polygon data
plots.all <- st_read(paste0('output/plot_data/', country, '/', country, '_plots_all.GeoJSON'))


# Perform some analyses
# Number of trees per plot
nr.trees.plot <- trees$Number.planted[duplicated(trees$plotID)==F]
nr.trees.plot <- data.table(cbind(NrTrees = nr.trees.plot, plotID = unique(trees$plotID)))
plots.all <- merge(plots.all, nr.trees.plot, by='plotID', all.x=T, all.y=F)

# Determine amount of species per plot
trees$NrSpecies <-  NA
for (i in plotID){
  print(i)
  unique.specie.plot <- unique(data.table(na.omit(trees$Species.name[trees$plotID==i])))
  trees$NrSpecies[trees$plotID==i] <- nrow(unique.specie.plot)
}
trees.unique.plot <- trees[duplicated(trees$plotID)==F,]
plots.all <- merge(plots.all, trees.unique.plot[,c('plotID', 'NrSpecies')], by='plotID', all.x=T, all.y=F)
View(plots.all)

# How often which species
number.species.total <- data.table(Species = unique(trees$Species.name))
number.species.total$NumberTrees <- NA
for (i in number.species.total$Species){
  print(i)
  
  species.total.rows <- trees[trees$Species.name==i,]
  unique.specie.cohort <- species.total.rows$Number.planted[duplicated(species.total.rows$Cohort_id)==F]
  number.species.total$NumberTrees[number.species.total$Species==i] <- sum(unique.specie.cohort)
}


# Visualisation
# Estimated vs Actual plot size
ggplot(plots.all, aes(Est.Hectares, Hectare))+
  geom_point()+
  geom_smooth(method = 'lm')+
  xlim(0,2.5)+
  ylim(0,2.5)+
  geom_abline(intercept=0, slope=1, lwd=1)+
  labs(title='Estimated plot size vs actual plot size', x='Estimated (ha)', y='Actual (ha)')

ggplot(plots.all, aes(plotID, NrTrees))+
  geom_point()+
  scale_y_continuous(trans = 'log2')+
  labs(title='Number of trees planted per plot')

ggplot(plots.all, aes(NrSpecies))+
  scale_x_continuous(breaks=1:10)+
  geom_histogram(boundary = -0.3)+
  labs(title="Number of spcies per plot")




