setwd("D:/uni onedrive/OneDrive - University of Plymouth/CM_copy_rf_files/unrestricted_SERGE_datasets")

meta_data <- read.csv("serge_metadata.csv")
lcgrid_df <- data.frame(LCGRID_ID = character(), small_lake = integer(), large_lake = integer(), small_bog = integer(), large_bog = integer())


file.list <- list.files("D:/uni onedrive/OneDrive - University of Plymouth/CM_copy_rf_files/all_RV/results.GC.jan26/RV") # list of all reveals files
cells_from_files <- as.character(gsub("\\D", "", file.list)) 



for (lcgrid in unique(as.character(meta_data$LCGRID_ID))) {
  if (lcgrid %in% cells_from_files){
    
    site_types <- meta_data[meta_data$LCGRID_ID == lcgrid, "sitetype"] # read the site type of the gridcell we want
    site_radius <- meta_data[meta_data$LCGRID_ID == lcgrid, "radius"]  # read the site radius of the gridcell we want
    
    #lake_count <- sum(site_types == "lake")
    #bog_count  <- sum(site_types == "bog")
    
    #calc size of lake/bog
    small_lake <- sum(site_types == "lake" & site_radius < 50) 
    large_lake <- sum(site_types == "lake" & site_radius >= 50)
    
    small_bog <- sum(site_types == "bog" & site_radius < 50)
    large_bog <- sum(site_types == "bog" & site_radius >= 50)
    
    #bind all cols together and the large lake etc ones as a df because it will be rows other wise
    lcgrid_df <- rbind( lcgrid_df, data.frame(LCGRID_ID = lcgrid, small_lake = small_lake, large_lake = large_lake, small_bog = small_bog, large_bog = large_bog)
    )
  }
}
  
#how many sites have 5+ small lakes and not any large lakes


#transpose... for some reason
dat <- as.data.frame(t(lcgrid_df[,-1]))




setwd("D:/uni onedrive/OneDrive - University of Plymouth/CM_copy_rf_files")

landclim <- read.csv("landclim_latlong.csv")
landclim <- landclim[, -c(3:36)] # retrive just lcgrid, lat, long, objectid
landclim <- landclim[, -1] # remove object_id col as it is not needed


#merge the lat long cells file with the bog lake size file
cells_latlong <- merge(lcgrid_df, landclim, by = "LCGRID_ID", all.x = TRUE)

cells_latlong$unreliable_sum <- rowSums(cells_latlong[,c(2,4)])

#keep rows if there is more than 0 arge larkes and there are more than one unreliable cells
cells_latlong2 <- subset(cells_latlong, large_lake > 0 |unreliable_sum > 1)

#cells_latlong2 <- subset(cells_latlong, )

View(cells_latlong2)

write.csv(lcgrid_df, "serge_lakeorbog.csv", row.names = F)

write.csv(cells_latlong2, "reliable_only_serge_lakeorbog_latlong.csv", row.names = F)








#reading in clipped serge reliable cells only file
reliable_clipped <- read.csv("reliable_only_serge_lakeorbog_latlong_clipped.csv")
