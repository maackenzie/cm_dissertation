#setwd("D:/uni onedrive/OneDrive - University of Plymouth/CM_copy_rf_files")
setwd("C:/Users/cmackenzie8/OneDrive - University of Plymouth/CM_copy_rf_files")


#cells_latlong_file <- read.csv("serge_cells_latlong.csv")
cells_latlong_file <- read.csv("reliable_only_serge_lakeorbog_latlong_clipped.csv")


# read csv with treecover, roc, and treecover change (in wide format)
treecover_and_roc <- read.csv("treecover_and_roc.csv", check.names = F)
#rename gridcell col to lcgrid_id
names(treecover_and_roc)[names(treecover_and_roc) == "GRIDCELL"] <- "LCGRID_ID" 

#give lat long values to treecover_and_roc file
roc_df2 <- merge(treecover_and_roc, cells_latlong_file[, c("LCGRID_ID", "lat", "long")], by = "LCGRID_ID", all.x = T)


#make data long for boxplots
roc_long2 <- pivot_longer(roc_df2,
    cols = -c(LCGRID_ID, lat, long),
    names_to = c(".value", "Year"),
    names_sep = "_")

roc_long2$Year <- as.numeric(roc_long2$Year) # make year col numeric



##################### make 3 different types of lat groupings ###########################################

### Equal count groups ###

#find equal count groups in lat using ceiling to find and rank to order them and divided by 5 for 5 groups
#roc_long2$lat_EC <- ceiling(rank(roc_long2$lat) / nrow(roc_long2) * 5)

#find out the largest and smallest values for each lat group
#aggregate(lat ~ lat_EC, data = roc_long2, FUN = function(x) c(min = min(x), max = max(x)))

#assign labels based on EC lat groups
#roc_long2$lat_EC <- factor(roc_long2$lat_EC, levels = 1:5, labels = c("37.5°-42.5°", "43.5°-46.5°", "47.5°-51.5°", "52.5°-53.5°", "54.5°-60.5°"))


roc_long2$lat_EC[roc_long2$lat <= 42.5] <- "37.5°-42.5°"
roc_long2$lat_EC[roc_long2$lat >= 43.5 & roc_long2$lat <= 46.5] <- "43.5°-46.5°"
roc_long2$lat_EC[roc_long2$lat >= 47.5 & roc_long2$lat <= 51.5] <- "47.5°-51.5°"
roc_long2$lat_EC[roc_long2$lat >= 52.5 & roc_long2$lat <= 53.5] <- "52.5°-53.5°" 
roc_long2$lat_EC[roc_long2$lat >= 54.5] <- "54.5°-60.5°"



#lat by Gisecke 2019 region
roc_long2$lat_region[roc_long2$lat <= 45] <- "Mediterranean"
roc_long2$lat_region[roc_long2$lat >= 45 & roc_long2$long <= 11] <- "Western temperate"
roc_long2$lat_region[roc_long2$lat >= 45 & roc_long2$long >= 11] <- "Eastern temperate"
roc_long2$lat_region[roc_long2$lat >= 45 & roc_long2$lat <= 47 & roc_long2$long <= 15& roc_df$long >= 5] <- NA # alps
roc_long2$lat_region[roc_long2$lat >= 57] <- "Boreal"


roc_long2$lat_region <- factor(
  roc_long2$lat_region,
  levels = c("Mediterranean","Western temperate","Eastern temperate","Boreal")
)

#pretty breaks
roc_long2$lat_PB <- cut(roc_long2$lat,
                      breaks = c(0, 40, 45, 50, 55, 60.6),
                      labels = c("<40°", "40°-45°", "45°-50°", "50°-55°", "55°-60.5°"))


### boxplots for each lat group breakdown for roc, tc, and tc change 


### boxplots for change at year -800,-400,0,400,800 for roc and lat_Ec

subset_data <- roc_long2[ which(roc_long2$Year == -800),]
boxplot(data = subset_data, ROC ~ lat_EC, main = "-800", ylab = "ROC", col = "darkkhaki", ylim = c(0,0.3))

subset_data <- roc_long2[ which(roc_long2$Year == -400),]
boxplot(data = subset_data, ROC ~ lat_EC, main = "-400", ylab = "ROC", col = "darkkhaki", ylim = c(0, 0.3))

subset_data <- roc_long2[ which(roc_long2$Year == 0),]
boxplot(data = subset_data, ROC ~ lat_EC, main = "0", ylab = "ROC", col = "darkkhaki", ylim = c(0, 0.3))

subset_data <- roc_long2[ which(roc_long2$Year == 400),]
boxplot(data = subset_data, ROC ~ lat_EC, main = "400", ylab = "ROC", col = "darkkhaki", ylim = c(0, 0.3))

subset_data <- roc_long2[ which(roc_long2$Year == 800),]
boxplot(data = subset_data, ROC ~ lat_EC, main = "800", ylab = "ROC", col = "darkkhaki", ylim = c(0, 0.3))





boxplot(data = subset_data, ROC ~ lat_region, main = "ROC", ylab = "ROC", col = "darkkhaki", ylim = c(0,0.3))

boxplot(data = subset_data, ROC ~ lat_PB, main = "ROC", ylab = "ROC", col = "darkkhaki", ylim = c(0,0.3))
boxplot(data = subset_data, ROC ~ lat_EC, main = "ROC", ylab = "ROC", col = "darkkhaki", ylim = c(0,0.3))



boxplot(data = subset_data, TC ~ lat_region, main = "TC%",ylab = "ROC", col = "darkgreen")
boxplot(data = subset_data, TCchange ~ lat_region, main = "TC % change", ylab = "ROC", col = "purple", ylim = c(-40,40))



par(mfrow = c(3,1), mar = c(2,2,2,2))

boxplot(data = roc_long2, ROC ~ lat_region, main = "ROC", ylab = "ROC", col = "darkkhaki", ylim = c(0,0.4))
boxplot(data = roc_long2, TC ~ lat_region, main = "TC%",ylab = "ROC", col = "darkgreen")
boxplot(data = roc_long2, TCchange ~ lat_region, main = "TC % change", ylab = "ROC", col = "purple", ylim = c(-20,20))


boxplot(data = roc_long2, ROC ~ lat_PB,  xlab = "Year", ylab = "ROC",  col = "darkkhaki", ylim = c(0,0.4))
boxplot(data = roc_long2, TC ~ lat_PB, xlab = "Year", ylab = "ROC",  col = "darkgreen")
boxplot(data = roc_long2, TCchange ~ lat_PB, xlab = "Year", ylab = "ROC", col = "purple", ylim = c(-20,20))


boxplot(data = roc_long2, ROC ~ lat_EC, xlab = "Year", ylab = "ROC", col = "darkkhaki", ylim = c(0,0.4))
boxplot(data = roc_long2, TC ~ lat_EC, xlab = "Year", ylab = "ROC",  col = "darkgreen")
boxplot(data = roc_long2, TCchange ~ lat_EC, xlab = "Year", ylab = "ROC",  col = "purple", ylim = c(-20,20))




roc_long2_file <- write.csv(roc_long2, "roc_long2.csv", row.names = F)


lat_splitting_list  <- c("lat_EC", "lat_PB","lat_region")
lat_splitting_names <- c("Equal count", "Pretty breaks",  "Regions")


col_list1 <- c("#fde725", "#5ec962",  "#21918c", "#3b528b", "#7e03a8")
col_list2 <- c("darkorange", "indianred", "darkmagenta", "darkslateblue", "darkblue")

for (j in seq_along(lat_splitting_list)) {
  
  #retrive the lat grouping needed from indexing by list of lat groups
  lat_split_col  <- lat_splitting_list[j]
  lat_split_name <- lat_splitting_names[j]    
  
  
  par(mfrow=c(2,3), mar=c(2,4,1,2), oma=c(2,1,5,0))
  
  ################# MEDIANS #############
  #create tmp of roc_long2 with lat_group being the name of the type I'm using = lat_regions, EC, PB
  tmp <- roc_long2
  tmp$lat_group <- tmp[[lat_split_col]]
  
  
  #find out median VALS of roc, tc, and tcchange and keep year and lat groups cols USING TMP file
  roc_long_medians <- aggregate(
    cbind(ROC, TC, TCchange) ~ Year + lat_group,
    data = tmp,
    FUN = median, na.rm = TRUE
  )
  
  #have to rename cols
  names(roc_long_medians)[2] <- "lat_group"
  names(roc_long_medians)[3:5] <- c("ROC_median","TC_median","TCchange_median")
  
  lat_group <- levels(factor(roc_long_medians$lat_group))
  
  #cols <- 1:length(lat_groups)
  
  
  if (lat_split_col == "lat_EC") {
    cols <- col_list1
  } else if (lat_split_col == "lat_PB"){
    cols <- col_list2
  } else  {
    cols <- 1:length(lat_group)+3
  }
  
  
  
  
  # n_groups <- length(lat_group)
  # cols <- colour_list[seq_len(n_groups) + (i - 1) * n_groups]
  
  ### ROC ###
  plot(c(-800,1000), c(0,0.2), ylab = "Median ROC", type="n", xaxs="i", cex.axis = 1.2, cex.lab = 1.5, font.axis = 1, font.lab = 1)
  for(i in seq_along(lat_group)){
    df_sub <- roc_long_medians[roc_long_medians$lat_group==lat_group[i],]
    lines(df_sub$Year, df_sub$ROC_median, col=cols[i], lwd = 3)
  }
  legend("topleft", legend=lat_group, col=cols, lty=1, lwd=2, bty="n", cex = 1.1)
  
  ### TC median ###
  plot(c(-800,1000), c(0,100), ylab = "Median TC%", type="n", xaxs="i", cex.axis = 1.2, cex.lab = 1.5, font.axis = 1, font.lab = 1)
  for(i in seq_along(lat_group)){
    df_sub <- roc_long_medians[roc_long_medians$lat_group==lat_group[i],]
    lines(df_sub$Year, df_sub$TC_median, col=cols[i], lwd = 3)
  }
  
  ### TCchange median ###
  plot(c(-800,1000), c(-10,10), ylab = "Median TCchange", type="n", xaxs="i", cex.axis = 1.2, cex.lab = 1.5, font.axis = 1, font.lab = 1)
  for(i in seq_along(lat_group)){
    df_sub <- roc_long_medians[roc_long_medians$lat_group==lat_group[i],]
    lines(df_sub$Year, df_sub$TCchange_median, col=cols[i], lwd = 3)
  }
  
  ####################################
  # MEANS
  ####################################
  
  
  roc_long_means <- aggregate(
    cbind(ROC, TC, TCchange) ~ Year + roc_long2[[lat_split_col]],
    data = tmp, FUN= mean, na.rm = T
  )
  
  names(roc_long_means)[2] <- "lat_group"
  names(roc_long_means)[3:5] <- c("ROC_mean","TC_mean","TCchange_mean")
  
  ### ROC mean ###
  plot(c(-800,1000), c(0,0.22), ylab = "Mean ROC", type="n", xaxs="i", cex.axis = 1.2, cex.lab = 1.5, font.axis = 1, font.lab = 1)
  for(i in seq_along(lat_group)){
    df_sub <- roc_long_means[roc_long_means$lat_group==lat_group[i],]
    lines(df_sub$Year, df_sub$ROC_mean, col=cols[i], lwd = 3)
  }
  
  ### TC mean ###
  plot(c(-800,1000), c(0,100), ylab= "Mean TC%", type="n", xaxs="i", cex.axis = 1.2, cex.lab = 1.5, font.axis = 1, font.lab = 1)
  for(i in seq_along(lat_group)){
    df_sub <- roc_long_means[roc_long_means$lat_group==lat_group[i],]
    lines(df_sub$Year, df_sub$TC_mean, col=cols[i], lwd = 3)
  }
  
  ### TCchange mean ###
  plot(c(-800,1000), c(-18,12), ylab= "Mean TCchange", type="n", xaxs="i", cex.axis = 1.2, cex.lab = 1.5, font.axis = 1, font.lab = 1)
  for(i in seq_along(lat_group)){
    df_sub <- roc_long_means[roc_long_means$lat_group==lat_group[i],]
    lines(df_sub$Year, df_sub$TCchange_mean, col=cols[i], lwd = 3)
  }
  
  mtext(lat_split_name, side=3, outer=TRUE, line=2, font = 2)
}
#
#
#bar charts of every year
par(mfrow = c(2,1))
#group by 400 year tw?
#tp get year, groups, and roc values




#for gridcell in file, how many of each lat group are there


# #something along lines of closely knit groups with both similar lat and similar neo date - how to do this??




# for latitude groupings for tc change
for (j in seq_along(lat_splitting_list)) {
  
  #what is the colum called for that latitude grouping
  lat_split_col  <- lat_splitting_list[j]
  
  #what is the name that I am calling it (lat_PB vs pretty breaks)
  lat_split_name <- lat_splitting_names[j]
  
  #create a tmp df for data with gridcell, roc, tc, tcchange
  tmp <- roc_long2
  
  #create a new col in this new df called lat group and put the latitude grouping col in there. using factor will ensure there are catergories for the lat groups rather than continuous data
  tmp$lat_group <- factor(tmp[[lat_split_col]])
  
  #use the levels function to retrive the different "catergories"/factors that are in the tmp$lat_group col (e.g., 40-45, 50-55 etc)
  lat_groups <- levels(tmp$lat_group)
  
  #sorting out colours based on what col is being looped
  if (lat_split_col == "lat_EC") {
    cols <- col_list1
  } else if (lat_split_col == "lat_PB"){
    cols <- col_list2
  } else {
    cols <- 1:length(lat_groups) +3

  }
  
  #split plots into 1:4 or 1:5 based on how many levels there are. set inner and outer margins
  par(mfrow=c(1,length(lat_groups)), mar=c(4,3,2,1), oma=c(4,4,3,1))
  
  #for each individual point in the lat group col
  for (i in seq_along(lat_groups)) {
    
    #create a subset of each individual point
    df_sub <- tmp[tmp$lat_group == lat_groups[i], ]
    
    # #make a boxplot using df_sub data by plotting ROC by a factor of a year (so each year is a new box)
    # boxplot(ROC ~ factor(Year),
    #         data=df_sub,
    #         col=cols[i],
    #         main=lat_groups[i], ylab="",xlab="", outline=FALSE, ylim = c(0,0.4), cex.axis = 1.2, cex.main = 1.6, cex = 1.4)
    # abline(v = 6, col = "darkred", lwd = 2)
    
    boxplot(TCchange ~ factor(Year),
            data=df_sub,
            col=cols[i],
            main=lat_groups[i],
            ylab="",
            xlab="",
            outline=FALSE, ylim = c(-35,35), cex.axis = 1.2, cex.main = 1.6, cex = 1.4)
    
    
  }
  
  mtext("Year", side=1, outer=TRUE, line=2)
  mtext("TC change", side=2, outer=TRUE, line=2) # or tcchange
  mtext(lat_split_name, side=3, outer=TRUE, line=1, font=2)
  
}










# for latitude grouping for roc
for (j in seq_along(lat_splitting_list)) {
  
  #what is the colum called for that latitude grouping
  lat_split_col  <- lat_splitting_list[j]
  
  #what is the name that I am calling it (lat_PB vs pretty breaks)
  lat_split_name <- lat_splitting_names[j]
  
  #create a tmp df for data with gridcell, roc, tc, tcchange
  tmp <- roc_long2
  
  #create a new col in this new df called lat group and put the latitude grouping col in there. using factor will ensure there are catergories for the lat groups rather than continuous data
  tmp$lat_group <- factor(tmp[[lat_split_col]])
  
  #use the levels function to retrive the different "catergories"/factors that are in the tmp$lat_group col (e.g., 40-45, 50-55 etc)
  lat_groups <- levels(tmp$lat_group)
  
  #sorting out colours based on what col is being looped
  if (lat_split_col == "lat_EC") {
    cols <- col_list1
  } else if (lat_split_col == "lat_PB"){
  ols <- col_list2
  } else {
    cols <- 1:length(lat_groups) +3
    
  }
  
  #split plots into 1:4 or 1:5 based on how many levels there are. set inner and outer margins
  par(mfrow=c(1,length(lat_groups)), mar=c(4,3,2,1), oma=c(4,4,3,1))
  
  #for each individual point in the lat group col
  for (i in seq_along(lat_groups)) {
    
    #create a subset of each individual point
    df_sub <- tmp[tmp$lat_group == lat_groups[i], ]
    
    #make a boxplot using df_sub data by plotting ROC by a factor of a year (so each year is a new box)
    boxplot(ROC ~ factor(Year),
            data=df_sub,
            col=cols[i],
            main=lat_groups[i], ylab="",xlab="", outline=FALSE, ylim = c(0,0.4), cex.axis = 1.2, cex.main = 1.6, cex = 1.4)
    abline(v = 6, col = "darkred", lwd = 2)


    
  }
  
  mtext("Year", side=1, outer=TRUE, line=2)
  mtext("ROC", side=2, outer=TRUE, line=2) # or tcchange
  mtext(lat_split_name, side=3, outer=TRUE, line=1, font=2)
  
}
















################
# roc tree cover scatter coloured by lattitude



par(mfrow=c(2,5), mar=c(2,2,2,2), oma = c(4, 4, 4, 2) )

# sort so goes -1000 to 800 and also is a func for for loop
years <- sort(as.numeric(unique(roc_long2$Year))) 



#plot multi-scatter plots of treecover % vs ROC per year
for (i in years[2:11]) { # 2:11 so it does -800 to 1000
  df_sub <- subset(roc_long2, Year == i)
  plot(df_sub$TC, df_sub$ROC, ylab = "", xlab = "", 
       main = paste(i), col = "forestgreen", pch = 19, ylim = c(0, 0.5), cex.axis = 1.2, cex.main = 1.6)
}


#write axes
mtext("ROC", side=2, outer=TRUE, line=2, cex = 1.4)
mtext("% Treecover", side=1, outer=TRUE, line=2, cex = 1.4)
mtext("Treecover % vs ROC per timewindow", side=3, outer=TRUE, line=2, cex = 1.4)

