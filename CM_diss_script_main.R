setwd("D:/uni onedrive/OneDrive - University of Plymouth/CM_copy_rf_files")

library(rioja)
library(vegan)


########### Neolithic start date formatting #########

# reads the interpolated neolithic dates file
StartNeeee <- read.csv("from_qgis_idw_neostart2.csv")

#retrive ldgid_id and startneo only
StartNeo3 <- data.frame(LCGRID_ID = StartNeeee$LCGRID_ID, startNeo = StartNeeee$DN_median)

#creates odd sequence of neo dates
tws <- seq(3100,11700,200)

# round neolithic start dates to every odd 200
StartNeo3$startNeo <- tws[findInterval(StartNeo3$startNeo, tws) +  round((StartNeo3$startNeo - tws[findInterval(StartNeo3$startNeo, tws)]) / 200)] 

#rename the neolithic df with odd dates to startneo for consistency
StartNeo <- StartNeo3




######### reading reveals data ############
input_dir <- "all_RV/results.GC.jan26.copy/fixed3"
file.list <- list.files(input_dir)

# this is a list of GRIDCELLS with data (222 files) # now has 279 after putting in UK data. still need to put in spain/portugal etc
cells_from_files <- as.character(gsub("\\D", "", file.list)) 


######### removing unreliable cells ###########

reliable_cell_df <- read.csv("qgis_clipped_reliable_cells2.csv", check.names = F)
reliable_cells <- as.character(reliable_cell_df$LCGRID_ID)

# find what cells have reveals data and what count as reliable cells of which are large lakes or more than 1 small lakes/bogs
keep <- cells_from_files %in% reliable_cells

# new file list is the file list subsetted to only include the cells which count as reliable 
file.list <- file.list[keep]

#cells from files is now only the cells which are reliable
cells_from_files <- cells_from_files[keep]



########## getting lstlong of cells ############

#retrive list of names of gridcells. 
LCGRID_ID <- as.character(gsub("\\D", "", file.list)) #just easier to call df this rather than cells_from_files version and hten changing the col header

#get the list of gridcells to plot in qgis
df_cells <- as.data.frame(LCGRID_ID)

landclim <- read.csv("landclim_latlong.csv")
landclim <- landclim[, -c(3:36)] # retrive just lcgrid, lat, long, objectid by removing other cols

#get gridcells with neo dates
cells_latlong <- merge(df_cells, landclim, by = "LCGRID_ID", all.x = TRUE)
cells_latlong_file <- write.csv(cells_latlong, "serge_cells_latlong.csv", row.names = F) 




############# preparing for loop ###############

noneo_list <- c()
GRIDCELL_list <- c()

#creates a df to put in gridcell and neo start date later
neo_start_for_qgis_df <- data.frame(LCGRID_ID = character(), NeoTW = numeric()) 

#creates a df to put in gridcell and taxa data sum (binary if there is data) later
new_taxa_sums_df <- data.frame(GRIDCELL = character(), taxa_sum = numeric()) 
roc_df <- data.frame()
treecover_df <- data.frame()
ET_df <- data.frame()
ST_df <- data.frame()



count <- 0
count2 <- 0

#to find out why cells are being lost
dropped_no_tw  <- character()
dropped_no_roc <- character()


########### BIG LOOP WITH REVEALS DATA ##########

#for unique gridcell in the list of gridcells that have a neolithic start date
for (GRIDCELL in as.character(unique(StartNeo$LCGRID_ID))){
  
  # count of each unique gridcell that has a neo date
  count <- count + 1 
  
  #if there is a file of that gridcell
  if (GRIDCELL %in% as.character(cells_from_files)){ 

    # count for each gridcell with a neo data that has pollen data
    count2 <- count2 + 1 
  
    #selects file for the gridcell we want
    i <- file.list[cells_from_files == GRIDCELL][1]
    
    #read in the pollen file for the grid cell
    dat <- read.csv(paste0(input_dir,"/",i))
    
    
    #do some formatting to re-shape the original data
    #get taxon names
    taxa <- dat[,1] 
    
    #drop the first column, then transpose because otherwise r has a hissy fit
    dat <- as.data.frame(t(dat[,-1])) 
  
    
    #rename the columns
    colnames(dat) <- taxa             
    taxa_names <- colnames(dat)     
    
    #get original TW as a column rather than X3100 etc
    dat$TW_BP <- as.numeric(gsub("X", "", row.names(dat)))
    
    #extract start time window for the gridcell indexed using gridcell
    # means going to the gridcell then extracting the data that is in the second col which is neo start
    NeoTW <- StartNeo[StartNeo$LCGRID_ID == GRIDCELL, 2] 
    


    #if statement to catch gridcells where there isn't a NEO start date
    #if there IS a NEO start date the variable will have length of 1 if not, will be 0
    if(length(NeoTW) == 1){
      
      #create a new variable called TW in the dataset
      dat$TW_NEO <- -(seq(3100,11700,200) - NeoTW)
      
      
      #filter to only include 11 key grid cell (5 before, start NEO, and 5 after plus 0)
      #subset data to only keep data between 1000 and -1000
      dat <- subset(dat, TW_NEO < 1100 & TW_NEO > -1100)
      
      #if the grid cell has no data around when the neolithic started then the following does not run
      if (nrow(dat) == 0) {
        dropped_no_tw <- c(dropped_no_tw, GRIDCELL)
        next
      }

      #creates the labels of tws for later col naming
      tw_labels <- dat$TW_NEO


      #calculate some useful pollen sums
      TBE1 <- dat$Picea
      TBE2 <- dat$'Abies alba'
      IBE  <- dat$Pinus
      MTBE <- dat$Phillyrea + dat$Pistacia + dat$'Quercus evergreen'
      TSE  <- dat$Juniperus
      MTSE <- dat$Ericaceae + dat$'Buxus sempervirens'
      IBS  <- dat$'Alnus glutinosa' + dat$Betula
      ISTS <- dat$'Corylus avellana' + dat$Fraxinus + dat$'Quercus deciduous'
      TBS  <- dat$'Carpinus betulus' + dat$'Carpinus orientalis' +
        dat$Castanea + dat$'Fagus sylvatica' + dat$Tilia + dat$Ulmus
      TSD  <- dat$Salix
      LSE  <- dat$'Calluna vulgaris'
      GL   <- dat$Artemisia + dat$'Amanranthaceae/Chenopodiaceae' + dat$Cyperaceae + dat$Filipendula + dat$Poaceae + dat$'Plantago lanceolata type' + dat$'Rumex acetosa-t'
      AL   <- dat$'Cerealia-t' + dat$Secale
      ET   <- TBE1 + TBE2 + IBE + MTBE + TSE + MTSE
      ST   <- IBS + ISTS + TBS + TSD # broadleaf
      OL   <- LSE + GL + AL

      treecover_ratio <- ((ET + ST)/ (ET + ST + OL)) * 100

      #create a df for treecover % csv's
      treecover_tmp <- data.frame(
        GRIDCELL   = GRIDCELL,
        TW_NEO     = dat$TW_NEO,
        treecover = ((ET + ST) / (ET + ST + OL)) * 100
      )
      ET_tmp <- data.frame(
        GRIDCELL = GRIDCELL,
        TW_NEO = dat$TW_NEO,
        ET_percent = (ET/(ET + ST + OL)) * 100
      )
      ST_tmp <- data.frame(
        GRIDCELL = GRIDCELL,
        TW_NEO = dat$TW_NEO,
        ST_percent = (ST/(ET + ST + OL)) * 100
      )
      

      #temp df above means each loop can be binded to the main treecover_df
      # bind rows of treecover and tmp file of gridcell, neo, actual bp
      treecover_df <- dplyr::bind_rows(treecover_df, treecover_tmp)
      ET_df <- dplyr::bind_rows(ET_df, ET_tmp)
      ST_df <- dplyr::bind_rows(ST_df, ST_tmp)


      #bind and write output
      pft.output <- as.data.frame(cbind(TBE1,TBE2,IBE,MTBE,TSE,MTSE,IBS,ISTS,TBS,TSD,LSE,GL,AL,ET,ST,OL))
      output <- cbind(dat$TW_NEO, dat$TW_BP, dat[,1:31], pft.output)

      #renames the columns to something meaningful
      colnames(output)[1:2] <- c("TW_NEO", "TW_BP")


      #CM ADDED TAXA SUMS - trying to find out what cells have data and what don't?
      #now this is probbaly irrelevant and can be deleted unless i want to use it to see specifically how many times there is pollen data within the cell  =============================================================================================
      taxa_flags <- as.numeric(colSums(dat[, taxa] > 0) > 0)  #can't use taxa_names instead of dat[, taxa] as that would return only vectors and not df data

      #have to transpose the taxa_flags (0/1 of taxa) because otherwise it'll print length(taxa) rows
      summary_row <- data.frame(GRIDCELL = GRIDCELL, taxa_sum = sum(taxa_flags), t(taxa_flags)) # creates new row to be added to main df later
      colnames(summary_row) <- c("GRIDCELL", "taxa_sum", taxa_names) # names the col names of the taxa summaries as the taxa names
      new_taxa_sums_df <- dplyr::bind_rows(new_taxa_sums_df, summary_row)

  
      
      #get simple rate of change
      roc <- as.matrix(paldist(output[,3:33]/100, dist.method="sq.chord"))
      roc <- roc[row(roc) == col(roc) + 1] ## extract off-diagonal
      roc[11] <- 0  #adds a value for the lowest sample+

      #creates a new df for ROC data
      roc_2 <- c(GRIDCELL, roc)
      roc_row <- as.data.frame(t(roc_2))
      roc_df <- rbind(roc_df, roc_row)

      #write neo.tw.gc file
      #write.csv(cbind(output, roc), paste0("NEO.TW.GC.", GRIDCELL, ".csv")) #########################################


      #draw basic plot centred around start of agriculture
      #note: 0 means -100 -> +100 i.e. start of agriculture


      # #pollen taxa
      # x <- strat.plot(output[,3:33], scale.percent = T, yvar = output$TW_NEO, plot.line = F,
      #                 lwd.bar = 35, yTop = 0.6, xRight = 0.8, y.tks = seq(-1000,1000,200),
      #                 title = paste("gridcell", gsub("output.mean.site.list.","",i)))
      # strat.plot(roc, yvar = output$TW_NEO, yTop = 0.6, xLeft = 0.8, y.axis = FALSE,
      #            plot.line = F, lwd.bar = 10, add = TRUE)
      # 
      # #plant functional types
      # x <- strat.plot(output[,34:49], scale.percent = T, yvar = output$TW_NEO, plot.line = F,
      #                 lwd.bar = 35, yTop = 0.6, xRight = 0.8, y.tks = seq(-1000,1000,200),
      #                 title = paste("gridcell", gsub("output.mean.site.list.","",i)))
      # strat.plot(roc, yvar = output$TW_NEO, yTop = 0.6, xLeft = 0.8, y.axis = FALSE,
      #            plot.line = F, lwd.bar = 10, add = TRUE)

    }
  }
}


#counts for the amount of cells that have or do not have pollen data
count
count2 # this is 186 and not 184 which is in roc. dropped_no_tw is 45 adn 46 meaning there is no pollen data for these cells when the neoltihic started

neo_cells_without_pollen <- count - count2
neo_cells_without_pollen


# create object?character?list? thing? of gridcell and then the tw labels 
colm_names_roc_df <- c("GRIDCELL", tw_labels)

#call the roc_df col headers gridcell and then the tw labels
colnames(roc_df) <- colm_names_roc_df



############# ensuring only reliable/valid roc values are kept ############


#count how many roc values exist that are no 0 or 1
#turn roc data into numeric which is all cols right of gridcell col
roc_df[, 2:ncol(roc_df)] <- lapply(roc_df[, 2:ncol(roc_df)], as.numeric)
#if grid cells are greater than 0.98 (meaning they are invalid), remove them
roc_df[, 2:ncol(roc_df)][roc_df[, 2:ncol(roc_df)] > 0.98] <- NA

#count how many gridcells are not 0 1 or na
roc_df$roc_count <- rowSums(roc_df[, 2:ncol(roc_df)] != 0 & roc_df[, 2:ncol(roc_df)] != 1 & !is.na(roc_df[, 2:ncol(roc_df)]))

#only keep roc gridcells when there is more than 8 roc values
roc_df <- roc_df[roc_df$roc_count >= 8, ]

#remove the roc_count col
roc_df <- roc_df[c(1:12)]

write.csv(roc_df, "All_ROCs.csv", row.names = FALSE)




################################################################################################################################################################
######################### PLOTTTTTTING DATA ###############################

#reshape with direction is to turn data to be long for plotting
#varying is saying the cols whcih have repeating data which need to be transferred into long so all except gridcell col. 
#v.names is saying what ones i want to be in long so what the new col will be called with all teh varying values. 
#idvar is saying that all the gridcells should be put into long format.



################ reshape data so it is long meaning gridcell is repeated 11 times #############

roc_long <- reshape(roc_df, varying = colnames(roc_df)[-1], v.names = "ROC", timevar = "Year", times = as.numeric(colnames(roc_df)[-1]), idvar = "GRIDCELL", direction = "long")

#remove the indexing? or whatever it is called when it seems like there are row names
row.names(roc_long) <- NULL

# merge roc and treecover together
merged_df <- merge(roc_long, treecover_df, by.x = c("GRIDCELL", "Year"), by.y = c("GRIDCELL", "TW_NEO"), all.x = TRUE) 

#rename year to tw_neo
names(merged_df)[names(merged_df) == "Year"] <- "TW_NEO"





################## plotting roc for the entirity of the data in a line plot ############
par(mfrow=c(1,1), mar=c(5,5,5,5))

# create empty plot
plot(roc_long$Year, roc_long$ROC, type = "n" ,  ylim = c(0, 1), xlab = "Years since Neolithic start date", ylab = "ROC", xaxs = "i", yaxs = "i") 

for (cell in unique(roc_long$GRIDCELL)){
  cell_line_df <- roc_long[roc_long$GRIDCELL == cell,]
  lines(cell_line_df$Year, cell_line_df$ROC, col = "darkslategray", lwd = 1.5)
}




############# plotting roc against treecover as a scatter plot #################

#set margins, outer margins, etc
par(mfrow=c(2,5), mar=c(2,2,2,2), oma = c(4, 4, 4, 2) )

# sort so goes -1000 to 800
years <- sort(as.numeric(unique(merged_df$TW_NEO))) 

#plot multi-scatter plots of treecover % vs ROC per year
for (i in years[2:11]) { # 2:11 so it does -800 to 1000
  df_sub <- subset(merged_df, TW_NEO == i)
  plot(df_sub$treecover, df_sub$ROC, ylab = "", xlab = "", 
       main = paste(i), col = "forestgreen", pch = 19, ylim = c(0, 0.5), cex.axis = 1.2, cex.main = 1.6)
}
mtext("ROC", side=2, outer=TRUE, line=2, cex = 1.4)
mtext("% Treecover", side=1, outer=TRUE, line=2, cex = 1.4)
mtext("Treecover % vs ROC per timewindow", side=3, outer=TRUE, line=2, cex = 1.4)






####################################################################################################################################################################################################################################################################################################################
### GROUPING TREE COVER BY THE PERCENT GROUP/CATERGORIES FOR BOXPLOT #####
###################################################################################################################################

#sort data by gridcell and then by tw_neo
treecover_change <- merged_df[order(merged_df$GRIDCELL, merged_df$TW_NEO),]

treecover_change$ROC <- as.numeric(treecover_change$ROC)


### split tree cover into percentage groups ###

treecover_change$percent_group <- cut(
  treecover_change$treecover,
  breaks = c(0, 50, 75, 100),
  labels = c("0-50", "50-75", "75-100")
)

 
# treecover_change$percent_group <- cut(
#   treecover_change$treecover,
#   breaks = c(0, 20, 40, 60, 80, 100),
#   labels = c("0-20", "20-40", "40-60", "60-80", "80-100")
# )



#loop through each cell to calc change in treecover between time windoows
for (cell in unique(treecover_change$GRIDCELL)) {
  i <- which(treecover_change$GRIDCELL == cell)
  #creates a value for change in tree cover.
  
  #na the first row (as there's nothing to compare it to and then continue)
  treecover_change$treecover_change[i] <- c(NA, diff(treecover_change$treecover[i]))
}


#### create subset list of subsetted data to loop through and create boxplots
subset_list <- list(
  subset(treecover_change, percent_group == "0-50"),
  subset(treecover_change, percent_group == "50-75"),
  subset(treecover_change, percent_group == "75-100")
)

# subset_list <- list(
#   subset(treecover_change, percent_group == "0-20"),
#   subset(treecover_change, percent_group == "20-40"),
#   subset(treecover_change, percent_group == "40-60"),
#   subset(treecover_change, percent_group == "60-80"),
#   subset(treecover_change, percent_group == "80-100")
# )



#subset_list <- list(subset_0_20, subset_20_40, subset_40_60, subset_60_80, subset_80_100) # create a list for the names of subsets
#names_of <- c("0-20", "20-40", "40-60", "60-80", "80-100") # create list of groups for figure labels

names_of <- c("0-50", "50-75", "75-100")

#roc has no data in -1000 so remove -1000 as a point
updated_date_list <- seq(-800, 1000, 200)


par(mfrow=c(1, 3), mar=c(1,2,2,1))

counting_loops <- 1


# roc_counts <- treecover_change %>%
#   filter(!is.na(ROC),
#          TW_NEO %in% updated_date_list) %>%
#   group_by(percent_group, TW_NEO) %>%
#   summarise(
#     n = sum(!is.na(ROC)),
#     .groups = "drop"
#   )
#View(roc_counts)

#############################################################
####################### ROC BOXPLOTS #######################
#############################################################

for (df in subset_list) {
  
  group <- names_of[counting_loops]
  
  boxplot(ROC ~ factor(TW_NEO), data = df, subset = TW_NEO %in% updated_date_list, ylim = c(0, .5), main = paste(group, "%"), space = 0.1, xlab = "", cex.axis = 1.2, cex.main = 1.6, cex = 1.4, xaxt = "n", col = "skyblue4") #\nTeecover
  
  axis(side = 1, tick = TRUE, at = seq(1,11,1), labels = FALSE) # WANT tick marks for all 
  axis(1, at = c(1,6,11), labels = c(-1000,0,1000), cex.axis = 1.4) # want labels for -1000, 0, 1000
  
  #create line to show where the start of the neolithic is
  abline(v = 6, col = "darkred", lwd = 2)
  counting_loops <- counting_loops + 1
}


mtext("ROC", side=2, outer=TRUE, line=2, cex = 1.2)
mtext("Years relative to Neolithic", side=1, outer=TRUE, line=2, cex = 1.2)
mtext("Treecover change groups from start of the Neolithic", side=3, outer=TRUE, line=2, cex = 1.2)



#############################################################
########### TREE COVER CHANGE BOXPLOTS ######################
#############################################################

par(mfrow=c(1, 3), mar=c(1,2,2,1))

counting_loops <- 1

for (df in subset_list) {
  
  #find the group by using indexing and going through the list based on how many times the loop has already ran
  group <- names_of[counting_loops]
  
  #create boxplot
  boxplot(treecover_change ~ factor(TW_NEO), data = df, subset = TW_NEO %in% updated_date_list, ylim = c(-40,40), main = paste(group, "%"), space = 0.1, xlab = "", ylab = "",cex.axis = 1.2, cex.main = 1.6, cex = 1.4, xaxt = "n", col = "slategray") #\nTeecover
  
  # WANT tick marks for all x axis 
  axis(side = 1, tick = TRUE, at = seq(1,11,1), labels = FALSE)
  
  # want labels for -1000, 0, 1000 always (to avoid there being too many dates)
  axis(1, at = c(1,6,11), labels = c(-1000,0,1000), cex.axis = 1.4) 
  
  #create line to show where teh start of the neolithic is
  abline(v = 6, col = "darkred", lwd = 2)
  counting_loops <- counting_loops + 1
}


mtext("% of tree cover change from previous TW", side=2, outer=TRUE, line=2, cex = 1.2)
mtext("Years relative to Neolithic", side=1, outer=TRUE, line=2, cex = 1.2)
mtext("Treecover change groups from start of the Neolithic", side=3, outer=TRUE, line=2, cex = 1.2)





###################### 
par(mfrow=c(1, 1), mar=c(5,5,5,5))

#remove all of the 0 and remove 1 from roc_long
roc_long <- roc_long[roc_long$ROC != 0 & roc_long$ROC < 0.99999, ]

#ensure that these are numeric to avoid errors/r being upset
roc_long$ROC  <- as.numeric(as.character(roc_long$ROC))
roc_long$Year <- as.numeric(as.character(roc_long$Year))

#want to remove the -1000 col as it is empty
roc_subset <- subset(roc_long, Year >= -800 & Year <= 1000)

#with the roc_long file, plot ROC by year
boxplot(data = roc_long, ROC ~ Year, xlab = "Year", ylab = "ROC", main = "ROC by Year", ylim = c(0, 0.25))




# #for qgis to see change in et treecover by tw
# ET_wide <- pivot_wider(ET_df, id_cols = GRIDCELL, names_from = TW_NEO, values_from = c(ET_percent, ET_change))
# write.csv(ET_wide, "ET_wide.csv", row.names = FALSE)
# 
# ST_wide <- pivot_wider(ST_df, id_cols = GRIDCELL, names_from = TW_NEO, values_from = c(ST_percent, ST_change))
# write.csv(ST_wide, "ST_wide.csv", row.names = FALSE)



###################################################################################################################################
####### PLOTTING TREE COVER CHANGE OVER TIME #############
###################################################################################################################################

#unreadable plot really...


# plot(treecover_change$TW_NEO, treecover_change$treecover_change, type = "n", ylim = c(-50, 50),
#      xlab = "Years relative to Neolithic", ylab = "% tree cover change", xaxs = "i", yaxs = "i", xlim = c(-800, 800))
# 
# 
# for (cell in unique(treecover_change$GRIDCELL)) {
#   sub <- treecover_change[treecover_change$GRIDCELL == cell, ]
#   lines(sub$TW_NEO, sub$treecover_change, col = "black")
# }




###################################################################################
###### plotting tree cover change over time but for evergreen vs boreal trees #####
###################################################################################

#set margine for multi plot
par(mfrow=c(2,1), mar=c(1,1,1,1), oma = c(3,4,7,1))


#ensure data is ordered by gridcell adn then neolithic date
ET_df <- ET_df[order(ET_df$GRIDCELL, ET_df$TW_NEO), ]

#for each gridcell in et df, find hte percentage of tree cover change from one tw to another
for(cell in unique(ET_df$GRIDCELL)) {
  i <- which(ET_df$GRIDCELL == cell)
  ET_df$ET_change[i] <- c(NA, diff(ET_df$ET_percent[i]))
}

#same as above
ST_df <- ST_df[order(ST_df$GRIDCELL, ST_df$TW_NEO), ]
for(cell in unique(ST_df$GRIDCELL)) {
  i <- which(ST_df$GRIDCELL == cell)
  ST_df$ST_change[i] <- c(NA, diff(ST_df$ST_percent[i]))
}



#create an empty plot 
plot(ET_df$TW_NEO, ET_df$ET_percent, type = "n", ylim = c(-50, 50),xaxs = "i", yaxs = "i", xlim = c(-800, 1000), xlab = "", ylab = "")

#plot percetage change of evergreen treecover per time window
for (cell in unique(ET_df$GRIDCELL)) {
  sub <- ET_df[ET_df$GRIDCELL == cell, ]
  lines(sub$TW_NEO, sub$ET_change, col = "palegreen4")
}
#write legend
legend("topright", legend = "Evergreen",
       col = "palegreen4", lty = 1, lwd = 2, bty = "n", cex = 0.9)



#create empy plot
plot(ST_df$TW_NEO, ST_df$ST_change, type = "n", ylim = c(-50, 50), xaxs = "i", yaxs = "i", xlim = c(-800,1000), xlab = "", ylab = "")

#plot percentage change of summer trees per time window
for (cell in unique(ST_df$GRIDCELL)) {
  sub <- ST_df[ST_df$GRIDCELL == cell, ]
  lines(sub$TW_NEO, sub$ST_change, col = "olivedrab4")
}

#write legen
legend("topright", legend = "Broadleaf",
       col = "olivedrab4", lty = 1, lwd = 2, bty = "n", cex = 0.9)


#write axis labels for multi plot
mtext("% Tree cover change from previous time window", side=2, outer=TRUE, line=2, cex = 1.2)
mtext("Years relative to Neolithic", side=1, outer=TRUE, line=2, cex = 1.2)
mtext("Tree cover change from start of the Neolithic \nfor evergreen and broadleaf trees", side=3, outer=TRUE, line=2, cex = 1.2)



#####################################################################################
######################### WRITING ROC AND TREECOVER CSV #############################
#####################################################################################

#remove %group col
treecover_change$percent_group <- NULL 

#remove na rows
roc_long<- na.omit(roc_long)

#make roc data wide
roc_wide <- pivot_wider(roc_long,  id_cols = GRIDCELL, names_from = Year, values_from = "ROC", names_prefix = "ROC_")

#make tree cover change wide
treecover_wide1 <- pivot_wider(treecover_change, id_cols = GRIDCELL, names_from = TW_NEO, names_prefix = "TCchange_", values_from = treecover_change)

#make tree cover wide
treecover_wide2 <- pivot_wider(treecover_change, id_cols = GRIDCELL, names_from = TW_NEO, names_prefix = "TC_", values_from = c(treecover))

# stick tree cover change and tree cover wide versions together
treecover_change_full <- merge(treecover_wide1, treecover_wide2, by= "GRIDCELL", all = TRUE)

#stick full tree cover change and roc together (this is done a long way as it is how i got it to work...)
roc_treecover_change <- merge(roc_wide, treecover_change_full, by= "GRIDCELL", all = TRUE)
write.csv(roc_treecover_change, "treecover_and_roc.csv", row.names = FALSE)


