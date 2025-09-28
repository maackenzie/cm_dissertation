setwd("D:/uni onedrive/OneDrive - University of Plymouth/CM_copy_rf_files")
#untitled1
library(rioja)
library(vegan)
library(readr)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(tidyr)


#input_dir <- "input_RV"
input_dir <- "all_RV/results.GC.jul23/RV"

#file.list <- list.files("./input_RV")
file.list <- list.files("all_RV/results.GC.jul23/RV")


#neolothic_idw_file <- read.csv("fullfull_neo_dataset_summary.csv") #currently, there is more than one data point for each gridcell so clearly this isnt right

neolothic_idw_file <- read.csv("no_duplicates_neostart_joined.csv") # has loads of na files
#if startneo date is na: remove row

neolothic_idw_file <- neolothic_idw_file %>% filter(!is.na(VALUE)) # if there is no value/neostart, then remove the row
neolothic_idw_file <- neolothic_idw_file[,c("LCGRID_ID", "VALUE")] # turn file into just gridcell and value


#names(neolothic_idw_file)[names(neolothic_idw_file) == 'LCGRID_ID'] <- 'GRIDCELL' #rename lcgrid to gridcell
names(neolothic_idw_file)[names(neolothic_idw_file) == 'VALUE'] <- 'startNeo' # rename value to neo date
StartNeo2 <- neolothic_idw_file
StartNeo2$startNeo <- round((StartNeo2$startNeo - 100) / 200) * 200 + 100 # make it odd number - was like this for the original data - can't remember why...-------------------------------------------------------
View(StartNeo2)





# reoder cols to gridcell then neo date

#idw_neo_date <- StartNeo$idw_neo_date


#i <- file.list[101:104]

noneo_list <- c()
GRIDCELL_list <- c()

neo_start_for_qgis_df <- data.frame(LCGRID_ID = character(), NeoTW = numeric()) #creates a df to put in gridcell and neo start date later
new_taxa_sums_df <- data.frame(GRIDCELL = character(), taxa_sum = numeric()) #creates a df to put in gridcell and taxa data sum (binary if there is data) later
roc_df <- data.frame()
treecover_df <- data.frame()



cells_from_files <- as.character(gsub("\\D", "", file.list)) # this is a list of GRIDCELLS with pollen data (222 files)

#has to be add character as dplyr::bind rows gets unhappy
#for each gridcell that is in isern dataset

listylist <- c()
listylist2 <- c()

#checking the number filtering in for loop - prints 162
# for (GRIDCELL in as.character(unique(StartNeo2$LCGRID_ID))){
#   if (GRIDCELL %in% cells_from_files){ #if there is a file of that gridcell
#     i <- file.list[cells_from_files == GRIDCELL][1]
#     listylist <- append(listylost, i)
#   }
# }
# print(length(listylist))


for (GRIDCELL in as.character(unique(StartNeo2$LCGRID_ID))){
  
  if (GRIDCELL %in% cells_from_files){ #if there is a file of that gridcell
    i <- file.list[cells_from_files == GRIDCELL][1]
    
    #only 162? have lost 60 gridcells somewhere?-----------------------------------e.g.142 - i think these are the cells of which are further east so have no neo data? maybe my interpolation should be a more easterly extent than current?
    
    #read in the pollen file for the grid cell
    dat <- read.csv(paste0(input_dir,"/",i))
    
    #do some formatting to re-shape the original data
    taxa <- dat[,1]
    
    #get taxon names
    dat <- as.data.frame(t(dat[,-1])) #drop the first column, then transpose
    colnames(dat) <- taxa             #rename the columns
    taxa_names <- colnames(dat)
    dat$TW_BP <- as.numeric(gsub("X", "", row.names(dat)))#get original TW as a column
    
    #extract start time window for the gridcell indexed using gridcell
    #NeoTW <- StartNeo[StartNeo$LCGRID_ID == GRIDCELL, 2]
    
    NeoTW <- StartNeo2[StartNeo2$LCGRID_ID == GRIDCELL, "startNeo"] # means going to the specific gridcell and then extracting the data that is in the third col - the neo start date
    
    
    #if statement to catch gridcells where there isn't a NEO start date
    #if there IS a NEO start date the variable will have length of 1
    
    if (length(NeoTW) == 1){
      #create a new variable called TW in the dataset
      
      dat$TW_NEO <- -(seq(3100, 11700, 200) - NeoTW)
      
      #at this point tw_neo is not rounded to nearest 200
      
      #filter to only include 11 key grid cell (5 before, start NEO, and 5 after)
      dat <- subset(subset(dat, TW_NEO < 1100), TW_NEO > -1100) #nested subset function
      
      
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
      ST   <- IBS + ISTS + TBS + TSD
      OL   <- LSE + GL + AL
      tree_cover_ratio <- ((ET + ST)/ (ET + ST + OL)) * 100
      
      treecover_row <- data.frame(GRIDCELL = GRIDCELL, t(as.data.frame(tree_cover_ratio)))
      colnames(treecover_row[-1]) <- tw_labels
      treecover_df <- dplyr::bind_rows(treecover_df, treecover_row)
      
      #bind and write output
      pft.output <- as.data.frame(cbind(TBE1,TBE2,IBE,MTBE,TSE,MTSE,IBS,ISTS,TBS,TSD,LSE,GL,AL,ET,ST,OL))
      output <- cbind(dat$TW_NEO, dat$TW_BP, dat[,1:31], pft.output)
      
      colnames(output)[1:2] <- c("TW_NEO", "TW_BP")   #renames the columns to something meaningful
      
      #doesn't currently differentiate grid cells with "missing" time windows
      
      #CM ADDED TAXA SUMS
      taxa_flags <- as.numeric(colSums(dat[, taxa] > 0) > 0)  #can't use taxa_names instead of dat[, taxa] as that would return only vectors and not df data
      
      #have to transpose the taxa_flags (0/1 of taxa) because otherwise it'll print length(taxa) rows
      summary_row <- data.frame(GRIDCELL = GRIDCELL, taxa_sum = sum(taxa_flags), t(taxa_flags)) # creates new row to be added to main df later
      colnames(summary_row) <- c("GRIDCELL", "taxa_sum", taxa_names) # names the col names of the taxa summaries as the taxa names
      new_taxa_sums_df <- dplyr::bind_rows(new_taxa_sums_df, summary_row)
      
      
      #get simple rate of change
      roc <- as.matrix(paldist(output[,3:33]/100, dist.method="sq.chord"))
      roc <- roc[row(roc) == col(roc) + 1] ## extract off-diagonal
      roc[11] <- 0  #adds a value for the lowest sample+
      
      roc_2 <- c(GRIDCELL, roc)
      roc_row <- as.data.frame(t(roc_2))
      roc_df <- rbind(roc_df, roc_row)
      
      
      # 
      # listylist <- append(listylist, i)
      # write.csv(cbind(output, roc), paste0("delete.NEO.TW.GC.", GRIDCELL, ".csv"))
      # 
      # #draw basic plot centred around start of agriculture
      # #note: 0 means -100 -> +100 i.e. start of agriculture
      # 
      # # pollen taxa
      # x <- strat.plot(output[, 3:33], scale.percent = T, yvar = output$TW_NEO, plot.line = F,
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
  #else {print(paste(GRIDCELL, "no pollen data"))}
  
  
  
}



colm_names_df <- c("GRIDCELL", tw_labels)
#colm_names_df <- c("GRIDCELL", paste0("TW_", tw_labels))

colnames(roc_df) <- colm_names_df
write.csv(roc_df, "All_ROCs.csv", row.names = FALSE)





#to find the gird cells with <5 taxa info
new_taxa_sums_df <- as.data.frame(new_taxa_sums_df)
new_taxa_sums_df$taxa_sum <- as.numeric(new_taxa_sums_df$taxa_sum)
df_sorted <- new_taxa_sums_df[order(new_taxa_sums_df$taxa_sum), ]
View(df_sorted) #why is it emptyyyyyy------------------------------------------------------








#print(df_sorted[df_sorted$taxa_sum < 5, c("GRIDCELL", "taxa_sum")], row.names = FALSE)
taxa_low_removed <- (df_sorted[df_sorted$taxa_sum > 15, c("GRIDCELL", "taxa_sum")])
#print(taxa_low_removed, row.names = F)



taxa_low_only <- taxa_low_removed["GRIDCELL"]
taxa_low_only_new <- as.numeric((as.character(unlist(as.list(taxa_low_only)))))
#print(taxa_low_only_new)



#grid cell taxa data sorted by frequancy (how many taxa calculated in each cell)
lowest <- (new_taxa_sums_df[order(new_taxa_sums_df$taxa_sum), c("GRIDCELL", "taxa_sum")])
#print(lowest, row.names = FALSE)


#print a list of cells with very minimal pollen data
write.csv(new_taxa_sums_df, "taxa_presence_summary.csv", row.names = FALSE)


#print the list of cells without neo data sorted by cell num
noneo_df <- data.frame(GRIDCELL = sort(as.numeric(noneo_list)))
write.csv(noneo_df, "NoNeo_Date_List.csv", row.names = FALSE)



# put neo start date in a csv
neo_start_for_qgis_df$NeoStart <- as.numeric(as.character(neo_start_for_qgis_df$NeoStart))
write.csv(neo_start_for_qgis_df, "neo_start_for_qgis.csv", row.names = FALSE)



#creating ROC graph

roc_data <- read.csv("All_ROCs.csv", check.names = F)
View(roc_data)

roc_data <- roc_data[(roc_data$GRIDCELL %in% taxa_low_only_new), ]
roc_data[roc_data > 0.98 & roc_data < 1.1] <- NA #remove 1's
write.csv(roc_data, "ROC_remove_low_taxa.csv", row.names = FALSE)
#empty


cell_df <- roc_data %>% pivot_longer(cols = -GRIDCELL, names_to = "Year", values_to = "ROC") %>% mutate(Year = as.numeric(Year)) # have to convert it to numeric otherwise it gets upset
# View(cell_df) #currently has no data


write.csv(cell_df, "cell_df.csv", row.names = FALSE)


for (cell_num in unique(roc_data$GRIDCELL)){
  specific_cell_df <- cell_df %>%
    filter(GRIDCELL == cell_num)

  plot1 <- ggplot(specific_cell_df, aes(x = Year, y = ROC)) + geom_point() + geom_line() + labs( title = paste("ROC for Gridcell", cell_num), x = "Year", y = "ROC" ) + theme_minimal()
  print(plot1) #prints single ROC graph
}



ggplot(cell_df, aes(x = Year, y = ROC, group = GRIDCELL)) + geom_line(color = "darkslategrey", size = 0.5) + geom_point(color = "darkslategrey", size = .5) + ylim(0, 0.5)+ scale_x_continuous(breaks = seq(-1000, 1000, by = 200)) +
  labs(title = "ROC Value pre and post neolithic", x = "Years since Neolithic start date", y = "ROC")
#some have been removed and taken down to 0.25 as this had the most data and so seemed the most revelvant


#collate all roc lines into a mean with confidence values to see change over time. would it then be useful to include time windows larger than -1000 to 1000 and hten cut it down so it doesnt start at 0?




#how does vegetation change before the neolithic. how does vegetation change after.



colnames(treecover_df) <- colm_names_df

roc_data <- roc_data[(roc_data$GRIDCELL %in% taxa_low_only_new), ]
roc_data[roc_data > 0.98 & roc_data < 1.1] <- NA #remove 1's
write.csv(roc_data, "ROC_remove_low_taxa.csv", row.names = FALSE)





all_roc_file <- read.csv("All_ROCs.csv", check.names = F)
treecover_file <- read.csv("treecover.csv", check.names = F)

roc_long <- all_roc_file %>%
  pivot_longer(cols = -GRIDCELL, names_to = "Year", values_to = "ROC")

treecover_long <- treecover_file %>%
  pivot_longer(cols = -GRIDCELL, names_to = "Year", values_to = "percent_tree_cover")

merged_df <- roc_long %>%
  left_join(treecover_long, by = c("GRIDCELL", "Year"))


merged_df$Year <- factor(merged_df$Year, levels = seq(-1000, 1000, by = 200))
ggplot(merged_df, aes(x = percent_tree_cover, y = ROC)) +
  geom_point(color = "forestgreen") + facet_wrap(~Year, scales = "free") +
  labs(x = "% Tree Cover", y = "ROC", title = "ROC vs Tree Cover per Year") + ylim(0, .9)



#measure change in vegetation vs







