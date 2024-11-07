### Processing EpiNano output for all tissues and stages

setwd('/no_backup_isis/enovoa/nextflow_outputs/mouse_rRNA/analysis_epinano')

# Load libraries
library('plyr')
library('ggplot2')
library('ggrepel')

# Function to read and filter input data
read_and_filter <- function(file) {
  data <- read.csv(file)
  data <- data[grepl("18S", data$X.Ref),]
  data$sum <- data$mis + data$del + data$ins
  return(data[, c("pos", "sum")])
}

# List of input files
files <- c(
  'brain_embryo_rep1.csv', 'brain_embryo_rep2.csv', 
  'brain_newborn_rep1.csv', 'brain_newborn_rep2.csv', 
  'brain_adult_rep1.csv', 'brain_adult_rep2.csv', 
  'heart_embryo_rep1.csv', 'heart_embryo_rep2.csv', 
  'heart_newborn_rep1.csv', 'heart_newborn_rep2.csv', 
  'heart_adult_rep1.csv', 'heart_adult_rep2.csv', 
  'liver_embryo_rep1.csv', 'liver_embryo_rep2.csv', 
  'liver_newborn_rep1.csv', 'liver_newborn_rep2.csv', 
  'liver_adult_rep1.csv', 'liver_adult_rep2.csv', 
  'e.9.5_rep1.csv', 'e.9.5_rep2.csv', 
  'testis_newborn_rep1.csv', 'testis_newborn_rep2.csv', 
  'testis_adult_rep1.csv', 'testis_adult_rep2.csv'
)

# Read and filter input data
inputs <- lapply(files, read_and_filter)

# Merge input data
merged <- Reduce(function(x, y) merge(x, y, by = "pos"), inputs)

# Set column names
colnames(merged) <- c("position", files)

# Write merged data to file
#write.table(merged, file = "18S_summed_errors_all_samples.txt", quote = FALSE, row.names = TRUE, sep = "\t")

# Read discovered positions
discovered_positions <- read.csv("discovered_positions_18S_from_clean_list.csv", header = FALSE)
colnames(discovered_positions) <- "position"

# Merge discovered positions with merged data
merged2 <- merge(discovered_positions, merged, by = "position")
merged2$position <- sub("^", "18S:", merged2$position)
row.names(merged2) <- merged2$position
merged2$position <- NULL

# Final merged data
merged_18S_tissues <- merged2



### 28S

# Load libraries
library('plyr')
library('ggplot2')
library('ggrepel')

# Function to read and filter input data
read_and_filter <- function(file) {
  data <- read.csv(file)
  data <- data[grepl("28S", data$X.Ref),]
  data$sum <- data$mis + data$del + data$ins
  return(data[, c("pos", "sum")])
}

# List of input files
files <- c(
  'brain_embryo_rep1.csv', 'brain_embryo_rep2.csv', 
  'brain_newborn_rep1.csv', 'brain_newborn_rep2.csv', 
  'brain_adult_rep1.csv', 'brain_adult_rep2.csv', 
  'heart_embryo_rep1.csv', 'heart_embryo_rep2.csv', 
  'heart_newborn_rep1.csv', 'heart_newborn_rep2.csv', 
  'heart_adult_rep1.csv', 'heart_adult_rep2.csv', 
  'liver_embryo_rep1.csv', 'liver_embryo_rep2.csv', 
  'liver_newborn_rep1.csv', 'liver_newborn_rep2.csv', 
  'liver_adult_rep1.csv', 'liver_adult_rep2.csv', 
  'e.9.5_rep1.csv', 'e.9.5_rep2.csv', 
  'testis_newborn_rep1.csv', 'testis_newborn_rep2.csv', 
  'testis_adult_rep1.csv', 'testis_adult_rep2.csv'
)

# Read and filter input data
inputs <- lapply(files, read_and_filter)

# Merge input data
merged <- Reduce(function(x, y) merge(x, y, by = "pos"), inputs)

# Set column names
colnames(merged) <- c("position", files)

# Write merged data to file
#write.table(merged, file = "28S_summed_errors_all_samples.txt", quote = FALSE, row.names = TRUE, sep = "\t")

# Read discovered positions
discovered_positions <- read.csv("discovered_positions_28S_from_clean_list.csv", header = FALSE)
colnames(discovered_positions) <- "position"

# Merge discovered positions with merged data
merged2 <- merge(discovered_positions, merged, by = "position")
merged2$position <- sub("^", "28S:", merged2$position)
row.names(merged2) <- merged2$position
merged2$position <- NULL

# Set column names for merged data
#colnames(merged2) <- colnames(merged_28S_tissues)

# Final merged data
merged_28S_tissues <- merged2

# Combine 18S and 28S merged data
merged_all_tissues_18S_28S <- rbind(merged_18S_tissues, merged_28S_tissues)


###### Panel B


library(tidyverse) 
library(dplyr)
library(ggplot2)
library(ggrepel) 
library(MASS)
library(reshape2)
library(ggpubr)
library(tune) # for coord_obs_pred(), which sets the scales of x & y axes to be equal
library(data.table)


# Create an object that will contain only replicates of the same sample and remove the first 20 and last 20 bases

#Embryo Brain
embryo_brain_replicability <- NULL
embryo_brain_replicability$position <- rownames(merged_18S_all)
embryo_brain_replicability <- as.data.frame(embryo_brain_replicability)
embryo_brain_replicability$x <- merged_18S_all$brain_embryo_rep1
embryo_brain_replicability$y <- merged_18S_all$brain_embryo_rep2
embryo_brain_replicability$score <- abs(embryo_brain_replicability[,2]-embryo_brain_replicability[,3])
embryo_brain_replicability <- embryo_brain_replicability[-(1850:1870), , drop = FALSE]
embryo_brain_replicability <- embryo_brain_replicability[-(1:20), , drop = FALSE]


#Embryo Heart
embryo_heart_replicability <- NULL
embryo_heart_replicability$position <- rownames(merged_18S_all)
embryo_heart_replicability <- as.data.frame(embryo_heart_replicability)
embryo_heart_replicability$x <- merged_18S_all$heart_embryo_rep1
embryo_heart_replicability$y <- merged_18S_all$heart_embryo_rep2
embryo_heart_replicability$score <- abs(embryo_heart_replicability[,2]-embryo_heart_replicability[,3])
embryo_heart_replicability <- embryo_heart_replicability[-(1850:1870), , drop = FALSE]
embryo_heart_replicability <- embryo_heart_replicability[-(1:20), , drop = FALSE]

# This should be done for all samples (testis, liver, newborn, adult)

# Define the scatterplot_genes function

scatterplot_genes <- function(my_data, condition_lab_a, condition_lab_b, plot_title, threshold, output) {  
  # condition_lab_a - label for 1st condition
  # condition_lab_b - label for 2nd condition
  # threshold - above which genes to be coloured in red
  # output - directory where the output file is to be written
  
  print(head(my_data))
  
  file_out_0 <- paste(condition_lab_a,"_vs_",condition_lab_b,".pdf", sep="") #name of the output file
  file_out <- paste(output,"/",file_out_0, sep="") # fullpath of the output file
  
  # my_data[,2] - data with condition_lab_a
  # my_data[,3] - data with condition_lab_b
  
  dcols<-densCols(my_data[,2],my_data[,3], colramp=colorRampPalette(blues9[-(1:3)]))
  
  pdf(file=file_out, height=5,width=5,onefile=FALSE)
  
  my_data$score <- abs(my_data[,2]-my_data[,3])
  print(my_data)
  subs<-subset(my_data, score>threshold)
  print(subs)
  
  print(ggplot(my_data) +
          geom_point(aes(x=my_data[,2], y=my_data[,3], col=dcols), size=1 ) +
          scale_color_identity() +
          {if(nrow(subs)!=0) geom_point(data=subs, aes(x=subs[,2], y=subs[,3], col="red"), size=1)}+ # colour red genes above threshold
          {if(nrow(subs)!=0) geom_text_repel(data=subs, aes(x=subs[,2], y=subs[,3], label=position), colour="black",segment.size  = 0.4,segment.color = "grey50",size=5)}+ # print names of genes above threshold
          geom_abline(slope=1, intercept=0,linetype="dashed")+
          coord_obs_pred()+
          
          ggtitle(plot_title)+ 
          xlab(condition_lab_a)+
          ylab(condition_lab_b) +
          stat_cor(aes(x=my_data[,2], y=my_data[,3]),method = "spearman", label.x = 0.01, label.y = 1.2)+ # position of the R corr on the plot - it'll depend on your data and its limits, can make it a variable
          theme_bw()+     	     
          theme(axis.text.x = element_text(face="bold", color="black",size=11),
                axis.text.y = element_text(face="bold", color="black", size=11),
                plot.title = element_text(color="black", size=15, face="bold.italic",hjust = 0.5),
                axis.title.x = element_text(color="black", size=15, face="bold"),
                axis.title.y = element_text(color="black", size=15, face="bold"),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black", size=0.5), 	  
                legend.title = element_text(color = "black", size = 15,face="bold"),
                legend.text = element_text(color = "black", size=15),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  )
  dev.off()
}

# Run it on the objects created in the previous step to make .pdf plots

scatterplot_genes(embryo_brain_replicability, "Embryo_Brain_Rep1", "Embryo_Brain_Rep2", plot_title= "Embryo_Brain_Replicability", threshold= 0.12, output= getwd())
scatterplot_genes(embryo_heart_replicability, "Embryo_heart_Rep1", "Embryo_heart_Rep2", plot_title= "Embryo_heart_Replicability", threshold= 0.12, output= getwd())


# The same approach is done with 28S, just loading the file with epinano results from 28S rRNA


#### Panel C

# Calculate median for each row
merged_18S_all$MEDIAN <- apply(merged_18S_all[, 2:25], 1, median)


#Brain_embryo_Rep1
brain_embryo_rep1 <- NULL
brain_embryo_rep1$position <- merged_18S_all$position
brain_embryo_rep1 <- as.data.frame(brain_embryo_rep1)
brain_embryo_rep1$x <- merged_18S_all$MEDIAN
brain_embryo_rep1$y <- merged_18S_all$brain_embryo_rep1
brain_embryo_rep1$score <- abs(brain_embryo_rep1[,2]-brain_embryo_rep1[,3])
brain_embryo_rep1 <- brain_embryo_rep1[-(1850:1870), , drop = FALSE]
brain_embryo_rep1 <- brain_embryo_rep1[-(1:20), , drop = FALSE]

#Brain_embryo_Rep2
brain_embryo_rep2 <- NULL
brain_embryo_rep2$position <- merged_18S_all$position
brain_embryo_rep2 <- as.data.frame(brain_embryo_rep2)
brain_embryo_rep2$x <- merged_18S_all$MEDIAN
brain_embryo_rep2$y <- merged_18S_all$brain_embryo_rep3
brain_embryo_rep2$score <- abs(brain_embryo_rep2[,2]-brain_embryo_rep2[,3])
brain_embryo_rep2 <- brain_embryo_rep2[-(1850:1870), , drop = FALSE]
brain_embryo_rep2 <- brain_embryo_rep2[-(1:20), , drop = FALSE]

#Brain_newborn_Rep1
brain_newborn_rep1 <- NULL
brain_newborn_rep1$position <- merged_18S_all$position
brain_newborn_rep1 <- as.data.frame(brain_newborn_rep1)
brain_newborn_rep1$x <- merged_18S_all$MEDIAN
brain_newborn_rep1$y <- merged_18S_all$brain_newborn_rep1
brain_newborn_rep1$score <- abs(brain_newborn_rep1[,2]-brain_newborn_rep1[,3])
brain_newborn_rep1 <- brain_newborn_rep1[-(1850:1870), , drop = FALSE]
brain_newborn_rep1 <- brain_newborn_rep1[-(1:20), , drop = FALSE]

#Brain_newborn_Rep2
brain_newborn_rep2 <- NULL
brain_newborn_rep2$position <- merged_18S_all$position
brain_newborn_rep2 <- as.data.frame(brain_newborn_rep2)
brain_newborn_rep2$x <- merged_18S_all$MEDIAN
brain_newborn_rep2$y <- merged_18S_all$brain_newborn_rep3
brain_newborn_rep2$score <- abs(brain_newborn_rep2[,2]-brain_newborn_rep2[,3])
brain_newborn_rep2 <- brain_newborn_rep2[-(1850:1870), , drop = FALSE]
brain_newborn_rep2 <- brain_newborn_rep2[-(1:20), , drop = FALSE]

#Brain_adult_Rep1
brain_adult_rep1 <- NULL
brain_adult_rep1$position <- merged_18S_all$position
brain_adult_rep1 <- as.data.frame(brain_adult_rep1)
brain_adult_rep1$x <- merged_18S_all$MEDIAN
brain_adult_rep1$y <- merged_18S_all$brain_adult_rep1
brain_adult_rep1$score <- abs(brain_adult_rep1[,2]-brain_adult_rep1[,3])
brain_adult_rep1 <- brain_adult_rep1[-(1850:1870), , drop = FALSE]
brain_adult_rep1 <- brain_adult_rep1[-(1:20), , drop = FALSE]
#Brain_adult_Rep2
brain_adult_rep2 <- NULL
brain_adult_rep2$position <- merged_18S_all$position
brain_adult_rep2 <- as.data.frame(brain_adult_rep2)
brain_adult_rep2$x <- merged_18S_all$MEDIAN
brain_adult_rep2$y <- merged_18S_all$brain_adult_rep2
brain_adult_rep2$score <- abs(brain_adult_rep2[,2]-brain_adult_rep2[,3])
brain_adult_rep2 <- brain_adult_rep2[-(1850:1870), , drop = FALSE]
brain_adult_rep2 <- brain_adult_rep2[-(1:20), , drop = FALSE]


#heart_embryo_Rep1
heart_embryo_rep1 <- NULL
heart_embryo_rep1$position <- merged_18S_all$position
heart_embryo_rep1 <- as.data.frame(heart_embryo_rep1)
heart_embryo_rep1$x <- merged_18S_all$MEDIAN
heart_embryo_rep1$y <- merged_18S_all$heart_embryo_rep1
heart_embryo_rep1$score <- abs(heart_embryo_rep1[,2]-heart_embryo_rep1[,3])
heart_embryo_rep1 <- heart_embryo_rep1[-(1850:1870), , drop = FALSE]
heart_embryo_rep1 <- heart_embryo_rep1[-(1:20), , drop = FALSE]

#heart_embryo_Rep2
heart_embryo_rep2 <- NULL
heart_embryo_rep2$position <- merged_18S_all$position
heart_embryo_rep2 <- as.data.frame(heart_embryo_rep2)
heart_embryo_rep2$x <- merged_18S_all$MEDIAN
heart_embryo_rep2$y <- merged_18S_all$heart_embryo_rep3
heart_embryo_rep2$score <- abs(heart_embryo_rep2[,2]-heart_embryo_rep2[,3])
heart_embryo_rep2 <- heart_embryo_rep2[-(1850:1870), , drop = FALSE]
heart_embryo_rep2 <- heart_embryo_rep2[-(1:20), , drop = FALSE]

#heart_newborn_Rep1
heart_newborn_rep1 <- NULL
heart_newborn_rep1$position <- merged_18S_all$position
heart_newborn_rep1 <- as.data.frame(heart_newborn_rep1)
heart_newborn_rep1$x <- merged_18S_all$MEDIAN
heart_newborn_rep1$y <- merged_18S_all$heart_newborn_rep1
heart_newborn_rep1$score <- abs(heart_newborn_rep1[,2]-heart_newborn_rep1[,3])
heart_newborn_rep1 <- heart_newborn_rep1[-(1850:1870), , drop = FALSE]
heart_newborn_rep1 <- heart_newborn_rep1[-(1:20), , drop = FALSE]

#heart_newborn_Rep2
heart_newborn_rep2 <- NULL
heart_newborn_rep2$position <- merged_18S_all$position
heart_newborn_rep2 <- as.data.frame(heart_newborn_rep2)
heart_newborn_rep2$x <- merged_18S_all$MEDIAN
heart_newborn_rep2$y <- merged_18S_all$heart_newborn_rep3
heart_newborn_rep2$score <- abs(heart_newborn_rep2[,2]-heart_newborn_rep2[,3])
heart_newborn_rep2 <- heart_newborn_rep2[-(1850:1870), , drop = FALSE]
heart_newborn_rep2 <- heart_newborn_rep2[-(1:20), , drop = FALSE]

#heart_adult_Rep1
heart_adult_rep1 <- NULL
heart_adult_rep1$position <- merged_18S_all$position
heart_adult_rep1 <- as.data.frame(heart_adult_rep1)
heart_adult_rep1$x <- merged_18S_all$MEDIAN
heart_adult_rep1$y <- merged_18S_all$heart_adult_rep1
heart_adult_rep1$score <- abs(heart_adult_rep1[,2]-heart_adult_rep1[,3])
heart_adult_rep1 <- heart_adult_rep1[-(1850:1870), , drop = FALSE]
heart_adult_rep1 <- heart_adult_rep1[-(1:20), , drop = FALSE]
#heart_adult_Rep2
heart_adult_rep2 <- NULL
heart_adult_rep2$position <- merged_18S_all$position
heart_adult_rep2 <- as.data.frame(heart_adult_rep2)
heart_adult_rep2$x <- merged_18S_all$MEDIAN
heart_adult_rep2$y <- merged_18S_all$heart_adult_rep2
heart_adult_rep2$score <- abs(heart_adult_rep2[,2]-heart_adult_rep2[,3])
heart_adult_rep2 <- heart_adult_rep2[-(1850:1870), , drop = FALSE]
heart_adult_rep2 <- heart_adult_rep2[-(1:20), , drop = FALSE]

#liver_embryo_Rep1
liver_embryo_rep1 <- NULL
liver_embryo_rep1$position <- merged_18S_all$position
liver_embryo_rep1 <- as.data.frame(liver_embryo_rep1)
liver_embryo_rep1$x <- merged_18S_all$MEDIAN
liver_embryo_rep1$y <- merged_18S_all$liver_embryo_rep1
liver_embryo_rep1$score <- abs(liver_embryo_rep1[,2]-liver_embryo_rep1[,3])
liver_embryo_rep1 <- liver_embryo_rep1[-(1850:1870), , drop = FALSE]
liver_embryo_rep1 <- liver_embryo_rep1[-(1:20), , drop = FALSE]

#liver_embryo_Rep2
liver_embryo_rep2 <- NULL
liver_embryo_rep2$position <- merged_18S_all$position
liver_embryo_rep2 <- as.data.frame(liver_embryo_rep2)
liver_embryo_rep2$x <- merged_18S_all$MEDIAN
liver_embryo_rep2$y <- merged_18S_all$liver_embryo_rep3
liver_embryo_rep2$score <- abs(liver_embryo_rep2[,2]-liver_embryo_rep2[,3])
liver_embryo_rep2 <- liver_embryo_rep2[-(1850:1870), , drop = FALSE]
liver_embryo_rep2 <- liver_embryo_rep2[-(1:20), , drop = FALSE]

#liver_newborn_Rep1
liver_newborn_rep1 <- NULL
liver_newborn_rep1$position <- merged_18S_all$position
liver_newborn_rep1 <- as.data.frame(liver_newborn_rep1)
liver_newborn_rep1$x <- merged_18S_all$MEDIAN
liver_newborn_rep1$y <- merged_18S_all$liver_newborn_rep1
liver_newborn_rep1$score <- abs(liver_newborn_rep1[,2]-liver_newborn_rep1[,3])
liver_newborn_rep1 <- liver_newborn_rep1[-(1850:1870), , drop = FALSE]
liver_newborn_rep1 <- liver_newborn_rep1[-(1:20), , drop = FALSE]

#liver_newborn_Rep2
liver_newborn_rep2 <- NULL
liver_newborn_rep2$position <- merged_18S_all$position
liver_newborn_rep2 <- as.data.frame(liver_newborn_rep2)
liver_newborn_rep2$x <- merged_18S_all$MEDIAN
liver_newborn_rep2$y <- merged_18S_all$liver_newborn_rep3
liver_newborn_rep2$score <- abs(liver_newborn_rep2[,2]-liver_newborn_rep2[,3])
liver_newborn_rep2 <- liver_newborn_rep2[-(1850:1870), , drop = FALSE]
liver_newborn_rep2 <- liver_newborn_rep2[-(1:20), , drop = FALSE]

#liver_adult_Rep1
liver_adult_rep1 <- NULL
liver_adult_rep1$position <- merged_18S_all$position
liver_adult_rep1 <- as.data.frame(liver_adult_rep1)
liver_adult_rep1$x <- merged_18S_all$MEDIAN
liver_adult_rep1$y <- merged_18S_all$liver_adult_rep1
liver_adult_rep1$score <- abs(liver_adult_rep1[,2]-liver_adult_rep1[,3])
liver_adult_rep1 <- liver_adult_rep1[-(1850:1870), , drop = FALSE]
liver_adult_rep1 <- liver_adult_rep1[-(1:20), , drop = FALSE]
#liver_adult_Rep2
liver_adult_rep2 <- NULL
liver_adult_rep2$position <- merged_18S_all$position
liver_adult_rep2 <- as.data.frame(liver_adult_rep2)
liver_adult_rep2$x <- merged_18S_all$MEDIAN
liver_adult_rep2$y <- merged_18S_all$liver_adult_rep2
liver_adult_rep2$score <- abs(liver_adult_rep2[,2]-liver_adult_rep2[,3])
liver_adult_rep2 <- liver_adult_rep2[-(1850:1870), , drop = FALSE]
liver_adult_rep2 <- liver_adult_rep2[-(1:20), , drop = FALSE]

#testis_newborn_Rep1
testis_newborn_rep1 <- NULL
testis_newborn_rep1$position <- merged_18S_all$position
testis_newborn_rep1 <- as.data.frame(testis_newborn_rep1)
testis_newborn_rep1$x <- merged_18S_all$MEDIAN
testis_newborn_rep1$y <- merged_18S_all$testis_newborn_rep1
testis_newborn_rep1$score <- abs(testis_newborn_rep1[,2]-testis_newborn_rep1[,3])
testis_newborn_rep1 <- testis_newborn_rep1[-(1850:1870), , drop = FALSE]
testis_newborn_rep1 <- testis_newborn_rep1[-(1:20), , drop = FALSE]

#testis_newborn_Rep2
testis_newborn_rep2 <- NULL
testis_newborn_rep2$position <- merged_18S_all$position
testis_newborn_rep2 <- as.data.frame(testis_newborn_rep2)
testis_newborn_rep2$x <- merged_18S_all$MEDIAN
testis_newborn_rep2$y <- merged_18S_all$testis_newborn_rep3
testis_newborn_rep2$score <- abs(testis_newborn_rep2[,2]-testis_newborn_rep2[,3])
testis_newborn_rep2 <- testis_newborn_rep2[-(1850:1870), , drop = FALSE]
testis_newborn_rep2 <- testis_newborn_rep2[-(1:20), , drop = FALSE]

#testis_adult_Rep1
testis_adult_rep1 <- NULL
testis_adult_rep1$position <- merged_18S_all$position
testis_adult_rep1 <- as.data.frame(testis_adult_rep1)
testis_adult_rep1$x <- merged_18S_all$MEDIAN
testis_adult_rep1$y <- merged_18S_all$testis_adult_rep1
testis_adult_rep1$score <- abs(testis_adult_rep1[,2]-testis_adult_rep1[,3])
testis_adult_rep1 <- testis_adult_rep1[-(1850:1870), , drop = FALSE]
testis_adult_rep1 <- testis_adult_rep1[-(1:20), , drop = FALSE]
#testis_adult_Rep2
testis_adult_rep2 <- NULL
testis_adult_rep2$position <- merged_18S_all$position
testis_adult_rep2 <- as.data.frame(testis_adult_rep2)
testis_adult_rep2$x <- merged_18S_all$MEDIAN
testis_adult_rep2$y <- merged_18S_all$testis_adult_rep2
testis_adult_rep2$score <- abs(testis_adult_rep2[,2]-testis_adult_rep2[,3])
testis_adult_rep2 <- testis_adult_rep2[-(1850:1870), , drop = FALSE]
testis_adult_rep2 <- testis_adult_rep2[-(1:20), , drop = FALSE]

#e9.5_Rep1
e9.5_rep1 <- NULL
e9.5_rep1$position <- merged_18S_all$position
e9.5_rep1 <- as.data.frame(e9.5_rep1)
e9.5_rep1$x <- merged_18S_all$MEDIAN
e9.5_rep1$y <- merged_18S_all$e9.5_rep1
e9.5_rep1$score <- abs(e9.5_rep1[,2]-e9.5_rep1[,3])
e9.5_rep1 <- e9.5_rep1[-(1850:1870), , drop = FALSE]
e9.5_rep1 <- e9.5_rep1[-(1:20), , drop = FALSE]

#e9.5_Rep2
e9.5_rep2 <- NULL
e9.5_rep2$position <- merged_18S_all$position
e9.5_rep2 <- as.data.frame(e9.5_rep2)
e9.5_rep2$x <- merged_18S_all$MEDIAN
e9.5_rep2$y <- merged_18S_all$e9.5_rep2
e9.5_rep2$score <- abs(e9.5_rep2[,2]-e9.5_rep2[,3])
e9.5_rep2 <- e9.5_rep2[-(1850:1870), , drop = FALSE]
e9.5_rep2 <- e9.5_rep2[-(1:20), , drop = FALSE]

# Define the scatterplot_genes function

scatterplot_genes <- function(my_data, condition_lab_a, condition_lab_b, plot_title, threshold, output) {  
  # condition_lab_a - label for 1st condition
  # condition_lab_b - label for 2nd condition
  # threshold - above which genes to be coloured in red
  # output - directory where the output file is to be written
  
  print(head(my_data))
  
  file_out_0 <- paste(condition_lab_a,"_vs_",condition_lab_b,".pdf", sep="") #name of the output file
  file_out <- paste(output,"/",file_out_0, sep="") # fullpath of the output file
  
  # my_data[,2] - data with condition_lab_a
  # my_data[,3] - data with condition_lab_b
  
  dcols<-densCols(my_data[,2],my_data[,3], colramp=colorRampPalette(blues9[-(1:3)]))
  
  pdf(file=file_out, height=5,width=5,onefile=FALSE)
  
  my_data$score <- abs(my_data[,2]-my_data[,3])
  print(my_data)
  subs<-subset(my_data, score>threshold)
  print(subs)
  
  print(ggplot(my_data) +
          geom_point(aes(x=my_data[,2], y=my_data[,3], col=dcols), size=1 ) +
          scale_color_identity() +
          {if(nrow(subs)!=0) geom_point(data=subs, aes(x=subs[,2], y=subs[,3], col="red"), size=1)}+ # colour red genes above threshold
          {if(nrow(subs)!=0) geom_text_repel(data=subs, aes(x=subs[,2], y=subs[,3], label=position), colour="black",segment.size  = 0.4,segment.color = "grey50",size=5)}+ # print names of genes above threshold
          geom_abline(slope=1, intercept=0,linetype="dashed")+
          coord_obs_pred()+
          
          ggtitle(plot_title)+ 
          xlab(condition_lab_a)+
          ylab(condition_lab_b) +
          stat_cor(aes(x=my_data[,2], y=my_data[,3]),method = "spearman", label.x = 0.01, label.y = 1.2)+ # position of the R corr on the plot - it'll depend on your data and its limits, can make it a variable
          theme_bw()+     	     
          theme(axis.text.x = element_text(face="bold", color="black",size=11),
                axis.text.y = element_text(face="bold", color="black", size=11),
                plot.title = element_text(color="black", size=15, face="bold.italic",hjust = 0.5),
                axis.title.x = element_text(color="black", size=15, face="bold"),
                axis.title.y = element_text(color="black", size=15, face="bold"),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black", size=0.5), 	  
                legend.title = element_text(color = "black", size = 15,face="bold"),
                legend.text = element_text(color = "black", size=15),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  )
  dev.off()
}

# Run it on the objects created in the previous step to make .pdf plots

###BRAIN
scatterplot_genes(brain_embryo_rep1, "Median", "Embryo_Brain_Rep1", plot_title= "Median_Difference", threshold= 0.12, output= getwd())
scatterplot_genes(brain_embryo_rep2, "Median", "Embryo_Brain_Rep2", plot_title= "Median_Difference", threshold= 0.12, output= getwd())

scatterplot_genes(brain_newborn_rep1, "Median", "Newborn_Brain_Rep1", plot_title= "Median_Difference", threshold= 0.12, output= getwd())
scatterplot_genes(brain_newborn_rep2, "Median", "Newborn_Brain_Rep2", plot_title= "Median_Difference", threshold= 0.12, output= getwd())

scatterplot_genes(brain_adult_rep1, "Median", "Adult_Brain_Rep1", plot_title= "Median_Difference", threshold= 0.12, output= getwd())
scatterplot_genes(brain_adult_rep2, "Median", "Adult_Brain_Rep2", plot_title= "Median_Difference", threshold= 0.12, output= getwd())

###HEART
scatterplot_genes(heart_embryo_rep1, "Median", "Embryo_heart_Rep1", plot_title= "Median_Difference", threshold= 0.12, output= getwd())
scatterplot_genes(heart_embryo_rep2, "Median", "Embryo_heart_Rep2", plot_title= "Median_Difference", threshold= 0.12, output= getwd())

scatterplot_genes(heart_newborn_rep1, "Median", "Newborn_heart_Rep1", plot_title= "Median_Difference", threshold= 0.12, output= getwd())
scatterplot_genes(heart_newborn_rep2, "Median", "Newborn_heart_Rep2", plot_title= "Median_Difference", threshold= 0.12, output= getwd())

scatterplot_genes(heart_adult_rep1, "Median", "Adult_heart_Rep1", plot_title= "Median_Difference", threshold= 0.12, output= getwd())
scatterplot_genes(heart_adult_rep2, "Median", "Adult_heart_Rep2", plot_title= "Median_Difference", threshold= 0.12, output= getwd())

###LIVER
scatterplot_genes(liver_embryo_rep1, "Median", "Embryo_liver_Rep1", plot_title= "Median_Difference", threshold= 0.12, output= getwd())
scatterplot_genes(liver_embryo_rep2, "Median", "Embryo_liver_Rep2", plot_title= "Median_Difference", threshold= 0.12, output= getwd())

scatterplot_genes(liver_newborn_rep1, "Median", "Newborn_liver_Rep1", plot_title= "Median_Difference", threshold= 0.12, output= getwd())
scatterplot_genes(liver_newborn_rep2, "Median", "Newborn_liver_Rep2", plot_title= "Median_Difference", threshold= 0.12, output= getwd())

scatterplot_genes(liver_adult_rep1, "Median", "Adult_liver_Rep1", plot_title= "Median_Difference", threshold= 0.12, output= getwd())
scatterplot_genes(liver_adult_rep2, "Median", "Adult_liver_Rep2", plot_title= "Median_Difference", threshold= 0.12, output= getwd())

###Testis
scatterplot_genes(testis_newborn_rep1, "Median", "Newborn_testis_Rep1", plot_title= "Median_Difference", threshold= 0.12, output= getwd())
scatterplot_genes(testis_newborn_rep2, "Median", "Newborn_testis_Rep2", plot_title= "Median_Difference", threshold= 0.12, output= getwd())

scatterplot_genes(testis_adult_rep1, "Median", "Adult_testis_Rep1", plot_title= "Median_Difference", threshold= 0.12, output= getwd())
scatterplot_genes(testis_adult_rep2, "Median", "Adult_testis_Rep2", plot_title= "Median_Difference", threshold= 0.12, output= getwd())

###E9.5

scatterplot_genes(e9.5_rep1, "Median", "E9.5_Rep1", plot_title= "Median_Difference", threshold= 0.12, output= getwd())
scatterplot_genes(e9.5_rep2, "Median", "E9.5_Rep2", plot_title= "Median_Difference", threshold= 0.12, output= getwd())

# The same approach is done with 28S, just loading the file with epinano results from 28S rRNA


#### Panel D

library(corrplot)

corrplot(cor(merged_all_tissues_18S_28S),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "full",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         title = "",       # Main title
         #col.lim = c(0.92,1),
         is.corr = FALSE,
         col= NULL)       # Color palette

#### Panel E

merged_all_tissues_18S_28S_t <- t(merged_all_tissues_18S_28S)
merged_all_tissues_18S_28S_t_scaled <- scale(merged_all_tissues_18S_28S_t)

library(ComplexHeatmap)
library(circlize)

eatmap(merged_all_tissues_18S_28S_t_scaled, name = "SumErr", 
        col = colorRamp2(c(-2, -1, 0, 1, 2), c("#2c7bb6","#abd9e9","floralwhite","#fdae61", "#d7191c"),space = "RGB"),
        cluster_columns = TRUE,
        column_title = "28S rRNA", 
        column_title_gp = gpar(fontsize = 10, fontface = "bold"),
        column_names_gp = gpar(fontsize = 7, fontface = "bold"),
        row_title = "Developmental stages", row_title_rot = 90,
        row_title_gp = gpar(fontsize = 8, fontface = "bold"),
        cluster_rows = FALSE,
        show_row_names = TRUE,
        show_column_names = TRUE,
        row_names_gp = gpar(fontsize = 5), #row names size
        column_dend_side = "top",
        column_names_side = "bottom",
        row_gap = unit(0, "mm"), #Gap
)


### The script for panel G is the same as the one in Figures S8 and S9

#### Panel H     

### Summed CMC scores were calculated by summing the CMC scores of the putatively modified site + 3 downstream positions. These summed CMC scores were used to plot the barplots and perform ANOVA.

# Load required package
library(dplyr)

# Create data frames for each position
data_18S_890 <- data.frame(
  Position = "18S_890",
  Sample = c("Adult_Brain_Rep1", "Adult_Brain_Rep2", 
             "Adult_Liver_Rep1", "Adult_Liver_Rep2", 
             "Embryo_Brain_Rep1", "Embryo_Brain_Rep2"),
  Value = c(43.13154652, 47.92384331, 
            54.26018225, 53.78817298, 
            23.53568724, 19.04361242)
)

data_18S_1315 <- data.frame(
  Position = "18S_1315",
  Sample = c("Adult_Brain_Rep1", "Adult_Brain_Rep2", 
             "Adult_Liver_Rep1", "Adult_Liver_Rep2", 
             "Embryo_Brain_Rep1", "Embryo_Brain_Rep2"),
  Value = c(32.43557191, 47.85931844, 
            6.318336303, 7.143216322, 
            27.80170353, 24.78900877)
)

data_18S_1359 <- data.frame(
  Position = "18S_1359",
  Sample = c("Adult_Brain_Rep1", "Adult_Brain_Rep2", 
             "Adult_Liver_Rep1", "Adult_Liver_Rep2", 
             "Embryo_Brain_Rep1", "Embryo_Brain_Rep2"),
  Value = c(37.69811189, 59.78318272, 
            9.394711471, 9.838716524, 
            16.90282476, 22.817041)
)

data_18S_1400 <- data.frame(
  Position = "18S_1400",
  Sample = c("Adult_Brain_Rep1", "Adult_Brain_Rep2", 
             "Adult_Liver_Rep1", "Adult_Liver_Rep2", 
             "Embryo_Brain_Rep1", "Embryo_Brain_Rep2"),
  Value = c(22.7810429, 34.21670304, 
            6.207352317, 7.265853683, 
            7.424083527, 7.424083527)
)

data_28S_1500 <- data.frame(
  Position = "28S_1500",
  Sample = c("Adult_Brain_Rep1", "Adult_Brain_Rep2", 
             "Adult_Liver_Rep1", "Adult_Liver_Rep2", 
             "Embryo_Brain_Rep1", "Embryo_Brain_Rep2"),
  Value = c(33.72704788, 45.0742921, 
            17.39821009, 19.74559152, 
            18.89314021, 26.16954074)
)

# Combine all data frames into one
combined_data <- bind_rows(data_18S_890, data_18S_1315, data_18S_1359, data_18S_1400, data_28S_1500)

# Extract relevant information
combined_data$Type <- ifelse(grepl("Adult", combined_data$Sample), "Adult", "Embryo")
combined_data$Tissue <- sub("^(Adult_|Embryo_)", "", sub("_Rep[0-9]+$", "", combined_data$Sample))


# Function to perform ANOVA for a given position
perform_anova <- function(data, position) {
  cat("ANOVA for position:", position, "\n")
  anova_result <- aov(Value ~ Tissue_type, data = data)
  print(summary(anova_result))
  cat("\n")
}


combined_data2<-combined_data
combined_data2$Tissue_type<-paste(combined_data2$Tissue,combined_data2$Type, sep="")
for (pos in positions) {
  pos_data <- subset(combined_data2, Position == pos)
  perform_anova(pos_data, pos)
}           
