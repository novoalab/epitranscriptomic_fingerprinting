### The .csv epinano output files can be found in the Figure_2/epinano_outputs folder.

input1 <- read.csv('brain_embryo_rep1.csv')
input1 <- input1[grepl("18S", input1$X.Ref),]

input2 <- read.csv('brain_embryo_rep2.csv')
input2 <- input2[grepl("18S", input2$X.Ref),]

input3 <- read.csv('brain_newborn_rep1.csv')
input3 <- input3[grepl("18S", input3$X.Ref),]

input4 <- read.csv('brain_newborn_rep2.csv')
input4 <- input4[grepl("18S", input4$X.Ref),]

input5 <- read.csv('brain_adult_rep1.csv')
input5 <- input5[grepl("18S", input5$X.Ref),]

input6 <- read.csv('brain_adult_rep2.csv')
input6 <- input6[grepl("18S", input6$X.Ref),]

input7 <- read.csv('mESc_rep1.csv')
input7 <- input7[grepl("18S", input7$X.Ref),]

input8 <- read.csv('mESc_rep2.csv')
input8 <- input8[grepl("18S", input8$X.Ref),]

input9 <- read.csv('NPC_rep1.csv')
input9 <- input9[grepl("18S", input9$X.Ref),]

input10 <- read.csv('NPC_rep2.csv')
input10 <- input10[grepl("18S", input10$X.Ref),]

input11 <- read.csv('Neurons_rep1.csv')
input11 <- input11[grepl("18S", input11$X.Ref),]

input12 <- read.csv('Neurons_rep2.csv')
input12 <- input12[grepl("18S", input12$X.Ref),]


input1$sum <- input1$mis + input1$del +input1$ins
input2$sum <- input2$mis + input2$del +input2$ins
input3$sum <- input3$mis + input3$del +input3$ins
input4$sum <- input4$mis + input4$del +input4$ins
input5$sum <- input5$mis + input5$del +input5$ins
input6$sum <- input6$mis + input6$del +input6$ins
input7$sum <- input7$mis + input7$del +input7$ins
input8$sum <- input8$mis + input8$del +input8$ins
input9$sum <- input9$mis + input9$del +input9$ins
input10$sum <- input10$mis + input10$del +input10$ins
input11$sum <- input11$mis + input11$del +input11$ins
input12$sum <- input12$mis + input12$del +input12$ins


merged <- merge(input1[,c("pos", "sum")], input2 [,c("pos","sum")], by = "pos")
merged <- merge (merged, input3[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input4[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input5[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input6[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input7[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input8[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input9[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input10[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input11[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input12[,c("pos", "sum")], by = "pos")

colnames(merged) <- c("position", "brain_embryo_rep1", "brain_embryo_rep2", "brain_newborn_rep1", "brain_newborn_rep2", "brain_adult_rep1", "brain_adult_rep2", "mESc_rep1", "mESc_rep2", "NPC_rep1", "NPC_rep2", "Neurons_rep1", "Neurons_rep2")
rownames(merged) <- merged$position


# Assuming 'merged' is your dataframe
merged$median <- apply(merged[, 2:13], 1, median)


#mESc_vs_median
mESc_vs_median <- NULL
mESc_vs_median$position <- rownames(merged)
mESc_vs_median <- as.data.frame(mESc_vs_median)
mESc_vs_median$x <- merged$median
mESc_vs_median$y <- merged$mESc_rep1
mESc_vs_median$score <- abs(mESc_vs_median[,2]-mESc_vs_median[,3])
mESc_vs_median <- mESc_vs_median[-(1850:1870), , drop = FALSE]
mESc_vs_median <- mESc_vs_median[-(1:20), , drop = FALSE]


#Neurons_vs_median
neurons_vs_median <- NULL
neurons_vs_median$position <- rownames(merged)
neurons_vs_median <- as.data.frame(neurons_vs_median)
neurons_vs_median$x <- merged$median
neurons_vs_median$y <- merged$Neurons_rep1
neurons_vs_median$score <- abs(neurons_vs_median[,2]-neurons_vs_median[,3])
neurons_vs_median <- neurons_vs_median[-(1850:1870), , drop = FALSE]
neurons_vs_median <- neurons_vs_median[-(1:20), , drop = FALSE]





#Libraries

library(tidyverse) 
library(dplyr)
library(ggplot2)
library(ggrepel) 
library(MASS)
library(reshape2)
library(ggpubr)
library(tune) # for coord_obs_pred(), which sets the scales of x & y axes to be equal
library(data.table)

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


scatterplot_genes(mESc_vs_median, "median", "mESc_rep1", plot_title= "mESc_vs_median", threshold= 0.12, output= getwd())
scatterplot_genes(neurons_vs_median, "median", "Neurons_rep1", plot_title= "mESc_vs_median", threshold= 0.12, output= getwd())






######### SAME, FOR 28S

input1 <- read.csv('brain_embryo_rep1.csv')
input1 <- input1[grepl("28S", input1$X.Ref),]

input2 <- read.csv('brain_embryo_rep2.csv')
input2 <- input2[grepl("28S", input2$X.Ref),]

input3 <- read.csv('brain_newborn_rep1.csv')
input3 <- input3[grepl("28S", input3$X.Ref),]

input4 <- read.csv('brain_newborn_rep2.csv')
input4 <- input4[grepl("28S", input4$X.Ref),]

input5 <- read.csv('brain_adult_rep1.csv')
input5 <- input5[grepl("28S", input5$X.Ref),]

input6 <- read.csv('brain_adult_rep2.csv')
input6 <- input6[grepl("28S", input6$X.Ref),]

input7 <- read.csv('mESc_rep1.csv')
input7 <- input7[grepl("28S", input7$X.Ref),]

input8 <- read.csv('mESc_rep2.csv')
input8 <- input8[grepl("28S", input8$X.Ref),]

input9 <- read.csv('NPC_rep1.csv')
input9 <- input9[grepl("28S", input9$X.Ref),]

input10 <- read.csv('NPC_rep2.csv')
input10 <- input10[grepl("28S", input10$X.Ref),]

input11 <- read.csv('Neurons_rep1.csv')
input11 <- input11[grepl("28S", input11$X.Ref),]

input12 <- read.csv('Neurons_rep2.csv')
input12 <- input12[grepl("28S", input12$X.Ref),]


input1$sum <- input1$mis + input1$del +input1$ins
input2$sum <- input2$mis + input2$del +input2$ins
input3$sum <- input3$mis + input3$del +input3$ins
input4$sum <- input4$mis + input4$del +input4$ins
input5$sum <- input5$mis + input5$del +input5$ins
input6$sum <- input6$mis + input6$del +input6$ins
input7$sum <- input7$mis + input7$del +input7$ins
input8$sum <- input8$mis + input8$del +input8$ins
input9$sum <- input9$mis + input9$del +input9$ins
input10$sum <- input10$mis + input10$del +input10$ins
input11$sum <- input11$mis + input11$del +input11$ins
input12$sum <- input12$mis + input12$del +input12$ins


merged <- merge(input1[,c("pos", "sum")], input2 [,c("pos","sum")], by = "pos")
merged <- merge (merged, input3[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input4[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input5[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input6[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input7[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input8[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input9[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input10[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input11[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input12[,c("pos", "sum")], by = "pos")

colnames(merged) <- c("position", "brain_embryo_rep1", "brain_embryo_rep2", "brain_newborn_rep1", "brain_newborn_rep2", "brain_adult_rep1", "brain_adult_rep2", "mESc_rep1", "mESc_rep2", "NPC_rep1", "NPC_rep2", "Neurons_rep1", "Neurons_rep2")
rownames(merged) <- merged$position


# Assuming 'merged' is your dataframe
merged$median <- apply(merged[, 2:13], 1, median)

# Print the updated dataframe
print(merged)



#mESc_vs_median
mESc_vs_median <- NULL
mESc_vs_median$position <- rownames(merged)
mESc_vs_median <- as.data.frame(mESc_vs_median)
mESc_vs_median$x <- merged$median
mESc_vs_median$y <- merged$mESc_rep1
mESc_vs_median$score <- abs(mESc_vs_median[,2]-mESc_vs_median[,3])
mESc_vs_median <- mESc_vs_median[-(4710:4730), , drop = FALSE]
mESc_vs_median <- mESc_vs_median[-(1:20), , drop = FALSE]


#Neurons_vs_median
neurons_vs_median <- NULL
neurons_vs_median$position <- rownames(merged)
neurons_vs_median <- as.data.frame(neurons_vs_median)
neurons_vs_median$x <- merged$median
neurons_vs_median$y <- merged$Neurons_rep1
neurons_vs_median$score <- abs(neurons_vs_median[,2]-neurons_vs_median[,3])
neurons_vs_median <- neurons_vs_median[-(4710:4730), , drop = FALSE]
neurons_vs_median <- neurons_vs_median[-(1:20), , drop = FALSE]





#Libraries

library(tidyverse) 
library(dplyr)
library(ggplot2)
library(ggrepel) 
library(MASS)
library(reshape2)
library(ggpubr)
library(tune) # for coord_obs_pred(), which sets the scales of x & y axes to be equal
library(data.table)

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


scatterplot_genes(mESc_vs_median, "median", "mESc_rep1", plot_title= "mESc_vs_median", threshold= 0.12, output= getwd())
scatterplot_genes(neurons_vs_median, "median", "Neurons_rep1", plot_title= "mESc_vs_median", threshold= 0.12, output= getwd())
