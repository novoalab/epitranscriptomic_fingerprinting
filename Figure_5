### Panel B

input1 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool1/epinano_flow/fast5---bc_1.plus_strand.per.site.csv')
input1 <- input1[grepl("18s", input1$X.Ref),]

input2 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool1/epinano_flow/fast5---bc_2.plus_strand.per.site.csv')
input2 <- input2[grepl("18s", input2$X.Ref),]

input3 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool1/epinano_flow/fast5---bc_3.plus_strand.per.site.csv')
input3 <- input3[grepl("18s", input3$X.Ref),]

input4 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool1/epinano_flow/fast5---bc_4.plus_strand.per.site.csv')
input4 <- input4[grepl("18s", input4$X.Ref),]

input5 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool2/epinano_flow/fast5---bc_1.plus_strand.per.site.csv')
input5 <- input5[grepl("18s", input5$X.Ref),]

input6 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool2/epinano_flow/fast5---bc_2.plus_strand.per.site.csv')
input6 <- input6[grepl("18s", input6$X.Ref),]

input7 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool2/epinano_flow/fast5---bc_3.plus_strand.per.site.csv')
input7 <- input7[grepl("18s", input7$X.Ref),]

input8 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool2/epinano_flow/fast5---bc_4.plus_strand.per.site.csv')
input8 <- input8[grepl("18s", input8$X.Ref),]


input1$sum <- input1$mis + input1$del +input1$ins
input2$sum <- input2$mis + input2$del +input2$ins
input3$sum <- input3$mis + input3$del +input3$ins
input4$sum <- input4$mis + input4$del +input4$ins
input5$sum <- input5$mis + input5$del +input5$ins
input6$sum <- input6$mis + input6$del +input6$ins
input7$sum <- input7$mis + input7$del +input7$ins
input8$sum <- input8$mis + input8$del +input8$ins

merged <- merge(input1[,c("pos", "sum")], input2 [,c("pos","sum")], by = "pos")
merged <- merge (merged, input3[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input4[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input5[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input6[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input7[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input8[,c("pos", "sum")], by = "pos")

colnames(merged) <- c("position", "Colon_Cancer", "Colon_Normal", "Lung_Cancer", "Lung_Normal", "Testis_Cancer", "Testis_Normal", "Liver_Cancer", "Liver_Normal")
rownames(merged) <- merged$position


library(tidyverse) 
library(dplyr)
library(ggplot2)
library(ggrepel) 
library(MASS)
library(reshape2)
library(ggpubr)
library(tune) # for coord_obs_pred(), which sets the scales of x & y axes to be equal
library(data.table)


merged <- merged[-(1850:1869), , drop = FALSE]
merged <- merged[-(1:20), , drop = FALSE]



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
  
  #pdf(file=file_out, height=5,width=5,onefile=FALSE)
  
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
          #stat_cor(aes(x=my_data[,2], y=my_data[,3]),method = "spearman", label.x = 0.01, label.y = 1.2)+ # position of the R corr on the plot - it'll depend on your data and its limits, can make it a variable
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
  #dev.off()
}


#Colon
Colon <- NULL
Colon$position <- rownames(merged)
Colon <- as.data.frame(Colon)
Colon$x <- merged$Colon_Cancer
Colon$y <- merged$Colon_Normal
Colon$score <- abs(Colon[,2]-Colon[,3])

#Liver
Liver <- NULL
Liver$position <- rownames(merged)
Liver <- as.data.frame(Liver)
Liver$x <- merged$Liver_Cancer
Liver$y <- merged$Liver_Normal
Liver$score <- abs(Liver[,2]-Liver[,3])

#Testis
Testis <- NULL
Testis$position <- rownames(merged)
Testis <- as.data.frame(Testis)
Testis$x <- merged$Testis_Cancer
Testis$y <- merged$Testis_Normal
Testis$score <- abs(Testis[,2]-Testis[,3])

#Lung
Lung <- NULL
Lung$position <- rownames(merged)
Lung <- as.data.frame(Lung)
Lung$x <- merged$Lung_Cancer
Lung$y <- merged$Lung_Normal
Lung$score <- abs(Lung[,2]-Lung[,3])


pdf("colon_cancer_vs_normal_18S_15062023.pdf",height=8, width=12)
scatterplot_genes(Colon, "Colon_Cancer", "Colon_Normal", plot_title= "Colon", threshold= 0.08, output= getwd())
dev.off()

pdf("liver_cancer_vs_normal_18S_15062023.pdf",height=8, width=12)
scatterplot_genes(Liver, "Liver_Cancer", "Liver_Normal", plot_title= "Liver", threshold= 0.10, output= getwd())
dev.off()

pdf("lung_cancer_vs_normal_18S_15062023.pdf",height=8, width=12)
scatterplot_genes(Lung, "Lung_Cancer", "Lung_Normal", plot_title= "Lung", threshold= 0.13, output= getwd())
dev.off()

pdf("testis_cancer_vs_normal_18S_15062023.pdf",height=8, width=12)
scatterplot_genes(Testis, "Testis_Cancer", "Testis_Normal", plot_title= "Testis", threshold= 0.10, output= getwd())
dev.off()



############################ 28S ###################################

input1 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool1/epinano_flow/fast5---bc_1.plus_strand.per.site.csv')
input1 <- input1[grepl("28s", input1$X.Ref),]

input2 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool1/epinano_flow/fast5---bc_2.plus_strand.per.site.csv')
input2 <- input2[grepl("28s", input2$X.Ref),]

input3 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool1/epinano_flow/fast5---bc_3.plus_strand.per.site.csv')
input3 <- input3[grepl("28s", input3$X.Ref),]

input4 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool1/epinano_flow/fast5---bc_4.plus_strand.per.site.csv')
input4 <- input4[grepl("28s", input4$X.Ref),]

input5 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool2/epinano_flow/fast5---bc_1.plus_strand.per.site.csv')
input5 <- input5[grepl("28s", input5$X.Ref),]

input6 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool2/epinano_flow/fast5---bc_2.plus_strand.per.site.csv')
input6 <- input6[grepl("28s", input6$X.Ref),]

input7 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool2/epinano_flow/fast5---bc_3.plus_strand.per.site.csv')
input7 <- input7[grepl("28s", input7$X.Ref),]

input8 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool2/epinano_flow/fast5---bc_4.plus_strand.per.site.csv')
input8 <- input8[grepl("28s", input8$X.Ref),]


input1$sum <- input1$mis + input1$del +input1$ins
input2$sum <- input2$mis + input2$del +input2$ins
input3$sum <- input3$mis + input3$del +input3$ins
input4$sum <- input4$mis + input4$del +input4$ins
input5$sum <- input5$mis + input5$del +input5$ins
input6$sum <- input6$mis + input6$del +input6$ins
input7$sum <- input7$mis + input7$del +input7$ins
input8$sum <- input8$mis + input8$del +input8$ins

merged <- merge(input1[,c("pos", "sum")], input2 [,c("pos","sum")], by = "pos")
merged <- merge (merged, input3[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input4[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input5[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input6[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input7[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input8[,c("pos", "sum")], by = "pos")

colnames(merged) <- c("position", "Colon_Cancer", "Colon_Normal", "Lung_Cancer", "Lung_Normal", "Testis_Cancer", "Testis_Normal", "Liver_Cancer", "Liver_Normal")
rownames(merged) <- merged$position


library(tidyverse) 
library(dplyr)
library(ggplot2)
library(ggrepel) 
library(MASS)
library(reshape2)
library(ggpubr)
library(tune) # for coord_obs_pred(), which sets the scales of x & y axes to be equal
library(data.table)


merged <- merged[-(5040:5070), , drop = FALSE]
merged <- merged[-(1:20), , drop = FALSE]



#Colon
Colon <- NULL
Colon$position <- rownames(merged)
Colon <- as.data.frame(Colon)
Colon$x <- merged$Colon_Cancer
Colon$y <- merged$Colon_Normal
Colon$score <- abs(Colon[,2]-Colon[,3])

#Liver
Liver <- NULL
Liver$position <- rownames(merged)
Liver <- as.data.frame(Liver)
Liver$x <- merged$Liver_Cancer
Liver$y <- merged$Liver_Normal
Liver$score <- abs(Liver[,2]-Liver[,3])

#Testis
Testis <- NULL
Testis$position <- rownames(merged)
Testis <- as.data.frame(Testis)
Testis$x <- merged$Testis_Cancer
Testis$y <- merged$Testis_Normal
Testis$score <- abs(Testis[,2]-Testis[,3])

#Lung
Lung <- NULL
Lung$position <- rownames(merged)
Lung <- as.data.frame(Lung)
Lung$x <- merged$Lung_Cancer
Lung$y <- merged$Lung_Normal
Lung$score <- abs(Lung[,2]-Lung[,3])


pdf("colon_cancer_vs_normal_28S_15062023.pdf",height=8, width=12)
scatterplot_genes(Colon, "Colon_Cancer", "Colon_Normal", plot_title= "Colon", threshold= 0.10, output= getwd())
dev.off()

pdf("liver_cancer_vs_normal_28S_15062023.pdf",height=8, width=12)
scatterplot_genes(Liver, "Liver_Cancer", "Liver_Normal", plot_title= "Liver", threshold= 0.10, output= getwd())
dev.off()

pdf("lung_cancer_vs_normal_28S_15062023.pdf",height=8, width=12)
scatterplot_genes(Lung, "Lung_Cancer", "Lung_Normal", plot_title= "Lung", threshold= 0.13, output= getwd())
dev.off()

pdf("testis_cancer_vs_normal_28S_15062023.pdf",height=8, width=12)
scatterplot_genes(Testis, "Testis_Cancer", "Testis_Normal", plot_title= "Testis", threshold= 0.10, output= getwd())
dev.off()


### Panel E

###### PCA with top dynamic sites in 18S and 28S

input1 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool1/epinano_flow/fast5---bc_1.plus_strand.per.site.csv')
input1$pos <- paste(input1$X.Ref, input1$pos, sep = ":")

input2 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool1/epinano_flow/fast5---bc_2.plus_strand.per.site.csv')
input2$pos <- paste(input2$X.Ref, input2$pos, sep = ":")

input3 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool1/epinano_flow/fast5---bc_3.plus_strand.per.site.csv')
input3$pos <- paste(input3$X.Ref, input3$pos, sep = ":")

input4 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool1/epinano_flow/fast5---bc_4.plus_strand.per.site.csv')
input4$pos <- paste(input4$X.Ref, input4$pos, sep = ":")

input5 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool2/epinano_flow/fast5---bc_1.plus_strand.per.site.csv')
input5$pos <- paste(input5$X.Ref, input5$pos, sep = ":")

input6 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool2/epinano_flow/fast5---bc_2.plus_strand.per.site.csv')
input6$pos <- paste(input6$X.Ref, input6$pos, sep = ":")

input7 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool2/epinano_flow/fast5---bc_3.plus_strand.per.site.csv')
input7$pos <- paste(input7$X.Ref, input7$pos, sep = ":")

input8 <- read.csv('/no_backup_isis/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/mop_mod/RNA090623_Pool2/epinano_flow/fast5---bc_4.plus_strand.per.site.csv')
input8$pos <- paste(input8$X.Ref, input8$pos, sep = ":")


input1$sum <- input1$mis + input1$del +input1$ins
input2$sum <- input2$mis + input2$del +input2$ins
input3$sum <- input3$mis + input3$del +input3$ins
input4$sum <- input4$mis + input4$del +input4$ins
input5$sum <- input5$mis + input5$del +input5$ins
input6$sum <- input6$mis + input6$del +input6$ins
input7$sum <- input7$mis + input7$del +input7$ins
input8$sum <- input8$mis + input8$del +input8$ins

merged <- merge(input1[,c("pos", "sum")], input2 [,c("pos","sum")], by = "pos")
merged <- merge (merged, input3[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input4[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input5[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input6[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input7[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input8[,c("pos", "sum")], by = "pos")

colnames(merged) <- c("position", "Colon_Cancer", "Colon_Normal", "Lung_Cancer", "Lung_Normal", "Testis_Cancer", "Testis_Normal", "Liver_Cancer", "Liver_Normal")
rownames(merged) <- merged$position

# Define the positions to be removed
positions_to_remove <- c("18s:1", "18s:2", "18s:3", "18s:4", "18s:5", "18s:6", "18s:7", "18s:8", "18s:9", "18s:10",
                         "18s:1850", "18s:1851", "18s:1852", "18s:1853", "18s:1854", "18s:1855", "18s:1856", "18s:1857", "18s:1858", "18s:1859",
                         "28s:1", "28s:2", "28s:3", "28s:4", "28s:5", "28s:6", "28s:7", "28s:8", "28s:9", "28s:10",
                         "28s:5050", "28s:5051", "28s:5052", "28s:5053", "28s:5054", "28s:5055", "28s:5056", "28s:5057", "28s:5058", "28s:5059")

# Remove the specified rows
merged <- merged[!(merged$position %in% positions_to_remove), ]

# Define the strings to be excluded
strings_to_exclude <- c("5s", "5.8s", "mt-12s", "mt-16s")

# Filter the dataframe to exclude rows containing the specified strings
merged <- merged[!grepl(paste(strings_to_exclude, collapse = "|"), merged$position), ]





# Calculate the average values for the Cancer and Normal groups for each row
cancer_avg <- rowMeans(merged[, c("Colon_Cancer", "Lung_Cancer", "Testis_Cancer", "Liver_Cancer")])
normal_avg <- rowMeans(merged[, c("Colon_Normal", "Lung_Normal", "Testis_Normal", "Liver_Normal")])

# Calculate the absolute difference of averages between Cancer and Normal
diff_averages <- abs(cancer_avg - normal_avg)

# Create a data frame of discovered positions
discovered_positions <- data.frame(position = merged$position)

# Sort the data frame by the absolute difference of averages
discovered_positions <- discovered_positions[order(diff_averages, decreasing = TRUE), , drop = FALSE]

# Print the top 20 positions with the largest absolute difference of averages
top_10_positions <- discovered_positions[1:10, , drop = FALSE]
print(top_10_positions)



merged2 <- merge(top_10_positions, merged, by="position")
row.names(merged2) <- merged2$position
merged2$position <- NULL

# Replace ":" with "_" in the row names of merged2
rownames(merged2) <- gsub(":", "_", rownames(merged2))


merged2_t <- t(merged2)




######### PCA ###########



using_this_here <- merged2_t ### CHANGE THIS BASED ON WHICH SAMPLE YOU'RE USING



df <- data.matrix(using_this_here)
df <- scale(df, center=FALSE)
data<- using_this_here


data_pca<-prcomp(df,center=TRUE) #PCA 
data_out <- as.data.frame(data_pca$x) #X table of PCA
data_out$Symbol <- sub("^[^_]+_(.*)", "\\1", rownames(data_out))
data_out$tissue <- sub("^([^_]+)_.*", "\\1", rownames(data_out))



##Calculation for percentage of variance explained by each component
eigs <- data_pca$sdev^2#Calculate percentage for PC values
percentage<- round(eigs/sum(eigs)*100,2)#Calculate percentage for PC values
percentage <- paste( colnames(data_out), "(", paste( as.character(percentage), "%", ")", sep="") ) #Calculate percentage for PC values

library(ggplot2)
library(grid)
library(gridExtra)
library(ggrepel)
library(ggforce)


print(ggplot(data_out,aes(x=PC1,y=PC2,label=tissue, color=Symbol))+
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2)+
        geom_point(alpha = 0.8, size = 3) +
        #geom_text_repel(show.legend = FALSE, aes(label=Symbol),hjust=-1, vjust=0)+
        theme(panel.background = element_blank(),
              panel.border=element_rect(fill=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background=element_blank(),
              axis.text.x=element_text(colour="black"),
              axis.text.y=element_text(colour="black"),
              axis.ticks=element_line(colour="black"),
              plot.margin=unit(c(1,1,1,1),"line"))+
        xlab(percentage[1]) + ylab(percentage[2]) #Labels containing percentages 
)
dev.off()


# plot the data with different shapes for each tissue


##PLOT FOR LOADINGS ALL LABELS
data_out_r <- as.data.frame(data_pca$rotation) #rotation data (loadings)
data_out_r$Symbol <- row.names(data_out_r) 
data_out_r$mod_type <- c("Ψ", "Ψ", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Nm", "Nm", "Unannotated", "Unannotated")


print(ggplot(data_out_r,aes(x=PC1,y=PC2,label=Symbol, color=mod_type))+
        geom_point()+
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2)+
        geom_point(alpha = 0.8, size = 3)+
        geom_text_repel(show.legend = FALSE, aes(label=Symbol),hjust=-1, vjust=0)+
        #xlim(-1.5,0.7)+
        #ylim(-0.5,0.5)+
        theme(aspect.ratio = 1,
              panel.background = element_blank(),
              panel.border=element_rect(fill=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background=element_blank(),
              axis.text.x=element_text(colour="black"),
              axis.text.y=element_text(colour="black"),
              axis.ticks=element_line(colour="black"),
              plot.margin=unit(c(1,1,1,1),"line"))+
        xlab(percentage[1]) + ylab(percentage[2])
)
dev.off()

