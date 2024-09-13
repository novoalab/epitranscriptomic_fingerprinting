### The .csv epinano outputs can be found in the folder Figure_S14/epinano_outputs. Barcode 1 = WT mESC, barcode 2 = WT neurons, barcode 3 = Snord90 KO mESC, barcode 4 = Snord90 KO neurons.


input1 <- read.csv('fast5---bc_1.plus_strand.per.site.csv')
input1 <- input1[grepl("18S", input1$X.Ref),]

input2 <- read.csv('fast5---bc_4.plus_strand.per.site.csv')
input2 <- input2[grepl("18S", input2$X.Ref),]

input3 <- read.csv('fast5---bc_2.plus_strand.per.site.csv')
input3 <- input3[grepl("18S", input3$X.Ref),]

input4 <- read.csv('fast5---bc_3.plus_strand.per.site.csv')
input4 <- input4[grepl("18S", input4$X.Ref),]

input1$sum <- input1$mis + input1$del +input1$ins
input2$sum <- input2$mis + input2$del +input2$ins
input3$sum <- input3$mis + input3$del +input3$ins
input4$sum <- input4$mis + input4$del +input4$ins


library(plyr)
library(ggplot2)
library(ggrepel)
library(MASS)
library(reshape2)
library(scales)

#Cleanup
cleanup <- function(input, label) {
  #Filter low coverage reads
  input <- subset(input, cov>30)
  #Filter read starts
  input <- subset(input, pos>20)
  #For Stress
  #Add a column with position
  input$position<- paste(input$X.Ref,input$pos)
  #Change column names
  input <- input[, c("X.Ref","pos","position", feature)]
  colnames(input)<- c("Chr","Position","chr_pos",feature )
  data_melted<- melt(data = input, id.vars = c("Chr", "Position", "chr_pos"))
  colnames(data_melted)[which(names(data_melted) == "value")] <- paste(label, "value", sep="_")
  return(data_melted)
}

###ANALYSIS FOR REPLICATE 1 

label1 <- as.character("WT")  #1st label
label2 <- as.character("KO")  #1st label


#Cleanup and process the data
data1 <- cleanup(input1, label1)
data2 <- cleanup(input3, label2)

data1$WT_value <- rescale(data1$WT_value, to = c(0, 1))
data2$KO_value <- rescale(data2$KO_value, to = c(0, 1))

merged <- merge(data1,data2[, c(3,5)], by="chr_pos")
merged$chr_pos <- paste(merged$Chr, merged$Position, sep = ":")
merged$Chr <- NULL
merged$Position <- NULL
merged$base <- NULL
merged$variable <- NULL


merged$score<- abs(merged[,c(paste(label1, "value", sep="_"))] - merged[,c(paste(label2, "value", sep="_"))])
merged <- separate(merged, col = chr_pos, into =c("chr", "pos"), sep=":", remove=F)
merged$pos <- as.numeric(merged$pos)
#merged <- merged[merged$pos > 50,] 

merged1 <- merged


subs <- merged



subs$score<- abs(subs$WT_value - subs$KO_value)
pos1 <- position_jitter(width = 0, seed = 0)




print(ggplot(subs, aes(x=pos, y=score)) +
        geom_bar(stat = "identity", width=1, fill="deepskyblue4") +
        geom_bar(data=subs[subs$score > 10*median(subs$score),], aes(x=pos, y=score),stat = "identity", width=1, fill="tan1")+
        geom_bar(data=subs[subs$score > 20*median(subs$score),], aes(x=pos, y=score),stat = "identity", width=1, fill="tomato")+
        ggtitle(paste("summed_errors_per_5mer", label1, label2, sep="_"))+
        xlab("Position")+
        ylab("Delta summed errors") +
        ylim(0, 1)+ ## here specify a subset of coordinates of the transcript if you need to
        theme_bw()+
        theme(axis.text.x = element_text(face="bold", color="black",size=20),
              axis.text.y = element_text(face="bold", color="black", size=20),
              plot.title = element_text(color="black", size=24, face="bold",hjust = 0.5),
              axis.title.x = element_text(color="black", size=20, face="bold"),
              axis.title.y = element_text(color="black", size=20, face="bold"),
              panel.background = element_blank(),
              legend.position = "none",
              axis.line = element_line(colour = "black", size=0.5)))



### Neurons


label1 <- as.character("WT")  #1st label
label2 <- as.character("KO")  #1st label


#Cleanup and process the data
data1 <- cleanup(input2, label1)
data2 <- cleanup(input4, label2)

data1$WT_value <- rescale(data1$WT_value, to = c(0, 1))
data2$KO_value <- rescale(data2$KO_value, to = c(0, 1))

merged <- merge(data1,data2[, c(3,5)], by="chr_pos")
merged$chr_pos <- paste(merged$Chr, merged$Position, sep = ":")
merged$Chr <- NULL
merged$Position <- NULL
merged$base <- NULL
merged$variable <- NULL


merged$score<- abs(merged[,c(paste(label1, "value", sep="_"))] - merged[,c(paste(label2, "value", sep="_"))])
merged <- separate(merged, col = chr_pos, into =c("chr", "pos"), sep=":", remove=F)
merged$pos <- as.numeric(merged$pos)
#merged <- merged[merged$pos > 50,] 

merged1 <- merged


subs <- merged


subs$score<- abs(subs$WT_value - subs$KO_value)
pos1 <- position_jitter(width = 0, seed = 0)




print(ggplot(subs, aes(x=pos, y=score)) +
        geom_bar(stat = "identity", width=1, fill="deepskyblue4") +
        geom_bar(data=subs[subs$score > 10*median(subs$score),], aes(x=pos, y=score),stat = "identity", width=1, fill="tan1")+
        geom_bar(data=subs[subs$score > 20*median(subs$score),], aes(x=pos, y=score),stat = "identity", width=1, fill="tomato")+
        ggtitle(paste("summed_errors_per_5mer", label1, label2, sep="_"))+
        xlab("Position")+
        ylab("Delta summed errors") +
        ylim(0, 1)+ ## here specify a subset of coordinates of the transcript if you need to
        theme_bw()+
        theme(axis.text.x = element_text(face="bold", color="black",size=20),
              axis.text.y = element_text(face="bold", color="black", size=20),
              plot.title = element_text(color="black", size=24, face="bold",hjust = 0.5),
              axis.title.x = element_text(color="black", size=20, face="bold"),
              axis.title.y = element_text(color="black", size=20, face="bold"),
              panel.background = element_blank(),
              legend.position = "none",
              axis.line = element_line(colour = "black", size=0.5)))



#### 28S

input1 <- read.csv('fast5---bc_1.plus_strand.per.site.csv')
input1 <- input1[grepl("28S", input1$X.Ref),]

input2 <- read.csv('fast5---bc_4.plus_strand.per.site.csv')
input2 <- input2[grepl("28S", input2$X.Ref),]

input3 <- read.csv('fast5---bc_2.plus_strand.per.site.csv')
input3 <- input3[grepl("28S", input3$X.Ref),]

input4 <- read.csv('fast5---bc_3.plus_strand.per.site.csv')
input4 <- input4[grepl("28S", input4$X.Ref),]

input1$sum <- input1$mis + input1$del +input1$ins
input2$sum <- input2$mis + input2$del +input2$ins
input3$sum <- input3$mis + input3$del +input3$ins
input4$sum <- input4$mis + input4$del +input4$ins


library(plyr)
library(ggplot2)
library(ggrepel)
library(MASS)
library(reshape2)
library(scales)

#Cleanup
cleanup <- function(input, label) {
  #Filter low coverage reads
  input <- subset(input, cov>30)
  #Filter read starts
  input <- subset(input, pos>20)
  #For Stress
  #Add a column with position
  input$position<- paste(input$X.Ref,input$pos)
  #Change column names
  input <- input[, c("X.Ref","pos","position", feature)]
  colnames(input)<- c("Chr","Position","chr_pos",feature )
  data_melted<- melt(data = input, id.vars = c("Chr", "Position", "chr_pos"))
  colnames(data_melted)[which(names(data_melted) == "value")] <- paste(label, "value", sep="_")
  return(data_melted)
}

###ANALYSIS FOR REPLICATE 1 

label1 <- as.character("WT")  #1st label
label2 <- as.character("KO")  #1st label


#Cleanup and process the data
data1 <- cleanup(input1, label1)
data2 <- cleanup(input3, label2)

data1$WT_value <- rescale(data1$WT_value, to = c(0, 1))
data2$KO_value <- rescale(data2$KO_value, to = c(0, 1))

merged <- merge(data1,data2[, c(3,5)], by="chr_pos")
merged$chr_pos <- paste(merged$Chr, merged$Position, sep = ":")
merged$Chr <- NULL
merged$Position <- NULL
merged$base <- NULL
merged$variable <- NULL


merged$score<- abs(merged[,c(paste(label1, "value", sep="_"))] - merged[,c(paste(label2, "value", sep="_"))])
merged <- separate(merged, col = chr_pos, into =c("chr", "pos"), sep=":", remove=F)
merged$pos <- as.numeric(merged$pos)


merged1 <- merged


subs <- merged

##barplot
######

subs$score<- abs(subs$WT_value - subs$KO_value)
pos1 <- position_jitter(width = 0, seed = 0)



print(ggplot(subs, aes(x=pos, y=score)) +
        geom_bar(stat = "identity", width=1, fill="deepskyblue4") +
        geom_bar(data=subs[subs$score > 10*median(subs$score),], aes(x=pos, y=score),stat = "identity", width=1, fill="tan1")+
        geom_bar(data=subs[subs$score > 20*median(subs$score),], aes(x=pos, y=score),stat = "identity", width=1, fill="tomato")+
        ggtitle(paste("summed_errors_per_5mer", label1, label2, sep="_"))+
        xlab("Position")+
        ylab("Delta summed errors") +
        ylim(0, 1)+ ## here specify a subset of coordinates of the transcript if you need to
        theme_bw()+
        theme(axis.text.x = element_text(face="bold", color="black",size=20),
              axis.text.y = element_text(face="bold", color="black", size=20),
              plot.title = element_text(color="black", size=24, face="bold",hjust = 0.5),
              axis.title.x = element_text(color="black", size=20, face="bold"),
              axis.title.y = element_text(color="black", size=20, face="bold"),
              panel.background = element_blank(),
              legend.position = "none",
              axis.line = element_line(colour = "black", size=0.5)))
dev.off()


### Neurons


label1 <- as.character("WT")  #1st label
label2 <- as.character("KO")  #1st label


#Cleanup and process the data
data1 <- cleanup(input2, label1)
data2 <- cleanup(input4, label2)

data1$WT_value <- rescale(data1$WT_value, to = c(0, 1))
data2$KO_value <- rescale(data2$KO_value, to = c(0, 1))

merged <- merge(data1,data2[, c(3,5)], by="chr_pos")
merged$chr_pos <- paste(merged$Chr, merged$Position, sep = ":")
merged$Chr <- NULL
merged$Position <- NULL
merged$base <- NULL
merged$variable <- NULL


merged$score<- abs(merged[,c(paste(label1, "value", sep="_"))] - merged[,c(paste(label2, "value", sep="_"))])
merged <- separate(merged, col = chr_pos, into =c("chr", "pos"), sep=":", remove=F)
merged$pos <- as.numeric(merged$pos)
 

merged1 <- merged


subs <- merged


subs$score<- abs(subs$WT_value - subs$KO_value)
pos1 <- position_jitter(width = 0, seed = 0)


print(ggplot(subs, aes(x=pos, y=score)) +
        geom_bar(stat = "identity", width=1, fill="deepskyblue4") +
        geom_bar(data=subs[subs$score > 10*median(subs$score),], aes(x=pos, y=score),stat = "identity", width=1, fill="tan1")+
        geom_bar(data=subs[subs$score > 20*median(subs$score),], aes(x=pos, y=score),stat = "identity", width=1, fill="tomato")+
        ggtitle(paste("summed_errors_per_5mer", label1, label2, sep="_"))+
        xlab("Position")+
        ylab("Delta summed errors") +
        ylim(0, 1)+ ## here specify a subset of coordinates of the transcript if you need to
        theme_bw()+
        theme(axis.text.x = element_text(face="bold", color="black",size=20),
              axis.text.y = element_text(face="bold", color="black", size=20),
              plot.title = element_text(color="black", size=24, face="bold",hjust = 0.5),
              axis.title.x = element_text(color="black", size=20, face="bold"),
              axis.title.y = element_text(color="black", size=20, face="bold"),
              panel.background = element_blank(),
              legend.position = "none",
              axis.line = element_line(colour = "black", size=0.5)))

