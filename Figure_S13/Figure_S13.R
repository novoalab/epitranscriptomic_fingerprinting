### Starting with the object merged_all_tissues_18S_28S created in the script Figure_1.R

using_this_here <- t(merged_all_tissues_18S_28S )

rownames(using_this_here) <- gsub("\\.csv", "", rownames(using_this_here))


df <- data.matrix(using_this_here)
df <- scale(df, center=FALSE)

data_pca<-prcomp(df,center=TRUE) #PCA 
data_out <- as.data.frame(data_pca$x) #X table of PCA

# Rename row names "e9.5_rep1" to "e9.5_embryo_rep1"
rownames(data_out)[rownames(data_out) == "e.9.5_rep1"] <- "e.9.5_embryo_rep1"

# Rename row names "e9.5_rep2" to "e9.5_embryo_rep2"
rownames(data_out)[rownames(data_out) == "e.9.5_rep2"] <- "e.9.5_embryo_rep2"

data_out$Symbol<- rownames(data_out) ### THIS NUMBER HAS TO BE CHANGED DEPENDING ON SAMPLE

data_out$stage <- sapply(strsplit(data_out$Symbol, "_"), function(x) x[2])
data_out$tissue <- sub("^([^_]+)_.*", "\\1", rownames(data_out))

# Calculation for percentage of variance explained by each component
eigs <- data_pca$sdev^2#Calculate percentage for PC values
percentage<- round(eigs/sum(eigs)*100,2)#Calculate percentage for PC values
percentage <- paste( colnames(data_out), "(", paste( as.character(percentage), "%", ")", sep="") ) #Calculate percentage for PC values


# Define the mapping between tissues and shapes
tissue_shapes <- c("brain" = "circle", 
                     "e9.5" = "triangle", 
                     "heart" = "square", 
                     "liver" = "cross", 
                     "testis" = "box")

# add a new column to data_out that maps tissues to shapes
data_out$shape <- tissue_shapes[as.character(data_out$tissue)]
data_out$shape <- factor(data_out$shape)


print(ggplot(data_out,aes(x=PC1,y=PC2,label=Symbol, color=stage, shape = tissue))+
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2)+
        geom_point(alpha = 0.8, size = 3) +
        #geom_text_repel(show.legend = FALSE, aes(label=Symbol),hjust=-1, vjust=0)+
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
        xlab(percentage[1]) + ylab(percentage[2]) #Labels containing percentages 
)


