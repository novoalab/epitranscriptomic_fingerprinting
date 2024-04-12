## Panel A

### Script for PCAs on all samples showing separation between stages

merged_all_tissues_18S_28S  <- as.data.frame(t(merged_all_tissues_18S_28S ))
using_this_here <- all_tissues_and_stages_18S_28S_t
using_this_here <- using_this_here[!grepl("brain_adult", rownames(using_this_here)), ]


df <- data.matrix(using_this_here)
df <- scale(df, center=FALSE)
data<- using_this_here


data_pca<-prcomp(df,center=TRUE) #PCA 
data_out <- as.data.frame(data_pca$x) #X table of PCA
data_out$Symbol<- rownames(data_out)[1:22] ### THIS NUMBER HAS TO BE CHANGED DEPENDING ON SAMPLE


data_out$tissue<- c("brain", "brain", "brain", "brain", "heart", "heart", "heart", "heart", "heart", "heart", "liver", "liver", "liver", "liver", "liver", "liver", "e9.5", "e9.5", "testis", "testis", "testis", "testis")
data_out$stage <- c("embryo", "embryo", "newborn", "newborn", "embryo", "embryo", "newborn", "newborn", "adult", "adult","embryo", "embryo", "newborn", "newborn", "adult", "adult", "embryo", "embryo", "newborn", "newborn", "adult", "adult")


##Calculation for percentage of variance explained by each component
eigs <- data_pca$sdev^2#Calculate percentage for PC values
percentage<- round(eigs/sum(eigs)*100,2)#Calculate percentage for PC values
percentage <- paste( colnames(data_out), "(", paste( as.character(percentage), "%", ")", sep="") ) #Calculate percentage for PC values

library(ggplot2)
library(grid)
library(gridExtra)
library(ggrepel)
library(ggforce)


# Define the mapping between tissues and shapes
tissue_to_shape <- c("brain" = "circle", 
                     "e9.5" = "triangle", 
                     "heart" = "square", 
                     "liver" = "cross", 
                     "testis" = "box")

# Create a new column in data_out called shape and assign the corresponding shape based on tissue
data_out$shape <- tissue_to_shape[data_out$tissue]
data_out$shape <- factor(data_out$shape)

# plot the data with different shapes for each tissue
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
dev.off()


##PLOT FOR LOADINGS ALL LABELS
data_out_r <- as.data.frame(data_pca$rotation) #rotation data (loadings)
data_out_r$Symbol <- row.names(data_out_r) 
data_out_r$mod_type <- c("Nm", "Ψ", "Ψ", "Nm", "Unannotated", "Nm", "Unannotated", "Unannotated", "Ψ", "Ψ", "Ψ", "Ψ", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Nm", "Unannotated", "Ψ", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Nm", "Ψ", "Ψ", "Nm", "Nm", "Ψ", "Ψ")


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


### Panel B



using_this_here <- t(merged_18S_28S_brain_cells) ### This object is created in the script for Figure 2

# remove brain samples to keep only the cells
using_this_here <- using_this_here[-(1:6), ]


df <- data.matrix(using_this_here)
df <- scale(df, center=FALSE)
data<- using_this_here


data_pca<-prcomp(df,center=TRUE) #PCA 
data_out <- as.data.frame(data_pca$x) #X table of PCA
data_out$Symbol<- rownames(data_out)[1:6] ### THIS NUMBER HAS TO BE CHANGED DEPENDING ON SAMPLE

data_out$stage<- c("mESc", "mESc", "NPC", "NPC", "Neurons", "Neurons")

##Calculation for percentage of variance explained by each component
eigs <- data_pca$sdev^2#Calculate percentage for PC values
percentage<- round(eigs/sum(eigs)*100,2)#Calculate percentage for PC values
percentage <- paste( colnames(data_out), "(", paste( as.character(percentage), "%", ")", sep="") ) #Calculate percentage for PC values

library(ggplot2)
library(grid)
library(gridExtra)
library(ggrepel)
library(ggforce)

print(ggplot(data_out,aes(x=PC1,y=PC2,label=Symbol, color=stage))+
        #scale_color_manual(values = c("#DE3225", "#000000"))+
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2)+
        geom_point(alpha = 0.8, size = 3)+
        geom_text_repel(show.legend = FALSE, aes(label=Symbol),hjust=-1, vjust=0)+
        #ggforce::geom_mark_ellipse(aes(fill = tissue, color = tissue)) +
        #coord_equal()+
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
        xlab(percentage[1]) + ylab(percentage[2]) #Labels containing percentages 
      
)
dev.off()

library(ggrepel)

##PLOT FOR LOADINGS ALL LABELS
data_out_r <- as.data.frame(data_pca$rotation) #rotation data (loadings)
data_out_r$Symbol <- row.names(data_out_r) 
data_out_r$mod_type <- c("Nm", "Nm", "Ψ", "Ψ", "Unannotated", "Unannotated", "Unannotated", 
                       "Nm", "Nm", "Unannotated", "m1A", "Nm", "Nm", "Nm", 
                       "Unannotated", "Unannotated", "Nm", "Unannotated", 
                       "Nm", "Ψ", "Ψ", "Ψ", "Unannotated")


print(ggplot(data_out_r, aes(x = PC1, y = PC2)) +
        geom_point(aes(size = 3), alpha = 0.8) +  # Set point size and transparency
        geom_text_repel(aes(label = Symbol), size = 3, box.padding = 0.5) +  # Label points without overlap
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2) +
        theme(aspect.ratio = 1,
              panel.background = element_blank(),
              panel.border = element_rect(fill = NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              axis.text.x = element_text(colour = "black"),
              axis.text.y = element_text(colour = "black"),
              axis.ticks = element_line(colour = "black"),
              plot.margin = unit(c(1, 1, 1, 1), "line")) +
        xlab(percentage[1]) +
        ylab(percentage[2])
)


dev.off()
