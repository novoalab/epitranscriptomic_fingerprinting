# Panel A

# Starting with the object 'merged_all_tissues_18S_28S' made in the script for the Figure 1

using_this_here <- t(merged_all_tissues_18S_28S)
using_this_here <- using_this_here[grepl("adult", rownames(using_this_here)), ]


df <- data.matrix(using_this_here)
df <- scale(df, center=FALSE)
data<- using_this_here


data_pca<-prcomp(df,center=TRUE) #PCA 
data_out <- as.data.frame(data_pca$x) #X table of PCA
data_out$Symbol<- rownames(data_out)[1:8] ### THIS NUMBER HAS TO BE CHANGED DEPENDING ON SAMPLE


data_out$tissue<- c("brain", "brain", "heart", "heart", "liver", "liver", "testis", "testis")


##Calculation for percentage of variance explained by each component
eigs <- data_pca$sdev^2 #Calculate percentage for PC values
percentage<- round(eigs/sum(eigs)*100,2) #Calculate percentage for PC values
percentage <- paste( colnames(data_out), "(", paste( as.character(percentage), "%", ")", sep="") ) #Calculate percentage for PC values

library(ggplot2)
library(grid)
library(gridExtra)
library(ggrepel)
library(ggforce)



# plot the PCA
print(ggplot(data_out,aes(x=PC1,y=PC2,label=Symbol, color=tissue))+
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2)+
        geom_point(alpha = 0.8, size = 3) +
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
data_out_r$mod_type <- c("Nm", "Nm", "Ψ", "Ψ", "Nm", "Unannotated", "Nm", "Unannotated", "Unannotated", "Ψ", "Ψ", "Ψ", "Ψ", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Nm", "Nm", "Ψ", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Nm", "Ψ", "Ψ", "Nm", "Nm", "Ψ", "Ψ")


print(ggplot(data_out_r,aes(x=PC1,y=PC2,label=Symbol, color=mod_type))+
        geom_point()+
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2)+
        geom_point(alpha = 0.8, size = 3)+
        geom_text_repel(show.legend = FALSE, aes(label=Symbol),hjust=-1, vjust=0)+
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




