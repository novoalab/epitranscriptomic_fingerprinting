# 1. Read data and clean up (orignal vs custom)
####################################
library(stringr)
library(ggplot2)
library(reshape2)
library(plyr)
library(grid)
library(gridExtra)
library(ggExtra)
library(ggsci)
library(ggrepel)


mod<- read.delim("cmc_sites_Ivan_filtered_21112022_edited_four_bases.txt")


#Input - these will be different files (eg. barcodes 11 and 12 are adult liver treated and untreated with CMC)

no_cmc_input<- read.delim("fast5---barcode12_s.STATS", sep="")
cmc30_input<- read.delim("fast5---barcode11_s.STATS", sep="")

clean_input<-function(data,label) {
data$pos <- data$pos-13 #position fixing
data<- subset(data, pos > 10) #Cut first 40 nucleotide
data$position<- paste(data$chr, data$pos, sep="_")
data$Enzyme<- rep(label, nrow(data))
        merged<-vector() #To remove the last 40 nt
        for (ref in (unique(data$chr))){
                subs<- subset(data, chr==ref)
                subs2<- subset(subs, pos< max(subs$pos)-10)
                merged<- rbind(merged, subs2)
        }
# Take the coverage from +1 position
        final <- vector()
        for (upos in unique(merged$position)) {
                subs <- subset(merged, position==upos)
                numb <- subs[,"pos"] + 1
                plus_one <- paste(subs$chr, numb, sep="_")
                subs_2 <- subset(data, position==plus_one)
                subs$cov_1 <- subs_2[,"coverage"]
                final<- rbind(final, subs)
        }

final$norm_rt<- final$rtstop/final$cov_1 #to normalize RTdrop by the coverage at that position
joined<- merge(final,mod, by.x="position",by.y="Chr_Pos")
return(joined)
}



no_cmc<-clean_input(no_cmc_input,"No_Treatment")
cmc30<-clean_input(cmc30_input,"CMC")


final_30 <- rbind(no_cmc,cmc30)



## Delta RT-Drop
delta_rt <- function(data_nocmc, data_cmc,label) {
        data_nocmc2<- data_nocmc[,c("chr", "pos", "position", "norm_rt", "CMC", "Mod", "ModStatus")]
        data_cmc2<- data_cmc[,c("position", "norm_rt")]
        joined<- join(data_nocmc2,data_cmc2, by="position")
        joined$delta<- abs(joined[,4]- joined[,8])
        joined$Sample<- label
        colnames(joined) <- c("chr","pos","position","untreated_norm_rt", "CMC", "Mod","ModStatus","treated_norm_rt", "delta", "Sample")
        final <- vector()
        for (ref in unique(joined$chr)){
                subs <- subset(joined, chr==ref)
                subs$CMC_Score <- subs$delta / median(subs$delta)
                final <- rbind(final, subs)
        }
        return(final)
}

delta_30<- delta_rt(no_cmc,cmc30,"30CMC_Delta")
write.table(delta_30, file="CMC_Scores_rRNA.tsv", sep="\t", quote=FALSE, row.names=FALSE)





palette_28s <-c("red", "gray", "steelblue")
palette_18s <-c("red", "gray", "steelblue")
plot_area_rtdrop_delta_facet<- function(merged, name, ref, ylim, palette) {
        subs <- subset(merged, chr==ref)
        pdf(file= paste(ref,name,"_rtdrop_facet.pdf",sep=""),height=3,width=10,onefile=FALSE)
        print(ggplot(subs, aes(x=pos, y=CMC_Score,color=Mod, fill=Mod)) +
    geom_bar(stat="identity")
    +ggtitle(paste(ref,name, sep="_"))
    +ylim(0, as.numeric(ylim))
    +scale_fill_manual(values=palette)
    +scale_colour_manual(values=palette)
        +geom_text_repel(data=subset(subs, CMC_Score > 12 & Mod== "Maybe"),aes(label=pos),colour="black",segment.size  = 0.4,segment.color = "grey50",size=5)
        +theme_classic()
        +geom_hline(yintercept = 12, linetype="dashed")
        +facet_wrap(~chr, scales="free", nrow=length(unique(subs$chr))))
        dev.off()
}

plot_area_rtdrop_delta_facet(delta_30,"Delta_Merged_30","28S", "130" , palette_28s)
plot_area_rtdrop_delta_facet(delta_30,"Delta_Merged_30","18S", "130" , palette_18s)
