###### PCA with top dynamic sites in 18S and 28S

setwd("/no_backup/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/heidelberg_lung_samples/metastasis_and_stage")

input1 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool1/epinano_output/epinano_flow/1727_normal_primary.plus_strand.per.site.csv')
input1$pos <- paste(input1$X.Ref, input1$pos, sep = ":")

input2 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool1/epinano_output/epinano_flow/1812_normal_primary.plus_strand.per.site.csv')
input2$pos <- paste(input2$X.Ref, input2$pos, sep = ":")

input3 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool1/epinano_output/epinano_flow/1868_normal_primary.plus_strand.per.site.csv')
input3$pos <- paste(input3$X.Ref, input3$pos, sep = ":")

input4 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool1/epinano_output/epinano_flow/2388_normal_primary.plus_strand.per.site.csv')
input4$pos <- paste(input4$X.Ref, input4$pos, sep = ":")

input5 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool1/epinano_output/epinano_flow/2416_normal_primary.plus_strand.per.site.csv')
input5$pos <- paste(input5$X.Ref, input5$pos, sep = ":")

input6 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool2/epinano_output/epinano_flow/2807_normal_primary.plus_strand.per.site.csv')
input6$pos <- paste(input6$X.Ref, input6$pos, sep = ":")

input7 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool2/epinano_output/epinano_flow/2875_normal_primary.plus_strand.per.site.csv')
input7$pos <- paste(input7$X.Ref, input7$pos, sep = ":")

input8 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool2/epinano_output/epinano_flow/2944_normal_primary.plus_strand.per.site.csv')
input8$pos <- paste(input8$X.Ref, input8$pos, sep = ":")

input9 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool2/epinano_output/epinano_flow/3029_normal_primary.plus_strand.per.site.csv')
input9$pos <- paste(input9$X.Ref, input9$pos, sep = ":")

input10 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool2/epinano_output/epinano_flow/3200_normal_primary.plus_strand.per.site.csv')
input10$pos <- paste(input10$X.Ref, input10$pos, sep = ":")

input11 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool1/epinano_output/epinano_flow/1727_tumor_primary.plus_strand.per.site.csv')
input11$pos <- paste(input11$X.Ref, input11$pos, sep = ":")

input12 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool1/epinano_output/epinano_flow/1812_tumor_primary.plus_strand.per.site.csv')
input12$pos <- paste(input12$X.Ref, input12$pos, sep = ":")

input13 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool1/epinano_output/epinano_flow/1868_tumor_primary.plus_strand.per.site.csv')
input13$pos <- paste(input13$X.Ref, input13$pos, sep = ":")

input14 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool1/epinano_output/epinano_flow/2388_tumor_primary.plus_strand.per.site.csv')
input14$pos <- paste(input14$X.Ref, input14$pos, sep = ":")

input15 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool1/epinano_output/epinano_flow/2416_tumor_primary.plus_strand.per.site.csv')
input15$pos <- paste(input15$X.Ref, input15$pos, sep = ":")

input16 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool2/epinano_output/epinano_flow/2807_tumor_primary.plus_strand.per.site.csv')
input16$pos <- paste(input16$X.Ref, input16$pos, sep = ":")

input17 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool2/epinano_output/epinano_flow/2875_tumor_primary.plus_strand.per.site.csv')
input17$pos <- paste(input17$X.Ref, input17$pos, sep = ":")

input18 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool2/epinano_output/epinano_flow/2944_tumor_primary.plus_strand.per.site.csv')
input18$pos <- paste(input18$X.Ref, input18$pos, sep = ":")

input19 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool2/epinano_output/epinano_flow/3029_tumor_primary.plus_strand.per.site.csv')
input19$pos <- paste(input19$X.Ref, input19$pos, sep = ":")

input20 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell1_pool2/epinano_output/epinano_flow/3200_tumor_primary.plus_strand.per.site.csv')
input20$pos <- paste(input20$X.Ref, input20$pos, sep = ":")

input21 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/epinano_output/epinano_flow/3263_normal_primary.plus_strand.per.site.csv')
input21$pos <- paste(input21$X.Ref, input21$pos, sep = ":")

input22 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/epinano_output/epinano_flow/3368_normal_primary.plus_strand.per.site.csv')
input22$pos <- paste(input22$X.Ref, input22$pos, sep = ":")

input23 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/epinano_output/epinano_flow/3374_normal_primary.plus_strand.per.site.csv')
input23$pos <- paste(input23$X.Ref, input23$pos, sep = ":")

input24 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/epinano_output/epinano_flow/3438_normal_primary.plus_strand.per.site.csv')
input24$pos <- paste(input24$X.Ref, input24$pos, sep = ":")

input25 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/epinano_output/epinano_flow/3477_normal_primary.plus_strand.per.site.csv')
input25$pos <- paste(input25$X.Ref, input25$pos, sep = ":")

input26 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/epinano_output/epinano_flow/3263_tumor_primary.plus_strand.per.site.csv')
input26$pos <- paste(input26$X.Ref, input26$pos, sep = ":")

input27 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/epinano_output/epinano_flow/3368_tumor_primary.plus_strand.per.site.csv')
input27$pos <- paste(input27$X.Ref, input27$pos, sep = ":")

input28 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/epinano_output/epinano_flow/3374_tumor_primary.plus_strand.per.site.csv')
input28$pos <- paste(input28$X.Ref, input28$pos, sep = ":")

input29 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/epinano_output/epinano_flow/3438_tumor_primary.plus_strand.per.site.csv')
input29$pos <- paste(input29$X.Ref, input29$pos, sep = ":")

input30 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/epinano_output/epinano_flow/3477_tumor_primary.plus_strand.per.site.csv')
input30$pos <- paste(input30$X.Ref, input30$pos, sep = ":")

input31 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/epinano_output/epinano_flow/3491_normal_primary.plus_strand.per.site.csv')
input31$pos <- paste(input31$X.Ref, input31$pos, sep = ":")

input32 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/epinano_output/epinano_flow/3502_normal_primary.plus_strand.per.site.csv')
input32$pos <- paste(input32$X.Ref, input32$pos, sep = ":")

input33 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/epinano_output/epinano_flow/3523_normal_primary.plus_strand.per.site.csv')
input33$pos <- paste(input33$X.Ref, input33$pos, sep = ":")

input34 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/epinano_output/epinano_flow/3536_normal_primary.plus_strand.per.site.csv')
input34$pos <- paste(input34$X.Ref, input34$pos, sep = ":")

input35 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/epinano_output/epinano_flow/3752_normal_primary.plus_strand.per.site.csv')
input35$pos <- paste(input35$X.Ref, input35$pos, sep = ":")

input36 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/epinano_output/epinano_flow/3491_tumor_primary.plus_strand.per.site.csv')
input36$pos <- paste(input36$X.Ref, input36$pos, sep = ":")

input37 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/epinano_output/epinano_flow/3502_tumor_primary.plus_strand.per.site.csv')
input37$pos <- paste(input37$X.Ref, input37$pos, sep = ":")

input38 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/epinano_output/epinano_flow/3523_tumor_primary.plus_strand.per.site.csv')
input38$pos <- paste(input38$X.Ref, input38$pos, sep = ":")

input39 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/epinano_output/epinano_flow/3536_tumor_primary.plus_strand.per.site.csv')
input39$pos <- paste(input39$X.Ref, input39$pos, sep = ":")

input40 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/epinano_output/epinano_flow/3752_tumor_primary.plus_strand.per.site.csv')
input40$pos <- paste(input40$X.Ref, input40$pos, sep = ":")



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
input13$sum <- input13$mis + input13$del +input13$ins
input14$sum <- input14$mis + input14$del +input14$ins
input15$sum <- input15$mis + input15$del +input15$ins
input16$sum <- input16$mis + input16$del +input16$ins
input17$sum <- input17$mis + input17$del +input17$ins
input18$sum <- input18$mis + input18$del +input18$ins
input19$sum <- input19$mis + input19$del +input19$ins
input20$sum <- input20$mis + input20$del +input20$ins
input21$sum <- input21$mis + input21$del +input21$ins
input22$sum <- input22$mis + input22$del +input22$ins
input23$sum <- input23$mis + input23$del +input23$ins
input24$sum <- input24$mis + input24$del +input24$ins
input25$sum <- input25$mis + input25$del +input25$ins
input26$sum <- input26$mis + input26$del +input26$ins
input27$sum <- input27$mis + input27$del +input27$ins
input28$sum <- input28$mis + input28$del +input28$ins
input29$sum <- input29$mis + input29$del +input29$ins
input30$sum <- input30$mis + input30$del +input30$ins
input31$sum <- input31$mis + input31$del +input31$ins
input32$sum <- input32$mis + input32$del +input32$ins
input33$sum <- input33$mis + input33$del +input33$ins
input34$sum <- input34$mis + input34$del +input34$ins
input35$sum <- input35$mis + input35$del +input35$ins
input36$sum <- input36$mis + input36$del +input36$ins
input37$sum <- input37$mis + input37$del +input37$ins
input38$sum <- input38$mis + input38$del +input38$ins
input39$sum <- input39$mis + input39$del +input39$ins
input40$sum <- input40$mis + input40$del +input40$ins



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
merged <- merge (merged, input13[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input14[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input15[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input16[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input17[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input18[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input19[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input20[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input21[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input22[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input23[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input24[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input25[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input26[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input27[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input28[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input29[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input30[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input31[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input32[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input33[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input34[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input35[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input36[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input37[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input38[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input39[,c("pos", "sum")], by = "pos")
merged <- merge (merged, input40[,c("pos", "sum")], by = "pos")


colnames(merged) <- c("position", "1727_normal", "1812_normal", "1868_normal", "2388_normal", "2416_normal", "2807_normal", "2875_normal", "2944_normal", "3029_normal", "3200_normal", "1727_tumor", "1812_tumor", "1868_tumor", "2388_tumor", "2416_tumor", "2807_tumor", "2875_tumor", "2944_tumor", "3029_tumor", "3200_tumor", "3263_normal", "3368_normal", "3374_normal", "3438_normal", "3477_normal", "3263_tumor", "3368_tumor", "3374_tumor", "3438_tumor", "3477_tumor", "3491_normal", "3502_normal", "3523_normal", "3536_normal", "3752_normal", "3491_tumor", "3502_tumor", "3523_tumor", "3536_tumor", "3752_tumor")
rownames(merged) <- merged$position

# Extract rows containing "18S" in the "position" column
merged_18S <- merged[grep("18s", merged$position), ]

# Extract the numeric part of the position column
position_suffix <- as.numeric(sub(".*:(\\d+)", "\\1", merged_18S$position))

# Order the dataframe based on the numeric position suffix
merged_18S <- merged_18S[order(position_suffix), ]

# Write the ordered dataframe to a CSV file
#write.csv(merged_18S, "human_cancer_lung_raw_summed_errors_18S.csv", row.names = FALSE)


# Extract rows containing "28S" in the "position" column
merged_28S <- merged[grep("28s", merged$position), ]

# Extract the numeric part of the position column
position_suffix <- as.numeric(sub(".*:(\\d+)", "\\1", merged_28S$position))

# Order the dataframe based on the numeric position suffix
merged_28S <- merged_28S[order(position_suffix), ]

# Write the ordered dataframe to a CSV file
#write.csv(merged_28S, "human_cancer_lung_raw_summed_errors_28S.csv", row.names = FALSE)


# Define the positions to be removed
positions_to_remove <- c("18s:1", "18s:2", "18s:3", "18s:4", "18s:5", "18s:6", "18s:7", "18s:8", "18s:9", "18s:10", "18s:11", "18s:12", "18s:13", "18s:14", "18s:15",
                         "18s:1850", "18s:1851", "18s:1852", "18s:1853", "18s:1854", "18s:1855", "18s:1856", "18s:1857", "18s:1858", "18s:1859",
                         "28s:1", "28s:2", "28s:3", "28s:4", "28s:5", "28s:6", "28s:7", "28s:8", "28s:9", "28s:10", "28s:11", "28s:12", "28s:13", "28s:14", "28s:15",
                         "28s:5050", "28s:5051", "28s:5052", "28s:5053", "28s:5054", "28s:5055", "28s:5056", "28s:5057", "28s:5058", "28s:5059")

# Remove the specified rows
merged <- merged[!(merged$position %in% positions_to_remove), ]

# Define the strings to be excluded
strings_to_exclude <- c("5s", "5.8s", "mt-12s", "mt-16s")

# Filter the dataframe to exclude rows containing the specified strings
merged <- merged[!grepl(paste(strings_to_exclude, collapse = "|"), merged$position), ]


# Get the names of the columns that contain the string 'tumor'
tumor_columns <- grep("tumor", names(merged), value = TRUE, ignore.case = TRUE)

# Keep the first column and the tumor columns
merged_filtered <- merged[, c(names(merged)[1], tumor_columns)]

# View the resulting dataframe
print(merged_filtered)



metadata_cancer_groups <- read.csv("metadata_group_only_tumor_samples_03062024.csv", header = TRUE)
# If you only want to keep the groups, without subgroups
metadata_cancer_groups$Stadium <- gsub("IA|IB", "I", metadata_cancer_groups$Stadium)
metadata_cancer_groups$Stadium <- gsub("IIA|IIB", "II", metadata_cancer_groups$Stadium)

# Add "_tumor" to all entries in the 'tissue' column
metadata_cancer_groups$tissue <- paste0(metadata_cancer_groups$tissue, "_tumor")


# Load necessary libraries
library(dplyr)

# Extract tumor sample names and their stages
stage_1_samples <- metadata_cancer_groups$tissue[metadata_cancer_groups$Stadium == "I"]
stage_2_samples <- metadata_cancer_groups$tissue[metadata_cancer_groups$Stadium == "II"]

# Ensure the sample names in metadata match the column names in merged_filtered
common_samples_1 <- intersect(stage_1_samples, colnames(merged_filtered))
common_samples_2 <- intersect(stage_2_samples, colnames(merged_filtered))

# Calculate the mean for each row for stage 1 and stage 2 samples
merged_filtered <- merged_filtered %>%
  rowwise() %>%
  mutate(mean_stage_1 = mean(c_across(all_of(common_samples_1)), na.rm = TRUE),
         mean_stage_2 = mean(c_across(all_of(common_samples_2)), na.rm = TRUE),
         abs_diff = abs(mean_stage_1 - mean_stage_2)) %>%
  ungroup()

# Find the top 20 rows with the highest absolute difference
top_20_rows <- merged_filtered %>%
  arrange(desc(abs_diff)) %>%
  slice(1:20)

# View the resulting top 20 rows
print(top_20_rows)

top_20_rows <- as.data.frame(top_20_rows)

rownames(top_20_rows) <- top_20_rows$position


top_20_rows$position <- NULL
top_20_rows$mean_stage_1 <- NULL
top_20_rows$mean_stage_2 <- NULL
top_20_rows$abs_diff <- NULL
top_20_rows$variation <- NULL



merged2_t <- t(top_20_rows)



using_this_here <- merged2_t ### CHANGE THIS BASED ON WHICH SAMPLE YOU'RE USING


df <- data.matrix(using_this_here)
df <- scale(df, center=FALSE)
data<- using_this_here


data_pca<-prcomp(df,center=TRUE) #PCA 
data_out <- as.data.frame(data_pca$x) #X table of PCA
data_out$Symbol <- sub("^[^_]+_(.*)", "\\1", rownames(data_out))
data_out$tissue <- sub("^([^_]+)_.*", "\\1", rownames(data_out))


metadata_cancer_group <- read.table("metadata_cancer_group_only_tumor.txt") 
colnames(metadata_cancer_group) <- "cancer_group"
data_out$cancer_group <- metadata_cancer_group$cancer_group




##Calculation for percentage of variance explained by each component
eigs <- data_pca$sdev^2#Calculate percentage for PC values
percentage<- round(eigs/sum(eigs)*100,2)#Calculate percentage for PC values
percentage <- paste( colnames(data_out), "(", paste( as.character(percentage), "%", ")", sep="") ) #Calculate percentage for PC values


# If you only want to keep the groups, without subgroups
data_out$cancer_group <- gsub("IA|IB", "I", data_out$cancer_group)
data_out$cancer_group <- gsub("IIA|IIB", "II", data_out$cancer_group)


data_out$cancer_group <- factor(data_out$cancer_group)


library(ggplot2)
library(ggrepel)

pdf("stages_tumor_only_09082024.pdf",height=8, width=10)
ggplot(data_out, aes(x = PC1, y = PC2, label = rownames(data_out), color = cancer_group)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(alpha = 0.8, size = 3) +
  geom_text_repel(nudge_x = 0.05, nudge_y = 0.05, size = 3, hjust = 0, vjust = 0) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "line")) +
  xlab(percentage[1]) + ylab(percentage[2])
dev.off()




##PLOT FOR LOADINGS ALL LABELS
data_out_r <- as.data.frame(data_pca$rotation) #rotation data (loadings)
data_out_r$Symbol <- row.names(data_out_r) 
#data_out_r$mod_type <- c("Ψ",  "Ψ", "Unannotated", "Unannotated", "Unannotated", "Nm", "Ψ", "Ψ", "Nm", "Nm", "Ψ", "Ψ", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated", "Unannotated")

#pdf(file="pca_loadings_for_lung_cancer_tissues_08042024.pdf",height=8,width=10)
print(ggplot(data_out_r,aes(x=PC1,y=PC2,label=Symbol))+
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


######## Metastasis


# Keep the first column and the tumor columns
merged_filtered <- merged[, c(names(merged)[1], tumor_columns)]

# View the resulting dataframe
print(merged_filtered)



metadata_cancer_groups <- read.table("metadata_cancer_metastasis_only_tumor.txt", header = FALSE)
colnames(metadata_cancer_groups) <- c("Stadium")

column_names <- colnames(merged_filtered)[2:21]

metadata_cancer_groups$tissue <- column_names



# Load necessary libraries
library(dplyr)


# Assuming merged_filtered and metadata_cancer_groups are already loaded

# Get column names for merged_filtered from columns 2 to 21
merged_cols <- colnames(merged_filtered)[2:21]

# Ensure 'tissue' in metadata_cancer_groups matches these column names
tissue <- metadata_cancer_groups$tissue

# Create logical vectors for "keine" and non-"keine" samples
is_keine <- metadata_cancer_groups$Stadium == "keine"
is_non_keine <- !is_keine

# Get column indices for "keine" and non-"keine" samples
keine_cols <- which(tissue %in% merged_cols & is_keine)
non_keine_cols <- which(tissue %in% merged_cols & is_non_keine)

# Calculate row means for "keine" and non-"keine" samples
keine_avg <- rowMeans(merged_filtered[, keine_cols + 1])  # +1 to account for position column
non_keine_avg <- rowMeans(merged_filtered[, non_keine_cols + 1])  # +1 to account for position column

# Calculate the absolute difference of averages between "keine" and non-"keine"
diff_averages <- abs(keine_avg - non_keine_avg)

# Create a data frame of discovered positions
discovered_positions <- data.frame(position = merged_filtered$position)

# Sort the data frame by the absolute difference of averages
discovered_positions <- discovered_positions[order(diff_averages, decreasing = TRUE), , drop = FALSE]

# Print the top 20 positions with the largest absolute difference of averages
top_20_positions <- discovered_positions[1:20, , drop = FALSE]
print(top_20_positions)




merged2 <- merge(top_20_positions, merged_filtered, by="position")
row.names(merged2) <- merged2$position
merged2$position <- NULL

merged2_t <- t(merged2)




using_this_here <- merged2_t ### CHANGE THIS BASED ON WHICH SAMPLE YOU'RE USING
using_this_here <- using_this_here[, !(colnames(using_this_here) == "28s:2085")]


df <- data.matrix(using_this_here)
df <- scale(df, center=FALSE)
data<- using_this_here


data_pca<-prcomp(df,center=TRUE) #PCA 
data_out <- as.data.frame(data_pca$x) #X table of PCA
data_out$Symbol <- sub("^[^_]+_(.*)", "\\1", rownames(data_out))
data_out$tissue <- sub("^([^_]+)_.*", "\\1", rownames(data_out))
data_out$Stadium <- metadata_cancer_groups$Stadium




##Calculation for percentage of variance explained by each component
eigs <- data_pca$sdev^2#Calculate percentage for PC values
percentage<- round(eigs/sum(eigs)*100,2)#Calculate percentage for PC values
percentage <- paste( colnames(data_out), "(", paste( as.character(percentage), "%", ")", sep="") ) #Calculate percentage for PC values




pdf("PCA_human_lung_cancer_tissues_heidelberg_04092023.pdf",height=8, width=10)
ggplot(data_out, aes(x = PC1, y = PC2, label = rownames(data_out), color = Stadium)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(alpha = 0.8, size = 3) +
  geom_text_repel(nudge_x = 0.05, nudge_y = 0.05, size = 3, hjust = 0, vjust = 0) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "line")) +
  xlab(percentage[1]) + ylab(percentage[2])
dev.off()
