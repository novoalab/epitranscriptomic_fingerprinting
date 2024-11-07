### The input files are the same ones used in Figure_1, where discovered positions files can also be found


#Load the libraries needed for this script
library('plyr')
library('ggplot2')
library('ggrepel') 

#Importing and manipulating the Epinano outputs
input1 <- read.csv('brain_embryo_rep1.csv')
input1 <- input1[grepl("18S", input1$X.Ref),]

input2 <- read.csv('brain_embryo_rep3.csv')
input2 <- input2[grepl("18S", input2$X.Ref),]

input3 <- read.csv('brain_newborn_rep1.csv')
input3 <- input3[grepl("18S", input3$X.Ref),]

input4 <- read.csv('brain_newborn_rep3.csv')
input4 <- input4[grepl("18S", input4$X.Ref),]

input5 <- read.csv('brain_adult_rep1.csv')
input5 <- input5[grepl("18S", input5$X.Ref),]

input6 <- read.csv('brain_adult_rep2.csv')
input6 <- input6[grepl("18S", input6$X.Ref),]

input7 <- read.csv('heart_embryo_rep1.csv')
input7 <- input7[grepl("18S", input7$X.Ref),]

input8 <- read.csv('heart_embryo_rep3.csv')
input8 <- input8[grepl("18S", input8$X.Ref),]

input9 <- read.csv('heart_newborn_rep1.csv')
input9 <- input9[grepl("18S", input9$X.Ref),]

input10 <- read.csv('heart_newborn_rep3.csv')
input10 <- input10[grepl("18S", input10$X.Ref),]

input11 <- read.csv('heart_adult_rep1.csv')
input11 <- input11[grepl("18S", input11$X.Ref),]

input12 <- read.csv('heart_adult_rep2.csv')
input12 <- input12[grepl("18S", input12$X.Ref),]

input13 <- read.csv('liver_embryo_rep1.csv')
input13 <- input13[grepl("18S", input13$X.Ref),]

input14 <- read.csv('liver_embryo_rep3.csv')
input14 <- input14[grepl("18S", input14$X.Ref),]

input15 <- read.csv('liver_newborn_rep1.csv')
input15 <- input15[grepl("18S", input15$X.Ref),]

input16 <- read.csv('liver_newborn_rep3.csv')
input16 <- input16[grepl("18S", input16$X.Ref),]

input17 <- read.csv('liver_adult_rep1.csv')
input17 <- input17[grepl("18S", input17$X.Ref),]

input18 <- read.csv('liver_adult_rep2.csv')
input18 <- input18[grepl("18S", input18$X.Ref),]

input19 <- read.csv('e.9.5_rep1.csv')
input19 <- input19[grepl("18S", input19$X.Ref),]

input20 <- read.csv('e.9.5_rep3.csv')
input20 <- input20[grepl("18S", input20$X.Ref),]

input21 <- read.csv('testis_newborn_rep1.csv')
input21 <- input21[grepl("18S", input21$X.Ref),]

input22 <- read.csv('testis_newborn_rep3.csv')
input22 <- input22[grepl("18S", input22$X.Ref),]

input23 <- read.csv('testis_adult_rep1.csv')
input23 <- input23[grepl("18S", input23$X.Ref),]

input24 <- read.csv('testis_adult_rep2.csv')
input24 <- input24[grepl("18S", input24$X.Ref),]

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

colnames(merged) <- c("position", "brain_embryo_rep1", "brain_embryo_rep3", "brain_newborn_rep1", "brain_newborn_rep3", "brain_adult_rep1", "brain_adult_rep2", "heart_embryo_rep1", "heart_embryo_rep3", "heart_newborn_rep1", "heart_newborn_rep3", "heart_adult_rep1", "heart_adult_rep2", "liver_embryo_rep1", "liver_embryo_rep3", "liver_newborn_rep1", "liver_newborn_rep3", "liver_adult_rep1", "liver_adult_rep2", "e9.5_rep1", "e9.5_rep2", "testis_newborn_rep1", "testis_newborn_rep3", "testis_adult_rep1", "testis_adult_rep2")
rownames(merged) <- merged$position


discovered_positions <- read.csv("discovered_positions_18S_from_clean_list.csv")

colnames(discovered_positions) <- "position"

merged2 <- merge(discovered_positions, merged, by="position")
merged2$position <- sub("^", "18S:", merged2$position)
row.names(merged2) <- merged2$position
merged2$position <- NULL

merged_18S_tissues <- merged2

#### 28S
#Importing and manipulating the Epinano outputs
input1 <- read.csv('brain_embryo_rep1.csv')
input1 <- input1[grepl("28S", input1$X.Ref),]

input2 <- read.csv('brain_embryo_rep3.csv')
input2 <- input2[grepl("28S", input2$X.Ref),]

input3 <- read.csv('brain_newborn_rep1.csv')
input3 <- input3[grepl("28S", input3$X.Ref),]

input4 <- read.csv('brain_newborn_rep3.csv')
input4 <- input4[grepl("28S", input4$X.Ref),]

input5 <- read.csv('brain_adult_rep1.csv')
input5 <- input5[grepl("28S", input5$X.Ref),]

input6 <- read.csv('brain_adult_rep2.csv')
input6 <- input6[grepl("28S", input6$X.Ref),]

input7 <- read.csv('heart_embryo_rep1.csv')
input7 <- input7[grepl("28S", input7$X.Ref),]

input8 <- read.csv('heart_embryo_rep3.csv')
input8 <- input8[grepl("28S", input8$X.Ref),]

input9 <- read.csv('heart_newborn_rep1.csv')
input9 <- input9[grepl("28S", input9$X.Ref),]

input10 <- read.csv('heart_newborn_rep3.csv')
input10 <- input10[grepl("28S", input10$X.Ref),]

input11 <- read.csv('heart_adult_rep1.csv')
input11 <- input11[grepl("28S", input11$X.Ref),]

input12 <- read.csv('heart_adult_rep2.csv')
input12 <- input12[grepl("28S", input12$X.Ref),]

input13 <- read.csv('liver_embryo_rep1.csv')
input13 <- input13[grepl("28S", input13$X.Ref),]

input14 <- read.csv('liver_embryo_rep3.csv')
input14 <- input14[grepl("28S", input14$X.Ref),]

input15 <- read.csv('liver_newborn_rep1.csv')
input15 <- input15[grepl("28S", input15$X.Ref),]

input16 <- read.csv('liver_newborn_rep3.csv')
input16 <- input16[grepl("28S", input16$X.Ref),]

input17 <- read.csv('liver_adult_rep1.csv')
input17 <- input17[grepl("28S", input17$X.Ref),]

input18 <- read.csv('liver_adult_rep2.csv')
input18 <- input18[grepl("28S", input18$X.Ref),]

input19 <- read.csv('e.9.5_rep1.csv')
input19 <- input19[grepl("28S", input19$X.Ref),]

input20 <- read.csv('e.9.5_rep3.csv')
input20 <- input20[grepl("28S", input20$X.Ref),]

input21 <- read.csv('testis_newborn_rep1.csv')
input21 <- input21[grepl("28S", input21$X.Ref),]

input22 <- read.csv('testis_newborn_rep3.csv')
input22 <- input22[grepl("28S", input22$X.Ref),]

input23 <- read.csv('testis_adult_rep1.csv')
input23 <- input23[grepl("28S", input23$X.Ref),]

input24 <- read.csv('testis_adult_rep2.csv')
input24 <- input24[grepl("28S", input24$X.Ref),]

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

colnames(merged) <- c("position", "brain_embryo_rep1", "brain_embryo_rep3", "brain_newborn_rep1", "brain_newborn_rep3", "brain_adult_rep1", "brain_adult_rep2", "heart_embryo_rep1", "heart_embryo_rep3", "heart_newborn_rep1", "heart_newborn_rep3", "heart_adult_rep1", "heart_adult_rep2", "liver_embryo_rep1", "liver_embryo_rep3", "liver_newborn_rep1", "liver_newborn_rep3", "liver_adult_rep1", "liver_adult_rep2", "e9.5_rep1", "e9.5_rep3", "testis_newborn_rep1", "testis_newborn_rep3", "testis_adult_rep1", "testis_adult_rep2")
rownames(merged) <- merged$position



discovered_positions <- read.table("discovered_positions_28S_from_clean_list.csv")
colnames(discovered_positions) <- "position"
merged2 <- merge(discovered_positions, merged, by="position")
merged2$position <- sub("^", "28S:", merged2$position)
row.names(merged2) <- merged2$position
merged2$position <- NULL

merged_28S_tissues <- merged2
colnames(merged_28S_tissues) <- colnames(merged_18S_tissues)

merged_all_tissues_18S_28S <- rbind(merged_18S_tissues, merged_28S_tissues)

# Select columns that contain the string "adult"
merged_all_tissues_18S_28S_adult <- merged_all_tissues_18S_28S[, grepl("adult", colnames(merged_all_tissues_18S_28S))]

# Calculate variance across rows and add as a new column
merged_all_tissues_18S_28S_adult$Variance <- apply(merged_all_tissues_18S_28S_adult, 1, var)

# Average the values for each pair of replicates
brain_avg <- rowMeans(merged_all_tissues_18S_28S_adult[, 1:2])
heart_avg <- rowMeans(merged_all_tissues_18S_28S_adult[, 3:4])
liver_avg <- rowMeans(merged_all_tissues_18S_28S_adult[, 5:6])
testis_avg <- rowMeans(merged_all_tissues_18S_28S_adult[, 7:8])

# Combine these averages into a matrix
averages_matrix <- cbind(brain_avg, heart_avg, liver_avg, testis_avg)

# Calculate the variance across the four averages
merged_all_tissues_18S_28S_adult$Variance <- apply(averages_matrix, 1, var)




### Load the annotated sites

annotated_sites <- read.table('/users/enovoa/imilenkovic/snoRNAs_across_mouse_tissues/annotated_positions.txt', header = TRUE)


# Assuming annotated_sites is a data frame with a column named 'position'
positions_to_keep <- annotated_sites$position

# Filter the rows of merged_all_tissues_18S_28S_adult based on row names
filtered_merged_all_tissues_18S_28S_adult <- merged_all_tissues_18S_28S_adult[rownames(merged_all_tissues_18S_28S_adult) %in% positions_to_keep, ]


snoRNA_positions_matches <- read.table ('/users/enovoa/imilenkovic/snoRNAs_across_mouse_tissues/pairs_snoRNA_modification_site.txt', header = TRUE)

# Assuming snoRNA_positions_matches is already loaded and contains a column 'snoRNA'
# Filter scaled_matrix to keep only rows with names in snoRNA_positions_matches$snoRNA




rRNA_mods <- filtered_merged_all_tissues_18S_28S_adult

rRNA_mods$Variance <- NULL

Pos355 <- rRNA_mods[1,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Pos355), varnames = c("mod_site", "sample"), value.name = "SE")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)

# Define the function to extract tissue type
extract_tissue_type <- function(colname) {
  if (!is.character(colname)) {
    colname <- as.character(colname)
  }
  strsplit(colname, "_")[[1]][1]
}
# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)


pdf("Pos355_across_mouse_tissues_SE_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = SE, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$mod_site)), x = "Tissue Type", y = "SE") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("brain" = "blue", "heart" = "red", "liver" = "green", "testis" = "purple")) +
  ylim(0, 1.5)
dev.off()

#### 577

Pos577 <- rRNA_mods[4,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Pos577), varnames = c("mod_site", "sample"), value.name = "SE")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)


# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)


pdf("Pos577_across_mouse_tissues_SE_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = SE, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$mod_site)), x = "Tissue Type", y = "SE") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("brain" = "blue", "heart" = "red", "liver" = "green", "testis" = "purple")) +
  ylim(0, 1.5)
dev.off()

#### 4325

Pos4325 <- rRNA_mods[15,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Pos4325), varnames = c("mod_site", "sample"), value.name = "SE")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)

# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)


pdf("Pos4325_across_mouse_tissues_SE_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = SE, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$mod_site)), x = "Tissue Type", y = "SE") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("brain" = "blue", "heart" = "red", "liver" = "green", "testis" = "purple")) +
  ylim(0, 1.5)
dev.off()



#### 3523

Pos3523 <- rRNA_mods[13,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Pos3523), varnames = c("mod_site", "sample"), value.name = "SE")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)

# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)


pdf("Pos3523_across_mouse_tissues_SE_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = SE, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$mod_site)), x = "Tissue Type", y = "SE") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("brain" = "blue", "heart" = "red", "liver" = "green", "testis" = "purple")) +
  ylim(0, 1.5)
dev.off()


#### 1178

Pos1178 <- rRNA_mods[8,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Pos1178), varnames = c("mod_site", "sample"), value.name = "SE")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)

# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)


pdf("Pos1178_across_mouse_tissues_SE_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = SE, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$mod_site)), x = "Tissue Type", y = "SE") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("brain" = "blue", "heart" = "red", "liver" = "green", "testis" = "purple")) +
  ylim(0, 1.5)
dev.off()


#### 1137

Pos1137 <- rRNA_mods[7,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Pos1137), varnames = c("mod_site", "sample"), value.name = "SE")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)

# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)


pdf("Pos1137_across_mouse_tissues_SE_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = SE, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$mod_site)), x = "Tissue Type", y = "SE") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("brain" = "blue", "heart" = "red", "liver" = "green", "testis" = "purple")) +
  ylim(0, 1.5)
dev.off()

#### 2596

Pos2596 <- rRNA_mods[11,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Pos2596), varnames = c("mod_site", "sample"), value.name = "SE")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)

# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)


pdf("Pos2596_across_mouse_tissues_SE_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = SE, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$mod_site)), x = "Tissue Type", y = "SE") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("brain" = "blue", "heart" = "red", "liver" = "green", "testis" = "purple")) +
  ylim(0, 1.5)
dev.off()



#### 407

Pos407 <- rRNA_mods[2,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Pos407), varnames = c("mod_site", "sample"), value.name = "SE")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)

# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)


pdf("Pos407_across_mouse_tissues_SE_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = SE, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$mod_site)), x = "Tissue Type", y = "SE") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("brain" = "blue", "heart" = "red", "liver" = "green", "testis" = "purple")) +
  ylim(0, 1.5)
dev.off()


### SNORNA PART


####### CPM

# Step 1: Calculate total counts per sample
total_counts <- colSums(snoRNAs_matrix)

# Step 2: Calculate counts per million (CPM)
cpm <- sweep(snoRNAs_matrix, 2, total_counts, "/") * 1e6


# Extract the vector of snoRNA names
snoRNA_names <- snoRNA_positions_matches$snoRNA

# Subset scaled_matrix based on these snoRNA names
filtered_cpm <- cpm[rownames(cpm) %in% snoRNA_names, ]

# Keep only columns with tissue types 'Brain', 'Heart', 'Liver', or 'Testis'
filtered_cpm <- filtered_cpm[, grepl('Brain|Heart|Liver|Testis', colnames(filtered_cpm))]



#### Extract snoRNAs one by one


#### Snord90

Snord90 <- filtered_cpm[1,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Snord90), varnames = c("snoRNA", "sample"), value.name = "TPM")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)

# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)
                                 

pdf("Snord90_across_mouse_tissues_TPMs_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = TPM, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$snoRNA)), x = "Tissue Type", y = "TPM") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("Brain" = "blue", "Heart" = "red", "Liver" = "green", "Testis" = "purple"))
dev.off()


#### Snord93

Snord93 <- filtered_cpm[2,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Snord93), varnames = c("snoRNA", "sample"), value.name = "TPM")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)

# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)


pdf("Snord93_across_mouse_tissues_TPMs_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = TPM, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$snoRNA)), x = "Tissue Type", y = "TPM") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("Brain" = "blue", "Heart" = "red", "Liver" = "green", "Testis" = "purple"))
dev.off()

#### Snora7a

Snora7a <- filtered_cpm[3,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Snora7a), varnames = c("snoRNA", "sample"), value.name = "TPM")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)

# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)


pdf("Snora7a_across_mouse_tissues_TPMs_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = TPM, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$snoRNA)), x = "Tissue Type", y = "TPM") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("Brain" = "blue", "Heart" = "red", "Liver" = "green", "Testis" = "purple"))
dev.off()


#### Snora30

Snora30 <- filtered_cpm[4,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Snora30), varnames = c("snoRNA", "sample"), value.name = "TPM")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)

# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)


pdf("Snora30_across_mouse_tissues_TPMs_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = TPM, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$snoRNA)), x = "Tissue Type", y = "TPM") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("Brain" = "blue", "Heart" = "red", "Liver" = "green", "Testis" = "purple"))
dev.off()


#### Snord92

Snord92 <- filtered_cpm[6,]

# Reshape the data frame to a long format
long_data <- melt(as.matrix(Snord92), varnames = c("snoRNA", "sample"), value.name = "TPM")

# Convert sample column to character if not already
long_data$sample <- as.character(long_data$sample)

# Add tissue type column
long_data$tissue_type <- sapply(long_data$sample, extract_tissue_type)


pdf("Snord92_across_mouse_tissues_TPMs_09082024.pdf",height=8, width=10)
ggplot(long_data, aes(x = tissue_type, y = TPM, color = tissue_type)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  labs(title = paste("Dot Plot for", unique(long_data$snoRNA)), x = "Tissue Type", y = "TPM") +
  theme_minimal() + stat_summary(fun = "median", fun.min = "median", fun.max = "median", size = 0.3, geom = "crossbar") +
  scale_color_manual(values = c("Brain" = "blue", "Heart" = "red", "Liver" = "green", "Testis" = "purple"))
dev.off()





