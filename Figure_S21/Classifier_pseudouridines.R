#### Testing AUCs only on pseudouridines

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


pseudoU_sites <- read.table('/no_backup/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/heidelberg_lung_samples/comparison_classifiers_08082024/pseudoU_modifications.txt', header = T)

merged2 <- merge(pseudoU_sites, merged, by="position")
#row.names(merged2) <- merged2$position

# Get column names that contain the string "tumor"
tumor_cols <- grep("tumor", colnames(merged2), ignore.case = TRUE)

# Calculate row means for tumor columns
tumor_avg <- rowMeans(merged2[, tumor_cols])

# Get column names that contain the string "normal"
normal_cols <- grep("normal", colnames(merged2), ignore.case = TRUE)

# Calculate row means for normal columns
normal_avg <- rowMeans(merged2[, normal_cols])



# Calculate the absolute difference of averages between Cancer and Normal
diff_averages <- abs(tumor_avg - normal_avg)

# Create a data frame of discovered positions
discovered_positions <- data.frame(position = merged2$position)

# Sort the data frame by the absolute difference of averages
discovered_positions <- discovered_positions[order(diff_averages, decreasing = TRUE), , drop = FALSE]

# Print the top 20 positions with the largest absolute difference of averages
top_20_positions <- discovered_positions[1:20, , drop = FALSE]
print(top_20_positions)



merged3 <- merge(top_20_positions, merged2, by="position")
row.names(merged3) <- merged3$position
merged3$position <- NULL

merged3_t <- t(merged3)


############# Classifier training


setwd("/no_backup/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/heidelberg_lung_samples")

library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)
library(reshape)
library(reshape2)
library(randomForest)

##### Using split bams as input

##### Predicting the tissue


data_all <- as.data.frame(t(merged3))

data1 <- as.data.frame(data_all[1:20, ])
data2 <- as.data.frame(data_all[21:40, ])



data1$tissue <- rownames(data1)

data1$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data1))

data2$tissue <- rownames(data2)

data2$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data2))


##### data1 will be used for training and data2 for testing

library(caret)
set.seed(123)
train_data <- data1
test_data <- data2

train_data$tissue <- sub('^([^_]+_[^_]+).*', '\\1', train_data$tissue)
test_data$tissue <- sub('^([^_]+_[^_]+).*', '\\1', test_data$tissue)

colnames(train_data) <- paste0("mod", colnames(train_data))
colnames(test_data) <- paste0("mod", colnames(test_data))

train_data$modtissue <- as.factor(train_data$modtissue)
test_data$modtissue <- as.factor(test_data$modtissue)

colnames(train_data) <- gsub(":", "_", colnames(train_data))
colnames(test_data) <- gsub(":", "_", colnames(test_data))


### Train the classifier with only 1 iteration of training
rf_model <- randomForest(modtissue ~ ., data = train_data, ntree = 500, mtry = 3, na.action = na.omit)

# Initialize rf as a list to store the models
rf <- list()

# Initialize acc as a numeric vector to store accuracy values
acc <- numeric(4)


### Train the classifier with 4 iterations of training

for (iter in 1:4){
  rf[[iter]] <- randomForest(modtissue ~ ., data = train_data, ntree = 500, mtry = 3, na.action = na.omit)
  
  # predict on test data
  predicted <- predict(rf[[iter]], newdata = test_data)
  
  # calculate accuracy
  acc[iter] <- mean(predicted == test_data$modtissue)
  
  # update training data for next iteration
  train_data <- rbind(train_data, test_data)
  train_data$modtissue <- as.factor(train_data$modtissue)
  
}



### Use the 1st iteration after retraining the model - this can be changed if needed

rf_model <- rf[[1]]

predicted <- predict(rf_model, newdata = test_data)
confusionMatrix(predicted, test_data$modtissue)




##### Adding the 1200 reads subset




input1 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1200_reads/epinano_flow/3263_normal_primary_s_1200_reads.plus_strand.per.site.csv')
input1$pos <- paste(input1$X.Ref, input1$pos, sep = ":")

input2 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1200_reads/epinano_flow/3368_normal_primary_s_1200_reads.plus_strand.per.site.csv')
input2$pos <- paste(input2$X.Ref, input2$pos, sep = ":")

input3 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1200_reads/epinano_flow/3374_normal_primary_s_1200_reads.plus_strand.per.site.csv')
input3$pos <- paste(input3$X.Ref, input3$pos, sep = ":")

input4 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1200_reads/epinano_flow/3438_normal_primary_s_1200_reads.plus_strand.per.site.csv')
input4$pos <- paste(input4$X.Ref, input4$pos, sep = ":")

input5 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1200_reads/epinano_flow/3477_normal_primary_s_1200_reads.plus_strand.per.site.csv')
input5$pos <- paste(input5$X.Ref, input5$pos, sep = ":")

input6 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1200_reads/epinano_flow/3491_normal_primary_s_1200_reads.plus_strand.per.site.csv')
input6$pos <- paste(input6$X.Ref, input6$pos, sep = ":")

input7 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1200_reads/epinano_flow/3502_normal_primary_s_1200_reads.plus_strand.per.site.csv')
input7$pos <- paste(input7$X.Ref, input7$pos, sep = ":")

input8 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1200_reads/epinano_flow/3523_normal_primary_s_1200_reads.plus_strand.per.site.csv')
input8$pos <- paste(input8$X.Ref, input8$pos, sep = ":")

input9 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1200_reads/epinano_flow/3536_normal_primary_s_1200_reads.plus_strand.per.site.csv')
input9$pos <- paste(input9$X.Ref, input9$pos, sep = ":")

input10 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1200_reads/epinano_flow/3752_normal_primary_s_1200_reads.plus_strand.per.site.csv')
input10$pos <- paste(input10$X.Ref, input10$pos, sep = ":")

input11 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1200_reads/epinano_flow/3368_tumor_primary_s_1200_reads.plus_strand.per.site.csv')
input11$pos <- paste(input11$X.Ref, input11$pos, sep = ":")

input12 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1200_reads/epinano_flow/3263_tumor_primary_s_1200_reads.plus_strand.per.site.csv')
input12$pos <- paste(input12$X.Ref, input12$pos, sep = ":")

input13 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1200_reads/epinano_flow/3374_tumor_primary_s_1200_reads.plus_strand.per.site.csv')
input13$pos <- paste(input13$X.Ref, input13$pos, sep = ":")

input14 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1200_reads/epinano_flow/3438_tumor_primary_s_1200_reads.plus_strand.per.site.csv')
input14$pos <- paste(input14$X.Ref, input14$pos, sep = ":")

input15 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1200_reads/epinano_flow/3477_tumor_primary_s_1200_reads.plus_strand.per.site.csv')
input15$pos <- paste(input15$X.Ref, input15$pos, sep = ":")

input16 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1200_reads/epinano_flow/3491_tumor_primary_s_1200_reads.plus_strand.per.site.csv')
input16$pos <- paste(input16$X.Ref, input16$pos, sep = ":")

input17 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1200_reads/epinano_flow/3502_tumor_primary_s_1200_reads.plus_strand.per.site.csv')
input17$pos <- paste(input17$X.Ref, input17$pos, sep = ":")

input18 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1200_reads/epinano_flow/3523_tumor_primary_s_1200_reads.plus_strand.per.site.csv')
input18$pos <- paste(input18$X.Ref, input18$pos, sep = ":")

input19 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1200_reads/epinano_flow/3536_tumor_primary_s_1200_reads.plus_strand.per.site.csv')
input19$pos <- paste(input19$X.Ref, input19$pos, sep = ":")

input20 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1200_reads/epinano_flow/3752_tumor_primary_s_1200_reads.plus_strand.per.site.csv')
input20$pos <- paste(input20$X.Ref, input20$pos, sep = ":")





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




merged <- merge(input1[,c("pos", "sum")], input2 [,c("pos","sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input3[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input4[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input5[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input6[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input7[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input8[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input9[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input10[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input11[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input12[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input13[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input14[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input15[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input16[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input17[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input18[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input19[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input20[,c("pos", "sum")], by = "pos", all.x = TRUE)



colnames(merged) <- c("position", "3368_normal", "3263_normal", "3374_normal", "3438_normal", "3477_normal", "3491_normal", "3502_normal", "3523_normal", "3536_normal", "3752_normal", "3368_tumor", "3263_tumor", "3374_tumor", "3438_tumor", "3477_tumor", "3491_tumor", "3502_tumor", "3523_tumor", "3536_tumor", "3752_tumor")
rownames(merged) <- merged$position

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


merged2 <- merge(top_20_positions, merged, by="position")
row.names(merged2) <- merged2$position
merged2$position <- NULL

merged2_t <- t(merged2)


#### PREDICTING ON THE SUBSET



#data_all <- as.data.frame(t(merged2))

data1$tissue <- rownames(data1)

data1$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data1))

data2 <- as.data.frame(merged2_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data2))


data2[is.na(data2)] <- 0

##### data1 will be used for training and data2 for testing

library(caret)
set.seed(123)
train_data <- data1
test_data_1200 <- data2

train_data$tissue <- sub('^([^_]+_[^_]+).*', '\\1', train_data$tissue)
test_data_1200$tissue <- sub('^([^_]+_[^_]+).*', '\\1', test_data_1200$tissue)

colnames(train_data) <- paste0("mod", colnames(train_data))
colnames(test_data_1200) <- paste0("mod", colnames(test_data_1200))

train_data$modtissue <- as.factor(train_data$modtissue)
test_data_1200$modtissue <- as.factor(test_data_1200$modtissue)

colnames(train_data) <- gsub(":", "_", colnames(train_data))
colnames(test_data_1200) <- gsub(":", "_", colnames(test_data_1200))


rf_model_1200 <- randomForest(modtissue ~ ., data = train_data, ntree = 1200, mtry = 3, na.action = na.omit)
predicted_1200 <- predict(rf_model, newdata = test_data_1200)
confusionMatrix(predicted_1200, test_data_1200$modtissue)


#### Adding the 1000 reads subset




input1 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1000_reads/epinano_flow/3263_normal_primary_s_1000_reads.plus_strand.per.site.csv')
input1$pos <- paste(input1$X.Ref, input1$pos, sep = ":")

input2 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1000_reads/epinano_flow/3368_normal_primary_s_1000_reads.plus_strand.per.site.csv')
input2$pos <- paste(input2$X.Ref, input2$pos, sep = ":")

input3 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1000_reads/epinano_flow/3374_normal_primary_s_1000_reads.plus_strand.per.site.csv')
input3$pos <- paste(input3$X.Ref, input3$pos, sep = ":")

input4 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1000_reads/epinano_flow/3438_normal_primary_s_1000_reads.plus_strand.per.site.csv')
input4$pos <- paste(input4$X.Ref, input4$pos, sep = ":")

input5 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1000_reads/epinano_flow/3477_normal_primary_s_1000_reads.plus_strand.per.site.csv')
input5$pos <- paste(input5$X.Ref, input5$pos, sep = ":")

input6 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1000_reads/epinano_flow/3491_normal_primary_s_1000_reads.plus_strand.per.site.csv')
input6$pos <- paste(input6$X.Ref, input6$pos, sep = ":")

input7 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1000_reads/epinano_flow/3502_normal_primary_s_1000_reads.plus_strand.per.site.csv')
input7$pos <- paste(input7$X.Ref, input7$pos, sep = ":")

input8 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1000_reads/epinano_flow/3523_normal_primary_s_1000_reads.plus_strand.per.site.csv')
input8$pos <- paste(input8$X.Ref, input8$pos, sep = ":")

input9 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1000_reads/epinano_flow/3536_normal_primary_s_1000_reads.plus_strand.per.site.csv')
input9$pos <- paste(input9$X.Ref, input9$pos, sep = ":")

input10 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1000_reads/epinano_flow/3752_normal_primary_s_1000_reads.plus_strand.per.site.csv')
input10$pos <- paste(input10$X.Ref, input10$pos, sep = ":")

input11 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1000_reads/epinano_flow/3368_tumor_primary_s_1000_reads.plus_strand.per.site.csv')
input11$pos <- paste(input11$X.Ref, input11$pos, sep = ":")

input12 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1000_reads/epinano_flow/3263_tumor_primary_s_1000_reads.plus_strand.per.site.csv')
input12$pos <- paste(input12$X.Ref, input12$pos, sep = ":")

input13 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1000_reads/epinano_flow/3374_tumor_primary_s_1000_reads.plus_strand.per.site.csv')
input13$pos <- paste(input13$X.Ref, input13$pos, sep = ":")

input14 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1000_reads/epinano_flow/3438_tumor_primary_s_1000_reads.plus_strand.per.site.csv')
input14$pos <- paste(input14$X.Ref, input14$pos, sep = ":")

input15 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/1000_reads/epinano_flow/3477_tumor_primary_s_1000_reads.plus_strand.per.site.csv')
input15$pos <- paste(input15$X.Ref, input15$pos, sep = ":")

input16 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1000_reads/epinano_flow/3491_tumor_primary_s_1000_reads.plus_strand.per.site.csv')
input16$pos <- paste(input16$X.Ref, input16$pos, sep = ":")

input17 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1000_reads/epinano_flow/3502_tumor_primary_s_1000_reads.plus_strand.per.site.csv')
input17$pos <- paste(input17$X.Ref, input17$pos, sep = ":")

input18 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1000_reads/epinano_flow/3523_tumor_primary_s_1000_reads.plus_strand.per.site.csv')
input18$pos <- paste(input18$X.Ref, input18$pos, sep = ":")

input19 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1000_reads/epinano_flow/3536_tumor_primary_s_1000_reads.plus_strand.per.site.csv')
input19$pos <- paste(input19$X.Ref, input19$pos, sep = ":")

input20 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/1000_reads/epinano_flow/3752_tumor_primary_s_1000_reads.plus_strand.per.site.csv')
input20$pos <- paste(input20$X.Ref, input20$pos, sep = ":")





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




merged <- merge(input1[,c("pos", "sum")], input2 [,c("pos","sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input3[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input4[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input5[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input6[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input7[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input8[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input9[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input10[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input11[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input12[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input13[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input14[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input15[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input16[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input17[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input18[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input19[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input20[,c("pos", "sum")], by = "pos", all.x = TRUE)



colnames(merged) <- c("position", "3368_normal", "3263_normal", "3374_normal", "3438_normal", "3477_normal", "3491_normal", "3502_normal", "3523_normal", "3536_normal", "3752_normal", "3368_tumor", "3263_tumor", "3374_tumor", "3438_tumor", "3477_tumor", "3491_tumor", "3502_tumor", "3523_tumor", "3536_tumor", "3752_tumor")
rownames(merged) <- merged$position

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






merged2 <- merge(top_20_positions, merged, by="position")
row.names(merged2) <- merged2$position
merged2$position <- NULL

merged2_t <- t(merged2)


#### PREDICTING ON THE SUBSET



#data_all <- as.data.frame(t(merged2))

data1$tissue <- rownames(data1)

data1$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data1))

data2 <- as.data.frame(merged2_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data2))


data2[is.na(data2)] <- 0

##### data1 will be used for training and data2 for testing

library(caret)
set.seed(123)
train_data <- data1
test_data_1000 <- data2

train_data$tissue <- sub('^([^_]+_[^_]+).*', '\\1', train_data$tissue)
test_data_1000$tissue <- sub('^([^_]+_[^_]+).*', '\\1', test_data_1000$tissue)

colnames(train_data) <- paste0("mod", colnames(train_data))
colnames(test_data_1000) <- paste0("mod", colnames(test_data_1000))

train_data$modtissue <- as.factor(train_data$modtissue)
test_data_1000$modtissue <- as.factor(test_data_1000$modtissue)

colnames(train_data) <- gsub(":", "_", colnames(train_data))
colnames(test_data_1000) <- gsub(":", "_", colnames(test_data_1000))


rf_model_1000 <- randomForest(modtissue ~ ., data = train_data, ntree = 1000, mtry = 3, na.action = na.omit)
predicted_1000 <- predict(rf_model, newdata = test_data_1000)
confusionMatrix(predicted_1000, test_data_1000$modtissue)

#### Adding the 700 reads subset




input1 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/700_reads/epinano_flow/3263_normal_primary_s_700_reads.plus_strand.per.site.csv')
input1$pos <- paste(input1$X.Ref, input1$pos, sep = ":")

input2 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/700_reads/epinano_flow/3368_normal_primary_s_700_reads.plus_strand.per.site.csv')
input2$pos <- paste(input2$X.Ref, input2$pos, sep = ":")

input3 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/700_reads/epinano_flow/3374_normal_primary_s_700_reads.plus_strand.per.site.csv')
input3$pos <- paste(input3$X.Ref, input3$pos, sep = ":")

input4 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/700_reads/epinano_flow/3438_normal_primary_s_700_reads.plus_strand.per.site.csv')
input4$pos <- paste(input4$X.Ref, input4$pos, sep = ":")

input5 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/700_reads/epinano_flow/3477_normal_primary_s_700_reads.plus_strand.per.site.csv')
input5$pos <- paste(input5$X.Ref, input5$pos, sep = ":")

input6 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/700_reads/epinano_flow/3491_normal_primary_s_700_reads.plus_strand.per.site.csv')
input6$pos <- paste(input6$X.Ref, input6$pos, sep = ":")

input7 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/700_reads/epinano_flow/3502_normal_primary_s_700_reads.plus_strand.per.site.csv')
input7$pos <- paste(input7$X.Ref, input7$pos, sep = ":")

input8 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/700_reads/epinano_flow/3523_normal_primary_s_700_reads.plus_strand.per.site.csv')
input8$pos <- paste(input8$X.Ref, input8$pos, sep = ":")

input9 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/700_reads/epinano_flow/3536_normal_primary_s_700_reads.plus_strand.per.site.csv')
input9$pos <- paste(input9$X.Ref, input9$pos, sep = ":")

input10 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/700_reads/epinano_flow/3752_normal_primary_s_700_reads.plus_strand.per.site.csv')
input10$pos <- paste(input10$X.Ref, input10$pos, sep = ":")

input11 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/700_reads/epinano_flow/3368_tumor_primary_s_700_reads.plus_strand.per.site.csv')
input11$pos <- paste(input11$X.Ref, input11$pos, sep = ":")

input12 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/700_reads/epinano_flow/3263_tumor_primary_s_700_reads.plus_strand.per.site.csv')
input12$pos <- paste(input12$X.Ref, input12$pos, sep = ":")

input13 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/700_reads/epinano_flow/3374_tumor_primary_s_700_reads.plus_strand.per.site.csv')
input13$pos <- paste(input13$X.Ref, input13$pos, sep = ":")

input14 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/700_reads/epinano_flow/3438_tumor_primary_s_700_reads.plus_strand.per.site.csv')
input14$pos <- paste(input14$X.Ref, input14$pos, sep = ":")

input15 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/700_reads/epinano_flow/3477_tumor_primary_s_700_reads.plus_strand.per.site.csv')
input15$pos <- paste(input15$X.Ref, input15$pos, sep = ":")

input16 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/700_reads/epinano_flow/3491_tumor_primary_s_700_reads.plus_strand.per.site.csv')
input16$pos <- paste(input16$X.Ref, input16$pos, sep = ":")

input17 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/700_reads/epinano_flow/3502_tumor_primary_s_700_reads.plus_strand.per.site.csv')
input17$pos <- paste(input17$X.Ref, input17$pos, sep = ":")

input18 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/700_reads/epinano_flow/3523_tumor_primary_s_700_reads.plus_strand.per.site.csv')
input18$pos <- paste(input18$X.Ref, input18$pos, sep = ":")

input19 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/700_reads/epinano_flow/3536_tumor_primary_s_700_reads.plus_strand.per.site.csv')
input19$pos <- paste(input19$X.Ref, input19$pos, sep = ":")

input20 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/700_reads/epinano_flow/3752_tumor_primary_s_700_reads.plus_strand.per.site.csv')
input20$pos <- paste(input20$X.Ref, input20$pos, sep = ":")





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




merged <- merge(input1[,c("pos", "sum")], input2 [,c("pos","sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input3[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input4[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input5[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input6[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input7[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input8[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input9[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input10[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input11[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input12[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input13[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input14[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input15[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input16[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input17[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input18[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input19[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input20[,c("pos", "sum")], by = "pos", all.x = TRUE)



colnames(merged) <- c("position", "3368_normal", "3263_normal", "3374_normal", "3438_normal", "3477_normal", "3491_normal", "3502_normal", "3523_normal", "3536_normal", "3752_normal", "3368_tumor", "3263_tumor", "3374_tumor", "3438_tumor", "3477_tumor", "3491_tumor", "3502_tumor", "3523_tumor", "3536_tumor", "3752_tumor")
rownames(merged) <- merged$position

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






merged2 <- merge(top_20_positions, merged, by="position")
row.names(merged2) <- merged2$position
merged2$position <- NULL

merged2_t <- t(merged2)


#### PREDICTING ON THE SUBSET



#data_all <- as.data.frame(t(merged2))

data1$tissue <- rownames(data1)

data1$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data1))

data2 <- as.data.frame(merged2_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data2))


data2[is.na(data2)] <- 0

##### data1 will be used for training and data2 for testing

library(caret)
set.seed(123)
train_data <- data1
test_data_700 <- data2

train_data$tissue <- sub('^([^_]+_[^_]+).*', '\\1', train_data$tissue)
test_data_700$tissue <- sub('^([^_]+_[^_]+).*', '\\1', test_data_700$tissue)

colnames(train_data) <- paste0("mod", colnames(train_data))
colnames(test_data_700) <- paste0("mod", colnames(test_data_700))

train_data$modtissue <- as.factor(train_data$modtissue)
test_data_700$modtissue <- as.factor(test_data_700$modtissue)

colnames(train_data) <- gsub(":", "_", colnames(train_data))
colnames(test_data_700) <- gsub(":", "_", colnames(test_data_700))


rf_model_700 <- randomForest(modtissue ~ ., data = train_data, ntree = 700, mtry = 3, na.action = na.omit)
predicted_700 <- predict(rf_model, newdata = test_data_700)
confusionMatrix(predicted_700, test_data_700$modtissue)


#### Adding the 500 reads subset





input1 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/500_reads_repeated/epinano_output/epinano_flow/3263_normal_primary_s_500_reads.plus_strand.per.site.csv')
input1$pos <- paste(input1$X.Ref, input1$pos, sep = ":")

input2 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/500_reads_repeated/epinano_output/epinano_flow/3368_normal_primary_s_500_reads.plus_strand.per.site.csv')
input2$pos <- paste(input2$X.Ref, input2$pos, sep = ":")

input3 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/500_reads_repeated/epinano_output/epinano_flow/3374_normal_primary_s_500_reads.plus_strand.per.site.csv')
input3$pos <- paste(input3$X.Ref, input3$pos, sep = ":")

input4 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/500_reads_repeated/epinano_output/epinano_flow/3438_normal_primary_s_500_reads.plus_strand.per.site.csv')
input4$pos <- paste(input4$X.Ref, input4$pos, sep = ":")

input5 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/500_reads_repeated/epinano_output/epinano_flow/3477_normal_primary_s_500_reads.plus_strand.per.site.csv')
input5$pos <- paste(input5$X.Ref, input5$pos, sep = ":")

input6 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/500_reads_repeated/epinano_output/epinano_flow/epinano_flow/3491_normal_primary_s_500_reads.plus_strand.per.site.csv')
input6$pos <- paste(input6$X.Ref, input6$pos, sep = ":")

input7 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/500_reads_repeated/epinano_output/epinano_flow/epinano_flow/3502_normal_primary_s_500_reads.plus_strand.per.site.csv')
input7$pos <- paste(input7$X.Ref, input7$pos, sep = ":")

input8 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/500_reads_repeated/epinano_output/epinano_flow/epinano_flow/3523_normal_primary_s_500_reads.plus_strand.per.site.csv')
input8$pos <- paste(input8$X.Ref, input8$pos, sep = ":")

input9 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/500_reads_repeated/epinano_output/epinano_flow/epinano_flow/3536_normal_primary_s_500_reads.plus_strand.per.site.csv')
input9$pos <- paste(input9$X.Ref, input9$pos, sep = ":")

input10 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/500_reads_repeated/epinano_output/epinano_flow/epinano_flow/3752_normal_primary_s_500_reads.plus_strand.per.site.csv')
input10$pos <- paste(input10$X.Ref, input10$pos, sep = ":")

input11 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/500_reads_repeated/epinano_output/epinano_flow/3368_tumor_primary_s_500_reads.plus_strand.per.site.csv')
input11$pos <- paste(input11$X.Ref, input11$pos, sep = ":")

input12 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/500_reads_repeated/epinano_output/epinano_flow/3263_tumor_primary_s_500_reads.plus_strand.per.site.csv')
input12$pos <- paste(input12$X.Ref, input12$pos, sep = ":")

input13 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/500_reads_repeated/epinano_output/epinano_flow/3374_tumor_primary_s_500_reads.plus_strand.per.site.csv')
input13$pos <- paste(input13$X.Ref, input13$pos, sep = ":")

input14 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/500_reads_repeated/epinano_output/epinano_flow/3438_tumor_primary_s_500_reads.plus_strand.per.site.csv')
input14$pos <- paste(input14$X.Ref, input14$pos, sep = ":")

input15 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/500_reads_repeated/epinano_output/epinano_flow/3477_tumor_primary_s_500_reads.plus_strand.per.site.csv')
input15$pos <- paste(input15$X.Ref, input15$pos, sep = ":")

input16 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/500_reads_repeated/epinano_output/epinano_flow/epinano_flow/3491_tumor_primary_s_500_reads.plus_strand.per.site.csv')
input16$pos <- paste(input16$X.Ref, input16$pos, sep = ":")

input17 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/500_reads_repeated/epinano_output/epinano_flow/epinano_flow/3502_tumor_primary_s_500_reads.plus_strand.per.site.csv')
input17$pos <- paste(input17$X.Ref, input17$pos, sep = ":")

input18 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/500_reads_repeated/epinano_output/epinano_flow/epinano_flow/3523_tumor_primary_s_500_reads.plus_strand.per.site.csv')
input18$pos <- paste(input18$X.Ref, input18$pos, sep = ":")

input19 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/500_reads_repeated/epinano_output/epinano_flow/epinano_flow/3536_tumor_primary_s_500_reads.plus_strand.per.site.csv')
input19$pos <- paste(input19$X.Ref, input19$pos, sep = ":")

input20 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/500_reads_repeated/epinano_output/epinano_flow/epinano_flow/3752_tumor_primary_s_500_reads.plus_strand.per.site.csv')
input20$pos <- paste(input20$X.Ref, input20$pos, sep = ":")





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




merged <- merge(input1[,c("pos", "sum")], input2 [,c("pos","sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input3[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input4[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input5[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input6[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input7[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input8[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input9[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input10[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input11[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input12[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input13[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input14[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input15[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input16[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input17[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input18[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input19[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input20[,c("pos", "sum")], by = "pos", all.x = TRUE)



colnames(merged) <- c("position", "3368_normal", "3263_normal", "3374_normal", "3438_normal", "3477_normal", "3491_normal", "3502_normal", "3523_normal", "3536_normal", "3752_normal", "3368_tumor", "3263_tumor", "3374_tumor", "3438_tumor", "3477_tumor", "3491_tumor", "3502_tumor", "3523_tumor", "3536_tumor", "3752_tumor")
rownames(merged) <- merged$position

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






merged2 <- merge(top_20_positions, merged, by="position")
row.names(merged2) <- merged2$position
merged2$position <- NULL

merged2_t <- t(merged2)


#### PREDICTING ON THE SUBSET



#data_all <- as.data.frame(t(merged2))

data1$tissue <- rownames(data1)

data1$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data1))

data2 <- as.data.frame(merged2_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data2))


data2[is.na(data2)] <- 0

##### data1 will be used for training and data2 for testing

library(caret)
set.seed(123)
train_data <- data1
test_data_500 <- data2

train_data$tissue <- sub('^([^_]+_[^_]+).*', '\\1', train_data$tissue)
test_data_500$tissue <- sub('^([^_]+_[^_]+).*', '\\1', test_data_500$tissue)

colnames(train_data) <- paste0("mod", colnames(train_data))
colnames(test_data_500) <- paste0("mod", colnames(test_data_500))

train_data$modtissue <- as.factor(train_data$modtissue)
test_data_500$modtissue <- as.factor(test_data_500$modtissue)

colnames(train_data) <- gsub(":", "_", colnames(train_data))
colnames(test_data_500) <- gsub(":", "_", colnames(test_data_500))


rf_model_500 <- randomForest(modtissue ~ ., data = train_data, ntree = 500, mtry = 3, na.action = na.omit)
predicted_500 <- predict(rf_model, newdata = test_data_500)
confusionMatrix(predicted_500, test_data_500$modtissue)


#### Adding the 250 reads subset




input1 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/250_reads/epinano_flow/3263_normal_primary_s_250_reads.plus_strand.per.site.csv')
input1$pos <- paste(input1$X.Ref, input1$pos, sep = ":")

input2 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/250_reads/epinano_flow/3368_normal_primary_s_250_reads.plus_strand.per.site.csv')
input2$pos <- paste(input2$X.Ref, input2$pos, sep = ":")

input3 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/250_reads/epinano_flow/3374_normal_primary_s_250_reads.plus_strand.per.site.csv')
input3$pos <- paste(input3$X.Ref, input3$pos, sep = ":")

input4 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/250_reads/epinano_flow/3438_normal_primary_s_250_reads.plus_strand.per.site.csv')
input4$pos <- paste(input4$X.Ref, input4$pos, sep = ":")

input5 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/250_reads/epinano_flow/3477_normal_primary_s_250_reads.plus_strand.per.site.csv')
input5$pos <- paste(input5$X.Ref, input5$pos, sep = ":")

input6 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/250_reads/epinano_flow/3491_normal_primary_s_250_reads.plus_strand.per.site.csv')
input6$pos <- paste(input6$X.Ref, input6$pos, sep = ":")

input7 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/250_reads/epinano_flow/3502_normal_primary_s_250_reads.plus_strand.per.site.csv')
input7$pos <- paste(input7$X.Ref, input7$pos, sep = ":")

input8 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/250_reads/epinano_flow/3523_normal_primary_s_250_reads.plus_strand.per.site.csv')
input8$pos <- paste(input8$X.Ref, input8$pos, sep = ":")

input9 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/250_reads/epinano_flow/3536_normal_primary_s_250_reads.plus_strand.per.site.csv')
input9$pos <- paste(input9$X.Ref, input9$pos, sep = ":")

input10 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/250_reads/epinano_flow/3752_normal_primary_s_250_reads.plus_strand.per.site.csv')
input10$pos <- paste(input10$X.Ref, input10$pos, sep = ":")

input11 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/250_reads/epinano_flow/3368_tumor_primary_s_250_reads.plus_strand.per.site.csv')
input11$pos <- paste(input11$X.Ref, input11$pos, sep = ":")

input12 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/250_reads/epinano_flow/3263_tumor_primary_s_250_reads.plus_strand.per.site.csv')
input12$pos <- paste(input12$X.Ref, input12$pos, sep = ":")

input13 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/250_reads/epinano_flow/3374_tumor_primary_s_250_reads.plus_strand.per.site.csv')
input13$pos <- paste(input13$X.Ref, input13$pos, sep = ":")

input14 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/250_reads/epinano_flow/3438_tumor_primary_s_250_reads.plus_strand.per.site.csv')
input14$pos <- paste(input14$X.Ref, input14$pos, sep = ":")

input15 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/250_reads/epinano_flow/3477_tumor_primary_s_250_reads.plus_strand.per.site.csv')
input15$pos <- paste(input15$X.Ref, input15$pos, sep = ":")

input16 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/250_reads/epinano_flow/3491_tumor_primary_s_250_reads.plus_strand.per.site.csv')
input16$pos <- paste(input16$X.Ref, input16$pos, sep = ":")

input17 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/250_reads/epinano_flow/3502_tumor_primary_s_250_reads.plus_strand.per.site.csv')
input17$pos <- paste(input17$X.Ref, input17$pos, sep = ":")

input18 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/250_reads/epinano_flow/3523_tumor_primary_s_250_reads.plus_strand.per.site.csv')
input18$pos <- paste(input18$X.Ref, input18$pos, sep = ":")

input19 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/250_reads/epinano_flow/3536_tumor_primary_s_250_reads.plus_strand.per.site.csv')
input19$pos <- paste(input19$X.Ref, input19$pos, sep = ":")

input20 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/250_reads/epinano_flow/3752_tumor_primary_s_250_reads.plus_strand.per.site.csv')
input20$pos <- paste(input20$X.Ref, input20$pos, sep = ":")





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




merged <- merge(input1[,c("pos", "sum")], input2 [,c("pos","sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input3[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input4[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input5[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input6[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input7[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input8[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input9[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input10[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input11[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input12[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input13[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input14[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input15[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input16[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input17[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input18[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input19[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input20[,c("pos", "sum")], by = "pos", all.x = TRUE)



colnames(merged) <- c("position", "3368_normal", "3263_normal", "3374_normal", "3438_normal", "3477_normal", "3491_normal", "3502_normal", "3523_normal", "3536_normal", "3752_normal", "3368_tumor", "3263_tumor", "3374_tumor", "3438_tumor", "3477_tumor", "3491_tumor", "3502_tumor", "3523_tumor", "3536_tumor", "3752_tumor")
rownames(merged) <- merged$position

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






merged2 <- merge(top_20_positions, merged, by="position")
row.names(merged2) <- merged2$position
merged2$position <- NULL

merged2_t <- t(merged2)


#### PREDICTING ON THE SUBSET



#data_all <- as.data.frame(t(merged2))

data1$tissue <- rownames(data1)

data1$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data1))

data2 <- as.data.frame(merged2_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data2))


data2[is.na(data2)] <- 0

##### data1 will be used for training and data2 for testing

library(caret)
set.seed(123)
train_data <- data1
test_data_250 <- data2

train_data$tissue <- sub('^([^_]+_[^_]+).*', '\\1', train_data$tissue)
test_data_250$tissue <- sub('^([^_]+_[^_]+).*', '\\1', test_data_250$tissue)

colnames(train_data) <- paste0("mod", colnames(train_data))
colnames(test_data_250) <- paste0("mod", colnames(test_data_250))

train_data$modtissue <- as.factor(train_data$modtissue)
test_data_250$modtissue <- as.factor(test_data_250$modtissue)

colnames(train_data) <- gsub(":", "_", colnames(train_data))
colnames(test_data_250) <- gsub(":", "_", colnames(test_data_250))


rf_model_250 <- randomForest(modtissue ~ ., data = train_data, ntree = 500, mtry = 3, na.action = na.omit)
predicted_250 <- predict(rf_model, newdata = test_data_250)
confusionMatrix(predicted_250, test_data_250$modtissue)


#### Adding the 100 reads subset




input1 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/100_reads/epinano_flow/3263_normal_primary_s_100_reads.plus_strand.per.site.csv')
input1$pos <- paste(input1$X.Ref, input1$pos, sep = ":")

input2 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/100_reads/epinano_flow/3368_normal_primary_s_100_reads.plus_strand.per.site.csv')
input2$pos <- paste(input2$X.Ref, input2$pos, sep = ":")

input3 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/100_reads/epinano_flow/3374_normal_primary_s_100_reads.plus_strand.per.site.csv')
input3$pos <- paste(input3$X.Ref, input3$pos, sep = ":")

input4 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/100_reads/epinano_flow/3438_normal_primary_s_100_reads.plus_strand.per.site.csv')
input4$pos <- paste(input4$X.Ref, input4$pos, sep = ":")

input5 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/100_reads/epinano_flow/3477_normal_primary_s_100_reads.plus_strand.per.site.csv')
input5$pos <- paste(input5$X.Ref, input5$pos, sep = ":")

input6 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/100_reads/epinano_flow/3491_normal_primary_s_100_reads.plus_strand.per.site.csv')
input6$pos <- paste(input6$X.Ref, input6$pos, sep = ":")

input7 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/100_reads/epinano_flow/3502_normal_primary_s_100_reads.plus_strand.per.site.csv')
input7$pos <- paste(input7$X.Ref, input7$pos, sep = ":")

input8 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/100_reads/epinano_flow/3523_normal_primary_s_100_reads.plus_strand.per.site.csv')
input8$pos <- paste(input8$X.Ref, input8$pos, sep = ":")

input9 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/100_reads/epinano_flow/3536_normal_primary_s_100_reads.plus_strand.per.site.csv')
input9$pos <- paste(input9$X.Ref, input9$pos, sep = ":")

input10 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/100_reads/epinano_flow/3752_normal_primary_s_100_reads.plus_strand.per.site.csv')
input10$pos <- paste(input10$X.Ref, input10$pos, sep = ":")

input11 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/100_reads/epinano_flow/3368_tumor_primary_s_100_reads.plus_strand.per.site.csv')
input11$pos <- paste(input11$X.Ref, input11$pos, sep = ":")

input12 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/100_reads/epinano_flow/3263_tumor_primary_s_100_reads.plus_strand.per.site.csv')
input12$pos <- paste(input12$X.Ref, input12$pos, sep = ":")

input13 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/100_reads/epinano_flow/3374_tumor_primary_s_100_reads.plus_strand.per.site.csv')
input13$pos <- paste(input13$X.Ref, input13$pos, sep = ":")

input14 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/100_reads/epinano_flow/3438_tumor_primary_s_100_reads.plus_strand.per.site.csv')
input14$pos <- paste(input14$X.Ref, input14$pos, sep = ":")

input15 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool1/subsampling/epinano_output/100_reads/epinano_flow/3477_tumor_primary_s_100_reads.plus_strand.per.site.csv')
input15$pos <- paste(input15$X.Ref, input15$pos, sep = ":")

input16 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/100_reads/epinano_flow/3491_tumor_primary_s_100_reads.plus_strand.per.site.csv')
input16$pos <- paste(input16$X.Ref, input16$pos, sep = ":")

input17 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/100_reads/epinano_flow/3502_tumor_primary_s_100_reads.plus_strand.per.site.csv')
input17$pos <- paste(input17$X.Ref, input17$pos, sep = ":")

input18 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/100_reads/epinano_flow/3523_tumor_primary_s_100_reads.plus_strand.per.site.csv')
input18$pos <- paste(input18$X.Ref, input18$pos, sep = ":")

input19 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/100_reads/epinano_flow/3536_tumor_primary_s_100_reads.plus_strand.per.site.csv')
input19$pos <- paste(input19$X.Ref, input19$pos, sep = ":")

input20 <- read.csv('/users/enovoa/imilenkovic/heidelberg_samples_lung/flowcell2_pool2/subsampling/epinano_output/100_reads/epinano_flow/3752_tumor_primary_s_100_reads.plus_strand.per.site.csv')
input20$pos <- paste(input20$X.Ref, input20$pos, sep = ":")





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




merged <- merge(input1[,c("pos", "sum")], input2 [,c("pos","sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input3[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input4[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input5[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input6[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input7[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input8[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input9[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input10[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input11[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input12[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input13[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input14[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input15[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input16[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input17[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input18[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input19[,c("pos", "sum")], by = "pos", all.x = TRUE)
merged <- merge (merged, input20[,c("pos", "sum")], by = "pos", all.x = TRUE)



colnames(merged) <- c("position", "3368_normal", "3263_normal", "3374_normal", "3438_normal", "3477_normal", "3491_normal", "3502_normal", "3523_normal", "3536_normal", "3752_normal", "3368_tumor", "3263_tumor", "3374_tumor", "3438_tumor", "3477_tumor", "3491_tumor", "3502_tumor", "3523_tumor", "3536_tumor", "3752_tumor")
rownames(merged) <- merged$position

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






merged2 <- merge(top_20_positions, merged, by="position")
row.names(merged2) <- merged2$position
merged2$position <- NULL

merged2_t <- t(merged2)


#### PREDICTING ON THE SUBSET



#data_all <- as.data.frame(t(merged2))

data1$tissue <- rownames(data1)

data1$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data1))

data2 <- as.data.frame(merged2_t)

data2$tissue <- rownames(data2)

data2$tissue <- sub('.*?_([^_]+)$', '\\1', rownames(data2))


data2[is.na(data2)] <- 0

##### data1 will be used for training and data2 for testing

library(caret)
set.seed(123)
train_data <- data1
test_data_100 <- data2

train_data$tissue <- sub('^([^_]+_[^_]+).*', '\\1', train_data$tissue)
test_data_100$tissue <- sub('^([^_]+_[^_]+).*', '\\1', test_data_100$tissue)

colnames(train_data) <- paste0("mod", colnames(train_data))
colnames(test_data_100) <- paste0("mod", colnames(test_data_100))

train_data$modtissue <- as.factor(train_data$modtissue)
test_data_100$modtissue <- as.factor(test_data_100$modtissue)

colnames(train_data) <- gsub(":", "_", colnames(train_data))
colnames(test_data_100) <- gsub(":", "_", colnames(test_data_100))


rf_model_100 <- randomForest(modtissue ~ ., data = train_data, ntree = 500, mtry = 3, na.action = na.omit)
predicted_100 <- predict(rf_model, newdata = test_data_100)
confusionMatrix(predicted_100, test_data_100$modtissue)



###### Commparing AUCs

setwd("/no_backup/enovoa/nextflow_outputs/human_rRNA_cancer_ivan/heidelberg_lung_samples/subsets_new_order_only_one_model_04092023")

predictions_percentage_1200 <- predicted_1200
predictions_percentage_1000 <- predicted_1000
predictions_percentage_700 <- predicted_700
predictions_percentage_500 <- predicted_500
predictions_percentage_250 <- predicted_250
predictions_percentage_100 <- predicted_100

base_models <- c("rf_model_1200", "rf_model_1000", "rf_model_700", "rf_model_500", "rf_model_250", "rf_model_100")
object_names <- c("predictions_percentage_1200", "predictions_percentage_1000", "predictions_percentage_700", "predictions_percentage_500", "predictions_percentage_250", "predictions_percentage_100")
test_data_names <- c("test_data_1200", "test_data_1000", "test_data_700", "test_data_500", "test_data_250", "test_data_100")


# Load the required libraries
library(pROC)
library(ggplot2)



# Define the base model names - IMPORTANT: this part is created in the script where the RF classifiers are trained

base_models <- c("rf_model_1200", "rf_model_1000", "rf_model_700", "rf_model_500", "rf_model_250", "rf_model_100")


# Define the corresponding object names
object_names <- c("predictions_percentage_1200", "predictions_percentage_1000", "predictions_percentage_700", "predictions_percentage_500", "predictions_percentage_250", "predictions_percentage_100")

# Define the corresponding test data names
test_data_names <- c("test_data_1200", "test_data_1000", "test_data_700", "test_data_500", "test_data_250", "test_data_100")


# Create the objects using a loop
for (i in seq_along(base_models)) {
  # Get the model object using the base model name
  model_object <- get(base_models[i])
  
  # Get the test data object using the corresponding test data name
  test_data_object <- get(test_data_names[i])
  
  # Create the object using the predicted probabilities
  assign(object_names[i], as.data.frame(predict(model_object, test_data_object, type = "prob")))
}



# Define the object names and test data names
object_names <- c("predictions_percentage_1200", "predictions_percentage_1000", "predictions_percentage_700", "predictions_percentage_500", "predictions_percentage_250", "predictions_percentage_100")
test_data_names <- c("test_data_1200", "test_data_1000", "test_data_700", "test_data_500", "test_data_250", "test_data_100")

# Perform the code for each object
for (i in seq_along(object_names)) {
  # Get the object using the object name
  predictions <- get(object_names[i])
  
  # Get the corresponding test data object
  test_data_object <- get(test_data_names[i])
  
  # Predict class and attach the observed test class
  predictions$predict <- names(predictions)[1:2][max.col(predictions[, 1:2])]
  predictions$observed <- test_data_object$modtissue
  
  # Create a binary vector indicating whether the prediction was accurate or not
  predictions$accurate <- ifelse(predictions$predict == predictions$observed, 1, 0)
  
  # Assign the updated object back to the environment
  assign(object_names[i], predictions)
}



# Define the object names  
object_names <- c("predictions_percentage_1200", "predictions_percentage_1000", "predictions_percentage_700", "predictions_percentage_500", "predictions_percentage_250", "predictions_percentage_100")

# Create a list to store the AUC values
auc_values <- vector("list", length(object_names))

# Define the class names
class_names <- c("tumor", "normal")

# Initialize a list to store ROC curve objects
roc_curves_list <- vector("list", length(object_names))

# Perform the code for each object
for (i in seq_along(object_names)) {
  # Get the object using the object name
  predictions <- get(object_names[i])
  
  # Extract the class probabilities
  class_probs <- as.matrix(predictions[, class_names])
  
  # Initialize a list to store binary response vectors for each class
  binary_responses <- vector("list", length(class_names))
  
  # Create a binary response vector for each class
  for (j in seq_along(class_names)) {
    binary_response <- ifelse(predictions$observed == class_names[j], 1, 0)
    binary_responses[[j]] <- binary_response
  }
  
  # Combine the ROC curves for all classes using multiclass.roc
  multiclass_roc <- multiclass.roc(as.numeric(unlist(binary_responses)), as.numeric(as.vector(class_probs)))
  
  # Calculate AUC for the multiclass ROC curve
  auc_value <- multiclass_roc$auc[1]
  
  # Store the AUC value in the list
  auc_values[[i]] <- auc_value
  
  # Store the multiclass_roc object in the list with appropriate names
  assign(paste0("multiclass_roc_", object_names[i]), multiclass_roc)
}




# Function to convert multiclass ROC curve to a data.frame
convert_roc_to_dataframe <- function(roc_curve) {
  data.frame(FPR = roc_curve$specificities[[1]], TPR = roc_curve$sensitivities[[1]])
}

# Create a data.frame with combined ROC curves and AUC values
roc_data_list <- lapply(seq_along(roc_curves_list), function(i) {
  data <- convert_roc_to_dataframe(roc_curves_list[[i]])
  if (nrow(data) > 0) {
    data$Classifier <- object_names[i]
    data$AUC <- auc_values[[i]]
  }
  data
})

# Filter out any empty data.frames
roc_data_list <- Filter(function(data) nrow(data) > 0, roc_data_list)

# Combine the data.frames
roc_data <- do.call(rbind, roc_data_list)


# Print the AUC values
for (i in seq_along(object_names)) {
  cat("AUC for", object_names[i], ":", auc_values[[i]], "\n")
}



############ PLOTTING THE ROCs






# Define the object names
object_names <- c("predictions_percentage_1200", "predictions_percentage_1000", "predictions_percentage_700", "predictions_percentage_500", "predictions_percentage_250", "predictions_percentage_100")


# Create a list to store the multiclass_roc objects
multiclass_roc_list <- vector("list", length(object_names))

# Perform the code for each object
for (i in seq_along(object_names)) {
  # Get the object using the object name
  predictions <- get(object_names[i])
  
  # Extract the class probabilities
  class_probs <- as.matrix(predictions[, c("tumor", "normal")])
  
  # Convert the observed tissue column to a factor with appropriate levels
  observed_tissue <- factor(predictions$observed, levels = colnames(class_probs))
  
  # Create the multiclass ROC object
  multiclass_roc <- multiclass.roc(observed_tissue, class_probs)
  
  # Store the multiclass ROC object in the list
  multiclass_roc_list[[i]] <- multiclass_roc
}


assign(paste0("multiclass_roc_percentage_", 1:6), multiclass_roc_list)



# Access the first multiclass_roc object
multiclass_roc_percentage_1200 <- multiclass_roc_list[[1]]

# Access the second multiclass_roc object
multiclass_roc_percentage_1000 <- multiclass_roc_list[[2]]

# Access the third multiclass_roc object
multiclass_roc_percentage_700 <- multiclass_roc_list[[3]]

# Access the fourth multiclass_roc object
multiclass_roc_percentage_500 <- multiclass_roc_list[[4]]

# Access the fifth multiclass_roc object
multiclass_roc_percentage_250 <- multiclass_roc_list[[5]]

# Access the sixth multiclass_roc object
multiclass_roc_percentage_100 <- multiclass_roc_list[[6]]






# Extract the ROC curve for predictions_percentage_5
roc_curve_1200 <- multiclass_roc_predictions_percentage_1200$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_1200 <- roc_curve_1200$specificities
sensitivities_1200 <- roc_curve_1200$sensitivities

# Extract the ROC curve for predictions_percentage_1000
roc_curve_1000 <- multiclass_roc_predictions_percentage_1000$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_1000 <- roc_curve_1000$specificities
sensitivities_1000 <- roc_curve_1000$sensitivities

# Extract the ROC curve for predictions_percentage_700
roc_curve_700 <- multiclass_roc_predictions_percentage_700$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_700 <- roc_curve_700$specificities
sensitivities_700 <- roc_curve_700$sensitivities

# Extract the ROC curve for predictions_percentage_500
roc_curve_500 <- multiclass_roc_predictions_percentage_500$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_500 <- roc_curve_500$specificities
sensitivities_500 <- roc_curve_500$sensitivities

# Extract the ROC curve for predictions_percentage_250
roc_curve_250 <- multiclass_roc_predictions_percentage_250$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_250 <- roc_curve_250$specificities
sensitivities_250 <- roc_curve_250$sensitivities

# Extract the ROC curve for predictions_percentage_100
roc_curve_100 <- multiclass_roc_predictions_percentage_100$rocs[[1]]  # Assuming predictions_percentage_01 is the first class

# Extract specificities and sensitivities from the ROC curve
specificities_100 <- roc_curve_100$specificities
sensitivities_100 <- roc_curve_100$sensitivities




# Create a data frame for plotting
roc_data_1200 <- data.frame(Specificity = specificities_1200, Sensitivity = sensitivities_1200)
roc_data_1000 <- data.frame(Specificity = specificities_1000, Sensitivity = sensitivities_1000)
roc_data_700 <- data.frame(Specificity = specificities_700, Sensitivity = sensitivities_700)
roc_data_500 <- data.frame(Specificity = specificities_500, Sensitivity = sensitivities_500)
roc_data_250 <- data.frame(Specificity = specificities_250, Sensitivity = sensitivities_250)
roc_data_100 <- data.frame(Specificity = specificities_100, Sensitivity = sensitivities_100)



library(ggplot2)

# Create a list of roc_data objects
roc_data_list <- list(
  roc_data_1200,
  roc_data_1000,
  roc_data_700,
  roc_data_500,
  roc_data_250,
  roc_data_100
)

# Create a vector of colors for each ROC curve
roc_colors <- c("red", "blue", "green", "purple", "orange", "yellow")

# Create a ggplot object with all ROC curves
p <- ggplot() +
  geom_line(data = roc_data_list[[1]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[1]) +
  geom_line(data = roc_data_list[[2]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[2]) +
  geom_line(data = roc_data_list[[3]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[3]) +
  geom_line(data = roc_data_list[[4]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[4]) +
  geom_line(data = roc_data_list[[5]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[5]) +
  geom_line(data = roc_data_list[[6]], aes(x = 1 - Specificity, y = Sensitivity), color = roc_colors[6]) +
  xlim(0, 1) +
  ylim(0, 1) +
  xlab("False Positive Rate (1 - Specificity)") +
  ylab("True Positive Rate (Sensitivity)") +
  ggtitle("ROC Curves for Different Classifiers") +
  theme_bw()

# Print the plot
print(p)

# Save the plot
pdf("ROC_curves_classifiers_PseudoUs_23082024.pdf", height = 8, width = 8)
print(p)
dev.off()



