## 1200 reads


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

### 1000 reads



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

### 700 reads




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

### 500 reads




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

### 250 reads




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

### 100 reads




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
