### .csv epinano ouput files can be found in the Figure_1/epinano_outputs folder.

# Load libraries
library('plyr')
library('ggplot2')
library('ggrepel')

# Function to read and filter input data
read_and_filter <- function(file) {
  data <- read.csv(file)
  data <- data[grepl("18S", data$X.Ref),]
  data$sum <- data$mis + data$del + data$ins
  return(data[, c("pos", "sum")])
}

# List of input files
files <- c(
  'brain_embryo_rep1.csv', 'brain_embryo_rep2.csv', 'brain_newborn_rep1.csv',
  'brain_newborn_rep2.csv', 'brain_adult_rep1.csv', 'brain_adult_rep2.csv',
  'mESc_rep1.csv', 'mESc_rep2.csv', 'NPC_rep1.csv', 'NPC_rep2.csv',
  'Neurons_rep1.csv', 'Neurons_rep2.csv')

# Read and filter input data
inputs <- lapply(files, read_and_filter)

# Merge input data
merged <- Reduce(function(x, y) merge(x, y, by = "pos"), inputs)

# Set column names
colnames(merged) <- c("position", "brain_embryo_rep1", "brain_embryo_rep2", 
                      "brain_newborn_rep1", "brain_newborn_rep2", "brain_adult_rep1",
                      "brain_adult_rep2", "mESc_rep1", "mESc_rep2", "NPC_rep1",
                      "NPC_rep2", "Neurons_rep1", "Neurons_rep2")


annotated_positions <- read.table("18S_annotated_positions_with_mod_type.txt")
colnames(annotated_positions) <- c("position", "type")


merged2 <- merge(annotated_positions, merged, by="position")
merged2$position <- sub("^", "18S:", merged2$position)
row.names(merged2) <- merged2$position
merged2$position <- NULL

merged_18S_tissues <- merged2

### 28S

# Function to read and filter input data
read_and_filter <- function(file) {
  data <- read.csv(file)
  data <- data[grepl("28S", data$X.Ref),]
  data$sum <- data$mis + data$del + data$ins
  return(data[, c("pos", "sum")])
}

# List of input files
files <- c(
  'brain_embryo_rep1.csv', 'brain_embryo_rep2.csv', 'brain_newborn_rep1.csv',
  'brain_newborn_rep2.csv', 'brain_adult_rep1.csv', 'brain_adult_rep2.csv',
  'mESc_rep1.csv', 'mESc_rep2.csv', 'NPC_rep1.csv', 'NPC_rep2.csv',
  'Neurons_rep1.csv', 'Neurons_rep2.csv')

# Read and filter input data
inputs <- lapply(files, read_and_filter)

# Merge input data
merged <- Reduce(function(x, y) merge(x, y, by = "pos"), inputs)

# Set column names
colnames(merged) <- c("position", "brain_embryo_rep1", "brain_embryo_rep2", 
                      "brain_newborn_rep1", "brain_newborn_rep2", "brain_adult_rep1",
                      "brain_adult_rep2", "mESc_rep1", "mESc_rep2", "NPC_rep1",
                      "NPC_rep2", "Neurons_rep1", "Neurons_rep2")

# Read the annotated positions table
annotated_positions <- read.table("28S_annotated_positions_with_mod_type.txt")

# Change column names of the annotated positions table
colnames(annotated_positions) <- c("position", "type")

# Merge the annotated positions with the data frame containing all positions
merged2 <- merge(annotated_positions, merged, by="position")

merged2$position <- sub("^", "28S:", merged2$position)

row.names(merged2) <- merged2$position

merged2$position <- NULL

merged_28S_tissues <- merged2

merged_all_tissues_18S_28S <- rbind(merged_18S_tissues, merged_28S_tissues)

mod_types <- as.data.frame(merged_all_tissues_18S_28S$type)
rownames(mod_types) <- rownames(merged_all_tissues_18S_28S)
colnames(mod_types) <- "type"

merged_all_tissues_18S_28S$type <- NULL
merged_all_tissues_18S_28S_matrix <- as.matrix(merged_all_tissues_18S_28S)


library(ComplexHeatmap)
library(circlize)

row_ha <- rowAnnotation(df = mod_types)



Heatmap(merged_all_tissues_18S_28S_matrix, name = "SumErr", 
        col = colorRamp2(c(0, 0.25, 0.5, 0.75, 1), c("#2c7bb6","#abd9e9","floralwhite","#fdae61", "#d7191c"),space = "RGB"),
        cluster_columns = FALSE,
        column_title = "18S and 28S rRNA", 
        column_title_gp = gpar(fontsize = 10, fontface = "bold"),
        column_names_gp = gpar(fontsize = 7, fontface = "bold"),
        row_title = "Developmental stages", row_title_rot = 90,
        row_title_gp = gpar(fontsize = 8, fontface = "bold"),
        cluster_rows = TRUE,
        show_row_names = TRUE,
        show_column_names = TRUE,
        row_names_gp = gpar(fontsize = 5), #row names size
        column_dend_side = "top",
        column_names_side = "bottom",
        row_gap = unit(1, "mm"), #Gap,
        split = mod_types,
        right_annotation = row_ha)


#### Option with Nm instead of individual methylations

# Load the dplyr package
library(dplyr)

# Create mod_types2 by mutating the type
mod_types2 <- mod_types %>%
  mutate(type = ifelse(type %in% c('Am', 'Um', 'Cm', 'Gm'), 'Nm', type))


row_ha <- rowAnnotation(df = mod_types2)



Heatmap(merged_all_tissues_18S_28S_matrix, name = "SumErr", 
        col = colorRamp2(c(0, 0.25, 0.5, 0.75, 1), c("#2c7bb6","#abd9e9","floralwhite","#fdae61", "#d7191c"),space = "RGB"),
        cluster_columns = FALSE,
        column_title = "18S and 28S rRNA", 
        column_title_gp = gpar(fontsize = 10, fontface = "bold"),
        column_names_gp = gpar(fontsize = 7, fontface = "bold"),
        row_title = "Developmental stages", row_title_rot = 90,
        row_title_gp = gpar(fontsize = 8, fontface = "bold"),
        cluster_rows = TRUE,
        show_row_names = TRUE,
        show_column_names = TRUE,
        row_names_gp = gpar(fontsize = 5), #row names size
        column_dend_side = "top",
        column_names_side = "bottom",
        row_gap = unit(1, "mm"), #Gap,
        split = mod_types2,
        right_annotation = row_ha)
