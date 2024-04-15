# Load libraries
library('plyr')
library('ggplot2')
library('ggrepel')
library('dplyr')
library('corrplot')

# Define a function to read and preprocess input data (epinano output, .csv files with mismatch, deletion and insertion values per base)
read_and_preprocess <- function(filename) {
  input <- read.csv(filename)
  input <- input[grepl("18S", input$X.Ref),]
  input$sum <- input$mis + input$del + input$ins
  return(input[, c("pos", "sum")])
}

# Read and preprocess input data
inputs <- lapply(
  c('brain_embryo_rep1.csv', 'brain_embryo_rep2.csv', 'brain_newborn_rep1.csv',
    'brain_newborn_rep2.csv', 'brain_adult_rep1.csv', 'brain_adult_rep2.csv',
    'mESc_rep1.csv', 'mESc_rep2.csv', 'NPC_rep1.csv', 'NPC_rep2.csv',
    'Neurons_rep1.csv', 'Neurons_rep2.csv'),
  read_and_preprocess
)

# Merge input data
merged <- Reduce(function(x, y) merge(x, y, by = "pos"), inputs)

# Rename columns
colnames(merged) <- c("position", "brain_embryo_rep1", "brain_embryo_rep2", 
                      "brain_newborn_rep1", "brain_newborn_rep2", "brain_adult_rep1",
                      "brain_adult_rep2", "mESc_rep1", "mESc_rep2", "NPC_rep1",
                      "NPC_rep2", "Neurons_rep1", "Neurons_rep2")

# Write merged data to a file
#write.table(merged, file = "18S_summed_errors_brain_with_cells.txt", 
            #quote = FALSE, row.names = TRUE, sep = "\t")

# Read discovered positions
discovered_positions <- read.csv("discovered_positions_18S_brain_cells_from_clean_list.csv", header = FALSE)
colnames(discovered_positions) <- "position"

# Merge discovered positions with merged data
merged2 <- merge(discovered_positions, merged, by = "position")

# Add "18S" to position names
merged2$position <- sub("^", "18S:", merged2$position)

# Rename row names by positions
row.names(merged2) <- merged2$position

# Remove position column
merged2$position <- NULL

merged_18S <- merged2


### For 28S


# Define a function to read and preprocess input data
read_and_preprocess <- function(filename) {
  input <- read.csv(filename)
  input <- input[grepl("28S", input$X.Ref),]
  input$sum <- input$mis + input$del + input$ins
  return(input[, c("pos", "sum")])
}

# Read and preprocess input data
inputs <- lapply(
  c('brain_embryo_rep1.csv', 'brain_embryo_rep2.csv', 'brain_newborn_rep1.csv',
    'brain_newborn_rep2.csv', 'brain_adult_rep1.csv', 'brain_adult_rep2.csv',
    'mESc_rep1.csv', 'mESc_rep2.csv', 'NPC_rep1.csv', 'NPC_rep2.csv',
    'Neurons_rep1.csv', 'Neurons_rep2.csv'),
  read_and_preprocess
)

# Merge input data
merged <- Reduce(function(x, y) merge(x, y, by = "pos"), inputs)

# Rename columns
colnames(merged) <- c("position", "brain_embryo_rep1", "brain_embryo_rep2", 
                      "brain_newborn_rep1", "brain_newborn_rep2", "brain_adult_rep1",
                      "brain_adult_rep2", "mESc_rep1", "mESc_rep2", "NPC_rep1",
                      "NPC_rep2", "Neurons_rep1", "Neurons_rep2")

# Write merged data to a file
#write.table(merged, file = "28S_summed_errors_brain_with_cells.txt", 
#quote = FALSE, row.names = TRUE, sep = "\t")

# Read discovered positions
discovered_positions <- read.csv("discovered_positions_28S_brain_cells_from_clean_list.csv", header = FALSE)
colnames(discovered_positions) <- "position"

# Merge discovered positions with merged data
merged2 <- merge(discovered_positions, merged, by = "position")

# Add "28S" to position names
merged2$position <- sub("^", "28S:", merged2$position)

# Rename row names by positions
row.names(merged2) <- merged2$position

# Remove position column
merged2$position <- NULL

merged_28S <- merged2

merged_18S_28S <- rbind(merged_18S, merged_28S)

### Panel B
### Correlation plot
library(corrplot)

corrplot(cor(merged_18S_28S),        # Correlation matrix
         transKeepSign = FALSE,
         method = "color", # Correlation plot method
         type = "full",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         title = "",       # Main title
         col.lim = c(-0.1,1),
         is.corr = FALSE,
         COL1(sequential = c("YlOrBr")))       # Color palette
dev.off()


### Panel C

merged_18S_28S_t <- t(merged_18S_28S)
merged_18S_28S_t_s <- scale(merged_18S_28S_t)

library(ComplexHeatmap)
library(circlize)

Heatmap(merged_18S_28S_t_s, name = "Z-scaled SumErr", 
        col = colorRamp2(c(-2, -1, 0, 1, 2), c("#2c7bb6","#abd9e9","floralwhite","#fdae61", "#d7191c"),space = "RGB"),
        cluster_columns = TRUE,
        column_title = "Dynamic rRNA modifications", 
        column_title_gp = gpar(fontsize = 10, fontface = "bold"),
        column_names_gp = gpar(fontsize = 7, fontface = "bold"),
        row_title = "Developmental and differentiation stages", row_title_rot = 90,
        row_title_gp = gpar(fontsize = 8, fontface = "bold"),
        cluster_rows = FALSE,
        show_row_names = TRUE,
        show_column_names = TRUE,
        row_names_gp = gpar(fontsize = 5), #row names size
        column_dend_side = "top",
        column_names_side = "bottom",
        row_gap = unit(0, "mm"), #Gap
)
dev.off()

# Assigning a new name to the object as it will be needed for other scripts later on
merged_18S_28S_brain_cells <- merged_18S_28S
