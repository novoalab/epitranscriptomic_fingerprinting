# rRNA epitranscriptomic fingerprinting
Scripts used to analyze the data and build the figures used in Milenkovic et al., XXXX

## Table of Contents  (to be edited)
- [General Description](#General-description)
- [Analysis of the Data](#Analysis-of-the-data)
  - [1. Identification of differentially modified sites](#1-identification-of-differentially-modified-sites)
  - [2. Replicability of rRNA modification patterns across conditions](#2-replicability-of-rrna-modification-patterns-across-conditions)
  - [3. Clustering samples by rRNA modification patterns](#3-clustering-samples-by-rrna-modification-patterns)
    - [3.1. Heatmap-based clustering](#3.1-heatmap-based-clustering)
    - [3.2. Principal component analysis](#3.2-principal-component-analysis)
  - [4. Linear discriminant analysis to predict tissue of origin](#4-linear-discriminant-analysis-to-predict-tissue-of-origin)
- [Dependencies and versions](#Dependencies-and-versions)
- [Citation](#Citation) 
- [Contact](#Contact) 


## General Description
The scripts used in the manuscript 'Epitranscriptomic fingerprinting reveals tissue-of-origin and tumor signatures'


## Analyis of the Data

### 1. Identification of differentially modified sites

The script used to identify differentially modified sites across tissues and neuronal cells can be found in folders Figure_1 and Figure_2.


## Dependencies and versions

Software | Version 
--- | ---
plyr | 1.8.7
ggplot2 | 3.5.0
tidyverse | 2.0.0
dplyr | 1.1.2
ggrepel | 0.9.3
MASS | 7.3-53
ggpubr | 0.6.0
tune | 1.1.1
data.table | 1.14.0
ComplexHeatmap | 2.6.2
circlize | 0.4.15
corrplot | 0.93
gridExtra | 2.3
ggforce | 0.4.1
grid | 4.0.3
klaR | 1.7-1
psych | 2.2.9
ggord | 1.1.7
devtools | 2.4.2
reshape | 0.8.9
reshape2 | 1.4.4
randomForest | 4.6-14
pROC | 1.18.0


## Citation

If you find this work/scripts useful, please cite: 

Milenkovic I, Cruciani S, Llovera L, Lucas MC, Medina R, Pauli C, Heid D, Muley T, Schneider MA, Klotz LV, Allgauer M, Muller-Tidow C and Novoa EM. Epitranscriptomic rRNA fingerprinting reveals tissue-of-origin and tumor-specific signatures. BioRxiv XXX. 

## Contact

Please open an issue in this repo if you encounter any difficulty running a script or have any doubts regarding the code.
For correspondance, please contact eva.novoa@crg.eu.
