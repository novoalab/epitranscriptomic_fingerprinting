# rRNA epitranscriptomic fingerprinting
Scripts used to analyze the sequencing data and build the figures used in Milenkovic et al., Mol Cell 2024 (accepted)

## Table of Contents  (to be edited)
- [General Description](#General-description)
- [Analysis of the Data](#Analysis-of-the-data)
  - [1. Identification of differentially modified sites](#1-identification-of-differentially-modified-sites)
  - [2. Validation of unannotated modification sites using Nano-CMC](#2-validation-of-unannotated-modification-sites-using-Nano-CMC)
  - [3. PCA and tissue classification based on dynamic rRNA modifications](#3-PCA-and-tissue-classification-based-on-dynamic-rRNA-modifications)
  - [4. Identifying tumor-specific dynamic rRNA modifications and building tumor-aware Random Forest classifiers](#4-identifying-tumor-specific-dynamic-rRNA-modifications-and-building-tumor-aware-random-forest-classifiers)
- [Dependencies and versions](#Dependencies-and-versions)
- [Citation](#Citation) 
- [Contact](#Contact) 


## General Description
The scripts used in the manuscript 'Epitranscriptomic fingerprinting reveals tissue-of-origin and tumor-specific signatures'


## Analysis of the Data

The sequencing data produced in this work was analyzed using scripts listed in this github repository.

### 1. Identification of differentially modified sites

The script used to identify differentially modified sites across tissues and neuronal cells can be found in folders Figure_1 and Figure_2. Additional scripts, including the ones used for replicability analyses and outlier identification can be found in the folder Figure_S1-6.

### 2. Validation of unannotated modification sites using Nano-CMC

The scripts used to validate putative pseudouridylation sites are found in folder Figure_S8-9. For additional scripts refer to https://github.com/novoalab/yeast_RNA_Mod/tree/master/Analysis/NanoCMCSeq.

### 3. PCA and tissue classification based on dynamic rRNA modifications

The scripts used for PCAs and tissue-aware epitranscriptomic fingerprinting can be found in folders Figure_3 and Figure_S17.

### 4. Identifying tumor-specific dynamic rRNA modifications and building tumor-aware Random Forest classifiers

The scripts used for identifying tumor-specific dynamic rRNA modifications and building corresponding classifiers can be found in folder Figure_6 and Figure_S21.


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

Milenkovic I, Cruciani S, Llovera L, Lucas MC, Medina R, Pauli C, Heid D, Muley T, Schneider MA, Klotz LV, Allgauer M, Latuca R, LaFontaine LJ D, Muller-Tidow C and Novoa EM. Epitranscriptomic rRNA fingerprinting reveals tissue-of-origin and tumor-specific signatures. Mol Cell 2024 (accepted). Preprint available: https://www.biorxiv.org/content/10.1101/2024.10.03.616461v1.full.pdf+html

## Contact

Please open an issue in this repo if you encounter any difficulty running a script or have any doubts regarding the code.
For correspondance, please contact eva.novoa@crg.eu.
