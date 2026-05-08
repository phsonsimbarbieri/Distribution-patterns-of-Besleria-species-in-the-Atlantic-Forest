This repository contains the dataset and the R script used to test hypotheses and examine the relationships between environmental conditions and the distribution patterns of species of the genus _Besleria_ in the Atlantic Forest.

The script performs:
- Mann-Whitney-Wilcoxon (MWW) tests, to compare species with narrow or wide distributions based on environmental variables;
- Linear regressions and Spearman’s correlation, to examine the relationship between habitat tolerance, extent of occurrence (EOO), and measures of environmental variability;
- Principal Coordinate Analysis (PCoA), to visualize multivariate environmental relationships among species while preserving their ecological dissimilarities, and to verify whether different groups of taxa occupy distinct portions of the environmental space.

All analyses were performed in R.

The file “besleria_raw_data_matrix.xlsx” contains the original database and a list of the herbarium specimens used in the study.
The file “besleria_summary_matrix.xlsx” contains the data matrix used to run the script, presenting compiled data on the mean, standard deviation, and range of the environmental variables.
The file “besleria_interval_matrix.xlsx” contains the data matrix used to generate the complementary PCoA for this study, presented as Supplementary Material. This matrix includes the minimum and maximum values of the intervals generated for continuous variables, while for categorical variables, it presents the number of categories occupied by each taxon in each condition.
