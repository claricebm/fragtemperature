# fragtemperature
R scripts for all analyzes ran for my Masters project

The R scripts here were all used for the analysis of my Masters project, turned into a paper: "Does habitat fragmentation affect landscape-level temperatures? A global analysis". See link for the study: https://link.springer.com/article/10.1007/s10980-020-01041-5

Analyzes were separeted in 3 steps

Script1_Percentage_to_number_of_patches: conversion of the original global forest dataset into a presence–absence forest cover map and calculation of the degree of fragmentation, i.e. number of patches, in each landscape (5 × 5 km areas) across the globe

Script2_Moving_window_analysis: application of a “moving window” searching strategy to compare pairs of landscapes with similar amount of forest cover (difference < 5%), but different levels of forest fragmentation (number of forest patches)

Script3_path_analysis_spatial_autocorrelation: calculation of the difference of each climatic variable between the cells of each pair of landscapes; application of statistical analyses to quantify causal relationships among the degree of forest fragmentation and the climatic variables

Please see further methodological details in the paper
