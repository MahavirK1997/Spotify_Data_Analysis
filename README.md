# Spotify Dataset Analysis

## Project Overview

This project explores a Spotify dataset, focusing on determining the attribute range for popular songs. The dataset comprises over 170k rows with attributes like acousticness, danceability, energy, and more.

### Objectives

1. Separate the most popular songs from the entire song list.
2. Compare the parametric range of the most popular songs with all listed songs.
3. Conduct Exploratory Data Analysis (EDA) on the Spotify music dataset.
4. Determine the attribute range for popular songs.

## Project Steps

### Part 1: Data Set Information and Attribute Range

#### Dataset Link
- [Spotify Dataset](https://jovian.ai/zhangxm963/spotify-dataframe/v/39/files?filename=spotifydataset-19212020-160k-tracks/data.csv)

#### Key Attributes and Data Cleaning
Columns irrelevant for analysis were dropped. The remaining attributes include acousticness, danceability, energy, etc.

### Part 2: Statistical Values and Graphical Analysis

#### Hypothesis Testing and Attribute Shifts
R was used to calculate statistical values for all songs and most popular songs. Histograms were plotted, indicating shifts in mean values.

### Part 3: Determining Attribute Range for Popular Songs

#### Quantile Approach and Exclusion of Attributes
A 90% interval around median values was considered. Attributes with no significant difference were excluded. After iterations and considering correlations, an optimal range was identified, fitting 62.16% of popular songs.

## Replicating the Project

1. Access the Spotify dataset from the provided link.
2. Perform data cleaning by dropping irrelevant columns.
3. Use R for statistical analysis.
4. Visualize data through histograms and box plots.
5. Determine attribute range for popular songs using a quantile approach and correlation analysis.

## Conclusion

Findings can assist composers and producers in understanding current trends in the music industry. The model helps determine listener preferences, aiding music creators in producing songs aligned with current trends and increasing the likelihood of achieving popularity.
