library(tidyverse) 
library(e1071) 
library(ggplot2)
library(fitdistrplus)
library(dplyr)
library(reshape2)
install.packages(ggpubr)
library(ggpubr)

# Improting the spotify Dataset. 
spotify_df <- read.csv('C:/Users/abc/Desktop/Spotify_Dataset.csv',  
                         header = TRUE,        
                         sep = ',')             
head(spotify_df)
################################################################################################################
#############################################################################################################

#### Part 1: Extracting the relevant columns from the dataset. 
req_col <- dplyr::select(spotify_df, acousticness, year, explicit, mode, danceability, instrumentalness, energy, tempo,
                         liveness, loudness, popularity, valence, speechiness, duration_ms, key)
head(req_col, 5)

################################################################################################################
###############################################################################################################

##Part 2 :Plotting the correkation heatmap
## creating correlation heatmap

corr_mat <- round(cor(req_col), 2)
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)

library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = 'black', size = 4) + 
  scale_fill_gradient(low = 'lightgreen',
                      high = 'darkgreen')

###########################################################################################################
###########################################################################################################
#### Part 3: Analysis of most popular songs  
# defining the yearwise datasets
df1 = req_col[req_col$year >=1920 & req_col$year <=1929,]
df2 = req_col[req_col$year >=1930 & req_col$year <=1939,]
df3 = req_col[req_col$year >=1940 & req_col$year <=1949,]
df4 = req_col[req_col$year >=1950 & req_col$year <=1959,]
df5 = req_col[req_col$year >=1960 & req_col$year <=1969,]
df6 = req_col[req_col$year >=1970 & req_col$year <=1979,]
df7 = req_col[req_col$year >=1980 & req_col$year <=1989,]
df8 = req_col[req_col$year >=1990 & req_col$year <=1999,]
df9 = req_col[req_col$year >=2000 & req_col$year <=2009,]
df10 = req_col[req_col$year>=2010 & req_col$year <=2021,]

#calculting mean of all songs dataset defined above for all atributes.
m1 = mean(df1$danceability)
m2 = mean(df2$danceability)
m3 = mean(df3$danceability)
m4 = mean(df4$danceability)
m5 = mean(df5$danceability)
m6 = mean(df6$danceability)
m7 = mean(df7$danceability)
m8 = mean(df8$danceability)
m9 = mean(df9$danceability)
m10 = mean(df10$danceability)

##################################################################################################################

###determining the maximum popular songs popularity range by hit and trial.
#Top 1% songs with highest popularity are considered for determining the minimum value of popularity
#attribute
pop_max = req_col[req_col$popularity >=75,]
nrow(pop_max)/nrow(req_col)

####################################################################################################################

###Hypothesis Testing
#population 1: all spotify released songs
#population 2: Popular songs (popularoty >= 75)

##Hypothesis testing 1 
#H0: population 1 mean < population 2 mean (for defined parameter)
#H1: population 1 mean > population 2 mean (for defined parameter)
set.seed(300)
spotify_sample <- sample_n(req_col, 200)
max_pop_sample <- sample_n(pop_max, 200) 
sample11<-spotify_sample$acousticness
sample12<-max_pop_sample$acousticness
var_11<-var(sample11)
var_12<-var(sample12)
mean11<- mean(sample11)
mean12<- mean(sample12)
n11 = length(sample11)
n12 = length(sample12)
z_cal1 = (mean11-mean12)/sqrt(var_11/n11 + var_12/n12)
z_cal1
qnorm(0.99)
# Z_cal > Z(0.01), so we reject the null hypothesis. 
# So, with 99% confidence we can state that for acousticness attribute all songs dataset will have higher mean than mean than popular songs. 

##Hypothesis testing 2 
#H0: population 1 mean > population 2 mean (for defined parameter)
#H1: population 1 mean < population 2 mean (for defined parameter)
set.seed(300)
spotify_sample <- sample_n(req_col, 200)
max_pop_sample <- sample_n(pop_max, 200) 
sample21<-spotify_sample$danceability
sample22<-max_pop_sample$danceability
var_21<-var(sample21)
var_22<-var(sample22)
mean21<- mean(sample21)
mean22<- mean(sample22)
n21 = length(sample21)
n22 = length(sample22)
z_cal2 = (mean21-mean22)/sqrt((var_21/n21) + (var_22/n22))
z_cal2
qnorm(0.01)
# Z_cal < Z(0.01), so we reject the null hypothesis. 
# So, with 99% confidence we can state that for danceability all songs will have lower mean than popular songs. 

##################################################################################################################

# calculating mean, median, and standard deviation of all songs dataframe attributes and plotting in table
#All songs dataset attrubute mean
mn1= mean(req_col$acousticness)
mn2= mean(req_col$speechiness)
mn3= mean(req_col$duration_ms)
mn4= mean(req_col$key)
mn5= mean(req_col$liveness)
mn6= mean(req_col$valence)
mn7= mean(req_col$energy)
mn8= mean(req_col$danceability)
mn9= mean(req_col$instrumentalness)
mn10= mean(req_col$loudness)
mn11= mean(req_col$popularity)
mn12= mean(req_col$tempo)

#All songs dataset attrubute median
mdn1= median(req_col$acousticness)
mdn2= median(req_col$speechiness)
mdn3= median(req_col$duration_ms)
mdn4= median(req_col$key)
mdn5= median(req_col$liveness)
mdn6= median(req_col$valence)
mdn7= median(req_col$energy)
mdn8= median(req_col$danceability)
mdn9= median(req_col$instrumentalness)
mdn10= median(req_col$loudness)
mdn11= median(req_col$popularity)
mdn12= median(req_col$tempo)

#All songs dataset attrubute standard deviation
sd1=	sd(req_col$acousticness)
sd2=	sd(req_col$speechiness)
sd3=	sd(req_col$duration_ms)
sd4=	sd(req_col$key)
sd5=	sd(req_col$liveness)
sd6=	sd(req_col$valence)
sd7=	sd(req_col$energy)
sd8=	sd(req_col$danceability)
sd9=	sd(req_col$instrumentalness)
sd10=	sd(req_col$loudness)
sd11=	sd(req_col$popularity)
sd12=	sd(req_col$tempo)

#Popular song attrubute mean
mn13= mean(pop_max$acousticness)
mn14= mean(pop_max$speechiness)
mn15= mean(pop_max$duration_ms)
mn16= mean(pop_max$key)
mn17= mean(pop_max$liveness)
mn18= mean(pop_max$valence)
mn19= mean(pop_max$energy)
mn20= mean(pop_max$danceability)
mn21= mean(pop_max$instrumentalness)
mn22= mean(pop_max$loudness)
mn23= mean(pop_max$popularity)
mn24= mean(pop_max$tempo)

#Popular song attrubute median
mdn13= median(pop_max$acousticness)
mdn14= median(pop_max$speechiness)
mdn15= median(pop_max$duration_ms)
mdn16= median(pop_max$key)
mdn17= median(pop_max$liveness)
mdn18= median(pop_max$valence)
mdn19= median(pop_max$energy)
mdn20= median(pop_max$danceability)
mdn21= median(pop_max$instrumentalness)
mdn22= median(pop_max$loudness)
mdn23= median(pop_max$popularity)
mdn24= median(pop_max$tempo)

#Popular song attrubute standard deviation
sd13=	sd(pop_max$acousticness)
sd14=	sd(pop_max$speechiness)
sd15=	sd(pop_max$duration_ms)
sd16=	sd(pop_max$key)
sd17=	sd(pop_max$liveness)
sd18=	sd(pop_max$valence)
sd19=	sd(pop_max$energy)
sd20=	sd(pop_max$danceability)
sd21=	sd(pop_max$instrumentalness)
sd22=	sd(pop_max$loudness)
sd23=	sd(pop_max$popularity)
sd24=	sd(pop_max$tempo)

table_statistic = data.frame(Parameter = c('acousticness', 'speechiness', 'duration_ms', 'key', 'liveness','valence','energy','danceability', 'instrumentalness','loudness','popularity','tempo'),
                        pop_mean = c(mn1, mn2, mn3, mn4, mn5, mn6, mn7, mn8, mn9, mn10, mn11, mn12),
                        max_pop_mean = c(mn13, mn14, mn15, mn16, mn17, mn18, mn19, mn20, mn21, mn22, mn23, mn24),
                        pop_median = c(mdn1,	mdn2,	mdn3,	mdn4,	mdn5,	mdn6,	mdn7,	mdn8,	mdn9,	mdn10,	mdn11,	mdn12),
                        max_pop_median = c(mdn13,	mdn14,	mdn15,	mdn16,	mdn17,	mdn18,	mdn19,	mdn20,	mdn21,	mdn22,	mdn23,	mdn24),
                        pop_sd = c(sd1,	sd2,	sd3,	sd4,	sd5,	sd6,	sd7,	sd8,	sd9,	sd10,	sd11,	sd12),
                        max_pop_sd = c(sd13,	sd14,	sd15,	sd16,	sd17,	sd18,	sd19,	sd20,	sd21,	sd22,	sd23,	sd24))
table_statistic

##################################################################################################################

##Plotting the histograms for attributes for all songs dataset and maximum popular song dataset
#Histogram for all song dataset
ggplot(req_col, aes(acousticness)) +
  geom_histogram(bins = 50, color = 'green', fill = 'steelblue')

#Histogram for maximum popular song dataset
ggplot(pop_max, aes(acousticness)) +
  geom_histogram(bins = 50, color = 'green', fill = 'steelblue')

###############################################################################################################

####Determining the range of attributes for most popular songs 
##determining the quantile for attributes in most popular song dataset for differenet probabilities
apply(pop_max, 2, quantile, probs = c(0.0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1.0))

##step 1: Taking the 90% data about the median from 5% to 95% range to calculate the proportion 
#of popular songs falling in the range 
pop_para1_df = pop_max[pop_max$acousticness <= 0.830 & pop_max$acousticness >= 0.003
                      & pop_max$instrumentalness <= 0.005
                      & pop_max$liveness <= 0.405 & pop_max$liveness >= 0.06
                      & pop_max$loudness >= -12.32 & pop_max$loudness <= -3.12
                      & pop_max$speechiness <= 0.323 & pop_max$speechiness >= 0.028
                      & pop_max$duration_ms <= 296028 & pop_max$duration_ms >= 135757
                      & pop_max$energy >= 0.292 & pop_max$energy<= 0.904
                      & pop_max$danceability >= 0.383 & pop_max$danceability<= 0.878
                      & pop_max$key >= 0 & pop_max$key<= 11
                      & pop_max$tempo >= 77.92 & pop_max$tempo<= 174.036
                      & pop_max$valence >= 0.132 & pop_max$valence<= 0.878,]
nrow(pop_para1_df)/nrow(pop_max)

##Step 2: Removing the attributes whose range is not varying much from population range and so they are
#not contributing much in popularity to calculte the proportion of popular song in that region. 
pop_para2_df = pop_max[pop_max$acousticness <= 0.830 & pop_max$acousticness >= 0.003
                       & pop_max$instrumentalness <= 0.005
                       & pop_max$liveness <= 0.405 & pop_max$liveness >= 0.06
                       & pop_max$loudness >= -12.32 & pop_max$loudness <= -3.12
                       & pop_max$speechiness <= 0.323 & pop_max$speechiness >= 0.028
                       & pop_max$duration_ms <= 296028 & pop_max$duration_ms >= 135757
                       & pop_max$energy >= 0.292 & pop_max$energy<= 0.904
                       & pop_max$danceability >= 0.383 & pop_max$danceability<= 0.878 ,]
nrow(pop_para2_df)/nrow(pop_max)

##step 3: Considering the correlation coefficient sign, taking the one sided range i.e. 0% to 90% for
#negative correlation and 10% to 100% range for positive correlation and adjusting range with hit and trial
#to maximixe the proportion of songs falling in the range.  

pop_para_df = pop_max[pop_max$acousticness <= 0.696 & pop_max$instrumentalness <= 0.05 & pop_max$liveness <= 0.4 & pop_max$liveness >= 0.02
                      & pop_max$loudness >= -10.33 & pop_max$loudness <= -1.19 & pop_max$speechiness <= 0.323 & pop_max$duration_ms <= 296028 & 
                        pop_max$duration_ms >= 135756.8 & pop_max$energy >= 0.35 & pop_max$energy<= 0.988
                      & pop_max$danceability >= 0.451 & pop_max$danceability<= 0.980,]
nrow(pop_para_df)/nrow(pop_max)

#####################################################################################################################
###################################################################################################################
######################################################################################################################

##Probability of getting song popular if song is falling in the above range 

pop_para_df2 = req_col[req_col$acousticness <= 0.696 & req_col$instrumentalness <= 0.05 & req_col$liveness <= 0.4 & req_col$liveness >= 0.02
                       & req_col$loudness >= -10.33 & req_col$loudness <= -1.19 & req_col$speechiness <= 0.323 & req_col$duration_ms <= 296028 & 
                         req_col$duration_ms >= 135756.8 & req_col$energy >= 0.35 & req_col$energy<= 0.988
                       & req_col$danceability >= 0.451 & req_col$danceability<= 0.980 & req_col$year >=2019,]

pop_para_df11 = pop_max[pop_max$acousticness <= 0.696 & pop_max$instrumentalness <= 0.05 & pop_max$liveness <= 0.4 & pop_max$liveness >= 0.02
                      & pop_max$loudness >= -10.33 & pop_max$loudness <= -1.19 & pop_max$speechiness <= 0.323 & pop_max$duration_ms <= 296028 & 
                        pop_max$duration_ms >= 135756.8 & pop_max$energy >= 0.35 & pop_max$energy<= 0.988
                      & pop_max$danceability >= 0.451 & pop_max$danceability<= 0.980 & pop_max$year >=2019,]

nrow(pop_para_df11)/nrow(pop_para_df2)





















