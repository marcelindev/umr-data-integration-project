#working directory 
setwd("C:/Users/21260/Desktop/Dataintegration/project")

#Libraries 
library(dplyr)
library(stringdist)
library(sets)
library(data.table)

#loading datasets for discovering
unemployement <- read.csv("unemployment_analysis.csv")
poverty <- read.csv("poverty.csv")
happiness_2015 <- read.csv("2015.csv")
happiness_2016 <- read.csv("2016.csv")
happiness_2017 <- read.csv("2017.csv")
happiness_2018 <- read.csv("2018.csv")
happiness_2019 <- read.csv("2019.csv")


## viewing heads of datasets
#View(head(unemployement))
#View(head(poverty))
#View(head(happiness_2015))
#View(head(happiness_2016))
#View(head(happiness_2017))
#View(head(happiness_2018))
#View(head(happiness_2019))

## notes :
# happiness datasets need to be merged by year
# some columns have to be removed (not relevant for our showcases)
# we need standardization for some data so we can access them easily 



##### Integration pipeline


# Happiness Matching and profiling
# Trying to match column names
# Happiness datasets need to be integrated first
# First select all the columns we need from Happiness datasets 

names(happiness_2015)
happiness_2015 <- select(happiness_2015,Country,Region,Happiness.Rank
                         ,Happiness.Score,Economy..GDP.per.Capita.,Health..Life.Expectancy.
                         ,Freedom,Generosity,Trust..Government.Corruption.)

names(happiness_2016)
happiness_2016 <- select(happiness_2016,Country,Region,Happiness.Rank,Happiness.Score
                         ,Economy..GDP.per.Capita.,Health..Life.Expectancy.,Freedom
                         ,Generosity,Trust..Government.Corruption.)


## Problem here we dont have region, maybe we ll need this one for grouping by region in the results
names(happiness_2017)
happiness_2017 <- select(happiness_2017,Country,Happiness.Rank,Happiness.Score
                         ,Economy..GDP.per.Capita.,Health..Life.Expectancy.,Freedom
                         ,Generosity,Trust..Government.Corruption.)



## Problem here is country and region are fused and some columns are not as obvious as previous
names(happiness_2018)
happiness_2018 <- select(happiness_2018,Country.or.region,Overall.rank,Score,GDP.per.capita
                         ,Healthy.life.expectancy,Freedom.to.make.life.choices,Generosity
                         ,Perceptions.of.corruption)


## The same problem is also here to be found
names(happiness_2019)
happiness_2019 <- select(happiness_2019,Country.or.region,Overall.rank,Score,GDP.per.capita
                         ,Healthy.life.expectancy,Freedom.to.make.life.choices,Generosity
                         ,Perceptions.of.corruption)


### Trying to profile columns

## viewing heads of filtered columns datasets
#View(head(happiness_2015))
#View(head(happiness_2016))
#View(head(happiness_2017))
#View(head(happiness_2018))
#View(head(happiness_2019))


## Country or region : any possibility to split ?
levels(factor(happiness_2018$Country.or.region)) # no possibility
levels(factor(happiness_2019$Country.or.region)) # no possibility

## First we need the standardization of columns
## String similarities using damereau-levenshtein in percentages: 
## We will take the first data set as basis

max_lengths <- sapply(1:length(tolower(names(happiness_2015)))
                      , function(i) max(nchar(tolower(names(happiness_2015))[i]), nchar(tolower(names(happiness_2016)[i]))))
first_second <- 100*(1- (stringdist(tolower(names(happiness_2015)),tolower(names(happiness_2016)),method = "dl")/
                max_lengths))

first_second

max_lengths <- sapply(1:length(tolower(names(happiness_2015[-2])))
                      , function(i) max(nchar(tolower(names(happiness_2015)[-2])[i]), nchar(tolower(names(happiness_2017))[i])))


## here we dont have region so we will have to eliminate it from 2017 dataset
first_third <- 100*(1- (stringdist(tolower(names(happiness_2015))[-2],tolower(names(happiness_2017)),method = "dl")/
                     max_lengths))

first_third


max_lengths <- sapply(1:length(tolower(names(happiness_2015[-2])))
                      , function(i) max(nchar(tolower(names(happiness_2015[-2]))[i]), nchar(tolower(names(happiness_2018))[i])))


first_fourth <-  100*(1- (stringdist(tolower(names(happiness_2015))[-2],tolower(names(happiness_2018)),method = "dl")/
                        max_lengths))

first_fourth


max_lengths <- sapply(1:length(tolower(names(happiness_2015[-2])))
                      , function(i) max(nchar(tolower(names(happiness_2015[-2]))[i]), nchar(tolower(names(happiness_2019))[i])))


first_fifth <-  100*(1- (stringdist(tolower(names(happiness_2015))[-2],tolower(names(happiness_2019)),method = "dl")/
                            max_lengths))

first_fifth

## Conclusion 
## first and second,third dataset are a good match, but fourth and fifth are a problem
## We see that the fourth and the fifth have the same names 
## so if we normalize one dataset we apply the same on the other one 
## so semantically we got those percentages
## 41.17647  35.71429  33.33333  58.33333  91.66667  25.00000 100.00000  44.82759
## we see that most of the similarities are above 40%
## Lets perform set similarity measures now to have a closer overview 
## one information thats helping is that there is no big gap between the years
## so the values will be the same
## we will use jaccard set similarity for strings (only for country)

## jaccard 
jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

fs <- jaccard(happiness_2015$Country,happiness_2016$Country)
ft <- jaccard(happiness_2015$Country,happiness_2017$Country)
ff <- jaccard(happiness_2015$Country,happiness_2018$Country.or.region)
fff <- jaccard(happiness_2015$Country,happiness_2019$Country.or.region)
fs
ft
ff
fff

## The values are almost the same so we have the same column semantically we choose to name it as dataset1

## for the other columns we ll use euclidean distance to see if they are similar
## Problem here is that we have some difference in the entries
## first have temp datasets where we have the same entries of countries
## we will only save the entries that are mutual
intersection <- Reduce(intersect, list(happiness_2015$Country, happiness_2016$Country
                                       , happiness_2017$Country, happiness_2018$Country.or.region
                                       , happiness_2019$Country.or.region))


## now we can filter all the rows from all datasets
happiness_2015_temp <- filter(happiness_2015,Country %in% intersection)
happiness_2016_temp <- filter(happiness_2016,Country %in% intersection)
happiness_2017_temp <- filter(happiness_2017,Country %in% intersection)
happiness_2018_temp <- filter(happiness_2018,Country.or.region %in% intersection)
happiness_2019_temp <- filter(happiness_2019,Country.or.region %in% intersection)


##euclidean distance function

euclidean <- function(a, b) sqrt(sum((a - b)^2))
euclidean(happiness_2015_temp$Happiness.Rank,happiness_2016_temp$Happiness.Rank)
# maybe it is better to have only the same length and not the same entries, because its a ranking

# we will only do this between the first and the last, we expect the same results for all
# we choose the first and the last because there is a 5 year gap between them
min_length <- min(length(happiness_2015$Happiness.Rank), length(happiness_2019$Overall.rank))


# Extend the shorter vector to match the minimum length
extended_vector1 <- rep(happiness_2015$Happiness.Rank, length.out = min_length)
extended_vector2 <- rep(happiness_2019$Overall.rank, length.out = min_length)


euclidean(extended_vector1,extended_vector2) # distance is too small as expected

# Extend the shorter vector to match the minimum length
extended_vector1 <- rep(happiness_2015$Happiness.Score, length.out = min_length)
extended_vector2 <- rep(happiness_2019$Score, length.out = min_length)


euclidean(extended_vector1,extended_vector2) # distance is too small as expected

# Extend the shorter vector to match the minimum length
extended_vector1 <- rep(happiness_2015$Economy..GDP.per.Capita., length.out = min_length)
extended_vector2 <- rep(happiness_2019$GDP.per.capita, length.out = min_length)


euclidean(extended_vector1,extended_vector2) # distance is too small as expected


# Extend the shorter vector to match the minimum length
extended_vector1 <- rep(happiness_2015$Health..Life.Expectancy., length.out = min_length)
extended_vector2 <- rep(happiness_2019$Healthy.life.expectancy, length.out = min_length)


euclidean(extended_vector1,extended_vector2) # distance is too small as expected



# Extend the shorter vector to match the minimum length
extended_vector1 <- rep(happiness_2015$Freedom, length.out = min_length)
extended_vector2 <- rep(happiness_2019$Freedom.to.make.life.choices, length.out = min_length)


euclidean(extended_vector1,extended_vector2) # distance is too small as expected


# Extend the shorter vector to match the minimum length
extended_vector1 <- rep(happiness_2015$Generosity, length.out = min_length)
extended_vector2 <- rep(happiness_2019$Generosity, length.out = min_length)


euclidean(extended_vector1,extended_vector2) # distance is too small as expected


# Extend the shorter vector to match the minimum length
extended_vector1 <- rep(happiness_2015$Trust..Government.Corruption., length.out = min_length)
extended_vector2 <- rep(happiness_2019$Perceptions.of.corruption, length.out = min_length)


euclidean(extended_vector1,extended_vector2) # distance is too small as expected


### so now as we identified whats in the 5 datasets, filtered and got the similarities we have
### we can standardize the name of columns and also merge the datasets two have the region in each
### question here is how to deal with unintersection, we choose to do it manualy


cnames <- c("Country","Region","Happiness_Rank","Happiness_Score","GDP","HLExpectancy","Freedom","Generosity","Corruption")
cnames_1 <- c("Country","Happiness_Rank","Happiness_Score","GDP","HLExpectancy","Freedom","Generosity","Corruption")
colnames(happiness_2015) <- cnames
colnames(happiness_2016) <- cnames
colnames(happiness_2017) <- cnames_1
colnames(happiness_2018) <- cnames_1
colnames(happiness_2019) <- cnames_1

## now we have the same column names 
## we add region to the left data sets
## we dont need them now maybe later on for analysis
merged_dataset1 <- merge(happiness_2016, happiness_2015[, c("Country", "Region")], by = "Country")
merged_dataset2 <- merge(happiness_2017,happiness_2015[, c("Country", "Region")], by = "Country")
merged_dataset3 <- merge(happiness_2018,happiness_2015[, c("Country", "Region")], by = "Country")
merged_dataset4 <- merge(happiness_2019,happiness_2015[, c("Country", "Region")], by = "Country")

##lost entries
setdiff(happiness_2016$Country,merged_dataset1$Country)
setdiff(happiness_2017$Country,merged_dataset2$Country)
setdiff(happiness_2018$Country,merged_dataset3$Country)
setdiff(happiness_2019$Country,merged_dataset4$Country)


### Done : Standardization of columns of happiness
### Now we need one dataset containing all and having year as key

## First add a year column to all datasets
year <- rep(2015,nrow(happiness_2015))
happiness_2015 <- cbind(happiness_2015,year)

year <- rep(2016,nrow(happiness_2016))
happiness_2016 <- cbind(happiness_2016,year)

year <- rep(2017,nrow(happiness_2017))
happiness_2017 <- cbind(happiness_2017,year)

year <- rep(2018,nrow(happiness_2018))
happiness_2018 <- cbind(happiness_2018,year)

year <- rep(2019,nrow(happiness_2019))
happiness_2019 <- cbind(happiness_2019,year)










## now we merge iterativaly by year
happiness_2015_2019 <- rbind(happiness_2015[,-2],happiness_2016[,-2],happiness_2017,happiness_2018,happiness_2019)
happiness_2015_2019 <- relocate(happiness_2015_2019,year)
View(happiness_2015_2019)
### now we have one dataset containing all the information 

### to do : same for poverty and unemployement

### Unemployement dataset :
### Problems : Year is over columns expanded. This needs to be fixed
### first we only need the gap between 2015-2019
unemployement_2015 <- select(unemployement,Country.Name,Country.Code,X2015)
unemployement_2016 <- select(unemployement,Country.Name,Country.Code,X2016)
unemployement_2017 <- select(unemployement,Country.Name,Country.Code,X2017)
unemployement_2018 <- select(unemployement,Country.Name,Country.Code,X2018)
unemployement_2019 <- select(unemployement,Country.Name,Country.Code,X2019)

## add year column for key 
unemployement_2015 <- cbind(unemployement_2015,rep(2015,nrow(unemployement_2015)))
unemployement_2016 <- cbind(unemployement_2016,rep(2016,nrow(unemployement_2016)))
unemployement_2017 <- cbind(unemployement_2017,rep(2017,nrow(unemployement_2017)))
unemployement_2018 <- cbind(unemployement_2018,rep(2018,nrow(unemployement_2018)))
unemployement_2019 <- cbind(unemployement_2019,rep(2019,nrow(unemployement_2019)))





## normalize colnames since its very obvious

colnames(unemployement_2015) <- c("Country","Country_code","Unemployement_rate","year")
colnames(unemployement_2016) <- c("Country","Country_code","Unemployement_rate","year")
colnames(unemployement_2017) <- c("Country","Country_code","Unemployement_rate","year")
colnames(unemployement_2018) <- c("Country","Country_code","Unemployement_rate","year")
colnames(unemployement_2019) <- c("Country","Country_code","Unemployement_rate","year")


## now we have one dataset containing all infos
unemployement_2015_2019 <- rbind(unemployement_2015,unemployement_2016,unemployement_2017
                                 ,unemployement_2018,unemployement_2019)



### same thing for poverty 
### we need to extract only the rows needed
### first normalize colnames
colnames(poverty) <- c("Country","Country_code","year","under_30")

poverty_2015 <- filter(poverty,year == 2015)
poverty_2016 <- filter(poverty,year == 2016)
poverty_2017 <- filter(poverty,year == 2017)
poverty_2018 <- filter(poverty,year == 2018)
poverty_2019 <- filter(poverty,year == 2019)

poverty_2015_2019 <- rbind(poverty_2015,poverty_2016,poverty_2017,poverty_2018,poverty_2019)

### Milestone : Datasets are all filtered
### Problem : country code is missed in happiness
### Solution : function to compute the country code for each country name
### First : create the country entity 

##lets discover the similarities between the datasets 
hp <- jaccard(unique(tolower(happiness_2015_2019$Country)),unique(tolower(poverty_2015_2019$Country)))
hu <- jaccard(unique(tolower(happiness_2015_2019$Country)),unique(tolower(unemployement_2015_2019$Country)))
pu <- jaccard(unique(tolower(unemployement_2015_2019$Country)),unique(tolower(poverty_2015_2019$Country)))
#The measures show that there is a similarity between the sets, so we can say its the same attribute and
#can get some good results for our showcases


## One problem encountered here is the name of countries, we need to remove some because they are the same
## Palestine for example
## it will be better if we first put everything in the country entity and then we can clean later
## take uniques all over the three datasets 

### Country entity :
### we ll do this with the ucc
countries <- unique(union(union( unique(toupper(happiness_2015_2019$Country)), unique(toupper(unemployement_2015_2019$Country)))
                          ,unique(toupper(poverty_2015_2019$Country))))

## create coutry dataset
## we have country codes only in poverty and unemployement
## matching country codes
Country <- data.frame(Country = sort(countries))




#Countrycodes erstellen und matching überprüfen

library(countrycode)


create_country_codes <- function(data, column_name) {
  
  data$Country_code <- countrycode(data[[column_name]], origin = "country.name", destination = "iso3c")
  
  return(data)
}

### adding country codes to data 
Country <- create_country_codes(Country, "Country")
happiness_2015_2019 <- create_country_codes(happiness_2015_2019,"Country")

get_non_matching_strings <- function(data1, column1, data2, column2) {
  non_matching <- setdiff(data1[[column1]], data2[[column2]])
  return(non_matching)
}

get_non_matching_strings(Country,"Country_code",poverty_2015_2019,"Country_code")

calculate_similarity <- function(data1, column1, data2, column2) {
  set1 <- unique(data1[[column1]])
  set2 <- unique(data2[[column2]])
  
  intersection <- intersect(set1, set2)
  union <- union(set1, set2)
  
  similarity <- length(intersection) / length(union)
  return(similarity)
}

calculate_similarity(Country,"Country_code",poverty_2015_2019,"Country_code")


calculate_attribute_similarity <- function(data1, data2) {
  attributes1 <- colnames(data1)
  attributes2 <- colnames(data2)
  
  set1 <- as.character(attributes1)
  set2 <- as.character(attributes2)
  
  intersection <- intersect(set1, set2)
  union <- union(set1, set2)
  
  similarity <- 2 * length(intersection) / (length(set1) + length(set2))
  return(similarity)
}


similarity <- calculate_attribute_similarity(happiness_2015,happiness_2019)



### to view fds,uccs, plis call functions from helpers.r 
source("Helpers.r")
## call functions here on dataset






#View integrated data. Note : data needs to be cleaned (cleaning step)
View(Country)
View(happiness_2015_2019)
View(poverty_2015_2019)
View(unemployement_2015_2019)

## the data now is matched with the original er model we presented in the first step


## Write data into csv 
write.csv(unemployement_2015_2019, "C:/Users/21260/Desktop/Dataintegration/project/last_ds/Unemployement.csv", row.names=FALSE)
write.csv(poverty_2015_2019, "C:/Users/21260/Desktop/Dataintegration/project/last_ds/Poverty.csv", row.names=FALSE)
write.csv(happiness_2015_2019, "C:/Users/21260/Desktop/Dataintegration/project/last_ds/Happiness.csv", row.names=FALSE)
write.csv(Country,"C:/Users/21260/Desktop/Dataintegration/project/last_ds/Country.csv", row.names=FALSE)























