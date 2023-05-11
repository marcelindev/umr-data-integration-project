#working directory 
setwd("put_your_path_in_here")

#loading datasets for discovering
unemployement <- read.csv("unemployement.csv")
poverty <- read.csv("poverty.csv")
happiness_2015 <- read.csv("2015.csv")
happiness_2016 <- read.csv("2016.csv")
happiness_2017 <- read.csv("2017.csv")
happiness_2018 <- read.csv("2018.csv")
happiness_2019 <- read.csv("2019.csv")


## viewing heads of datasets
View(head(unemployement))
View(head(poverty))
View(head(happiness_2015))
View(head(happiness_2016))
View(head(happiness_2017))
View(head(happiness_2018))
View(head(happiness_2019))

## notes :
# happiness datasets needs to be merged by year
# some columns have to be removed (not relevant for our showcases)
# we need standardization for some data so we can access them easily 
