#Set working directory and load data and libraries
setwd("C:\\Users\\user\\Downloads") #set the wd
moviedf <- read.csv("Movies_Data.csv") 
library(ggplot2)
library(extrafont)

#Checking my dataframe
nrow(moviedf)
ncol(moviedf)
head(moviedf)
str(moviedf)
summary(moviedf)


#Gross...mill. and some other variables, appears as chr (categorical var) instead of num (numerical). That is because they 
#use 1,340.86 format, with a comma. I remove the commas so that the value can be read as numerical 1340.86 and then i make it num.
moviedf$Gross...mill. <- as.numeric(gsub(",", "", moviedf$Gross...mill.))
moviedf$Overseas...mill. <- as.numeric(gsub(",", "", moviedf$Overseas...mill.))
moviedf$Profit...mill. <- as.numeric(gsub(",", "", moviedf$Profit...mill.))
moviedf$Adjusted.Gross...mill. <- as.numeric(gsub(",", "", moviedf$Adjusted.Gross...mill.))
#Also i like having my categorical variables as factors (the ones that make sense. Movie Title will have as many levels as its own values)
moviedf$Day.of.Week <- factor(moviedf$Day.of.Week)
moviedf$Genre <- factor(moviedf$Genre)
str(moviedf)

#i need to create a subset because i want SOME of the Genres on the x axis.
GenreSubset <- c("action", "adventure", "animation", "comedy", "drama")
StudioSubset <- c("Buena Vista Studios", "Fox", "Paramount Pictures", "Sony", "Universal", "WB")
subsetdf <- moviedf[moviedf$Genre %in% GenreSubset & moviedf$Studio %in% StudioSubset,]  #i will work with a new df, the "subsetdf" which is the moviedf minus the unwanted Genres
subsetdf$Studio <- factor(subsetdf$Studio)
subsetdf$Genre <- factor(subsetdf$Genre)
str(subsetdf)
summary(subsetdf)

#start plotting
myplot <- ggplot(data=subsetdf, aes(x=Genre, y=Gross...US))
q <- myplot + geom_jitter(aes(size=Budget...mill.,colour=Studio)) + 
  geom_boxplot(alpha=0.7, outlier.colour = NA, show.legend = FALSE) +
  xlab("Genre") + 
  ylab("Gross % US") + 
  ggtitle("Domestic Gross % by Genre") +
  theme(
                                             text = element_text(family="Comic Sans MS"),
                                             axis.title.x = element_text(colour="Blue", size=30),
                                             axis.title.y = element_text(colour="Blue", size=30),
                                             axis.text.x = element_text(size=20),
                                             axis.text.y = element_text(size=20),
                                             legend.title = element_text(size=15),
                                             legend.text = element_text(size=11),
                                             plot.title = element_text(colour="Black", size=30, hjust=0.5))
q$labels$size <- "Budget $ M"
q                                             


