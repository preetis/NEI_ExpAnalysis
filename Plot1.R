#Script: Plot1.R
#Purpose: Read the National Emissions Inventory Database and answer the question below:
#         Question: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#         Using the base plotting system, make a plot showing the total PM2.5 emission from all 
#         sources for each of the years 1999, 2002, 2005, and 2008.

#Load the required R libraries 
library(dplyr)

#Set working directory
setwd("~/OneDrive/Coursera/ExpDataAnalysis/Wk3/Project")

#Read the data for National Emissions Inventory Database into dataframe- NEI with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008
NEI <- readRDS(paste(getwd(),"/Data/NEI_data/summarySCC_PM25.rds",sep=""))

#Create dataset with only the required elements- emissions, year and source name from NEI dataset. Remove any NAs in the year or Emissions column
NEI_emission_source <- filter(NEI,is.na(year) == FALSE & is.na(Emissions) == FALSE) %>% 
  select(Emissions, year)

#Group the emission data by year to get enable summarization of the emissions data at year level
GroupByYear <- group_by(NEI_emission_source, year)
TotalEmissionPerYear <-summarize(GroupByYear, totalEmissions = sum(Emissions))

#Establish the plot framework without drawing any points
par(mar = c(4,4,4,2))
with(TotalEmissionPerYear, plot(year, totalEmissions, type="n", ylab="Total emissions from PM2.5", xlab="Year", main="Total emissions from PM2.5 in U.S."))

#Plot the total emission from PM2.5 between years 1999 to 2008
with(TotalEmissionPerYear, points(year, totalEmissions, pch=19, type="b", col = "blue"))

#Plot a regression line for the total emission from PM2.5 between years 1999 to 2008
model<-lm(totalEmissions ~ year, TotalEmissionPerYear)
abline(model, lty =2, col="darkgrey" )

#Copy the graph to a png file
dev.copy(png, file="Plot1.png")
dev.off()