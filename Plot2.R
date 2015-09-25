#Script: Plot2.R
#Purpose: Read the National Emissions Inventory Database and answer the question below:
#         Question: Have total emissions from PM2.5 decreased in the 
#                   Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#                   Use the base plotting system to make a plot answering this question

#Load the required R libraries 
library(dplyr)

#Set working directory
setwd("~/OneDrive/Coursera/ExpDataAnalysis/Wk3/Project")

#Read the data for National Emissions Inventory Database into dataframe- NEI with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008
NEI <- readRDS(paste(getwd(),"/Data/NEI_data/summarySCC_PM25.rds",sep=""))

#Create dataset with only the required elements from NEI dataset by filtering out emissions data for Baltimore city only. 
#Remove any NAs in the year or Emissions column and rename column 'type' to 'Source' for easier readability
NEI_emission_source <- filter(NEI, fips == "24510" & is.na(year) == FALSE & is.na(Emissions) == FALSE) %>% select(Emissions, year, type)

#Group the emission data for Baltimore city by year to get enable summarization of the emissions data at year level
GroupByYear <- group_by(NEI_emission_source, year)
TotalEmissionPerYear <-summarize(GroupByYear, totalEmissions = sum(Emissions))

#Establish the plot framework without drawing any points
par(mar = c(4,4,4,1))
with(TotalEmissionPerYear, plot(year, totalEmissions, type="n", ylab="Total emissions from PM2.5", xlab="Year", main="Total PM2.5 emissions in Baltimore, Maryland"))

#Plot the total emission from PM2.5 between years 1999 to 2008
with(TotalEmissionPerYear, points(year, totalEmissions, pch=19, type="b", col = "red"))

#Plot a regression line for the total emission from PM2.5 between years 1999 to 2008
model<-lm(totalEmissions ~ year, TotalEmissionPerYear)
abline(model, lty =2, col="darkgrey" )

#Copy the graph to a png file
dev.copy(png, file="Plot2.png")
dev.off()