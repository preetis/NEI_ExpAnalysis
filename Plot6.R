#Script: Plot6.R
#Purpose: Read the National Emissions Inventory Database and answer the question below:
#         Question: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
#                   vehicle sources in Los Angeles County, California (fips == "06037"). 
#                   Which city has seen greater changes over time in motor vehicle emissions?
##
##Assumption: If the EI.Sector column in SCC dataset contains the word "mobile", then it is a motor vehicle source

#Load the required R libraries 
library(dplyr)
library(ggplot2)

#Set working directory
setwd("~/OneDrive/Coursera/ExpDataAnalysis/Wk3/Project")

#Read the data for National Emissions Inventory Database into dataframe- NEI with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008
NEI <- readRDS(paste(getwd(),"/Data/NEI_data/summarySCC_PM25.rds",sep=""))
#Read the data from Source Classification Code Table containing mapping from the SCC digit strings in the Emissions table to the actual name and details of the PM2.5 source
SCC <- readRDS(paste(getwd(),"/Data/NEI_data/Source_Classification_Code.rds",sep=""))

#Filter out the motor vehicle sources from Source Classification code dataset- SCC. Assumption is if the EI.Sector contains the word "mobile", then it is a motor vehicle source
motorVehicleSources <- filter(SCC,grepl("mobile", SCC$EI.Sector, ignore.case=TRUE))

#Create dataset with only the required elements from NEI dataset by specifically filtering for emissions data for Baltimore (fips=24510) and Los Angeles (fips=06037). 
#Remove any NAs in the year or Emissions column and create a new column State to assign the state name based on the fips code for easier readability
NEI_emission_source <- filter(NEI, is.na(year) == FALSE & is.na(Emissions) == FALSE & (fips == "24510" | fips == "06037")) %>% 
  select(SCC, Emissions, year, type, fips) %>% 
  rename(Source = type) %>% 
  mutate(State = ifelse(fips == "24510", "Baltimore","Los Angeles County"))

#Retrieve emission data for only motor vehice sources based on joining the datasets- motorVehicleSources, NEI_emission_source
motorVehicleEmission <- merge(x = motorVehicleSources, y=NEI_emission_source, all.x=FALSE)

#Group the emission data by year and State to get enable summarization of the emissions data at year and State level
groupByYearByState <- group_by(motorVehicleEmission, year, State)
TotalEmissionPerYear <-summarize(groupByYearByState, totalEmissions = sum(Emissions))

#Define color palette to use for the plot
cbPalette <- c("orange", "lightcoral")

#Use ggplot2 graphing system to show comparison of total emissions from motor vehicle sources from 1999 to 2008 for Baltimore vs Los Angeles
g<-ggplot(TotalEmissionPerYear,aes(year,totalEmissions,fill=State, color=State))
print(g+
        geom_bar(stat="identity",position='dodge')+ #Put bars side by side instead of stacked
        scale_fill_manual(values=cbPalette)+ #Set the colors to be used for filss
        scale_colour_manual(values=cbPalette)+ #Set colors to be used for lines
        scale_x_continuous(breaks=TotalEmissionPerYear$year)+ #Define the scale for x-axis
        geom_text(aes(label=round(totalEmissions)), color="black", size=3) + #Add total emission measure for each bar
        geom_smooth(method="lm", linetype = 2, se=FALSE)+ #Add regression line to show the trend of total emission over the years for each city
        labs(list( #Set the title, x and y axis labels for the plot
          title=expression("Comparison of Total Emissions from " * PM[2.5] * " for Baltimore and Los Angeles County"), 
          x="Year", 
          y=expression("Total Emissions from " * PM[2.5]))))

#Copy the graph to a png file
ggsave("Plot6.png")