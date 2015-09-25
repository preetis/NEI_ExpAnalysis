#Script: Plot5.R
#Purpose: Read the National Emissions Inventory Database and answer the question below:
#         Question: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
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

#Create dataset with only the required elements from NEI dataset by specifically filtering for emissions data for Baltimore (fips=24510) 
#Remove any NAs in the year or Emissions column and rename column 'type' to 'Source' for easier readability
NEI_emission_source <- filter(NEI, is.na(year) == FALSE & is.na(Emissions) == FALSE & fips == "24510") %>% 
  select(SCC, Emissions, year, type) %>% #Select only the required columns
  rename(Source = type) #Rename column type to Source

#Retrieve emission data for only motor vehice sources based on joining the datasets- motorVehicleSources, NEI_emission_source
motorVehicleEmission <- merge(x = motorVehicleSources, y=NEI_emission_source, all.x=FALSE)

#Group the emission data by year to get enable summarization of the emissions from all motor vehicle sources at year level
groupByYear <- group_by(motorVehicleEmission, year)
TotalEmissionPerYear <-summarize(groupByYear, totalEmissions = sum(Emissions))

#Use ggplot2 graphing system to show total emissions per source from 1999 to 2008 for Baltimore city for motor vehicle sources
g<-ggplot(TotalEmissionPerYear, aes(clarity,x = year, y = totalEmissions))
print(g+
        scale_x_continuous(breaks=TotalEmissionPerYear$year)+ #Define the scale for x-axis
        geom_bar(stat = "identity", width = 1, fill="coral") + #Put bars side by side instead of stacked
        geom_text(aes(y = totalEmissions,label=round(totalEmissions)), color="black", size=3) + #Add total emission measure for each bar
        geom_smooth(method="lm", linetype = 2, se=FALSE)+ #Add regression line to show the trend of total emission over the years for the city
        labs(list(  #Set the title, x and y axis labels for the plot
          title=expression("Total Emissions from " * PM[2.5] * " for Baltimore, Maryland"),
          x="Year", 
          y=expression("Total Emissions from " * PM[2.5]))))

#Copy the graph to a png file
ggsave("Plot5.png")