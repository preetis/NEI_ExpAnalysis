#Script: Plot4.R
#Purpose: Read the National Emissions Inventory Database and answer the question below:
#         Question: Across the United States, how have emissions from coal combustion-related
#                   sources changed from 1999–2008?
##
##Assumption: If the SCC.Level.One contains the word "comb" and SCC.Level.Four contains "coal", then it is a coal combustion related source

#Load the required R libraries 
library(dplyr)
library(ggplot2)

#Set working directory
setwd("~/OneDrive/Coursera/ExpDataAnalysis/Wk3/Project")

#Read the data for National Emissions Inventory Database into dataframe- NEI with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008
NEI <- readRDS(paste(getwd(),"/Data/NEI_data/summarySCC_PM25.rds",sep=""))
#Read the data from Source Classification Code Table containing mapping from the SCC digit strings in the Emissions table to the actual name and details of the PM2.5 source
SCC <- readRDS(paste(getwd(),"/Data/NEI_data/Source_Classification_Code.rds",sep=""))

#Filter out the coal combustion related sources from Source Classification code dataset- SCC. Assumption is if the SCC.Level.One contains the word "comb" and SCC.Level.Four contains "coal", then it is a coal combustion related source
CoalSources <- filter(SCC,grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE) & grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE))

#Create dataset with only the required elements from NEI dataset. Remove any NAs in the year or Emissions column and rename column 'type' to 'Source' for easier readability
NEI_emission_source <- filter(NEI, is.na(year) == FALSE & is.na(Emissions) == FALSE) %>% 
  select(SCC, Emissions, year, type) %>% #Select only the required columns
  rename(Source = type) #Rename column type to Source

#Retrieve emission data for only coal combustion related sources based on joining the datasets- CoalSources, NEI_emission_source
CoalEmission <- merge(x = CoalSources, y=NEI_emission_source, all.x=FALSE)

#Group the emission data by year to get enable summarization of the emissions from coal combustion related sources at year level
GroupByYear <- group_by(CoalEmission, year)
TotalEmissionPerYear <-summarize(GroupByYear, totalEmissions = sum(Emissions))

#Add a label 'All coal-combustion sources' to the total emission data summarized at year level for all sources
TotalEmissionPerYear <- mutate(TotalEmissionPerYear, EI.Sector = "All coal-combustion sources")

#Group the emission data by year and source to get enable summarization of the emissions data at year and source level (identified by EI.Sector column in the dataset)
GroupByYearBySource <- group_by(CoalEmission, year, EI.Sector)
TotalEmissionPerYearPerSource <- summarize(GroupByYearBySource, totalEmissions = sum(Emissions))

#Merge the dataset- TotalEmissionPerYear and TotalEmissionPerYearPerSource to get the summarized Total Emission information for all sources combined and at individual source level (identified by EI.Sector column in the dataset)
TotalEmission <- merge(x=TotalEmissionPerYear, y=TotalEmissionPerYearPerSource, all=TRUE)

#Use ggplot2 graphing system to show total emissions from coal combustion related sources across 1999 to 2008 in U.S.
g<-ggplot(TotalEmission, aes(clarity, x = year, y = totalEmissions, color = EI.Sector, label=totalEmissions))
print(g+
        geom_point()+  #Plot the points
        geom_path()+   #Connect the emission points to show trend 
        facet_grid( EI.Sector ~ .) + #Lays out panels in a grid to show emissions across different coal combustion related sources
        geom_text(aes(label=round(totalEmissions)), color="black", size=2.25, angle=45) + #Add data point values to each point
        geom_smooth(method="lm", color = "black", linetype = 2, se=FALSE)+ #Add a dashed regression line in black to add a smoothed conditional mean to each panel
        labs(list(  #Set the title, x and y axis labels for the plot
          title=expression("Total Emissions from " * PM[2.5] * " for United States"),  
          x="Year", 
          y=expression("Total Emissions from " * PM[2.5]))))

#Copy the graph to a png file
ggsave("Plot4.png")