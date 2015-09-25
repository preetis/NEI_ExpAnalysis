#Script: Plot3.R
#Purpose: Read the National Emissions Inventory Database and answer the question below:
#         Question: Of the 4 types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
#                   which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#                   Which have seen increases in emissions from 1999–2008? 
#                   Use the ggplot2 plotting system to make a plot answer this question.

#Load the required R libraries 
library(dplyr)
library(ggplot2)

#Set working directory
setwd("~/OneDrive/Coursera/ExpDataAnalysis/Wk3/Project")

#Read the data for National Emissions Inventory Database into dataframe- NEI with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008
NEI <- readRDS(paste(getwd(),"/Data/NEI_data/summarySCC_PM25.rds",sep=""))

#Create dataset with only the required elements from NEI dataset by filtering out emissions data for Baltimore city only. 
#Remove any NAs in the year or Emissions column and rename column 'type' to 'Source' for easier readability
NEI_emission_source <- filter(NEI, fips == "24510" & is.na(year) == FALSE & is.na(Emissions) == FALSE) %>% select(Emissions, year, type) %>% rename(Source = type)

#Group the emission data for Baltimore by year to get enable summarization of the emissions data at year level
GroupByYear <- group_by(NEI_emission_source, year)
TotalEmissionPerYear <-summarize(GroupByYear, totalEmissions = sum(Emissions))

#Add a label 'All Sources' to the total emission data summarized at year level for all sources 
TotalEmissionPerYear <- mutate(TotalEmissionPerYear, Source = "All Sources")

#Group the emission data by year and source to get enable summarization of the emissions data at year and source identifed by column- Source
groupByYearBySource <- group_by(NEI_emission_source, year, Source)
TotalEmissionPerYearPerSource <- summarize(groupByYearBySource, totalEmissions = sum(Emissions))

#Merge the dataset- TotalEmissionPerYear and TotalEmissionPerYearPerSource to get the summarized Total Emission information for all sources
TotalEmissionBaltimore <- merge(x=TotalEmissionPerYear, y=TotalEmissionPerYearPerSource, all=TRUE)

#Use ggplot2 graphing system to show total emissions per source across 1999 to 2008 for Baltimore city
g<-ggplot(TotalEmissionBaltimore, aes(year, totalEmissions, color = Source, label=totalEmissions))
print(g+
        geom_point()+  #Plot the points
        geom_path() +  #Connect the emission points to show trend 
        scale_y_continuous(limits=c(-1000, 4000))+ #Set limits for y-axis
        geom_text(aes(label=as.character(round(totalEmissions))), vjust=2, hjust=0.5, size=3) + #Add data point values to each point
        facet_grid(Source ~ .) + #Lays out panels in a grid to show emissions across different sources
        geom_smooth(method="lm", color = "black", linetype = 2, se=FALSE)+ #Add a dashed regression line in black to add a smoothed conditional mean to each panel
        labs(list(  #Set the title, x and y axis labels for the plot
          title=expression("Total Emissions from " * PM[2.5] * " for Baltimore, Maryland"), 
          x="Year", 
          y=expression("Total Emissions from " * PM[2.5]))))

#Copy the graph to a png file
ggsave("Plot3.png")