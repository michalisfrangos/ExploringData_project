# Data Science Course,  Exploratory data analysis project week 4
# Michalis Frangos; frangos@frangos.eu

# clear workspace
#rm(list = ls()) 

## DOWNLOADING and UNZIPING DATA Function
downloadDataFile <- function(fileUrl,zipFileName,fileName1,fileName2){
    zipFileDir <- "./downloads/project_data.zip"
    if (!file.exists("downloads") |(file.exists("downloads") & !file.exists(zipFileDir))){
        message("- downloading data")
        dir.create("downloads")
        download.file(fileUrl,destfile = "./downloads/project_data.zip",method = "auto") 
        dateDownloaded <- date()
        message("- data downloaded")
    } else {message("- data already downloaded")}
    if  (!file.exists(fileName1)|!file.exists(fileName2)){
        message("- unzipping data")
        unzip("./downloads/project_data.zip")
        message("- data unzipped")
    } else {message("- data file exists")}
}

makePlot1 <- function(NEI){
        message("- making plot")
        titleString <- "Total PM2.5 emission from all sources for each year"
        xlabelString <- "Year" 
        ylableString <- "Total Emissons"
        
        years <- c(1999,2002,2005,2008)
        df <- subset(NEI,NEI$year %in% years,select = c(Emissions, year))%>% 
                group_by(year) %>%
                summarise(totalEmissions=sum(Emissions, na.rm=TRUE))
        
        plot(df$year,df$totalEmissions,
             xlim = c(1998,2009), ylim = range(df$totalEmission), 
             pch = 20, lwd = 5,
             main = titleString,
             xlab = xlabelString, ylab = ylableString)
        
        model <-lm(totalEmissions~year,df)
        abline(model,lwd = 2,col = 'blue')
        message("- plot completed")
}

## MAKING PLOTS
library(httr)
library(dplyr)

fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
zipFileName <- "exdata_data_NEI_data.zip"
fileName1 <- "summarySCC_PM25.rds"
fileName2 <- "Source_Classification_Code.rds"
downloadDataFile(fileUrl,zipFileName,fileName1,fileName2)

if(!exists("NEI") | !exists("SCC")){
    message("- loading data (takes some time)")
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    message("- data loaded")
}

graphics.off() 
png(filename ="plot1.png", width = 840, height = 480)
makePlot1(NEI)
dev.off()