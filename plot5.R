# Data Science Course,  Exploratory data analysis project week 4

# Michalis Frangos
# frangos@frangos.eu

# set working directory
script.dir <- 'D:/FRANGOS_FOLDER/CoursesCertificates/Coursera_Spec_DataAnalysis_2016/ExploratoryDataAnalysis/ExploringData_project'

# Set working directory
#script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

#rm(list = ls()) 

#library(R.utils)
library(httr)
library(plyr)
library(dplyr)
library(ggplot2)


## DOWNLOADING and UNZIPING DATA Function
downloadDataFile <- function(fileUrl,zipFileName,fileName1,fileName2){
        zipFileDir <- "./downloads/project_data.zip"
        if (!file.exists("downloads") |(file.exists("downloads") & !file.exists(zipFileDir))){
                message("- downloading data")
                dir.create("downloads")
                download.file(fileUrl,destfile = "./downloads/project_data.zip",method = "auto") 
                dateDownloaded <- date()
                message("- data downloaded")
                #file.remove("./downloads/project_data.zip")
        } else {
                message("- data already downloaded")  
        }
        
        if  (!file.exists(fileName1)|!file.exists(fileName2)){
                message("- unzipping data")
                unzip("./downloads/project_data.zip")
                message("- data unzipped")
        } else {
                message("- data file exists")      
        }
        
}



## LOADING DATA; returns a list of data frames
loadData <- function(fileName){
        
        message("- loading data (takes some time)")
        
        ## read each of the two files using the readRDS() function in R 
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        dataList <- list(NEI,SCC)
        
        message("- data loaded")
        return(dataList)
}

makePlot5 <- function(data){
        # How have emissions from motor vehicle sources changed from 1999-2008 
        # in Baltimore City?
        
        message("- making plot")
        
        # define options 
        titleString <- "Emissions from motor vehicle sources in Baltimore City"
        xlabelString <- "Year" 
        ylableString <- "Total Emissons"
        
        place <- "24510"
        #select = c(Emissions, year,type))%>% 
        
        
        motorVehicles <- grep("([H]ighway *[Vv]eh)",SCC$Short.Name)
        motorVehiclesSCC <- unique(SCC$SCC[motorVehicles])
        
        
        years <- c(1999,2002,2005,2008)
        
        df <- subset(NEI,    NEI$year %in% years &
                             NEI$SCC %in% motorVehiclesSCC &
                             NEI$fips==place,
                     select = c(Emissions, year))%>% 
                group_by(year) %>%
                summarise(totalEmissions=sum(Emissions, na.rm=TRUE))
        df<-ungroup(df)
        
        localenv <- environment()
        g <- ggplot(df,aes(year,totalEmissions)) + labs(title = titleString)
        g <- g + geom_point() + geom_smooth(method = "lm") # + facet_grid(.~type)
        print(g)
        
        message("- plot completed")
        
        return(g)
        
}



## MAKING PLOTS

fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
zipFileName <- "exdata_data_NEI_data.zip"
fileName1 <- "summarySCC_PM25.rds"
fileName2 <- "Source_Classification_Code.rds"

downloadDataFile(fileUrl,zipFileName,fileName1,fileName2)

flagLoad <- 0
if (flagLoad==1){
        message("- loading data (takes some time)")
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
}


graphics.off() 
message("- data loaded")

png(filename ="plot5.png", width = 480, height = 480)
makePlot5(NEI)
dev.off()
