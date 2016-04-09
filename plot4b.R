# Data Science Course,  Exploratory data analysis project week 4
# Michalis Frangos ;  frangos@frangos.eu
#rm(list = ls()) 

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

makePlot4 <- function(data){
        # Across the United States, how have emissions from coal
        # combustion-related sources changed from 1999-2008?
        
        message("- making plot")
        
        # define options 
        titleString <- "Emissions from coal combustion-related sources in the USA"
        xlabelString <- "Year" 
        ylableString <- "Total Emissons"
        
        
        
        coalCombustionSCC <- unique(SCC$SCC[coalCombustion]) 
        years <- c(1999,2002,2005,2008)
        df <- subset(NEI,NEI$year %in% years & NEI$SCC %in% coalCombustionSCC,
                     select = c(Emissions, year))%>% group_by(year) %>% 
            summarise(totalEmissions=sum(Emissions, na.rm=TRUE))
        

        
        coalCombustion <- grepl("[Cc]oal",SCC$EI.Sector) &
                          grepl("[Cc]omb",SCC$EI.Sector)
                                 
        
        coalCombustionSCC <- unique(SCC$SCC[coalCombustion])
        
        years <- c(1999,2002,2005,2008)
        
        dfNEI <- subset(NEI,NEI$year %in% years & NEI$SCC %in% coalCombustionSCC,
                     select = c(Emissions, year))
        
        dfSCC <- subset(SCC,coalCombustion, select = c(SCC,EI.Sector))
        
        df<-merge(dfNEI,dfSCC)%>%
                group_by(year,EI.Sector)%>%
                summarise(totalEmissions=sum(Emissions, na.rm=TRUE))
        
        localenv <- environment()
        g <- ggplot(df,aes(year,totalEmissions,group = EI.Sector,color = EI.Sector)) + 
                labs(title = titleString)
        g <- g + geom_point() +geom_line() 
        print(g)
        
        message("- plot completed")
}


## MAKING PLOTS
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
message("- data loaded")

#png(filename ="plot4.png", width = 480, height = 480)
makePlot4(NEI)
#dev.off()

