# Data Science Course,  Exploratory data analysis project week 4
# Michalis Frangos; frangos@frangos.eu
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
        } else {message("- data already downloaded")}
        if  (!file.exists(fileName1)|!file.exists(fileName2)){
                message("- unzipping data")
                unzip("./downloads/project_data.zip")
                message("- data unzipped")
        } else {message("- data file exists")}
}

makePlot6 <- function(NEI,SCC){
        message("- making plot")
        titleString <- "Emissions from motor vehicle sources in Baltimore and Los Angeles"
        xlabelString <- "Year" 
        ylableString <- "Total Emissons"
        
        places <- c("24510","06037")
        typeTransport  <- "ON-ROAD"
        motorVehicles <- grep("[Vv]eh",SCC$Short.Name)
        motorVehiclesSCC <- unique(SCC$SCC[motorVehicles])
        years <- c(1999,2002,2005,2008)
        
        df <- subset(NEI,    NEI$year %in% years &
                             NEI$SCC %in% motorVehiclesSCC &
                             NEI$type %in% typeTransport &
                             NEI$fips %in% places,
                     select = c(Emissions, year,fips))%>% 
                group_by(year,fips) %>%
                summarise(totalEmissions=sum(Emissions, na.rm=TRUE))
        df<-ungroup(df)
        df$fips[df$fips=="06037"]= "LA"
        df$fips[df$fips=="24510"]= "Baltimore"
        localenv <- environment()
        g <- ggplot(df,aes(year,totalEmissions)) 
        g <- g + geom_point(aes(color=fips),size=2) + 
                geom_boxplot(aes(color=fips),outlier.colour = NA, fill = NA) +
                facet_grid(.~fips) + labs(title = titleString) +
                labs(y = "Emissions")

        message("- plot completed")
        print(g)
        return(g)
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
png(filename ="plot6.png", width = 840, height = 480)
makePlot6(NEI,SCC)
dev.off()

