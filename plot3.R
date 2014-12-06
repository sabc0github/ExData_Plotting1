## Introduction
## ------------
## This assignment uses data from the UC Irvine Machine Learning Repository, a popular repository for machine learning datasets. 
## In particular, we will be using the "Individual household electric power consumption Data Set":
##        
##        * Dataset: Electric power consumption [20Mb]
##        * Description: Measurements of electric power consumption in one household with a one-minute sampling rate over a period of almost 4 years. 
##          Different electrical quantities and some sub-metering values are available.
##
## The following descriptions of the 9 variables in the dataset are taken from the UCI web site:
##        
##        1. Date: Date in format dd/mm/yyyy
##        2. Time: time in format hh:mm:ss
##        3. Global_active_power: household global minute-averaged active power (in kilowatt)
##        4. Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
##        5. Voltage: minute-averaged voltage (in volt)
##        6. Global_intensity: household global minute-averaged current intensity (in ampere)
##        7. Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
##        8. Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
##        9. Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.
## 
## Loading the data
## ----------------
## When loading the dataset into R, please consider the following:
##        
##      * The dataset has 2,075,259 rows and 9 columns. First calculate a rough estimate of how much memory the dataset will require in memory before reading into R. 
##        Make sure your computer has enough memory (most modern computers should be fine).
##      * We will only be using data from the dates 2007-02-01 and 2007-02-02. 
##        One alternative is to read the data from just those dates rather than reading in the entire dataset and subsetting to those dates.
##      * You may find it useful to convert the Date and Time variables to Date/Time classes in R using the strptime() and as.Date() functions.
##      * Note that in this dataset missing values are coded as ?.
##
## Making Plots
## ----------------
## Our overall goal here is simply to examine how household energy usage varies over a 2-day period in February, 2007. 
## Your task is to reconstruct the following plots below, all of which were constructed using the base plotting system.
##
## First you will need to fork and clone the following GitHub repository: https://github.com/rdpeng/ExData_Plotting1
##
## For each plot you should
## 
##      * Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
##      * Name the plot files as plot3.png.
##      * Create a separate R code file (plot3.R) that constructs the corresponding plot, i.e. code in plot3.R constructs the plot3.png plot. 
##        Your code file should include code for reading the data so that the plot can be fully reproduced. You should also include the code that creates the PNG file.
##      * Add the PNG file and R code file to your git repository
## 
## When you are finished with the assignment, push your git repository to GitHub so that the GitHub version of your repository is up to date. There should be four PNG files and four R code files.

plot3 <- function(){
        
        ## Import Libraries
        library(data.table) 
        
        ## Subset the data for Header Names
        hpcHeadres <- names(fread(
                input = "./data/household_power_consumption.txt",
                sep = ";",
                header = TRUE,
                na.strings = c("?", ""),
                nrows = 1,
                colClasses = c("character", "character", "character", "character", "character", "character", "character", "character", "character"),
                data.table = TRUE
        ))
        
        ## Subset the data starting from the date 1/2/2007 and read rows = 2880 (i.e., 60 minutes * 24 hours * 2 days)
        hpcData <- fread(
                input = "./data/household_power_consumption.txt",
                sep = ";",
                header = FALSE,
                na.strings = c("?", ""),
                skip = "1/2/2007",
                nrows = 60 * 24 * 2,
                colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"),
                data.table = TRUE
        )        
        
        ## Set the HeaderNames to Data Table
        setnames(hpcData, hpcHeadres)
        
        ## convert the Date and Time variables to Date/Time classes in R using the strptime() function.
        DateTime <- strptime(paste(hpcData$Date, hpcData$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
        
        ## Append the DateTime column to the Data Table
        hpc <- cbind(hpcData, DateTime)
        
        ## Construct Line plot
        plot(hpc$DateTime, hpc$Sub_metering_1, xlab = "", ylab = "Energy sub metering", type = "l")
        lines(hpc$DateTime, hpc$Sub_metering_2, col = "Red")
        lines(hpc$DateTime, hpc$Sub_metering_3, col = "Blue")
        
        ## Add Legend to the Line Plot
        legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd = 1, col = c("Black", "Red", "Blue"))

        ## Copy my plot to PNG file
        dev.copy(png, file = "plot3.png")
        
        ## Close the PNG Device
        dev.off()        
}