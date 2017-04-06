#This function requires the use of the dplyr and data.table packages
#This function reads in household power data, creates a lattice plot
#of 3 line plots and 1 histogram

plot4 <- function(homedir = getwd()) {
      #Load data.table and dplyr packages
      library(data.table)
      library(dplyr)
      
      
      #Read in the source dataset, change the column names to lowercase and
      #set the date column to date class type
      hpc <- fread(paste(homedir, "household_power_consumption.txt" , sep = "/"), 
                   na.strings = "?")
      
      names(hpc) <- tolower(names(hpc))
      
      hpc$date <- as.Date(hpc$date, format = "%d/%m/%Y")
      
      
      #Filter dataset to just the data for the dates of interest
      hpc <- filter(hpc, date >= "2007-02-01" & date <= "2007-02-02")
      
      
      #Combine date and time column data into one parameter column with 
      #date/time class and assign as a new parameter
      datetime <- strptime(paste(hpc$date, hpc$time, sep = " "), 
                           format = "%Y-%m-%d %H:%M:%S")
      
      
      #Bind the new datetime parameter to household power dataset by column
      hpc <- cbind(datetime, hpc)
      
      
      #Launch png graphics device, call plot function to create plot and close
      #png graphics device
      png(filename = "plot4.png")
      
      #plot4
      par(mfcol=c(2,2))
      
      ##plot1.1
      with(hpc, plot(global_active_power ~ datetime, type = "l", xlab = "", 
                     ylab = "Global Active Power"))
      ##plot2.1
      with(hpc, plot(datetime, sub_metering_1, xlab = "", 
                     ylab = "Energy sub metering", type = "n"))
      
      with(hpc, points(datetime, sub_metering_1, col = "black", type = "l"))
      
      with(hpc, points(datetime, sub_metering_2, col = "red", type = "l"))
      
      with(hpc, points(datetime, sub_metering_3, col = "blue", type = "l"))
      
      legend("topright", lty = c(1, 1, 1), col=c("black", "red", "blue"), 
             legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
             bty = "n", cex = 0.95, pt.cex = 1)
      
      ##plot1.2
      with(hpc, plot(voltage ~ datetime, type = "l", xlab = "datetime", 
                     ylab = "Voltage"))
      
      ##plot2.2
      with(hpc, plot(global_reactive_power ~ datetime, type = "l", 
                     xlab = "datetime", ylab = "Global_reactive_power"))
      
      dev.off()
}