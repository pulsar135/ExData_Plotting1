#This function requires the use of the dplyr and data.table packages
#This function reads in household power data, creates a histogram plot and
#saves the plot to a png file

plot1 <- function(homedir = getwd()) {
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
      # date/time class and assign as a new parameter
      datetime <- strptime(paste(hpc$date, hpc$time, sep = " "), 
                           format = "%Y-%m-%d %H:%M:%S")
      
      
      #Bind the new datetime parameter to household power dataset by column
      hpc <- cbind(datetime, hpc)
      
      
      #Launch png graphics device, call plot function to create plot and close
      #png graphics device
      png(filename = "plot1.png")
      
      #plot1
      with(hpc, hist(global_active_power, col = "red", 
                     xlab = "Global Active Power (kilowatts)", 
                     main = "Global Active Power"))
      
      dev.off()
}