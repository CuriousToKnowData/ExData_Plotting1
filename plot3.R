library(readr)

read_power_data <- function() {
  #read data from saved location
  power <- read_delim("data/power.csv", delim = ";",
                      escape_double = FALSE, 
                      col_types = cols(Date = col_date(format = "%d/%m/%Y"),
                                       Time = col_time(format = "%H:%M:%S"),
                                       Global_active_power = col_number(),
                                       Global_reactive_power = col_number(),
                                       Voltage = col_number(), 
                                       Global_intensity = col_number(),
                                       Sub_metering_1 = col_number(),
                                       Sub_metering_2 = col_number(),
                                       Sub_metering_3 = col_number()),
                      trim_ws = TRUE)
  #filter the required time slice
  filter_condition = (power$Date >= "2007-02-01" & power$Date <= "2007-02-02")
  power_slice = power[filter_condition,]
  return(power_slice)
}

generate_plot_3 <- function() {
  #read the data
  data = read_power_data()
  
  #set layout
  par(mfrow = c(1,1))
  
  #set name of the plot
  png("plot3.png", width = 480,height = 480)
  
  #data preproc
  data$datetime <- strptime(paste(data$Date, data$Time), "%Y-%m-%d %H:%M:%S")
  data$datetime_posixct <- as.POSIXct(data$datetime)
  
  #plot
  with(data, plot(Sub_metering_1 ~ datetime_posixct, type = "l",
                  ylab = "Energy sub metering", 
                  xlab = "", xaxt="n"))
  with(data, lines(Sub_metering_2 ~ datetime_posixct, type = "l", col = "red"))
  with(data, lines(Sub_metering_3 ~ datetime_posixct, type = "l", col = "blue"))

  #setting for x-axis
  mid = round(length(data$datetime_posixct)/2)
  end = length(data$datetime_posixct)-1
  axis(side=1, at=c(data$datetime_posixct[1], data$datetime_posixct[mid], data$datetime_posixct[end]), labels=c("Thurs","Fri","Sat"))
  
  #setting for legend  
  legend("topright", inset = c(0, 0), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
          x.intersp = 1.0, y.intersp = 1.0, cex = 0.5, lwd = 1, col = c('black', 'red', 'blue'))
  
  #reset to off
  dev.off()
}

generate_plot_3()

