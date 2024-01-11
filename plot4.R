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

plot_1 <- function(data) {
  data$datetime <- strptime(paste(data$Date, data$Time), "%Y-%m-%d %H:%M:%S")
  data$datetime_posixct <- as.POSIXct(data$datetime)
  
  with(data, plot(Global_active_power ~ datetime_posixct, type = "l",
                  ylab = "Global Active Power (kilowatts)", 
                  xlab = "", xaxt="n"))
  mid = round(length(data$datetime_posixct)/2)
  end = length(data$datetime_posixct)-1
  axis(side=1, at=c(data$datetime_posixct[1], data$datetime_posixct[mid], data$datetime_posixct[end]), labels=c("Thurs","Fri","Sat"))
}

plot_2 <- function(data) {
  data$datetime <- strptime(paste(data$Date, data$Time), "%Y-%m-%d %H:%M:%S")
  data$datetime_posixct <- as.POSIXct(data$datetime)
  
  with(data, plot(Voltage ~ datetime_posixct, type = "l",
                  ylab = "Voltage", 
                  xlab = "datetime", xaxt="n"))
  mid = round(length(data$datetime_posixct)/2)
  end = length(data$datetime_posixct)-1
  axis(side=1, at=c(data$datetime_posixct[1], data$datetime_posixct[mid], data$datetime_posixct[end]), labels=c("Thurs","Fri","Sat"))
}

plot_3 <- function(data) {
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
  legend("topright", inset = c(0.0, 0.0), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
          bty = "n", x.intersp = 0.75, y.intersp = 0.55, cex = 0.65, lwd = 1, col = c('black', 'red', 'blue'))
}

plot_4 <- function(data) {
  data$datetime <- strptime(paste(data$Date, data$Time), "%Y-%m-%d %H:%M:%S")
  data$datetime_posixct <- as.POSIXct(data$datetime)
  
  with(data, plot(Global_reactive_power ~ datetime_posixct, type = "l",
                  ylab = "Global_reactive_power", 
                  xlab = "datetime", xaxt="n"))
  mid = round(length(data$datetime_posixct)/2)
  end = length(data$datetime_posixct)-1
  axis(side=1, at=c(data$datetime_posixct[1], data$datetime_posixct[mid], data$datetime_posixct[end]), labels=c("Thurs","Fri","Sat"))
}

generate_4_plots_consolidation <- function() {
  #read the data
  data = read_power_data()
  png("plot4.png",width = 480,height = 480)
  par(mfrow = c(2,2), mar=c(4,4,4,4))
  plot_1(data)
  plot_2(data)
  plot_3(data)
  plot_4(data)
  
  dev.off()
}

generate_4_plots_consolidation()
