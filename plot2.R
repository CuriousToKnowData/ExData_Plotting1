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

generate_plot_2 <- function() {
  #read the data
  data = read_power_data()
  par(mfrow = c(1,1))
  png("plot2.png", width = 480,height = 480)
  data$datetime <- strptime(paste(data$Date, data$Time), "%Y-%m-%d %H:%M:%S")
  data$datetime_posixct <- as.POSIXct(data$datetime)

  with(data, plot(Global_active_power ~ datetime_posixct, type = "l",
                  ylab = "Global Active Power (kilowatts)", 
                  xlab = "", xaxt="n"))
  mid = round(length(data$datetime_posixct)/2)
  end = length(data$datetime_posixct)-1
  axis(side=1, at=c(data$datetime_posixct[1], data$datetime_posixct[mid], data$datetime_posixct[end]), labels=c("Thurs","Fri","Sat"))
  
  dev.off()
}

generate_plot_2()

