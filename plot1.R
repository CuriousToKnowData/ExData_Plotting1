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

generate_plot_1 <- function() {
  #read the data
  data = read_power_data()
  par(mfrow = c(1,1))
  png("plot1.png", width = 480,height = 480)
  hist(data$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)" )
  dev.off()
}

generate_plot_1()
