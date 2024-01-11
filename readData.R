#dataURL = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
#download.file(dataURL, "./data/power.csv", method = "curl")

library(readr)

read_power_data <- function() {
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
  filter_condition = (power$Date >= "2007-02-01" & power$Date <= "2007-02-02")
  power_slice = power[filter_condition,]
  return(power_slice)
}

max(data$Date)
min(data$Date)

data = read_power_data()

