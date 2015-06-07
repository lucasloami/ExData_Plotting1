library(lubridate)
Sys.setlocale("LC_TIME", "eng")

read_data <- function(data_path, col_names) {
  data <- read.table(data_path, sep=';', col.names=col_names, skip=66637, nrow=2880)
  return(data)
}

convert_date <- function(x) {
  return(wday(as.Date(x, "%d/%m/%Y"), label=TRUE))
  # return(strptime(x, "%d/%m/%Y %H:%M:%S"))
}

convert_thousand <- function(x) {
  character_val <- as.character(x)
  return(as.numeric(character_val))
}

clean_data <- function(data) {
  data$Datetime <- as.POSIXlt(paste(as.Date(data$Date, format="%d/%m/%Y"), data$Time, sep=" "))
  data['Global_active_power'] <- apply(data['Global_active_power'], 1, convert_thousand)

  df <- data.frame(Datetime=data['Datetime'], Global_active_power=data['Global_active_power'])

  return(df)
}

build_plot <- function(data) {
  plot(data$Datetime, data$Global_active_power, type="l", ylab="Global Active Power", xlab="")
  dev.copy(png, file="./figure/plot02.png", width=480, height=480)
  dev.off()
}

exec <- function() {
  data_path <- "./data/household_power_consumption.txt"
  col.names <- c("Date", "Time", "Global_active_power", "Global_reactive_power",
    "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2","Sub_metering_3")
  data <- read_data(data_path, col.names)
  cleaned_data <- clean_data(data)
  build_plot(cleaned_data)
}