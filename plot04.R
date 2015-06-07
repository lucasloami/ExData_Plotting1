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
  data['Voltage'] <- apply(data['Voltage'], 1, convert_thousand)
  data['Global_reactive_power'] <- apply(data['Global_reactive_power'], 1, convert_thousand)
  data['Sub_metering_1'] <- apply(data['Sub_metering_1'], 1, convert_thousand)
  data['Sub_metering_2'] <- apply(data['Sub_metering_2'], 1, convert_thousand)
  data['Sub_metering_3'] <- apply(data['Sub_metering_3'], 1, convert_thousand)

  return(data)
}

build_plot <- function(data) {
  par(mfrow=c(2,2))
  with(data, {
    #draw first chart
    plot(Datetime, Global_active_power, type="l", ylab="Global Active Power", xlab="")
    #draw second chart
    plot(Datetime, Voltage, type="l", ylab="Voltage", xlab="datetime", yaxt="n")
    axis(2, at=seq(234, 246, 4))
    # draw third chart
    plot(Datetime, Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
    lines(Datetime, Sub_metering_2, col="red")
    lines(Datetime, Sub_metering_3, col="blue")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
      lty=c(1,1), col=c("black", "red", "blue"))
    # draw fourth chart
    plot(Datetime, Global_reactive_power, type="l", ylab="Global_reactive_power",
     xlab="datetime")

  })
  dev.copy(png, file="plot04.png", width=480, height=480)
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