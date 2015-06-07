Sys.setlocale("LC_TIME", "eng")

read_data <- function(data_path, col_names) {
  data <- read.table(data_path, sep=';', col.names=col_names, skip=66637, nrow=2880)
  return(data)
}
convert_date <- function(x) {
  return(as.Date(x, "%d/%m/%Y"))
}

convert_thousand <- function(x) {
  character_val <- as.character(x)
  return(as.numeric(character_val))
}

clean_data <- function(data) {
  data['Date'] <- apply(data['Date'], 1, convert_date)
  data['Global_active_power'] <- apply(data['Global_active_power'], 1, convert_thousand)

  return(data)
}

build_plot <- function(data) {
  hist(data$Global_active_power, col="red", xlab="Global Active Power (kilowatts)",
    main="Global Active Power")
  dev.copy(png, file="./figure/plot01.png", width=480, height=480)
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