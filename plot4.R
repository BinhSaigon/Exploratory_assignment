plot3 <- function(fileUrl = NULL, dataAlready = TRUE,...){
  ## Validate if data has already been placed in working dir, 
  ## data processing is implemented step-by-step.
  
  if (!dataAlready){
    ##code for downloading data from url and extracting to ./data
    ##"data" folder should be created in working dir after this chunk.
    if (!file.exists("data")){
      dir.create("data")
    }
    
    ##checking defaul Url, NULL = default Url
    if (is.null(fileUrl)){
      fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"  
    }
    
    ##download and extract data files into ./data folder
    print(paste("Please wait for downloading datasets from", fileUrl))
    filename <- "hh-power.zip"
    download.file(fileUrl, filename,...) ##should be passed method="curl" for MAC OSX
    print("Downloading has been completed.")
    print(paste("Filename", filename, "is created at", date()))
    print(paste("Unzipping", filename, "into ./data folder."))
    unzip(filename, exdir="./data")
    print("Unzipping is completed")
  }
  
  filedir <- paste0("./", "data")
  
  ##STEP 1: preparing dataframe
  print("Plot 3 is proceeding...")
  
  ## reading Date colummn and finding range value for graphics
  data.date <- read.table(paste0(filedir,"/household_power_consumption.txt"), 
                          header = TRUE, sep = ";", colClasses = c(NA, rep("NULL", 8)))
  print("Succeeded...")
  data.date <- as.Date(data.date[,1], "%d/%m/%Y")
  
  ##we can modify to get params startDate, endDate from function call
  startDate  <- as.Date("2007-02-01")
  endDate <- as.Date("2007-02-02")
  
  readRows <- length(data.date[data.date >= startDate & data.date <= endDate])
  readPos <- which(data.date %in% startDate)[1]
  
  ##loading dataset by startDate and endDate
  data <- read.table(paste0(filedir,"/household_power_consumption.txt"), 
                     nrows = 1, header = TRUE, sep = ";")
  cNames <- colnames(data)
  data <- read.table(paste0(filedir,"/household_power_consumption.txt"), 
                     skip = readPos,
                     nrows = readRows,
                     header = TRUE, sep = ";")
  
  colnames(data) <- cNames
  data[,"Date"] <- as.Date(data[,"Date"], "%d/%m/%Y")
  
  Time <- strptime(paste(data[,"Date"], data[,"Time"]), format = "%F %H:%M:%S")
  data$Time <- Time
  
  ##Plotting graphic and saving into PNG file
  png(file = "plot3.png")
  with(data, plot(Time, Sub_metering_1, type = "l",
                  ylab = "Energy sub metering",
                  xlab = ""))
  with(data, lines(Time, Sub_metering_2, type = "l", col = "red"))
  with(data2, lines(Time, Sub_metering_3, type = "l", col = "blue"))
  legend("topright", paste0("Sub_metering", 1:3), lty = c(1,1,1), col = c("black", "blue", "red"))
  
  dev.off()
  
  print("Plot3.png has been created in current working directory.")
  data
}
