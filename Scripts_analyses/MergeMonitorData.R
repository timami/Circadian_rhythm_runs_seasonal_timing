library(tidyr)
setwd("C:/Users/timami/Documents/HahnLab/Circadian_rhythm_runs_seasonal_timing/Data/raw/Trikinetics/") #change working directory to project folder

files = list.files() #lists all available files in this folder
#exclude "prelim" datasets
files = files[-grep("prelim",files)] #change to only 4 latest files

read.monitor = function(data.source, monitor.num)
{
  data = read.table(file=data.source)
  
  #data read in as 3 columns, need to combine
  data$V2 = paste(data$V2, data$V3, data$V4, sep=" ")
  data$V3 = NULL
  data$V4 = NULL
  colnames(data)[3:ncol(data)] = paste("V",seq(3,42), sep="")
  
  #exclude unused columns
  data = data[,-c(6:9)]
  
  #append monitor number column
  data = data.frame(cbind(monitor = monitor.num, data))
  
  colnames(data)[2:7] = c("index", "date", "time","status", "extra", "DAM2_light")
  colnames(data)[8:ncol(data)] = paste("channel", seq(1,32), sep="")
  
  #transform data from wide to long format
  #instead of each channel having its own column
  #now each channel for each index has its own rows
  #console: install.packages("tidyr")
  data = gather(data, key=channel, activity_count, c(8:ncol(data)))
  return(data)
}

all.monitors = read.monitor(data.source = files[1], monitor.num = substr(files[1], nchar(files[1])-4, nchar(files[1])-4))

for (i in files[-1])
{
  mon.num <-  substr(i, nchar(i)-4, nchar(i)-4)
  all.monitors = rbind(all.monitors, read.monitor(data.source = i, monitor.num = substr(i, nchar(i)-4, nchar(i)-4)))
}
write.csv(all.monitors, "monitors_combined.csv", row.names=FALSE)
