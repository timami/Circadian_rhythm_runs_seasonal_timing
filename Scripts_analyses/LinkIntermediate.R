setwd("~/HahnLab/Circadian_rhythm_runs_seasonal_timing/Data/")
library(magrittr)

#read in combined dataset with all monitors
#script: mergeMonitorsData.r
all.monitors <- read.csv("Monitors_Combined/monitors_combined.csv", header=TRUE, stringsAsFactors = FALSE)

#parse out "channel" from channel col values
all.monitors$channel <- gsub("channel", "", all.monitors$channel)

#read in the intermediate/linking file
#2017-11-17_subset_host_comparison.csv
int.file <- read.csv("Intermediate/2017-11-17_subset_host_comparison.csv", header = TRUE, stringsAsFactors = FALSE)
int.file <- int.file[,c(23:39)] #limit to columns of interest

#clean dates in both datasets
library(lubridate)

#--------------------all.monitors----------------------------
#replace abbreviated string months with numeric EIther fix way its done or add a method to change month.abb to num
all.monitors$date <- gsub(" Oct ", "-10-", all.monitors$date)
all.monitors$date <- gsub(" Sep ", "-09-", all.monitors$date)

#convert date field from char --> date
all.monitors$date <- dmy(all.monitors$date)

#--------------------int.file--------------------------------
int.file$eclosion_date <- ymd(int.file$eclosion_date)
int.file$Trikinetic_exit_date <- ymd(int.file$Trikinetic_exit_date)
int.file$Free_run_entry_date <- ymd(int.file$Free_run_entry_date)
int.file$Free_run_exit_date <- ymd(int.file$Free_run_exit_date)

int.file$Trikinetics_entry_LD_time <- paste(int.file$eclosion_date, int.file$Trikinetics_entry_LD_time, sep = " ")
int.file$Trikinetics_entry_LD_time <- ymd_hm(int.file$Trikinetics_entry_LD_time, tz = "US/Eastern")
int.file$Trikinetics_exit_LD_time <- paste(int.file$Trikinetic_exit_date, int.file$Trikinetics_exit_LD_time, sep=" ")
int.file$Trikinetics_exit_LD_time <- ymd_hm(int.file$Trikinetics_exit_LD_time, tz = "US/Eastern")
int.file$Free_run_entry_time <- paste(int.file$Free_run_entry_date, int.file$Free_run_entry_time, sep = " ")
int.file$Free_run_entry_time <- ymd_hm(int.file$Free_run_entry_time, tz = "US/Eastern")
int.file$Free_run_exit_time <- paste(int.file$Free_run_exit_date, int.file$Free_run_exit_time, sep = " ")
int.file$Free_run_exit_time <- ymd_hm(int.file$Free_run_exit_time, tz = "US/Eastern")

today <- Sys.Date() #for flies with no exit date, assume exit date is today
now <- Sys.time() #for flies with no exit time, assume exit date/time is right now

fly.id <- vector()
day <- vector()
day.time <- vector()
experiment <- vector()
counts <- vector() #
cont.timing <- vector()
monitor <- vector()
channel <- vector()

#create function
activity.data <- all.monitors
fly.info <- int.file
my.act <- activity.data[1,]

if (my.act[,"monitor"] == 1 || my.act[,"monitor"] == 2)
{
  
  fly.info <- fly.info %>% subset(Trik_monitor == my.fly[,"monitor"]) %>% subset(Trikinetics_position == my.fly[,"channel"])
  fly.into <- fly.ingo %>% subset()
}

if (my.act[,"monitor"] == 3 || my.act[,"monitor"] == 4)
{
  
  fly.info <- fly.info %>% subset(Free_run_trik_monitor == my.fly[,"monitor"]) %>% subset(Free_run_trik_position == my.fly[,"channel"])
  fly.into <- fly.ingo %>% subset()
}

assign.fly <- function(activity)
{
  
  fly.info <- fly.info %>% subset(monitor == activity$monitor) %>% subset(channel == activity$channel) #subset the potential flies to only those in the same monitor and channel as the activity parameter
  
  #create an object for the fly from this subset that was in
  #during the activity
  assigned.fly <- 0
  for (i in 1:nrow(fly.info))
  {
    current.fly.interval <- interval(time_in, time_out)
    if (activity$time %within% current.fly.interval)
    {
      assigned.fly <- fly.info[i,"uniqueid"]
    }
  }
  return(assigned.fly)
}

