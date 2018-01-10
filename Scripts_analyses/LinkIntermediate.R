setwd("~/HahnLab/Circadian_rhythm_runs_seasonal_timing/")
library(magrittr)

#read in combined dataset with all monitors
#script: MergeMonitorData.r
source("Scripts_analyses/MergeMonitorData.R")
#get all.monitors data frame from this ^^ 

#parse out "channel" from channel col values
all.monitors$channel <- gsub("channel", "", all.monitors$channel)

#read in the intermediate/linking file
#2017-11-17_subset_host_comparison.csv
int.file <- read.csv("~/HahnLab/Circadian_rhythm_runs_seasonal_timing/Data/Intermediate/2017-11-17_subset_host_comparison.csv", header = TRUE, stringsAsFactors = FALSE)
int.file <- int.file[,c(23:39)] #limit to columns of interest

#clean dates in both datasets
library(lubridate)

#--------------------all.monitors----------------------------
#replace abbreviated string months with numeric EIther fix way its done or add a method to change month.abb to num
all.monitors$date <- gsub(" Oct ", "-10-", all.monitors$date)
all.monitors$date <- gsub(" Sep ", "-09-", all.monitors$date)

#convert date field from char --> date
all.monitors$date <- dmy(all.monitors$date)
all.monitors$time <- ymd_hms(paste(all.monitors$date, all.monitors$time, sep = " "), tz = "US/Eastern")

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

#create function
activity.data <- all.monitors
fly.info <- int.file
my.act <- activity.data[1,]

assign.fly <- function(activity)
{
  
  #create an object for the fly from this subset that was in
  #during the activity
  assigned.fly <- 0
  
  if (my.act[,"monitor"] == 1 || my.act[,"monitor"] == 2)
  {
    #subset fly.info to that for the appropriate monitor (for the param activity)
    #and channel = position 
    fly.info <- fly.info %>% subset(Trik_monitor == my.fly[,"monitor"]) %>% subset(Trikinetics_position == my.fly[,"channel"])
  }
  
  if (my.act[,"monitor"] == 3 || my.act[,"monitor"] == 4)
  {
    #subset fly.info to that for the appropriate monitor (for the param activity)
    #and channel = position 
    fly.info <- fly.info %>% subset(Free_run_trik_monitor == my.fly[,"monitor"]) %>% subset(Free_run_trik_position == my.fly[,"channel"])
  }
  for (i in 1:nrow(fly.info))
  {
    current.fly.interval <- interval(time_in, time_out)
    
    if (activity$time %within% current.fly.interval)
    {
      assigned.fly <- fly.info[i,"uniqueID"]
    }
  }
  return(assigned.fly)
}

matched.flies <- vector()

for (i in 1:nrow(activity.data))
{
  matched.flies <- append(matched.flies, assign.fly(activity.data[i,]))
}

