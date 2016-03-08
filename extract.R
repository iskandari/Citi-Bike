#How to extract truck deliveries of bikes in citibike monthly datasets by isolating bikeids
#that have a different start.station.id than previous end.station.id 

#First you need to download the data from the website <https://www.citibikenyc.com/system-data>

#IMPORTANT: Please keep in mind that there are two separate codes for extracting truck deliveries.

# The first is for months in which the time format = "%m/%d/%Y %H:%M" , for example June and March 2015
# The second is for months in which the time format = "%m/%d/%Y %H:%M:%S", for example August-Junember 2015
# Please check the time formats first before running the code because citibike data does not seem to follow 
# a consistent pattern with time formats

#IMPORTANT: Time difference is displayed in seconds 
#IMPORTANT: Running the code can take up to 20 minutes because for loops take a long time 
#WARNING Please make sure the system time is EST! 


Sys.setenv(TZ='EST')

raw_data = Nov2014

unique_id = unique(raw_data$bikeid)
output1 <- data.frame("bikeid"= integer(0), "end.station.id"= integer(0), "start.station.id" = integer(0), "diff.time" = numeric(0),  "stoptime" = character(),"starttime" = character(), stringsAsFactors=FALSE)

for (bikeid in unique_id)
{
  onebike <- raw_data[ which(raw_data$bikeid== bikeid), ]
  onebike$starttime <- strptime(onebike$starttime, "%m/%d/%Y %H:%M:%S", tz = "EST")
  onebike <- onebike[order(onebike$starttime, decreasing = FALSE),]
  onebike$starttime <- as.factor(as.character(onebike$starttime))
  onebike$stoptime <- as.factor(as.character(onebike$stoptime))
  
  if(nrow(onebike) >=2 ){
    for(i in 2:nrow(onebike )) {
      if(is.integer(onebike[i-1,"end.station.id"]) & is.integer(onebike[i,"start.station.id"]) &
         onebike[i-1,"end.station.id"] != onebike[i,"start.station.id"]){
        diff_time <- as.double(difftime(strptime(onebike[i,"starttime"], "%Y-%m-%d %H:%M:%S", tz = "EST"),
                                        strptime(onebike[i-1,"stoptime"], "%m/%d/%Y %H:%M:%S", tz = "EST")
                                        ,units = "secs"))
        new_row <- c(bikeid, onebike[i-1,"end.station.id"], onebike[i,"start.station.id"], diff_time, as.character(onebike[i-1,"stoptime"]), as.character(onebike[i,"starttime"]))
        output1[nrow(output1) + 1,] = new_row
      }
    }
  }
}

output1_stoptime <- as.POSIXct(output1$stoptime, "%m/%d/%Y %H:%M:%S", tz = "EST")
output1[5] <- output1_stoptime
output1_starttime <- as.POSIXct(output1$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")
output1[6] <- output1_starttime

output1$diff.time <- as.numeric(output1$diff.time)





# Second code 

unique_id = unique(raw_data$bikeid)

output2 <- data.frame("bikeid"= integer(0), "end.station.id"= integer(0), "start.station.id" = integer(0), "diff.time" = numeric(0),  "stoptime" = character(),"starttime" = character(), stringsAsFactors=FALSE)

for (bikeid in unique_id)
{
  onebike <- raw_data[ which(raw_data$bikeid== bikeid), ]
  
  if(nrow(onebike) >=2 ){
    for(i in 2:nrow(onebike )) {
      if(is.integer(onebike[i-1,"end.station.id"]) & is.integer(onebike[i,"start.station.id"]) &
         onebike[i-1,"end.station.id"] != onebike[i,"start.station.id"]){
        diff_time <- as.double(difftime(strptime(onebike[i,"starttime"], "%Y-%m-%d %H:%M:%S", tz = "EST"),
                                        strptime(onebike[i-1,"stoptime"], "%Y-%m-%d %H:%M:%S", tz = "EST")
                                        ,units = "secs"))
        new_row <- c(bikeid, onebike[i-1,"end.station.id"], onebike[i,"start.station.id"], diff_time, as.character(onebike[i-1,"stoptime"]), as.character(onebike[i,"starttime"]))
        output2[nrow(output2) + 1,] = new_row
      }
    }
  }
}

#Convert times into POSIXct for "%m/%d/%Y %H:%M" format 

output1_stoptime <- as.POSIXct(output1$stoptime, "%m/%d/%Y %H:%M:%S", tz = "EST")
output1[5] <- output1_stoptime
output1_starttime <- as.POSIXct(output1$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")
output1[6] <- output1_starttime

output1$diff.time <- as.numeric(output1$diff.time)




# Convert times into POSIXct for "%m/%d/%Y %H:%M" format 

output2_stoptime <- as.POSIXct(output2$stoptime, "%Y-%m-%d %H:%M:%S", tz = "EST")
output2[5] <- output2_stoptime
output2_starttime <- as.POSIXct(output2$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")
output2[6] <- output2_starttime

Sys.setenv(TZ='EST')

##Extract truck movements for the entire month that occurred within a specified time window. 
#Default is 3600 seconds = 1 hour. This way you can pinpint the timeframe in which a movement was made

# Make sure your system timezone is on EST as that is the time zone of New York 
Sys.setenv(TZ='EST')

output1$diff.time <- as.numeric(output1$diff.time)
output2$diff.time <- as.numeric(output2$diff.time)

one_hour_all_Jun <- output1[output1$diff.time < 3600 & output1$diff.time > 0,]
one_hour_all_Dec<- output2[output2$diff.time < 3600 & output2$diff.time > 0,]

one_hour_all_Dec$midtime <- as.POSIXct((as.numeric(one_hour_all_Dec$stoptime) + as.numeric(one_hour_all_Dec$starttime)) / 2, origin = '1970-01-01')

by_day <- as.data.frame(as.Date(one_hour_all_Dec$midtime, tz = "EST"))
colnames(by_day)[1] <- "SUM"
days_Dec2013 <-as.data.frame(table(by_day$SUM))
colnames(days_Dec2013)[1] <- "DATE"

days_Dec2013 

##Extract truck movements on a specific day that occurred within a specified time window. 

#After importing from folder 
ALLRB$stoptime <- strptime(ALLRB$stoptime, "%Y-%m-%d %H:%M:%S", tz = "EST")
ALLRB$starttime <- strptime(ALLRB$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")
ALLRB$midtime <- as.POSIXct((as.numeric(ALLRB$stoptime) + as.numeric(ALLRB$starttime)) / 2, origin = '1970-01-01')ye

ALLRB$year <- year(ALLRB$midtime)


ALLRB_2014 <- ALLRB[ALLRB$year == 2014, ]


lapply(All_One_Hour_2014, class)

ALLRB$stoptime <- strptime(ALLRB$stoptime, "%Y-%m-%d %H:%M:%S", tz = "EST")
ALLRB$starttime <- strptime(ALLRB$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")
ALLRB$midtime <- as.POSIXct((as.numeric(ALLRB$stoptime) + as.numeric(ALLRB$starttime)) / 2, origin = '1970-01-01')


All_2014$midtime <- strptime(All_2014$midtime, "%Y-%m-%d %H:%M:%S", tz = "EST")

one.day_1 <- output1[output1$stoptime >="2015-11-02 00:00:00" & output1$starttime <= "2015-11-03 00:00:00" & output1$diff.time < 3600 & output1$diff.time > 0, ]

one.day_2 <- output2[output2$stoptime >= "2013-11-01 00:00:00" & output2$starttime <= "2013-11-02 00:00:00" & output2$diff.time < 3600 & output2$diff.time > 0,]

one.day_3 <- output2[output2$stoptime >= "2013-10-02 00:00:00" & output2$starttime <= "2013-10-03 00:00:00" & output2$diff.time < 3600 & output2$diff.time > 0,]


one.day_1
one.day_2 
one.day_3

# Now you should have the all of the movements made in January(output1) and all of the movements made in 
#August(output2), as well as two tables - the first (movements_Jan) indicating the number of movements per day
#in January and the second (movements_Aug) indicating the number of movements per day in August.
#In addition, you have the all truck movements for the entire month that occurred within a one-hourtime window (one_hour_all_Jan,  one_hour_all_Aug)

#You should also have two tables of pinpointed truck movements that occurred within a one-hour timeframe for the
#the 25th of January (one.day_1) and on the 15th of August (one_.day_2)

write.csv(output2, "all_Dec2013.csv")
write.csv(one_hour_all_Dec, "one_hour_Dec2013.csv")
write.csv(days_Dec2013, "days_Dec2013.csv")
write.csv(one.day_2, "Dec04_2013.csv")

# Extract rows from data set 

myData <- myData[-c(2, 4, 6), ]

# Bind movements together

one_hour_ALL <- rbind(one_hour_all_Jan, one_hour_all_Feb, one_hour_all_Mar, one_hour_all_Apr, one_hour_all_May, one_hour_all_Jun, one_hour_all_Jul, one_hour_all_Aug, one_hour_all_Sep, one_hour_all_Oct, one_hour_all_Jun)

bikeid.ll<- bikeid.ll[c(2,1,3,4,5,6,7,8,9,10,11,12,13)]


#get midpoints 

one_hour_ALL$midtime <- as.POSIXct((as.numeric(one_hour_ALL$stoptime) + as.numeric(one_hour_ALL$starttime)) / 2, origin = '1970-01-01')

#get movements per day for the whole year 

by_day_output1 <- as.data.frame(as.Date(one_hour_ALL$midtime, tz = "EST"))
colnames(by_day_output1)[1] <- "SUM"
movements_ALL <-as.data.frame(table(by_day_output1$SUM))
colnames(movements_ALL)[1] <- "DATE"

one_hour_ALL$midtime <- as.POSIXct((as.numeric(one_hour_ALL$stoptime) + as.numeric(one_hour_ALL$starttime)) / 2, origin = '1970-01-01')
one_hour_ALL$midtime <- as.POSIXct((as.numeric(one_hour_ALL$stoptime) + as.numeric(one_hour_ALL$starttime)) / 2, origin = '1970-01-01')
one_hour_ALL$midtime <- as.POSIXct((as.numeric(one_hour_ALL$stoptime) + as.numeric(one_hour_ALL$starttime)) / 2, origin = '1970-01-01')

All_One_Hour <- read.csv("All_One_Hour_2014.csv")

Oct <- Oct[Oct$diff.time > 0 & Oct$diff.time < 7200,]
Oct <- Oct[Oct$start.station.id == 465,]

all_Oct2014

all_Oct2014 <- all_Oct2014[all_Oct2014$diff.time > 0 & all_Oct2014$diff.time < 86400,]
all_Oct2014 <- all_Oct2014[all_Oct2014$start.station.id == 465,]

Json_Octobers$created_at <- strptime(Json_Octobers$created_at, "%Y-%m-%d %H:%M:%S", tz = "EST")


Oct2013$starttime <- strptime(Oct2013$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")

Oct2015$starttime <- strptime(Oct2015$starttime, "%m/%d/%Y %H:%M:%S", tz = "EST")





all_Oct2014$midtime <- as.POSIXct((as.numeric(all_Oct2014$stoptime) + as.numeric(all_Oct2014$starttime)) / 2, origin = '1970-01-01')

json_AUG19 < strptime(json$created_at, "%Y-%m-%d %H:%M:%S", tz = "EST")

rbOct2013$stoptime <- strptime(rbOct2013$stoptime, "%Y-%m-%d %H:%M:%S", tz = "EST")
rbOct2013$starttime <- strptime(rbOct2013$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")
rbOct2013$midtime <- as.POSIXct((as.numeric(rbOct2013$stoptime) + as.numeric(rbOct2013$starttime)) / 2, origin = '1970-01-01')

rb$stoptime <- strptime(rb$stoptime, "%Y-%m-%d %H:%M:%S", tz = "EST")
rb$starttime <- strptime(rb$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")
rb$midtime <- as.POSIXct((as.numeric(rb$stoptime) + as.numeric(rb$starttime)) / 2, origin = '1970-01-01')

Oct2015$stoptime <- strptime(Oct2015$stoptime, "%Y-%m-%d %H:%M:%S", tz = "EST")

Oct2015$starttime <- strptime(Oct2015$starttime, "%m/%d/%Y %H:%M:%S", tz = "EST")

Oct2015$midtime <- as.POSIXct((as.numeric(Oct2015$stoptime) + as.numeric(Oct2015$starttime)) / 2, origin = '1970-01-01')

rbOct2013 <- rbOct2013[rbOct2013$diff.time > 0 & rbOct2013$diff.time < 43200,]
rbOct2014 <- rbOct2014[rbOct2014$diff.time > 0 & rbOct2014$diff.time < 43200,]
rbOct2015 <- rbOct2015[rbOct2015$diff.time > 0 & rbOct2015$diff.time < 43200,]

rbOct2013$hour <- hour(rbOct2013$midtime) 

rbOct2014$hour <- hour(rbOct2014$midtime) 
rbOct2015$hour <- hour(rbOct2015$midtime) 
<- strptime(Oct_2014$starttime, "%m/%d/%Y %H:%M:%S", tz = "EST")


a2015$starttime <- strptime(a2015$starttime, "%m/%d/%Y %H:%M:%S", tz = "EST")a




one_day$stoptime <- strptime(one_day$stoptime, "%Y-%m-%d %H:%M:%S", tz = "EST")
one_day$starttime <- strptime(one_day$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")

one_day$midtime <- as.POSIXct((as.numeric(one_day$stoptime) + as.numeric(one_day$starttime)) /2 , origin = '1970-01-01')
one_day$first <- one_day$stoptime + 0.1*difftime(one_day$starttime, one_day$stoptime, units = "secs")
one_day$second <-  one_day$stoptime + 0.2*difftime(one_day$starttime, one_day$stoptime, units = "secs")
one_day$thrid <- one_day$stoptime + 0.3*difftime(one_day$starttime, one_day$stoptime, units = "secs")
one_day$fourth <- one_day$stoptime + 0.4*difftime(one_day$starttime, one_day$stoptime, units = "secs")
one_day$fifth <- one_day$stoptime + 0.5*difftime(one_day$starttime, one_day$stoptime, units = "secs")
one_day$sixth <- one_day$stoptime + 0.6*difftime(one_day$starttime, one_day$stoptime, units = "secs")
one_day$seventh <- one_day$stoptime + 0.7*difftime(one_day$starttime, one_day$stoptime, units = "secs")
one_day$eighth <- one_day$stoptime + 0.8*difftime(one_day$starttime, one_day$stoptime, units = "secs")
one_day$ninth <- one_day$stoptime + 0.9*difftime(one_day$starttime, one_day$stoptime, units = "secs")

heydt$created_at <- strptime(heydt$created_at, "%Y-%m-%d %H:%M:%S", tz = "EST")

All$starttime <- strptime(All$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")
All$midtime <- as.POSIXct((as.numeric(All$stoptime) + as.numeric(All$starttime)) / 2, origin = '1970-01-01')

$starttime<- as.POSIXct(a2013$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")
a2013$stoptime<- as.POSIXct(a2013$stoptime, "%Y-%m-%d %H:%M:%S", tz = "EST")

a$starttime <- Aug[Aug$starttime > "2014-08-19 00:00:00" & Aug$starttime < "2014-08-20 00:00:00", ]
a <- Aug[Aug$stoptime > "2014-08-19 00:00:00" & Aug$stoptime < "2014-08-20 00:00:00", ]





twodays <- Aug[Aug$starttime > "2014-08-18 06:00:00" & Aug$stoptime < "2014-08-19 10:00:00",]


ALLRB$year <- year(ALLRB$midtime)
ALLRB_2013 <- ALLRB[ALL]


Oct2013$starttime <- as.POSIXct((as.numeric(data$stoptime) + as.numeric(data$starttime)) / 2, origin = '1970-01-01')






