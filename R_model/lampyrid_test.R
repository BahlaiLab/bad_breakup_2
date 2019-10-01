#########################################################################################

#now, let's give this a try with some real data
source("bad_breakup_script.R")

#lets' start with the lampyrid dataset because I'm familiar with it
#and know there is enough data to do it
 
#bring data in from figshare
lampyrid<-read.csv(file="https://ndownloader.figshare.com/files/3686040",
                   header=T)

#pull in our data cleaning code from https://github.com/cbahlai/lampyrid/blob/master/lampyrid_analysis.R
#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable
library(lubridate)
lampyrid$newdate<-mdy(lampyrid$DATE)
lampyrid$year<-year(lampyrid$newdate)
lampyrid$DOY<-yday(lampyrid$newdate)
lampyrid<-na.omit(lampyrid)
lampyrid$TREAT_DESC<-gsub("Early succesional community", "Early successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Early sucessional community", "Early successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Succesional", "Successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Sucessional", "Successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Conventional till", "Conventional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-as.factor(lampyrid$TREAT_DESC)
lampyrid$HABITAT<-as.factor(lampyrid$HABITAT)
lampyrid$REPLICATE<-as.factor(lampyrid$REPLICATE)
lampyrid$STATION<-as.factor(lampyrid$STATION)

# data is currently structured as subsamples, replicates, across treatments. We know from 
# http://biorxiv.org/content/early/2016/09/11/074633 that the organisms were most abundant in
# Alfalfa and no-till treatments, so we should use data from these treatments if we're 
# interested in picking up trends over time. We also don't really care about within
# year dynamics for this experiment- we essentially want a summary measure of what was going on
#within a rep, within a year. So let's reshape the data, drop out irrelevant treatments.

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
lampyrid1<-melt(lampyrid, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
lampyrid2<-dcast(lampyrid1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
lampyrid3<-dcast(lampyrid1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(lampyrid2)[4]<-"ADULTS"
names(lampyrid3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from lampyrid3
lampyrid_summary<-lampyrid2
lampyrid_summary$TRAPS<-lampyrid3$TRAPS

#create a new variable to account for trapping effort in a given year
lampyrid_summary$pertrap<-lampyrid_summary$ADULTS/lampyrid_summary$TRAPS

#get rid of columns we don't need for analysis
lampyrid_summary$REPLICATE<-NULL
lampyrid_summary$ADULTS<-NULL
lampyrid_summary$TRAPS<-NULL

# pull out relevant treatments, create a data frame for each
lampyrid_alfalfa<-subset(lampyrid_summary, TREAT_DESC=="Alfalfa")
lampyrid_notill<-subset(lampyrid_summary, TREAT_DESC=="No till")

#get rid of data that isn't needed for our analysis
lampyrid_alfalfa$TREAT_DESC<-NULL
lampyrid_notill$TREAT_DESC<-NULL

#ok, these data frames should be ready to go

#here goes nothing
multiple_breakups(lampyrid_alfalfa)
# there are some perculiarities because 2007 is missing, but I think we're working now.
# try it with other data too
multiple_breakups(lampyrid_notill)
pyramid_plot(lampyrid_notill, rsq_points=TRUE)
stability_time(lampyrid_notill)
relative_range(lampyrid_notill)
proportion_wrong(lampyrid_notill)

wrongness<-proportion_wrong_series(lampyrid_notill)
plot(wrongness$window_length,wrongness$proportion_wrong, xlab="Window length", 
     ylab="Proportion wrong", pch=19)
lines(lowess(wrongness$window_length,wrongness$proportion_wrong), col="blue")
points(wrongness$window_length,wrongness$avg_r_square, pch=17, col="orange")
lines(lowess(wrongness$window_length,wrongness$avg_r_square), col="red")

