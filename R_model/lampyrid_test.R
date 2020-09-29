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
lampyrid_es<-subset(lampyrid_summary, TREAT_DESC=="Early successional")

#get rid of data that isn't needed for our analysis
lampyrid_es$TREAT_DESC<-NULL

#ok, these data frames should be ready to go

#here goes nothing

output_frame<-multiple_breakups(lampyrid_es)
output_frame
pyramid_plot(lampyrid_es, rsq_points=TRUE)
stability_time(lampyrid_es, min_percent = 95, error_multiplyer = 1)
relative_range(lampyrid_es)
relative_range_after_stability(lampyrid_es)
proportion_significant(lampyrid_es)
proportion_wrong(lampyrid_es)
proportion_wrong_before_stability(lampyrid_es)

wrongness_plot(lampyrid_es)
broken_stick_plot(lampyrid_es, window_length = 3)
broken_stick_plot(lampyrid_es, window_length = 4)
broken_stick_plot(lampyrid_es, window_length = 5)
broken_stick_plot(lampyrid_es, window_length = 6)
broken_stick_plot(lampyrid_es, window_length = 7)
broken_stick_plot(lampyrid_es, window_length = 8)
broken_stick_plot(lampyrid_es, window_length = 9)
broken_stick_plot(lampyrid_es, window_length = 10)
broken_stick_plot(lampyrid_es, window_length = 11)


#making figures for the paper

library(gridExtra)

#figure1 is made in excel. Don't judge

#figure 2 is adapted from Hermann et al 2016

#get averages by treatment

lampyrid_summary1<-dcast(lampyrid_summary, year+TREAT_DESC~., value.var="pertrap", mean)
names(lampyrid_summary1)[3]<-"pertrap"

#panel 1 with complete dataset
lampyrid.summary.treatment<-ggplot(lampyrid_summary1, aes(year, pertrap, 
                                                              fill=as.factor(TREAT_DESC)))+
  scale_fill_brewer(palette="Set3")+
  geom_smooth(aes(year, pertrap, fill=NULL), method="loess", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Treatment"))+
  theme(legend.key=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylim(0, 4)+
  xlab("\nYear")+
  ylab("Adults per trap\n")
lampyrid.summary.treatment

#panel with four year window
#pull out 2011 to 2014 data
lampyrid11_14<-lampyrid_summary1[which(as.numeric(lampyrid_summary1$year)>=2011& 
                                        as.numeric(lampyrid_summary1$year)<=2014),]

lampyrid.summary.treatment.subset<-ggplot(lampyrid11_14, aes(year, pertrap, 
                                                          fill=as.factor(TREAT_DESC)))+
  scale_fill_brewer(palette="Set3")+
  geom_smooth(aes(year, pertrap, fill=NULL), method="lm", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Treatment"))+
  theme(legend.key=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() )+
  ylim(0, 4)+
  xlab("\nYear")+
  ylab("Adults per trap\n")
lampyrid.summary.treatment.subset

#remove legend from panel A, add label
lampyrid.summary.treatment1<-lampyrid.summary.treatment+guides(fill=FALSE)+
  annotate("text", x=2004.6, y=3.9, label="A", size=12)
#remove Y axis title from panel B, add label
lampyrid.summary.treatment.subset1<-lampyrid.summary.treatment.subset+ylab(NULL)+
  annotate("text", x=2011.2, y=3.9, label="B", size=11)
#stack it together
grid.arrange(arrangeGrob(lampyrid.summary.treatment1, 
                         lampyrid.summary.treatment.subset1, 
                         ncol=2, widths=c(0.35, 0.55)))


#save to pdf
# pdf("figures/figure2.pdf", height=5, width=10)
# grid.arrange(arrangeGrob(lampyrid.summary.treatment1, 
#                          lampyrid.summary.treatment.subset1, 
#                          ncol=2, widths=c(0.355, 0.55)))
# dev.off()

#figure 3 is the pyramid plot

pyramid<-pyramid_plot(lampyrid_es, rsq_points=TRUE)+theme_bw(base_size = 18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# 
# pdf("figures/figure3.pdf", height=5, width=6)
# pyramid
# dev.off()



#figure 4 is the wrongness plot

wrongness<-wrongness_plot(lampyrid_es)+theme_bw(base_size = 18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# pdf("figures/figure4.pdf", height=5, width=7)
# wrongness
# dev.off()


#figure 5 is the multipanel broken stick plot

stick3<-broken_stick_plot(lampyrid_es, window_length = 3)+ylab(NULL)+xlab(NULL)+
  annotate("text", x=2005.5, y=3.9, label="3 year", size=5)
stick4<-broken_stick_plot(lampyrid_es, window_length = 4)+ylab(NULL)+xlab(NULL)+
  annotate("text", x=2005.5, y=3.9, label="4 year", size=5)
stick5<-broken_stick_plot(lampyrid_es, window_length = 5)+ylab(NULL)+xlab(NULL)+
  annotate("text", x=2005.5, y=3.9, label="5 year", size=5)
stick6<-broken_stick_plot(lampyrid_es, window_length = 6)+ylab(NULL)+xlab(NULL)+
  annotate("text", x=2005.5, y=3.9, label="6 year", size=5)
stick7<-broken_stick_plot(lampyrid_es, window_length = 7)+ylab(NULL)+xlab(NULL)+
  annotate("text", x=2005.5, y=3.9, label="7 year", size=5)
stick8<-broken_stick_plot(lampyrid_es, window_length = 8)+ylab(NULL)+xlab(NULL)+
  annotate("text", x=2005.5, y=3.9, label="8 year", size=5)
stick9<-broken_stick_plot(lampyrid_es, window_length = 9)+ylab(NULL)+xlab(NULL)+
  annotate("text", x=2005.5, y=3.9, label="9 year", size=5)
stick10<-broken_stick_plot(lampyrid_es, window_length = 10)+ylab(NULL)+xlab(NULL)+
  annotate("text", x=2005.5, y=3.9, label="10 year", size=5)
stick11<-broken_stick_plot(lampyrid_es, window_length = 11)+ylab(NULL)+xlab(NULL)+
  annotate("text", x=2005.5, y=3.9, label="11 year", size=5)


grid.arrange(arrangeGrob(stick3, stick4, stick5, stick6, stick7, stick8, stick9,
                         stick10, stick11, 
                         ncol=3, widths=c(0.5, 0.5, 0.5)),
             left="Z-scaled response", bottom="Year")


pdf("figures/figure3.pdf", height=7, width=8)
grid.arrange(arrangeGrob(stick3, stick4, stick5, stick6, stick7, stick8, stick9,
                         stick10, stick11, 
                         ncol=3, widths=c(0.5, 0.5, 0.5)),
             left="Z-scaled response", bottom="Year")

dev.off()



#Simplified figure 1

#get averagesper rep by treatment

lampyrid_summary1<-lampyrid2
lampyrid_summary1$TRAPS<-lampyrid3$TRAPS

#create a new variable to account for trapping effort in a given year
lampyrid_summary1$pertrap<-lampyrid_summary1$ADULTS/lampyrid_summary1$TRAPS

lampyrid_summary2<-dcast(lampyrid_summary1, year+TREAT_DESC+REPLICATE~., value.var="pertrap", mean)
names(lampyrid_summary2)[3]<-"pertrap"

lampyrid_summary3<-subset(lampyrid_summary1, TREAT_DESC=="Early successional")

#panel 1 with complete dataset
lampyrid.summary.treatment<-ggplot(lampyrid_summary3, aes(year, pertrap))+
  geom_smooth( method="loess", span=0.5, colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Treatment"))+
  theme(legend.key=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylim(0, 2)+
  xlab("\nYear")+
  ylab("Adults per trap\n")
lampyrid.summary.treatment

#panel with four year window
#pull out 2011 to 2014 data
lampyrid05_08<-lampyrid_summary3[which(as.numeric(lampyrid_summary3$year)>=2005& 
                                         as.numeric(lampyrid_summary3$year)<=2008),]

lampyrid.summary.treatment.subset<-ggplot(lampyrid05_08, aes(year, pertrap))+
  geom_smooth(aes(year, pertrap), method="lm", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Treatment"))+
  theme(legend.key=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() )+
  ylim(0, 2)+
  xlab("\nYear")+
  ylab("Adults per trap\n")
lampyrid.summary.treatment.subset

#remove legend from panel A, add label
lampyrid.summary.treatment1<-lampyrid.summary.treatment+guides(fill=FALSE)+
  annotate("text", x=2004.6, y=1.95, label="A", size=12)
#remove Y axis title from panel B, add label
lampyrid.summary.treatment.subset1<-lampyrid.summary.treatment.subset+ylab(NULL)+
  annotate("text", x=2005.2, y=1.95, label="B", size=12)
#stack it together
grid.arrange(arrangeGrob(lampyrid.summary.treatment1, 
                         lampyrid.summary.treatment.subset1, 
                         ncol=2, widths=c(0.55, 0.5)))

#get slope and stats for each panel

summary(lm(pertrap~year, data=lampyrid_summary3))
summary(lm(pertrap~year, data=lampyrid05_08))

#save to pdf
pdf("figures/figure1.pdf", height=5, width=10)
grid.arrange(arrangeGrob(lampyrid.summary.treatment1, 
                         lampyrid.summary.treatment.subset1, 
                         ncol=2, widths=c(0.55, 0.55)))
dev.off()

#combine figs 3 and 4 for submission

#figure 2A

pyramid.1<-pyramid+annotate("text", x=11.8, y=-1.3, label="A", size=11)



#figure 2B

wrongness.1<-wrongness+annotate("text", x=3.3, y=0.97, label="B", size=11)

pdf("figures/figure2.pdf", height=5, width=13)
grid.arrange(arrangeGrob(pyramid.1, 
                         wrongness.1, 
                         ncol=2, widths=c(0.5, 0.5)))
dev.off()
