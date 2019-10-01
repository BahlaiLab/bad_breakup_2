# How does when you look, and how long, affect the conclusions you reach about your data?
# Are short term studies more likely to yield significant results?
# Are short term studies more likely to find *erroneous* significant trends?
# This script will perform a simple moving window analysis to answer these questions
# for long term data across a variety of domains- essentially, data gets subsetted, 
# we run a simple linear regression on each subset and record summary stats, trends

#assume data is coming in in the form year, response variable
#use this test data set to build stuff
test<-read.csv(file="https://raw.githubusercontent.com/BahlaiLab/bad_breakup_2/master/R_model/test.csv", header=TRUE)

# set it so we get our decimal places rather than sci notation in our outputs
options(scipen=10)

# technical things that need to be done:

# data needs to be normalized in some way because we will be operating across domains 
# and likely with dramatically different values. Let's use a standardization approach so
# we don't give undue influence to outliers but also gets responses in the same magnitude
# something like x(standard)=(x-mean)/Stdev, the z score

standardize<-function(data){
  #operate on the second column in data, where our response variable is
  data$stand.response<-(data[,2]-mean(data[,2]))/sd(data[,2])
  #name the columns consistently so we don't get headaches later
  names(data)<-c("year", "response", "stand.response")
  #spit back our new data frame
  return(data)
}

#try it on test data
test1<-standardize(test)
#seems to be functioning

# next we need a function that runs a simple linear model of x=year, y=response variable

linefit<-function (data){
  #fit the model
  model<-lm(stand.response~year, data=data)
  #create a vector of relevant outputs. We want slope, error, P value
  output<-c(min(data$year), #year the analysis started on
            nrow(data), #number of data points the analysis includes
            length(unique(data$year)), #number of years the analysis includes
            summary(model)$coefficients[2,1], # slope
            summary(model)$coefficients[2,2], # se for slope
            summary(model)$coefficients[2,4], #p value
            summary(model)$r.squared, #r-squared
            summary(model)$adj.r.squared) #adjusted r-squared
  return(output)
}

#and try this on test data
linefit(test1)
# functional!

#now we need to think about how to iterate through the dataset. We want a
#function that starts at the first year, counts the number of rows specified
#and then feeds that rsultant data frame to the fittling function. 
#then we want to discard the first row of the data set, and repeat until fewer than
#the number of rows specified remains

breakup<-function(data, window){ #window is the size of the window we want to use
  remaining<-data #create dummy data set to operate on
  output<-data.frame(year=integer(0), #create empty data frame to put our output variables in
                     length=integer(0), 
                     years=integer(0),
                     slope=numeric(0), 
                     slope_se=numeric(0), 
                     p_value=numeric(0),
                     r_square=numeric(0),
                     adj_r_square=numeric(0))
  numyears<-length(unique(data$year))
  while (numyears>(window-1)){ #while there's still more years of data than in the window
    chunk<-subset(remaining, year<(min(year)+window)) #pull out a chunk as big as the window from the top of the data
    out<-linefit(chunk) #fit a linear model and get relevant statistics on chunk
    #add a conditional so that if there's missing data, it's not included in output
    if (window==length(unique(chunk$year))){
      output<-rbind(output, out) #append the stats to the output data frame
    }else{
      output<-output #leave it out if it has missing data
    }
    
    remaining<-subset(remaining, year>min(year)) #cut out the first year of the remaining data + repeat
    numyears<-length(unique(remaining$year))
  }
  names(output)<-c("start_year", "N_data", "N_years", "slope", "slope_se", "p_value",
                   "r_square", "adj_r_square")
  return(output)#output the data frame
}

#and now try this on the test data
breakup(test1, 3)

# now time to write the function that will iterate through our targetted windows
# let's make a decision rule that our test data set must be greater than 10y in length
# let's make this idiot-proof and build the standardize function right into this function
# so we can literally run each properly prepared raw data set with a single line

multiple_breakups<-function(data){
  data1<-standardize(data) #standardize data
  count<-length(data1$year)
  output<-data.frame(year=integer(0), #create empty data frame to put our output variables in
                     length=integer(0), 
                     years=integer(0),
                     slope=numeric(0), 
                     slope_se=numeric(0), 
                     p_value=numeric(0),
                     r_square=numeric(0),
                     adj_r_square=numeric(0))
  for(i in 3:(count-1)){
    outeach<-breakup(data1, i) #fit at each window length
    output<-rbind(output, outeach)#bind it to the frame
  }
  
  outall<-linefit(data1) #fit a line to the complete data set too
  out<-rbind(output, outall)
  return(out)
}

test2<-multiple_breakups(test)
#fan-flipping-tastic! it looks like that works


#let's create a plotting function
library(ggplot2)

pyramid_plot<- function(data, title="", significance=0.05, plot_insig=TRUE, rsq_points=FALSE){
  out<-multiple_breakups(data)
  years<-length(unique(out$start_year))
  maxyears<-max(out$N_years)
  count<-nrow(out)
  #compute mean and sd of longest series for vertical lines
  true_slope<-out[count,4] #find the slope of the longest series
  #remember to convert standard error to standard deviation
  true_error<-(out[count,5])*(sqrt(out[count, 2]))#find the error of the longest series
  max_true<-true_slope+true_error #compute max and min values for slopes we are calling true
  min_true<-true_slope-true_error
  out$significance<-ifelse(out$p_value<significance, "YES", "NO")
  if(rsq_points==TRUE){
    point_scale<-10*out$r_square
    yespt<-1
  }else{
    point_scale<-2
    yespt<-16
  }
  if(plot_insig==FALSE){
    out<-out[which(out$p_value<significance),]
  }
  plot<- ggplot(out) +
    theme_classic() +
    geom_hline(yintercept = true_slope, linetype = 2) +
    geom_hline(yintercept = max_true, linetype = 3, color="grey") +
    geom_hline(yintercept = min_true, linetype = 3, color="grey") +
    aes(y = slope, x = N_years,  ymin = (slope-slope_se), 
        ymax = (slope+slope_se), shape=significance, color=significance) +
    geom_linerange(show.legend = F)+ 
    geom_point(size=point_scale)+ ggtitle(title)+
    scale_shape_manual(values=c("NO"=4,"YES"=yespt))+
    scale_color_manual(values=c("NO"="red","YES"="black"))+
    xlab("Number of years in window")+
    scale_x_continuous(lim=c(3, maxyears))+
    coord_flip()
  return(plot)
}

pyramid_plot(test, title="test plot", plot_insig = TRUE, significance=0.05, rsq_points =TRUE)



#now that we have visualization, we need a way to pull relevant metrics out of the computation
#so let's say our longest series is our 'truth', and we want to know how many years it takes 
#to reach 'stability'-so let's define stability as >(some percentage of slopes) occuring within 
#the standard deviation of the slope of the longest series, for a given window length, allow user to change # of SEs

stability_time<-function(data, min_percent=95, error_multiplyer=1){#returns a number 
  test<-multiple_breakups(data)
  count<-nrow(test)
  true_slope<-test[count,4] #find the slope of the longest series
  #remember to convert standard error to standard deviation
  true_error<-(test[count,5])*(sqrt(test[count, 2]))*error_multiplyer #find the error of the longest series
  max_true<-true_slope+true_error #compute max and min values for slopes we are calling true
  min_true<-true_slope-true_error
  windows<-unique(test$N_years)#get a list of unique window lengths
  stability<-max(windows) #start with the assumption that the longest window is the only stable one
  for(i in 1:length(windows)){#for each window length, compute proportion 'correct'
    window_length<-windows[i]
    test_subset<-test[which(test$N_years==window_length),]
    number_of_windows<-nrow(test_subset)#how many windows
    correct_subset<-test_subset[which((test_subset$slope<max_true) & (test_subset$slope>min_true)),]
    number_of_correct<-nrow(correct_subset)#how many windows give the right answer
    percentage_correct<-100*number_of_correct/number_of_windows
    if(percentage_correct > min_percent){
      if(window_length < stability){
        stability<-window_length
      }
    }
  }
  return(stability)
}

#and a test
stability_time(test, error_multiplyer = 1.5)

#now a function that finds the absoloute range of findings, and the absolute 
#range of significant findings

abs_range<- function(data, only_significant=FALSE, significance=0.05){#returns a two unit vector with the max and min slopes
  test<-multiple_breakups(data)
  if(only_significant== TRUE){ #if user specifies only significant values wanted, pull those
    test1<-test[which(test$p_value<significance),]
  }else{
    test1<-test
  }
  max_slope<-max(test1$slope)
  min_slope<-min(test1$slope)
  sloperange<-c(min_slope, max_slope)
  return(sloperange)
  
}

#and try it out
abs_range(test, only_significant = FALSE, significance = 0.05)

#now we want to find the absolute over and under estimate compared to the slope of the 
#longest series

relative_range<- function(data, only_significant=FALSE, significance=0.05){#returns a two unit vector with the max and min slopes
  test<-multiple_breakups(data)
  count<-nrow(test)
  true_slope<-test[count,4] #find the slope of the longest series
  if(only_significant== TRUE){ #if user specifies only significant values wanted, pull those
    test1<-test[which(test$p_value<significance),]
  }else{
    test1<-test
  }
  max_slope<-max(test1$slope)-true_slope
  min_slope<-min(test1$slope)-true_slope
  sloperange<-c(min_slope, max_slope)
  return(sloperange)
  
}

relative_range(test, only_significant = FALSE, significance = 0.05)

#proportion significant- finds the proportion of total windows with statistically significant values

proportion_significant<- function(data, significance=0.05){#returns a single value between 0 and 1
  test<-multiple_breakups(data)
  count<-nrow(test)
  significant_regressions<-test[which(test$p_value<significance),]
  count_sig<-nrow(significant_regressions)
  proportion<-count_sig/count
  return(proportion)
  
}

proportion_significant(test, significance=0.05)

#proportion significantly wrong- we're going to define this as 'directionally wrong'
#where there is a significant relationship that does not match the direction of the true slope

proportion_wrong<- function(data, significance=0.05){#returns a single value between 0 and 1
  test<-multiple_breakups(data)
  count<-nrow(test)
  true_slope<-test[count,4] #find the slope of the longest series
  true_p<-test[count,6]
  #case 1: true slope is not significant
  if (true_p>significance){
    wrong_windows<-test[which(test$p_value<significance),]
  }else{ #true slope is significant
    if(true_slope>0){#true slope is positive
      wrong_windows<test[which(test$slope<0|test$p_value>significance),]#wrong means the slope is the wrong sign or 0
    }else{#true slope is negative
      wrong_windows<test[which(test$slope>0|test$p_value>significance),]#wrong means the slope is the wrong sign or 0
    }
  }
  count_wrong<-nrow(wrong_windows)
  proportion<-count_wrong/count
  return(proportion)
  
}

proportion_wrong(test, significance=0.01)



#proportion wrong by series length- basically the same thing as proportion wrong but looped 
#over all the unique window lengths. Will output a data frame with a window length and proportion
#of outputs are significantly misleading, plus average r square for that window length

proportion_wrong_series<- function(data, significance=0.05){#returns a single value between 0 and 1
  test<-multiple_breakups(data)
  count<-nrow(test)
  true_slope<-test[count,4] #find the slope of the longest series
  true_p<-test[count,6]
  windows<-unique(test$N_years)#get a list of unique window lengths
  prop.vec<-c()#create a blank vector to store proportions in
  r.vec<-c()#create vector for storing average rsquare values in
  for(i in 1:length(windows)){#for each window length, compute proportion 'wrong'
    window_length<-windows[i]
    test_subset<-test[which(test$N_years==window_length),]
    number_of_windows<-nrow(test_subset)#how many windows
    #case 1: true slope is not significant
    if (true_p>significance){
      wrong_windows<-test_subset[which(test_subset$p_value<significance),]
    }else{ #true slope is significant
      if(true_slope>0){#true slope is positive
        wrong_windows<test_subset[which(test_subset$slope<0|test_subset$p_value>significance),]#wrong means the slope is the wrong sign or 0
      }else{#true slope is negative
        wrong_windows<test_subset[which(test_subset$slope>0|test_subset$p_value>significance),]#wrong means the slope is the wrong sign or 0
      }
    }
    count_wrong<-nrow(wrong_windows)
    proportion<-count_wrong/number_of_windows
    prop.vec<-c(prop.vec, proportion)
    avg.confidence<-mean(test_subset$r_square)
    r.vec<-c(r.vec, avg.confidence)
  }
  
  x_name <- "window_length"
  y_name <- "proportion_wrong"
  z_name <- "avg_r_square"
  
  df <- data.frame(windows, prop.vec, r.vec)
  names(df) <- c(x_name, y_name, z_name)
  return(df)
  
}


#test it
proportion_wrong_series(test, significance = 0.1)

