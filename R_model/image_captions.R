#########################################################################################

#image captions script
#pull in all the functions we wrote before
source("bad_breakup_script.R")

#here's most of the objects we go on to create- pretty much all info for the caption 
#templates is contained in them:

output_frame<-multiple_breakups([data])
output_frame
pyramid_plot([data], rsq_points=TRUE)
stability_time([data], min_percent = 95, error_multiplyer = 1)
relative_range([data])
relative_range_after_stability([data])
proportion_significant([data])
proportion_wrong([data])
proportion_wrong_before_stability([data])

#other functions- abs_range, proportion_wrong_series, broken_stick_plot
