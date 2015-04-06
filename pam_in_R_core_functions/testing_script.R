# This is a testing script that loads in the pam in r functions and tests them on some test data.
#
# author: Robert Johnson
# credits: Robert Johnson, Simon Reeves, Shihong Lee, Emma Flukes
# version: 1.1
# maintainer: Robert Johnson
# email: robtheoceanographer@gmail.com
# status: Development
###########################################################################################################################

# 1) clean everything up
rm(list = ls())

# 2) set our working directory - eg. this is where the data is.
setwd("/Users/Rob_MacPro/RobsCodeLibrary/R/PAM_in_R/pam_rel/testData/")

# 3) load in the analysis functions.
source("/Users/Rob_MacPro/RobsCodeLibrary/R/PAM_in_R/pam_rel/pam_in_R_core_functions/pam_in_R_functions.R")

# 4) load the data file.
raw_file_data <- pam_file_reader('TransPAM(june).csv', your_file_delimiter=';')

# 5) format the data file.
usable_file <- data_munger(raw_file_data)

# 5b) optionally replace the par values.
usable_file <- replace_par_values(usable_file, c(0, 9, 29, 60, 101, 151, 207, 308, 422))

# 6) quality control the data file.
pam_qc_data <- pam_data_quality_control(usable_file)

# 7) Fnd out how many light curves are in this file.
the_number_of_light_curves <- how_many_light_curves(pam_qc_data)

# 8) extract a single light curve of your choosing.
a_light_curve <- extract_a_light_curve(pam_qc_data, 10)

# 9) do a trial fit and plot up result.
rm(platt_fit_obj, new) # clean up.
platt_fit_obj <- pam_platt_fit(a_light_curve,calcBetaSwitch = TRUE, tolerance_level = 0.0000001, maximum_number_iterations = 1000) # do fit
platt_fitted_pam_data <- platt_fit_obj$first # extract the fit data
platt_obj <- platt_fit_obj$second #extract the fit object
plot(a_light_curve$PAR,a_light_curve$newETR,pch=16,cex=1.5) # plot points
new = data.frame(I = seq(min(na.omit(a_light_curve$PAR)),max(na.omit(a_light_curve$PAR)),len=200)) # create simultaed data to plot line with
lines(new$I,predict(platt_obj, newdata=new)) # plot a line.

summary(platt_obj) # report this number too: Residual standard error: 0.4338 on 6 degrees of freedom


# TIX: add a figure download button to the processing page - make the title and savename the "filename_curveNumber_memNumb.png"



################### EXPERIMENTAL WORK BELOW ##########################

# When the max etr value is the second point the fit tends to underestimate.
# To avoid this we could add some more points between point 1 and 2 that allow the nls algroithm to have something else to pull it back to a nice curve shape?

a_light_curve[10,] <- c("2014-02-02","10:28:01","F",1033,578,1302,10,0.556,3.5,38,5)
a_light_curve[11,] <- c("2014-02-02","10:28:01","F",1033,578,1302,20,0.556,3.5,38,15)

a_light_curve_temp <- a_light_curve
a_light_curve_temp[1,] <- a_light_curve[1,]
a_light_curve_temp[2,] <- c("2014-02-02","10:28:01","F",1033,578,1302,10,0.556,3.5,38,5)
a_light_curve_temp[3,] <- c("2014-02-02","10:28:01","F",1033,578,1302,20,0.556,3.5,38,15)
a_light_curve_temp[4,] <- a_light_curve[2,]
a_light_curve_temp[5,] <- a_light_curve[3,]
a_light_curve_temp[6,] <- a_light_curve[4,]
a_light_curve_temp[7,] <- a_light_curve[5,]
a_light_curve_temp[8,] <- a_light_curve[6,]
a_light_curve_temp[9,] <- a_light_curve[7,]
a_light_curve_temp[10,] <- NA
a_light_curve_temp[11,] <- NA

a_light_curve <- a_light_curve_temp

rm(platt_fit_obj, new) # clean up.
platt_fit_obj <- pam_platt_fit(a_light_curve,calcBetaSwitch = TRUE, tolerance_level = 0.001) # do fit
platt_fitted_pam_data <- platt_fit_obj$first # extract the fit data
platt_obj <- platt_fit_obj$second #extract the fit object
plot(a_light_curve$PAR,a_light_curve$newETR,pch=16,cex=1.5) # plot points
new = data.frame(I = seq(0,490,len=200)) # create simultaed data to plot line with
lines(new$I,predict(platt_obj, newdata=new)) # plot a line.


# as you tighten up the tolerance the fit gets better - until it breaks...



















##################
# another example of fitting from: http://www.walkingrandomly.com/?p=5254

# construct the data vectors using c()
xdata = c(-2,-1.64,-1.33,-0.7,0,0.45,1.2,1.64,2.32,2.9)
ydata = c(0.699369,0.700462,0.695354,1.03905,1.97389,2.41143,1.91091,0.919576,-0.730975,-1.42001)

# look at it
plot(xdata,ydata)

# some starting values
p1 = 1
p2 = 0.2

# do the fit
fit = nls(ydata ~ p1*cos(p2*xdata) + p2*sin(p1*xdata), start=list(p1=p1,p2=p2))

# summarise
summary(fit)

# add to plot
new = data.frame(xdata = seq(min(xdata),max(xdata),len=200))
lines(new$xdata,predict(fit,newdata=new))

#Getting the sum of squared residuals is easy enough:
sum(resid(fit)^2)

#Finally, lets get the parameter confidence intervals.
confint(fit)
