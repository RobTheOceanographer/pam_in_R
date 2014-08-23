# THIS IS WHERE YOU CAN LAUNCH THE PAM PROCESSOR APP FROM.
#
# --
# Step 1.
# If this is your first time running our app then please ensure that your R install is up to data and then install the following packages that it depends on:
install.packages("shiny")
install.packages("functional")
#
# --
# Step 2.
#change the directory to the folder you have the app stored in and then run these few lines of code to launch the app. It will open in your default web browser. we recommend using google chrome for best performance.
library(shiny)
rm(list = ls())
setwd("/Users/shihongl/Desktop/PAM_in_R/PAM_APP_LeesWorkingCopy/")
runApp("pam_app")
#
# --
# Step 3.
# close your web browser and press exit within R to turn off the app.

