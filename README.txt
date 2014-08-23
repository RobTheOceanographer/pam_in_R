WELCOME TO THE WONDERFUL WORLD OF PAM! This program is brought to you by Rob Johnson, Shihong Lee, Simon Reeves, and Emma Flukes. We are PhD students at the University of Tasmania and we were fed up with the ridiculous amount of time it was taking to analyse PAM Rapid Light Curves using clunky and expensive stats software (copy... paste... copy... paste... SPSS... boooo!). So, we built the PAM Processor! using the free and open stats scripting language R and the web application tool Shiny.

At the moment the PAM Processor! v0.1 is only set up for Micro-Algae and can NOT calculate Beta - it basically ignores photoinhibition. We are working on bringing you this, and many more, features in the near future.

2014 Robert Johnson, Shihong Lee, Simon Reeves, and Emma Flukes.

Feedback is appreciated: Robert Johnson's homepage http://www.robtheoceanographer.com Email robtheoceanographer@gmail.com





Notes to myself…


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

