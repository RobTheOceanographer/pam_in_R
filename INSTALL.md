## Install guide:

- Download the latest release from github: [https://github.com/RobTheOceanographer/pam_in_R/releases](https://github.com/RobTheOceanographer/pam_in_R/releases)
- The current PAM shiny app is in the "pam_app" folder.
- Ensure that your R and RStudio install is up to date.
- Install the following packages that the pam app depends on:
`install.packages("shiny")` and `install.packages("functional")`

## Running:

- Start RStudio.
- Load the shiny library: `library(shiny)`
- Change the working directory to the folder you have the app stored in. e.g. `setwd("~/PAM_in_R/")` (N.B. this is the folder that contains the `pam_app` folder.)
- Run the app using `runApp("pam_app")`
