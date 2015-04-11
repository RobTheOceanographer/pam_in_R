library(shiny)
# This is the second version of the PAM processing shiny app.
#
# This program is brought to you by Rob Johnson, Shihong Lee, Simon Reeves, and Emma Flukes.
# We were fed up with the ridiculous amount of time it was taking to analyse PAM Rapid Light Curves using clunky
# and expensive stats software (copy... paste... copy... paste... SPSS... boooo!).
# So, we built the PAM Processor! using the free and open stats scripting language R and the web application tool Shiny.
#
# At the moment the PAM Processor! v0.1 is only set up for Micro-Algae and can NOT calculate Beta
# - it basically ignores photoinhibition. We are working on bringing you this, and many more, features in the near future.
#
# author: Robert Johnson
# credits: Robert Johnson, Simon Reeves, Shihong Lee, Emma Flukes
# version: 1.1
# maintainer: Robert Johnson
# email: robtheoceanographer@gmail.com
# status: Development
###########################################################################################################################

shinyUI(
  navbarPage("PAM Processor! v1.1",
    #first tab
    tabPanel("Load RAW Data",
      sidebarLayout(
        sidebarPanel(h4('Please load the data you wish to work on in this session:'),
        fileInput('raw_file_data', 'Choose a CSV File',accept=c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
        radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'),';'),
        br(),
        hr(),
        br(),
        #textInput('newPAR','new PAR Values:', ""),
        wellPanel(
          h5('If you wish to replace this files PAR data, you can do that here:'),
          textInput('PAR1', "enter PAR value 1:",""),
          textInput('PAR2', "enter PAR value 2:",""),
          textInput('PAR3', "enter PAR value 3:",""),
          textInput('PAR4', "enter PAR value 4:",""),
          textInput('PAR5', "enter PAR value 5:",""),
          textInput('PAR6', "enter PAR value 6:",""),
          textInput('PAR7', "enter PAR value 7:",""),
          textInput('PAR8', "enter PAR value 8:",""),
          textInput('PAR9', "enter PAR value 9:",""),
          actionButton("submit_PAR","Submit your data")
        ),
        hr(),
        h5("If you want to save the quality controlled dataset for later use, click the download button below:"),
        downloadButton('downloadCleanData', 'Download Dataset'),
        helpText("Note: Sometimes this download button won't work unless you've already moved to the processing tab first - this is because of how we have set up the dataflow within the app and is something we're working to rectify."),  
        hr()
      ),
      
      mainPanel(
        #tableOutput('contents')
        tableOutput('table')
      )
    )
    ),  
    
    # Next tab - Processing.
    tabPanel("Process",
      sidebarLayout(
        sidebarPanel(
          h3("It's PAM Time!"),
          p("The data presented on the right is the first Rapid Light Curve (RLC) in the file you just loaded. Refine the fit, shown by the dashed line, by removing any crazy outliers (using the box below) and adjusting the fit parameters, and when you're happy with the estimated parameters press 'SAVE' to store those data. To move on to the next RLC just advance the number in the box below. Once you've finished your PAM Processing session you can download the data you've saved by pressing the 'DOWNLOAD' button. I hope you enjoy your PAM Processor! session."),
          h5(textOutput("numLCs")),
          hr(),
          h5("Choose a RLC to process:"),
          numericInput('LC_Num', 'Light Curve Number:', 1,min = 1, max = 10), actionButton("saveButton", "SAVE!"),
          helpText("Note: don't panic if you press 'SAVE' more than once, the PAM Processor! will only store data from the last time you click 'SAVE' on any RLC."),  
          h5("Remove any crazy outliers:"),
          textInput("removePoints",
                    "Enter a list containing the points you want to remove:",""),
          helpText("Note: You can enter one value or a several seperated by a comma (e.g: 9,6,3)",
                   "If you do not enter any values then no points will be removed.
              If you remove data, then points to be used in the analysis will turn red."),
          h5("Fitting parameters:"),
          numericInput('tol', 'The tolerance value for the convergence criterion:', 0.25,min = 0, max = 5, step=0.1),
          numericInput('maxIters', 'The maximum number of iterations allowed.:', 2000, min = 100, max = 10000),
          checkboxInput('beta', 'Calculate Beta?', FALSE),
          br(),

          
          h5("When you've finished processing all the RLCs you want then press 'Download' to export your analysis to a file:"),
          downloadButton('downloadData', 'Download'),
          hr(),
          h5("Feedback is appreciated:"),
          p("robtheoceanographer@gmail.com")
        ),
      mainPanel(
        h3("A plot of the selected Rapid Light Curve:"),
        h5(textOutput("memNum")),
        plotOutput('plot1'),
        tags$hr(),
        h3("Estimated coefficients:"),
        h5(textOutput("fitText1")),
        h5(textOutput("fitText7")),
        #h5(textOutput("fitText2")),
        h5(textOutput("fitText3")),
        h5(textOutput("fitText8")),
        h5(textOutput("fitText4")),
        h5(textOutput("fitText5")),
        h5(textOutput("fitText6")),
        tags$hr(),
        h3("A print out of the selected Rapid Light Curve data:"),
        tableOutput("currentLC"),
        tags$hr()
      )
    )
  )

))