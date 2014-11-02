library(shiny)

shinyUI(navbarPage("PAM Processor! v0.1",
#                    tabPanel("Welcome",
#                             sidebarLayout(position = "right",
#                                           sidebarPanel( HTML('<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/InteractiveResource" property="dct:title" rel="dct:type">PAM Processor!</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="www.robtheoceanographer.com" property="cc:attributionName" rel="cc:attributionURL">Robert Johnson</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.')),
#                                           mainPanel(
#                                           h3("WELCOME TO THE WONDERFUL WORLD OF PAM!",align = "center"),
#                                           p("This program is brought to you by Rob Johnson, Shihong Lee, Simon Reeves, and Emma Flukes. We are PhD students at the University of Tasmania and we were fed up with the ridiculous amount of time it was taking to analyse PAM Rapid Light Curves using clunky and expensive stats software (copy... paste... copy... paste... SPSS... boooo!). So, we built the PAM Processor! using the free and open stats scripting language R and the web application tool Shiny.", align = "center"),
#                                           p("At the moment the PAM Processor! v0.1 is only set up for Micro-Algae and can NOT calculate Beta - it basically ignores photoinhibition. We are working on bringing you this, and many more, features in the near future.",align = "center"),
#                                           p('2014 Robert Johnson, Shihong Lee, Simon Reeves, and Emma Flukes.',align = "center"),
#                                           h5("Feedback is appreciated:",align = "center"),
#                                           p(a("Robert Johnson's homepage.", href = "http://www.robtheoceanographer.com"),'Email: robtheoceanographer@gmail.com',align = "center"),
#                                           br(),
#                                           hr(),
#                                           br(),
#                                             h3('PAM PROCESSOR! LICENSE AGREEMENT',align = "center"),
#                                             p('By downloading or using this Software, you agree to be bound by the following legal agreement between you and the creators, and to the creative commons "Attribution-ShareAlike 4.0 International" license.',span("If you do not agree to the terms of this Agreement, do not download or use the Software.", style = "color:blue")),
#                                             h4('1. USE'),
#                                             p('PAM Processor! is allowed to be used free of charge under the creative commons "Attribution-ShareAlike 4.0 International" license, as described on the right of this page.'),
#                                             h4('2. REDISTRIBUTION'),
#                                             p('Redistribution of the PAM Processor! in any medium or format is permitted so long as it is attributed to the original creators (shown below)— You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.'),
#                                             p('ShareAlike — If you remix, transform, or build upon the PAM Processor!, you must distribute your contributions under the same license as the original.'),
#                                             h4('3. WARRANTY DISCLAIMER'),
#                                             p('THE PAM Processor! SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH YOU.  SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.'),
#                                             p('IN NO EVENT WILL Robert Johnson, OR ANY PAM Processor! CREATOR OR COPYRIGHT HOLDER BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY DIRECT, INDIRECT, GENERAL, SPECIAL, EXEMPLARY, INCIDENTAL OR CONSEQUENTIAL DAMAGES HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY ARISING OUT OF THE USE OR INABILITY TO USE THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES, A FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE OR BUSINESS INTERRUPTION).'),
#                                           br(),
#                                           p(a("Shiny's homepage", href = "http://shiny.rstudio.com/"),a(", R's homepage.", href = "http://www.r-project.org/")),
#                                           br()
#                                           )
#                             )
#                    ),
  
                   
  tabPanel("Load Data",
  sidebarLayout(
    sidebarPanel(h4('Please load the data you wish to work on in this session:'),
                 fileInput('file1', 'Choose a CSV File',accept=c('text/csv',  'text/comma-separated-values,text/plain', '.csv')),
                 br(),
                 hr(),
                 br(),
                 h5('If you wish to add new PAR data, you can do that here:'),
                 fileInput('file2', 'Choose a PAR CSV File',accept=c('text/csv',  'text/comma-separated-values,text/plain', '.csv')),                 
                 h5("Feedback is appreciated:"),
                 p("robtheoceanographer@gmail.com")
    ),
    mainPanel(
      tableOutput('contents')
    )
  )
  ),  
  
  tabPanel("Process",
           sidebarLayout(
             sidebarPanel(
               h3("It's PAM Time!"),
               p("The data presented on the right is the first Rapid Light Curve (RLC) in the file you just loaded. Refine the fit, shown by the dashed line, by removing any crazy outliers (using the box below) and when you're happy with the estimated parameters press 'SAVE' to store those data. To move on to the next RLC just advance the number in the box below. Once you've finished your PAM Processing session you can download the data you've saved by pressing the 'DOWNLOAD' button. I hope you enjoy your PAM Processor! session."),
               h5(textOutput("numLCs")),
               hr(),
               h5("Choose a RLC to process:"),
               numericInput('LC_Num', 'Light Curve Number:', 1,min = 1, max = 10),
               br(),
               actionButton("saveButton", "SAVE!"),
               helpText("Note: don't panic if you press 'SAVE' more than once, the PAM Processor! will only store data from the last time you click 'SAVE' on any RLC."),
               hr(),
               h5("Removing any crazy outliers:"),
               textInput("removePoints",
                         "Enter a list containing the points you want to remove:",""),
               helpText("Note: You can enter one value or a several seperated by a comma (e.g: 9,6,3)",
                                   "If you do not enter any values then no points will be removed.
                                   If you remove data, then points to be used in the analysis will turn red."),
               hr(),
               checkboxInput('beta', 'Calculate Beta?', FALSE),
               #hr(),
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
               h5(textOutput("fitText4")),
               h5(textOutput("fitText5")),
               h5(textOutput("fitText6")),
               tags$hr(),
               h3("A print out of the selected Rapid Light Curve data:"),
               tableOutput("currentLC"),
               h4("This is a quick plot of the raw data - it is here to help you deal with errors above"),
               plotOutput('plot2'),
#                h4("These are your results so far:"),
#                tableOutput("savedResults"),
               tags$hr()
             )
           )
  )
))