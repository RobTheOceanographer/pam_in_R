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
library(shiny)

#load in the pam processing functions
source("../pam_in_R_core_functions/pam_in_R_functions.R")

shinyServer(function(input, output, session) {
  
  # brings in the new par data if any.
  newPARdata = reactive({
    if (input$submit_PAR > 0) {
      df <- as.numeric(c(input$PAR1,input$PAR2,input$PAR3,input$PAR4,input$PAR5,input$PAR6,input$PAR7,input$PAR8,input$PAR9))
      return(df)
    }
  })
  
  # load in the users data.
  dataInput <- reactive({  
    input$get
    raw_file_string <- input$raw_file_data
    file_delimiter <- input$sep
    new_par_values <- input$newPAR
    
    if (is.null(raw_file_string)){
      return(NULL)
    } else{
      # 1) load the data file.
      raw_file_data <- pam_file_reader(raw_file_string$datapath, your_file_delimiter=file_delimiter)
      # 2) format the data file.
      usable_file <- data_munger(raw_file_data)
      if(is.null(newPARdata())){
      } else{
        new_par_values <- newPARdata()
      # 2b) optionally replace the par values.
        usable_file <- replace_par_values(usable_file, new_par_values)
      }      
      # 3) quality control the data file.
      pam_qc_data <- pam_data_quality_control(usable_file)
      # 4) return the data.
      return(pam_qc_data)
    }
  })
  
  #send the loaded data back out to the ui for the users to look at.
  output$table <- renderTable({
    dataInput()
  })
  
  # tell me how many LC's there are in this file.
  maxLCs <- reactive(how_many_light_curves(dataInput()))
  
  observe({
    updateNumericInput(session, "LC_Num",  label = NULL, value = NULL, min = NULL, max = maxLCs(), step = NULL)
  })  
  
  observe({
    updateTextInput(session, "saveDir", value = getwd())
  })
  
  # extract the current light curve for the user.
  currentLC <- reactive({
    if (is.null(dataInput()))
      return(NULL)
    extract_a_light_curve(dataInput(), input$LC_Num)
  })
  
  # now render a table from the extracted light curve data.
  output$currentLC <- renderTable({
    if (is.null(currentLC()))
      return(NULL)
    currentLC()
  })
  
  # send a string that tells the user which lc they're up to and what that mem number is.
  output$memNum <- renderText({
    d <- currentLC()
    paste("You are currently viewing curve number ", input$LC_Num ," which begins with the memory number: ",d$MemNo[1])
  })
  
  # shows the user how many lc are in the file.
  output$numLCs <- renderText({
    paste("There are ", maxLCs() ," curves in this file.")
  })
  
  # remove points from the data file.
  reducedLC <- reactive({
    if(nchar(input$removePoints)<1){
      currentLC()
    }else{
      d <- currentLC()
      d[-(as.numeric(strsplit(input$removePoints, ",")[[1]])),]
    }
  })
  
  # do the fitting.
  PlattFit  <- reactive({
    # pamFIT(reducedLC(),FALSE)
    # pamFIT(reducedLC(),input$beta)
    platt_fit_obj <- pam_platt_fit(reducedLC(), calcBetaSwitch = input$beta, maximum_number_iterations = input$maxIters, tolerance_level = input$tol)
    if(is.null(platt_fit_obj)){
      return(NULL)
    }
    platt_fitted_pam_data <- platt_fit_obj$first # extract the fit data
    platt_obj <- platt_fit_obj$second #extract the fit object
    return(platt_fitted_pam_data)
  })
  
  
  # Output the current fit data:
  output$fitText1 <- renderText({
    p = PlattFit()
    paste("alpha: ",round(p$A,3))
  })
  output$fitText7 <- renderText({
    p = PlattFit()
    paste("alpha p value: ",round(p$A_pValue,7))
  })
  output$fitText8 <- renderText({
    p = PlattFit()
    paste("Beta p value: ",round(p$B_pValue,7))
  })
  output$fitText2 <- renderText({
    p = PlattFit()
    paste("rETRscaler: ",round(p$rETRscal,3))
  })
  output$fitText3 <- renderText({
    p = PlattFit()
    paste("Beta: ",round(p$B,4))
  })
  output$fitText4 <- renderText({
    p = PlattFit()
    paste("rETR max: ",round(p$rETRmax,3))
  })
  output$fitText5 <- renderText({
    p = PlattFit()
    paste("Ek: ", round(p$Ek,3))
  })
  output$fitText6 <- renderText({
    p = PlattFit()
    paste("Fv/Fm: ",round(p$FvFm,3))
  })

  # plot the current light curve and its fit and any removed points.
  output$plot1 <- renderPlot({
    d <- currentLC()
    d2 <- reducedLC()
    plot(d$PAR,d$newETR,pch=16,cex=1.5,xlab='PAR',ylab='ETR',col="black")
    text(d$PAR,d$newETR, c(1:nrow(d)),3)
    points(d2$PAR,d2$newETR,pch=16,cex=1.5,col="red")
    
    platt_fit_obj <- pam_platt_fit(reducedLC(), calcBetaSwitch = input$beta, maximum_number_iterations = input$maxIters, tolerance_level = input$tol)
    if(is.null(platt_fit_obj)){
      return(NULL)
    }
    platt_fitted_pam_data <- platt_fit_obj$first # extract the fit data
    platt_obj <- platt_fit_obj$second #extract the fit object
    
    new = data.frame(I = seq(min(na.omit(d$PAR)),max(na.omit(d$PAR)),len=200)) # create simultaed data to plot line with
    lines(new$I,predict(platt_obj, newdata=new)) # plot a line.  
#    pars2 <-  pamFIT(reducedLC(),input$beta)
#    with(pars2,curve(rETRscal*(1-exp((-A*x)/rETRscal))*exp((-B*x)/rETRscal), add=TRUE, lty=2, lwd=1))
  })
  
  # plot the original data at the bottom of the page.
  output$plot2 <- renderPlot({
    d <- currentLC()
    plot(d$PAR,d$newETR,pch=16,cex=1.5,main='Original Raw Data',xlab='PAR',ylab='ETR',col="black")  
  })


  result1 <- reactive({
    PF <- PlattFit()
    input$get
    inFile <- input$raw_file_data
    if (is.null(inFile))
      return(NULL)
    filename = inFile$name
    toADD = data.frame(matrix(NA, ncol = 8))
    colnames(toADD) = c("Filename","First_MemNo","FvFm","rETRmax","Ek","alpha","beta","rETRscaler")
    toADD$Filename = filename
    toADD$First_MemNo = as.numeric(PF$First_MemNo)
    toADD$FvFm = as.numeric(PF$FvFm)
    toADD$rETRmax = as.numeric(PF$rETRmax)
    toADD$Ek = as.numeric(PF$Ek)
    toADD$alpha = as.numeric(PF$A)
    toADD$beta = as.numeric(PF$B)
    toADD$rETRscaler = as.numeric(PF$rETRscal)
    return(toADD)
    
  })
  
  # a temp log file that i use to hold the saved data.
  logfile <- "tempDataFile.csv"
  
  values <<- data.frame("Filename" = NA,"First_MemNo"=NA,"FvFm"=NA,"rETRmax"=NA,"Ek"=NA,"alpha"=NA,"beta"=NA,"rETRscaler"=NA)
  
  observe({
    input$saveButton
    # Assuming your saveButton is an actionButton, from the shiny-incubator package
    if (input$saveButton == 0)
      return()
    isolate({
      values <<- rbind(values, result1())
      write.csv(values, logfile)    
    })
  })
  
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadData <- downloadHandler(
    # filename = c('data.csv'),
    # filename = function() {paste('Processed_PAM_data_',  format(Sys.time(), "%Y-%m-%d_%H-%M"),'.csv',sep="")},
    filename = function() {paste('Processed_PAM_data_',result1()$Filename,'_', format(Sys.time(), "%Y-%m-%d_%H-%M"),'.csv',sep="")},
    content = function(file) {
    write.csv(pamFinalClean(values), file, row.names=FALSE)
    }
  )
  
  output$downloadCleanData <- downloadHandler(
    filename = function() {paste('QualityControlled_PAM_data_',result1()$Filename,'_', format(Sys.time(), "%Y-%m-%d_%H-%M"),'.csv',sep="")},
    content = function(con) {
      write.csv(dataInput(), con, row.names=FALSE)
    }
  )
  
  
})