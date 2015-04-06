library(shiny)
source("pam_helper.R")


results <- data.frame(matrix(NA, ncol = 8))
colnames(results) = c("Filename","First_MemNo","FvFm","rETRmax","Ek","alpha","beta","rETRscaler")


shinyServer(function(input, output, session) {
  dataInput <- reactive({  
    input$get
    inFile <- input$file1
    inFile2 <- input$file2
    if (is.null(inFile))
      return(NULL)
    
    if (is.null(inFile2)){
      return(dataCleanUp1(read.csv(inFile$datapath, sep = ";",header = FALSE)))
    } else{
      return(dataCleanUp(read.csv(inFile$datapath, sep = ";",header = FALSE),read.csv(inFile2$datapath, sep = ",",header = TRUE)))
    }
      # return(NULL)
  })
  
  
  dataInput2 <- reactive({  
    input$get
    inFile10 <- input$file10
    inFile20 <- input$file20
    if (is.null(inFile10))
      return(NULL)
    if (is.null(inFile20)){
      return(NULL)
    } else{
      return(quenchingCalculation(read.csv(inFile10$datapath, sep = ",",header = TRUE),read.csv(inFile20$datapath, sep = ",",header = TRUE, stringsAsFactors=FALSE)))
    }
  })
  
  
  output$contents2 <- renderTable({

    dataInput2()
  })
  
  
  
  
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
#     inFile <- input$file1
#     if (is.null(inFile))
#       return(NULL)
#     #read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
#     d = dataCleanUp(read.csv(inFile$datapath, sep = ";",header = FALSE))
#     return(d)
    dataInput()
  })

  maxLCs <- reactive(howManyLCs(dataInput()))

observe({
    updateNumericInput(session, "LC_Num",  label = NULL, value = NULL, min = NULL, max = maxLCs(), step = NULL)
  })  
observe({
  updateTextInput(session, "saveDir", value = getwd())
})

  currentLC <- reactive({
    if (is.null(dataInput()))
      return(NULL)
    extractLC(dataInput(), input$LC_Num)
  })
  
  output$currentLC <- renderTable({
    currentLC()
  })

  output$memNum <- renderText({
    d <- currentLC()
    paste("You are currently viewing curve number ", input$LC_Num ," which begins with the memory number: ",d$MemNo[1])
  })

  output$numLCs <- renderText({
    paste("There are ", maxLCs() ," curves in this file.")
  })
  
  reducedLC <- reactive({
    if(nchar(input$removePoints)<1){
      currentLC()
    }else{
      d <- currentLC()
      d[-(as.numeric(strsplit(input$removePoints, ",")[[1]])),]
    }
  })

  PlattFit  <- reactive({
    # pamFIT(reducedLC(),FALSE)
    pamFIT(reducedLC(),input$beta)
  })



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

  output$plot1 <- renderPlot({
    d <- currentLC()
    d2 <- reducedLC()
    plot(d$PAR,d$newETR,pch=16,cex=1.5,xlab='PAR',ylab='ETR',col="black")
    text(d$PAR,d$newETR, c(1:nrow(d)),3)
    points(d2$PAR,d2$newETR,pch=16,cex=1.5,col="red")
    pars2 <-  pamFIT(reducedLC(),input$beta)
    with(pars2,curve(rETRscal*(1-exp((-A*x)/rETRscal))*exp((-B*x)/rETRscal), add=TRUE, lty=2, lwd=1))
    
  })

output$plot2 <- renderPlot({
  d <- currentLC()
  plot(d$PAR,d$newETR,pch=16,cex=1.5,main='Original Raw Data',xlab='PAR',ylab='ETR',col="black")  
})

# output$result <- renderTable({
#   result1()
# })

result1 <- reactive({
  PF <- PlattFit()
  input$get
  inFile <- input$file1
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
  
  #write.csv(toADD, file = "/Users/Rob_MacPro/RobsCodeLibrary/R/PAM_in_R/PAM_APP/file_Data_Summary.csv", row.names = F)    # files are saved to folder specified in input$dir
  #write.csv(as.list(results), file = paste("file",results$Filename,"memNum",results$First_MemNo,".csv"), sep = ",",row.names = F)
  
})


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


output$downloadNPQData <- downloadHandler(
  filename = function() {paste('NPQ_data_', format(Sys.time(), "%Y-%m-%d_%H-%M"),'.csv',sep="")},
  content = function(con1) {
    write.csv(dataInput2(), con1, row.names=FALSE)
  }
)




  #next thing goes here.  
  
})