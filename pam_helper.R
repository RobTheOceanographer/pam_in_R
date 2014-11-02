# pam helper functions.
# 
# rm(list = ls())
# setwd("/Users/Rob_MacPro/RobsCodeLibrary/R/PAM_in_R/pam_dev/codeDevelopment/testData")
# filename = '01042014_zeroPAR.csv'
# data = read.csv(filename, sep = ";",header = FALSE)
# par = read.csv('/Users/Rob_MacPro/RobsCodeLibrary/R/PAM_in_R/pam_dev/codeDevelopment/testData/01042014_PARdata.csv', sep = ",",header = TRUE)

dataCleanUp <- function(data,par){
  library(functional)
  
  dataM = as.matrix(data[names(data)]) #takes the keys for the list and builds a matrix out of the stupid list. The matrix is a char matrix as we have mixed types within a column (eg numbers and letters).
  DatePos = which(dataM[2,] == 'Date')
  TimePos = which(dataM[2,] == 'Time')
  TypePos = which(dataM[2,] == 'Type')
  NoPos = which(dataM[2,] == 'No.')
  FPos = which(dataM[2,] == '1:F')
  FmPos = which(dataM[2,] == "1:Fm'")
  PARPos = which(dataM[2,] == '1:PAR')
  YieldPos = which(dataM[2,] == '1:Y (II)')
  ETRPos = which(dataM[2,] == '1:ETR')
  

  
  #Next step is to clean up the matrix.
  y = which(dataM[,TypePos] == 'FO')
  #create an empty data.frame
  goodY = matrix(NA, nrow = length(y), ncol = 1)
  
  # This checks that ther light crurves have got 9 points.
  for (i in 1:length(y)) {
    if((y[i]+8) <= nrow(dataM)){
      if(sum(dataM[(y[i]:(y[i]+8)),TypePos] == c("FO", "F", "F", "F", "F", "F", "F", "F", "F")) == 9){
        goodY[i] = y[i]
      }
    }
  }
  
  # y is the original position of the FO's and goodY is the position of those that had F0 + 8 rows (eg the light curve is a complete 9 points). It would be good to save this information and print it for some users??
  y
  goodY
  
  rm(i)
  Y <- goodY[apply(goodY, 1, Compose(is.finite, all)),] # checks for infinite values where the F0 names occur - eg their indexes.
  
  dataF = data.frame(matrix(NA, nrow = (length(Y)*9), ncol = ncol(dataM)))
  colnames(dataF) = c("Date", "Time", "IDX", "MemNo", "F", "Fm", "PAR","Yield", "ETR")
  
  for(i in 1:length(Y)){
    if(i ==1){ # This corrects for the first light curve not starting at row one in the original dataset.
      dataF$Date[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),DatePos]
      dataF$Time[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),TimePos]
      dataF$IDX[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),TypePos]
      dataF$MemNo[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),NoPos]
      dataF$F[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),FPos]
      dataF$Fm[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),FmPos]
      dataF$PAR[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),PARPos]
      dataF$Yield[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),YieldPos]
      dataF$ETR[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),ETRPos]
    }else{
      dataF$Date[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),DatePos]
      dataF$Time[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),TimePos]
      dataF$IDX[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),TypePos]
      dataF$MemNo[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),NoPos]
      dataF$F[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),FPos]
      dataF$Fm[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),FmPos]
      dataF$PAR[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),PARPos]
      dataF$Yield[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),YieldPos]
      dataF$ETR[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),ETRPos]
    }
  }
  
  # replace the par values for the data that we have asked the user for.
  #dataF['newPAR'] <- NA
  
  if(is.null(par)){
    
  }else{
    for(i in seq(1, nrow(par), 9)) {
      ind = which(dataF$MemNo == par$UMemNo[i])
      dataF$PAR[ind:(ind+8)] = par$PAR[i:(i+8)]
    }
    rm(i,ind)
  }
    
  df <- dataF[,colSums(is.na(dataF))<nrow(dataF)] #removes any extra columns of NA
  dataFIN = na.omit(df) #removes any extra rows of NA
  rm(i,Y,goodY,data,dataF,dataM,df, DatePos, TimePos, TypePos, NoPos, FPos, FmPos, PARPos, YieldPos, ETRPos) #cleans up.
  
  # There is still some '-' in the dataset that we now need to remove.
  
  dataFIN[dataFIN$IDX == 'FO' & dataFIN == '-'] = 0 # finds all FO rows that also have '-' in some columns and replaces them with 0.
  # removes rows with '-'
  i = which(dataFIN == '-',arr.ind = TRUE)
  if(length(i) > 0){
    dataFIN <- dataFIN[-(i[,1]),]
  }
  rm(i)
  
  # delete any light curves that have less than 5 points left after cleaning.
  i <- which(dataFIN$IDX == 'FO')
  # manually check the last curve.
  if(as.numeric(length(dataFIN$IDX) - i[length(i)] < 6)){
    dataFIN[i[length(i)]:(length(dataFIN$IDX)),] = NA
  } 
# now check the rest of the curves.  
  for(n in 1:(length(i)-1)){
    if(as.numeric(i[n+1] - i[n]) < 6){
      dataFIN[i[n]:(i[n+1]-1),] = NA
    } 
  }
  dataFIN = na.omit(dataFIN) 
  #recals etr = yield * par
  P = as.numeric(dataFIN$PAR)
  #Calculate new ETR
  dataFIN$newETR = as.numeric(dataFIN$Yield) * P
  
  return(dataFIN)
}
#----------------------------------------------------------------------------------
#


dataCleanUp1 <- function(data){
  library(functional)
  
  
  dataM = as.matrix(data[names(data)]) #takes the keys for the list and builds a matrix out of the stupid list. The matrix is a char matrix as we have mixed types within a column (eg numbers and letters).
  DatePos = which(dataM[2,] == 'Date')
  TimePos = which(dataM[2,] == 'Time')
  TypePos = which(dataM[2,] == 'Type')
  NoPos = which(dataM[2,] == 'No.')
  FPos = which(dataM[2,] == '1:F')
  FmPos = which(dataM[2,] == "1:Fm'")
  PARPos = which(dataM[2,] == '1:PAR')
  YieldPos = which(dataM[2,] == '1:Y (II)')
  ETRPos = which(dataM[2,] == '1:ETR')
  
  
  
  #Next step is to clean up the matrix.
  y = which(dataM[,TypePos] == 'FO')
  #create an empty data.frame
  goodY = matrix(NA, nrow = length(y), ncol = 1)
  
  for (i in 1:length(y)) {
    if((y[i]+8) <= nrow(dataM)){
      if(sum(dataM[(y[i]:(y[i]+8)),TypePos] == c("FO", "F", "F", "F", "F", "F", "F", "F", "F")) == 9){
        goodY[i] = y[i]
      }
    }
  }
  
  rm(i, y)
  Y <- goodY[apply(goodY, 1, Compose(is.finite, all)),]
  
  dataF = data.frame(matrix(NA, nrow = (length(Y)*9), ncol = ncol(dataM)))
  colnames(dataF) = c("Date", "Time", "IDX", "MemNo", "F", "Fm", "PAR","Yield", "ETR")
  
  for(i in 1:length(Y)){
    if(i ==1){
      dataF$Date[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),DatePos]
      dataF$Time[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),TimePos]
      dataF$IDX[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),TypePos]
      dataF$MemNo[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),NoPos]
      dataF$F[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),FPos]
      dataF$Fm[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),FmPos]
      dataF$PAR[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),PARPos]
      dataF$Yield[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),YieldPos]
      dataF$ETR[i:(i+8)] <- dataM[(Y[i]:(Y[i]+8)),ETRPos]
    }else{
      dataF$Date[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),DatePos]
      dataF$Time[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),TimePos]
      dataF$IDX[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),TypePos]
      dataF$MemNo[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),NoPos]
      dataF$F[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),FPos]
      dataF$Fm[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),FmPos]
      dataF$PAR[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),PARPos]
      dataF$Yield[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),YieldPos]
      dataF$ETR[(((i-1)*9)+1):(((i-1)*9)+9)] <- dataM[(Y[i]:(Y[i]+8)),ETRPos]
    }
  }
  
  # replace the par values for the data that we have asked the user for.
  #dataF['newPAR'] <- NA
#   
#   if(is.null(par)){
#     
#   }else{
#     for(i in seq(1, nrow(par), 9)) {
#       ind = which(dataF$MemNo == par$UMemNo[i])
#       dataF$PAR[(ind-8):ind] = par$PAR[i:(i+8)]
#     }
#     rm(i,ind)
#   }
#   
  df <- dataF[,colSums(is.na(dataF))<nrow(dataF)] #removes any extra columns of NA
  dataFIN = na.omit(df) #removes any extra rows of NA
  rm(i,Y,goodY,data,dataF,dataM,df, DatePos, TimePos, TypePos, NoPos, FPos, FmPos, PARPos, YieldPos, ETRPos) #cleans up.
  
  # There is still some '-' in the dataset that we now need to remove.
  
  dataFIN[dataFIN$IDX == 'FO' & dataFIN == '-'] = 0 # finds all FO rows that also have '-' in some columns and replaces them with 0.
  # removes rows with '-'
  i = which(dataFIN == '-',arr.ind = TRUE)
  if(length(i) > 0){
    dataFIN <- dataFIN[-(i[,1]),]
  }
  rm(i)
  
  # delete any light curves that have less than 5 points left after cleaning.
  i <- which(dataFIN$IDX == 'FO')
# manually check the last curve.
if(as.numeric(length(dataFIN$IDX) - i[length(i)] < 6)){
  dataFIN[i[length(i)]:(length(dataFIN$IDX)),] = NA
} 
# now check the rest of the curves.  

  for(n in 1:(length(i)-1)){
    if(as.numeric(i[n+1] - i[n]) < 6){
      dataFIN[i[n]:(i[n+1]-1),] = NA
    } 
  }
  dataFIN = na.omit(dataFIN) 
  #recals etr = yield * par
  P = as.numeric(dataFIN$PAR)
  #Calculate new ETR
  dataFIN$newETR = as.numeric(dataFIN$Yield) * P
  
  return(dataFIN)
}











# setwd("/Users/Rob_MacPro/RobsCodeLibrary/R/PAM_in_R/")
# filename = '150414.csv'
# data = read.csv(filename, sep = ";",header = FALSE)
# dataFIN = dataCleanUp(data)
# LC = extractLC(dataFIN,2)

howManyLCs <- function(dataFIN){
  return(length(dataFIN$IDX[dataFIN$IDX == 'FO']))
}

# n = 1
extractLC <- function(dataFIN,n){
  # Extract a light curve to work on.
  # Pull out the actual data and create another dataframe that we will use in the calc - this way it has all the info it needs already in it - like length etc.
  startPos <- which(dataFIN$IDX == 'FO')  
    # pulls out the data we're going to use for this step.
    i = startPos[n]
    if(i == startPos[length(startPos)]){
      step = (length(dataFIN$IDX) - startPos[n])
    }else{
      step = (startPos[n+1] - startPos[n]) - 1  
    }
    currentData <- dataFIN[startPos[n]:(startPos[n]+step),]
    # converting the numerical values into numbers... need to do this higher up and get rid of this section later.
    currentData$MemNo = as.numeric(currentData$MemNo)
    currentData$F = as.numeric(currentData$F)
    currentData$Fm = as.numeric(currentData$Fm)
    currentData$PAR = as.numeric(currentData$PAR)
    currentData$Yield = as.numeric(currentData$Yield)
    currentData$ETR = as.numeric(currentData$ETR)
    currentData$newETR = as.numeric(currentData$newETR)
    currentData$currentP <- as.numeric(currentData$PAR)
   
  return(currentData)  
}
  
  ## -- this is the auto QC stuff.
#   #if we're only interested in the no B version then we can divide the data set into half and use the max from te first half and exclude data after that.
#     ETRhalf = currentData$newETR[1:(ceiling(length(currentData$newETR)/2)+1)]
#     Ihalf =  currentP[1:(ceiling(length(currentP)/2)+1)]
#     cutoffMax = max(ETRhalf)    
#     if(length(which(currentData$newETR > cutoffMax)) > 0){
#       ETR = currentData$newETR[-(which(currentData$newETR > cutoffMax))]
#       I = currentP[-(which(currentData$newETR > cutoffMax))]
#     }else{
#       ETR = currentData$newETR
#       I = currentP
#     }    
#     if(length(ETR) > 0){
#     }else{
#       print("quality control has deleted all your data. Bad luck.")
#       I = NA
#       ETR = NA
#     }
#     # Quality control - this checks that the first few values of the light curve are not zero - this sometimes happens in the PAM
#     if(ETR[2] > 0 & ETR[2] > ETR[1] & ETR[3] > ETR[2]){ #checks if the second value in the etr is greater than zero. if so then use value 3 as the initial in the platt eqn
#       strt = 2
#       I = I
#       ETR = ETR
#     }else{
#       if(ETR[3] > 0 & ETR[3] > ETR[1] & ETR[4] > ETR[3]){ #if the 2nd value is a dud then it will check the 3rd and use the 4th.
#         strt = 3
#         I = I[-2]
#         ETR = ETR[-2]
#       }else{
#         strt = 4
#         I = I[-2]
#         I = I[-2]
#         ETR = ETR[-2]
#         ETR = ETR[-2]
#       }
#     }
#     
#     if(length(ETR) > 0){
#       #       if(debugging){
#       #         plot(I,ETR)  
#       #       }
#     }else{
#       print("quality control has deleted all your data. Bad luck.")
#       I = NA
#       ETR = NA
#     }
#     
#     
#     lowerCutOff = (max(ETR) - max(ETR)*0.5)
#     
#     median(ETR[-1]) #middle of our values
#     median(ETR[-1]) + sd(ETR[-1])
#     median(ETR[-1]) - sd(ETR[-1])
#     
#     # only cut it off at 50% after the max point.
#     
#     if(length(which(ETR[2:length(ETR)] < lowerCutOff))>0){
#       I = I[-(which(ETR[which(ETR == max(ETR)):length(ETR)] < lowerCutOff) + (which(ETR == max(ETR))-1) )]
#       ETR = ETR[-(which(ETR[which(ETR == max(ETR)):length(ETR)] < lowerCutOff) + (which(ETR == max(ETR))-1) )]
#       #I = I[-(which(ETR[2:length(ETR)] < lowerCutOff)+1)]
#       #ETR = ETR[-(which(ETR[2:length(ETR)] < lowerCutOff)+1)] 
#     }else{
#       I = I
#       ETR = ETR
#     }
#     if(length(ETR) > 0){
#       #       if(debugging){
#       #         plot(I,ETR)  
#       #       }
#     }else{
#       print("quality control has deleted all your data. Bad luck.")
#       I = NA
#       ETR = NA
#     }
#   
# ## ------

# setwd("/Users/Rob_MacPro/RobsCodeLibrary/R/PAM_in_R/")
# filename = '150414.csv'
# data = read.csv(filename, sep = ";",header = FALSE)
# dataFIN = dataCleanUp(data)
# LC = extractLC(dataFIN,2)
# p = pamFIT(LC,FALSE)
# rm(data, dataFIN, filename)
# dataFIN = LC
# calcBetaSwitch = FALSE

pamFIT <- function(dataFIN,calcBetaSwitch){
  ETR = as.numeric(dataFIN$newETR)
  I = as.numeric(dataFIN$PAR)
    # define platt et al 1980 eqn. and calculate initial guesses for the alpha and rETR and Beta
    if(calcBetaSwitch){
      PlattEqn = ETR ~ rETRscal*(1-exp((-A*I)/rETRscal))*exp((-B*I)/rETRscal) #defining the Platt (1980) equation used to fit models to raw RLC data
      B=((ETR[length(ETR)-2]-ETR[length(ETR)]) / (I[length(I)]-I[length(I)-2]))
    }else{
      PlattEqn = ETR ~ rETRscal*(1-exp((-A*I)/rETRscal))*exp((-0*I)/rETRscal) #B set to 0
      B = 0
    }
    A = mean(c(ETR[2],ETR[3])) / mean(c(I[2],I[3]))
    rETRscal=as.numeric(mean(c(ETR[which(ETR == max(ETR))],ETR[which(ETR == max(ETR))-1])))

  if(calcBetaSwitch){
    Platt = nls(PlattEqn, start=c(rETRscal=rETRscal, A=A, B=B), control=nls.control(maxiter = 2000, tol = 0.25, minFactor = 0, warnOnly=TRUE));
  }else{
    Platt = nls(PlattEqn, start=c(rETRscal=rETRscal, A=A), control=nls.control(maxiter = 2000, tol = 0.25, minFactor = 0, warnOnly=TRUE));    
  }
  #print(summary(Platt))
  params = summary(Platt)$"parameters"[,1] #rETRscal, A & B
  fits = summary(Platt)$"parameters"[,2:4]
  #print(params)
  
  PFit = data.frame(matrix(NA, nrow = 1, ncol = 16))
  colnames(PFit)  = c("A","A_stdErr","A_tValue","A_pValue","B","B_stdErr","B_tValue","B_pValue","rETRscal","rETRscal_stdErr","rETRscal_tValue","rETRscal_pValue","rETRmax","Ek","First_MemNo","FvFm")
  c = coef(Platt)
  PFit$A = c[2]
  PFit$B = B
  PFit$rETRscal = c[1]
  PFit$rETRmax = abs(as.numeric(params[1]*params[2]/(params[2]+abs(0))*(abs(0)/(params[2]+abs(0)))^(abs(0)/params[2])))
  PFit$Ek = as.numeric(PFit$rETRmax/params[2]) # actuallt Ek?
  PFit$First_MemNo = dataFIN$MemNo[1]
  PFit$FvFm = dataFIN$Yield[1]
  # adding the fit info:
  PFit$A_stdErr = fits[2,1]
  PFit$A_tValue = fits[2,2]
  PFit$A_pValue = fits[2,3]
  PFit$B_stdErr = fits[3,1]
  PFit$B_tValue = fits[3,2]
  PFit$B_pValue = fits[3,3]
  PFit$rETRscal_stdErr = fits[1,1]
  PFit$rETRscal_tValue = fits[1,2]
  PFit$rETRscal_pValue = fits[1,3]
  
# plot(I,ETR,pch=16,cex=1.5)
# pars2<- PFit  
# with(pars2,curve(rETRscal*(1-exp((-A*x)/rETRscal))*exp((-B*x)/rETRscal), add=TRUE, lty=2, lwd=1))  
  
  return(PFit)
  }
  


pamSAVE <- function(results){
  if (is.null(results)){
    return(NULL)
  }else{
  savename = paste("/Users/Rob_MacPro/RobsCodeLibrary/R/PAM_in_R/PAM_APP/file",results$Filename,"memNum",results$First_MemNo,".Rda")
  save(results, file = savename)
  }
}





#-- 
# 
# setwd("/Users/Rob_MacPro/RobsCodeLibrary/R/PAM_in_R/")
# filename = '150414.csv'
# data = read.csv(filename, sep = ";",header = FALSE)
# dataFIN = dataCleanUp(data)
# LC = extractLC(dataFIN,2)
# p = pamFIT(LC,FALSE)
# rm(data, dataFIN, filename)
# dataFIN = LC
# calcBetaSwitch = FALSE
# filename = 'file2014Trial.csv'
# PlattFit = p
# results <- data.frame(matrix(NA, ncol = 8))
# colnames(results) = c("Filename","First_MemNo","FvFm","rETRmax","Ek","alpha","beta","rETRscaler")

pamAppend <- function(results,PlattFit,filename){

  toADD = data.frame(matrix(NA, ncol = 8))
  colnames(toADD) = c("Filename","First_MemNo","FvFm","rETRmax","Ek","alpha","beta","rETRscaler")
  toADD$Filename = filename
  toADD$First_MemNo = as.numeric(PlattFit$First_MemNo)
  toADD$FvFm = as.numeric(PlattFit$FvFm)
  toADD$rETRmax = as.numeric(PlattFit$rETRmax)
  toADD$Ek = as.numeric(PlattFit$Ek)
  toADD$alpha = as.numeric(PlattFit$A)
  toADD$beta = as.numeric(PlattFit$B)
  toADD$rETRscaler = as.numeric(PlattFit$rETRscal)
  
  #check if it exists?
  
  
#   if(PlattFit$First_MemNo %in% results$First_MemNo){
#     i = which(results$First_MemNo == PlattFit$First_MemNo)
#     results[i,] = toADD
#   }else{
     results = rbind(results,toADD) 
#   }
  return(results)
}




pamAppend2 <- function(p){
  
  if(n ==1){
    return(p)
  }else{
    if(p$First_MemNo %in% p$First_MemNo){
      i = which(results$First_MemNo == PlattFit$First_MemNo)
      results[i,] = toADD
    }else{
      results = rbind(results,toADD) 
    }
    n <<- n+1
    return(results)
    
    
  }  
}


# data = values
# this will be a function for cleaning up and saving the data out.
pamFinalClean <- function(data){
  dataNoNA = na.omit(data) #removes any extra rows of NA
  finData <<- data.frame("Filename" = NA,"First_MemNo"=NA,"FvFm"=NA,"rETRmax"=NA,"Ek"=NA,"alpha"=NA,"beta"=NA,"rETRscaler"=NA)
  memNos = unique(dataNoNA$First_MemNo)
  for(i in 1:length(memNos)){
  posi = which(dataNoNA$First_MemNo == memNos[i])
  #if (is.null(posi))
  #  return(NULL)s
  finData[i,] = dataNoNA[posi[length(posi)],]
  }
  return(finData)
}
  




#---
#   
# # create an empty dataframe redy to put the new results into
# results = data.frame(matrix(NA, nrow = (length(dataFIN$IDX[dataFIN$IDX == 'FO'])), ncol = 10))
# colnames(results) = c("Filename","First_MemNo","FvFm","rETRmax","Imm","Ib","Ek","alpha","beta","rETRscaler")
# results$Filename = filename
# 
# 
# 
# # Addition of some error handling code...
# errors = tryCatch({
#   if(calcBetaSwitch){
#     Platt = nls(PlattEqn, start=c(rETRscal=rETRscal, A=A, B=B), control=nls.control(maxiter = 2000, tol = 0.25, minFactor = 0, warnOnly=TRUE));
#   }else{
#     Platt = nls(PlattEqn, start=c(rETRscal=rETRscal, A=A), control=nls.control(maxiter = 2000, tol = 0.25, minFactor = 0, warnOnly=TRUE));    
#   }
#   print(summary(Platt))
#   if(calcBetaSwitch){
#     params = summary(Platt)$"parameters"[,1] #rETRscal, A & B
#     results$rETRmax[n] = abs(as.numeric(params[1]*params[2]/(params[2]+abs(params[3]))*(abs(params[3])/(params[2]+abs(params[3])))^(abs(params[3])/params[2])))
#     results$Imm[n] = as.numeric(params[1]/params[2]*log((params[2]+abs(params[3]))/abs(params[3]), exp(1)))  #"Im" called "Imm" so as not to clash with inbuilt function
#     results$Ek[n] = as.numeric(results$rETRmax[n]/params[2]) # actuallt Ek?
#     results$Ib[n] = as.numeric(params[1]/abs(params[3]))                 
#     results$alpha[n] = as.numeric(params[2])
#     results$beta[n] = as.numeric(params[3])     #"Beta" called "Betaa" so as not to clash with inbuilt function
#     results$rETRscaler[n] = as.numeric(params[1])
#     results$First_MemNo[n] = dataFIN$MemNo[i]
#     results$FvFm[n] = dataFIN$Yield[i]
#   }else{
#     params = summary(Platt)$"parameters"[,1] #rETRscal, A & B
#     results$rETRmax[n] = abs(as.numeric(params[1]*params[2]/(params[2]+abs(0))*(abs(0)/(params[2]+abs(0)))^(abs(0)/params[2])))
#     results$Imm[n] = NA #Im
#     results$Ek[n] = as.numeric(results$rETRmax[n]/params[2]) # actuallt Ek?
#     results$Ib[n] = NA
#     results$alpha[n] = as.numeric(params[2])
#     results$beta[n] = 0 #"Beta" called "Betaa" so as not to clash with inbuilt function
#     results$rETRscaler[n] = as.numeric(params[1])
#     results$First_MemNo[n] = dataFIN$MemNo[i]
#     results$FvFm[n] = dataFIN$Yield[i]    
#   }
#   #simple plot.
#   i = nchar(results$Filename[n], type = "chars", allowNA = FALSE);namTemp = results$Filename[n]
#   png(file = paste(d,'/plots/curve',results$First_MemNo[n],'file',substr(namTemp,1,i-4),'.png', sep = "", collapse = NULL))
#   plot(as.numeric(currentP),  as.numeric(currentData$newETR),pch=16,cex=1.5,main=c(results$First_MemNo[n],results$Filename[n]))
#   points(I,  ETR,pch=16,cex=1.5,col="red")
#   pars2<- as.list(coef(Platt))  
#   with(pars2,curve(rETRscal*(1-exp((-A*x)/rETRscal))*exp((-B*x)/rETRscal), add=TRUE, lty=2, lwd=1))
#   dev.off()
#   
#   # clean up some stuff.
#   #rm(strt,step,rETRscal,params,I,ETR,ed,B,A,currentData,currentP)
#   
# }, warning = function(w) {
#   print("Your data are badly formatted and need cleaning up or modifying before we can fit a curve.")
#   plot(as.numeric(currentP),  as.numeric(currentData$newETR),pch=16,cex=1.5,main=c(results$First_MemNo[n],results$Filename[n]))
#   print('Please click on the points you wish to EXCLUDE from the analysis and then click the "finish" button when you are done')
#   print('********')
#   coords <- identify(as.numeric(currentP),  as.numeric(currentData$newETR)) # identify points 
#   coords # display list
#   I = as.numeric(currentP[-coords])
#   ETR = as.numeric(currentData$newETR[-coords])
#   # define platt et al 1980 eqn. and calculate initial guesses for the alpha and rETR and Beta
#   if(calcBetaSwitch){
#     PlattEqn = ETR ~ rETRscal*(1-exp((-A*I)/rETRscal))*exp((-B*I)/rETRscal) #defining the Platt (1980) equation used to fit models to raw RLC data
#     B=((ETR[length(ETR)-2]-ETR[length(ETR)]) / (I[length(I)]-I[length(I)-2]))
#   }else{
#     PlattEqn = ETR ~ rETRscal*(1-exp((-A*I)/rETRscal))*exp((-0*I)/rETRscal) #B set to 0
#     B = 0
#   }
#   A=mean(c(ETR[2],ETR[3])) / mean(c(I[2],I[3]))
#   rETRscal=as.numeric(mean(c(ETR[which(ETR == max(ETR))],ETR[which(ETR == max(ETR))-1])))
#   
#   
#   if(calcBetaSwitch){
#     Platt = nls(PlattEqn, start=c(rETRscal=rETRscal, A=A, B=B), control=nls.control(maxiter = 2000, tol = 0.25, minFactor = 0, warnOnly=TRUE));
#   }else{
#     Platt = nls(PlattEqn, start=c(rETRscal=rETRscal, A=A), control=nls.control(maxiter = 2000, tol = 0.25, minFactor = 0, warnOnly=TRUE));    
#   }
#   print(summary(Platt))
#   if(calcBetaSwitch){
#     params = summary(Platt)$"parameters"[,1] #rETRscal, A & B
#     results$rETRmax[n] = abs(as.numeric(params[1]*params[2]/(params[2]+abs(params[3]))*(abs(params[3])/(params[2]+abs(params[3])))^(abs(params[3])/params[2])))
#     results$Imm[n] = as.numeric(params[1]/params[2]*log((params[2]+abs(params[3]))/abs(params[3]), exp(1)))  #"Im" called "Imm" so as not to clash with inbuilt function
#     results$Ek[n] = as.numeric(results$rETRmax[n]/params[2]) # actuallt Ek?
#     results$Ib[n] = as.numeric(params[1]/abs(params[3]))                 
#     results$alpha[n] = as.numeric(params[2])
#     results$beta[n] = as.numeric(params[3])     #"Beta" called "Betaa" so as not to clash with inbuilt function
#     results$rETRscaler[n] = as.numeric(params[1])
#     results$First_MemNo[n] = dataFIN$MemNo[i]
#     results$FvFm[n] = dataFIN$Yield[i]
#   }else{
#     params = summary(Platt)$"parameters"[,1] #rETRscal, A & B
#     results$rETRmax[n] = abs(as.numeric(params[1]*params[2]/(params[2]+abs(0))*(abs(0)/(params[2]+abs(0)))^(abs(0)/params[2])))
#     results$Imm[n] = NA #Im
#     results$Ek[n] = as.numeric(results$rETRmax[n]/params[2]) # actuallt Ek?
#     results$Ib[n] = NA
#     results$alpha[n] = as.numeric(params[2])
#     results$beta[n] = 0 #"Beta" called "Betaa" so as not to clash with inbuilt function
#     results$rETRscaler[n] = as.numeric(params[1])
#     results$First_MemNo[n] = dataFIN$MemNo[i]
#     results$FvFm[n] = dataFIN$Yield[i]    
#   }
#   #simple plot.
#   i = nchar(results$Filename[n], type = "chars", allowNA = FALSE);namTemp = results$Filename[n]
#   png(file = paste(d,'/plots/curve',results$First_MemNo[n],'file',substr(namTemp,1,i-4),'.png', sep = "", collapse = NULL))
#   plot(as.numeric(currentP),  as.numeric(currentData$newETR),pch=16,cex=1.5,main=c(results$First_MemNo[n],results$Filename[n]))
#   points(I,  ETR,pch=16,cex=1.5,col="red")
#   pars2<- as.list(coef(Platt))  
#   with(pars2,curve(rETRscal*(1-exp((-A*x)/rETRscal))*exp((-B*x)/rETRscal), add=TRUE, lty=2, lwd=1))
#   dev.off()
#   
#   # clean up some stuff.
#   #   rm(strt,step,rETRscal,params,I,ETR,ed,B,A,currentData,currentP)
# }, error = function(e) {
#   print("Something really badly went wrong... 'NA' has been placed in the results data.frame for this curve.")
#   results$rETRmax[n] = NA
#   results$Imm[n] = NA
#   results$Ek[n] = NA
#   results$Ib[n] = NA
#   results$alpha[n] = NA
#   results$beta[n] = NA
#   results$rETRscaler[n] = NA
#   results$First_MemNo[n] = dataFIN$MemNo[i]
#   results$FvFm[n] = dataFIN$Yield[i]
# }, finally = {
#   #print("This is the critical step... if you didn't receive any errors then you're good to go. ")
# })
  
  
#}

#pamCalc(dataFIN,1)
