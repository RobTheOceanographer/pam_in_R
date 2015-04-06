# This will become a set of functions that can be packaged up into a module and released to the world at some stage... 
# but until then it is a place for me to re-write stuff without brealking the oringal.
#
#
#
# author: Robert Johnson
# credits: Robert Johnson, Simon Reeves, Shihong Lee, Emma Flukes
# version: 1.1
# maintainer: Robert Johnson
# email: robtheoceanographer@gmail.com
# status: Development
###########################################################################################################################

pam_file_reader <- function(name_of_file_to_load, your_file_delimiter=';'){
  #------------------------------------------------------------------------------------------------------------#
  # This function is an easy loader function that has a default delimiter set to ; but can be easily switched to ,
  #
  # USAGE: file_data <- pam_file_reader('T9allsites and Tfinal Mn.csv')
  #
  # author: Robert Johnson
  # version: 0.1
  # maintainer: Robert Johnson
  # email: robtheoceanographer@gmail.com
  # status: Development
  #
  # TODO: add error handling to tell the user what's happening when things break.
  #
  #------------------------------------------------------------------------------------------------------------#
  file_data = read.csv(name_of_file_to_load, sep = your_file_delimiter,header = FALSE)    
  return(file_data)
}

data_munger <- function(raw_file_data, number_of_light_levels = 9){
  #------------------------------------------------------------------------------------------------------------#
  # This function is a data munging function that takes da loaded pam file and hacks it into shape read for 
  # quality control.
  #
  # USAGE: usable_file <- data_munger(raw_file_data)
  #
  # Expects:
  #     - to find the following header names: 'Date', 'Time', 'Type', 'No.', '1:F', '1:Fm', '1:PAR', '1:Y (II)', '1:ETR'
  #     - to either be given the number of light levels per light curve or that there are 9.
  #
  # author: Robert Johnson
  # version: 0.1
  # maintainer: Robert Johnson
  # email: robtheoceanographer@gmail.com
  # status: Development
  #
  # TODO: add error handling to tell the user what's happening when things break.
  #
  #------------------------------------------------------------------------------------------------------------#

  # 1) Build a matrix of the data file - The matrix is a char matrix as we have mixed types within a column (eg numbers and letters).
  raw_data_matrix = as.matrix(raw_file_data[names(raw_file_data)])
  
  # 2) Find the columns where the interesting data is by matching header strings.
  DatePos = which(raw_data_matrix[2,] == 'Date')
  TimePos = which(raw_data_matrix[2,] == 'Time')
  TypePos = which(raw_data_matrix[2,] == 'Type')
  NoPos = which(raw_data_matrix[2,] == 'No.')
  FPos = which(raw_data_matrix[2,] == '1:F')
  FmPos = which(raw_data_matrix[2,] == "1:Fm'")
  PARPos = which(raw_data_matrix[2,] == '1:PAR')
  YieldPos = which(raw_data_matrix[2,] == '1:Y (II)')
  ETRPos = which(raw_data_matrix[2,] == '1:ETR')
  
  # 3) Find the starting position of each light curve by indexing all the "FO"'s.
  LC_start_pos = which(raw_data_matrix[,TypePos] == 'FO')
  
  # 4) Create an empty data.frame to put the index of complete or good light curves into
  LC_check_for_length = matrix(NA, nrow = length(LC_start_pos), ncol = 1)
  
  # 5) Fill in a series of index place holders for each light curves. The default is 9 but you can set what you like.
  for(i in 1:length(LC_start_pos)) { # work through each light curve.
    if(LC_start_pos[i]+(number_of_light_levels-1) <= nrow(raw_data_matrix)){ # makes sure that we're not at the end of the file.      
      if(length(raw_data_matrix[(LC_start_pos[i]:(LC_start_pos[i]+(number_of_light_levels-1))),TypePos]) == number_of_light_levels){ #check that there are the right number of levels in this light curve.
        LC_check_for_length[i] = LC_start_pos[i] # shoves the index of the good lc into the empty matrix
      }
    }
  }
  
  # 6) some clean up...
  rm(i,   LC_start_pos)
  
  # 7) extracts only the light curves that are complete.
  library(functional)
  Y <- LC_check_for_length[apply(LC_check_for_length, 1, Compose(is.finite, all)),] 
  
  # 8) makes a new data matrix to shove our data into.
  munged_pam_data = data.frame(matrix(NA, nrow = (length(Y)*9), ncol = ncol(raw_data_matrix)))
  colnames(munged_pam_data) = c("Date", "Time", "IDX", "MemNo", "F", "Fm", "PAR","Yield", "ETR")
  
  # 9) shoves the data into the new matrix.
  for(i in 1:length(Y)){
    if(i ==1){ # for the first light curve.
      munged_pam_data$Date[i:(i+8)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),DatePos]
      munged_pam_data$Time[i:(i+8)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),TimePos]
      munged_pam_data$IDX[i:(i+8)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),TypePos]
      munged_pam_data$MemNo[i:(i+8)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),NoPos]
      munged_pam_data$F[i:(i+8)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),FPos]
      munged_pam_data$Fm[i:(i+8)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),FmPos]
      munged_pam_data$PAR[i:(i+8)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),PARPos]
      munged_pam_data$Yield[i:(i+8)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),YieldPos]
      munged_pam_data$ETR[i:(i+8)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),ETRPos]
    }else{
      munged_pam_data$Date[(((i-1)*9)+1):(((i-1)*9)+9)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),DatePos]
      munged_pam_data$Time[(((i-1)*9)+1):(((i-1)*9)+9)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),TimePos]
      munged_pam_data$IDX[(((i-1)*9)+1):(((i-1)*9)+9)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),TypePos]
      munged_pam_data$MemNo[(((i-1)*9)+1):(((i-1)*9)+9)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),NoPos]
      munged_pam_data$F[(((i-1)*9)+1):(((i-1)*9)+9)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),FPos]
      munged_pam_data$Fm[(((i-1)*9)+1):(((i-1)*9)+9)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),FmPos]
      munged_pam_data$PAR[(((i-1)*9)+1):(((i-1)*9)+9)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),PARPos]
      munged_pam_data$Yield[(((i-1)*9)+1):(((i-1)*9)+9)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),YieldPos]
      munged_pam_data$ETR[(((i-1)*9)+1):(((i-1)*9)+9)] <- raw_data_matrix[(Y[i]:(Y[i]+8)),ETRPos]
    }
  }
    
  
  # 10) Removes any extra columns of NA.
  final_munged_pam_data <- munged_pam_data[,colSums(is.na(munged_pam_data))<nrow(munged_pam_data)]
  
  # 11) clean up a bit
  rm(i,Y,LC_check_for_length,munged_pam_data,raw_data_matrix, DatePos, TimePos, TypePos, NoPos, FPos, FmPos, PARPos, YieldPos, ETRPos)
  
  # 12) return the loaded and lightly munged data frame.
  return(final_munged_pam_data)
}


replace_par_values <- function(usable_file, list_of_par_vals){
  #------------------------------------------------------------------------------------------------------------#
  # This function replaces the par values throughout an entire file with the 9 values give to it.
  #
  # USAGE: replace_par_values(usable_file, list_of_par_vals)
  #        e.g: list_of_par_vals = c(0, 38, 58, 86, 117, 170, 228, 337, 481)
  # Expects:
  #     - that the par values given to it will be 9 lines and that they are to be pasted over the existing data throughout the file.
  #     
  #
  # author: Robert Johnson
  # version: 0.1
  # maintainer: Robert Johnson
  # email: robtheoceanographer@gmail.com
  # status: Development
  #
  # TODO: - add error handling to tell the user what's happening when things break. 
  #
  #------------------------------------------------------------------------------------------------------------#
  indx_of_lcs <- which(usable_file$IDX == 'FO')
  for(lc in indx_of_lcs){
    usable_file$PAR[lc:(lc+8)] <- as.numeric(list_of_par_vals)
  }
  rm(lc,list_of_par_vals)
  return(usable_file)
}



pam_data_quality_control <- function(usable_file){
  #------------------------------------------------------------------------------------------------------------#
  # This function is a quality control function that takes the munged pam file and checks that each light curve is good
  #
  # USAGE: pam_qc_data <- pam_data_quality_control(usable_file)
  #
  # Expects:
  #     
  #     - to either be given the number of light levels per light curve or that there are 9.
  #
  # author: Robert Johnson
  # version: 0.1
  # maintainer: Robert Johnson
  # email: robtheoceanographer@gmail.com
  # status: Development
  #
  # TODO: - add error handling to tell the user what's happening when things break. 
  #
  #------------------------------------------------------------------------------------------------------------#
  
  # 1) Remove any extra rows of NA
  clean_and_usable_file = na.omit(usable_file)
  
  # There is still some '-' in the dataset that we now need to remove.
  # 2) # finds all FO rows that also have '-' in some columns and replaces them with 0.
  clean_and_usable_file[clean_and_usable_file$IDX == 'FO' & clean_and_usable_file == '-'] = 0 
    
  # 3) Removes all other rows that have the '-'
  i = which(clean_and_usable_file == '-',arr.ind = TRUE)
  if(length(i) > 0){
    # We used to delete the rows that had any dashes in them... like this:
    #clean_and_usable_file <- clean_and_usable_file[-(i[,1]),]
    # now we will just replace the - with NA... like this:
    clean_and_usable_file[i] <- NA
  }
  rm(i)
  
  # 4) Delete any light curves that have less than 5 points left after cleaning
  i <- which(clean_and_usable_file$IDX == 'FO')
  if(as.numeric(length(clean_and_usable_file$IDX) - i[length(i)] < 6)){ #checking the last curve first
    clean_and_usable_file[i[length(i)]:(length(clean_and_usable_file$IDX)),] = NULL
  } 
  #now check the rest of the curves.
  for(n in 1:(length(i)-1)){
    if(as.numeric(i[n+1] - i[n]) < 6){
      clean_and_usable_file[i[n]:(i[n+1]-1),] = NA
    } 
  }
  
  # 5) re-check for na's 
  #clean_and_usable_file = na.omit(clean_and_usable_file) 
  
  # 6) check for dodgy strings in the file.
  clean_and_usable_file$IDX[which(is.na(clean_and_usable_file$IDX))] = 'Z' # convert NA to Z - because of the NA's we've left in until here.
  for(i in 1:length(clean_and_usable_file$IDX)){
    if (clean_and_usable_file$IDX[i] != 'FO' && clean_and_usable_file$IDX[i] != 'F'){
      clean_and_usable_file$IDX[i] <- NA
      clean_and_usable_file$MemNo[i] <- NA
      clean_and_usable_file$F[i] <- NA
      clean_and_usable_file$Fm[i] <- NA
      clean_and_usable_file$PAR[i] <- NA
      clean_and_usable_file$Yield[i]<-NA
      clean_and_usable_file$ETR[i]<- NA
    }  
  }
  
  
  # 7) converting the numerical values into numbers.
  clean_and_usable_file$MemNo = as.numeric(clean_and_usable_file$MemNo)
  clean_and_usable_file$F = as.numeric(clean_and_usable_file$F)
  clean_and_usable_file$Fm = as.numeric(clean_and_usable_file$Fm)
  clean_and_usable_file$PAR = as.numeric(clean_and_usable_file$PAR)
  clean_and_usable_file$Yield = as.numeric(clean_and_usable_file$Yield)
  clean_and_usable_file$ETR = as.numeric(clean_and_usable_file$ETR)
#  clean_and_usable_file$currentP <- as.numeric(clean_and_usable_file$PAR)
  
  
  # 8) find all the dates that are NA and delete these rows - this is to remove the larger chunks of invalid data.
  clean_and_usable_file <- clean_and_usable_file[-(which(is.na(clean_and_usable_file$Date))),]  
  
  # 9) recalculate etr = yield * par
  P = as.numeric(clean_and_usable_file$PAR) # pull out the par data
  clean_and_usable_file$newETR = as.numeric(clean_and_usable_file$Yield) * P  
  rm(i, n, P)
  
  # remove any rows where the F or FO indexi is NA.
  clean_and_usable_file <- clean_and_usable_file[-(which(is.na(clean_and_usable_file$IDX))),]
  
  # 10) if a  new ETR value is NA change the corresponding PAR values to NA so that the nls stuff doesn't fall over later.
  clean_and_usable_file$PAR[which(is.na(clean_and_usable_file$newETR))] <- NA
  
  return(clean_and_usable_file)
}



how_many_light_curves <- function(pam_qc_data){
  #------------------------------------------------------------------------------------------------------------#
  # This function takes a cleaned pam dataset and returns how many light curves are in it.
  #
  # USAGE: the_number_of_light_curves <- how_many_light_curves(pam_qc_data)
  #
  # author: Robert Johnson
  # version: 0.1
  # maintainer: Robert Johnson
  # email: robtheoceanographer@gmail.com
  # status: Development
  #
  # TODO: add error handling to tell the user what's happening when things break.
  #
  #------------------------------------------------------------------------------------------------------------#
  
  # 1) count how many FO's there are in the file.
  return(length(pam_qc_data$IDX[pam_qc_data$IDX == 'FO']))
}



extract_a_light_curve <- function(pam_qc_data,the_light_curve_to_get){
  #------------------------------------------------------------------------------------------------------------#
  # This function extracts ta light curve from ta file based on the curve number.
  #
  # USAGE: a_light_curve <- extract_a_light_curve(pam_qc_data, 1)
  #
  # the_light_curve_to_get = the number of the curve in the file: e.g. the 1st or the 20th etc...
  #
  # author: Robert Johnson
  # version: 0.1
  # maintainer: Robert Johnson
  # email: robtheoceanographer@gmail.com
  # status: Development
  #
  # TODO: add error handling to tell the user what's happening when things break.
  #
  #------------------------------------------------------------------------------------------------------------#
  
  # 1) find all the light curve indexes based on 'FO'
  startPos <- which(pam_qc_data$IDX == 'FO')
  
  # 2) Find the index of the curve asked for and work out the length of data needed to extract
  i = startPos[the_light_curve_to_get]
  if(i == startPos[length(startPos)]){
    step = (length(pam_qc_data$IDX) - startPos[the_light_curve_to_get])
  }else{
    step = (startPos[the_light_curve_to_get+1] - startPos[the_light_curve_to_get]) - 1  
  }
  
  # 3) extract the curve.
  a_light_curve <- pam_qc_data[startPos[the_light_curve_to_get]:(startPos[the_light_curve_to_get]+step),]
    
  return(a_light_curve)  
}




pam_platt_fit <- function(a_light_curve,calcBetaSwitch = FALSE, maximum_number_iterations = 2000, tolerance_level = 1, min_step_size = 0){
  #------------------------------------------------------------------------------------------------------------#
  # This function takes a single light curve and fits the platt et al 1980 eqn. to it.
  #
  # USAGE: platt_fitted_pam_data <- pam_platt_fit(a_light_curve)
  #
  #               maximum_number_iterations = 2000 #the maximum number of iterations allowed.
  #               tolerance_level = 0.25 #the tolerance level for the relative offset convergence criterion
  #               min_step_size = 0 #the minimum step-size factor allowed on any step in the iteration
  #
  # author: Robert Johnson
  # version: 0.1
  # maintainer: Robert Johnson
  # email: robtheoceanographer@gmail.com
  # status: Development
  #
  # TODO: add error handling and print statements to tell the user what's happening when things break.
  #
  #------------------------------------------------------------------------------------------------------------#
  
  # 1) extract the ETR and PAR data for this lc
  ETR = na.omit(as.numeric(a_light_curve$newETR))
  I = na.omit(as.numeric(a_light_curve$PAR))
  
  # 2) define platt et al 1980 eqn. and calculate some initial guesses for the alpha and rETR and Beta
  if(calcBetaSwitch){ # this is for a beta calculation
    PlattEqn = ETR ~ rETRscal*(1-exp((-A*I)/rETRscal))*exp((-B*I)/rETRscal)
    B=((ETR[length(ETR)-2]-ETR[length(ETR)]) / (I[length(I)]-I[length(I)-2]))
  }else{ # this is for without a beta calculation
    PlattEqn = ETR ~ rETRscal*(1-exp((-A*I)/rETRscal))*exp((-0*I)/rETRscal) #B set to 0
    B = 0
  }
  # initial guesses:
  A = mean(c(ETR[2],ETR[3])) / mean(c(I[2],I[3]))
  
  # this is the average between the max and the first values... this is how we originally did it.
  #rETRscal=as.numeric(mean(c(ETR[which(ETR == max(ETR))],ETR[which(ETR == max(ETR))-1]))) 
  
  # now trialling the max value.
  rETRscal=ETR[which(ETR == max(ETR))]
  
  # 3) solve the model using the nonlinear (weighted) least-squares estimate of the parameters of the nonlinear model.
  
  if(calcBetaSwitch){ #with beta
    Platt = nls(PlattEqn, start=c(rETRscal=rETRscal, A=A, B=B), control=nls.control(maxiter = maximum_number_iterations, tol = tolerance_level, minFactor = min_step_size, warnOnly=TRUE));
  }else{ #without beta
    Platt = nls(PlattEqn, start=c(rETRscal=rETRscal, A=A), control=nls.control(maxiter = maximum_number_iterations, tol = tolerance_level, minFactor = min_step_size, warnOnly=TRUE));    
  }

  if(Platt$convInfo$isConv  == FALSE){
    return(NULL)
  }
  
  # 4) extract the parameter and fit data from the nls object
  params = summary(Platt)$"parameters"[,1] #rETRscal, A & B
  fits = summary(Platt)$"parameters"[,2:4]
  Platt_Fit = data.frame(matrix(NA, nrow = 1, ncol = 16)) # makes an empty data frame to stor the output
  colnames(Platt_Fit)  = c("A","A_stdErr","A_tValue","A_pValue","B","B_stdErr","B_tValue","B_pValue","rETRscal","rETRscal_stdErr","rETRscal_tValue","rETRscal_pValue","rETRmax","Ek","First_MemNo","FvFm")
  c = coef(Platt)
  Platt_Fit$A = c[2]
  Platt_Fit$B = B
  Platt_Fit$rETRscal = c[1]
  Platt_Fit$rETRmax = abs(as.numeric(params[1]*params[2]/(params[2]+abs(B))*(abs(B)/(params[2]+abs(B)))^(abs(B)/params[2])))
  Platt_Fit$Ek = as.numeric(Platt_Fit$rETRmax/params[2]) # actuallt Ek?
  Platt_Fit$First_MemNo = a_light_curve$MemNo[1]
  Platt_Fit$FvFm = a_light_curve$Yield[1]
  # adding the fit info:
  Platt_Fit$A_stdErr = fits[2,1]
  Platt_Fit$A_tValue = fits[2,2]
  Platt_Fit$A_pValue = fits[2,3]
  if(calcBetaSwitch){
    Platt_Fit$B_stdErr = fits[3,1]
    Platt_Fit$B_tValue = fits[3,2]
    Platt_Fit$B_pValue = fits[3,3]
  }else{
    Platt_Fit$B_stdErr = NA
    Platt_Fit$B_tValue = NA
    Platt_Fit$B_pValue = NA
  }
  Platt_Fit$rETRscal_stdErr = fits[1,1]
  Platt_Fit$rETRscal_tValue = fits[1,2]
  Platt_Fit$rETRscal_pValue = fits[1,3]
  
  return(list(first=Platt_Fit, second=Platt))
}




# Shiny App specific functions:

# This is a function for cleaning up and saving the data out of shiny.
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



