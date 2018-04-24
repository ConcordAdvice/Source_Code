

# Here's the source code use in the modeling process
# This is a file for recode the modeling functions

library(MASS)
library(partykit)
library(klaR)
library(glmnet)
library(Formula)


# We will add new features to the Data Clean
# 1. All out of range to a specific value
# 2. All data type transformed to the dictionary





# <------------------------------------------------------------------->
# This function will generate a time range sheet with 6 new columns
# The one end with "IND" are numeric column, without are characters
# <------------------------------------------------------------------->

DataProcess.GenerateTimeSheet <- function(Data, 
                                    AppDate.name = "AppDate", 
                                    AppData.format = "%Y-%m-%d", 
                                    RecordID.name = "MRID"){
  # Generate a time range sheet with 6 new columns
  #
  # Args:
  #   Data: The data set with Master Key ID and associated date
  #   AppDate.name: The name of colume which has the date
  #   AppDate.format: The character format of the date colume
  #   RecordID.name: The name of colume which had Master Key ID
  #
  # Returns:
  #   TimeSheet: An 8 col matrix with time factors 
  #   Years Years_IND Months Months_IND Quarter Quarter_IND
  # 
  # 

  cat("--- < Function(GenerateTimeSheet) > ---","\n")
  
  cat("<< AppDate.name :",AppDate.name,"\n")
  cat("<< AppData.format :",AppData.format,"\n")
  cat("<< RecordID.name :",RecordID.name,"\n")
  
  TimeSheet = data.frame(Data[RecordID.name][,1], Data[AppDate.name][,1])
  colnames(TimeSheet) <- c(RecordID.name,AppDate.name)
  TimeSheet[AppDate.name][,1] = as.Date(substr(as.character(TimeSheet[AppDate.name][,1]),1,10),AppData_format)
  format(TimeSheet[AppDate.name][,1],"%m/%d/%y")
  
  head(TimeSheet)
  colnames(TimeSheet)
  TimeSheet$Years <- format(TimeSheet[2][,1],'%y')
  TimeSheet$Years <- paste0("Y",TimeSheet$Years)
  TimeSheet$Years_IND <- format(TimeSheet[2][,1],'%Y')
  TimeSheet$Years_IND <- as.numeric(TimeSheet$Years_IND)
  unique(TimeSheet$Years_IND)
  
  TimeSheet$Months = paste0(TimeSheet$Years,"M",format(TimeSheet[AppDate.name][,1],'%m'))
  TimeSheet$Months_IND <- format(TimeSheet[AppDate.name][,1],'%m')
  TimeSheet$Months_IND <- as.numeric(TimeSheet$Months_IND)
  
  TimeSheet$Quarter = cut(TimeSheet[AppDate.name][,1], breaks = "quarter")
  n <- length(levels(TimeSheet$Quarter))
  k <- c()
  for(i in 1:n){
    tt <- as.numeric(substr(levels(TimeSheet$Quarter)[i],6,7))
    if(tt == 1 ){
      k[i] <- "Q1"
    }else if(tt == 4){
      k[i] <- "Q2"
    }else if(tt == 7){
      k[i] <- "Q3"
    }else if(tt == 10){
      k[i] <- "Q4"
    }else{
      k[i] <- "NA"
    }
  }
  levels(TimeSheet$Quarter) <- k
  TimeSheet$Quarter_IND <- TimeSheet$Quarter
  levels(TimeSheet$Quarter_IND) <- as.numeric(paste(substr(k,2,2)))
  TimeSheet$Quarter <- paste0(TimeSheet$Years,TimeSheet$Quarter)
  TimeSheet$Quarter_IND <- as.numeric(levels(TimeSheet$Quarter_IND))[TimeSheet$Quarter_IND]
  
  head(TimeSheet)
  
  cat(">> dim(TimeSheet) :", dim(TimeSheet),"\n")
  
  return(TimeSheet)
}

TimeSheet <- DataProcess.GenerateTimeSheet(Data_Performance)


# <------------------------------------------------------------------->
# This function will use the time sheet to subset the data 
# Input can be Y16M01 - Y16M12 ; Y16Q1 - Y16Q4; Y16 - Y18 etc
# <------------------------------------------------------------------->

DataProcess.SubsetUsingTimeSheet <- function(Data,Subset.range,
                                             Time.Sheet = TimeSheet,
                                             RecordID.name = "MRID"){
  
  # Generate the subset data set with the Time Sheet
  #
  # Args:
  #   Data: The data set with Master Key ID and associated date
  #   Subset.range: The character string indicating the date range
  #   TimeSheet: The matrix with time dummies
  #   TimeSheet.name: Name of the matrix with time dummies
  #   RecordID.name: The name of colume which had Master Key ID
  #
  # Returns:
  #   SubsetDate: The data with time subset 
  # 
  # Error
  if (is.null(Time.Sheet)) {
    stop(">>> No default TimeSheet matrix found \n")
  }
  # 
 
  cat("---- < function(SubsetUsingTimeSheet) > ----","\n")

  if(Subset.range %in% TimeSheet$Years == TRUE){
    catgo1 <- TimeSheet$Years
  }else if(Subset.range %in% TimeSheet$Quarter == TRUE){
    catgo1 <- TimeSheet$Quarter
  }else if(Subset.range %in% TimeSheet$Months == TRUE){
    catgo1 <- TimeSheet$Months
  }else{
    catgo1 <- NULL
  }
  
  # Error
  if(is.null(catgo1) ){
    stop(">>> Illegal time range string \n","eg: Y16M01 - Y16M12 ; Y16Q1 - Y16Q4; Y16 - Y18")
  } 
  #
  
  dat1 <- data
  id1 <- TimeSheet[catgo1 == Subset.range,RecordID.name]
  SubsetDate <- dat1[dat1[,RecordID.name] %in% id1, ] 
  
  cat(">>> dim(subset) :",dim(SubsetDate)," \n")

  return(SubsetDate)
}

subdata <- DataProcess.SubsetUsingTimeSheet(Data_Independent,"Y16M03")

# <------------------------------------------------------------------->
# This function will calculate the weight of evidence


Data.SMBin_numeric <- function(data,y){
  
  data[is.na(data)] <- 0.01
  data <- lapply(data, function(x) as.numeric(as.character(unlist(x))))
  data <- as.data.frame(data)
  # index = which(colnames(data) == Dependent)
  colnames = colnames(data)
  smb_list = list()
  length(smb_list) = length(colnames)
  names(smb_list) = colnames
  
  data_ctree <- cbind(y,data)
  data_ctree$y <- as.factor(data_ctree$y)
  
  for(i in 1:length(colnames)){
    x = colnames[i]
    ct = ctree(formula(paste("y~",colnames[i])),data=data_ctree)
    bins=width(ct)
    n=length(ct)
    range=range(as.numeric(unlist(na.omit(data_ctree[x]))))
    
    bin <- vector(length=0)
    for (j in 1:n) {
      bin=c(bin,ct[j]$node$split$breaks)
    }
    breaks=c(-Inf,sort(bin),Inf)
    variable <- as.numeric(unlist(data_ctree[x]))
    variable <- cut(variable,breaks=breaks)
    smb_list[[x]] = breaks
    data[x][,1] = variable
  }
  
  data <- apply(data,2,function(x) as.factor(as.character(x)))
  
  return(list(data = data,list = smb_list ))
}













