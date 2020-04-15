# eFormsOrganizeData_byTable.r
# Purpose: For each type of data, organize into data frames
# First figure out the type of data by sample type
#
# Created 2/27/2019 by Karen Blocksom
###############################################################

eFormsOrganize_byTable.ncca <- function(rawData){
  # Extract visit info
  visitinfo <- as.data.frame(rawData[1:7],stringsAsFactors=F)
  # Extract sample type from 8th element in each file
  sampletype <- names(rawData)[8]

  # Create data frame of parsed data to start with, making them all character variables 
  parsedData <- as.data.frame(rawData[8])
  parsedData[,names(parsedData)] <- lapply(parsedData[,names(parsedData)], as.character)
  
  # run parsed data through organizing function, based on sample type 
  switch(sampletype,
    ASSESSMENT = {rr <- organizeAssessment.ncca(parsedData)},
    CALIBRATION = {rr <- organizeCalibration.ncca(parsedData)},
    VERIFICATION = {rr = organizeVerification.ncca(parsedData)},
    SAMPLES = {rr = organizeSamples.ncca(parsedData)},
    PROFILE = {rr = organizeProfile.ncca(parsedData)},
    ECOFISH = {rr = organizeEcofish.ncca(parsedData)},
    HHFISH = {rr = organizeHHfish.ncca(parsedData)},
    SAMPLE_PROCESS = {rr = organizeSampProc.ncca(parsedData)}
  )
  
  ss <- list(cbind(visitinfo, rr))
    # Add new object to list with sample type name
  ss[["SAMPLE_TYPE"]] <- sampletype
  return(ss)
}

#############################################################################################################
# This begins the section which organizes the parsed data by sample type

organizeVerification.ncca <- function(parsedIn){
# Simply melt these data and clean up parameter names
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'VERIF'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar=c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  aa.long$PARAMETER <- with(aa.long, gsub('VERIFICATION\\.', '', PARAMETER)) 
  
  aa.long$SAMPLE_TYPE <- with(aa.long, ifelse(grepl('MACRO_ALGAE|BOTTOM_TYPE|HABITAT|MACRO_ABUNDANCE|MACROALGAE|MARINE_DEBRIS|MARINE_DEBRIS_TYPE|SAV|SAV_ABUNDANCE', PARAMETER), 'SHAB', 'VERIF'))
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)
  
}

organizeSamples.ncca <- function(parsedIn){
  # Simply melt these data by SAMPLE_TYPE and clean up parameter names
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'SAMPLES'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar = 'SAMPLE_TYPE', varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  aa.long$SAMPLE_TYPE <- with(aa.long, substring(as.character(variable), 9, 12))
  aa.long$PARAMETER <- with(aa.long, ifelse(str_detect(variable, '\\_COMMENT'),
                                      substring(as.character(variable), 9, nchar(as.character(variable))),
                                      substring(as.character(variable),14,nchar(as.character(variable)))))
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)
}

organizeAssessment.ncca <- function(parsedIn){
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'ASSESS'
  
  aa.long <- reshape(aa, idvar = 'SAMPLE_TYPE', varying = names(parsedIn), 
                     times = names(parsedIn), v.names = 'RESULT', timevar = 'PARAMETER',
                     direction = 'long')
  
  aa.long$PARAMETER <- with(aa.long, gsub("ASSESSMENT\\.", "", PARAMETER))
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)

}

organizeProfile.ncca <- function(parsedIn){
 # NEED TO FIND PARAMETERS THAT START WITH CHARACTER VS. NUMBER
  aa <- subset(parsedIn, select=str_starts(names(parsedIn),'PROFILE\\.[:alpha:]'))
  aa$SAMPLE_TYPE <- 'HYDRO'
  aa$LINE <- '0'
  
  varLong <- names(aa)[!(names(aa) %in% c('SAMPLE_TYPE','LINE'))]
  aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE','LINE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  aa.long$PARAMETER <- with(aa.long, str_replace(PARAMETER, "PROFILE\\.",""))
  aa.long$SAMPLE_TYPE <- with(aa.long, ifelse(str_starts(PARAMETER,'CLEAR_TO_BOTTOM|DISAPPEARS|REAPPEARS|SECCHI'),'SECC', SAMPLE_TYPE))
  
  aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','LINE','PARAMETER','RESULT'))
  
# bb pulls out and formats species by line number and sample type
  bb <- subset(parsedIn, select=str_starts(names(parsedIn), 'PROFILE\\.[:digit:]'))
  bb$SAMPLE_TYPE <- 'HYDRO'
  
  varLong <- names(bb)[names(bb)!='SAMPLE_TYPE']
  bb.long <- reshape(bb, idvar='SAMPLE_TYPE', varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  bb.long$variable <- with(bb.long, gsub('PROFILE\\.', '', variable))
  bb.long$LINE <- str_extract(bb.long$variable, '[:digit:]+')
  bb.long$PARAMETER <- str_replace(bb.long$variable, '[:digit:]+\\_', '')

  bb.out <- base::subset(bb.long, select=c('SAMPLE_TYPE','LINE','PARAMETER','RESULT'))
  
  cc <- rbind(aa.out, bb.out) 
  
  return(cc)
  
}

organizeCalibration.ncca <- function(parsedIn){
  # Simply melt data and clean up parameter names
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'CALIB'
  
  varLong <- names(aa)[names(aa)!='SAMPLE_TYPE']
  aa.long <- reshape(aa, idvar = 'SAMPLE_TYPE', varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long') 
  
  aa.long$PARAMETER <- gsub('CALIBRATION\\.', '', aa.long$PARAMETER)
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))

  return(aa.out)  
  
}

organizeEcofish.ncca <- function(parsedIn){
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'EINF'
  
  varLong <- names(aa)[names(aa)!='SAMPLE_TYPE']
  aa.long <- reshape(aa, idvar = 'SAMPLE_TYPE', varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  aa.long$SAMPLE_TYPE <- substring(aa.long$PARAMETER, 9, 12)
  aa.long <- subset(aa.long, str_detect(PARAMETER, 'REVIEW')==FALSE)
  aa.long$PARAMETER <- with(aa.long, substring(PARAMETER, 14, nchar(PARAMETER)))
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE', 'PARAMETER', 'RESULT'))
  
  return(aa.out)
}

organizeHHfish.ncca <- function(parsedIn){
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'HINF' 
  
  varLong <- names(aa)[names(aa)!='SAMPLE_TYPE']
  aa.long <- reshape(aa, idvar = 'SAMPLE_TYPE', varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  
  aa.long$SAMPLE_TYPE <- substring(aa.long$PARAMETER, 8, 11)
  aa.long <- subset(aa.long, str_detect(PARAMETER, 'REVIEW')==FALSE)
  aa.long$PARAMETER <- with(aa.long, substring(PARAMETER, 13, nchar(PARAMETER)))
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE', 'PARAMETER', 'RESULT'))
  
  return(aa.out)
  
}

organizeSampProc.ncca <- function(parsedIn){

  # Simply melt these data by SAMPLE_TYPE and clean up parameter names
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'SAMPLES'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar = 'SAMPLE_TYPE', varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  aa.long$variable <- with(aa.long, gsub('SAMPLE\\_PROCESS\\.', '', variable))
  aa.long$SAMPLE_TYPE <- with(aa.long, substring(as.character(variable), 1, 4))
  aa.long <- subset(aa.long, str_detect(variable, 'REVIEW')==FALSE)
  aa.long$PARAMETER <- substring(aa.long$variable, 6, nchar(aa.long$variable))
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)
  
}
