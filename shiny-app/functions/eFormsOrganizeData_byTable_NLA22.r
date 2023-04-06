# eFormsOrganizeData_byTable_NLA.r
# Purpose: For each type of data, organize into data frames
# First figure out the type of data by sample type
###############################################################

eFormsOrganize_byTable.nla22 <- function(rawData){
  # Extract visit info
  visitinfo <- as.data.frame(rawData[1:7],stringsAsFactors=F)
  
  # Extract sample type from 8th element in each file
  sampletype <- names(rawData)[8]
  
  # # PHAB sample types are special
  sampletype <- ifelse(substring(sampletype,1,4) %in% c('PHAB'),'PHAB',sampletype)
  
  # Create data frame of parsed data to start with, making them all character variables 
  parsedData <- as.data.frame(rawData[8])
  parsedData[,names(parsedData)] <- lapply(parsedData[,names(parsedData)], as.character)
  
  # run parsed data through organizing function, based on sample type 
  switch(sampletype,
         ASSESSMENT = {rr <- organizeAssessment.nla22(parsedData)},
         INDEX_SAMPLE = {rr <- organizeIndex.nla22(parsedData)},
         PROFILE_CALIBRATION = {rr <- organizeCalibration.nla22(parsedData)},
         LITTORAL_SAMPLE = {rr <- organizeLittoral.nla22(parsedData)},
         PHAB = {rr <- organizePhab.nla22(parsedData)},
         VERIFICATION = {rr <- organizeVerification.nla22(parsedData)},
         PROFILE_DATA = {rr <- organizeProfile.nla22(parsedData)},
         ESA = {rr <- organizeESA.nla22(parsedData)},
         FISH = {rr <- organizeFish.nla22(parsedData)}
  )
  
  ss <- list(cbind(visitinfo, rr))
  if(sampletype=='PHAB'){
    names(ss) <- c('PHAB')
  }
  # Add new object to list with sample type name
  ss[["SAMPLE_TYPE"]] <- sampletype
  
  return(ss)
}

#############################################################################################################
# This begins the section which organizes the parsed data by sample type

organizeVerification.nla22 <- function(parsedIn){
  # Simply melt these data and clean up parameter names
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'VERIF'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  aa.long$PARAMETER <- with(aa.long, gsub('VERIFICATION\\.', '', PARAMETER))
  
  aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)
}

organizeIndex.nla22 <- function(parsedIn){
  # Simply melt these data by SAMPLE_TYPE and clean up parameter names
  aa <- subset(parsedIn, select = !(substring(names(parsedIn), 14, 17) %in% c('SECH','CHEM','FDNA','MICZ','TRIA','PHYX','CHLX','ZOCN','ZOFN')))
  aa$SAMPLE_TYPE <- 'PROF'
  
  varLong <- names(aa)[names(aa) != 'SAMPLE_TYPE']
  aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  aa.long <- subset(aa.long, !str_detect(variable, 'REVIEW'))
  aa.long$PARAMETER <- with(aa.long, str_replace(variable, 'INDEX\\_SAMPLE\\.', ''))
  
  aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  
  bb <- subset(parsedIn, select= substring(names(parsedIn),14,17) %in% c('SECH','CHEM','FDNA','MICZ','TRIA','PHYX','CHLX','ZOCN','ZOFN'))
  bb$SAMPLE_TYPE <- 'INDEX'
  
  varLong <- names(bb)[names(bb)!='SAMPLE_TYPE']
  bb.long <- reshape(bb, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  bb.long$SAMPLE_TYPE <- with(bb.long, substring(variable, 14, 17))
  bb.long$PARAMETER <- with(bb.long, substring(variable, 19, nchar(variable)))
  
  bb.out <- subset(bb.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  cc <- rbind(aa.out, bb.out) 
  cc <- cc[order(cc$SAMPLE_TYPE, cc$PARAMETER),]
  
  return(cc)
}

organizeLittoral.nla22 <- function(parsedIn){
  # Simply melt these data by SAMPLE_TYPE and clean up parameter names
  # Benthic samples
  aa <- subset(parsedIn, select = str_starts(names(parsedIn),'LITTORAL\\_SAMPLE\\.BENT'))
  aa$SAMPLE_TYPE <- 'BENT'
  
  varLong <- names(aa)[names(aa)!='SAMPLE_TYPE']
  aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  aa.long$PARAMETER <- with(aa.long, str_replace(variable,'LITTORAL\\_SAMPLE\\.BENT\\_',''))
  aa.long$STATION <- 'ALL'
  
  aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','STATION','PARAMETER','RESULT'))
  
  # Benthic substrate by station
  bb <- subset(parsedIn, select= str_starts(names(parsedIn),'LITTORAL\\_SAMPLE\\.[:alpha:]\\_SUBBENT'))
  bb$SAMPLE_TYPE <- 'SUBBENT'
  
  varLong <- names(bb)[names(bb)!='SAMPLE_TYPE']
  bb.long <- reshape(bb, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  bb.long$STATION <- with(bb.long, substring(variable,17,17))
  bb.long$PARAMETER <- with(bb.long, str_replace(variable,'LITTORAL\\_SAMPLE\\.[:alpha:]\\_SUBBENT\\_',''))
  
  bb.out <- subset(bb.long, select = c('SAMPLE_TYPE','STATION','PARAMETER','RESULT'))
  
  # Enterococci
  cc <- subset(parsedIn, select = str_starts(names(parsedIn), 'LITTORAL\\_SAMPLE\\.ENTE'))
  cc$SAMPLE_TYPE <- 'ENTE'
  
  varLong <- names(cc)[names(cc)!='SAMPLE_TYPE']
  cc.long <- reshape(cc, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  cc.long$STATION <- 'ALL'
  cc.long$PARAMETER <- with(cc.long, str_replace(variable,'LITTORAL\\_SAMPLE\\.ENTE\\_',''))
  
  cc.out <- subset(cc.long, select = c('SAMPLE_TYPE','STATION','PARAMETER','RESULT'))
  
  # Other parameters by station
  dd <- subset(parsedIn, select = str_detect(names(parsedIn), 'LDNA|MUSSEL'))
  dd$SAMPLE_TYPE <- "SUBBENT"
  
  varLong <- names(dd)[names(dd)!='SAMPLE_TYPE']
  dd.long <- reshape(dd, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  dd.long$STATION <- with(dd.long, substring(variable, 17, 17))
  dd.long$PARAMETER <- with(dd.long, str_replace(variable, 'LITTORAL\\_SAMPLE\\.[:alpha:]\\_', ''))
  
  dd.out <- subset(dd.long, select = c('SAMPLE_TYPE','STATION','PARAMETER','RESULT'))
  
  ee <- rbind(aa.out, bb.out, cc.out, dd.out) 
  
  return(ee)
}

organizeAssessment.nla22 <- function(parsedIn){
  
  # Simply melt data and clean up parameter names
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'ASSESS'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  aa.long$PARAMETER <- with(aa.long, gsub('ASSESSMENT\\.', '', PARAMETER))
  
  aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)
  
}

organizeProfile.nla22 <- function(parsedIn){
  # NEED TO FIND PARAMETERS THAT START WITH CHARACTER VS. NUMBER
  aa <- subset(parsedIn, select=str_detect(names(parsedIn), 'F1')==FALSE)
  aa$SAMPLE_TYPE <- 'PROF'
  
  varLong <- names(aa)[names(aa)!='SAMPLE_TYPE']
  aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  aa.long$variable <- with(aa.long, str_replace(variable, "PROFILE\\_DATA\\.",""))
  aa.long$LINE <- with(aa.long, str_extract(variable,'[:digit:]+\\_'))
  aa.long$LINE <- with(aa.long, str_replace(LINE,"\\_",""))
  aa.long$PARAMETER <- with(aa.long, str_replace(variable,"[:digit:]+\\_",''))
  
  aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','LINE','PARAMETER','RESULT'))
  
  bb <- subset(parsedIn, select=str_detect(names(parsedIn),'F1')) 
  
  if(ncol(bb) > 0){
    bb$SAMPLE_TYPE <- 'PROF'
    
    varLong <- names(bb)[names(bb)!='SAMPLE_TYPE']
    bb.long <- reshape(bb, idvar = 'SAMPLE_TYPE', varying = varLong, times = varLong,
                       v.names = 'COMMENT', timevar = 'variable', direction = 'long')
    bb.long$variable <- with(bb.long, str_replace(variable, "PROFILE\\_DATA\\.",""))
    bb.long$LINE <- with(bb.long, str_extract(variable,'[:digit:]+\\_'))
    bb.long$LINE <- with(bb.long, str_replace(LINE,"\\_",""))
    
    bb.out <- subset(bb.long, select = c('SAMPLE_TYPE','LINE','COMMENT'))
    
    cc <- merge(aa.out, bb.out, by=c('SAMPLE_TYPE','LINE'), all=TRUE) 
  }else{
    cc <- aa.out
  }
  
  return(cc)
  
}

organizeCalibration.nla22 <- function(parsedIn){
  # Simply melt data and clean up parameter names
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'CALIB'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  aa.long$PARAMETER <- with(aa.long, gsub('PROFILE\\_CALIBRATION\\.', '', PARAMETER))
  aa.long$LINE <- '0'
  
  aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','LINE','PARAMETER','RESULT'))
  
  return(aa.out)  
  
}

organizePhab.nla22 <- function(parsedIn){
  
  aa <- subset(parsedIn, select=str_detect(names(parsedIn),'COMMENT')==FALSE)
  aa$SAMPLE_TYPE <- 'PHAB'
  
  varLong <- names(aa)[names(aa)!='SAMPLE_TYPE']
  aa.long <- reshape(aa, idvar = 'SAMPLE_TYPE', varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  aa.long$STATION <- with(aa.long, substring(variable,6,6))
  aa.long$PARAMETER <- with(aa.long, str_replace(variable,'PHAB\\.[:alpha:]\\_',''))
  
  aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','STATION','PARAMETER','RESULT'))
  
  bb <- subset(parsedIn, select=str_detect(names(parsedIn),'COMMENT')) 
  
  if(ncol(bb) > 0){
    bb$SAMPLE_TYPE <- 'PHAB'
    
    varLong <- names(bb)[names(bb)!='SAMPLE_TYPE']
    bb.long <- reshape(bb, idvar = 'SAMPLE_TYPE', varying = varLong, times = varLong,
                       v.names = 'COMMENT', timevar = 'variable', direction = 'long')
    bb.long$STATION <- with(bb.long, substring(variable,6,6))
    bb.long$PARAMETER <- with(bb.long, str_replace(variable,'PHAB\\.[:alpha:]\\_',''))
    bb.long$PARAMETER <- with(bb.long, str_replace(PARAMETER,'\\_COMMENT',''))
    
    bb.out <- subset(bb.long, select = c('SAMPLE_TYPE','STATION','PARAMETER','COMMENT'))
    
    cc <- merge(aa.out, bb.out, by=c('SAMPLE_TYPE','STATION','PARAMETER'), all=T)
  }else{
    cc <- aa.out
  }
  
  return(cc)
  
}

organizeESA.nla22 <- function(parsedIn){
  aa <- subset(parsedIn, select=str_detect(names(parsedIn), 'F1')==FALSE)
  aa$SAMPLE_TYPE <- 'ESA'
  
  varLong <- names(aa)[names(aa)!='SAMPLE_TYPE']
  aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  aa.long$variable <- with(aa.long, str_replace(variable, "ESA\\.",""))
  aa.long$LINE <- with(aa.long, str_extract(variable,'[:digit:]+\\_'))
  aa.long$LINE <- with(aa.long, str_replace(LINE,"\\_",""))
  aa.long$PARAMETER <- with(aa.long, str_replace(variable,"[:digit:]+\\_ESA\\_",''))
  
  aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','LINE','PARAMETER','RESULT'))
  
  return(aa.out)
  
}

organizeFish.nla22 <- function(parsedIn){
  # Simply melt data and clean up parameter names
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'FTIS'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  aa.long$PARAMETER <- with(aa.long, gsub('FISH\\.', '', PARAMETER))
  
  aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)  
  
}