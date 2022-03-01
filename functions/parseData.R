


narsOrganizationShiny <- function(surv, pathlist, filelist){
  finalOut <- list() 
  
  for(i in 1:length(filelist)){
    fileName <- filelist[i]
    filePath <- pathlist[i]
    
    print(fileName)

    
    # This step parses data and then organizes data in each file, ignoring any tracking files
    if(grepl('TRACKING|SHIPPING',fileName,ignore.case=TRUE)==FALSE){
      
      fileName <- gsub("[[:alnum:]]+[[:punct:]][[:alpha:]]+[[:punct:]][[:alnum:]]+[[:punct:]][[:alnum:]][[:punct:]]", "", fileName)
      # If the above pattern is not present, it should be the following
      fileName <- gsub("[[:alnum:]]+[[:punct:]][[:alnum:]]+[[:punct:]][[:alnum:]]+[[:punct:]]", "", fileName)
      fileName <- gsub('.json*', '', fileName)
      fileName <- gsub('.*/', '', fileName)
      # Limit the files that are processed to avoid error if user selects wrong survey
      if((surv=='nla17' & str_detect(fileName, 'ASSESSMENT|INDEX_SAMPLE|PROFILE_CALIBRATION|LITTORAL_SAMPLE|PHAB|VERIFICATION|PROFILE_DATA'))|
         (surv=='nrsa1819' & (fileName %in% c('FISH','FISHGEAR', 'BENTHIC',
                                              'VERIFICATION',
                                              'FIELD','SAMPLES','ASSESSMENT',
                                              'CONSTRAINT','DISCHARGE','SLOPE','TORRENT')|grepl('PHAB',fileName,ignore.case=TRUE)==TRUE))|
         (surv=='ncca20' & fileName %in% c('ASSESSMENT',
                                           'CALIBRATION','VERIFICATION','SAMPLE',
                                           'PROFILE','ECOFISH','HHFISH','SAMPLE_PROCESS'))|
         (surv=='nwca21' & fileName %in% c('AA-1','H-1','P-1','P-2','PV-1','S-1','V-1','V-2','V-3','V-4','W-1'))|
        (surv=='nla22' & str_detect(fileName, 'ASSESSMENT|INDEX|PROFILE|ESA|FISH|CALIBRATION|LITTORAL|PHAB|VERIFICATION'))){
        
        rr <- eFormsParseJSON(filePath)
        switch(surv,
               'nrsa1819' = {tt <- eFormsOrganize_byTable.nrsa(rr)},
               'ncca20' = {tt <- eFormsOrganize_byTable.ncca(rr)},
               'nla17' = {tt <- eFormsOrganize_byTable.nla(rr)},
               'nwca21' = {tt <- eFormsOrganize_byTable.nwca(rr)},
               'nla22' = {tt <- eFormsOrganize_byTable.nla22(rr)}
        )
          
          finalOut[[fileName]] <- tt
        }
      }
    }
  
    return(finalOut)
  }
    


narsWriteShiny <- function(surv, filelist, finalList){
  # Create the first part of the filename for writing to a .csv file, based on visit info and sample type
  subName.out <- unique(finalList[[1]][[1]]$UID)
  print(subName.out)
    objLen <- map(finalList, length)

  if(surv %in% c('nla17', 'nla22')){
    specialCases <- names(finalList)[substring(names(finalList),1,4)=='PHAB']
    
    for(i in 1:length(specialCases)){
      names(finalList)[names(finalList)==specialCases[i]] <- 'PHAB'
    }
    
    others <- finalList[!(names(finalList)=='PHAB')]
    
  }else{
    specialCases <- names(objLen[objLen>2])
    others <- finalList[!(names(finalList) %in% specialCases)]
  }  
    

  if(surv=='nrsa1819'){  
    phab_channel <- finalList[specialCases]
    
    phab_channel <- map_df(phab_channel, 'channel')
    
    phab_chanrip <- finalList[specialCases]
    phab_chanrip <- map_df(phab_chanrip, 'chanrip')
    
    phab_chanxsec <- finalList[specialCases]
    phab_chanxsec <- map_df(phab_chanxsec, 'chanxsec')
    
    phab_littoral <- finalList[specialCases]
    phab_littoral <- map_df(phab_littoral, 'littoral')
    
    phab_thalweg <- finalList[specialCases]
    phab_thalweg <- map_df(phab_thalweg, 'thalweg')    
    
    phab <- list(PHAB_channel = phab_channel, PHAB_chanrip = phab_chanrip, 
                 PHAB_chanxsec = phab_chanxsec, PHAB_littoral = phab_littoral, 
                 PHAB_thalweg = phab_thalweg)
    
    meta <- list(Metadata = metadata.nrsa) 
    
  }else if(surv %in% c('nla17', 'nla22')){
    phab_all <- finalList[names(finalList)=='PHAB']
    phab_all <- map_df(phab_all, 'PHAB')
    
    phab <- list(PHAB=phab_all)
    
    meta <- list(Metadata = metadata.nla)
    
  }else if(surv=='ncca20'){
    meta <- list(Metadata = metadata.ncca) 
  }else if(surv=='nwca21'){
    meta <- list(Metadata = metadata.nwca)
  }
     
  
  if(surv=='nrsa1819'){  
    
    return(c(map(others,1), phab, meta))
    
  }else if(surv %in% c('nla17', 'nla22')){
    
    return(c(map(others,1), phab, meta))
    
  }else{
    
    return(c(map(others,1), meta))
    
  }
}


