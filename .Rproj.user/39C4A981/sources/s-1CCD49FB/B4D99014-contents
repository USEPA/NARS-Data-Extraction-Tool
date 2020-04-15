


narsOrganizationShiny <- function(surv, pathlist, filelist){
  finalOut <- list() 
  
  for(i in 1:length(filelist)){
    fileName <- filelist[i]
    filePath <- pathlist[i]
    
    print(fileName)

    
    # This step parses data and then organizes data in each file, ignoring any tracking files
    if(grepl('TRACKING',fileName,ignore.case=TRUE)==FALSE){
    
      fileName <- gsub("[[:alnum:]]+[[:punct:]][[:alpha:]]+[[:punct:]][[:alnum:]]+[[:punct:]][[:alnum:]][[:punct:]]", "", fileName)
      fileName <- gsub('.json*', '', fileName)
      fileName <- gsub('.*/', '', fileName)
      
      rr <- eFormsParseJSON(filePath)
      switch(surv,
             'nrsa1819' = {tt <- eFormsOrganize_byTable.nrsa(rr)},
             'ncca20' = {tt <- eFormsOrganize_byTable.ncca(rr)}
             # ,
             # 'nla17' = {tt <- eFormsOrganize_byTable_NLA(rr)},
             # 'nwca21' = {tt <- eFormsOrganize_byTable_NWCA(rr)}
      )
      tt <- eFormsOrganize_byTable(rr)
      
      finalOut[[fileName]] <- tt
      }
    }
  
    return(finalOut)
  }
    


narsWriteShiny <- function(surv, filelist, finalList){
  # Create the first part of the filename for writing to a .csv file, based on visit info and sample type
  subName.out <- unique(finalList[[1]][[1]]$UID)
  print(subName.out)
    objLen <- map(finalList, length)
    specialCases <- names(objLen[objLen>2]) # deal with list objects with > 2 separately
    
    others <- finalList[!(names(finalList) %in% specialCases)]
  if(surv=='nrsa1819'){  
    phab_channel <- finalList[specialCases]
    #phab_channel <- sapply(phab_channel, function(x) x$channel, simplify=TRUE)
    phab_channel <- map_df(phab_channel, 'channel')
    # print(phab_channel)

    phab_chanrip <- finalList[specialCases]
    phab_chanrip <- map_df(phab_chanrip, 'chanrip')
    
    phab_chanxsec <- finalList[specialCases]
    phab_chanxsec <- map_df(phab_chanxsec, 'chanxsec')
    
    phab_littoral <- finalList[specialCases]
    phab_littoral <- map_df(phab_littoral, 'littoral')
    
    phab_thalweg <- finalList[specialCases]
    phab_thalweg <- map_df(phab_thalweg, 'thalweg')    
    
    phab <- list(PHAB_channel = phab_channel, PHAB_chanrip = phab_chanrip, PHAB_chanxsec = phab_chanxsec, PHAB_littoral = phab_littoral, PHAB_thalweg = phab_thalweg)
  }
    meta <- list(Metadata = metadata)
  
  if(surv=='nrsa1819'){  
    return(c(map(others,1),phab,meta))
  }else{
    return(c(map(others,1),meta))
  }
}


