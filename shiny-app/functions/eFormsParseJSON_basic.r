#require(RJSONIO)
#require(RUnit)

eFormsParseJSON <- function(fName, rxNameCharacters='A-Za-z0-9_\\-', rxDisallowedValueCharacters='\n\r\t', trimValues=TRUE)
  # Attempts to parse the specified file as JSON.  Returns a named list containing 
  # the parsed JSON, or character string if an error occurs.  
  #
  # NOTE: Logging the response is done by the caller
  # NOTE: The parsed JSON output is modified to conform to last year's XML output.  
  # If elements SITE_ID or VISIT_NO are absent from the input, then an attempt to
  # create them based on the contents of element UID is made, but no error is 
  # returned if this is not successful; that case must be checked handled upstream.
  # Simplification of the parsed results is turned off so that unbranched lists of a
  # single type will remain as lists and not be 'simplified' into vectors, as is the 
  # default for fromJSON(), unless they are unnamed.
#
# ARGUMENTS:
# fname     character string containing full path of file. 
# rxNameCharacters    character string consisting of a regular expression
#                     specifying allowed characters in element names
# rxValueCharacters   character string consisting of a regular expression
#                     specifying allowed characters in element values
# trimValues logical value set to TRUE to trim whitespace from ends of values
#
{					
  # parsedContents <- try(fromJSON(fName, simplify=Strict, simplifyWithNames=FALSE)
  #                      ,silent=TRUE
  # 			 		 )
  # if(!exists('parsedContents')) {
  # 	return('Wow, fromJSON() returned nothing, not sure how this happened')
  # } else if(class(parsedContents) == 'try-error') {
  # 	return(sprintf("fromJSON() returned %s", paste(parsedContents, collapse='; ')))
  # }
  
  parsedContents <- eFormsParseJSONtext(fName)
  if(is.character(parsedContents)) {
    return(parsedContents)
  }
  
  if(!is.null(parsedContents$UID)) {
    parsedUID <- unlist(strsplit(parsedContents$UID, '#'))	# UID is of form "SITE_ID#VISIT_NO#YR"
    site_id <- parsedUID[1]
    visit_no <- parsedUID[2]
    year <- parsedUID[3]
    
    if(is.null(parsedContents$SITE_ID)) {
      parsedContents$SITE_ID <- site_id
    }
    if(is.null(parsedContents$VISIT_NO)) {
      parsedContents$VISIT_NO <- visit_no
    }
    if(is.null(parsedContents$YEAR)) {
      parsedContents$YEAR <- year
    }
  }
  
  parsedContents <- eFormsParseJSON.cleanNames(parsedContents, rxNameCharacters)
  parsedContents <- eFormsParseJSON.cleanValues(parsedContents, rxDisallowedValueCharacters, trimValues)
  
  return(parsedContents)
}


eFormsParseJSON.cleanNames <- function(ll, allowedCharacters)
  # Removes unwanted characters from element names in a list. 
  # NOTE: This currently ignores the names in named vectors as we don't currently
  #       need that to happen.  It does NOT ignore the name associated with a
  #       vector, however (see the namedcharacter element in this test).
{
  if(!is.null(allowedCharacters)) {
    if(!is.na(allowedCharacters)) {
      
      if(is.list(ll)) {
        if(!is.null(names(ll))) {
          # remove characters not wanted in the element names
          names(ll) <- gsub(sprintf('([^%s])', allowedCharacters), '', names(ll))
        }
        
        # recurse on elements that are themselves lists
        ll <- ll %>% 
          lapply(function(el) {
            rc<-eFormsParseJSON.cleanNames(el, allowedCharacters)
            return(rc)
          })
      }
      
    }
  }
  
  return(ll)
}



eFormsParseJSON.cleanValues <- function(ll, disallowedCharacters, trimEnds)
  # Removes unwanted characters from element values in a list. 
{
  if(!is.null(disallowedCharacters)) {
    if(!is.na(disallowedCharacters)) {
      
      if(is.list(ll)) {
        # remove characters not wanted in the element values
        ll <- ll %>%
          lapply(function(el) {
            rc <- el
            if(is.character(el)) {
              rc <- gsub(sprintf('([%s])', disallowedCharacters), '', el) #%>%
                #iconv('utf-8','latin1')
              if(trimEnds) rc <- trimws(rc)
            } else if (is.list(el)) {
              rc<-eFormsParseJSON.cleanValues(el, disallowedCharacters, trimEnds)
            }
            return(rc)
          })
      }
      
    }
  }
  
  return(ll)
}