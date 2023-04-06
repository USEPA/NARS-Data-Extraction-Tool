# eFormsParseJSONtext.r
#
#  9/05/14 cws created
# 10/12/15 cws Corrected name of unit test
#  6/12/17 cws Updated comment documentation to reflect use in parsing contents
#          of a file, after refactoring eFormsParseJSON to use this function
#          instead of calling fromJSON directly.
#  2/05/19 cws Modified unit test to handle two possible responses when the
#          argument is a string of zero characters ("").
#

# require(RJSONIO)
# require(RUnit)

eFormsParseJSONtext <- function(text)
# Attempts to parse the text string as JSON.  Returns a named list containing 
# the parsed JSON, or character string if an error occurs.  
#
# NOTE: Logging the response is done by the caller
#
# ARGUMENTS:
# text     character string, either of JSON formatted data OR of full path of 
#          file containing JSON to parse.
#
{					
	parsedContents <- try(fromJSON(text, simplify=Strict, simplifyWithNames=FALSE)
	                     ,silent=TRUE
				 		 )
	if(class(parsedContents) == 'try-error') {
		return(sprintf("fromJSON() returned %s", paste(parsedContents, collapse='; ')))
	}
		
	return(parsedContents)
}


eFormsParseJSONtextTest <- function()
# Unit test for eFormsParseJSON
{
	# Test with parseable JSON 
    checkEquals(list(LINE=3, LONG='mile')
               ,eFormsParseJSONtext('{"LINE":3, \'LONG\':"mile"}')
               ,"Incorrect parsing of well formed JSON with data"
               )
    
    # Test with parseable empty JSON 
    checkEquals(within(list(x=3), x<-NULL)
               ,eFormsParseJSONtext('{}')
               ,"Incorrect parsing of empty JSON"
               )
    
    # Test with empty string
    checkTrue(eFormsParseJSONtext('') %in% 
              c("fromJSON() returned Error in fromJSON(content, handler, default.size, depth, allowComments,  : \n  invalid JSON input\n"
               ,'fromJSON() returned Error in file(con, "r") : cannot open the connection\n'
               )
               ,"Incorrect response for empty string"
               )
    
    # Test with NA string
    checkEquals("fromJSON() returned Error in file(con, \"r\") : cannot open the connection\n"
               ,eFormsParseJSONtext(as.character(NA))
               ,"Incorrect response of NA string"
               )
    
	# Test with unparseable JSON
    checkEquals("fromJSON() returned Error in file(con, \"r\") : cannot open the connection\n"
               ,eFormsParseJSONtext('"LINE":"3"')
               ,"Incorrect parsing of malformed JSON"
               )
    
}


# end of file