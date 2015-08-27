#nhaneS - retrieve data from the CDC NHANES repository

nhanesURL <- 'http://wwwn.cdc.gov/Nchs/Nhanes/'

# Create a list of nhanes groups
# Include convenient aliases
nhanes_group <- list()
nhanes_group['DEMO']          <- "DEMO"
nhanes_group['DEMOGRAPHICS']  <- "DEMO"
nhanes_group['DIETARY']       <- "DIETARY"
nhanes_group['DIET']          <- "DIETARY"
nhanes_group['EXAMINATION']   <- "EXAMINATION"
nhanes_group['EXAM']          <- "EXAMINATION"
nhanes_group['LABORATORY']    <- "LABORATORY"
nhanes_group['LAB']           <- "LABORATORY"
nhanes_group['QUESTIONNAIRE'] <- "QUESTIONNAIRE"
nhanes_group['Q']             <- "QUESTIONNAIRE"
nhanes_survey_groups <- unlist(unique(nhanes_group))

# Although continuous NHANES is grouped in 2-year intervals,
# for convenience we want to specify using a single year
nh_years <- list()
nh_years['1999'] <- "1999-2000"
nh_years['2000'] <- "1999-2000"
nh_years['2001'] <- "2001-2002"
nh_years['2002'] <- "2001-2002"
nh_years['2003'] <- "2003-2004"
nh_years['2004'] <- "2003-2004"
nh_years['2005'] <- "2005-2006"
nh_years['2006'] <- "2005-2006"
nh_years['2007'] <- "2007-2008"
nh_years['2008'] <- "2007-2008"
nh_years['2009'] <- "2009-2010"
nh_years['2010'] <- "2009-2010"
nh_years['2011'] <- "2011-2012"
nh_years['2012'] <- "2011-2012"

# Continuous NHANES table names have a letter suffix that indicates the collection interval
data_idx <- list()
data_idx['1999-2000'] <- ""
data_idx['2001-2002'] <- "B"
data_idx['2003-2004'] <- "C"
data_idx['2005-2006'] <- "D"
data_idx['2007-2008'] <- "E"
data_idx['2009-2010'] <- "F"
data_idx['2011-2012'] <- "G"


# An internal function that converts a year into the nhanes interval.
# 
# E.g. 2003 is converted to '2003-2004'
# @param year where year is numeric in yyyy format
# @return The 2-year interval that includes the year, e.g. 2001-2002
# 

#------------------------------------------------------------------------------

get_nh_survey_years <- function(year) {
  if(as.character(year) %in% names(nh_years)) {
    return( as.character(nh_years[as.character(year)]) )
  }
  else {
    stop('Data for year ', year, ' is not available')
    return(NULL)
  }
}

#------------------------------------------------------------------------------

#' Returns a list of table names for the specified survey group
#' 
#' @importFrom stringr str_replace str_c str_match
#' @param nh_surveygroup The type of survey (DEMOGRAPHIC, DIETARY, EXAMINATION, LABORATORY, QUESTIONNAIRE)
#' @param year The year in yyyy format where 1999 <= yyyy <= 2012
#' @param details If true then a more detailed description of the tables is returned
#' @return The names of the tables in the specified survey group
#' @details The first step in browsing NHANES data is to display the table names. 
#' The table names are stored internal to the nhanes package, i.e. the information is retrieved from 
#' internal tables such as vars_EXAMINATION_2005-2006. 
#' @examples
#' nhanesTables('EXAM', 2007)
#' nhanesTables('LAB', 2009, details=TRUE)
#' @export
#' 
nhanesTables <- function(nh_surveygroup, year, details = FALSE) {
  if( !(nh_surveygroup %in% names(nhanes_group)) ) {
    stop("Invalid survey group")
    return(NULL)
  }
  
  #cat(year)
  nh_year <- get_nh_survey_years(year)
  
  vartablename <- str_c('vars_', nhanes_group[[nh_surveygroup]], '_', str_replace(get_nh_survey_years(year),'-','_'), collapse='')
  df <- do.call(data.frame, list(as.name(vartablename)) )

  idx <- str_c('_', data_idx[[nh_year]], sep='')
  if( idx == '_') {idx = '' }
  
  if(details == TRUE) {
    df <- unique(df[,3:length(df)])
    df <- df[!is.na(str_match(df[['Data.File.Name']], idx)),]
    row.names(df) <- c(1:nrow(df))
    return(df)
  }
  else {
    tablenames <- as.character(unique(df[['Data.File.Name']]))
    tablenames <- tablenames[!is.na(str_match(tablenames, idx))]
    desc  <- character(length(tablenames))
    for(i in 1:length(tablenames)) {
      desc[i] <- as.character(df[df[['Data.File.Name']]==tablenames[i],][['Data.File.Description']][[1]])
    }
    df <- data.frame(cbind(tablenames,desc))
    names(df) <- c('FileName', 'Description')
  }
  return(df)  
}

#------------------------------------------------------------------------------

#' Displays a list of variables in the specified NHANES table.
#' 
#' If truncated = TRUE then only the field names and field descriptions are returned.
#' 
#' @importFrom stringr str_replace str_c str_sub
#' @param nh_surveygroup The type of survey (DEMOGRAPHIC, DIETARY, EXAMINATION, LABORATORY, QUESTIONNAIRE)
#' @param year The year in yyyy format where 1999 <= yyyy <= 2012
#' @param nh_table The name of the specific table to retrieve
#' @param truncated If true then only the variable names and descriptions are returned, which is often sufficient
#' @param nchar The number of characters in the Variable Description to print. Values are limited to 0<=nchar<=127.
#' This is used to enhance readability, cause variable descriptions can be very long.
#' @return The names of the tables in the specified survey group
#' @details Each data table contains multiple, sometimes more than 100, fields. It is helpful to list the field
#' descriptions to ascertain quickly if a data table is of interest.
#' @examples
#' nhanesTableVars('EXAM', 2007, 'BPX_E')
#' nhanesTableVars('EXAM', 2007, 'OHX_E', truncated=TRUE)
#' @export
#' 
nhanesTableVars <- function(nh_surveygroup, year, nh_table, truncated = FALSE, nchar=100) {
  if( !(nh_surveygroup %in% names(nhanes_group)) ) {
    stop("Invalid survey group")
    return(NULL)
  }
  
  tablename <- str_c('vars_', nhanes_group[[nh_surveygroup]], '_', str_replace(get_nh_survey_years(year),'-','_'), collapse='')
  df <- do.call(data.frame, list(as.name(tablename)) )
  
  if(!(nh_table %in% df$Data.File.Name)) {
    stop('Table ', nh_table, ' not present in the ', year, ' ', nh_surveygroup, ' survey' )
    return(NULL)
  }
  
  nchar_max <- 300
  if(nchar > nchar_max) {
    nchar <- nchar_max
  }
  if( truncated == TRUE ) {
    df <- df[df$Data.File.Name == nh_table,1:2]
    df[[2]] <- str_sub(df[[2]],1,nchar)
  }
  else {
    df <- df[df$Data.File.Name == nh_table,]
    df[[2]] <- str_sub(df[[2]],1,nchar)
  }
  row.names(df) <- c(1:nrow(df))
  return(df)
}

#------------------------------------------------------------------------------

#' Download an XPT table and return as a data frame
#' @importFrom Hmisc sasxport.get
#' @importFrom stringr str_c
#' @param year The year in yyyy format where 1999 <= yyyy <= 2012
#' @param nh_table The name of the specific table to retrieve
#' @return The table is returned as a data frame
#' @details Currently, a table is downloaded in its entirety. Other than nhanesAttr, this is the
#' only function that actually accesses data from the nhanes site directly.
#' @examples 
#' nhanes(2007, 'BPX_E')
#' nhanes(2009, 'FOLATE_F')
#' @export
#' 
nhanes <- function(year, nh_table) {
  nht <- tryCatch({    
    url <- str_c(nhanesURL, get_nh_survey_years(year), '/', nh_table, '.XPT', collapse='')
    return(sasxport.get(url))
#    return(read.xport(url))
  },
  error = function(cond) {
    message(paste("Data set ", nh_table, " from year", year, "is not available"))
    message(url)
    return(NA)
  },
  warning = function(cond) {
    message(cond, '\n')    
  }  
  )
  return(nht)
}

#------------------------------------------------------------------------------

#' Returns the attributes of an NHANES data table
#' @importFrom Hmisc sasxport.get
#' @importFrom stringr str_c
#' @importFrom utils object.size
#' @param year  The year in yyyy format where 1999 <= yyyy <= 2012
#' @param nh_table The name of the specific table to retrieve
#' @return The following attributes are returned as a list \cr
#' nrow = number of rows \cr
#' ncol = number of columns \cr
#' names = name of each column \cr
#' unique = true if all SEQN values are unique \cr
#' na = number of 'NA' cells in the table \cr
#' size = total size of table in bytes \cr
#' types = data types of each column
#' @details nhanesAttr allows one to check the size and other charactersistics of a data table 
#' before importing into R. To retrieve these characteristics, the specified table is downloaded,
#' characteristics are determined, then the table is deleted.
#' @examples 
#' nhanesAttr(2007, 'BPX_E')
#' nhanesAttr(2009, 'FOLATE_F')
#' @export
#' 
nhanesAttr <- function(year, nh_table) {
  nht <- tryCatch({    
    url <- str_c(nhanesURL, get_nh_survey_years(year), '/', nh_table, '.XPT', collapse='')
#    tmp <- read.xport(url)
    tmp <- sasxport.get(url)
    nhtatt <- attributes(tmp)
    nhtatt$row.names <- NULL
    nhtatt$nrow <- nrow(tmp)
    nhtatt$ncol <- ncol(tmp)
    nhtatt$unique <- (length(unique(tmp$SEQN)) == nhtatt$nrow)
    nhtatt$na <- sum(is.na(tmp))
    nhtatt$size <- object.size(tmp)
    nhtatt$types <- sapply(tmp,class)
    rm(tmp)
    return(nhtatt)
  },
  error = function(cond) {
    message(paste("Data from year", year, "are not available"))
    return(NA)
  },
  warning = function(cond) {
    message(cond, '\n')    
  }  
  )
  return(nht)  
}

#------------------------------------------------------------------------------

#' Translates code to it's text value
#' 
#' Many of the NHANES data tables have encoded values. E.g. 1 = 'Male', 2 = 'Female'
#' We want to access the code as needed either by simply displaying the code translation,
#' or applying the translation directly to the table. E.g, for gender we may want to
#' translate the code (1,2) to the represented values (Male, Female).
#' 
#' Any coded column will have a specific data code, and many columns may share the same code translation.
#' @param nh_surveygroup The survey group that contains the columns to be translated.
#' @param colname The name of the column to translate
#' @return The code translation
#' @details Code translations are done internally in the nhanes package. Specifically, the translations
#' are indicated in the nhanesCodeMapXXXXX lists. 
#' @examples
#' nhanesTranslate('DEMO', 'DMQADFC')
#' nhanesTranslate('LAB', 'DCD030')
#' @export
#' 
nhanesTranslate <- function(nh_surveygroup, colname) {
  if(is.null(colname)) {
    message('Column name is required')
    return(0)
  }
  
  switch(nh_surveygroup,
         'DEMO' = NHANEScolumn <- nhanesTranslator[[nhanesCodeMapDemographics[[colname]]]],
         'DEMOGRAPHICS' = NHANEScolumn <- nhanesTranslator[[nhanesCodeMapDemographics[[colname]]]],
         'DIET' = NHANEScolumn <- nhanesTranslator[[nhanesCodeMapDietary[[colname]]]],
         'DIETARY' = NHANEScolumn <- nhanesTranslator[[nhanesCodeMapDietary[[colname]]]],
         'EXAM' = NHANEScolumn <- nhanesTranslator[[nhanesCodeMapExamination[[colname]]]],
         'EXAMINATION' = NHANEScolumn <- nhanesTranslator[[nhanesCodeMapExamination[[colname]]]],
         'LAB' = NHANEScolumn <- nhanesTranslator[[nhanesCodeMapLaboratory[[colname]]]],
         'LABORATORY' = NHANEScolumn <- nhanesTranslator[[nhanesCodeMapLaboratory[[colname]]]],
         'Q' = NHANEScolumn <- nhanesTranslator[[nhanesCodeMapQuestionnaire[[colname]]]],
         'QUESTIONNAIRE' = NHANEScolumn <- nhanesTranslator[[nhanesCodeMapQuestionnaire[[colname]]]],
         stop("Invalid survey group")
  )
  
  if(is.null(NHANEScolumn)) {
    message('Column is either not present or does not need translation. Returning NULL')
    return(NHANEScolumn)
  }
  else {
    translated <- as.data.frame(unlist(NHANEScolumn))
    names(translated) <- 'Translation'
    return(translated)
  } 
}

#------------------------------------------------------------------------------
