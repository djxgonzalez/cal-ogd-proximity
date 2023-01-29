##============================================================================##
## 1.01 - cleans and prepares raw ACS input data for further analysis

# data inputs to function must be processed in Excel as recommended by Census:
# https://www2.census.gov/programs-surveys/acs/summary_file/2019/documentation/tech_docs/ACS_SF_Excel_Import_Tool.pdf

tidyACSData <- function(data_in, head) {
  
  # replaces spaces in column names with periods
  names(data_in) <- str_replace_all(names(data_in), c(" " = "."))
  
  # extracts only block group rows
  data <- data_in[grepl("Block Group", data_in$Geography.Name), ]
  
  # extracts LOGRECNO, GEOID, Geography.Name, and columns matching the detailed table code
  start <- data[, c(7:9)]
  detailed_table <- data[, grepl(head, names(data))]
  
  data <- cbind(start, detailed_table)
  
  # returns processed dataset
  return(data)
}

##============================================================================##

