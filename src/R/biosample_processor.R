get_biosample <- function(study="SDY80",output_folder="~/Downloads",host="35.229.69.7") {
#
# Function to get biosample.txt files for an arbitrary ImmPort study 
# Example: 
#
# > studies <- c("SDY80","SDY112")
# > sapply(studies,get_biosample(x))
#

  library(RMySQL)
  library(dplyr)
  library(readr)
  
  # Connect to our Google study base
  
  con <- dbConnect(RMySQL::MySQL(),user='study', 
                   password='allstudies', 
                   dbname='shared_data', 
                   host=host)
  
  # dbplyr does lazy connections so it doesn't actually fetch the data at first
  
  biosample <- tbl(con,"biosample")
  fname <- paste(study,"biosample.tsv",sep="_")
  fname <- paste(output_folder,fname,sep="/")
  
  # Because dbplyr does lazy evaluation we have to use the collection 
  # function to force it to pull the data out of the data base
  
  write_tsv(collect(biosample),fname)
  msg <- paste0("Processed ",study," to filename: ",fname)
  print(msg)
  dbDisconnect(con)
}