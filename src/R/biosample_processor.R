get_biosample <- function(studies="SDY80",output_folder="~/Downloads") {
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
  
  # The IP number of our MySQL databaase on the GCP 
  
  host <- "35.229.69.7"
  
  # Connect to our Google study base
  
  con <- dbConnect(RMySQL::MySQL(),user='study', 
                   password='allstudies', 
                   dbname='shared_data', 
                   host=host)
  
  for (ii in 1:length(studies)) { 
    
    # dbplyr does lazy connections so it doesn't actually fetch the data at first
    
    biosample <- tbl(con,"biosample")
    fname <- paste(studies[ii],"biosample.txt",sep="_")
    fname <- paste(output_folder,fname,sep="/")
    
    # Because dbplyr does lazy evaluation we have to use the collection 
    # function to force it to pull the data out of the data base
    
    write_tsv(collect(biosample),fname)
    msg <- paste0("Processed ",studies[ii]," to filename: ",fname)
    print(msg)
  }
  dbDisconnect(con)
}