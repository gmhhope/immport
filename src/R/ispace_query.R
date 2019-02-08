# Put the study names you want here.

# Note that SDY311, SDY739, SDY111, SDY1288 are not recognized by ImmuneSpace

study.names <- c("SDY63","SDY80","SDY112","SDY144","SDY180",
                 "SDY212","SDY269","SDY296","SDY299","SDY301",
                 "SDY312","SDY314","SDY315","SDY364","SDY368","SDY387",
                 "SDY400","SDY404","SDY520","SDY522")

data.types <- c("elisa","elispot",
                "fcs_analyzed_result",
                "hai",
                "neut_ab_titer",
                "gene_expression_files")


data.explorer <- function(studies=study.names,data_types=data.types,write=TRUE) {
#
# Example: 
# > outmat <- data.explorer(studies=study.names,data_types=data.types,write=TRUE)
#
# Function to query ImmuneSpace using one or more stydy names
# for each study figure out if it has data types as specified by
# the data_types vector. This is a quick and dirty way to do this.
# A better way would be to get the datasets on offer and capture them
# to an outputfile and then read through that instead of querying the
# server. But we really only need to do this once or twice. 
#
# INPUT: studies - a vector containing valid study names on ImmPort/ImmuneSpace
#        data_types - data types of interest
#        write - a boolean value that if TRUE (default) it will write out the 
#                matrix/data frame for importing into Excel 
#
# OUTPUT: om - a data frame where number of rows is length(studies)  
#              and number of columns is length(data_types)
#              Elements will have a "Y" when that study name has a dataset
#              of tyoe data_type. If not, there will be a "N"
#
  
    library(ImmuneSpaceR)
    library(dplyr)
    library(readr)
    
    # Create a matrix whose row names are the studies and the column names
    # are the data_types
    
    outmat <- matrix("Y",nrow=length(studies),ncol=length(data_types))
    
    row.names(outmat) <- studies
    colnames(outmat) <- data_types
    
    # For each study (row) - connect to ImmuneSpace
    for (ii in 1:length(studies)) {
      tmp <- CreateConnection(studies[ii])
      
      # Now for each study type try to see if that data type exists
      # If it does then put a "Y" in the correspinding element. If not
      # then put "N"
      
      for (jj in 1:length(data_types)) {
        print(paste("Processing",data_types[jj],sep=" "))
        ds <- tmp$getDataset(data_types[jj])
        print(nrow(ds))
        if (nrow(ds) == 0) {
          outmat[ii,jj] = "N"
        } else {
          outmat[ii,jj] = "Y"
        }
     }
    }
    
    # Okay, let's write out the file which most users will want to read using
    # Excel so let's make it tab delimited. Also, we have to do some conversion
    # on the matrix into a data frame.
    
    om <- data.frame(outmat,stringsAsFactors = FALSE)
    om <- cbind(study=rownames(om),om)
    rownames(om) <- NULL
    
    if (write) {
      write_tsv(om,path="studies_by_data_types.tsv")
    }
    return(om)
}


tm <- data.frame(tmat,stringsAsFactors = FALSE)
tm <- cbind(study=rownames(tm),tm)
rownames(tm) <- NULL