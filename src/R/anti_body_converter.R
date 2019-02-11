antibody_fetch <- function(study="SDY80",type="neut_ab_titer") {
  
    library(forcats)
    library(ImmuneSpaceR)
    library(tidyr)
    
    # This uses the Puthon code sample Shuzhao placed at 
    #  https://storage.cloud.google.com/immport/Shuzhao_example_SDY80/README.txt
    # 
    
    tmp <- CreateConnection(study)
    df  <- tmp$getDataset(type)
    virus <- df$virus
    unique(virus)
    
    # So as with the fcs data we want to create a variable called observation_id that is based on a combination of
    # the participant id and time point. 
    
    ids <- sapply(strsplit(df$`participant_id`,"\\."),`[`,1)
    observation_id <- gsub(" ","",
                           paste(ids,format(df$`study_time_collected`,nsmall=1),
                                 sep="_"))
    
    # Create a new data frame that has the observation_id as the first column. 
    
    df.2 <- data.frame(cbind(observation_id=observation_id,df),stringsAsFactors = FALSE)
    
    
    # Pick out the desired row names as specified by the code sample
    # https://storage.cloud.google.com/immport/Shuzhao_example_SDY80/README.txt
    
    df.3 <- df.2[,c("observation_id","participant_id","age_reported","gender","race","cohort",
                    "study_time_collected","study_time_collected_unit")]
    
    # Get just the the non duplicated rows
    
    df.4 <- df.3[!duplicated(df.3),]
    
    # Write this out as the annotation file
    
    anno.out <- paste(study,type,sep="_")
    
    anno.out <- paste(anno.out,"annotation.txt",sep="_")
    
    write_tsv(df.4,path=anno.out)
    
    # Now let's write out the matrix portion. We will use df.2 since it has all the data
    # Now, the code at https://storage.cloud.google.com/immport/Shuzhao_example_SDY80/README.txt 
    # wants to collape the virus names into a normalized set of four names. We can use the forcats
    # package to simplify this. 
    
    # Make virus a factor which is required by the forcats package
    
    df.2$virus <- as.factor(df.2$virus)
    
    # Normalize Virus strains by collapsing the existing virus names into a smaller set.
    # Notice that this specific to the study and data type and would need to be adjusted so
    # this code will fail for example if you tried to access SDY180 hai data
    
    virus.tmp <- fct_collapse(df.2$virus,
                              Brisbane_A=c("A/Brisbane/59/2007"),
                              California=c("A/California/7/2009","A_Ca_07_swine"),
                              Uruguay=c("A/Uruguay/716/2007","A_Uruguay_716_2007"),
                              Brisbane_B=c("B/Brisbane/60/2008","B_Brisbane_60_2001"))
    
    # Create a new column at the end of the data frame with the newly collpsed normalized virus names
    
    df.2$neut_ab_titer <- virus.tmp
    
    # Now we want to create a table of the virus names by the observation ideas and then sum the 
    # value_preferred column
    
    df.2 %>% group_by(neut_ab_titer,observation_id) %>% summarize(sum=sum(value_preferred))
    
    out <- df.2 %>% group_by(neut_ab_titer,observation_id) %>% 
      summarize(sum=sum(value_preferred)) %>% 
      spread(observation_id,sum)
  
    matrix.out <- paste(study,type,sep="_")
    matrix.out <- paste(matrix.out,"data_matrix.txt",sep="_")
    write_tsv(out,matrix.out)
    return(out)
}
