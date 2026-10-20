# combining the cleaned raw data bases and removing duplicates
# Dominique Maucieri

library(here)
library(dplyr)

################################################################################
# combining Scopus
################################################################################

scopus_filenames  <- str_subset(list.files(here("data/Scopus/tidy_iterations",""), 
                                               pattern = "\\.csv$", full.names = TRUE), 
                                    pattern = fixed(".csv"))

scopus_files_list <- lapply(scopus_filenames, read.csv)

for (i in 1:length(scopus_filenames)){
  
  if (i == 1){
    scopus_references <- as.data.frame(scopus_files_list[[i]])
  } else{
    scopus_references <- rbind(scopus_references, as.data.frame(scopus_files_list[[i]]))
  }
}



################################################################################
# combining Web of Science
################################################################################

wos_filenames  <- str_subset(list.files(here("data/Web_of_Science/tidy_iterations",""), 
                                           pattern = "\\.csv$", full.names = TRUE), 
                                pattern = fixed(".csv"))

wos_files_list <- lapply(wos_filenames, read.csv)

for (i in 1:length(wos_filenames)){
  
  if (i == 1){
    wos_references <- as.data.frame(wos_files_list[[i]])
  } else{
    wos_references <- rbind(wos_references, as.data.frame(wos_files_list[[i]]))
  }
}



################################################################################
# combining OpenAlex
################################################################################

openalex_filenames  <- str_subset(list.files(here("data/OpenAlex/tidy_iterations",""), 
                                        pattern = "\\.csv$", full.names = TRUE), 
                             pattern = fixed(".csv"))

openalex_files_list <- lapply(openalex_filenames, read.csv)

for (i in 1:length(openalex_filenames)){
  
  if (i == 1){
    openalex_references <- as.data.frame(openalex_files_list[[i]])
  } else{
    openalex_references <- rbind(openalex_references, as.data.frame(openalex_files_list[[i]]))
  }
}


