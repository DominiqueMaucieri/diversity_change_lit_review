#### code to subset out a new set of data to use the model on
#### author: '[Dominique Maucieri](https://www.dominiquemaucieri.com)'


## need to supply:
# data_iteration - the iteration of training
if(!exists("data_iteration")) stop("missing data_iteration object")
data_iteration_char <- as.character(data_iteration)
data_iteration_num <- as.numeric(data_iteration)


# n_papers_to_select - number of how many papers to select
if(!exists("n_papers_to_select")) stop("missing n_papers_to_select object")
n_papers_to_select_num <- as.numeric(n_papers_to_select)



full_reference_list <- readLines(here("05_archived_examples/example_nlp" ,
                                      "wos_download_full.ris"), warn = FALSE)

previous_references <- 

#so the subsetting is reproducible but still random
sample_seed <- 783645 + data_iteration_num





# sample code below ***need to finish
example_references <- readLines(here("05_archived_examples/example_nlp" ,
                                     "wos_download_full.ris"), warn = FALSE)

all_references <- example_references

# all_references <- readLines(here("inital_raw_database_downloads" ,
#                                     "wos_download_full.ris"), warn = FALSE)

end_of_sample_indices <- which(all_references == "ER  -")

all_papers <- list()
start_idx <- 1

for (end_idx in end_of_sample_indices) {
  all_papers[[length(all_papers) + 1]] <- all_references[start_idx:end_idx]
  start_idx <- end_idx + 1
}

set.seed(756)
training_and_holdout_samples <- sample(all_papers, 
                                       size = number_of_training_samples + number_of_holdout_samples,
                                       replace = FALSE)

training_samples <- training_and_holdout_samples[1:number_of_training_samples]


training_output_lines <- unlist(training_samples)

writeLines(training_output_lines, 
           con = here("03_exported_data/initial_to_be_classified",
                      "initial_covidence_training.ris"), useBytes = TRUE)


holdout_samples <- training_and_holdout_samples[(number_of_training_samples +
                                                   1):(number_of_training_samples + 
                                                         number_of_holdout_samples)]

holdout_output_lines <- unlist(holdout_samples)

writeLines(holdout_output_lines, 
           con = here("03_exported_data/initial_to_be_classified",
                      "holdout_samples.ris"), useBytes = TRUE)
