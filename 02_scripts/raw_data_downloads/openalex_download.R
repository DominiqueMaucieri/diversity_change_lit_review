#OpenAlex Query and Download
# Dominique Maucieri

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(here)
library(openalexR)
library(readr)

options(openalexR.api_key = "Y0GWLrpc864Y8HFAyDFrAB")

# targeted search parameters
keywords_1 <- "(species OR population OR community OR assemblage OR ecological)"
keywords_2 <- "(richness OR abundance OR trend OR diversity OR composition OR response)"
search_query <- paste(keywords_1, "AND", keywords_2)


# importing the ledger or creating if it doesnt already exist
if (file.exists(here("01_data/inital_raw_database_downloads/OpenAlex",
                     "download_progress_ledger.csv"))) {
  
  ledger <- read.csv(here("01_data/inital_raw_database_downloads/OpenAlex",
                          "download_progress_ledger.csv"), stringsAsFactors = FALSE)
} else {
  
  years_to_process <- 1700:2035
  
  ledger <- data.frame(
    year = years_to_process,
    expected_count = NA_integer_,
    downloaded_count = NA_integer_,
    status = "Pending",
    stringsAsFactors = FALSE
  )
  
  for (yr in years_to_process) {
    cat(sprintf("Checking count for year %d... ", yr))
    
    raw_count <- tryCatch({
      oa_fetch(
        entity = "works",
        title_and_abstract.search = search_query,
        publication_year = yr,
        count_only = TRUE,
        verbose = FALSE 
      )
    }, error = function(e) {
      return(NA)
    })
    
    clean_count <- tryCatch({
      val <- as.numeric(unlist(raw_count)[1])
      if(is.na(val) || length(val) == 0) 0 else as.integer(val)
    }, error = function(e) {
      0
    })
    
    ledger$expected_count[ledger$year == yr] <- clean_count
    cat(sprintf("%s papers found.\n", format(clean_count, big.mark=",")))
    
    Sys.sleep(0.5) 
  }
  
  
  ledger_no0s <- ledger %>%
    filter(expected_count > 0) %>%
    as.data.frame() %>%
    mutate(year = as.integer(unlist(.$year)),
           expected_count = as.integer(unlist(.$expected_count)),
           downloaded_count = as.integer(unlist(.$downloaded_count)),
           status = as.character(unlist(.$status))) %>%
    mutate(status = if_else(expected_count <= downloaded_count, "Completed",  "Pending"))
  
  write.csv(ledger_no0s, 
            file.path(here("01_data/inital_raw_database_downloads/OpenAlex"), 
                      "download_progress_ledger.csv"), 
            row.names = FALSE)
}

years_to_process <- ledger$year


# now looking through the years
for (yr in years_to_process) {
  
  output_filename  <- sprintf("openalex_export_year_%04d.csv", yr)
  full_output_path <- file.path(here("01_data/inital_raw_database_downloads/OpenAlex/separated", 
                                     output_filename))
  
  # Fetch current state details from the ledger matrix
  row_idx <- which(ledger$year == yr)
  current_status <- ledger$status[row_idx]
  exp_count <- ledger$expected_count[row_idx]
  
  # SKIP CRITERIA: Skip if file physically exists AND ledger confirms completion
  if (file.exists(full_output_path) && current_status == "Completed") {
    cat(sprintf(">>> Year %d already completed. Skipping.\n", yr))
    next
  }
  
  cat(sprintf("\n--- Starting Fetch for Year: %d ---\n", yr))
  
  raw_results <- list()
  
  combined_data <- tryCatch({
    oa_fetch(
      entity = "works",
      title_and_abstract.search = search_query,
      publication_year = yr,
      verbose = TRUE
    )
  }, error = function(e) {
    if (grepl("429", e$message) || grepl("Too Many Requests", e$message, ignore.case = TRUE)) {
      cat(sprintf("\n RATE LIMIT HIT (429) during Year %d! API has throttled our requests.\n", yr))
      return("TRIGGER_RATE_LIMIT_BREAK")
    }
    
    cat(sprintf("ERROR: Fetch execution broken for year %d: %s\n", yr, e$message))
    return(data.frame())
  })
  
  # Act on the 429 trigger to stop the entire loop immediately
  if (identical(combined_data, "TRIGGER_RATE_LIMIT_BREAK")) {
    cat("Safely stopping execution to preserve API key standing. Resume tomorrow!\n")
    break
  }

  
  final_df_flat <- combined_data %>%
    mutate(authorships = map_chr(authorships, function(auth_df) {
      if (is.null(auth_df) || nrow(auth_df) == 0) return(NA_character_)
      # Extract only the display_name column and paste them together with semicolons
      paste(auth_df$display_name, collapse = "; ") })) %>%
    select(
      any_of(c(
        "id", 
        "doi", 
        "title", 
        "authorships", 
        "publication_year", 
        "publication_date", 
        "language", 
        "type", 
        "abstract" 
      ))
    )
  
  write.csv(final_df_flat, full_output_path, row.names = FALSE)
  
  # Record tracking milestones into our flattened progress state ledger
  ledger$downloaded_count[row_idx] <- nrow(final_df_flat)
  ledger <- ledger %>%
    mutate(status = if_else(expected_count <= downloaded_count, "Completed",  "Pending"))
  write.csv(ledger, 
            file.path(here("01_data/inital_raw_database_downloads/OpenAlex"), 
                      "download_progress_ledger.csv"), 
            row.names = FALSE)
  
  ledger <- read.csv(here("01_data/inital_raw_database_downloads/OpenAlex",
                          "download_progress_ledger.csv"), stringsAsFactors = FALSE)
  
  # Polite API cool-down interval
  Sys.sleep(1.5)
}




all_csv_files <- list.files(
  path = here("01_data/inital_raw_database_downloads/OpenAlex/separated"), 
  pattern = "^openalex_export_year_\\d{4}\\.csv$", 
  full.names = TRUE
) %>%
  map_df(~read_csv(.x, col_types = cols(.default = "c"), show_col_types = FALSE)) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(
    publication_year = as.integer(publication_year),
    publication_date = as.Date(publication_date)
  )



write.csv(all_csv_files, 
          file.path(here("01_data/inital_raw_database_downloads/OpenAlex"), 
                    "raw_full_openalex_export.csv"), 
          row.names = FALSE)

