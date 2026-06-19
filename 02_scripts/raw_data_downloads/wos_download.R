#Web of Science Query and Download
# Dominique Maucieri

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(here)
library(synthesisr)
library(readr)


API_KEY <- "81c1aeb12f41f166431e5187f7730f2402f1b24c" 


keywords_1 <- "(species OR population* OR communit* OR assemblage* OR ecological)"
keywords_2 <- "(richness* OR abundance* OR trend* OR diversit* OR composition* OR respon*)"
TS_BLOCK   <- paste0("TS=(", keywords_1, " AND ", keywords_2, ")")

if (file.exists(here("01_data/inital_raw_database_downloads/Web_of_Science",
                     "download_progress_ledger.csv"))) {
  ledger <- read.csv(here("01_data/inital_raw_database_downloads/Web_of_Science",
                          "download_progress_ledger.csv"), stringsAsFactors = FALSE)
} else {
  years_to_process <- 1700:2035
  
  ledger <- data.frame(
    year = years_to_process,
    expected_count = NA_integer_,
    downloaded_count = 0,
    status = "Pending",
    stringsAsFactors = FALSE
  )
  
  for (yr in years_to_process) {
    cat(sprintf("Checking targets for year %d... ", yr))
    q_str <- sprintf("PY=%d AND %s", yr, TS_BLOCK)
    
    # Direct inline API check for the year count
    params <- list('db' = 'WOS', 'limit' = 1, 'q' = q_str, 'page' = 1)
    res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", query = params, add_headers('X-ApiKey' = API_KEY))
    
    if (status_code(res) == 429) {
      Sys.sleep(30)
      res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", query = params, add_headers('X-ApiKey' = API_KEY))
    }
    
    clean_count <- 0
    if (status_code(res) == 200) {
      content_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
      if (!is.null(content_json$metadata$total)) clean_count <- as.numeric(content_json$metadata$total)
    }
    
    ledger$expected_count[ledger$year == yr] <- clean_count
    cat(sprintf("%s papers discovered.\n", format(clean_count, big.mark=",")))
    Sys.sleep(1.5)
  }
  
  ledger_no0 <- ledger %>% 
    filter(expected_count > 0) %>%
    mutate(status = if_else(expected_count <= downloaded_count, "Completed", "Pending"))
  
  write.csv(ledger_no0, here("01_data/inital_raw_database_downloads/Web_of_Science",
                         "download_progress_ledger.csv"), row.names = FALSE)
  
  ledger <- read.csv(here("01_data/inital_raw_database_downloads/Web_of_Science",
                             "download_progress_ledger.csv"), stringsAsFactors = FALSE)
}



years_to_process <- ledger$year

for (yr in years_to_process) {
  
  full_output_path <- file.path(here("01_data/inital_raw_database_downloads/Web_of_Science/separated", 
                                     sprintf("wos_export_year_%04d.csv", yr)))

  row_idx <- which(ledger$year == yr)
  current_status <- ledger$status[row_idx]
  exp_count <- ledger$expected_count[row_idx]
  
  if(file.exists(full_output_path) && current_status == "Completed") {
    cat(sprintf(">>> Year %d already completed. Skipping.\n", yr))
    next
  }
  
  cat(sprintf("\n--- Starting WoS Extraction Pipeline for Year: %d ---\n", yr))
  
  # if (exp_count >= 5000) {
  #   cat(sprintf(">>> Year %d has %s records (>= 5000). Skipping for now as requested.\n", yr, format(exp_count, big.mark=",")))
  #   next
  # } else {
    
    full_path <- file.path(here("01_data/inital_raw_database_downloads/Web_of_Science/separated", 
                                sprintf("wos_export_year_%04d.csv", yr)))
    
    q_str <- sprintf("PY=%d AND %s", yr, TS_BLOCK)
    params <- list('db' = 'WOS', 'limit' = 50, 'q' = q_str, 'page' = 1)
      
    res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", 
               query = params, add_headers('X-ApiKey' = API_KEY))
    
    if (status_code(res) == 429) {
      stop(" API Rate Limit (429) encountered on initial request. Stopping script execution for the day.")
    }
    
    if (status_code(res) == 200) {
      initial_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
      docs_found   <- as.numeric(initial_json$metadata$total)
      
      if (docs_found > 0) {
        reqs_needed <- ((docs_found - 1) %/% 50) + 1
        all_records <- list()
        if (!is.null(initial_json$hits)) all_records[[1]] <- initial_json$hits
        
        page_num <- 2
        while (page_num <= reqs_needed) {
          cat(sprintf("   Downloading page %d of %d...\n", page_num, reqs_needed))
          params$page <- page_num
          page_res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", query = params, add_headers('X-ApiKey' = API_KEY))
          
          # If it throws a 429, stop for the day
          if (status_code(page_res) == 429) {
            stop(" API Rate Limit (429) encountered during pagination. Stopping script execution for the day.")
          }
          
          if (status_code(page_res) == 200) {
            p_json <- fromJSON(content(page_res, as = "text", encoding = "UTF-8"), flatten = TRUE)
            if (!is.null(p_json$hits)) all_records[[page_num]] <- p_json$hits
          }
          page_num <- page_num + 1
          Sys.sleep(0.4)
        }
        
        combined_data <- bind_rows(all_records)
        
        if (nrow(combined_data) > 0) {
          final_df_flat <- combined_data %>%
            mutate(
            authors = map_chr(names.authors, function(cell) {
                if (is.null(cell) || length(cell) == 0) return(NA_character_)
                paste(as.character(unlist(cell)), collapse = "; ")
              })
            ) %>%
            mutate(across(where(is.list), ~ map_chr(.x, function(cell) {
              if (is.null(cell) || length(cell) == 0) return(NA_character_)
              paste(as.character(unlist(cell)), collapse = "; ")
            }))) %>%
            select(any_of(c(
              id = "uid",
              doi = "identifiers.doi",
              title = "title",
              authorships = "names.authors",
              publication_year = "source.publishYear",
              type = "types",
              issn = "identifiers.issn",
              journal = "source.sourceTitle",
              volume = "source.volume",
              issue = "source.issue",
              pages = "source.pages.range",
              citations = "citations"
            )))
          
          write.csv(final_df_flat, full_path, row.names = FALSE)
          ledger$downloaded_count[row_idx] <- nrow(final_df_flat)
        }
      }
    }
  # }
  
  ledger <- ledger %>%
    mutate(status = if_else(expected_count <= downloaded_count, "Completed", "Pending"))
  
  write.csv(ledger, here("01_data/inital_raw_database_downloads/Web_of_Science",
                         "download_progress_ledger.csv"), row.names = FALSE)
  Sys.sleep(1.5)
}




all_csv_files <- list.files(
    path = here("01_data/inital_raw_database_downloads/Web_of_Science/separated"), 
    pattern = "\\.csv$", 
    full.names = TRUE
  ) %>%
    map_df(~read_csv(.x, col_types = cols(.default = "c"), show_col_types = FALSE))

if (nrow(all_csv_files) > 0 && "id" %in% names(all_csv_files)) {
  all_csv_files <- all_csv_files %>% distinct(id, .keep_all = TRUE)
}

write.csv(all_csv_files, 
          file.path(here("01_data/inital_raw_database_downloads/Web_of_Science"), 
                    "raw_full_wos_export.csv"), row.names = FALSE)


