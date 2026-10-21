#Web of Science Query and Download
# Dominique Maucieri

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(here)
library(synthesisr)
library(readr)


source(here("", "my_keys.R"))

keywords_1 <- "(species OR population* OR communit* OR assemblage* OR ecological)"
keywords_2 <- "(richness OR abundan* OR count* OR cover* OR densit* OR biomass* OR diversit* OR composition*)"
keywords_3 <-"(respon* OR trend* OR impact* OR outcome* OR chang* OR effect*)"
TS_BLOCK   <- paste0("TS=(", keywords_1, " AND ", keywords_2, " AND ", keywords_3, ")")


if (file.exists(here("01_data/inital_raw_database_downloads/Web_of_Science",
                     "wos_download_progress_ledger.csv"))) {
  ledger <- read.csv(here("01_data/inital_raw_database_downloads/Web_of_Science",
                          "wos_download_progress_ledger.csv"), stringsAsFactors = FALSE)
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
    res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", query = params, add_headers('X-ApiKey' = WOS_API_KEY))
    
    if (status_code(res) == 429) {
      Sys.sleep(30)
      res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", query = params, add_headers('X-ApiKey' = WOS_API_KEY))
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
    mutate(status = if_else(expected_count == downloaded_count, "Completed", "Pending"),
           download_date = as.Date(NA))
  
  write.csv(ledger_no0, here("01_data/inital_raw_database_downloads/Web_of_Science",
                             "wos_download_progress_ledger.csv"), row.names = FALSE)
  
  ledger <- read.csv(here("01_data/inital_raw_database_downloads/Web_of_Science",
                          "wos_download_progress_ledger.csv"), stringsAsFactors = FALSE)
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
  
  q_str <- sprintf("PY=%d AND %s", yr, TS_BLOCK)
  params <- list('db' = 'WOS', 'limit' = 1, 'q' = q_str, 'page' = 1)
  res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", query = params, add_headers('X-ApiKey' = WOS_API_KEY))
  content_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
  clean_count <- as.numeric(content_json$metadata$total)
  ledger$expected_count[ledger$year == yr] <- clean_count
  exp_count <- ledger$expected_count[row_idx]
  
  
  
  
  
  cat(sprintf("\n--- Starting WoS Extraction Pipeline for Year: %d ---\n", yr))
  
  full_path <- file.path(here("01_data/inital_raw_database_downloads/Web_of_Science/separated", 
                              sprintf("wos_export_year_%04d.csv", yr)))
  
  if (exp_count >= 50000) {
    cat(sprintf(">>> Year %d has %s records (>= 50000). Will download in month segments.\n", yr, format(exp_count, big.mark=",")))
    
    
    start_dates <- c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01", 
                     "07-01", "08-01", "09-01", "10-01", "11-01", "12-01")
    end_dates   <- c("01-31", "03-01", "03-31", "04-30", "05-31", "06-30", 
                     "07-31", "08-31", "09-30", "10-31", "11-30", "12-31")
    
    q_str_months <- sprintf(
      "((PY=(%d)) AND %s) AND DOP=(%d-%s/%d-%s)",
      yr, TS_BLOCK, yr, start_dates, yr, end_dates
    )
    
    combined_data <- data.frame()
    
    for (q_str in q_str_months) {
      
      params <- list('db' = 'WOS', 'limit' = 50, 'q' = q_str, 'page' = 1)
      
      res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", 
                 query = params, add_headers('X-ApiKey' = WOS_API_KEY))
      
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
            page_res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", query = params, add_headers('X-ApiKey' = WOS_API_KEY))
            
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
          
          combined_data_i <- bind_rows(all_records)
          
        }
      }
      
      combined_data <- bind_rows(combined_data, combined_data_i) %>%
        distinct()
      
      Sys.sleep(0.4)
      
    }
    
    #now early and late access 
    
    q_str_alt_years <- c((yr-10):(yr-1), (yr+1):(yr+10))
    
    q_str_years <- sprintf(
      "((PY=(%d AND %d)) AND %s)",
      q_str_alt_years, yr, TS_BLOCK
    )
    
    for (q_str in q_str_years) {
      
      params <- list('db' = 'WOS', 'limit' = 50, 'q' = q_str, 'page' = 1)
      
      res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", 
                 query = params, add_headers('X-ApiKey' = WOS_API_KEY))
      
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
            page_res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", query = params, add_headers('X-ApiKey' = WOS_API_KEY))
            
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
          
          combined_data_i <- bind_rows(all_records)
          
        }
      }
      
      combined_data <- bind_rows(combined_data, combined_data_i) %>%
        distinct()
      
      Sys.sleep(0.4)
      
    }
    
  } else {
    
    q_str <- sprintf("PY=%d AND %s", yr, TS_BLOCK)
    params <- list('db' = 'WOS', 'limit' = 50, 'q' = q_str, 'page' = 1)
    
    res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", 
               query = params, add_headers('X-ApiKey' = WOS_API_KEY))
    
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
          page_res <- GET("https://api.clarivate.com/apis/wos-starter/v1/documents", query = params, add_headers('X-ApiKey' = WOS_API_KEY))
          
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
      }
    }
  }
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
    ledger$download_date[row_idx] <- Sys.Date()
  }
  
  ledger <- ledger %>%
    mutate(status = if_else(expected_count <= downloaded_count, "Completed", "Pending"))
  
  write.csv(ledger, here("01_data/inital_raw_database_downloads/Web_of_Science",
                         "wos_download_progress_ledger.csv"), row.names = FALSE)
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




#adding download dates manually as they werent in the loop originally

# for(i in 1:nrow(ledger)){
#   
#   full_output_path <- file.path(here("01_data/inital_raw_database_downloads/Web_of_Science/separated", 
#                                      sprintf("wos_export_year_%04d.csv", ledger$year[i])))
#   ledger$download_date[i] <- as.Date(file.info(full_output_path)$mtime)
# }
# 
# write.csv(ledger, here("01_data/inital_raw_database_downloads/Web_of_Science",
#                        "wos_download_progress_ledger.csv"), row.names = FALSE)
