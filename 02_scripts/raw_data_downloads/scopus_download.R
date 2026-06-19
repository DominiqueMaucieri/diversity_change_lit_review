#Scopus Query and Download
# Dominique Maucieri

library(rscopus)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(here)
library(readr)


set_api_key("a20af0da26bc953a4a7bd0dc39e8b684")
scopus_api_key <- "a20af0da26bc953a4a7bd0dc39e8b684"

api_headers <- c(
  `X-ELS-APIKey`    = scopus_api_key,
  `Accept`          = "application/json"
)

# Targeted search parameters
keywords_1 <- "(species OR population OR community OR assemblage OR ecological)"
keywords_2 <- "(richness OR abundance OR trend OR diversity OR composition OR response)"
base_query <- paste0("TITLE-ABS-KEY(", keywords_1, " AND ", keywords_2, ")")



# either build or import the ledger
if (file.exists(here("01_data/inital_raw_database_downloads/Scopus", "download_progress_ledger.csv"))) {
  ledger <- read.csv(here("01_data/inital_raw_database_downloads/Scopus", 
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
    cat(sprintf("Checking Scopus baseline count for year %d... ", yr))
    
    query_str <- paste0("(", base_query, ") AND PUBYEAR = ", yr)
    
    # Use native rscopus tool to check expected counts safely
    raw_count <- tryCatch({
      res <- scopus_search(query = query_str, count = 1, view = "STANDARD")
      as.integer(res$total_results)
    }, error = function(e) {
      return(NA_integer_)
    })
    
    if (is.na(raw_count)) {
      cat("\nPossible API limit or connection issue. Stopping initialization.\n")
      break
    }
    
    ledger$expected_count[ledger$year == yr] <- raw_count
    cat(sprintf("%s papers found.\n", format(raw_count, big.mark=",")))
    Sys.sleep(0.5)
  }
  
  ledger_no0s <- ledger %>% 
    filter(expected_count > 0)
  write.csv(ledger_no0s, here("01_data/inital_raw_database_downloads/Scopus", 
                              "download_progress_ledger.csv"), row.names = FALSE)
  
  ledger <- read.csv(here("01_data/inital_raw_database_downloads/Scopus", 
                          "download_progress_ledger.csv"), stringsAsFactors = FALSE)
}

years_to_process <- ledger$year

# now loop through the years
for (yr in years_to_process) {
  
  full_output_path <- file.path(here("01_data/inital_raw_database_downloads/Scopus/separated", 
                                     sprintf("scopus_export_year_%04d.csv", yr)))
  
  row_idx <- which(ledger$year == yr)
  current_status <- ledger$status[row_idx]
  exp_count <- ledger$expected_count[row_idx]
  
  if(file.exists(full_output_path) && current_status == "Completed") {
    cat(sprintf(">>> Year %d already completed. Skipping.\n", yr))
    next
  }
  
  cat(sprintf("\n--- Starting Scopus Fetch for Year: %d ---\n", yr))
  
  full_query <- sprintf("(%s) AND PUBYEAR = %d", base_query, yr)
  
  raw_results <- list()
  next_cursor <- "*"   
  has_more <- TRUE
  page_count <- 1
  rate_limit_triggered <- FALSE
  
  while (has_more) {
    # Print clean progress status updates every 5 pages or on edge items
    if (page_count == 1 || page_count %% 5 == 0) {
      cat(sprintf("     Streaming page %d... (Cursor: %s)\n", page_count, substr(next_cursor, 1, 15)))
    }
  
    request_url <- sprintf(
      "https://api.elsevier.com/content/search/scopus?query=%s&count=200&cursor=%s&view=STANDARD",
      URLencode(full_query, reserved = TRUE),
      URLencode(next_cursor, reserved = TRUE)
    )
    
    # Raw API Execution via HTTP GET
    response <- tryCatch({
      GET(request_url, add_headers(.headers = api_headers))
    }, error = function(e) { NULL })
    
    if (is.null(response)) {
      cat(" Network failure encountered. Retrying in 5 seconds...\n")
      Sys.sleep(5)
      next
    }
    
    status <- status_code(response)
    
    if (status == 429) {
      rate_limit_triggered <- TRUE
      break
    } else if (status == 403) {
      cat("\n HTTP 403 Forbidden\n")
      stop("Execution terminated due to authentication rejection.")
    } else if (status != 200) {
      cat(sprintf("\n Server returned unexpected HTTP error %d. Pausing process.\n", status))
      break
    }
    
    raw_json <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    entries  <- raw_json$`search-results`$entry
    
    if (!is.null(entries) && is.data.frame(entries) && nrow(entries) > 0) {
      raw_results[[length(raw_results) + 1]] <- entries
    }
    
    links <- raw_json$`search-results`$link
    next_link_row <- links %>% filter(tolower(`@ref`) == "next")
    
    if (nrow(next_link_row) > 0) {
      next_url <- next_link_row$`@href`[1]
      parsed_cursor <- sub(".*cursor=([^&]+).*", "\\1", next_url)
      
      # If the cursor pointer does not cycle forward, we have completely drained the target year
      if (parsed_cursor == next_cursor) {
        has_more <- FALSE
      } else {
        next_cursor <- parsed_cursor
        page_count  <- page_count + 1
      }
    } else {
      has_more <- FALSE
    }
    
    Sys.sleep(0.35) # Safe backend hit buffer
  }
  
  if (rate_limit_triggered) {
    cat(sprintf("\n Quota / Rate Limit Hit (429) during Year %d! Saving tracking state and exiting.\n", yr))
    break
  }

  
  final_df_flat <- bind_rows(raw_results) %>%
    distinct() %>%
    mutate(
      id = if ("dc:identifier" %in% names(.)) `dc:identifier` else NA_character_,
      eid = if ("eid" %in% names(.)) eid else NA_character_,
      doi = if ("prism:doi" %in% names(.)) `prism:doi` else NA_character_,
      title = if ("dc:title" %in% names(.)) `dc:title` else NA_character_,
      authorships = if ("dc:creator" %in% names(.)) `dc:creator` else NA_character_,
      publication_date = if ("prism:coverDate" %in% names(.)) `prism:coverDate` else NA_character_,
      type = if ("subtypeDescription" %in% names(.)) subtypeDescription else NA_character_,
      issn = if ("prism:issn" %in% names(.)) `prism:issn` else NA_character_,
      volume = if ("prism:volume" %in% names(.)) `prism:volume` else NA_character_,
      issue = if ("prism:issueIdentifier" %in% names(.)) `prism:issueIdentifier` else NA_character_,
      pages = if ("prism:pageRange" %in% names(.)) `prism:pageRange` else NA_character_
    ) %>%
    mutate(
      publication_year = as.integer(substr(as.character(publication_date), 1, 4))
    ) %>%
    select(
      any_of(c("id", "eid", "doi", "title", "authorships", "publication_year", "publication_date", "type", "issn", "volume", "issue", "pages"))
    )
  
  # Save individual year spreadsheet
  write.csv(final_df_flat, full_output_path, row.names = FALSE)
  
  # Update and save ledger records
  ledger$downloaded_count[row_idx] <- nrow(final_df_flat)
  ledger$status[row_idx] <- if_else(nrow(final_df_flat) >= exp_count, "Completed", "Pending")
  write.csv(ledger, here("01_data/inital_raw_database_downloads/Scopus", 
                         "download_progress_ledger.csv"), row.names = FALSE)
  
  Sys.sleep(1.5)
}




# combine all together

all_csv_files <- list.files(path = SEP_DIR, pattern = "^scopus_export_year_\\d{4}\\.csv$", full.names = TRUE) %>%
  map_df(~read_csv(.x, col_types = cols(.default = "c"), show_col_types = FALSE)) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(
    publication_year = as.integer(publication_year),
    publication_date = as.Date(publication_date)
  )

write.csv(all_csv_files, file = here("01_data/inital_raw_database_downloads/Scopus", 
                                     "raw_full_scopus_export.csv"), row.names = FALSE)
