******
Scripts and Data for:
# Literature Review examining how Biodiversity is being quantified. 

Code by Dominique Maucieri

********


### File Nomenclature

Filenames are made up of 3 parts

one\_two\_three.extension

- ```one```: where the data came from
	- cov = covidence
	- wos = raw web of science download
	- scop = raw scopus download
	- comb = combined WoS and scopus (all unlabelled)

	
- ```two```: iteration
	- whole number is the subset iteration
	- decimal is the training iteration for that subset
	- ex: 1.2 is the first subset of the full reference list, and the 2nd training iteration

	
- ```three```: subset of the data
	- irrelevant, relevant, excluded
	- labelled
	- unlabelled

	
# Workflow

## 1. Data downloads

- [wos_download.R](02_scripts/raw_data_downloads/wos_download.R) uses WoS API to download the titles using search string (~2.5m)
	- NOTE: will take 13 days to run ... and wont get the abstracts

- [scopus_download.R](02_scripts/raw_data_downloads/scopus_download.R) uses Scopus API to download titles and abstracts using the search string (~2.7m)

- [openalex_download.R](02_scripts/raw_data_downloads/openalex_download.R) uses OpenAlex API to download titles and abstracts using the search string (~ 3.9m)
	- NOTE: will take 8 days to run but will extract abstracts

## 2. Initial Training
	
- [training_dataset_creation.Rmd](02_scripts/training_dataset_creation.Rmd) subsets the full list of citaitons to get the inital training data set and the holdout subset. 

	- **add code to remove duplicates**

	
## 3. NLP iterations



## 4. Examining Holdouts