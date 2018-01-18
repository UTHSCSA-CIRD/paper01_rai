#' ---
#' title: "RAI Main Analysis"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' ---
#' 
#' Please read this file through before trying to run it. The comments tell
#' you what you need to edit in order to proceed.
#' 
source('global.R');
source('run.R');


#' ## Load data if it exists 
#' 
#' (useful later, right now don't bother saving sessions)
#'if(session %in% list.files()) load(session);
#' Load your data. Notice that we're using `read_csv()` from the readr library.
#' It is a little smarter than the built-in `read.csv()`
cost0 <- read_tsv(inputdata_cost,na=c('(null)',''));
colnames(cost0) <- tolower(colnames(cost0));


#' Create copy of original dataset
cost1 <- cost0;


#' Fixing the date columns in the 'cost' dataset:
cost1$admission_date <- as.Date(cost1$admission_date, format = '%m/%d/%y')
cost1$discharge_date <- as.Date(cost1$discharge_date, format = '%m/%d/%Y')


#' Filtering out hopefully the indext cases. This needs work but this is a 
#' a quick dirty way to get the index cases:
cost2 <- cost1 %>% filter(admit_date_diff < 20 & admit_date_diff > -20)


#' Isolating the 2016 UHS colectomy data elements:
col2016 <- dat1subs[["all_colon_all"]] %>% 
  filter(hospital_admissn_dt < '2017-01-01' & hospital_admissn_dt > '2015-12-31')


#' Merging the datasets:
 costdata <-  merge(col2016, cost1, by = 'idn_mrn', all.x = TRUE)


