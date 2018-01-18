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
dat0 <- read_tsv(inputdata,na=c('(null)',''));

#' Read in the data dictionary
dct0 <- read_csv(dctfile,na = '');
dct1 <- read_csv(cptfile,na='');
colnames(dat0) <- tolower(colnames(dat0));
colnames(cost0) <- tolower(colnames(cost0));


#' Create copy of original dataset
cost1 <- cost0;
dat1 <- dat0;
names(dat1)<-gsub('deid_patient','idn_mrn',names(dat1));
names(dat1)<-gsub('deid_visit','lmrn_visit',names(dat1));
names(dat1)<-gsub('deid_case','case_number',names(dat1));


#' Fixing the date columns in the 'cost' dataset:
cost1$admission_date <- as.Date(cost1$admission_date, format = '%m/%d/%y')
cost1$discharge_date <- as.Date(cost1$discharge_date, format = '%m/%d/%Y')
