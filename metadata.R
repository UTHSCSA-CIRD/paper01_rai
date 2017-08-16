#' ---
#' title: "RAI, Metadata"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' ---
#' 
#' This file (`metadata.R`) is loaded before the data is loaded, so you should not 
#' rely on the data anywhere in this file. Please stick to variable assignments
#' that are not specific to your own computer (those go into `config.R`). Also
#' do not create functions here. Those go into `functions.R`
#' 
#' ## Non-analytic column names or regexps
#' 
#' Please create below as many lines as you need to for the the code you 
#' are using to find the columns which should be excluded from all plots, 
#' tables, and models. These columns include but are not limited to 
#' patient/visit IDs, free text, and anything that is a comma-separated list
#' of codes. These should be character vectors either of column names or of grep
#' patterns for selecting those names.  



#' ## Column-names that may need to be transformed
#' 
#' Same idea as above section
#' 
#' ### Numeric
#' 
cnum <- c();

#' ### Numeric columns that it would make sense to bin
#' 
#' Such as BMI, age, and income
cnum2bin <- c();

#' ### Date
#' 
cdate <- c();


#' ### Boolean/Logical
#' 
ctf <- c();


#' ### Factor/Discrete
#' 
cfactr <- c();


#' ### Integer
#' 
cintgr <- c();


#' ## Outcomes
#'
#' ### Clavien-Dindo 4 column names
#' 
ccd4 <- c();


#' ### All Complications column names
#' 
ccomp <- c();


#' ### Complications to exclude column names (2)
#' 
ccompexc <- c();

#' ### Serious complications
#' 
csrscomp <- setdiff(ccomp,ccompexc);

#' ### PATOS
#' 
#' Columns that _have_ a corresponding PATOS (present at time of surgery) 
#' column
chavepatos <- c();
#' Columns that _are_ PATOS columns 
#' 
#' Note that this one is a regexp so let's name the variable that holds it
#' a name that begins with 'g'. Feel free to make whichever variables are
#' convenient into lists or regexps instead of literal names.
garepatos <- c('_patos$');


#' ### Time-to-event column names
#' 
ctime <- c();


#' ## Predictors
#' 
#' ### RAI components
crai <- c();


#' ### Various individual predictors
#'
#' (replace the empty strings with actual column names of your data frame)
#' These start with v to indicate individual exact variable names rather than
#' vectors
#' 
#' Hispanic
vhisp <- '';
#' Income
vinc <- '';
#' Patient ID (for grouping)
vid <- '';
#' Date of surgery
vdts <- '';
#' Date of birth
vdob <- '';
