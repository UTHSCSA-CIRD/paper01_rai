#' ---
#' title: "RAI Data Crosschecks"
#' author: "Bokov, Shireman"
#' date: "06/24/2018"
#' ---
#'
#+ echo=FALSE, message=FALSE
knitr::opts_chunk$set(echo=F,warning = F,cache=T,message=F);
#+ cache=FALSE
source('global.R');
instrequire('pander');
#' 
#' Report date: `r Sys.Date()`.
#'
#' Revision: `r gitstamp()`.
#'
#' The purpose of this file is to load two different R environments created by
#' the same version of `run.R` on two different input files and compare them. To
#' enable this, assign valid paths to `inputenv_old` and `inputenv_new` in your
#' `config.R` file. Otherwise, it will default to comparing identical copies of
#' the current `inputdata` environment created using the current `run.R` file.
#+ cache=TRUE
if(!(exists('inputenv_new')&&file.exists(inputenv_new))){
    warning('inputenv_new variable either not defined or file does not exist, creating new one.');
    if(!file.exists(session)){
      system(sprintf('R -e "source(\'run.R\'); save.image(file=\'%s\')"',session));
    }
  inputenv_new <- session;
}
if(!(exists('inputenv_old')&&file.exists(inputenv_old))){
  warning('inputenv_old variable either not defined or file does not exist, copying new one to old.');
  inputenv_old <- inputenv_new;
}
#' Old environment: `r inputenv_old`
#' 
#' New environment: `r inputenv_new`
oldenv <- new.env(); load(inputenv_old,envir = oldenv);
newenv <- new.env(); load(inputenv_new,envir = newenv);

#' ### Does every top-leveltable have the same row counts?
test_rowcounts <- sapply(oldenv,nrow) %>% Filter(Negate(is.null),.) %>% names %>% 
  union(sapply(newenv,nrow) %>% Filter(Negate(is.null),.) %>% names) %>% 
  sapply(function(xx) c(nrow(oldenv[[xx]]),nrow(newenv[[xx]]))) %>% t %>% 
  data.frame %>% setNames(c('old','new'));
test_rowcounts;

#' ### What columns are different in the old and new data?
setdiff(names(oldenv$dat0),names(newenv$dat0));
setdiff(names(newenv$dat0),names(oldenv$dat0));
test_samenames <- intersect(names(oldenv$dat1),names(newenv$dat1));

#' ### For the column names that are the same, which ones have different values?
#' 
#' Create a datold and datnew for dat1, ordered by case_number
dat1old <- with(oldenv,dat1[order(dat1$case_number),]);
dat1new <- with(newenv,dat1[order(dat1$case_number),]);
#' Set the income_final column of the new dat1 to what it was in the old
dat1new$income_final <- dat1new$income_2013; 
#' Update `test_samenames`
test_samenames <- c('income_final',test_samenames);
#' Same thing with 2016 colectomy
col2016old <- with(oldenv$sbs0$all2016,all_colon_all[order(all_colon_all$case_number),]);
col2016new <- with(newenv$sbs0$all2016,all_colon_all[order(all_colon_all$case_number),]);
#' Set the income_final column of the new dat1 to what it was in the old
col2016new$income_final <- col2016new$income_2013; 

#' Compare all the shared columns between the old and new
test_coldiffs <- mapply(all.equal
                        ,dat1old[,test_samenames]
                        ,dat1new[,test_samenames]);
if(!all(test_coldiffs=='TRUE')) {
  warning('Some columns differ:');
  cbind(test_coldiffs[test_coldiffs!='TRUE']);
} else message('\nAll columns shared between old and new have identical values');

test_coldiffs_col2016 <- mapply(all.equal
                                ,col2016old[,test_samenames]
                                ,col2016new[,test_samenames]);
if(!all(test_coldiffs_col2016=='TRUE')) {
  warning('Some columns differ:');
  cbind(test_coldiffs_col2016[test_coldiffs_col2016!='TRUE']);
} else message('\nAll columns shared between old and new have identical values');


#' ### How did missingness change between the 2013 incomes and 2016 ones?
#' 
test_missinginc <- with(dat1new,table(`2013`=is.na(income_2013)
                                      ,`2016`=is.na(income_2016))) %>% 
  addmargins();
test_missinginc;
#' Seems there are `r test_missinginc[1,2]` more missing values now.
#' 
with(dat1new,table(`No Address`=no_address,`2016`=is.na(income_2016))) %>% 
  addmargins();
with(dat1new,table(`No Address`=no_address,`2013`=is.na(income_2013))) %>% 
  addmargins();
#' This is not due to more missing addresses
nrow(subset(dat1new,no_address!=1&no_idmatch==1));
#' There are none missing due to missing ID that are not also missing due to 
#' missing address. So, the missing incomes are ones for which addresses are 
#' available, just no incomes. Inspecting the data shows that we do have 
#' education levels available...
head(subset(dat1new,no_address!=1&is.na(income_2016)&!is.na(income_2013))[,1:11]);
#' This means that the census block groups themselves no longer have incomes 
#' available.
#' 
#' TODO: Confirm by looking at those block groups and if they 
#' really are missing in the raw census data, fall back to tract or zip

#' ## Missingness specifically in colectomy 2016
test_missinginc_col2016 <- with(col2016new,table(`2013`=is.na(income_2013)
                                      ,`2016`=is.na(income_2016))) %>% 
  addmargins();
test_missinginc_col2016;
#' 
#' There are `r test_missinginc_col2016[1,2]` new missing incomes for the 2016 
#' data
subset(col2016new,is.na(income_2016)&!is.na(income_2013))[,1:11];
#' Again, these are not due to missing address.
#' 
#' Conclusion: there are 33 patients for whom we lack addresses.
#' TODO: Confirm that all the manual patches have been applied database-end
#' TODO: Find their MRNs and get their addresses from sunrise.client or patient_dimension
