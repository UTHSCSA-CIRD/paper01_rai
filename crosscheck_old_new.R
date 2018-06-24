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
    # warn
    if(!file.exists(session)){
      system(sprintf('R -e "source(\'run.R\'); save.image(file=\'%s\')"',session));
    }
  inputenv_new <- session;
}
if(!(exists('inputenv_old')&&file.exists(inputenv_old))){
  # warn
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
#' ...for the `dat1` s of the old and new versions
test_coldiffs <- mapply(all.equal
                        ,with(oldenv,dat1[order(dat1$idn_mrn),test_samenames])
                        ,with(newenv,dat1[order(dat1$idn_mrn),test_samenames]));
cbind(test_coldiffs[test_coldiffs!='TRUE']);
