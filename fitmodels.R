#' ---
#' title: "RAI Model Fits"
#' author: "Wilson, Bokov, Shireman"
#' date: "03/01/2018"
#' ---
#'
#+ echo=FALSE, message=FALSE
knitr::opts_chunk$set(echo=F,warning = F,cache=T,message=F);
#+ cache=FALSE,results='hide'
source('global.R');
prod<-F;
#' Report date: `r Sys.Date()`.
#' 
#+ cache=FALSE,results='hide'
# first, try to load the 'incache_run' data
if(exists('PI') && !is.null(PI$incache_run[1]) && 
   !is.na(PI$incache_run[1]) && file.exists(PI$incache_run[1])){
  load(PI$incache_run[1]);
  PI$previous <- previous.PI;
  msg_cache <- '\nCached results from `run.R` were used.\n';
} else {
  # otherwise try sourcing it the old way
  source('run.R');
  msg_cache <- '';
}
#' #### Audit Trail
#+ projinfo,results='asis'
cat(msg_cache,'\n');
if(exists('PI')){
  PI$basefname <- basename(as.character(parent.frame(2)$ofile));
  currentpi <- PI;
  temp_seeds <- c();
  while(is.list(currentpi)){
    lapply(currentpi[names(currentpi)!='previous'],rbind) %>% 
      lapply(as.character) %>% lapply(`length<-`,2) %>%
      bind_cols %>% t %>% 
      kable(format = 'markdown',col.names = c('value','hash')) %>%
      cat(sep = '\n');
    cat('\n\n---\n\n');
    currentpi <- currentpi$previous;
    temp_seeds <- c(temp_seeds,currentpi$project_seed);
  }
  if(is.null(PI$project_seed)) PI$project_seed <- temp_seeds[1];
} else {
  cat('Revision:',gitstamp(production=prod,branch = T),'\n');
  cat('inputdata:',inputdata);
  cat('inputdata_cost:',inputdata_cost);
}
#' Declare the predictors, responses (by type), covariates
#' and the right-hand sides of additive models and of models
#' with 2-way interactions
l_mainpreds <- c('a_rai','a_rockwood');
ll_resps <- list(
  counts = c('a_postop','a_cd4')
  ,t2event = 'a_t'
  ,cevent = c('a_c_death','a_c_readm','a_c')
);
l_covars <- c('income_final','sex','hispanic_ethnicity','age_at_time_surg')

l_forms_add <- sapply(l_mainpreds
                      ,function(xx) paste0('.~',xx,paste(l_covars,collapse='+'))
                      ,simplify=F) %>% lapply(as.formula);
l_forms_2int <- lapply(l_forms_add,update,.~(.)^2);
l_forms_univar <- sapply(l_mainpreds,function(xx) paste0('.~',xx)) %>% 
  lapply(as.formula);
#' 
#' Then we will create the left-hand sides, which is trickier due to some
#' responses being Surv(a_t,a_c_FOO)~. and some being FOO~
#' 
#' 
#' 
#' 
#' 
previous.PI <- PI;
rm(PI);
# The below gives errors, needs to be dealt with later
#if(!is.na(previous.PI$basefname) && 
#   (!interactive()||file.exists('cleanrun'))) {
#  warning(sprintf('Creating %s results cache',as.character(previous.PI$basefname)));
#   save.image(file=cfile<-paste0(previous.PI$outcache_path
#                                 ,'/'
#                                 ,nametag<-gsub('\\.r$','',previous.PI$basefname,ignore.case = T)
#                                 ,format(Sys.time(),'.%Y.%m.%d.%s.rdata')));
#   # update the local 'config.R' with the path to this cache file, if the 
#   # user has indicated they want this by setting option(runr.updincacherun=T)
#   if(getOption(sprintf('runr.updincache%s',nametag),F)){
#     write(sprintf("incache_%s <- '%s'\n",c(nametag,cfile)),file='config.R',append=T);
#   };
#   # now delete everything to keep it from mucking up the environment of whatever
#   # is calling this script
#   rm(list=ls(all=T));
#};
