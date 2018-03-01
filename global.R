#' ---
#' title: "RAI Libraries and Global Settings"
#' author: "Wilson, Bokov, Shireman"
#' date: "10/16/2017"
#' ---
#' 
#' ## Load libraries
#+ warning=FALSE, message=FALSE
rq_libs <- c('compiler'                                   # just-in-time compilation
             ,'MatchIt','DHARMa'                          # propensity scores and glm residuals
             ,'pscl'                                      # zero-inflated poisson, sigh
             ,'survival','MASS','Hmisc','zoo','coin'      # various analysis methods
             ,'survAUC','survivalROC','pROC'
             ,'Matrix'                                    # for pd matrices needed by faker()
             ,'readr','dplyr','stringr','magrittr'        # data manipulation & piping
             ,'lubridate','tools'
             ,'ggplot2','ggfortify','grid','GGally'       # plotting
             ,'survminer','gridExtra','scales'
             ,'stargazer','broom', 'tableone','janitor'   # table formatting
             ,'knitr','htmltab');
rq_installed <- sapply(rq_libs,require,character.only=T);
rq_need <- names(rq_installed[!rq_installed]);
if(length(rq_need)>0) install.packages(rq_need,repos='https://cran.rstudio.com/',dependencies = T);
sapply(rq_need,require,character.only=T);
#' Turn JIT to max: pre-compile all closures, `for`, `while`, and `repeat` loops
enableJIT(3);

#' ## Load local configurations.
#+ global_val, results='asis'
tryCatch(source('./config.R'),error=function(ee) stop(ee)
         ,finally=cat('\nValidating config.R file:\n'));
pi<-list(); comment(pi) <- "Project Info";
tryCatch(pi[['inputdata']] <-c(file=normalizePath(inputdata),hash=md5sum(inputdata))
         ,error=function(ee) stop(ee)
         ,finally=cat('Checking for valid inputdata file...\n'));
tryCatch(pi[['inputdata_cost']] <-c(file=normalizePath(inputdata_cost),hash=md5sum(inputdata_cost))
         ,error=function(ee) stop(ee)
         ,finally=cat('Checking for valid inputdata_cost file...\n'));
tryCatch(pi[['incache_run']] <-c(file=normalizePath(incache_run),hash=md5sum(incache_run))
         ,error=function(ee) warning(ee,immediate. = T)
         ,finally=cat('Checking for valid incache_run file...\n'));
cat('Checking for valid outcache_path...\n');
if(exists('outcache_path') && file_test('-d',outcache_path)){
  pi[['outcache_path']]<-normalizePath(outcache_path);
} else stop("The 'outcache_path' variable either was not set or does not specify a valid directory path. Please set it in your config.R")

options(runr.makeoutcache=if(exists('create_outcache')) create_outcache else F);
if(exists('update_incache_run')) options(runr.updincacherun=update_incache_run);

#' ## Load project-level metadata and functions
source('./metadata.R'); 
source('./functions.R');
#' ## Set generic variables
#' 
#' data dictionary:
dctfile <- 'VariableNamesFromUHSNSQIP.csv';
tryCatch(pi$dctfile <- c(file=normalizePath(dctfile),hash=md5sum(dctfile))
         ,error=function(ee) stop(ee)
         ,finally=cat('Checking for valid dctfile...\n'));
rm(dctfile);

#' CPT code dictionary
cptfile <- 'cpt_dictionary.csv';
tryCatch(pi$cptfile <- c(file=normalizePath(cptfile),hash=md5sum(cptfile))
         ,error=function(ee) stop(ee)
         ,finally=cat('Checking for valid cptfile...\n'));
rm(cptfile);
#' saved session data (not used right now)
session <- 'session.rdata';
