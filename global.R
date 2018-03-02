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
#' 
#' To turn on debug messages for this chunk, set results to 'markup' or 'as-is'
#+ global_val, message=FALSE
tryCatch(source('./config.R'),error=function(ee) stop(ee)
         ,finally=cat('\nValidating config.R file:\n'));
PI<-list(); comment(PI) <- "Project Info";
tryCatch(PI[['inputdata']] <-c(value=normalizePath(inputdata),hash=unname(md5sum(inputdata)))
         ,error=function(ee) stop(ee)
         ,finally=cat('Checking for valid inputdata file...\n'));
tryCatch(PI[['inputdata_cost']] <-c(value=normalizePath(inputdata_cost),hash=unname(md5sum(inputdata_cost)))
         ,error=function(ee) stop(ee)
         ,finally=cat('Checking for valid inputdata_cost file...\n'));
tryCatch(PI[['incache_run']] <-c(value=normalizePath(incache_run),hash=unname(md5sum(incache_run)))
         ,error=function(ee) warning(ee,immediate. = T)
         ,finally=cat('Checking for valid incache_run file...\n'));
cat('Checking for valid outcache_path...\n');
if(exists('outcache_path',1) && file_test('-d',outcache_path)){
  PI[['outcache_path']]<-c(value=normalizePath(outcache_path));
} else stop("The 'outcache_path' variable either was not set or does not specify a valid directory path. Please set it in your config.R");

options(runr.makeoutcache=if(exists('create_outcache')) create_outcache else F);
if(exists('update_incache_run',1)) options(runr.updincacherun=update_incache_run);

#' ## Load project-level metadata and functions
source('./metadata.R'); 
source('./functions.R');
#' ## Set generic variables
#' 
#' git hash
PI$revision <- gitstamp(production=F,branch=T);
tryCatch(PI$revision <- gitstamp(production = getOption('runr.prodgitstamp',T),branch=T)
         ,error=function(ee) stop(ee,immediate. =T)
         ,finally=cat('Checking to see if code is properly checked in...\n'));
#' 
#' data dictionary:
dctfile <- 'VariableNamesFromUHSNSQIP.csv';
tryCatch(PI$dctfile <- c(value=normalizePath(dctfile),hash=unname(md5sum(dctfile)))
         ,error=function(ee) stop(ee)
         ,finally=cat('Checking for valid dctfile...\n'));
rm(dctfile);

#' CPT code dictionary
cptfile <- 'cpt_dictionary.csv';
tryCatch(PI$cptfile <- c(value=normalizePath(cptfile),hash=unname(md5sum(cptfile)))
         ,error=function(ee) stop(ee)
         ,finally=cat('Checking for valid cptfile...\n'));
rm(cptfile);
#' saved session data (not used right now)
session <- 'session.rdata';
