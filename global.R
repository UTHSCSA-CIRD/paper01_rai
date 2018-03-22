#' ---
#' title: "RAI Libraries and Global Settings"
#' author: "Wilson, Bokov, Shireman"
#' date: "10/16/2017"
#' ---
#' 
#' ## Load libraries
#+ warning=FALSE, message=FALSE
source('./functions.R');
instrequire(c('compiler'                                   # just-in-time compilation
             ,'MatchIt','DHARMa'                          # propensity scores and glm residuals
             ,'pscl'                                      # zero-inflated poisson, sigh
             ,'survival','MASS','Hmisc','zoo','coin'      # various analysis methods
             ,'survAUC','survivalROC','pROC'
             ,'Matrix'                                    # for pd matrices needed by faker()
             ,'readr','dplyr','stringr','magrittr'        # data manipulation & piping
             ,'lubridate'
             ,'ggplot2','ggfortify','grid','GGally'       # plotting
             ,'survminer','gridExtra','scales'
             ,'stargazer','broom', 'tableone','janitor'   # table formatting
             ,'knitr','htmltab'));
#' Turn JIT to max: pre-compile all closures, `for`, `while`, and `repeat` loops
enableJIT(3);
#' ## Load local config file
#' 
source('./config.R');
source('./metadata.R'); 



#' ## Set generic variables
#' 
#' data dictionary:
dctfile <- 'VariableNamesFromUHSNSQIP.csv';
cptfile <- 'cpt_dictionary.csv';
#' saved session data (not used right now)
session <- 'session.rdata';
