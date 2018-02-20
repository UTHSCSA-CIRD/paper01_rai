#' ---
#' title: "RAI Configuration"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' ---
#' 
#' Please copy this file to `config.R`, edit that copy, and copy it over to
#' the working directory whenever you check out this project. This is just an
#' example of what computer-specific variables should be set. This does not 
#' actually get called by `run.R`. A file needs to be called `config.R` in order
#' to be used as a source of configuration information by our scripts
#' 
#' ## Used by all
#' Replace the following with the full path to the actual directory that 
#' resulted after you ran `git clone git@github.com:UTHSCSA-CIRD/paper01_rai`
#' Here is mine...
workdir <- '/tmp/paper01_rai/';
#' ### Used by run.R. Please put a path to your nsqip CSV file here.
inputdata <- '/home/a/dx/16_rai.shireman/in/LDS_170817_NSQIP.csv';
#' ### Please put a path to your NSQIP cost data here
inputdata_cost <- '/home/a/dx/16_rai.shireman/in/LDS_NSQIPwithCostData.csv';
#' ### What .rdata file to read instaed of running run.R each time
incache_run <- './local/in/paper01_rai_run_223a939f011c3d6c6e05d6e107790346.rdata';
outcache_path <- 'local/in/'
#' ### Only used for Shiny/TABSIE, not currently active
shinypw <- '';
