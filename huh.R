#' ---
#' title: "RAI Main Analysis"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' ---
#' 
#' Please read this file through before trying to run it. The comments tell
#' you what you need to edit in order to proceed.
#' 
#' ## Load libraries
#+ warning=FALSE, message=FALSE
rq_libs <- c('compiler'                              # just-in-time compilation
             ,'survival','MASS','Hmisc','zoo'        # various analysis methods
             ,'readr','dplyr','stringr','magrittr'   # data manipulation & piping
             ,'ggplot2','ggfortify','grid','GGally'  # plotting
             ,'stargazer','broom');                  # table formatting
rq_installed <- sapply(rq_libs,require,character.only=T);
rq_need <- names(rq_installed[!rq_installed]);
if(length(rq_need)>0) install.packages(rq_need,repos='https://cran.rstudio.com/',dependencies = T);
sapply(rq_need,require,character.only=T);
#' Turn JIT to max: pre-compile all closures, `for`, `while`, and `repeat` loops
enableJIT(3);
#' ## Load local config file
#' 
#' Please edit the file referenced below, it has instructions in the 
#' comments.
source('./config.R');
#' Please edit the file referenced below, it has instructions in the
#' comments
source('./metadata.R');
#' This file has some possible useful functions. You might not need to edit
#' it but should read it.
source('./functions.R');
#'
#' ## Set generic variables
session <- 'session.rdata';

#' ## Load data if it exists 
#' 
#' (useful later, right now don't bother saving sessions)
if(session %in% list.files()) load(session);
#' Load your data. Notice that we're using `read_csv()` from the readr library.
#' It is a little smarter than the built-in `read.csv()`
dat0 <- read_delim(inputdata,na='(null)', delim="\t");
colnames(dat0) <- tolower(colnames(dat0));
#' ## Create the groups of exact column names for this dataset
#' 
#' Any vector in `metadata.R` that is composed of regexps should get
#' resolved here to a vector of literal names using this expression as
#' an example:
#' 
carepatos <- grepor(dat0,garepatos);

#' If you need to modify lists of column names using `gsub()` or if you
#' need to dynamically generate lists of column names using something
#' other than `grepor()` put the code for doing so here. Might want to
#' find columns with all/almost-all missing values, or ones with a value that
#' is always the same for all rows.



#' ## Convert columns
#' 
#' Create copy of original dataset
dat1 <- dat0;
#' Convert appropriate columns to factor
# Example only, doesn't run, you need to actually populate `cfactr` with
# names of columns for it to work
dat1[,cfactr] <- sapply(dat1[,cfactr],factor,simplify = F);

#' Normalize weight units
#' 
#' Normalize the weight units
dat1[dat0$weight_unit=='lbs','weight'] <- dat1[dat0$weight_unit=='lbs','weight']*0.453592;
#' Similarly for height...

#' Do likewise for dates, factors, maybe logicals, maybe numerics
#' Turns out read_


#' For each column in `chavepatos` create a column that is true only 
#' if that column is true and the corresponding column in `carepatos`
#' is false. Hint: use `mapply()` or `sapply()` for this and replace
#' the original `chavepatos` columns with these



#' Create binned versions of certain numeric vars.
dat1[,paste0('bin_',cnum2bin)] <- sapply(dat1[,cnum2bin],function(ii){
  qii <- c(0,quantile(ii,seq(.25,.5,.75)),Inf);
  cut(ii,breaks = qii);
})

#' ## Create response variables
#' 
#' Create a column that is sum of all complications. Lets name analytically
#' created colums with an `a_` prefix. This way you could make it binary
#' via `dat1$a_allcomp > 0` or leave it as an integer and use number of 
#' different complications as a proxy for severity of outcome.
dat1$a_allcomp <- rowSums(dat1[,csrscomp]);

#' Do the same as above but just for the `ccd4` complications
dat1$a_cd4comp <- rowSums(dat1[,ccd4]);


#' ## Transform Rows
#'
#' ### Sort the rows by patient ID and then by date of surgery, ascending
#' 
dat1 <- dat1[order(dat1[[vdts]]),];
#' 
#' ### Drop patients without an income
dat1 <- dat1[!is.na(dat1[[vinc]]),]

#' ### Create a version of the dataset that only has each patient's 1st encounter
#' 
#' (you need to have specified the name of the ID column in `metadata.R`)
dat2[['vid']] <- dat2[[vid]]
dat2 <- group_by(dat1,vid) %>% summarise_all(dat1,first);

#' ### Create your random sample
.Random.seed <- 20170816;
pat_samp <- sample(dat2$vid,1000,rep=T);
dat3 <- subset(dat2,vid %in% pat_samp);

#' ## Exploration
#' 
#' Try making pivot tables...
#group_by(dat3,FOO) %>% summarize(AllComp_Fraction=mean(a_allcomp>0)
#                                 ,CD4Comp_Fraction=mean(a_cd4comp>0));
#' ...by replacing FOO with NON-quoted column names representing sex, ethnicity
#' and binned income. Not a vector, just an arbitrarily long set of 
#' non-quoted names separated by commas. This can go in your abstract!
#' 
#' Try plotting a hist on each numeric value...
layout(matrix(25,nrow=5));
.junk<-sapply(c(cnum,cintgr),function(ii) hist(ii,breaks=100,main=ii));
#' You will probably need to adjust the nrow/ncol for the `layout()`
#' command, and probably plot some of them individually so you 
#' can adjust the `xlim`, `breaks`, etc. The goal is to look for
#' skewed or otherwise wierd distributions.

#' Try using `ggduo()` to plot all predictors vs all 
#' responses.
resps <- c('a_allcomp','a_cd4comp');
ggduo(dat3,c(cnum,cintgr),resps);
ggduo(dat3,c(ctf,cfactr),resps);
#' The goal is to find the most obvious relationships beteen
#' predictors and variables.

#' NOW you have probed your data in a sufficiently deep and 
#' methodical way that you can start making decisions about how
#' to analyze it.
#' 
