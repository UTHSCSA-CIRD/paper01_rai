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
cnopatos <- sub('_patos','',carepatos);

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
#dat1[,cfactr] <- sapply(dat1[,cfactr],factor,simplify = F);

#' Normalize weight units
#' 
#' Normalize the weight units
dat1[dat0$weight_unit=='lbs','weight'] <- dat1[dat0$weight_unit=='lbs','weight']*0.453592;
#' Similarly for height...

#' -Do likewise for dates, factors, maybe logicals, maybe numerics-
#' Turns out `read_delim()` is good at recognizing dates and numerics 
#' on its own.


#' For each column in `chavepatos` create a column that is true only 
#' if that column is true and the corresponding column in `carepatos`
#' is false. Hint: use `mapply()` or `sapply()` for this and replace
#' the original `chavepatos` columns with these


#' getting rid of the 2's in the column:
firstidx <- which(dat1[,cnopatos[1]] > 1);
secondidx <- which(dat1[,cnopatos[7]] > 1);
dat1[firstidx,cnopatos[1]]  <- 1;
dat1[secondidx,cnopatos[7]]  <- 1;


#' creating modified cnopatos columns:
dat1[,paste0('mod_',cnopatos)] <- mapply(function(xx,yy){ifelse(xx,0,yy)}, xx=dat1[carepatos], yy=dat1[,cnopatos]);


#' counting all complications after surgery in the "mod_" columns:
sumnopatos <- rowSums(dat1[,paste0('mod_',cnopatos)]);
sumnopatostf <- ifelse(sumnopatos>0, 1, 0);

#' creating sum ccd4 column:
one <- as.numeric(ifelse(unlist(dat1[,ccd4[1]])=="None", 1, 0));
two <- as.numeric(ifelse(unlist(dat1[,ccd4[2]])>0, 1, 0));
three <- as.numeric(unlist(dat1[,ccd4[3]]));
four <- as.numeric(unlist(dat1[,ccd4[4]]));
five <- as.numeric(ifelse(unlist(dat1[,ccd4[5]])>0, 1, 0));
six <- as.numeric(unlist(dat1[,ccd4[6]]));
seven <- as.numeric(unlist(dat1[,ccd4[7]]));
eight <- as.numeric(ifelse(unlist(dat1[,ccd4[5]])>0, 1, 0));

sumccd4 <-rowSums(cbind(one, two, three, four, five, six, seven, eight));
sumccd4tf <- ifelse(sumccd4 > 0, 1, 0);
dat2 <- cbind(dat1, hisp, sumccd4tf, sumnopatostf);
hisp <- unlist(dat1[,'hispanic_ethnicity']);
hispfactor <- hisp;

dat2 %>% 
    select('income_final', 'gender', 'hispanic_ethnicity', 'sumccd4tf', 'sumnopatostf') %>% 
    group_by('sumccd4tf', 'sumnopatostf', 'gender', 'hispanic_ethnicity','income_final') %>%
    summarise(N=length("gender"));

#' creating a box plot for 'sepsis_sirs_sepsis_sepshk_48h' as a
#' predictor variable for sumnopatos:
hisp <- unlist(dat1[,'hispanic_ethnicity']);
hispfactor <- hisp;
table(sumnopatos, hisp);
table(sumnopatostf, hisp);
levels(hispfactor) <- c("No", "Yes", "Unknown");
boxplot(sumnopatos ~ as.factor(hispfactor));
chiSquare(sumnopatostf ~ hispfactor);
#'results here are interesting. There's significance but I think this is the wrong model

glm(sumnopatostf ~ hispfactor); #negative coefficient; appears being hispanic is protective
coef(summary(glm(sumnopatostf ~ hispfactor)));
#all of the p-values are significantly associated with sumnopatostf
#as a matter of fact, the odds ratio for being hispanic and having
#a sumnopatos score greater that 0 is 0.827 (not including unknowns)
table(cut(dat1$age_at_time_surg, breaks=c(25,45,65,130), right = FALSE), hisp);

#Dr. Bokov's tasks:
boxplot(unlist(dat1[,ccd4[1]]) ~ hispfactor, main=ccd4[1]); #error
boxplot(unlist(dat1[,ccd4[2]]) ~ hispfactor, main=ccd4[2]);  #looks weird
boxplot(unlist(dat1[,ccd4[3]]) ~ hispfactor, main=ccd4[3]);  #looks weird
boxplot(unlist(dat1[,ccd4[4]]) ~ hispfactor, main=ccd4[4]);  #looks weird
boxplot(unlist(dat1[,ccd4[5]]) ~ hispfactor, main=ccd4[5]);  #looks weird
boxplot(unlist(dat1[,ccd4[6]]) ~ hispfactor, main=ccd4[6]);  #looks weird
boxplot(unlist(dat1[,ccd4[7]]) ~ hispfactor, main=ccd4[7]);  #looks weird
boxplot(unlist(dat1[,ccd4[8]]) ~ hispfactor, main=ccd4[8]);  #error

boxplot(sumnopatos ~ hispfactor, main='SumComplicationScore'); #looks weird
boxplot(sumnopatostf ~ hispfactor, main='SumComplicationScore'); #looks weird

plot(unlist(dat1[,ccd4[1]]) ~ dat1$income_final, main=ccd4[1]); #error
plot(unlist(dat1[,ccd4[2]]) ~ dat1$income_final, main=ccd4[2]);  #looks weird
plot(unlist(dat1[,ccd4[3]]) ~ dat1$income_final, main=ccd4[3]);  #looks weird
plot(unlist(dat1[,ccd4[4]]) ~ dat1$income_final, main=ccd4[4]);  #looks weird
plot(unlist(dat1[,ccd4[5]]) ~ dat1$income_final, main=ccd4[5]);  #looks weird
plot(unlist(dat1[,ccd4[6]]) ~ dat1$income_final, main=ccd4[6]);  #looks weird
plot(unlist(dat1[,ccd4[7]]) ~ dat1$income_final, main=ccd4[7]);  #looks weird
plot(unlist(dat1[,ccd4[8]]) ~ dat1$income_final, main=ccd4[8]);  #error

plot(sumnopatos ~ dat1$income_final, main='SumComplicationScore', las=2); #looks weird
boxplot()

typeof(dat1[,ccd4])















boxplot(log(dat1$income_final) ~ sumnopatos+hispfactor, las=2); #this log fxn is ln (natural log)
#hispanics with complications slightly more likely to be poor compared to others?????
#bit of a stretch...
boxplot(log(dat1$income_final) ~ sumnopatostf+hispfactor, las=2); #this log fxn is ln (natural log)


boxplot(sumnopatos ~ cut(log(dat1$income_final), 5) + hispfactor, las=2); #this log fxn is ln (natural log)
table(sumnopatos, cut(log(dat1$income_final), 5), hispfactor, las=2); #this log fxn is ln (natural log)


boxplot(unlist(dat1[,ccd4[2]]) ~ hispfactor, main=ccd4[2], las=2);



boxplot(unlist(dat1[,ccd4[3]]) ~ unlist(dat1[,'hispanic_ethnicity']))
#' Create binned versions of certain numeric vars.
dat1[,paste0('bin_',cnum2bin)] <- sapply(dat1[,cnum2bin],function(ii){
  qii <- c(0,quantile(ii,seq(.25,.5,.75)),Inf);
  cut(ii,breaks = qii);
})
head(dat1[,paste0('bin_',cnum2bin)])
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
