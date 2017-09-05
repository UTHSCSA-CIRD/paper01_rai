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
             ,'survival','MASS','Hmisc','zoo','coin' # various analysis methods
             ,'readr','dplyr','stringr','magrittr'   # data manipulation & piping
             ,'ggplot2','ggfortify','grid','GGally'  # plotting
             ,'stargazer','broom', 'tableone');                  # table formatting
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
#' 
#' data dictionary:
dctfile = 'VariableNamesFromUHSNSQIP.csv';
#' saved session data (not used right now)
session <- 'session.rdata';

#' ## Load data if it exists 
#' 
#' (useful later, right now don't bother saving sessions)
if(session %in% list.files()) load(session);
#' Load your data. Notice that we're using `read_csv()` from the readr library.
#' It is a little smarter than the built-in `read.csv()`
dat0 <- read_tsv(inputdata,na=c('(null)',''));
#' Read in the data dictionary
dct0 <- read_csv(dctfile,na = '');
colnames(dat0) <- tolower(colnames(dat0));
#' ## Create the groups of exact column names for this dataset
#' 
#' Any vector in `metadata.R` that is composed of regexps should get
#' resolved here to a vector of literal names using this expression as
#' an example:
#' 
#carepatos <- grepor(dat0,garepatos);
#cnopatos <- sub('_patos','',carepatos);
#' We have a new way to get column names whenever we need them:
#' `v(c_cd4)` . To see what other groups of column names are currently available
#' do `names(dct0)[-(1:2)]`

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
# firstidx <- which(dat1[,cnopatos[1]] > 1);
# secondidx <- which(dat1[,cnopatos[7]] > 1);
# dat1[firstidx,cnopatos[1]]  <- 1;
# dat1[secondidx,cnopatos[7]]  <- 1;
#' The below was wrong! Thes columns are counts, so can occur more than once.
#dat1[,cnopatos] <- sapply(dat1[,cnopatos],function(xx) xx>0,simplify=F);
#'
#' ## Column names of primary relevance
modelvars <- c(v(c_rai),v(c_postop),'income_final','hispanic_ethnicity');

#' Backup up the modified cnopatos column
#' ...because it's easier if patos-subtracted columns are modified in place
c_canbepatos <- v(c_canbepatos);
c_patos <- v(c_patos);
dat1[,paste0('bak_',c_canbepatos)] <- dat1[,c_canbepatos];
#' Since the postop & accompanying columns are counts, we just subtract patos
#' from their postop counterparts.
#dat1[,c_canbepatos] <- mapply(function(xx,yy){ifelse(xx,0,yy)}, dat1[,carepatos], dat1[,cnopatos]);
dat1[,c_canbepatos] <- dat1[,c_canbepatos] - dat1[,c_patos];


#' Create binned versions of certain numeric vars.
#' (commented out until we can put a c_num2bin or something into dct0)
# dat1[,paste0('bin_',cnum2bin)] <- sapply(dat1[,cnum2bin],function(ii){
#   qii <- c(0,quantile(ii,c(.25,.5,.75),na.rm=T),Inf);
#   cut(ii,breaks = qii);
# })

#' ## Create response variables
#' 
#' Create a column that is sum of all complications. Lets name analytically
#' created colums with an `a_` prefix. This way you could make it binary
#' via `dat1$a_allcomp > 0` or leave it as an integer and use number of 
#' different complications as a proxy for severity of outcome.
#dat1$a_allcomp <- rowSums(dat1[,csrscomp]);
c_postop_yesno <- setdiff(v(c_postop),v(c_count));
c_postop_count <- intersect(v(c_postop),v(c_count));
dat1$a_postop <- rowSums(dat1[,c_postop_count]) +
  apply(dat1[,c_postop_yesno],1,function(xx) sum(na.omit(xx %in% c('Yes','Positive'))));

#' -Hack the values of these variables to be binary for now.-
#dat1$sepsis_sirs_sepsis_sepshk_48h <- dat1$sepsis_sirs_sepsis_sepshk_48h != 'None';
#dat1$first_unp_ret_or <- dat1$first_unp_ret_or == 'Yes';
#dat1$hisp <- dat1$hispanic_ethnicity == 'Yes';
c_cd4_yesno <- setdiff(v(c_cd4),v(c_count));
c_cd4_count <- intersect(v(c_cd4),v(c_count));


#' Do the same as above but just for the `ccd4` complications
#dat1$a_cd4 <- rowSums(dat1[,c_cd4]);
dat1$a_cd4 <- rowSums(dat1[,c_cd4_count]) + 
  apply(dat1[,c_cd4_yesno],1,function(xx) sum(na.omit(xx=='Yes')));

dat1$a_transfer <- dat1$origin_status!='Not transferred (admitted from home)';

#' Obtain the RAI score
dat1$a_rai <- raiscore(dat1);

#' ## Transform Rows
#'
#' ### Sort the rows by patient ID and then by date of surgery, ascending
#' 
dat1 <- dat1[order(dat1$proc_surg_start),];
#' 
#' ### Drop patients without an income
#dat1 <- dat1[!is.na(dat1$income_final),];

#' ### Create a version of the dataset that only has each patient's 1st encounter
#' 
#' (you need to have specified the name of the ID column in `metadata.R`)
dat2 <- group_by(dat1,idn_mrn) %>% summarise_all(first);

#' Filter down to only NHW and hispanic
dat3<-subset(dat2,hispanic_ethnicity!='Unknown'&(hispanic_ethnicity=='Yes'|race=='White'));
dat3$hispanic_ethnicity<-factor(dat3$hispanic_ethnicity);
#' creating a counts table of missingness for each variable:
#' TODO: Fix these, to include the actual RAI components
# misstable <- transform(dat3, missing_income=is.na(income_final)) %>%
#   CreateTableOne(data=., vars= c(therai,c_cd4_count), strata='missing_income') %>%
#   print;
# misstable[,'p'] %>%  gsub("<","", x=.) %>% trimws() %>%
#   as.numeric() %>% p.adjust() %>% cbind(misstable,padj=.) %>%
#   data.frame %>% select(-test) -> misstable;
# 
# misstable2 <- CreateTableOne(data=., vars= c(therai,c_cd4_count), strata='gender');
# newmisstable2 <- print(misstable2);
# newmisstable2[,'p'] %>%  gsub("<","", x=.) %>% trimws() %>%
#   as.numeric() %>% p.adjust() %>% cbind(newmisstable2,padj=.) %>%
#   data.frame %>% select(-test) -> newmisstable2;
#' writing output to a 'Results' folder:
# variables <- rownames(newmisstable)
# newmisstable <- cbind(variables, newmisstable)
# write.table(x=newmisstable, file=paste(outputpath, 'IncomeMissingTable.csv', sep=''), na="", col.names=TRUE, row.names=FALSE, sep=',');

modvarstrata <-sapply(modelvars,function(ii) {try(eval(parse(text=sprintf("stratatable(dat3,modelvars,str=is.na(%s)|%s=='Unknown')",ii,ii))))});
modvarstrata <- modvarstrata[sapply(modvarstrata,class)=='matrix'];

#' Let's treat the variables with more than 6 distinct values as continuous and
#' the rest we tabulate whether or not they are factors. For this, 
#' `modelvars_iscont`, a named vector of T/F values, is created. We really ought
#' to just create a c_discrete column in dct0, but it might be out of date with 
#' the latest version so we'll clean it up later and use this vector for now.
modelvars_iscont<-sapply(dat3[,modelvars],function(xx) length(unique(xx))>6);
#' Now we just first do the continuous ones with summary and then the discrete
#' ones with table, and we `c()` them all together because when you do that to
#' lists, you get a list as the result.
modelvarsumtab <- c(sapply(dat3[,names(modelvars_iscont[modelvars_iscont])]
                         ,function(xx) cbind(summary(xx))),
                  sapply(dat3[,names(modelvars_iscont[!modelvars_iscont])]
                         ,function(xx) cbind(table(xx,useNA='always'))));

#modvarsumtab <- sapply(modelvars, function(ii){table(dat3[,ii],useNA = 'always')});
#newmodvarstrata <- print(modvarstrata);
#newmodvarsumtab <- print(modvarsumtab);

modvarstratafile <- paste0(outputpath,'StrataComplicationsMissingTables.csv');
modvartabfile <- paste0(outputpath,'SumComplicationsMissingTables.csv');
sapply(names(modvarstrata), function(x) try({
  # this one just skips a few lines to make the output easier to read
  write("\n\n",file=modvarstratafile,append=T);
  # this one writes the name of the table
  write(x,file=modvarstratafile,append=T);
  # this one writes the header manually because write.table is so dumb
  write(",Present,Missing,p-adj",file=modvarstratafile,append=T);
  # writing the actual table
  write.table( modvarstrata[[x]], file=modvarstratafile, na="", col.names=FALSE, row.names=TRUE
               , append= TRUE, sep=',' )
  }));
sapply(names(modelvarstab),function(xx){
  # Same as above
  cat("\n\n",xx,"\n",file=modvartabfile,append=T);
  write.table(modelvarsumtab[[xx]],file=modvartabfile,na="",col.names=F,row.names = T,append=T,sep=',');
});
#write.csv2(x=newmodvarstrata, file=paste(outputpath, 'StrataComplicationsMissingTables.csv', sep=''), na="", col.names=TRUE, row.names=FALSE, sep=',');
#write.csv2(x=newmodvarsumtab, file=paste(outputpath, 'SumComplicationsMissingTables.csv', sep=''), na="", col.names=TRUE, row.names=FALSE, sep=',');

#' ### Summary counts
# subset(dat3,race=='White'|hispanic_ethnicity=='Yes') %>% 
#   mutate(hisp=hispanic_ethnicity=='Yes') %>% 
#   group_by(gender,hisp) %>% 
#   summarise(
#     age=paste0(round(mean(age_at_time_surg,na.rm=T),1),' (',round(sd(age_at_time_surg,na.rm=T),1),')')
#     ,no_complications=sum((a_cd4+a_postop)==0)
#     ,income_nocomp=paste0(round(median(income_final[!(a_cd4|a_postop)],na.rm=T)/1000,1),' (',paste0(round(quantile(income_final[!(a_cd4|a_postop)],c(.25,.75),na.rm=T)/1000,1),collapse=','),')')
#     ,income_comp=paste0(round(median(income_final[a_cd4|a_postop],na.rm=T)/1000,1),' (',paste0(round(quantile(income_final[a_cd4|a_postop],c(.25,.75),na.rm=T)/1000,1),collapse=','),')')
#     ,n_cd4=sum(a_cd4>0)
#     , n_postop=sum(a_postop>0)) %>% 
#   mutate(hisp=ifelse(hisp,'Yes','No')) %>% 
#   setNames(c('Sex','Hispanic','Age (SD)','No Comp (N)','Income No Comp (IQR)','Income Comp (IQR)','CvDn4 (N)','Postop (N)')) ->   summary_counts;
# 
# write_tsv(summary_counts,'summary_counts.tsv');

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
#layout(matrix(1:25,nrow=5));
#.junk<-sapply(union(cnum,cintgr),function(ii) hist(dat3[[ii]],main=ii));
#' You will probably need to adjust the nrow/ncol for the `layout()`
#' command, and probably plot some of them individually so you 
#' can adjust the `xlim`, `breaks`, etc. The goal is to look for
#' skewed or otherwise wierd distributions.

#' Try using `ggduo()` to plot all predictors vs all 
#' responses.
resps <- c('a_postop','a_cd4');

#' ### Create your random sample
.Random.seed <- 20170816;
pat_samp <- sample(dat3$idn_mrn,1000,rep=T);
dat4 <- subset(dat3,idn_mrn %in% pat_samp);

#ggduo(dat4,union(cnum,cintgr),resps);
#ggduo(dat4,union(ctf,cfactr),resps);
#' The goal is to find the most obvious relationships beteen
#' predictors and variables.

#' NOW you have probed your data in a sufficiently deep and 
#' methodical way that you can start making decisions about how
#' to analyze it. For example...
glmpostop <- glm(a_postop~1,dat4,family='poisson');
glmpostopaic <- stepAIC(update(glmpostop,subset=!is.na(income_final)),scope=list(lower=.~1,upper=.~(a_rai+hispanic_ethnicity+income_final)^3),direction='both');
summary(glmpostopaic);
glmcd4 <- glm(a_cd4~1,dat4,family='poisson');
glmcd4aic <- stepAIC(update(glmcd4,subset=!is.na(income_final)),scope=list(lower=.~1,upper=.~(a_rai+hispanic_ethnicity+income_final)^3),direction='both');
summary(glmcd4aic);
#' BUt this is kind of a waste of time because RAI-A was never properly weighted. Let's fix that...
glmp_cd4<-glm(a_cd4~gender+x_loss_bw_6_months_prior_surg+dyspnea+currently_dialysis+chr_30_dy_prior_surg+functnal_heath_status+disseminated_cancer*age_at_time_surg+serum_creatinine+a_transfer
              ,dat4,subset=!is.na(serum_creatinine)&dyspnea!='At Rest'&functnal_heath_status!='Unknown',family='poisson');
#' Can we improve on RAI by adding the above fitted model?
anova(update(glmp_cd4,.~a_rai),update(glmp_cd4,.~.+a_rai),test='LRT');
#' Yes
#' 
#' How about in reverse? Can we improve on the model by adding RAI?
anova(glmp_cd4,update(glmp_cd4,.~.+a_rai),test='LRT');
#' Yes also
#' 
#' Now let's do stepwise regression to see what really needs to be in the model
glmp_cd4_aic <- stepAIC(glmp_cd4,scope=list(lower=~1,upper=~(.)^2),direction='both');
#' Now can this be improved with original RAI?
#' Can we improve on RAI by adding the above fitted model?
anova(update(glmp_cd4_aic,.~a_rai),update(glmp_cd4_aic,.~.+a_rai),test='LRT');
#' And does RAI improve it?
anova(glmp_cd4_aic,update(glmp_cd4_aic,.~.+a_rai),test='LRT');
#' A little
