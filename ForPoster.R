#' ---
#' title: "RAI Main Analysis"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' ---
#'
#+ echo=FALSE, message=FALSE
knitr::opts_chunk$set(echo=F,warning = F,cache=F,message=F);
#+ cache=FALSE
source('global.R');
#' Report date: `r date()`.
#'
#' Revision: `r gitstamp()`.
#'
#' Data file: `r inputdata`.
#' 
#' Cost data file: `r inputdata_cost`.
#' 
#' 
#' 
#+ cache=TRUE
source('run.R');
#' # Abstract
#' 
#' # Introduction
#' 
#' * RAI-A good for predicting surgical risk
#' * ...but it cannot be used on live data because it's hard-coded to NSQIP
#' * The availability of NSQIP variables can differ between sites with different
#'   levels of participation and some variables may be discontinued.
#' * Rockwood Index can be calculated on anything, tolerates missing data,
#'   and therefore it can be calculated not only on manually curated registry
#'   data but on any other sources of laboratory values and diagnoses including
#'   billing records and EMR systems.
#' * Rockwood has been thoroughly vetted by geriatricians and its statistical
#'   assumptions have been explicitly tested.
#' * Rockwood could be used for automated risk-calculation to support surgical
#'   decisions it it could be shown that it is as good as or better than RAI-A
#'   for predicting surgical outcomes.
#'   
#' # Methods
#' 
#' ## Data Sources
#' 
#' ## Analysis
#' 
#' # Results
#' 
#' ## RAI-A and Rockwood both are reasonable predictors of 30-day mortality and readmission
#' 
fit_srvs <- list(`RAI-A`=survfit(Surv(a_t,a_c) ~ I(a_rai>median(a_rai))
                            , data = sbs0$all$all_emergency,subset=a_t>0)
                ,Rockwood=survfit(Surv(a_t,a_c) ~ I(a_rockwood>median(a_rockwood))
                            , data = sbs0$all$all_emergency,subset=a_t>0));
pl_srvs <- mapply(function(aa,bb) autoplot(aa,ylim=c(0.5,1),xlim=c(0,30)) + 
                    ggtitle(paste0(bb,' as a predictor of  30-day outcomes')) +
                    scale_y_continuous(labels=scales::percent) +
                    scale_color_discrete(bb,labels=c('Low','High')) +
                    scale_fill_discrete(bb,labels=c('Low','High')) +
                    labs(x='Days Post-Surgery',y='Readmission-Free Survival')
                  ,fit_srvs,names(fit_srvs),SIMPLIFY = F);
# these are duplicates of the plots created above and rendered by multiplot() 
# below
#ggsurvplot(surv.rai);
#ggsurvplot(surv.rock);
multiplot(plotlist=pl_srvs,cols=1);
#' ## RAI-A and Rockwood have very similar AUC metrics
#' 
#' 

# This is the creation of an analytic variable. Therefore it should happen in run.R
# Also, this should be done on dat1, right after a_rockwood gets created, so all
# the other subsets can inherit it. I moved it there.
#sbs0$all$all_emergency$a_rockwood_range <- cut(sbs0$all$all_emergency$a_rockwood, 7)

thecolnames1 <- c("RAI-A Score"='rai_range'
                  ,"Rockwood Score"='a_rockwood_range'
                  ,"Income"='income_final'
                  ,"Gender"='gender'
                  ,"Hispanic"='hispanic_ethnicity'
                  ,"BMI"='bmi'
                  ,"Age"='age_at_time_surg'
                  ,"Count"='bin_n'
                  ,"Cumulative Count"='cumul_count'
                  ,"Died 30d"="postop_death_30_dy_proc_n"
                  ,"Died 30d Frac."="postop_death_30_dy_proc_frac"
                  ,"Readmission 30d"="a_readm_30_dy_n"
                  ,"Readmission 30d Frac."="a_readm_30_dy_frac"
);

# We set the default value of the outcomes argument for the purposes of this 
# script so we don't have to keep repeating it.
formals(countfrac)$outcomes <- c('postop_death_30_dy_proc','a_readm_30_dy');
# Now we render the tables. You don't need all that stuff below.
countfrac(sbs0$all$all_emergency,groupcols = 'a_rockwood_range') %>% 
  mapnames(thecolnames1) %>% kable(format='markdown',digits = 2);
countfrac(sbs0$all$all_emergency,groupcols = 'rai_range') %>% 
  mapnames(thecolnames1) %>% kable(format='markdown',digits = 2);

#' ### Demographics
#+ results='asis'
mutate(sbs0$all$all_emergency,`RAI-A`=factor(a_rai>median(a_rai)
                                             ,levels = c('FALSE','TRUE')
                                             ,labels=c('Low','High'))) %>% 
  mapnames(thecolnames1) %>% 
  CreateTableOne(names(thecolnames1)[3:7],'RAI-A',.) %>% 
  print(printToggle=F,noSpaces=T) %>% `[`(,-4) %>% 
  kable(format='markdown');
