#' ---
#' title: "RAI Main Analysis"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' ---
#'
#+ echo=FALSE, message=FALSE
knitr::opts_chunk$set(echo=F,warning = F,cache=T,message=F);
#+ cache=FALSE
source('global.R');
#' Report date: `r date()`.
#'
# Revision: `r gitstamp()`.
#'
#' Data file: `r inputdata`.
#' 
#' Cost data file: `r inputdata_cost`.
#' 
#' 
#' 
#+ cache=TRUE
source('run.R');
surv.rai <- survfit(Surv(a_t,a_c) ~ I(a_rai>median(a_rai))
                    , data = sbs0$all$all_emergency,subset=a_t>0);
surv.rock <- survfit(Surv(a_t,a_c) ~ I(a_rockwood>median(a_rockwood))
                     , data = sbs0$all$all_emergency,subset=a_t>0);

#ggsurvplot(surv.rai);
#ggsurvplot(surv.rock);


pl_surv <-list(RAI=autoplot(surv.rai), Rockwood=autoplot(surv.rock));

pl_surv <- sapply(names(pl_surv)
                  ,function(xx) pl_surv[[xx]] + 
                    theme(legend.position = 'none') + 
                    ggtitle(paste0(xx,', Split by Median')) +
                    scale_y_continuous(limits=c(.5,1),labels = scales::percent) +
                    labs(x='Time in Days', y = 'Survival'),simplify=F);
multiplot(plotlist=pl_surv,cols=1);

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
