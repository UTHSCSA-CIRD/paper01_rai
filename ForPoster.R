#' ---
#' title: "RAI Main Analysis"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' bibliography: Frailty.bib
#' ---
#'
#+ echo=FALSE, message=FALSE
knitr::opts_chunk$set(echo=F,warning = F,cache=F,message=F);
options(knitr.kable.NA='');
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
#+ poster_variables
thecolnames1 <- c("RAI-A Score"='rai_range'
                  ,"Rockwood Score"='a_rockwood_range'
                  ,"RAI-A"='a_rai'
                  ,"Rockwood"='a_rockwood'
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
                  # here are the to-replace column names for tidy/glance
                  ,"N"='n'
                  ,"Events"='nevent'
                  ,"Effect"='estimate'
                  ,"Std. Err."='std.error'
                  ,"Estimate/SE"='statistic'
                  ,"P"='p.value'
                  ,"R^2"='r.squared'
                  ,"Concordance"='concordance'
                  ,"Concordance Std. Err."='std.error.concordance'
);

# We set the default value of the outcomes argument for the purposes of this 
# script so we don't have to keep repeating it.
formals(countfrac)$outcomes <- c('postop_death_30_dy_proc','a_readm_30_dy');
#' # Abstract
#' 
#' # Introduction
#' 
#' * RAI-A good for predicting surgical risk [@HallDevelopmentInitialValidation2017]
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
#' ## Patient Demographics
#+ table_demog,results='asis'
mutate(sbs0$all$all_emergency,`RAI-A`=factor(a_rai>median(a_rai)
                                             ,levels = c('FALSE','TRUE')
                                             ,labels=c('Low','High'))) %>% 
  mapnames(thecolnames1) %>% 
  CreateTableOne(names(thecolnames1)[5:9],'RAI-A',.) %>% 
  print(printToggle=F,noSpaces=T) %>% `[`(,-4) %>% 
  kable(format='markdown');
#' ## Analysis
#' 
#' # Results
#' 
#' ## RAI-A and Rockwood both are reasonable predictors of 30-day mortality and readmission
#' 
#+ plot_survfits
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
#'
#+ results='asis'
sapply(fit_coxs<-list(`RAI-A`=cox.rai.train,`Rockwood`=cox.rock.train)
       ,function(xx) cbind(tidy(xx),glance(xx)),simplify=F) %>% 
  do.call('rbind',.) %>% `[`(,c('n','nevent','estimate','std.error','statistic'
                                ,'p.value','r.squared','concordance'
                                ,'std.error.concordance','AIC','BIC')) %>% 
  mapnames(thecolnames1) %>% t %>% kable(format = 'markdown',digits=4);
#starkable(cox.rai.train,-1,searchrep = cbind(c('V1','Dependent variable','a_rai|a_rockwood'),c('Statistic','Value','Effect (SD)')));
#starkable(cox.rock.train,-1,searchrep = cbind(c('V1','Dependent variable','a_rai|a_rockwood'),c('Statistic','Value','Effect (SD)')));
# .junk<-capture.output(stargazer(cox.rai.train,type='html',omit.table.layout = 'n-!=!d'
#                          ,covariate.labels = 'Effect Size'
#                          ,omit.stat = c('wald','max.rsq')) %>% htmltab %>% 
#                  kable(col.names=c('Statistic','Value'),row.names=F) -> cox.rai.train.summary);
# cat(cox.rai.train.summary,sep='\n');
# .junk<-capture.output(stargazer(cox.rai.train,type='html',omit.table.layout = 'n-!=!d'
#                                 ,covariate.labels = 'Effect Size'
#                                 ,omit.stat = c('wald','max.rsq')) %>% htmltab %>% 
#                         kable(col.names=c('Statistic','Value'),row.names=F) -> cox.rock.train.summary);
# cat(cox.rock.train.summary,sep='\n');
#'
#+ table_counts, results='asis'
countfrac(sbs0$all$all_emergency,groupcols = 'a_rockwood_range') %>% 
  mapnames(thecolnames1) %>% kable(format='markdown',digits = 2);
countfrac(sbs0$all$all_emergency,groupcols = 'rai_range') %>% 
  mapnames(thecolnames1) %>% kable(format='markdown',digits = 2);
#' 
#' ## RAI-A and Rockwood have equivalent concordances and AUCs
#' 
#' As can be seen from table 2, the concordances are `r paste0(paste(round(summary(cox.rai.train)$concordance,3),collapse=' (SE='),')')`
#' and `r paste0(paste(round(summary(cox.rock.train)$concordance,3),collapse=' (SE='),')')` for
#' the Cox models whose predictors are RAI-A and Rockwood, respectively. Their 
#' Receiver-Operator Characteristic (ROC) curves can be seen in Figure 2, along 
#' with their areas under the curve (AUCs).
l_rocs <- with(sbs0$train,lapply(all_emergency[,c('a_rai','a_rockwood')]
                                 ,function(xx) roc(response=all_emergency$a_c
                                                   ,predictor=xx)));
plot(l_rocs$a_rai,col='orange');
lines(l_rocs$a_rockwood,col='darkgreen');
legend('topleft',bty ='n',col=c('orange','darkgreen'),lwd=3
       ,legend=sprintf('%s (AUC=%0.3f)',c('RAI-A','Rockwood'),sapply(l_rocs,auc)));
#' 
#' ## Finding optimal threshold values for RAI-A and Rockwood
#' 
#' In order to do a fair comparison between 
#' 
#' 
#' # Discussion and Conclusions
#' 
#' # Acknowledgments
#' 
#' # References

