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
#' Revision: `r gitstamp(production=F)`.
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
                  # these ones are for the ROC threshold values
                  ,"True Negative"='tn'
                  ,"True Positive"='tp'
                  ,"False Negative"='fn'
                  ,"False Positive"='fp'
                  ,"Negative Predictive Value"='npv'
                  ,"Positive Predictive Value"='ppv'
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
#'   Frailty is the inability to maintain homeostasis when the human body 
#'   becomes challenged by a daily stressor [Torpy JM, Lymn C and Glass RM. Frailty in Older Adults. JAMA. 2006;296(18):2280. doi:10.1001/jama.296.18.2280].  Low physical activity, muscle weakness, slowed performance, fatigue and unintentional weight loss are all characteristics of frailty [Torpy JM, Lymn C and Glass RM. Frailty in Older Adults. JAMA. 2006;296(18):2280. doi:10.1001/jama.296.18.2280]. As the U.S. population ages, frailty will be a growing concern for the US. According to the U.S. Census Bureau Population Division, the U.S. population aged 65 and older is projected to increase by 45% (from 55 million to 80 million) by 2050 [U.S. Census Bureau, Population Division. 2012. “Table 12. Projections of the Population by Age and Sex for the United States: 2015 to 2060 (NP2012-T12)”]. This population group alone will make up approximately 20% of the U.S. population in 2050 [U.S. Census Bureau, Population Division. 2012. “Table 12. Projections of the Population by Age and Sex for the United States: 2015 to 2060 (NP2012-T12)”]. While it is true that the 45 to 64 age group has a higher percentage of ambulatory surgeries compared to the 65 and above age demographic, the risk of adverse surgical outcomes is higher in the latter compared to the former [Hall MJ, Schwartzman A, Zhang J and Liu X. Ambulatory Surgery Data From Hospitals and Ambulatory Surgery Centers: United States, 2010. Natl Health Stat Report. 2017 Feb;(102):1-15;  Polanczyk CA, Marcantonio E, Goldman L, et al. Impact of age on perioperative complications and length of stay in patients undergoing noncardiac surgery. Ann Intern Med. 2001;134(8):637- 643. doi:10.7326/0003-4819-134-8-200104170-00008]. Furthermore, the number of overnight hospital stays increases with age [Lucas JW, Benson V. Tables of Summary Health Statistics for the U.S. Population: 2015 National Health Interview Survey. National Center for Health Statistics. 2017. Available from:  http://www.cdc.gov/nchs/nhis/SHS/tables.htm ]. Frailty is associated with increased risk of post-operative complications and does increase with age [Brahmbhatt R, Brewster LP, Shafii S, et al. Gender and frailty predict poor outcomes in infrainguinal vascular surgery. J Surg Res. 201(1), 156-165. DOI: 10.1016/j.jss.2015.10.026]. Based on these findings, there seems to be an overlap between the frail population and the high medical need population (i.e. individuals that repeatedly visit the hospitals for serious health concerns). Therefore, frailty could be one way to identify a subpopulation of high-need patients prior to 30 day post-operative readmission. 
#'
#' There are many frailty metrics that were developed to identify frail patients. Generally, these metrics fall in two major categories: metrics that require a physical interaction with the patient (such as a physical assessment or completion of a questionnaire); and metrics obtained from a patient’s electronic health record (i.e. no patient contact). Inside an electronic health record (EHR) lies a wealth of billing, medical and sociodemographic information that could be used to 1) classify patients based on a frailty score and 2) predict the likelihood of a post-operative complication experienced by the patient. Since our strength is with EHR analysis, this study chose to focus on two frailty metrics that lend themselves well to EHR data: the Risk Analysis Index (RAI) and the Rockwood & Mitnitsky Frailty Index [Hall DE, Arya S, Schmid KK, et al. Development and Initial Validation of the Risk Analysis Index for Measuring Frailty in Surgical Populations. JAMA Surg. 2017 Feb 1;152(2):175-182. doi: 10.1001/jamasurg.2016.4202;  Mitnitski AB, Mogilner AJ and Rockwood K. Accumulation of deficits as a proxy measure of aging. Scientific World Journal. 2001 Aug 8;1:323-36]. The RAI-A (RAI Administrative) frailty metric is a frailty screening tool 
#'
#'   
#' # Methods
#' 
#' A total of 6408 University Hospital System (UHS) cases were used in this study. These cases occurred between April 2013 to February 2017. A total of 140 annotated and highly curated variables were extracted from each EHR, among them was address. In order to determine a median income approximation for each case, we linked the census block group level data from the 2015 American Community Survey Median Income table to each cases’ address. To calculate the the RAI-A frailty score for each case, we used the calculation described in Hall DE, Arya S, Schmid KK, et al. Development and Initial Validation of the Risk Analysis Index for Measuring Frailty in Surgical Populations. JAMA Surg. 2017 Feb 1;152(2):175-182. doi: 10.1001/jamasurg.2016.4202. In short, the calculation can be seen here:
#' <div id="rai">
#'   <img src="RAI-Cv2.png" alt="">
#' </div> 
#' Additionally, the Rockwood index was calculated for each case using the following paper: Mitnitski AB, Mogilner AJ and Rockwood K. Accumulation of deficits as a proxy measure of aging. Scientific World Journal. 2001 Aug 8;1:323-36. Briefly, the Rockwood index for each patient is calculated by the following equation: 
#' <div id="rai">
#'   <img src="FormulaV2.png" alt="">
#' </div>
#' 
#' ## Data Sources
#' 
#' The following data sources were used:
#' 
#' * University Hospital System (UHS) electronic health records (EHR)
#' * 2015 American Community Survey 5-year estimates Median Income (from years 2011 to 2015) 
#' 
#' ## Patient Demographics
#+ table_demog,results='asis'
mutate(sbs0$all$all_emergency,a_rai=factor(a_rai>median(a_rai)
                                             ,levels = c('FALSE','TRUE')
                                             ,labels=c('Low','High'))) %>% 
  mapnames(thecolnames1) %>% 
  CreateTableOne(names(thecolnames1)[5:9],'RAI-A',.) %>% 
  print(printToggle=F,noSpaces=T) %>% `[`(-4) %>% 
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
# what if we cut them along their optimal thresholds?
fit_srvs_optcut <- list(`RAI-A`=survfit(Surv(a_t,a_c) ~ I(a_rai>9.5)
                                 , data = sbs0$all$all_emergency,subset=a_t>0)
                 ,Rockwood=survfit(Surv(a_t,a_c) ~ I(a_rockwood>0.264245)
                                   , data = sbs0$all$all_emergency,subset=a_t>0));
pl_srvs <- mapply(function(aa,bb) autoplot(aa,ylim=c(0.5,1),xlim=c(0,30)) + 
                    ggtitle(paste0(bb,' as a predictor of  30-day outcomes')) +
                    scale_y_continuous(labels=scales::percent) +
                    scale_color_discrete(bb,labels=c('Low','High')) +
                    scale_fill_discrete(bb,labels=c('Low','High')) +
                    labs(x='Days Post-Surgery',y='Readmission-Free Survival')
                  ,fit_srvs,names(fit_srvs),SIMPLIFY = F);
pl_srvs_optcut <- mapply(function(aa,bb) autoplot(aa,ylim=c(0.5,1),xlim=c(0,30)) + 
                    ggtitle(paste0(bb,' as a predictor, OPTCUT')) +
                    scale_y_continuous(labels=scales::percent) +
                    scale_color_discrete(bb,labels=c('Low','High')) +
                    scale_fill_discrete(bb,labels=c('Low','High')) +
                    labs(x='Days Post-Surgery',y='Readmission-Free Survival')
                  ,fit_srvs_optcut,names(fit_srvs),SIMPLIFY = F);

# these are duplicates of the plots created above and rendered by multiplot() 
# below
#ggsurvplot(surv.rai);
#ggsurvplot(surv.rock);
#multiplot(plotlist=c(pl_srvs,pl_srvs_optcut),cols=2);
multiplot(plotlist=pl_srvs,cols=1);
#'
#+ results='asis'
t_coxresults <- sapply(fit_coxs<-list(`RAI-A`=cox.rai.train,`Rockwood`=cox.rock.train)
                       ,function(xx) cbind(tidy(xx),glance(xx)),simplify=F) %>% 
  do.call('rbind',.) %>% `[`(,c('n','nevent','estimate','std.error','statistic'
                                ,'p.value','r.squared','concordance'
                                ,'std.error.concordance','AIC','BIC'));
mapnames(t_coxresults,thecolnames1) %>% t %>% kable(format = 'markdown',digits=4);
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
#' As can be seen from table 2, the concordances are `r do.call(sprintf,c('%0.2f (SE=%0.2f)',t_coxresults['RAI-A',c('concordance','std.error.concordance')]))`
#' and `r do.call(sprintf,c('%0.2f (SE=%0.2f)',t_coxresults['Rockwood',c('concordance','std.error.concordance')]))` for
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
#' ## Optimal threshold values for RAI-A and Rockwood
#' 
#' To enable a fair comparison between the two frailty scores, we used 
#' Youden's Index [@YoudenIndexratingdiagnostic1950] to find for each of them
#' the threshold value that maximized sensitivity and specificity.
#' 
#' #### Optimal threshold scores for RAI-A and Rockwood
#+ results='asis'
lapply(l_rocs,coords,'b',ret=c('threshold','sensitivity','specificity','accuracy'
                               #,'tn','tp','fn','fp'
                               ,'npv','ppv','precision','recall')) %>% 
  lapply(mapnames,thecolnames1) %>% 
  lapply(function(xx) setNames(xx,capitalize(names(xx)))) %>% data.frame %>% 
  mapnames(thecolnames1) %>% kable(format='markdown',digits=3);
#' 
#' # Discussion and Conclusions
#' 
#' # Acknowledgments
#' 
#' # References

