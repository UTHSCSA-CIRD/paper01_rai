#' ---
#' title: "RAI Main Analysis"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' bibliography: Frailty.bib
#' ---
#'
#+ echo=FALSE, message=FALSE
# output params backup
#   html_document:
#     keep_md: true
#   word_document:
#     keep_md: true
#     reference_docx: styletemplate.docx
knitr::opts_chunk$set(echo=F,warning = F,cache=F,message=F);
options(knitr.kable.NA='');
#+ cache=FALSE
source('global.R');
#' Report date: `r date()`.
#'
#' Revision: `r gitstamp(production=T)`.
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
                  ,"Cum Count"='cumul_count'
                  ,"Died 30d"="postop_death_30_dy_proc_n"
                  ,"Died 30d Frac."="postop_death_30_dy_proc_frac"
                  ,"Readmit 30d"="a_readm_30_dy_n"
                  ,"Readmit 30d Frac."="a_readm_30_dy_frac"
                  # here are the to-replace column names for tidy/glance
                  ,"N"='n'
                  ,"Events"='nevent'
                  ,"Effect"='estimate'
                  ,"Std. Err."='std.error'
                  ,"Effect/SE"='statistic'
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
# this is a list of functions from survAUC that return predictive accuracy metrics
c_auclist<-c('AUC.cd','AUC.hc','AUC.sh','AUC.uno','BeggC','GHCI','Nagelk','OXS','UnoC','XO');

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
#' * Rockwood Index can be calculated on anything, tolerates missing data, and 
#'   therefore it can be calculated not only on manually curated registry data 
#'   but on any other sources of laboratory values and diagnoses including 
#'   billing records and EMR systems
#' * Rockwood has been thoroughly vetted by geriatricians and its statistical 
#'   assumptions have been explicitly tested. 
#' * Rockwood could be used for automated risk-calculation to support surgical 
#'   decisions it it could be shown that it is as good as or better than RAI-A 
#'   for predicting surgical outcomes.
#' 
#' Frailty is the inability to maintain homeostasis when the human body becomes 
#' challenged by a daily stressor [@torpy2006frailty]. Low physical activity, 
#' muscle weakness, slowed performance, fatigue and unintentional weight loss 
#' are all characteristics of frailty [@torpy2006frailty]. As the U.S.
#' population ages, frailty will be a growing concern for the US. According to
#' the U.S. Census Bureau Population Division, the U.S. population aged 65 and
#' older is projected to increase by 45% (from 55 million to 80 million) by 2050
#' [@bureau_2014_nodate]. This population group alone will make up approximately
#' 20% of the U.S. population in 2050 [@bureau_2014_nodate]. While it is true
#' that the 45 to 64 age group has a higher percentage of ambulatory surgeries
#' compared to the 65 and above age demographic, the risk of adverse surgical
#' outcomes is higher in the latter compared to the former 
#' [@hall2017ambulatory]; @polanczyk2001impact]. Furthermore, the number of
#' overnight hospital stays increases with age [@lucas_nhis_2018]. Frailty is
#' associated with increased risk of post-operative complications and does
#' increase with age [@brahmbhatt2016gender]. Based on these findings, there
#' seems to be an overlap between the frail population and the high medical need
#' population (i.e. individuals that repeatedly visit the hospitals for serious
#' health concerns). Therefore, frailty could be one way to identify a
#' subpopulation of high-need patients prior to 30 day post-operative
#' readmission.
#' 
#' There are many frailty metrics that were developed to identify frail 
#' patients. Generally, these metrics fall in two major categories: metrics that
#' require a physical interaction with the patient (such as a physical 
#' assessment or completion of a questionnaire); and metrics obtained from a 
#' patient’s electronic health record (i.e. no patient contact). Inside an 
#' electronic health record (EHR) lies a wealth of billing, medical and 
#' sociodemographic information that could be used to 1) classify patients based
#' on a frailty score and 2) predict the likelihood of a post-operative 
#' complication experienced by the patient. Since our strength is with EHR 
#' analysis, this study chose to focus on two frailty metrics that lend 
#' themselves well to EHR data: the Risk Analysis Index (RAI) and the Rockwood &
#' Mitnitsky Frailty Index [@HallDevelopmentInitialValidation2017;
#' @MitnitskiAccumulationDeficitsProxy2001,]. The RAI-A (RAI Administrative) frailty metric is
#' a frailty screening tool that uses administrative data (like EHR) to 
#' differentiate between frail and fit patients opting for elective surgery 
#' [@HallDevelopmentInitialValidation2017]. The RAI-A score is calculated using
#' 11 variables that can be extracted from the EHR, such as medical
#' co-morbidities, cognitive decline and activities of daily living (ADLs)
#' [@HallDevelopmentInitialValidation2017]. This frailty metric was developed to
#' be implemented quickly and efficiently
#' [@HallDevelopmentInitialValidation2017]. If one variable is missing, however,
#' the RAI-A score could lead to an underestimate of a patient’s frailty
#' assessment. The Rockwood index, on the other hand, uses 30 unique lab values
#' (such as serum creatinine levels, serum albumin level, etc.) found in a
#' patient’s EHR to calculate a patient’s frailty score. An advantage to the
#' Rockwood is that it is a robust frailty metric for missing lab values. 
#' Missing lab values will not lead to an underestimate of a patient’s frailty 
#' assessment determined by the Rockwood index [@MitnitskiAccumulationDeficitsProxy2001,]. 
#' Both tools can be used retrospectively using EHRs.…
#' 
#' 
#' # Methods
#' 
#' A total of 6408 University Hospital System (UHS) cases were used in this 
#' study. These cases occurred between April 2013 to February 2017. A total of 
#' 140 variables were extracted from the local copy of the data reported to
#' NSQIP, and supplemented with address data (for purposes of linking
#' socioeconomic variables) from the UHS and UTMedicine electronic medical
#' record (EMR) systems. In order to determine a median income approximation for
#' each case, we linked the census block group level data from the 2015 American
#' Community Survey Median Income table to each cases’ address. To calculate the
#' the RAI-A frailty score for each case, we used the calculation described in
#' [@HallDevelopmentInitialValidation2017] and [@MitnitskiAccumulationDeficitsProxy2001,] 
#' respectively. The RAI-A was calculated as follows:

# Using the include_graphics() command rather than HTML allows this to render 
# properly in Word (and hopefully PDF?)
include_graphics('RAI-Cv2.png');

#' Additionally, the Rockwood index was calculated as follows:
#+ eqn_rock, out.width="20%"
include_graphics('FormulaV2.png');
#' 
#' ## Patient Demographics
#' 

#' The UTHSCSA data warehouse contains EHR and billing data from our faculty
#' practice plan and from University Hospital System (UHS) linked into one
#' coherent dataset in an i2b2 data warehouse [MurphyInstrumentinghealthcare2009]. 
#' UHS is a nationally recognized academic medical center, network of outpatient clinics
#' strategically located in at-risk communities, and a Level I trauma center.
#' UHS is the largest SNH in South Texas and treats a predominately Hispanic
#' population. This is reflected in the patient counts in Table 1.
#'
#' #### Table 1. Patient Demographics
#' 
#+ table_demog,results='asis'
mutate(sbs0$all$all_emergency,t_strata=factor(a_c==1
                                             ,levels = c('FALSE','TRUE')
                                             ,labels=c('No Event','Death/Readmission'))) %>% 
  mapnames(thecolnames1) %>% 
  CreateTableOne(names(thecolnames1)[3:9],'t_strata',.) %>% 
  print(printToggle=F,noSpaces=T) %>% `[`(,-4) %>% 
  kable(format='markdown');

#' 
#' Surgeries vary greatly by their level of invasiveness and risk. For the 
#' results reported here we extracted only  surgeries in order to mitigate 
#' possible selection bias due to frail patients and providers opting out of 
#' elective surgeries. Furthermore, though the at-risk sample size was smaller
#' there were a larger proportion of observed deaths and readmissions making this
#' a tractable dataset for initial model development.
#' 
#' In the 2013-2017 NSQIP data there were 541 emergency cases. These
#' were randomly assigned to one of three subsets: a training set 
#' (N=`r length(pat_samples$train)`) and a test set (N=`r length(pat_samples$test)`). 
#' The remaining set (N=`r length(pat_samples$train)`) is being
#' held out for future analysis and was not used in the work reported here.

#' ## Analysis
#' 
#' For the `r length(pat_samples$train)` surgery cases in the training set we fit
#' Cox proportional hazard models using RAI-A  and the Rockwood index as
#' predictors. The outcome being measured was time until readmission or all-cause
#' after surgery, with a 30-day followup. 

#' 
#' # Results
#' 
#' ## RAI-A and Rockwood both are reasonable predictors of 30-day mortality and readmission
#' 
#' #### Figure 1. Predicting post-surgical survival and recovery
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

multiplot(plotlist=pl_srvs,cols=1);
#' The shaded areas represent 95% confidence intervals. On the x-axis day
#' 0 represents the respective dates of surgeries for the patients, and continues
#' out to 30 days. In each plot, the patients are grouped according to whether the
#' corresponding frailty score is above the median (blue) or below the median 
#' (pink). For both RAI-A and Rockwood, patients scores in the upper half for
#' the cohor die earlier and higher rates.
#'   
#' #### Table 2. Results of Cox survival model fits
#'
#+ results='asis'
t_coxresults <- sapply(fit_coxs<-list(`RAI-A`=cox.rai.train,`Rockwood`=cox.rock.train)
                       ,function(xx) cbind(tidy(xx),glance(xx)),simplify=F) %>% 
  do.call('rbind',.) %>% `[`(,c('n','nevent','estimate','std.error','statistic'
                                ,'p.value','r.squared','concordance'
                                ,'std.error.concordance','AIC','BIC'));
mapnames(t_coxresults,thecolnames1) %>% t %>% kable(format = 'markdown',digits=4);
#' 
#' In Table 2 we can see that even on this relatively small sample size, both 
#' measures of frailty are significantly (RAI-A `r sprintf('p = 
#' %0.4f',t_coxresults['RAI-A','p.value'])`, Rockwood `r sprintf('p = 
#' %0.4f',t_coxresults['Rockwood','p.value'])`) associated with risk of 
#' mortality or readmission.. In both cases the 'Effect' row represents the 
#' natural logarithm of the increase in risk per unit change in the frailty 
#' score. These scores have different scales but a comparison can be made by
#' dividing them by their standard errors ('Effect/SE', also known as the Wald
#' statistic).and these standardized values are close to each other. Their 
#' Akaike Information Criteria and Bayes Information Criteria are also close to
#' each other.
#' 

#' 
#' Tables 3a and 3b show that the fraction of patients dying as well as the
#' fraction being readmitted within 30 days increases with increasing RAI-A and
#' increasing rockwood. In the highest risk brackets there are too few patients
#' reliably measure a rate.
#' 

#' #### Table 3a. Event frequencies broken down by RAI-A range.
#' 
#+ table_raicounts, results='asis'
countfrac(sbs0$all$all_emergency,groupcols = 'rai_range') %>% 
  mapnames(thecolnames1) %>% kable(format='markdown',digits = 2);

#' #### Table 3b. Event frequencies broken down by Rockwood range
#' 
#+ table_rockcounts, results='asis'
countfrac(sbs0$all$all_emergency,groupcols = 'a_rockwood_range') %>% 
  mapnames(thecolnames1) %>% kable(format='markdown',digits = 2);
#' 
#' ## RAI-A and Rockwood have equivalent concordances and AUCs
#' 
#' As can be seen from Tables 2, the concordances are `r
#' do.call(sprintf,c('%0.2f
#' (SE=%0.2f)',t_coxresults['RAI-A',c('concordance','std.error.concordance')]))`
#' and `r do.call(sprintf,c('%0.2f
#' (SE=%0.2f)',t_coxresults['Rockwood',c('concordance','std.error.concordance')]))`
#' for the Cox models whose predictors are RAI-A and Rockwood, respectively.
#' Their Receiver-Operator Characteristic (ROC) curves can be seen in Figure 2,
#' along with their areas under the curve (AUCs).
#' 

#' 
#' #### Figure 2. ROC curves for RAI-A and Rockwood
#' 
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

#' 
#' To enable a fair comparison between the two frailty scores, we used Youden's
#' Index [@YoudenIndexratingdiagnostic1950] to find for each of them the
#' threshold value that maximized sensitivity and specificity.
#' 

#' 
#' #### Table 4. Optimal threshold scores for RAI-A and Rockwood
#+ tab_snsp, results='asis'
lapply(l_rocs,coords,'b',ret=c('threshold','sensitivity','specificity','accuracy'
                               ,'npv','ppv','precision','recall')) %>% 
  lapply(mapnames,thecolnames1) %>% 
  lapply(function(xx) setNames(xx,capitalize(names(xx)))) %>% data.frame %>% 
  mapnames(thecolnames1) %>% kable(format='markdown',digits=3);
#' 
#' #### Table 5a. and 5b.
#' 
#+ tab_errormtx, results='asis'
lapply(l_rocs,coords,'b',ret=c('tn','fn','fp','tp')) %>% 
  lapply(mapnames,thecolnames1) %>% 
  lapply(function(xx) paste(names(xx),'=',xx)) %>% 
  lapply(matrix,nrow=2,dimnames=list(c('No Events','Death/Readmission')
                                     ,c('Low Risk','High Risk'))) %>% 
  lapply(kable,format='markdown') %>% mapnames(thecolnames1) %>% 
  setNames(.,paste0(names(.),' Error Matrix')) %>% capture.output() %>% 
  gsub('^[$`]{1,2}','\n\n#### ',.) %>% gsub('`','',.) %>% cat(sep='\n');
# Now we create a data.frame containing predictors and outcomes for both the training
# and the test data. First the outcomes and raw predictors.
#+ prep_coxauc
crossval_cox <- lapply(sbs0[c('train','test')],function(xx) {
  with(subset(xx$all_emergency,a_t>0)
       ,data.frame(resp=Surv(a_t,a_c),a_rai,a_rockwood))});
# now the Cox linear predictors
for(ii in names(crossval_cox)) for(jj in names(fit_coxs)) 
  crossval_cox[[ii]][[jj]] <- predict(fit_coxs[[jj]],newdata = crossval_cox[[ii]]);
# run the panel of AUC tests on them
auc_coxs <- sapply(names(fit_coxs),function(xx) {
  with(crossval_cox
       ,survAUC(Surv.rsp=train$resp,Surv.rsp.new = test$resp
                ,lp=eval(train[[xx]]),lpnew = eval(test[[xx]])
                ,times=1:30,FUNS=c_auclist))
  },simplify=F);
# Now tabulate the results;
t_auccox <- sapply(auc_coxs,sapply,function(xx) 
  if(length(xx)>1&&'iauc' %in% names(xx)) xx[['iauc']] else 
    if(length(xx)==1 && is.numeric(xx)) xx else NaN);
#' 
#' #### Table 6. RAI-A and Rockwood compared on their ability to predict death or readmission in the validation sed using a panel of predictive accuracy metrics.
#+ tab_coxauc, results='asis'
kable(t_auccox,format='markdown',digits=3);
#' # Discussion and Conclusions
#' 
#' In the population demographic table, approximately 22% of the 541 cases
#' resulted in death or hospital readmission within 30 days after surgery.
#' There appears to be a significant difference between “No Event” vs “Event” 
#' RAI-A and Rockwood scores. The RAI-A and Rockwood scores are higher in the
#' “Event” cases compared to “No Event.” Additionally, age is higher in the
#' “Event” population in comparison. Interestingly, it appears that the Rockwood 
#' has a higher standard error margin compared to RAI-A. 
#'
#' Both the RAI-A and Rockwood survival curves depict a significant difference
#' between the upper 50% RAI-A and Rockwood scores compared to the lower 50%.
#' Unlike the confidence intervals (the shaded regions) of the RAI-A survival
#' curves, the Rockwood survival curves overlap slightly. This seems to
#' corroborate the earlier observation of a slightly larger standard error margin
#' with the Rockwood metric compared to RAI-A. The subsequent survival curve
#' model summary table also depicts the same story. Even on the training set,
#' the RAI-A model performs slightly better than Rockwood, as evidenced by the
#' standard error and AIC score.
#'
#' A visible trend that can be seen in the Event Frequencies tables. Generally
#' speaking, the death and readmission incidence increases as the frailty score
#' increases. 
#'
#' Finally we calculated the predictive ability of both frailty metrics. Although
#' the AUC demonstrates that both models are comparable in detecting 30 day
#' post-operative complications, the subsequent table reveals that the RAI-A is
#' more specific in detecting true post-operative complication events. 
#'
#' Both Rockwood and the RAI-A frailty metrics are powerful frailty metrics that
#' can be implemented using EHR data. Based on our results, it appears that the
#' RAI-A slightly outperforms the Rockwood however due to a low event rate (~22%),
#' more data points need to be added and therefore more validation is needed.
#' Nevertheless, we are the first to demonstrate the predictive power both frailty
#' metrics have in detecting post-operative complications. Based on the results
#' from this study, RAI-A may have slightly more power in detecting real
#' post-operative complication rates and therefore increased ability to pre-emptively
#' identify high-need patients prior to death and a hospital readmission. In doing
#' so will lead to optimized patient care as we strive for precision medicine.
#' 
#' # Acknowledgments
#' 
#' # References

