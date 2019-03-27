#' ---
#' title: "Side by Side Comparison of Rockwood Index (RI) and Risk Analysis Index (RAI-A) as Predictors of Surgical Readmissions at a Texas Safety Net Hospital"
#' author: "Alex F. Bokov *, PhD, MS, Desiree S. Wilson, PhD, Sara E. Espinoza, MD, Paula K. Shireman, MD"
#' date: "02/23/2018"
#' bibliography: Frailty.bib
#' csl: ieee.csl
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
options(runr.prodgitstamp=F);
# Report date: `r Sys.Date()`.
#'
# #### Audit Info
#' 
#+ cache=TRUE,echo=FALSE,results='hide'
source('run.R');
#+ source_global,results='hide'
source('global.R');
#+ poster_variables
thecolnames1 <- c("RAI-A Score"='rai_range'
                  ,"RI Score"='a_rockwood_range'
                  ,"RAI-A"='a_rai'
                  ,"RI"='a_rockwood'
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
#'
#'
#+ spacerplot, fig.width=2, fig.height=6
par(bg='#FFFFFF00'); 
plot(0,type='n',ylab='',xlab='',axes=F);
#'
#' # Abstract
#' Frailty, the loss of ability to maintain homeostasis under everyday stressors, is associated with increased risk of post-operative complications. Screening for frailty could lead to better outcomes in vulnerable populations. Here we compare two frailty metrics : the Rockwood Index (RI) and the Administrative Risk Analysis Index (RAI-A). Both are calculated using registry data but RI is feasible to calculate directly from the electronic health record (EHR).
#' 
#' # Introduction
#' 
# * RAI-A good for predicting surgical risk [@HallDevelopmentInitialValidation2017] 
# * ...but it cannot be used on live data because it's hard-coded to NSQIP 
# * The availability of NSQIP variables can differ between sites with different 
#   levels of participation and some variables may be discontinued. 
# * RI can be calculated on anything, tolerates missing data, and 
#   therefore it can be calculated not only on manually curated registry data 
#   but on any other sources of laboratory values and diagnoses including 
#   billing records and EHR systems
# * RI has been thoroughly vetted by geriatricians and its statistical 
#   assumptions have been explicitly tested. 
# * RI could be used for automated risk-calculation to support surgical 
#   decisions it it could be shown that it is as good as or better than RAI-A 
#   for predicting surgical outcomes.
# 
#' 
#' Frailty is the inability to maintain homeostasis when the human body becomes 
#' challenged by a daily stressor. Low physical activity, muscle weakness, 
#' slowed performance, fatigue and unintentional weight loss are all 
#' characteristics of frailty [@torpy2006frailty]. With the 65 and older 
#' population increasing 45% (from 55 million to 80 million, approximately 20% 
#' of the total population) by 2050 in the US [@bureau_2014_nodate] frailty will
#' become even more important. Frailty is associated with increased risk of 
#' post-operative complications and while frailty increases with age, age alone 
#' does not predict frailty [@brahmbhatt2016gender]. Screening for frailty and 
#' designing care pathways more suitable for frail patients could lead to better
#' outcomes in this vulnerable patient population.
#' 
#' 
#' Multiple frailty metrics have been developed and fall in two major 
#' categories: 1) functional phenotype by Fried [@FriedFrailtyOlderAdults2001] 
#' with a focus on physical function assessment and 2) the deficit accumulation 
#' model developed by Rockwood [@MitnitskiAccumulationDeficitsProxy2001] which 
#' can use retrospective data including electronic health records (EHR). Our 
#' study compares the the Risk Analysis Index (RAI) which is a functional score
#' with the Rockwood Index (RI) which represents deficit accumulation. 
#' 
#' The RAI-A (administrative RAI) 
#' [@MelinPreoperativefrailtyRisk2015;@HallDevelopmentInitialValidation2017] 
#' can be calculated retrospectively while the RAI-C
#' (clinical RAI) can be collected prospectively in clinics with brief 
#' questionnaires that are minimally disruptive to patient flow 
#' [@HallDevelopmentInitialValidation2017].
#' 
#' RAI-A uses 11 variables corresponding to those used by the American College of 
#' Surgeons National Surgical Quality Improvement Program (NSQIP) registry, including 
#' medical co-morbidities and functional status [@MelinPreoperativefrailtyRisk2015]. 
#' If one variable is missing, however, the RAI-A could lead to an underestimate 
#' of a patient’s frailty. In our data, one such variable-- mental impairment--
#' was not available. Moreover, RAI-A was adapted to surgical outcomes from 
#' an earlier frailty score [@PorockMDSMortalityRisk2010] that was trained on a 
#' completely different dataset and population without re-fitting nor 
#' re-validating the regression model on which it was based against the new data.
#' 

#' On the other hand the RI [@MitnitskiAccumulationDeficitsProxy2001] does not 
#' depend on one specific set of variables–
#' it is simply an unweighted fraction of variables with abnormal values
#' relative to the total number of variables under consideration. The only
#' stipulation is that the variables should be chosen to represent a diverse set
#' of different systems. RI is robust against missing data and comparison of
#' different data sources, though until now it has not been applied to NSQIP
#' data nor surgical outcomes. If RI can be shown to be non-inferior to RAI-A,
#' then RI may facilitate many new decision-support and quality-improvement 
#' applications on large EHR-derived datasets because of its acceptance in the 
#' frailty field and the fact that it is not dependent on a curated dataset like 
#' RAI-A is nor on patient questionnaires like RAI-C
#' 
#' 
#' # Methods
#' 
# A total of 6408 University Hospital System (UHS) cases were used in this 
# study. These cases occurred between April 2013 to February 2017. A total of 
# 140 variables were extracted from the local copy of the data reported to
# NSQIP, and supplemented with address data (for purposes of linking
# socioeconomic variables) from the UHS and UTMedicine electronic medical
# record (EHR) systems. In order to determine a median income approximation for
# each case, we linked the census block group level data from the 2015 American
# Community Survey Median Income table to each cases’ address. To calculate the
# the RAI-A frailty score for each case, we used the calculation described in
# [@HallDevelopmentInitialValidation2017] and [@MitnitskiAccumulationDeficitsProxy2001,] 
# respectively. The RAI-A was calculated as follows:

# Using the include_graphics() command rather than HTML allows this to render 
# properly in Word (and hopefully PDF?)
#include_graphics('RAI-Cv2.png');

# Additionally, the Rockwood index was calculated as follows:
# eqn_rock, out.width="20%"
#include_graphics('FormulaV2.png');
#' 
#' ## Patient Demographics
#' 

#' The UTHSCSA i2b2 data warehouse [@MurphyInstrumentinghealthcare2009] contains 
#' EHR and billing data from our faculty
#' practice plan and from University Hospital System (UHS). 
#' UHS is a nationally recognized academic medical center, network of outpatient clinics
#' strategically located in at-risk communities, and a Level I trauma center.
#' It is the largest safety net hospital in South Texas and treats a predominately Hispanic
#' population. 
#' 
#' Surgeries vary greatly by their level of invasiveness, risk, and urgency. 
#' Initially we are focusing on emergency surgeries to test the
#' accuracy of the RAI-A in a group that has a higher death and complication rate
#' secondary to frailty and also factors unrelated to frailty.
#' 
#' In the 2013-2017 ACS NSQIP data there were 541 emergency cases. These
#' were randomly assigned to one of three subsets: a training set 
#' (N=`r sum(sbs0$all$all_emergency$idn_mrn %in% pat_samples$train)`) and a 
#' validation set 
#' (N=`r sum(sbs0$all$all_emergency$idn_mrn %in% pat_samples$test)`). All Cox 
#' models reported in Tables 2, 5a, and 5b were first fitted on the training set and their
#' predictions validated against the validation set to obtain the panel of 
#' predictive accuracy metrics in Table 6.
#' The remaining test set 
#' (N=`r sum(sbs0$all$all_emergency$idn_mrn %in% pat_samples$val)`) is being
#' held out for future analysis and was not used in the work reported here.
#' 
#' However, all `r nrow(sbs0$all$all_emergency)` cases were used for plotting 
#' survival in Figure 1, the 
#' demographic summary in Table 1, and the event frequencies in Tables 3a and 3b
#' since the purpose of all these is cohort characterization rather prediction 
#' or hypothesis testing.
#'
#' #### Table 1. Patient Demographics
#' 
#+ table_demog,results='asis'
mutate(sbs0$all$all_emergency,t_strata=factor(a_c==1
                                             ,levels = c('FALSE','TRUE')
                                             ,labels=c('Event-Free','Death/Readmission'))) %>% 
  mapnames(thecolnames1) %>% 
  CreateTableOne(names(thecolnames1)[3:9],'t_strata',.) %>% 
  assign('t_demog',.,envir=.GlobalEnv) %>% 
  print(printToggle=F,noSpaces=T) %>% `[`(,-4) %>% 
  kable(format='markdown');
#'
#' RAI-A is measured on a continuous scale from 0-81 with higher scores being 
#' associated with frailty. RI is measured on a scale of 0 to 1, with 0
#' meaning that there are no functional defecits at all and 1 meaning that 
#' a functional deficit was reported by every data element capable of doing so.
#' 

#' 
#' ## Analysis
#' 
#' For the `r sum(sbs0$all$all_emergency$idn_mrn %in% pat_samples$train)` 
#' surgery cases in the training set we fit
#' Cox proportional hazard models using either RAI-A  or the RI as
#' predictors. The outcome being measured was time until readmission. 
#' The survival plots in Figure 1 show the time elapsed until readmission 
#' during the 30 days after surgery.
#' 
#' 
#' # Results
#' 
#' ## RAI-A and RI both are predictors of 30-day readmission
#' 
#' #### Figure 1. Predicting post-surgery readmission
#+ plot_survfits, fig.width=10, fig.height=8
fit_srvs <- list(`RAI-A`=survfit(Surv(a_trdm,a_c) ~ I(a_rai>median(a_rai))
                            , data = sbs0$all$all_emergency,subset=a_t>0)
                ,RI=survfit(Surv(a_trdm,a_c) ~ I(a_rockwood>median(a_rockwood))
                            , data = sbs0$all$all_emergency,subset=a_t>0));
# what if we cut them along their optimal thresholds?
fit_srvs_optcut <- list(`RAI-A`=survfit(Surv(a_t,a_c) ~ I(a_rai>2) # 9.5 # 2
                                 , data = sbs0$all$all_emergency,subset=a_t>0)
                 ,RI=survfit(Surv(a_t,a_c) ~ I(a_rockwood>0.137) #0.264245 # 0.137
                                   , data = sbs0$all$all_emergency,subset=a_t>0));
pl_srvs <- mapply(function(aa,bb) autoplot(aa) +
                                           #,ylim=c(0.5,1),xlim=c(0,30),asp=1.45) + 
                    ggtitle(paste0(bb,' as a predictor of 30-day readmission')) +
                    scale_y_continuous(labels=scales::percent,limits = c(0.5,1)) +
                    scale_color_discrete(bb,labels=c('Low','High')) +
                    scale_fill_discrete(bb,labels=c('Low','High')) +
                    labs(x='Days Post-Surgery',y='Readmission-Free Survival') +
                    theme_light(base_size=19) + 
                    guides(color=guide_legend(title='')
                           ,fill=guide_legend(title=''))
                  ,fit_srvs,names(fit_srvs),SIMPLIFY = F);
pl_srvs_optcut <- mapply(function(aa,bb) autoplot(aa,ylim=c(0.5,1),xlim=c(0,30)) + 
                    ggtitle(paste0(bb,' as a predictor, OPTCUT')) +
                    scale_y_continuous(labels=scales::percent) +
                    scale_color_discrete(bb,labels=c('Low','High')) +
                    scale_fill_discrete(bb,labels=c('Low','High')) +
                    labs(x='Days Post-Surgery',y='Readmission-Free Survival')
                  ,fit_srvs_optcut,names(fit_srvs),SIMPLIFY = F);

multiplot(plotlist=pl_srvs,cols=1);
#' 
#' The lines are Kaplan-Meier curves with the x-axis representing time from
#' surgery until first readmission (30-day follow-up).and the y-axis represents
#' percent of patients who have not yet had their first re-admission. The shaded
#' areas represent 95% confidence intervals. The 'High' and 'Low' lines
#' represent patients binned into the upper and lower halves of RAI-A and RI in
#' the respective plots.
#'   
#' #### Table 2. Results of Cox survival model fits
#'
#+ results='asis'
t_coxresults <- sapply(fit_coxs<-list(`RAI-A`=cox.rai.train,`RI`=cox.rock.train)
                       ,function(xx) cbind(tidy(xx),glance(xx)),simplify=F) %>% 
  do.call('rbind',.) %>% `[`(,c('n','nevent','estimate','std.error','statistic'
                                ,'p.value','r.squared','concordance'
                                ,'std.error.concordance','AIC','BIC'));
mapnames(t_coxresults,thecolnames1) %>% t %>% kable(format = 'markdown',digits=4);
#' 
#' Table 2 shows that even on this relatively small sample size, both 
#' measures of frailty are significantly (RAI-A `r sprintf('p = 
#' %0.4f',t_coxresults['RAI-A','p.value'])`, RI `r sprintf('p = 
#' %0.4f',t_coxresults['RI','p.value'])`) associated with risk of 
#' readmission. The 'Events' row represents the number of cases
#' of the `r with(fit_coxs[[1]],n+nevent)` where the patient was readmitted.
#' 
# In both cases the 'Effect' row represents the 
# natural logarithm of the increase in risk per unit change in the frailty 
# score. These scores have different scales but a comparison can be made by
# dividing them by their standard errors ('Effect/SE', also known as the Wald
# statistic).and these standardized values are close to each other. 
#' 
#' 
#' Tables 3a and 3b show patient 30-day mortality and readmissions increases with 
#' increasing RAI-A and RI. In the highest risk brackets there are too few patients
#' reliably measure a rate.
#' 

#' #### Table 3a. Event frequencies broken down by RAI-A range.
#' 
#+ table_raicounts, results='asis'
countfrac(sbs0$all$all_emergency,groupcols = 'rai_range') %>% 
  mapnames(thecolnames1) %>% kable(format='markdown',digits = 2);

#' #### Table 3b. Event frequencies broken down by RI range
#' 
#+ table_rockcounts, results='asis'
countfrac(sbs0$all$all_emergency,groupcols = 'a_rockwood_range') %>% 
  mapnames(thecolnames1) %>% kable(format='markdown',digits = 2);
#' 
#' ## RAI-A and RI have similar concordances and AUCs
#' 
# As can be seen from Tables 2, the concordances are 
# `r do.call(sprintf,c('%0.2f
# (SE=%0.2f)',t_coxresults['RAI-A',c('concordance','std.error.concordance')]))`
# and `r do.call(sprintf,c('%0.2f
# (SE=%0.2f)',t_coxresults['RI',c('concordance','std.error.concordance')]))`
# for the Cox models whose predictors are RAI-A and Rockwood, respectively. Both
# concordances pass the traditional threshold because the lower bounds of both
# their 95%$ confidence intervals are greater than 0.5
#' 
#' For RAI-A the concordance was `r round(concordance(fit_coxs[['RAI-A']])$conc,3)`
#' (SE `r round(sqrt(concordance(fit_coxs[['RAI-A']])$var),3)`) and for RI it was
#' `r round(concordance(fit_coxs$RI)$conc,3)`
#' (SE `r round(sqrt(concordance(fit_coxs$RI)$var),3)`). The Receiver-Operator 
#' Characteristic (ROC) curves can be seen in Figure 2, along with their 
#' respective areas under the curve (AUCs).
#' 
binomnull <- glm(a_any_cd4~1,data=sbs0$train$all_emergency,family='binomial');
l_binomfits <- sapply(c('a_rai','a_rockwood')
                     ,function(xx) update(binomnull,paste0('.~',xx)),simplify=F);
l_binomrocs <- with(sbs0$test,lapply(l_binomfits
                                     ,function(xx) roc(response=all_emergency$a_any_cd4,predictor=predict(xx,all_emergency,type='response'))));
#'
#' #### Figure 2. ROC curves for RAI-A and RI
#' 
#+ plot_roc, fig.width=10, fig.height=8
l_rocs <- with(sbs0$train,lapply(all_emergency[,c('a_rai','a_rockwood')]
                                 ,function(xx) roc(response=all_emergency$a_c
                                                   ,predictor=xx
                                                   ,of='thresholds')));
plot(smooth(l_rocs$a_rai,method='density'),col='orange'
     #,print.thres=T,print.thres.col='orange'
     #,print.auc=T,print.auc.col='orange'
     ,ci=F);
plot(smooth(l_rocs$a_rockwood,method='density'),col='darkgreen'
     #,print.thres=T,print.thres.col='darkgreen'
     #,print.auc=T,print.auc.col='darkgreen'
     ,ci=F,add=T);
legend('topleft',bty ='n',col=c('orange','darkgreen'),lwd=3
       ,legend=sprintf('%s (AUC=%0.3f)',c('RAI-A','RI'),sapply(l_rocs,auc)));
#' 
#' ## Optimal threshold values for RAI-A and RI
#' 

#' 
#' For a fair comparison between the two frailty scores, we used Youden's
#' Index [@YoudenIndexratingdiagnostic1950] to determine the
#' threshold values that maximized sensitivity and specificity. The thresholds,
#' sensitivities and specificities are shown in Table 4. RI has a small
#' advantage in sensitivity and RAI-A in specificity.
#' 

#' 
#' #### Table 4. Optimal threshold scores for RAI-A and RI
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

#'
#' These are the error-matrices corresponding to the thresholds, sensitivities
#' specificities, and other metrics from Table 4.
#'

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
# Now tabulate the results; as well as concordances
t_auccox <- sapply(auc_coxs,sapply,function(xx) 
  if(length(xx)>1&&'iauc' %in% names(xx)) xx[['iauc']] else 
    if(length(xx)==1 && is.numeric(xx)) xx else NaN);
#' Survival analysis models, such as Cox, incorporate information not only about
#' whether or not a death or readmission has occurred but also when it occurred.
#' So AUC calculation is more complex than for logistic models and several
#' alternative methods exist. In Table 6 is a panel with results from seven
#' AUC/C-statistic estimators (Chambless and Diao, Song and Zhou, Hung and
#' Chiang, Uno, Begg, and Gonen and Heller) and three statiscial measures
#' similar to R^2 (OXS,Nagel-k, and XO). In table 7 are detailed results of 
#' Harrel's C [@newson2006confidence].
#' 
#' #### Table 6. RAI-A and RI compared on their ability to predict death or readmission in the out-of-sample validation set using a panel of predictive accuracy metrics.
#+ tab_coxauc, results='asis'
kable(t_auccox,format='markdown',digits=3);
#' #### Table 7. Harrel's C for RAI-A and RI
#+ tab_harrelc, results='asis'
with(sbs0$train$all_emergency
     ,cbind(`RAI-A`=rcorr.cens(a_rai,Surv(a_trdm,a_crdm))
            ,RI=rcorr.cens(a_rockwood,Surv(a_trdm,a_crdm)))) %>% pander;

#
#'
#' # Conclusions
#' 
#' RAI-A and RI both are reasonable predictors of 30-day readmission, with
#' RI having a slightly higher Harrell's C (0.369) than did RAI-A (0.349). The 
#' two scores have similar concordances and AUCs.
#' Even a sample size of `r with(fit_coxs[[1]],n+nevent)` was sufficient to find 
#' a significant effect for both RAI-A and RI. The threshold value that 
#' maximizes the sum of sensitivity and specificity was, in this population, 
#' `r coords(l_rocs$a_rai,'b',ret='threshold')` for RAI-A and and
#' `r round(coords(l_rocs$a_rockwood,'b',ret='threshold'),3)` for RI.
#' 
# In the population demographic table, approximately 22% of the 541 cases
# resulted in death or hospital readmission within 30 days after surgery.
# There appears to be a significant difference between “No Event” vs “Event” 
# RAI-A and RI scores. The RAI-A and RI scores are higher in the
# “Event” cases compared to “No Event.” Additionally, age is higher in the
# “Event” population in comparison. Interestingly, it appears that the Rockwood 
# has a higher standard error margin compared to RAI-A. 
#
# Both the RAI-A and RI survival curves depict a significant difference
# between the upper 50% RAI-A and RI compared to the lower 50%.
# Unlike the confidence intervals (the shaded regions) of the RAI-A survival
# curves, the RI survival curves overlap slightly. This seems to
# corroborate the earlier observation of a slightly larger standard error margin
# with the RI compared to RAI-A. The subsequent survival curve
# model summary table also depicts the same story. Even on the training set,
# the RAI-A model performs slightly better than RI, as evidenced by the
# standard error and AIC score.
#
# A visible trend that can be seen in the Event Frequencies tables. Generally
# speaking, the death and readmission incidence increases as the frailty score
# increases. 
#
# Finally we calculated the predictive ability of both frailty metrics. Although
# the AUC demonstrates that both models are comparable in detecting 30 day
# post-operative complications, the subsequent table reveals that the RAI-A is
# more specific in detecting true post-operative complication events. 
#
# Both RI and the RAI-A frailty metrics are powerful frailty metrics that
# can be implemented using EHR data. Based on our results, it appears that the
# RAI-A slightly outperforms the RI however due to a low event rate (~22%),
# more data points need to be added and therefore more validation is needed.
# Nevertheless, we are the first to demonstrate the predictive power both frailty
# metrics have in detecting post-operative complications. Based on the results
# from this study, RAI-A may have slightly more power in detecting real
# post-operative complication rates and therefore increased ability to pre-emptively
# identify high-need patients prior to death and a hospital readmission. In doing
# so will lead to optimized patient care as we strive for precision medicine.
#' 
#' # Acknowledgments
#' This study was supported in part by the San Antonio Pepper Center 
#' P30AG044271, U01TR002393 and 1UL1TR002645
#' 
#' # References
#' 
#' # Audit Info
#+ projinfo,results='asis',cache=FALSE
lapply(PI,rbind) %>% lapply(as.character) %>% lapply(`length<-`,2) %>% 
  bind_cols %>% t %>% kable(format = 'markdown',col.names = c('value','hash'));

c()
