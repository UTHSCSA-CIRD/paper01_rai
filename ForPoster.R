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
#'   Frailty is the inability to maintain homeostasis when the human body becomes challenged by a daily stressor [Torpy JM, Lymn C and Glass RM. Frailty in Older Adults. JAMA. 2006;296(18):2280. doi:10.1001/jama.296.18.2280].  Low physical activity, muscle weakness, slowed performance, fatigue and unintentional weight loss are all characteristics of frailty [Torpy JM, Lymn C and Glass RM. Frailty in Older Adults. JAMA. 2006;296(18):2280. doi:10.1001/jama.296.18.2280]. As the U.S. population ages, frailty will be a growing concern for the US. According to the U.S. Census Bureau Population Division, the U.S. population aged 65 and older is projected to increase by 45% (from 55 million to 80 million) by 2050 [U.S. Census Bureau, Population Division. 2012. “Table 12. Projections of the Population by Age and Sex for the United States: 2015 to 2060 (NP2012-T12)”]. This population group alone will make up approximately 20% of the U.S. population in 2050 [U.S. Census Bureau, Population Division. 2012. “Table 12. Projections of the Population by Age and Sex for the United States: 2015 to 2060 (NP2012-T12)”]. While it is true that the 45 to 64 age group has a higher percentage of ambulatory surgeries compared to the 65 and above age demographic, the risk of adverse surgical outcomes is higher in the latter compared to the former [Hall MJ, Schwartzman A, Zhang J and Liu X. Ambulatory Surgery Data From Hospitals and Ambulatory Surgery Centers: United States, 2010. Natl Health Stat Report. 2017 Feb;(102):1-15;  Polanczyk CA, Marcantonio E, Goldman L, et al. Impact of age on perioperative complications and length of stay in patients undergoing noncardiac surgery. Ann Intern Med. 2001;134(8):637- 643. doi:10.7326/0003-4819-134-8-200104170-00008]. Furthermore, the number of overnight hospital stays increases with age [Lucas JW, Benson V. Tables of Summary Health Statistics for the U.S. Population: 2015 National Health Interview Survey. National Center for Health Statistics. 2017. Available from:  http://www.cdc.gov/nchs/nhis/SHS/tables.htm ]. Frailty is associated with increased risk of post-operative complications and does increase with age [Brahmbhatt R, Brewster LP, Shafii S, et al. Gender and frailty predict poor outcomes in infrainguinal vascular surgery. J Surg Res. 201(1), 156-165. DOI: 10.1016/j.jss.2015.10.026]. Based on these findings, there seems to be an overlap between the frail population and the high medical need population (i.e. individuals that repeatedly visit the hospitals for serious health concerns). Therefore, frailty could be one way to identify a subpopulation of high-need patients prior to 30 day post-operative readmission. 
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
#' * University Hospital System (UHS) electronic health records (EHR)
#' * 2015 American Community Survey 5-year estimates Median Income (from years 2011 to 2015) 
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

sbs0$all$all_emergency$a_rockwood_range <- cut(sbs0$all$all_emergency$a_rockwood, 7)
rai_table <- sbs0$all$all_emergency %>%  group_by(rai_range) %>% 
         summarise(rai_n = n()	    
                   ,cumul_count = cumsum(n())
                   ,died_n = sum(postop_death_30_dy_proc =='Yes') 
                   ,died_frac = mean(postop_death_30_dy_proc =='Yes')
                   ,readmsn_n = sum(a_readm_30_dy=='TRUE')
                   ,readmsn_frac = mean(a_readm_30_dy=='TRUE')
         ) %>% 
         mutate(cumul_count=rev(cumsum(rev(rai_n)))) %>% 
         arrange(desc(rai_range));

thecolnames <- c("RAI-A Range"
                 ,"RAI-A Score N"
                 ,"Cumulative Count"
                 ,"Died 30days N"
                 ,"Died 30days Fraction"
                 ,"30day Readmission N"
                 ,"30day Readmission Fraction");

colnames(rai_table) <- thecolnames;
kable(rai_table)


rock_table <- sbs0$all$all_emergency %>%  group_by(a_rockwood_range) %>% 
  summarise(rai_n = n()	    
            ,cumul_count = cumsum(n())
            ,died_n = sum(postop_death_30_dy_proc =='Yes') 
            ,died_frac = mean(postop_death_30_dy_proc =='Yes')
            ,readmsn_n = sum(a_readm_30_dy=='TRUE')
            ,readmsn_frac = mean(a_readm_30_dy=='TRUE')
  ) %>% 
  mutate(cumul_count=rev(cumsum(rev(rai_n)))) %>% 
  arrange(desc(a_rockwood_range));

thecolnames2 <- c("Rockwood Range"
                 ,"Rockwood Score N"
                 ,"Cumulative Count"
                 ,"Died 30days N"
                 ,"Died 30days Fraction"
                 ,"30day Readmission N"
                 ,"30day Readmission Fraction");

colnames(rock_table) <- thecolnames2;
kable(rock_table)
