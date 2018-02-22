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
