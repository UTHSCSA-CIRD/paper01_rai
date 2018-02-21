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
surv.rai <- survfit(Surv(a_t,a_c) ~ I(a_rai>median(a_rai))
                    , data = sbs0$all$all_emergency,subset=a_t>0);
surv.rock <- survfit(Surv(a_t,a_c) ~ I(a_rockwood>median(a_rockwood))
                     , data = sbs0$all$all_emergency,subset=a_t>0);

ggsurvplot(surv.rai);
ggsurvplot(surv.rock);


pl_surv <-list(RAI=autoplot(surv.rai), Rockwood=autoplot(surv.rock));

pl_surv <- sapply(names(pl_surv)
                  ,function(xx) pl_surv[[xx]] + 
                    theme(legend.position = 'none') + 
                    ggtitle(paste0(xx,', Split by Median')) +
                    scale_y_continuous(limits=c(.5,1),labels = scales::percent) +
                    labs(x='Time in Days', y = 'Survival'),simplify=F);
multiplot(plotlist=pl_surv,cols=1);

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