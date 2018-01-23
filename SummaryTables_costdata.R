#' ---
#' title: "RAI Main Analysis"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' ---
#' 
#' Please read this file through before trying to run it. The comments tell
#' you what you need to edit in order to proceed.
#' 

source('global.R');
source('run.R');
#source('run_costdata.R');


table_01 <- costdata %>% group_by(rai_range) %>% 
  summarize(rai_n = n()	    
            ,cumul_count = cumsum(n())
            ,died_n = sum(postop_death_30_dy_proc =='Yes') 
            ,died_frac = mean(postop_death_30_dy_proc =='Yes')
            ,comp_n = sum(a_any_postop.x=='TRUE')
            ,comp_frac = mean(a_any_postop.x=='TRUE')
            ,cd4_n = sum(a_any_cd4.x=='TRUE')
            ,cd4_frac = mean(a_any_cd4.x=='TRUE')
            ,readmsn_n = sum(a_readm_30_dy.x=='TRUE')
            ,readmsn_frac = mean(a_readm_30_dy.x=='TRUE')
  ) %>% 
  mutate(cumul_count=rev(cumsum(rev(rai_n)))) %>% 
  arrange(desc(rai_range)) 



thecolnames <- c("RAI-A Range"
                 ,"RAI-A Score N"
                 ,"Cumulative Count"
                 ,"Died 30days N"
                 ,"Died 30days Fraction"
                 ,"Complications 30days N"
                 ,"Complications 30days Fraction"
                 ,"Clavien-Dindo Grade4 30days N"
                 ,"Clavien-Dindo Grade4 30days Fraction"
                 ,"30day Readmission N"
                 ,"30day Readmission Fraction")


table_02 <- renamecol(table_01)

