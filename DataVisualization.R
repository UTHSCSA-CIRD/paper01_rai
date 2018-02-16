#' ---
#' title: "RAI Visualization"
#' author: "Wilson, Bokov, Shireman"
#' date: "10/20/2017"
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

# Note: above I set the global options so that the code is hidden in this report.
# Comments beginning with # are code and therefore get hidden (only the output
# is shown). Comments with #' however are turned into markdown. So you should 
# only have the #' comments below in places where you want those comments to 
# be visible in the final product. I suspect everything after line 64 or so should
# be a regular comment.
#
# I am commenting out the line below because the PDF files it generates seem to
# have stopped working. However, you could just render this notebook as a PDF
# file instead of HTML, and that will look better than using the built-in PDF
# device anyway. If you really need to generate a parallel PDF copy in this manner
# maybe experiment with dev.copy2pdf() but the below do not work.
#pdf(height = 10, width = 7.5, onefile = TRUE, file = paste0(outputpath,"UHS_ACSNSQIP_CD4comps_boxplots-DSW-", format(Sys.Date(), '%m-%d-%Y'),".pdf"))

# Income VS Frailty VS Clavien-Dindo Grade 4 complications 
# in ALL UHS colectomy patients:
#+ cache=FALSE
plt_frl_inc_cd4 <- autoboxplot(sbs0$all$all_colon_all
                               ,xx='a_discrete_rai',yy='income_final'
                               ,zz='a_any_cd4'
                               ,subset=!is.na(income_final)
                               ,fill.name=wrap_format(14)("Clavien-Dindo\n Grade 4")
                               ,fill.labels = c('Yes', 'No')
                               ,xx.name='Frailty Group',yy.name='Household Income',title='');
grid.arrange(plt_frl_inc_cd4
             ,top=wrap_format(30)("Income Vs Frailty VS CD4 Complications in ALL UHS Colectomy Patients")
             );

# Income VS Hispanic Ethnicity VS Clavien-Dindo Grade 4 complications
# in ALL UHS colectomy patients:
plt_eth_inc_cd4 <- autoboxplot(sbs0$all$all_colon_all
                               ,xx='hispanic_ethnicity',yy='income_final'
                               ,zz='a_any_cd4'
                               ,subset=hispanic_ethnicity!='Unknown'&!is.na(income_final)
                               ,fill.name=wrap_format(14)("Clavien-Dindo\n Grade 4")
                               ,fill.labels = c('Yes', 'No')
                               ,xx.name='Hispanic Ethnicity',yy.name=NA,title='');

plt_all_inc_cd4 <- update(plt_eth_inc_cd4, xx=T, subset=!is.na(income_final), fill.name=NA, xx.name='All', yy.name='Household Income');

grid.arrange(plt_all_inc_cd4,plt_eth_inc_cd4
             ,top=wrap_format(30)("Income Vs CD4 Complications in all UHS Colectomy Patients")
             ,nrow=1,widths=1:2);

# 
# Income VS Frailty VS Hispanic Ethnicty with NO CD4 Complications
plt_frl_inc_eth_noc4 <- autoboxplot(sbs0$all$all_colon_all
                               ,xx='a_discrete_rai',yy='income_final'
                               ,zz='hispanic_ethnicity'
                               ,subset=a_any_cd4==FALSE&!is.na(income_final)
                               ,fill.name='Hispanic Ethnicity'
                               ,fill.labels = c('Hispanic', 'Non-Hispanic', 'Unknown')
                               ,xx.name='Frailty Group',yy.name='Household Income',title='');

grid.arrange(plt_frl_inc_eth_noc4
             ,top=wrap_format(60)("Income Vs Frailty VS Hispanic Ethnicity in all UHS Colectomy Patients with NO CD4 Complications")
             );

#' 
#' 
# Income VS Frailty VS Hispanic Ethnicty WITH CD4 Complications
plt_frl_inc_eth_c4 <- update(plt_frl_inc_eth_noc4, subset=a_any_cd4==TRUE&!is.na(income_final));

grid.arrange(plt_frl_inc_eth_c4
             ,top=wrap_format(60)("Income Vs Frailty VS Hispanic Ethnicity in all UHS Colectomy Patients WITH CD4 Complications")
);


# Income VS Hispanic Ethnicity VS SSI (surgical site infection)
#+ cache=FALSE
plt_ssi_inc_eth <- autoboxplot(sbs0$all$full
                               ,xx='hispanic_ethnicity',yy='income_final'
                               ,zz='a_any_ssi'
                               ,subset=hispanic_ethnicity!='Unknown'&!is.na(income_final)
                               ,fill.name=wrap_format(14)("SSI")
                               ,fill.labels=c("No", "Yes")
                               ,xx.name='Hispanic',yy.name=NA,title='');

plt_ssi_inc_eth2 <- update(plt_ssi_inc_eth, xx=T, subset=!is.na(income_final), fill.name=NA, xx.name='All', yy.name='Household Income');

grid.arrange(plt_ssi_inc_eth2,plt_ssi_inc_eth
             ,top=wrap_format(30)("Income Vs SSI VS Hispanic Ethnicty\n in all UHS Patients")
             ,nrow=1,widths=1:2);
# The following code checks the counts in the graph:
sbs0$all$full %>% filter(!is.na(income_final)) %>% select(hispanic_ethnicity, a_any_ssi) %>% group_by(hispanic_ethnicity, a_any_ssi) %>% count() %>% View()
# the numbers do check out. Unfortunately, there is nothing interesting
# going on here.
# 


#trying to create the survival plots:
plt_rai_surv_death <-autoplot(coxph(Surv((dt_death - dt_birth), postop_death_30_dy_proc=='No') ~ a_discrete_rai, data = sbs0$all$all_emergency))

res.cox <- coxph(Surv((dt_death - dt_birth), postop_death_30_dy_proc=='No') ~ a_discrete_rai, data = sbs0$all$all_emergency)
autoplot(res.cox)
summary(res.cox)

res.fit <- survfit(Surv((dt_death - dt_birth), postop_death_30_dy_proc=='No') ~ a_discrete_rai, data = sbs0$all$all_emergency)
ggsurvplot(res.fit)
ggsurvplot(res.cox) # doesn't run properly


