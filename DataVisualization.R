#' ---
#' title: "RAI Visualization"
#' author: "Wilson, Bokov, Shireman"
#' date: "10/20/2017"
#' ---
#'
#+ echo=FALSE, message=FALSE
knitr::opts_chunk$set(echo=F,warning = F,cache=T,message=F);
#+ cache=FALSE,results='hide'
source('global.R');
#' Report date: `r Sys.Date()`.
#'
#' Revision: `r gitstamp(production=F)`.
#' 
#' #### Audit Info
#+ projinfo,results='asis'
lapply(pi,rbind) %>% lapply(as.character) %>% lapply(`length<-`,2) %>% 
  bind_cols %>% t %>% kable(format = 'markdown',col.names = c('value','hash'));
#' 
#+ cache=TRUE,results='hide'
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
#plt_rai_surv_death <-autoplot(coxph(Surv(a_t,a_c) ~ I(a_rai>median(a_rai)), data = sbs0$all$all_emergency)) # doesn't run at all
# If it errors, don't leave it in uncommented-- that will cause it to fail
# our RMarkdown test. Please either delete it or if you need it for future 
# reference comment it out (plain comments, not #').

# coxph is an analytic function so it goes into run.R. I realize that I probably
# didn't communicate it very clearly because I also was talking about "keeping
# everything in once place". I meant all the output-related functions in one 
# place, but the clean separation between analysis and output such as 
# vis/tabulation needs to maintained. I am commenting the coxph out and moving 
# it to the end of run.R.
#res.cox <- coxph(Surv(a_t,a_c) ~ a_rai, data = sbs0$all$all_emergency)
# Now, the survfit() ones are a different story: they are not really analytic
# they are for plots and tables, so they can stay here. But they need more 
# descriptive names. Also, adding on an optional subset argument to get rid of
# six negative time values. We need to investigate those later, but not time 
# right now.
surv.rai <- survfit(Surv(a_t,a_c) ~ I(a_rai>median(a_rai))
                    , data = sbs0$all$all_emergency,subset=a_t>0);
surv.rock <- survfit(Surv(a_t,a_c) ~ I(a_rockwood>median(a_rockwood))
                     , data = sbs0$all$all_emergency,subset=a_t>0);
#res.fit2 <- survfit(Surv(a_t,a_c) ~ cut(a_rai,3), data = sbs0$all$all_emergency)
ggsurvplot(surv.rai);
ggsurvplot(surv.rock);
# The below is not actually doing facet_wrap for some reason-- the goal had been
# to get them on one plot.
#facet_wrap(ggsurvplot(surv.rai), ggsurvplot(surv.rock));
# If it causes an error, don't leave it active.
#ggsurvplot(res.cox) # doesn't run properly


# This overwrites the earlier res.cox with an incorrect predictor.
#res.cox <- coxph(Surv(a_t,a_c) ~ I(a_rai>median(a_rai)), data = sbs0$all$all_emergency)
# Why is this being created again? 
#res.fit0 <- survfit(Surv(a_t,a_c) ~ I(a_rai>median(a_rai)), data = sbs0$all$all_emergency)

# Need a more descriptive name than theplotlist, making it pl_surv-- pl_ just like
# the other pl_ objects created in this script indicates that it is plottable,
# and surv means they are survival plots. Currently our only ones. As this evolves
# beyond the February 2018 poster, we may later need to have an even more 
# specific naming scheme
pl_surv <-list(RAI=autoplot(surv.rai), Rockwood=autoplot(surv.rock));
# below not worth it: a_rai identical to above, a_rockwood very close
#pl_surv_roc <-list(RAI=autoplot(update(surv.rai,.~I(a_rai>6)))
#                   , Rockwood=autoplot(update(surv.rock,.~I(a_rockwood>0.26))));


# These were not plots_cph_numeric, so renaming it. Actually reusing the pl_surv
# object because the sapply result contains the same number and identities of
# objects, with each one a superset of its respective original. If you disagree
# and want to keep the previous step for debugging or whatever, please give it a 
# prefix like .debug_ or .temp_ or .junk_ 
pl_surv <- sapply(names(pl_surv)
                  ,function(xx) pl_surv[[xx]] + 
                    theme(legend.position = 'none') + 
                    ggtitle(paste0(xx,', Split by Median')) +
                    scale_y_continuous(limits=c(.5,1),labels = scales::percent) +
                    labs(x='Time in Days', y = 'Survival'),simplify=F);
multiplot(plotlist=pl_surv,cols=1);


dev.off()
