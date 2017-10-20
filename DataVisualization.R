#' calling 'run.R' so that I can load the dataset. 'run.R' also does minor data cleaning.
#+ echo=FALSE, message=FALSE
source('global.R');
#' Revision: `r gitstamp()`.
#' 
#' Data file: `r inputdata`.
#+ cache=TRUE, echo=FALSE, message=FALSE
source('run.R');

#' exploring the relationship between income and frailty incidence in all colectomy patients
#' that have a Clavien-Dindo Grade 4 complication (TRUE) or not (FALSE)
dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, a_any_cd4, income_final) %>% group_by( a_discrete_rai,hispanic_ethnicity, a_any_cd4) %>% 
   ggplot(aes(x=a_discrete_rai,y=income_final,fill=a_any_cd4)) + 
   geom_boxplot() + labs(title = "Income Vs Frailty Vs Clavien-Dindo Grade 4 Complications in All Colectomy Patients") +
   scale_fill_discrete(name="Clavien-Dindo Grade4"
                      ,breaks=c("FALSE", "TRUE")
                      ,labels=c("No", "Yes")
                    ) -> plot_any_cd4;
 

#' selecting patients that DO NOT have Clavien-Dindo Grade 4 complications
dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai,hispanic_ethnicity) %>% 
  filter(a_any_cd4=='FALSE') %>%
  ggplot(aes(x=a_discrete_rai,y=income_final,fill=hispanic_ethnicity)) + 
  geom_boxplot() + labs(title = "Income Vs Frailty Vs Hispanic Ethnicity in All Colectomy Patients with No CD4 Complications") +
  scale_fill_discrete(name="Ethnicity"
                     ,breaks=c("No", "Unknown", "Yes")
                     ,labels=c("Non-Hispanic", "Unknown", "Hispanic")
                    )-> plot_no_cd4;


#selecting patients that HAVE Clavien-Dindo Grade 4 complications
dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai,hispanic_ethnicity) %>% 
  filter(a_any_cd4=='TRUE') %>%
  ggplot(aes(x=a_discrete_rai,y=income_final,fill=hispanic_ethnicity)) + 
  geom_boxplot() + labs(title = "Income Vs Frailty Vs Hispanic Ethnicity in All Colectomy Patients with CD4 Complications") +
  scale_fill_discrete(name="Ethnicity"
                     ,breaks=c("No", "Unknown", "Yes")
                     ,labels=c("Non-Hispanic", "Unknown", "Hispanic")
                     )-> plot_cd4;

# pdf(height = 11, width = 8.5, onefile = TRUE, file = paste0(outputpath,"CD4complications.pdf"))
# plot_any_cd4
# plot_no_cd4
# plot_cd4
# dev.off()
