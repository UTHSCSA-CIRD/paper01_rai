#' ---
#' title: "RAI Visualization"
#' author: "Wilson, Bokov, Shireman"
#' date: "10/20/2017"
#' ---
#' 
#+ echo=FALSE, message=FALSE
source('global.R');
#' Revision: `r gitstamp()`.
#' 
#' Data file: `r inputdata`.
#+ cache=TRUE, echo=FALSE, message=FALSE, warning=FALSE
source('run.R');

#' Relationship between income and frailty incidence in all colectomy patients
#' that have a Clavien-Dindo Grade 4 complication (TRUE) or not (FALSE)
#' 
#+ echo=FALSE, warning=FALSE
dat1subs[["all_colon_all"]] %>% 
  # the next line doesn't actually do anything...
  # select(a_discrete_rai,hispanic_ethnicity, a_any_cd4, income_final) %>% 
  # group_by( a_discrete_rai,hispanic_ethnicity, a_any_cd4) %>% 
  # mutate(count=length(a_discrete_rai)) %>% 
   ggplot(aes(x=a_discrete_rai,y=income_final,fill=a_any_cd4)) + 
   geom_boxplot(coef=100) + labs(title = "Income vs Frailty & Clavien-Dindo Grade 4 
Complications in all Colectomy Patients") +
   scale_fill_discrete(name="Clavien-Dindo\nGrade4"
                      ,breaks=c("FALSE", "TRUE")
                      ,labels=c("No", "Yes")
                    ) -> plot_any_cd4;
plot_any_cd4 + stat_summary(fun.data=n_fun,geom='text',position=position_dodge(width=.75));
#' The mid-lines are median values, the boxes extend from the
#' 25th percentile to the 75th percentile-- i.e. they contain 
#' half the observations for their respective groups. The
#' vertical lines extend to the maximum and minimum observation,
#' so 100% of the data is contained within them. The numbers indicate
#' the number of cases in each group.
#' 
#' 
#' 

#' Patients that DO NOT have Clavien-Dindo Grade 4 complications
#' 
#+ echo=FALSE, warning=FALSE
dat1subs[["all_colon_all"]] %>% 
  filter(a_any_cd4=='FALSE') %>%
  ggplot(aes(x=a_discrete_rai,y=income_final,fill=hispanic_ethnicity)) + 
  geom_boxplot(coef=100) + labs(title = "Income Vs Frailty Vs Hispanic Ethnicity in 
All Colectomy Patients with No CD4 Complications") +
  scale_fill_discrete(name="Ethnicity"
                     ,breaks=c("No", "Unknown", "Yes")
                     ,labels=c("Non-Hispanic", "Unknown", "Hispanic")
                    )-> plot_no_cd4;
plot_no_cd4 + stat_summary(fun.data=n_fun,geom='text',position=position_dodge(width=.75));
#' The mid-lines are median values, the boxes extend from the
#' 25th percentile to the 75th percentile-- i.e. they contain 
#' half the observations for their respective groups. The
#' vertical lines extend to the maximum and minimum observation,
#' so 100% of the data is contained within them. The numbers indicate
#' the number of cases in each group.
#' 
#' 
#' 


#' Patients that HAVE Clavien-Dindo Grade 4 complications
#' 
#+ echo=FALSE, warning=FALSE
dat1subs[["all_colon_all"]] %>% 
  filter(a_any_cd4=='TRUE') %>%
  ggplot(aes(x=a_discrete_rai,y=income_final,fill=hispanic_ethnicity)) + 
  geom_boxplot(coef=100) + labs(title = "Income Vs Frailty Vs Hispanic Ethnicity in 
All Colectomy Patients with CD4 Complications") +
  scale_fill_discrete(name="Ethnicity"
                     ,breaks=c("No", "Unknown", "Yes")
                     ,labels=c("Non-Hispanic", "Unknown", "Hispanic")
                     )-> plot_cd4;
plot_cd4 + stat_summary(fun.data=n_fun,geom='text',position=position_dodge(width=.75));
#' The mid-lines are median values, the boxes extend from the
#' 25th percentile to the 75th percentile-- i.e. they contain 
#' half the observations for their respective groups. The
#' vertical lines extend to the maximum and minimum observation,
#' so 100% of the data is contained within them. The numbers indicate
#' the number of cases in each group.
#' 

#+ echo=FALSE
# pdf(height = 11, width = 8.5, onefile = TRUE, file = paste0(outputpath,"CD4complications.pdf"))
# plot_any_cd4
# plot_no_cd4
# plot_cd4
# dev.off()
