#calling 'run.R' so that I can load the dataset. 'run.R' also does minor data cleaning.
source('./run.R');


#exploring the relationship between income and frailty incidence in all colectomy patients
#that have a Clavien-Dindo Grade 4 complication (TRUE) or not (FALSE)
thedata <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai, a_any_cd4, income_final) %>% group_by( a_discrete_rai, a_any_cd4)
ggplot(data = thedata, aes(x = factor(a_discrete_rai)
                          ,y = income_final,fill=a_any_cd4)) + 
geom_boxplot(coef=100) + labs(title = "Income Vs Frailty Vs Clavien-Dindo Grade 4 Complications in All UHS Colectomy Patients") +
scale_fill_discrete(name = "Clavien-Dindo Grade4"
                    ,breaks = c("FALSE", "TRUE")
                    ,labels = c("No", "Yes")
                    ) -> plot_any_cd4;
#' I should figure out how to print this table too:
thecounts <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai, a_any_cd4, income_final) %>% group_by( a_discrete_rai, a_any_cd4) %>% count()
plot_any_cd4 + annotate("text", x = c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2)
                        ,y = 10000, label = as.character(thecounts$n)
                        ,size = 6)
 

#selecting patients that DO NOT have Clavien-Dindo Grade 4 complications
thedata2 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai,hispanic_ethnicity) %>% 
            filter(a_any_cd4=='FALSE')
ggplot(data = thedata2, aes(x = a_discrete_rai
                           ,y = income_final, fill = hispanic_ethnicity)) + 
geom_boxplot(coef=100) + labs(title = "Income Vs Frailty Vs Hispanic Ethnicity in All UHS Colectomy Patients with No CD4 Complications") +
scale_fill_discrete(name = "Ethnicity"
                    ,breaks = c("No", "Unknown", "Yes")
                    ,labels = c("Non-Hispanic", "Unknown", "Hispanic")
                    )-> plot_no_cd4;

#' I should figure out how to print this table too:
thecounts2 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai,hispanic_ethnicity) %>% 
              filter(a_any_cd4=='FALSE') %>% count()
plot_no_cd4 + annotate("text", x = c(0.75, 1.0, 1.25, 1.8, 2.2, 3.0)
                        ,y = 10000, label = as.character(thecounts2$n)
                        ,size = 5)

#selecting patients that HAVE Clavien-Dindo Grade 4 complications
thedata3 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai,hispanic_ethnicity) %>% 
            filter(a_any_cd4=='TRUE')
ggplot(data = thedata3, aes(x = a_discrete_rai, y = income_final,fill=hispanic_ethnicity)) + 
geom_boxplot(coef=100) + labs(title = "Income Vs Frailty Vs Hispanic Ethnicity in All UHS Colectomy Patients with CD4 Complications") +
scale_fill_discrete(name = "Ethnicity"
                    ,breaks = c("No", "Unknown", "Yes")
                    ,labels = c("Non-Hispanic", "Unknown", "Hispanic")
                    )-> plot_cd4;
#' I should figure out how to print this table too:
thecounts3 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai,hispanic_ethnicity) %>% 
              filter(a_any_cd4=='TRUE') %>% count()
plot_cd4 + annotate("text", x = c(0.75, 1.0, 1.25, 1.8, 2.2, 3.0)
                       ,y = 10000, label = as.character(thecounts3$n)
                       ,size = 5)

#selecting patients that DO NOT have Clavien-Dindo Grade 4 complications
#and are not hispanic
thedata4 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai) %>% 
  filter(hispanic_ethnicity=='No')
  ggplot(data = thedata4, aes(x = a_discrete_rai
                            ,y = income_final, fill = a_any_cd4)) + 
  geom_boxplot(coef=100) + 
  labs(title = "Income Vs Frailty Vs CD4 Complications in Non-Hispanic UHS Colectomy Patients") +
  scale_fill_discrete(name = "Clavien-Dindo Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")
  )-> plot_no_cd4_nohisp;

#' I should figure out how to print this table too:
thecounts4 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai, a_any_cd4) %>% 
  filter(hispanic_ethnicity=='No') %>% count()
plot_no_cd4_nohisp + annotate("text", x = c(0.75, 1.2, 1.8, 2.2)
                              ,y = 10000, label = as.character(thecounts4$n)
                              ,size = 5)
#selecting patients that DO NOT have Clavien-Dindo Grade 4 complications
#and are hispanic
thedata5 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai) %>% 
  filter(hispanic_ethnicity=='Yes')
  ggplot(data = thedata5, aes(x = a_discrete_rai
                            ,y = income_final, fill = a_any_cd4)) + 
  geom_boxplot(coef=100) + 
  labs(title = "Income Vs Frailty Vs CD4 Complications in UHS Hispanic Colectomy Patients") +
  scale_fill_discrete(name = "Clavien-Dindo Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")
  )-> plot_no_cd4_hisp;

#' I should figure out how to print this table too:
thecounts5 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by(a_discrete_rai, a_any_cd4) %>% 
  filter(hispanic_ethnicity=='Yes') %>% count()
plot_no_cd4_hisp + annotate("text", x = c(0.75, 1.2, 1.8, 2.2, 2.8, 3.2)
                            ,y = 10000, label = as.character(thecounts5$n)
                            ,size = 5)

#selecting patients that DO NOT have Clavien-Dindo Grade 4 complications
#and are hispanic
d2 <- rbind(dat1subs[["all_colon_all"]], dat1subs[["all_colon_all"]])
d2$hispanic_ethnicity2 <- c(rep("All", times=nrow(dat1subs[["all_colon_all"]])), dat1subs[["all_colon_all"]]$hispanic_ethnicity)
#re-leveling ethnicity
d2$hispanic_ethnicity3 <- factor(d2$hispanic_ethnicity2, levels = c("All", "No", "Yes", "Unknown"))


thedata6 <- d2 %>% select(hispanic_ethnicity3, income_final, a_any_cd4) %>% 
  filter(hispanic_ethnicity3 %in% c("All", "No", "Yes")) %>% group_by( hispanic_ethnicity3, a_any_cd4) 

ggplot(data = thedata6, aes(x = hispanic_ethnicity3
                            ,y = income_final, fill = a_any_cd4)) + 
  geom_boxplot(coef=100) + 
  labs(title = "Income Vs CD4 Complications in UHS Hispanic Colectomy Patients") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "Clavien-Dindo Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")) +
  scale_x_discrete(name = "Ethnicity"
                   ,breaks = c("All", "No", "Yes")
                   ,labels = c("All", "Non-Hispanic", "Hispanic")
  ) -> plot_no_rai_cd4_hisp;

#' I should figure out how to print this table too:
thecounts6 <- thedata6 %>% group_by(hispanic_ethnicity3, a_any_cd4) %>% count()
plot_no_rai_cd4_hisp + annotate("text", x = c(0.75, 1.2, 1.8, 2.25, 2.8, 3.2)
                                       ,y = 5000, label = as.character(thecounts6$n)
                                       ,size = 5)

pdf(height = 10, width = 7.5, onefile = TRUE, file = paste0(outputpath,"UHS_ACSNSQIP_CD4comps_boxplots-DSW-", format(Sys.Date(), '%m-%d-%Y'),".pdf"))
plot_any_cd4 + annotate("text", x = c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2)
                        ,y = 10000, label = as.character(thecounts$n)
                        ,size = 6)
plot_no_cd4 + annotate("text", x = c(0.75, 1.0, 1.25, 1.8, 2.2, 3.0)
                       ,y = 10000, label = as.character(thecounts2$n)
                       ,size = 5)
plot_cd4 +annotate("text", x = c(0.75, 1.0, 1.25, 1.8, 2.2, 3.0)
                   ,y = 10000, label = as.character(thecounts3$n)
                   ,size = 5)
plot_no_cd4_nohisp + annotate("text", x = c(0.75, 1.2, 1.8, 2.2)
                              ,y = 10000, label = as.character(thecounts4$n)
                              ,size = 5)
plot_no_cd4_hisp + annotate("text", x = c(0.75, 1.2, 1.8, 2.2, 2.8, 3.2)
                            ,y = 10000, label = as.character(thecounts5$n)
                            ,size = 5)
plot_no_rai_cd4_hisp + annotate("text", x = c(0.75, 1.2, 1.8, 2.25, 2.8, 3.2)
                                ,y = 5000, label = as.character(thecounts6$n)
                                ,size = 5)

dev.off()


#exploring the relationship between income and frailty incidence in all colectomy patients
#that have a Clavien-Dindo Grade 4 complication (TRUE) or not (FALSE)
thedata <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai, a_any_cd4, income_final) %>% group_by( a_discrete_rai, a_any_cd4)
ggplot(data = thedata, aes(x = factor(a_discrete_rai)
                           ,y = income_final,fill=a_any_cd4)) + 
  geom_violin(coef=100) + labs(title = "Income Vs Frailty Vs Clavien-Dindo Grade 4 Complications in All UHS Colectomy Patients") +
  scale_fill_discrete(name = "Clavien-Dindo Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")
  ) -> plot_any_cd4_violin;
#' I should figure out how to print this table too:
thecounts <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai, a_any_cd4, income_final) %>% group_by( a_discrete_rai, a_any_cd4) %>% count()
plot_any_cd4_violin + annotate("text", x = c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2)
                        ,y = 10000, label = as.character(thecounts$n)
                        ,size = 6)


#selecting patients that DO NOT have Clavien-Dindo Grade 4 complications
thedata2 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai,hispanic_ethnicity) %>% 
  filter(a_any_cd4=='FALSE')
ggplot(data = thedata2, aes(x = a_discrete_rai
                            ,y = income_final, fill = hispanic_ethnicity)) + 
  geom_violin(coef=100) + labs(title = "Income Vs Frailty Vs Hispanic Ethnicity in All UHS Colectomy Patients with No CD4 Complications") +
  scale_fill_discrete(name = "Ethnicity"
                      ,breaks = c("No", "Unknown", "Yes")
                      ,labels = c("Non-Hispanic", "Unknown", "Hispanic")
  )-> plot_no_cd4_violin;

#' I should figure out how to print this table too:
thecounts2 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai,hispanic_ethnicity) %>% 
  filter(a_any_cd4=='FALSE') %>% count()
plot_no_cd4_violin + annotate("text", x = c(0.75, 1.0, 1.25, 1.8, 2.2, 3.0)
                       ,y = 10000, label = as.character(thecounts2$n)
                       ,size = 5)

#selecting patients that HAVE Clavien-Dindo Grade 4 complications
thedata3 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai,hispanic_ethnicity) %>% 
  filter(a_any_cd4=='TRUE')
ggplot(data = thedata3, aes(x = a_discrete_rai, y = income_final,fill=hispanic_ethnicity)) + 
  geom_violin(coef=100) + labs(title = "Income Vs Frailty Vs Hispanic Ethnicity in All UHS Colectomy Patients with CD4 Complications") +
  scale_fill_discrete(name = "Ethnicity"
                      ,breaks = c("No", "Unknown", "Yes")
                      ,labels = c("Non-Hispanic", "Unknown", "Hispanic")
  )-> plot_cd4_violin;
#' I should figure out how to print this table too:
thecounts3 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai,hispanic_ethnicity) %>% 
  filter(a_any_cd4=='TRUE') %>% count()
plot_cd4_violin + annotate("text", x = c(0.75, 1.0, 1.25, 1.8, 2.2, 3.0)
                    ,y = 10000, label = as.character(thecounts3$n)
                    ,size = 5)

#selecting patients that DO NOT have Clavien-Dindo Grade 4 complications
#and are not hispanic
thedata4 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai) %>% 
  filter(hispanic_ethnicity=='No')
ggplot(data = thedata4, aes(x = a_discrete_rai
                            ,y = income_final, fill = a_any_cd4)) + 
  geom_violin(coef=100) + 
  labs(title = "Income Vs Frailty Vs CD4 Complications in Non-Hispanic UHS Colectomy Patients") +
  scale_fill_discrete(name = "Clavien-Dindo Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")
  )-> plot_no_cd4_nohisp_violin;

#' I should figure out how to print this table too:
thecounts4 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai, a_any_cd4) %>% 
  filter(hispanic_ethnicity=='No') %>% count()
plot_no_cd4_nohisp_violin + annotate("text", x = c(0.75, 1.2, 1.8, 2.2)
                              ,y = 10000, label = as.character(thecounts4$n)
                              ,size = 5)
#selecting patients that DO NOT have Clavien-Dindo Grade 4 complications
#and are hispanic
thedata5 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by( a_discrete_rai) %>% 
  filter(hispanic_ethnicity=='Yes')
ggplot(data = thedata5, aes(x = a_discrete_rai
                            ,y = income_final, fill = a_any_cd4)) + 
  geom_violin(coef=100) + 
  labs(title = "Income Vs Frailty Vs CD4 Complications in UHS Hispanic Colectomy Patients") +
  scale_fill_discrete(name = "Clavien-Dindo Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")
  )-> plot_no_cd4_hisp_violin;

#' I should figure out how to print this table too:
thecounts5 <- dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, income_final, a_any_cd4) %>% group_by(a_discrete_rai, a_any_cd4) %>% 
  filter(hispanic_ethnicity=='Yes') %>% count()
plot_no_cd4_hisp_violin + annotate("text", x = c(0.75, 1.2, 1.8, 2.2, 2.8, 3.2)
                            ,y = 10000, label = as.character(thecounts5$n)
                            ,size = 5)


#selecting patients that DO NOT have Clavien-Dindo Grade 4 complications
#and are hispanic
d2 <- rbind(dat1subs[["all_colon_all"]], dat1subs[["all_colon_all"]])
d2$hispanic_ethnicity2 <- c(rep("All", times=nrow(dat1subs[["all_colon_all"]])), dat1subs[["all_colon_all"]]$hispanic_ethnicity)
#re-leveling ethnicity
d2$hispanic_ethnicity3 <- factor(d2$hispanic_ethnicity2, levels = c("All", "No", "Yes", "Unknown"))


thedata6 <- d2 %>% select(hispanic_ethnicity3, income_final, a_any_cd4) %>% 
  filter(hispanic_ethnicity3 %in% c("All", "No", "Yes")) %>% group_by( hispanic_ethnicity3, a_any_cd4) 

  ggplot(data = thedata6, aes(x = hispanic_ethnicity3
                            ,y = income_final, fill = a_any_cd4)) + 
  geom_violin(coef=100) + 
  labs(title = "Income Vs CD4 Complications in UHS Hispanic Colectomy Patients") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "Clavien-Dindo Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")) +
  scale_x_discrete(name = "Ethnicity"
                   ,breaks = c("All", "No", "Yes")
                   ,labels = c("All", "Non-Hispanic", "Hispanic")
                   ) -> plot_no_rai_cd4_hisp_violin;

#' I should figure out how to print this table too:
thecounts6 <- thedata6 %>% group_by(hispanic_ethnicity3, a_any_cd4) %>% count()
plot_no_rai_cd4_hisp_violin + annotate("text", x = c(0.75, 1.2, 1.8, 2.25, 2.8, 3.2)
                                   ,y = 5000, label = as.character(thecounts6$n)
                                   ,size = 5)



pdf(height = 10, width = 7.5, onefile = TRUE, file = paste0(outputpath,"UHS_ACSNSQIP_CD4comps_violinplots-DSW-", format(Sys.Date(), '%m-%d-%Y'),".pdf"))
plot_any_cd4_violin + annotate("text", x = c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2)
                        ,y = 10000, label = as.character(thecounts$n)
                        ,size = 6)
plot_no_cd4_violin + annotate("text", x = c(0.75, 1.0, 1.25, 1.8, 2.2, 3.0)
                       ,y = 10000, label = as.character(thecounts2$n)
                       ,size = 5)
plot_cd4_violin +annotate("text", x = c(0.75, 1.0, 1.25, 1.8, 2.2, 3.0)
                   ,y = 10000, label = as.character(thecounts3$n)
                   ,size = 5)
plot_no_cd4_nohisp_violin + annotate("text", x = c(0.75, 1.2, 1.8, 2.2)
                              ,y = 10000, label = as.character(thecounts4$n)
                              ,size = 5)
plot_no_cd4_hisp_violin + annotate("text", x = c(0.75, 1.2, 1.8, 2.2, 2.8, 3.2)
                            ,y = 10000, label = as.character(thecounts5$n)
                            ,size = 5)
plot_no_rai_cd4_hisp_violin + annotate("text", x = c(0.75, 1.2, 1.8, 2.25, 2.8, 3.2)
                                       ,y = 5000, label = as.character(thecounts6$n)
                                       ,size = 5)

dev.off()