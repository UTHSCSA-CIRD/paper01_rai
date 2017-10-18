source('./run.R');

dat1subs[["all_colon_all"]] %>% select(a_discrete_rai,hispanic_ethnicity, a_any_cd4, income_final) %>% group_by( a_discrete_rai,hispanic_ethnicity, a_any_cd4) %>% 
   # the mean of a T/F variable is the percent TRUE
   summarise(income=IQR(income_final, na.rm=TRUE)) %>%
   ggplot(aes(x=a_discrete_rai,y=income,fill=a_any_cd4)) + 
   geom_boxplot() -> plot_any_cd4;
 

 
