#' ---
#' title: "RAI Main Analysis"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' ---
#' 
#' Please read this file through before trying to run it. The comments tell
#' you what you need to edit in order to proceed.
#' 

# source('global.R');
#source('run.R');
# source('run_costdata.R');
source('SummaryTables_costdata.R')

#creating the graphs:
d2 <- rbind(costdata, costdata)
d2$hispanic_ethnicity2 <- c(rep("All", times=nrow(costdata)), as.character(costdata$hispanic_ethnicity))

#re-leveling ethnicity
d2$hispanic_ethnicity3 <- factor(d2$hispanic_ethnicity2, levels = c("All", "No", "Yes", "Unknown"))

#Income ~ CD4 Violin Plot
thedata1 <- d2 %>% select(hispanic_ethnicity3, income_final, a_any_cd4.x) %>% 
  filter(hispanic_ethnicity3 %in% c("All", "No", "Yes")) %>% group_by( hispanic_ethnicity3, a_any_cd4.x) 

ggplot(data = thedata1, aes(x = hispanic_ethnicity3
                            ,y = income_final, fill = a_any_cd4.x)) + 
  geom_violin(coef=100) + 
  labs(title = "Income Vs CD4 Complications in UHS Colectomy\n Patients - 2016 Data Only") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "Clavien-Dindo\n Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")) +
  scale_x_discrete(name = "Ethnicity"
                   ,breaks = c("All", "No", "Yes")
                   ,labels = c("All", "Non-Hispanic", "Hispanic")
  ) -> plot_2016_cd4_income_violin;
thecounts1 <- thedata1 %>% filter(!is.na(income_final)) %>% group_by(hispanic_ethnicity3, a_any_cd4.x) %>% count()


#Total Cost ~ CD4 Violin Plot
thedata2 <- d2 %>% select(hispanic_ethnicity3, tot_charges_tech_pro, a_any_cd4.x) %>% 
  filter(hispanic_ethnicity3 %in% c("All", "No", "Yes")) %>% group_by( hispanic_ethnicity3, a_any_cd4.x) 

ggplot(data = thedata2, aes(x = hispanic_ethnicity3
                            ,y = tot_charges_tech_pro, fill = a_any_cd4.x)) + 
  geom_violin(coef=100) + 
  labs(title = "Total Charges Vs CD4 Complications in UHS Colectomy\n Patients - 2016 Data Only") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "Clavien-Dindo\n Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")) +
  scale_x_discrete(name = "Ethnicity"
                   ,breaks = c("All", "No", "Yes")
                   ,labels = c("All", "Non-Hispanic", "Hispanic")
  ) -> plot_2016_cd4_totcost_violin;
thecounts2 <- thedata2 %>% filter(!is.na(tot_charges_tech_pro)) %>% group_by(hispanic_ethnicity3, a_any_cd4.x) %>% count()


#Variable Cost ~ CD4 Violin Plot
thedata3 <- d2 %>% select(hispanic_ethnicity3, tot_vbl_direct_cost, a_any_cd4.x) %>% 
  filter(hispanic_ethnicity3 %in% c("All", "No", "Yes")) %>% group_by( hispanic_ethnicity3, a_any_cd4.x) 

ggplot(data = thedata3, aes(x = hispanic_ethnicity3
                            ,y = tot_vbl_direct_cost, fill = a_any_cd4.x)) + 
  geom_violin(coef=100) + 
  labs(title = "Variable Cost Vs CD4 Complications in UHS Colectomy\n Patients - 2016 Data Only") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "Clavien-Dindo\n Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")) +
  scale_x_discrete(name = "Ethnicity"
                   ,breaks = c("All", "No", "Yes")
                   ,labels = c("All", "Non-Hispanic", "Hispanic")
  ) -> plot_2016_cd4_varcost_violin;
thecounts3 <- thedata3 %>% filter(!is.na(tot_vbl_direct_cost)) %>% group_by(hispanic_ethnicity3, a_any_cd4.x) %>% count()


#Income ~ CD4 Boxplot
thedata1 <- d2 %>% select(hispanic_ethnicity3, income_final, a_any_cd4.x) %>% 
  filter(hispanic_ethnicity3 %in% c("All", "No", "Yes")) %>% group_by( hispanic_ethnicity3, a_any_cd4.x) 

#' ### This is the plot I'll rework as an example for the other plots
#' 
#' old version
ggplot(data = thedata1, aes(x = hispanic_ethnicity3
                            ,y = income_final, fill = a_any_cd4.x)) + 
  geom_boxplot(coef=100) + 
  labs(title = "Income Vs CD4 Complications in UHS Colectomy\n Patients - 2016 Data Only") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "Clavien-Dindo\n Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")) +
  scale_x_discrete(name = "Ethnicity"
                   ,breaks = c("All", "No", "Yes")
                   ,labels = c("All", "Non-Hispanic", "Hispanic")
  ) -> plot_2016_cd4_income_box;
thecounts1 <- thedata1 %>% filter(!is.na(income_final)) %>% group_by(hispanic_ethnicity3, a_any_cd4.x) %>% count()

#' new version
#' 
#' To make the code more self-documenting, let's preface plot objects with plt_
#' `autoboxplot()` is the new function I wrote.
plt_eth_inc_cd4 <- autoboxplot(costdata   # first arg is a data.frame as usual
  # There is no reason for thedata1, thedata2, etc. to proliferate and clutter up
  # .GlobalEnv if each one is used only briefly. We either keep data subsets in 
  # a few big lists or we subset them on the fly. I'm using costdata directly 
  # and as you will see it gives the same results as your code does/did
  # The xx and yy are the first and second positional arguments, I'm only 
  # using the argument names for readability
  ,xx='hispanic_ethnicity',yy='income_final'
  # zz is an optional argument for the fill variable... I'm guessing a_any_cd4.x
  # is an artifact of linking the two datasets? No worries, we'll use it for now 
  # and clean it up before merging into integration branch
  ,zz='a_any_cd4.x'
  # subset is an optional argument if needed... here I am getting rid of both
  # the unknown ethnicity and missing incomes-- though it doesn't matter for
  # plots, it does matter for the labels and this function does BOTH plots and
  # annotations. This argument can contain any expression which returns a 
  # TRUE/FALSE vector of the same length as the rows in the data.frame and whose 
  # variables exist in the data.frame which is the first argument.
  ,subset=hispanic_ethnicity!='Unknown'&!is.na(income_final)
  # What to label the legend, if you omit it, the function will come up with 
  # a reasonable (albeit ugly) default. The following optional arguments are 
  # also available: fill.breaks and fill.labels, which are self-explanatory.
  ,fill.name='Clavien-Dindo\n Grade 4'
  # What to name the x-axis. Also available are xx.breaks and xx.labels but are
  # optional
  ,xx.name='Hispanic Ethnicity'
  # We set yy.name to NA in order to suppress printing of axis labels, 
  # you'll see why in a minute
  ,yy.name=NA
  # optional, if you don't specify an empty string the default value will be a
  # descriptive (but possibly ugly) one that is automatically generated
  ,title='');
#' Now we make the 'All' boxplot. It's more elegant (and readable) to make them
#' separately and combine them later instead of torturing the data.frame.
plt_all_inc_cd4 <- autoboxplot(costdata
                               # We aren't grouping by xx, so in that case just
                               # use the dummy value T or TRUE (the function 
                               # looks specifically for such a case and does 
                               # some stuff differently)
                               ,xx=T
                               ,yy='income_final',zz='a_any_cd4.x'
                               ,subset=hispanic_ethnicity!='Unknown'&!is.na(income_final)
                               # to suppress the legend we set fill.name to NA 
                               # because we only need on copy of the legend
                               # zz is still needed because we're still splitting
                               # on it, just not showing the legend
                               ,fill.name=NA
                               # this time we do want to show the y-axis and 
                               # labels so we give it a name (optional, as long
                               # as it's not NA the function will auto generate
                               # a name).
                               ,yy.name='Household Income'
                               ,title='');

#' We can combine these into one plot:
grid.arrange(plt_all_inc_cd4,plt_eth_inc_cd4
             ,top="Income Vs CD4 Complications in UHS Colectomy\n Patients - 2016 Data Only"
             ,nrow=1,widths=1:2);

#' Here are the above without the comments... but I bet now you can see what's 
#' going on even so:
plt_eth_inc_cd4 <- autoboxplot(costdata
                               ,xx='hispanic_ethnicity',yy='income_final'
                               ,zz='a_any_cd4.x'
                               ,subset=hispanic_ethnicity!='Unknown'&!is.na(income_final)
                               ,fill.name='Clavien-Dindo\n Grade 4'
                               ,xx.name='Hispanic Ethnicity',yy.name=NA,title='');

plt_all_inc_cd4 <- autoboxplot(costdata,xx=T,yy='income_final',zz='a_any_cd4.x'
                               ,subset=hispanic_ethnicity!='Unknown'&!is.na(income_final)
                               ,fill.name=NA,yy.name='Household Income'
                               ,title='');

grid.arrange(plt_all_inc_cd4,plt_eth_inc_cd4
             ,top="Income Vs CD4 Complications in UHS Colectomy\n Patients - 2016 Data Only"
             ,nrow=1,widths=1:2);



#Total Cost ~ CD4 Boxplot
thedata2 <- d2 %>% select(hispanic_ethnicity3, tot_charges_tech_pro, a_any_cd4.x) %>% 
  filter(hispanic_ethnicity3 %in% c("All", "No", "Yes")) %>% group_by( hispanic_ethnicity3, a_any_cd4.x) 

ggplot(data = thedata2, aes(x = hispanic_ethnicity3
                            ,y = tot_charges_tech_pro, fill = a_any_cd4.x)) + 
  geom_boxplot(coef=100) + 
  labs(title = "Total Charges Vs CD4 Complications in UHS Colectomy\n Patients - 2016 Data Only") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "Clavien-Dindo\n Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")) +
  scale_x_discrete(name = "Ethnicity"
                   ,breaks = c("All", "No", "Yes")
                   ,labels = c("All", "Non-Hispanic", "Hispanic")
  ) -> plot_2016_cd4_totcost_box;
thecounts2 <- thedata2 %>% filter(!is.na(tot_charges_tech_pro)) %>% group_by(hispanic_ethnicity3, a_any_cd4.x) %>% count()


#Variable Cost ~ CD4 Boxplot
thedata3 <- d2 %>% select(hispanic_ethnicity3, tot_vbl_direct_cost, a_any_cd4.x) %>% 
  filter(hispanic_ethnicity3 %in% c("All", "No", "Yes")) %>% group_by( hispanic_ethnicity3, a_any_cd4.x) 

ggplot(data = thedata3, aes(x = hispanic_ethnicity3
                            ,y = tot_vbl_direct_cost, fill = a_any_cd4.x)) + 
  geom_boxplot(coef=100) + 
  labs(title = "Variable Cost Vs CD4 Complications in UHS Colectomy\n Patients - 2016 Data Only") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "Clavien-Dindo\n Grade4"
                      ,breaks = c("FALSE", "TRUE")
                      ,labels = c("No", "Yes")) +
  scale_x_discrete(name = "Ethnicity"
                   ,breaks = c("All", "No", "Yes")
                   ,labels = c("All", "Non-Hispanic", "Hispanic")
  ) -> plot_2016_cd4_varcost_box;
thecounts3 <- thedata3 %>% filter(!is.na(tot_vbl_direct_cost)) %>% group_by(hispanic_ethnicity3, a_any_cd4.x) %>% count()



#writing to file:
pdf(height = 10, width = 7.5, onefile = TRUE, file = paste0(outputpath,"UHS_ACSNSQIP_2016_CD4comps_boxplots-DSW-", format(Sys.Date(), '%m-%d-%Y'),".pdf"))
plot_2016_cd4_income_box + annotate("text", x = c(0.75, 1.2, 1.8, 2.25, 2.8, 3.2)
                                    ,y = 5000, label = as.character(thecounts1$n)
                                    ,size = 5)
plot_2016_cd4_income_violin + annotate("text", x = c(0.75, 1.2, 1.8, 2.25, 2.8, 3.2)
                                       ,y = 5000, label = as.character(thecounts1$n)
                                       ,size = 5)
plot_2016_cd4_totcost_box #+ annotate("text", x = c(0.75, 1.2, 1.8, 2.25, 2.8, 3.2)
                          #           ,y = 5000, label = as.character(thecounts2$n)
                          #           ,size = 5)
#' Commenting out the annotate part because it's giving errors that you are 
#' better situated to fix than I am, I'm just focusing on getting this to run
#' on both our environments so we can start reorganizing the overall project's 
#' code. Again, my commenting this out in this case doesn't mean it shouldn't be
#' there, only that it's erroring. The error in question is:
#' 
#' `Error: Unequal parameter lengths: x (6), label (0)` and caused by 
#' `thecounts2$n` and `thecounts3$n` being `integer(0)`
plot_2016_cd4_totcost_violin #+ annotate("text", x = c(0.75, 1.2, 1.8, 2.25, 2.8, 3.2)
                                        #,y = 5000, label = as.character(thecounts2$n)
                                        #,size = 5)
plot_2016_cd4_varcost_box #+ annotate("text", x = c(0.75, 1.2, 1.8, 2.25, 2.8, 3.2)
                          #           ,y = 1000, label = as.character(thecounts3$n)
                          #           ,size = 5)
plot_2016_cd4_varcost_violin #+ annotate("text", x = c(0.75, 1.2, 1.8, 2.25, 2.8, 3.2)
                             #          ,y = 1000, label = as.character(thecounts3$n)
                             #          ,size = 5)
dev.off()

