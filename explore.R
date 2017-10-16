#' ---
#' title: "RAI Exploratoru Tables and Plots"
#' author: "Wilson, Bokov, Shireman"
#' date: "09/30/2017"
#' ---
#' 
#' After `run.R` has completed, we do whatever
#' summary tables and plots in this script. All
#' the libraries and `config.R` variables are taken
#' care of by `run.R`
#' TODO: Just discovered that conditioning sourcing `run.R` on the existence of 
#' `dat4` will skip sourcing it as intended but because of that also skip 
#' loading the libraries. Sounds like we will need to create a separate `init.R`
#' file for just the libraries and have everything call that file.
#if('clearenv'%in% ls()) clearenv():
#+ cache=FALSE, echo=FALSE, message=FALSE
if(!'dat4' %in% ls()) source('run.R',echo = F);
#+ echo=FALSE, results='asis'
cat('\nGit commit number:',gitstamp(),'<br/>');
#+ echo=FALSE, results='asis'
cat('Data file:',inputdata,'\n');

#' Moved over from run.R
#' 
#' We're redoing the table on each thing in dat1subs. Therefore sapply is needed, 
#' and the entire table-creating pipeline is wrapped in a throwaway function 
#' (there is a formal name for those, btw-they are called lambdas). This could 
#' be made a permanent function with a few tweaks:
#' 
#' * The human-readable names are cumbersome to work with and what if the desired
#' choice of columns changes? Solution: we keep the original column names and 
#' create a column in dct0 from which to look up the table headers we would like
#' to use for each one.
#' * The columns are an alternating series of `sum()` and `mean()`. But this could
#' change, not something that we should hard-code. Solution: if we make this into
#' a real function it will take a list of functions as an argument. The 
#' default value of the list will be `c(sum,mean)` repeated but this way the user
#' can override that default with a different pattern if necessary
#' * Inside each `sum()` or `mean()` is a logical vector. It highlights the fact
#' that regardless of whether or not we make this into a function, we need to 
#' standardize these values in `run.R`! This job would be easier all these 
#' `'Yes'/'No'` and `'TRUE'/'FALSE'` values were mass-converted to actual logical
#' `TRUE/FALSE`. In fact I think the reason we even have these `'TRUE'/'FALSE'` 
#' character columns is that I thought this would be cleaner later on for model
#' fitting. However, if we do our models all in one file or one part of the file
#' we could just mass-convert all the logical columns to factors at that point. 
#' It may even be that `glm()` and friends give exactly the same output for
#' logical vectors versus factors, we should check to see if we even need to
#' convert logical vectors to anything. At any rate, when these are logical vectors
#' we will be able to shorten, e.g., `a_any_postop=='TRUE'` to `a_any_postop`.
#' * We are not always checking for true values, sometimes we are checking for 
#' false ones. So our function, in addition to a list of functions and a vector of
#' columns of interest our function will need a logical vector where each `TRUE`
#' or `FALSE` value indicates whether or not to invert the value of that
#' particular column.
#' 
sapply(dat1subs, function(xx) group_by(xx,rai_range) %>% 
  summarize(`RAI Range` = n(), `Non-Elective Surgery` = sum(elective_surg=='No')
            ,`Non-Elective Surgery Fraction` = mean(elective_surg=='No')
            ,`Emergency Case N` = sum(emergency_case=='Yes')
            ,`Emergency Case Fraction` = mean(emergency_case=='Yes')
            ,`Died 30days N` = sum(postop_death_30_dy_proc =='Yes') 
            ,`Died 30days Fraction` = mean(postop_death_30_dy_proc =='Yes')
            ,`Complications 30days N` = sum(a_any_postop=='TRUE')
            ,`Complications 30days Fraction` = mean(a_any_postop=='TRUE')
            ,`Clavien-Dindo Grade4 30days N` = sum(a_any_cd4=='TRUE')
            ,`Clavien-Dindo Grade4 30days Fraction` = mean(a_any_cd4=='TRUE')
  ) %>% 
  mutate(`Cumulative Count`=cumsum(`RAI Range`)),simplify=F) -> tables_01;
#' Putting it all together, a permanent version of this function would have a 
#' call that looks something like this (shortened for didactic purposes):
# hypothetical_function(data=foo
#                       # each column gets repeated however many times is is used
#                       # in the table
#                       ,cols=c(elective_surg,elective_surg,emergency_case,emergency_case)
#                       # notice the absence of quotes and parentheses-- we are not
#                       # executing these functions, we are passing them as objects 
#                       # that will be executed later on, inside this function
#                       ,funs=c(sum,mean,sum,mean)
#                       ,invert=c(F,F,T,F));
#' We don't need to write this function until the columns requested start to 
#' change frequently or differ from one set of tables to the next. Even then, 
#' `tableone` or one `stargazer` might have already done something equivalent
#' (I don't remember what if anything prevented us from using them here).
#' 
#' But the reason I took the time to write this up in detail is so you would see
#' the thought process behind looking at a bunch of semi-repetitive code and 
#' seeing how it can be generalize to be a reusable function.

#' `sapply()` for its side effect of creating `View()` panels, but `sapply()`
#' always creates output (in this case a named list of NULL values). It doesn't 
#' break anything but it does clutter the console, so we capture that output to 
#' a `.junk` variable and do nothing with it.
#' 
#' Notice we need to do it differently depending on if this is running 
#' inside an R Markdown report or interactively.
if(interactive()) .junk <- sapply(tables_01,View) else {
  sapply(tables_01,identity,simplify=F)};



# # this one writes the name of the table
# write(x,file=modvarstratafile,append=T);
# # this one writes the header manually because write.table is so dumb
# write(",Present,Missing,p-adj",file=modvarstratafile,append=T);
# # writing the actual table
# write.table( modvarstrata[[x]], file=modvarstratafile, na="", col.names=FALSE, row.names=TRUE
#              , append= TRUE, sep=',' )
# }));
# sapply(names(modelvarsumtab),function(xx){
#   # Same as above
#   cat("\n\n",xx,"\n",file=modvartabfile,append=T);
#   write.table(modelvarsumtab[[xx]],file=modvartabfile,na="",col.names=F,row.names = T,append=T,sep=',');
# });
#write.csv2(x=newmodvarstrata, file=paste(outputpath, 'StrataComplicationsMissingTables.csv', sep=''), na="", col.names=TRUE, row.names=FALSE, sep=',');
#write.csv2(x=newmodvarsumtab, file=paste(outputpath, 'SumComplicationsMissingTables.csv', sep=''), na="", col.names=TRUE, row.names=FALSE, sep=',');

#' ### Summary counts
# subset(dat3,race=='White'|hispanic_ethnicity=='Yes') %>% 
#   mutate(hisp=hispanic_ethnicity=='Yes') %>% 
#   group_by(gender,hisp) %>% 
#   summarise(
#     age=paste0(round(mean(age_at_time_surg,na.rm=T),1),' (',round(sd(age_at_time_surg,na.rm=T),1),')')
#     ,no_complications=sum((a_cd4+a_postop)==0)
#     ,income_nocomp=paste0(round(median(income_final[!(a_cd4|a_postop)],na.rm=T)/1000,1),' (',paste0(round(quantile(income_final[!(a_cd4|a_postop)],c(.25,.75),na.rm=T)/1000,1),collapse=','),')')
#     ,income_comp=paste0(round(median(income_final[a_cd4|a_postop],na.rm=T)/1000,1),' (',paste0(round(quantile(income_final[a_cd4|a_postop],c(.25,.75),na.rm=T)/1000,1),collapse=','),')')
#     ,n_cd4=sum(a_cd4>0)
#     , n_postop=sum(a_postop>0)) %>% 
#   mutate(hisp=ifelse(hisp,'Yes','No')) %>% 
# setNames(c('Sex','Hispanic','Age (SD)','No Comp (N)','Income No Comp (IQR)','Income Comp (IQR)','CvDn4 (N)','Postop (N)')) ->   summary_counts;
# 
# write_tsv(summary_counts,'summary_counts.tsv');

# reminder: you can sum over T/F values (and average over them too)
# 
# if it's not T/F, this might not always be reliable... foo == bar
# ...if NAs exist... so for a strictly T/F output use isTRUE(foo == bar)
# 
# Second sheet, as you said, is a job for table(...)
# 
# Ditto third sheet.
# 
# Done!
# 
# (don't forget to commit your changes)
# 

#' ## Exploration
#' 
#' Try making pivot tables...
#group_by(dat3,FOO) %>% summarize(AllComp_Fraction=mean(a_allcomp>0)
#                                 ,CD4Comp_Fraction=mean(a_cd4comp>0));
#' ...by replacing FOO with NON-quoted column names representing sex, ethnicity
#' and binned income. Not a vector, just an arbitrarily long set of 
#' non-quoted names separated by commas. This can go in your abstract!
#' 
#' Try plotting a hist on each numeric value...
#layout(matrix(1:25,nrow=5));
#.junk<-sapply(union(cnum,cintgr),function(ii) hist(dat3[[ii]],main=ii));
#' You will probably need to adjust the nrow/ncol for the `layout()`
#' command, and probably plot some of them individually so you 
#' can adjust the `xlim`, `breaks`, etc. The goal is to look for
#' skewed or otherwise wierd distributions.

#' Try using `ggduo()` to plot all predictors vs all 
#' responses.





#' dat3<-subset(dat2,hispanic_ethnicity!='Unknown'&(hispanic_ethnicity=='Yes'|race=='White'));
#' dat3$hispanic_ethnicity<-factor(dat3$hispanic_ethnicity);
#' #' creating a counts table of missingness for each variable:
#' #' TODO: Fix these, to include the actual RAI components
#' # misstable <- transform(dat3, missing_income=is.na(income_final)) %>%
#' #   CreateTableOne(data=., vars= c(therai,c_cd4_count), strata='missing_income') %>%
#' #   print;
#' # misstable[,'p'] %>%  gsub("<","", x=.) %>% trimws() %>%
#' #   as.numeric() %>% p.adjust() %>% cbind(misstable,padj=.) %>%
#' #   data.frame %>% select(-test) -> misstable;
#' # 
#' # misstable2 <- CreateTableOne(data=., vars= c(therai,c_cd4_count), strata='gender');
#' # newmisstable2 <- print(misstable2);
#' # newmisstable2[,'p'] %>%  gsub("<","", x=.) %>% trimws() %>%
#' #   as.numeric() %>% p.adjust() %>% cbind(newmisstable2,padj=.) %>%
#' #   data.frame %>% select(-test) -> newmisstable2;
#' #' writing output to a 'Results' folder:
#' # variables <- rownames(newmisstable)
#' # newmisstable <- cbind(variables, newmisstable)
#' # write.table(x=newmisstable, file=paste(outputpath, 'IncomeMissingTable.csv', sep=''), na="", col.names=TRUE, row.names=FALSE, sep=',');
#' 
#' modvarstrata <-sapply(c_modelvars,function(ii) {
#'   try(
#'     eval(
#'       parse(text=sprintf("stratatable(dat3,c_modelvars,str=is.na(%s)|%s=='Unknown')",ii,ii))))
#'   });
#' modvarstrata <- modvarstrata[sapply(modvarstrata,class)=='matrix'];
#' 
#' #' Let's treat the variables with more than 6 distinct values as continuous and
#' #' the rest we tabulate whether or not they are factors. For this, 
#' #' `modelvars_iscont`, a named vector of T/F values, is created. We really ought
#' #' to just create a c_discrete column in dct0, but it might be out of date with 
#' #' the latest version so we'll clean it up later and use this vector for now.
#' modelvars_iscont<-sapply(dat3[,c_modelvars],function(xx) length(unique(xx))>6);
#' #' Now we just first do the continuous ones with summary and then the discrete
#' #' ones with table, and we `c()` them all together because when you do that to
#' #' lists, you get a list as the result.
#' modelvarsumtab <- c(sapply(dat3[,names(modelvars_iscont[modelvars_iscont])]
#'                          ,function(xx) cbind(summary(xx))),
#'                   sapply(dat3[,names(modelvars_iscont[!modelvars_iscont])]
#'                          ,function(xx) cbind(table(xx,useNA='always'))));
#' 
#' #modvarsumtab <- sapply(c_modelvars, function(ii){table(dat3[,ii],useNA = 'always')});
#' #newmodvarstrata <- print(modvarstrata);
#' #newmodvarsumtab <- print(modvarsumtab);
#' 
#' modvarstratafile <- paste0(outputpath,'StrataComplicationsMissingTables'
#'                            ,gsub(" ","_",paste0(Sys.time(),'CDT.csv')));
#' modvartabfile <- paste0(outputpath,'SumComplicationsMissingTables'
#'                         ,gsub(" ","_",paste0(Sys.time(),'CDT.csv')));
#' sapply(names(modvarstrata), function(x) try({
#'   # this one just skips a few lines to make the output easier to read
#'   write("\n\n",file=modvarstratafile,append=T);

#' Create a tabsie object for visualization
#' How well does Rockwood correlate with RAI-A?
#' smoothScatter(dat4$a_rockwood
#'               ,dat4$a_rai
#'               ,bandwidth = c(.03,.5)
#'               ,nrpoints = 0);
#' #' ### Plot of percent having any complication versus ethnicity and RAI bin
#' # if you are grouping by something else, edit the next line
#' group_by(dat4,hispanic_ethnicity,a_discrete_rai) %>% 
#'   # the mean of a T/F variable is the percent TRUE
#'   summarise(a_any_postop=mean(a_any_postop=='TRUE')) %>% 
#'   # if you are grouping by something else, update to match the group_by
#'   ggplot(aes(x=a_discrete_rai,y=a_any_postop,fill=hispanic_ethnicity)) + 
#'   geom_bar(stat='identity',position='dodge') -> plot_anypostop;
#' 
#' # create tabsie dataset for visualization
#' # mktabsie(dat4
#' #          ,c(Full=T,OnlyPostop=bquote(a_postop!=0),OnlyNoPostop=bquote(a_postop==0))
#' #          ,pw=shinypw
#' #          ,serverTitle = 'RAI Pilot Project'
#' #          ,serverStatement = bquote(h4("Now we can each see the data. Please note, only pairwise comparisons are available with this tool."))
#' #          ,vars=c_tabsievars);
#' 
#' tidy(lmrr<-lm(a_rockwood~a_rai,dat4));
#' glance(lmrr);
#' cxbase<-coxph(Surv(a_t,a_c)~1,dat4);
#' #' How predictive is RAI of deaths and readmissions?
#' tidy(cxrai <- update(cxbase,.~a_rai));
#' glance(cxrai);
#' survfit(Surv.a_t..a_c.~.fitted>median(.fitted),data=augment(cxrai)) %>% 
#'   autoplot(ylim=c(.75,1)) +
#'   labs(x='Time in Days', y = 'Event-Free') +
#'   scale_fill_discrete('RAI-A   ',labels=c('Low','High')) +
#'   scale_color_discrete('RAI-A   ',labels=c('Low','High')) +
#'   ggtitle('RAI-A as Predictor of 30 Mortality or Readmission');
#' #' How predictive is Rockwood of deaths and readmissions?
#' tidy(cxrck <- update(cxbase,.~a_rockwood));
#' glance(cxrck);
#' survfit(Surv.a_t..a_c.~.fitted>median(.fitted),data=augment(cxrck)) %>% 
#'   autoplot(ylim=c(.75,1)) +
#'   labs(x='Time in Days', y = 'Event-Free') +
#'   scale_fill_discrete('Rockwood',labels=c('Low','High')) +
#'   scale_color_discrete('Rockwood',labels=c('Low','High')) +
#'   ggtitle('Rockwood Index as Predictor of 30 Mortality or Readmission');
#' #ggduo(dat4,union(cnum,cintgr),c_resps);
#' #ggduo(dat4,union(ctf,cfactr),c_resps);
#' 
#' #plotting graphs:
#' cnum <- vartype(dat4, 'numeric'); #<= I created this function in 'functions.R' file
#' cintgr <- vartype(dat4, 'integer');
#' ctf <- vartype(dat4, 'logical');
#' cfactr <- vartype(dat4, 'factor');
#' ggduo(dat4,union(cnum[1:2],cintgr[1:2]),c_resps);
#' ggduo(dat4,union(ctf,cfactr),c_resps);
#' 
#' #the following plots are interesting:
#' ggduo(dat4, columnsX='income_final', columnsY=c('a_postop','a_cd4'), c_resps);
#' ggduo(dat4, columnsX='age_at_time_surg', columnsY=c('a_postop','a_cd4'), c_resps);
#' ggduo(dat4, columnsY='age_at_time_surg', columnsX=c('hispanic_ethnicity'), c_resps);
#' ggduo(dat4, columnsY='income_final', columnsX=c('hispanic_ethnicity'), c_resps);
#' ggduo(dat4, columnsY='hispanic_ethnicity', columnsX=c('a_postop','a_cd4'), c_resps);
#' 
#' 
#' #' The goal is to find the most obvious relationships beteen
#' #' predictors and variables.
#' 
#' #' NOW you have probed your data in a sufficiently deep and 
#' #' methodical way that you can start making decisions about how
#' #' to analyze it. For example...
#' glmpostop <- glm(a_postop~1,dat4,family='poisson');
#' glmpostopaic <- stepAIC(update(glmpostop,subset=!is.na(income_final))
#'                         ,scope=list(lower=.~1,upper=.~(a_rai+hispanic_ethnicity+age_at_time_surg+income_final)^3)
#'                         ,direction='both');
#' summary(glmpostopaic);
#' glmcd4 <- glm(a_cd4~1,dat4,family='poisson');
#' glmcd4aic <- stepAIC(update(glmcd4,subset=!is.na(income_final)),scope=list(lower=.~1,upper=.~(a_rai+hispanic_ethnicity+age_at_time_surg+income_final)^3),direction='both');
#' summary(glmcd4aic);
#' #' BUt this is kind of a waste of time because RAI-A was never properly weighted.
#' #' Let's fix that...
#' glmp_cd4<-glm(a_cd4~gender+x_loss_bw_6_months_prior_surg+dyspnea+currently_dialysis+chr_30_dy_prior_surg+functnal_heath_status+disseminated_cancer*age_at_time_surg+serum_creatinine+a_transfer
#'               ,dat4,subset=!is.na(serum_creatinine)&dyspnea!='At Rest'&functnal_heath_status!='Unknown',family='poisson');
#' #' Can we improve on RAI by adding the above fitted model?
#' anova(update(glmp_cd4,.~a_rai),update(glmp_cd4,.~.+a_rai),test='LRT');
#' #' Yes
#' #' 
#' #' How about in reverse? Can we improve on the model by adding RAI?
#' anova(glmp_cd4,update(glmp_cd4,.~.+a_rai),test='LRT');
#' #' Yes also
#' #' 
#' #' Now let's do stepwise regression to see what really needs to be in the model
#' glmp_cd4_aic <- stepAIC(glmp_cd4,scope=list(lower=~1,upper=~(.)^2),direction='both');
#' #' Now can this be improved with original RAI?
#' #' Can we improve on RAI by adding the above fitted model?
#' anova(update(glmp_cd4_aic,.~a_rai),update(glmp_cd4_aic,.~.+a_rai),test='LRT');
#' #' And does RAI improve it?
#' anova(glmp_cd4_aic,update(glmp_cd4_aic,.~.+a_rai),test='LRT');
#' #' A little
#' #' 
#' 
#' #' ## (yet another) summary demographic table for NSQIP
#' #' 
#' #' (with patients aged 60-89)
#' #' 
#' subset(dat2,between((Sys.time() - dt_birth)/365.25,60,89)) %>% 
#'   mutate(income=income_final/1000
#'          ,age=(Sys.time()-dt_birth)/365.25
#'          ,ohw=(ifelse(hispanic_ethnicity=='Yes'
#'                       ,'Hispanic'
#'                       ,ifelse(race=='White'
#'                               ,'White'
#'                               ,'Other')))) %>% 
#'   group_by(ohw,gender) %>% 
#'   summarise(N=n()
#'             ,mage=sprintf('%0.1f (%0.1f, %0.1f)'
#'                           ,median(age,na.rm=T)
#'                           ,quantile(age,.25,na.rm=T)
#'                           ,quantile(age,.75,na.rm=T))
#'             ,inc=sprintf('%0.0f (%0.0f, %0.0f)'
#'                          ,median(income,na.rm=T)
#'                          ,quantile(income,.25,na.rm=T)
#'                          ,quantile(income,.75,na.rm=T))) %>%
#'   View;
