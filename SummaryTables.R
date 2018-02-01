#' Create a header here like there already is in `DataVisualization.R`
source('./run.R');

#' ### **AFB Summary:** I see no errors in how this was done, good job! What we need to do is...
#' 
#' * **You need to read in Brad's data in the same place in `run.R` as we already
#' read in the NSQIP input file, creating a variable for it in your `config.R`.
#' For example cst0 or something. Let's talk about the pros and cons of joining
#' it to `dat1` versus joining it to `all_colon_all_2017`**
#' * Create a 2016-only subset of all_colon_all in the `ssply()` statement that
#' begins on line 163 in `run.R`. This will shorten the code by half!
#' * Use `summarise()`, either on a non-grouped version of each data.frame or on the 
#' aggregated-by-rai_range data.frame and then `rbind()` the latter to it and then
#' pipe it to `setNames()` to reduce the code-size by another half I think.
#' * Do you have any objections to us converting the naturally-binary variables 
#' like the ones used here to `TRUE`/`FALSE` in `dat1` so you don't have to do
#' that here? This will make the code simpler still because then you can use 
#' `summarize_each()` on a whole list of columns.
#' 

#' Creating open colon summary tables:
#' 
#' **AFB:** Oops-- my bad, I should have named it `a_rai_range` in `run.R`
lapply(dat1subs, function(xx) group_by(xx,rai_range) %>% 
  summarise(rai_n = n()	    
            ,cumul_count = cumsum(n())
            ,died_n = sum(postop_death_30_dy_proc =='Yes') 
            ,died_frac = mean(postop_death_30_dy_proc =='Yes')
            ,comp_n = sum(a_any_postop=='TRUE')
            ,comp_frac = mean(a_any_postop=='TRUE')
            ,cd4_n = sum(a_any_cd4=='TRUE')
            ,cd4_frac = mean(a_any_cd4=='TRUE')
            ,readmsn_n = sum(a_readm_30_dy=='TRUE')
            ,readmsn_frac = mean(a_readm_30_dy=='TRUE')
            ) %>% 
  mutate(cumul_count=rev(cumsum(rev(rai_n)))) %>% 
  arrange(desc(rai_range))
)  -> tables_01;

#' I have to create a summary row for each table in the list
#' Here, I am creating a variable that renames the columns:
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

#' Here is where I am creating a summary row for each table
#' in the list:
#' 
#' **AFB:** This was a tough job and you did it well! For next time (not this
#' time, it works so no need to change it) there are a couple of things you can
#' simplify. First, instead of creating a function, have the function be 
#' `function(xx) setNames(rbind(xx,summarize(xx,...),thecolnames)` and replace `...` with a comma
#' separated list of what you already have in your function but without the `xx$`.
#' For `cumul_count` you can just do `max(cumul_count)`. For the rest, which all
#' look like `died_frac` you could simplify it to `died_frac/cumul_count` and it
#' will give the right answer because by the time the `died_frac` variable is 
#' created, `died_n` and `cumul_count` have already been created. In fact, you 
#' already half use this because you're using `cumul_count` instead of 
#' `xx$cumul_count[nrow(xx)]`. 
tables_02 <- lapply(tables_01, function(xx) {
                                #browser()
                                rai_range <- "Total"
                                rai_n <- sum(xx$rai_n)
                                cumul_count <- xx$cumul_count[nrow(xx)]
                                died_n <- sum(xx$died_n)
                                #this is how Dr. Shireman wanted the
                                #summary fraction reported:
                                #the total number of people in cumulative
                                #count in each table divided by the
                                #total number of people that died (in this case)
                                died_frac <- sum(xx$died_n)/cumul_count
                                comp_n <- sum(xx$comp_n)
                                #the total number of people in cumulative
                                #count in each table divided by the
                                #total number of people that had
                                #complications (in this case)
                                comp_frac <- sum(xx$comp_n)/cumul_count
                                cd4_n <- sum(xx$cd4_n)
                                #the total number of people in cumulative
                                #count in each table divided by the
                                #total number of people that had
                                #Clavien-Dindo Grade 4 complications (in this case)
                                cd4_frac <- sum(xx$cd4_n)/cumul_count
                                readmsn_n <- sum(xx$readmsn_n)
                                #the total number of people in cumulative
                                #count in each table divided by the
                                #total number of people that had
                                #readmissions (in this case)
                                readmsn_frac <- sum(xx$readmsn_n)/cumul_count
                                newtotals <- t(c(rai_range, rai_n, cumul_count
                                                 ,died_n, died_frac, comp_n
                                                 ,comp_frac, cd4_n, cd4_frac
                                                 ,readmsn_n, readmsn_frac))
                                colnames(newtotals) <- colnames(xx)
                                newxx <- rbind(xx, newtotals)
                                #colnames(newxx) <- thecolnames
                                return(newxx)
                              }
)
                           
#' Here', I'm writing the tables to a file:
#lapply(tables_02, write.table, paste0(outputpath, 'UHS_ACSNSQIP_SummaryTables-DSW-', format(Sys.Date(), '%m-%d-%Y'),'.xlsx'), row.names=FALSE, append=TRUE, sep='\t')
savetablelist(tables_02,'UHS_ACSNSQIP_SummaryTables-DSW-');

#' Focus on UHS colectomy data January 10, 2018:
#' 
#' **AFB:** slightly more concise way to do the same thing:
# dat1subs[["all_colon_all"]] %>% 
# subset(as.character(hospital_admissn_dt,'%Y')=='2016') %>% [the rest of it]
#' But your way is _not_ wrong, and I just confirmed that it gives the same result.
#' Just out of curiousity, is there an advantage to using `filter()` as opposed 
#' to `subset()` or are they interchangeable as far as you know?
#' 
#' Anyway, in `run.R` we should add one more item, `all_colon_all_2017` that 
#' includes this criterion and get rid of this entire block of code since it's
#' basically a repeat of the one above, right?
table_01 <- dat1subs[["all_colon_all"]] %>% 
  filter(hospital_admissn_dt > "2015-12-31" & hospital_admissn_dt < "2017-01-01") %>% 
  group_by(rai_range) %>% 
  summarise(rai_n = n()	    
            ,cumul_count = cumsum(n())
            ,died_n = sum(postop_death_30_dy_proc =='Yes') 
            ,died_frac = mean(postop_death_30_dy_proc =='Yes')
            ,comp_n = sum(a_any_postop=='TRUE')
            ,comp_frac = mean(a_any_postop=='TRUE')
            ,cd4_n = sum(a_any_cd4=='TRUE')
            ,cd4_frac = mean(a_any_cd4=='TRUE')
            ,readmsn_n = sum(a_readm_30_dy=='TRUE')
            ,readmsn_frac = mean(a_readm_30_dy=='TRUE')
  ) %>% mutate(cumul_count=rev(cumsum(rev(rai_n)))) %>% 
  arrange(desc(rai_range)) 

#' **AFB:** As above-- once `all_colon_all_2017` becomes one of the subsets, you 
#' won't even need to run this function on it, it will be one of objects inside 
#' `tables_02`
#' 
#' I now think that the function we really need is summarize_each(), we just need
#' to recode the 'Yes'/'No' and 'TRUE'/'FALSE' to actual binary `TRUE`/`FALSE`
#' variables in `dat1`. It's my fault that they aren't, I don't remember what 
#' reason I could have had for not doing that.
#' 
renamecol <- function(xx) {
  rai_range <- "Total"
  rai_n <- sum(xx$rai_n)
  cumul_count <- xx$cumul_count[nrow(xx)]
  died_n <- sum(xx$died_n)
  #this is how Dr. Shireman wanted the
  #summary fraction reported:
  #the total number of people in cumulative
  #count in each table divided by the
  #total number of people that died (in this case)
  died_frac <- sum(xx$died_n)/cumul_count
  comp_n <- sum(xx$comp_n)
  #the total number of people in cumulative
  #count in each table divided by the
  #total number of people that had
  #complications (in this case)
  comp_frac <- sum(xx$comp_n)/cumul_count
  cd4_n <- sum(xx$cd4_n)
  #the total number of people in cumulative
  #count in each table divided by the
  #total number of people that had
  #Clavien-Dindo Grade 4 complications (in this case)
  cd4_frac <- sum(xx$cd4_n)/cumul_count
  readmsn_n <- sum(xx$readmsn_n)
  #the total number of people in cumulative
  #count in each table divided by the
  #total number of people that had
  #readmissions (in this case)
  readmsn_frac <- sum(xx$readmsn_n)/cumul_count
  newtotals <- t(c(rai_range, rai_n, cumul_count
                   ,died_n, died_frac, comp_n
                   ,comp_frac, cd4_n, cd4_frac
                   ,readmsn_n, readmsn_frac))
  colnames(newtotals) <- colnames(xx)
  newxx <- rbind(xx, newtotals)
  colnames(newxx) <- thecolnames
  return(newxx)
}

table_02 <- renamecol(table_01)
#write.table(table_02,  paste0(outputpath, 'UHS_ACSNSQIP_2016ColectomyTables-DSW-', format(Sys.Date(), '%m-%d-%Y'),'.txt'), row.names=FALSE, append=TRUE, sep='\t')
savetablelist(table_02,'UHS_ACSNSQIP_2016ColectomyTables-DSW-');


#' Here, Dr. Shireman is interested in 30 readmission data: 
table_03 <- dat1subs[["all_colon_all"]] %>% 
  filter(readm_30_dy >0) %>%
  filter(hospital_admissn_dt > "2015-12-31" & hospital_admissn_dt < "2017-01-01") %>% 
  group_by(rai_range) %>% 
  summarise(rai_n = n()	    
            ,cumul_count = cumsum(n())
            ,died_n = sum(postop_death_30_dy_proc =='Yes') 
            ,died_frac = mean(postop_death_30_dy_proc =='Yes')
            ,comp_n = sum(a_any_postop=='TRUE')
            ,comp_frac = mean(a_any_postop=='TRUE')
            ,cd4_n = sum(a_any_cd4=='TRUE')
            ,cd4_frac = mean(a_any_cd4=='TRUE')
            ,readmsn_n = sum(a_readm_30_dy=='TRUE')
            ,readmsn_frac = mean(a_readm_30_dy=='TRUE')
  ) %>% mutate(cumul_count=rev(cumsum(rev(rai_n)))) %>% 
  arrange(desc(rai_range)) 

table_04 <- renamecol(table_03)
write.table(table_04,  paste0(outputpath, 'UHS_ACSNSQIP_2016ColectomyTables30DayReadmission-DSW-', format(Sys.Date(), '%m-%d-%Y'),'.txt'), row.names=FALSE, append=TRUE, sep='\t')

#' Here, Dr. Shireman is interested in 30 readmission data: 
table_05 <- dat1subs[["ssi_all"]] %>% 
  group_by(rai_range) %>% 
  summarise(rai_n = n()	    
            ,cumul_count = cumsum(n())
            ,died_n = sum(postop_death_30_dy_proc =='Yes') 
            ,died_frac = mean(postop_death_30_dy_proc =='Yes')
            ,comp_n = sum(a_any_postop=='TRUE')
            ,comp_frac = mean(a_any_postop=='TRUE')
            ,cd4_n = sum(a_any_cd4=='TRUE')
            ,cd4_frac = mean(a_any_cd4=='TRUE')
            ,readmsn_n = sum(a_readm_30_dy=='TRUE')
            ,readmsn_frac = mean(a_readm_30_dy=='TRUE')
  ) %>% mutate(cumul_count=rev(cumsum(rev(rai_n)))) %>% 
  arrange(desc(rai_range)) 

table_06 <- renamecol(table_05)
#write.table(table_06,  paste0(outputpath, 'UHS_ACSNSQIP_SSI_Table-DSW-', format(Sys.Date(), '%m-%d-%Y'),'.txt'), row.names=FALSE, append=TRUE, sep='\t')
savetablelist(table_06,'UHS_ACSNSQIP_SSI-DSW-');
