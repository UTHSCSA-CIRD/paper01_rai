source('./run.R');

#' Creating open colon summary tables:
lapply(dat1subs, function(xx) group_by(xx,rai_range) %>% 
  summarize(rai_n = n()	    
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
tables_02 <- lapply(tables_01, function(xx) {
                                browser()
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
)
                           
#' Here', I'm writing the tables to a file:
lapply(tables_02, write.table, paste0(outputpath, 'UHS_ACSNSQIP_SummaryTables-DSW-', format(Sys.Date(), '%m-%d-%Y'),'.xlsx'), row.names=FALSE, append=TRUE, sep='\t')

