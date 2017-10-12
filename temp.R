#'  creating tables similar to the tables that Dan MacCarthy creates for the VASQIP data:
dat2$rai_range <- cut(dat2$a_rai, breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55));

dat2$readm_tf <- ifelse(dat2$readm_30_dy==0, 'FALSE', 'TRUE'); 

#used for pulling out 'colectomy' and 'laparoscopy' procedures:
surg <- c("laparoscopy", "colectomy")

## all surgeries ##
#dat2 %>% # <= this tabulates everything; there are 5386 records
#dat2[which(grepl("Yes",dat2$elective_surg, ignore.case=TRUE)==TRUE),] %>% #<= this tabulates all Elective Yes surgeries; there are 2926 records
#dat2[which((grepl("No", dat2$elective_surg, ignore.case=TRUE) & grepl("No",dat2$emergency_case, ignore.case=TRUE))==TRUE),] %>% #<= this tabulates all Elective No AND Emergency No surgeries; there are 1974 records
#dat2[which(grepl("Yes", dat2$emergency_case, ignore.case=TRUE)==TRUE),] %>% #<= this tabulates all Emergency Yes surgeries; there are 489 records

## all colectomies ##
#dat2[which(grepl(surg[2],dat2$cpt_descriptn, ignore.case=TRUE)==TRUE),] %>% # <= this tabulates all colectomy procedures; there are 451 records

## open colectomies ##
#dat2[which(grepl(paste0('^(?!.*', surg[1], ').*', surg[2]),dat2$cpt_descriptn, ignore.case=TRUE, perl=TRUE)==TRUE),] %>% # <= this tabulates all open colectomies; there are 268 records 
#dat2[which((grepl(paste0('^(?!.*', surg[1], ').*', surg[2]),dat2$cpt_descriptn, ignore.case=TRUE, perl=TRUE) & grepl("Yes",dat2$elective_surg, ignore.case=TRUE))==TRUE),] %>% # <= this tabulates all open colectomy Elective Yes surgeries; there are 112 records 
#dat2[which((grepl(paste0('^(?!.*', surg[1], ').*', surg[2]),dat2$cpt_descriptn, ignore.case=TRUE, perl=TRUE) & grepl("No", dat2$elective_surg, ignore.case=TRUE) & grepl("No",dat2$emergency_case, ignore.case=TRUE))==TRUE),] %>% # <= this tabulates all open colectomy Elective No AND Emergency No surgeries; there are 110 records 
#dat2[which((grepl(paste0('^(?!.*', surg[1], ').*', surg[2]),dat2$cpt_descriptn, ignore.case=TRUE, perl=TRUE) & grepl("Yes",dat2$emergency_case, ignore.case=TRUE))==TRUE),] %>% # <= this tabulates all open colectomy Emergency Yes surgeries; there are ... records 

## all laparoscopic colectomies ##
#dat2[which(grepl(paste(surg, collapse = ".*"),dat2$cpt_descriptn, ignore.case=TRUE)==TRUE),] %>% # <= this tabulates all laparoscopic colectomy procedures; there are 183 records
#dat2[which((grepl(paste(surg, collapse = ".*"),dat2$cpt_descriptn, ignore.case=TRUE) & grepl("Yes",dat2$elective_surg, ignore.case=TRUE))==TRUE),] %>% # <= this tabulates all open colectomy Elective Yes surgeries; there are 112 records 
#dat2[which((grepl(paste(surg, collapse = ".*"),dat2$cpt_descriptn, ignore.case=TRUE) & grepl("No", dat2$elective_surg, ignore.case=TRUE) & grepl("No",dat2$emergency_case, ignore.case=TRUE))==TRUE),] %>% # <= this tabulates all open colectomy Elective No AND Emergency No surgeries; there are 110 records 
dat2[which((grepl(paste(surg, collapse = ".*"),dat2$cpt_descriptn, ignore.case=TRUE) & grepl("Yes",dat2$emergency_case, ignore.case=TRUE))==TRUE),] %>% # <= this tabulates all open colectomy Emergency Yes surgeries; there are ... records 

group_by(`RAI-A Range` = rai_range) %>% 
  summarize(`RAI-A Score N` = n()
            ,`Cumulative Count`=cumsum(n())
            ,`Died 30days N` = sum(postop_death_30_dy_proc =='Yes') 
            ,`Died 30days Fraction` = mean(postop_death_30_dy_proc =='Yes')
            ,`Complications 30days N` = sum(a_any_postop=='TRUE')
            ,`Complications 30days Fraction` = mean(a_any_postop=='TRUE')
            ,`Clavien-Dindo Grade4 30days N` = sum(a_any_cd4=='TRUE')
            ,`Clavien-Dindo Grade4 30days Fraction` = mean(a_any_cd4=='TRUE')
            ,`30day Readmission N` = sum(readm_tf=='TRUE')
            ,`30day Readmission Fraction` = mean(readm_tf=='TRUE')
  ) %>% 
  mutate(`Cumulative Count`=rev(cumsum(rev(`RAI-A Score N`)))) %>% 
  arrange(desc(`RAI-A Range`)) -> footable;
#' Example of how to render this table in markdown
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
#xtable(footable) %>%  
#  print(type='html',html.table.attributes="border=1 cellspacing=3");

write.table(footable, file=paste0(outputpath, 'footable.csv'), row.names=FALSE, sep=',')



#Dr. Shireman wants to know what the following for each patient:
#1) 1st operation cpt description
#2) 1nd operation cpt code
#3) 2nd operation cpt description
#4) 2nd operation cpt code
#and so on until I reach the patient that is listed 5 times in this dataset.
