#'  creating tables similar to the tables that Dan MacCarthy creates for the VASQIP data:
dat1$rai_range <- cut(dat1$a_rai, breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55));

dat1$readm_tf <- ifelse(dat1$readm_30_dy==0, 'FALSE', 'TRUE'); 

#used for pulling out 'colectomy' and 'laparoscopy' procedures:
surg <- c("laparoscopy", "colectomy")

## all surgeries ##
#dat1 %>% # <= this tabulates everything; there are 5386 records
#dat1[which(grepl("Yes",dat1$elective_surg, ignore.case=TRUE)==TRUE),] %>% #<= this tabulates all Elective Yes surgeries; there are 2926 records
#dat1[which((grepl("No", dat1$elective_surg, ignore.case=TRUE) & grepl("No",dat1$emergency_case, ignore.case=TRUE))==TRUE),] %>% #<= this tabulates all Elective No AND Emergency No surgeries; there are 1974 records
#dat1[which(grepl("Yes", dat1$emergency_case, ignore.case=TRUE)==TRUE),] %>% #<= this tabulates all Emergency Yes surgeries; there are 489 records

## all colectomies ##
#dat1[which(grepl(surg[2],dat1$cpt_descriptn, ignore.case=TRUE)==TRUE),] %>% # <= this tabulates all colectomy procedures; there are 451 records

## open colectomies ##
#dat1[which(grepl(paste0('^(?!.*', surg[1], ').*', surg[2]),dat1$cpt_descriptn, ignore.case=TRUE, perl=TRUE)==TRUE),] %>% # <= this tabulates all open colectomies; there are 268 records 
#dat1[which((grepl(paste0('^(?!.*', surg[1], ').*', surg[2]),dat1$cpt_descriptn, ignore.case=TRUE, perl=TRUE) & grepl("Yes",dat1$elective_surg, ignore.case=TRUE))==TRUE),] %>% # <= this tabulates all open colectomy Elective Yes surgeries; there are 112 records 
#dat1[which((grepl(paste0('^(?!.*', surg[1], ').*', surg[2]),dat1$cpt_descriptn, ignore.case=TRUE, perl=TRUE) & grepl("No", dat1$elective_surg, ignore.case=TRUE) & grepl("No",dat1$emergency_case, ignore.case=TRUE))==TRUE),] %>% # <= this tabulates all open colectomy Elective No AND Emergency No surgeries; there are 110 records 
#dat1[which((grepl(paste0('^(?!.*', surg[1], ').*', surg[2]),dat1$cpt_descriptn, ignore.case=TRUE, perl=TRUE) & grepl("Yes",dat1$emergency_case, ignore.case=TRUE))==TRUE),] %>% # <= this tabulates all open colectomy Emergency Yes surgeries; there are ... records 

## all laparoscopic colectomies ##
#dat1[which(grepl(paste(surg, collapse = ".*"),dat1$cpt_descriptn, ignore.case=TRUE)==TRUE),] %>% # <= this tabulates all laparoscopic colectomy procedures; there are 183 records
#dat1[which((grepl(paste(surg, collapse = ".*"),dat1$cpt_descriptn, ignore.case=TRUE) & grepl("Yes",dat1$elective_surg, ignore.case=TRUE))==TRUE),] %>% # <= this tabulates all open colectomy Elective Yes surgeries; there are 112 records 
#dat1[which((grepl(paste(surg, collapse = ".*"),dat1$cpt_descriptn, ignore.case=TRUE) & grepl("No", dat1$elective_surg, ignore.case=TRUE) & grepl("No",dat1$emergency_case, ignore.case=TRUE))==TRUE),] %>% # <= this tabulates all open colectomy Elective No AND Emergency No surgeries; there are 110 records 
dat1[which((grepl(paste(surg, collapse = ".*"),dat1$cpt_descriptn, ignore.case=TRUE) & grepl("Yes",dat1$emergency_case, ignore.case=TRUE))==TRUE),] %>% # <= this tabulates all open colectomy Emergency Yes surgeries; there are ... records 

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
  
opencol <- dat1[which(grepl(surg[2],dat1$cpt_descriptn, ignore.case=TRUE)==TRUE),]

idx <- which(table(opencol$idn_mrn)>1)
thepats <- names(idx)
thepatsidx <- which(dat1$idn_mrn %in% thepats)
newtable <- dat1[thepatsidx, c("idn_mrn", "income_final","cpt_code", "cpt_descriptn", "operatn_dt")]


write.table(newtable, 'UHScolon.txt', row.names=FALSE, quote=FALSE, sep='\t')
