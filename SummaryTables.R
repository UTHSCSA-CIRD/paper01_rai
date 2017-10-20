source('./run.R');

#' Creating open colon summary tables:
lapply(dat1subs, function(xx) group_by(xx,rai_range) %>% 
  summarize(`RAI-A Score N` = n()	    
            ,`Cumulative Count`=cumsum(n())
            ,`Died 30days N` = sum(postop_death_30_dy_proc =='Yes') 
            ,`Died 30days Fraction` = mean(postop_death_30_dy_proc =='Yes')
            ,`Complications 30days N` = sum(a_any_postop=='TRUE')
            #,`Cumulative Count` = cumsum()
            ,`Complications 30days Fraction` = mean(a_any_postop=='TRUE')
            ,`Clavien-Dindo Grade4 30days N` = sum(a_any_cd4=='TRUE')
            ,`Clavien-Dindo Grade4 30days Fraction` = mean(a_any_cd4=='TRUE')
	    ,`Complications 30days N` = sum(a_any_postop=='TRUE')
            ,`Complications 30days Fraction` = mean(a_any_postop=='TRUE')
	    ,`30day Readmission N` = sum(a_readm_30_dy=='TRUE')
            ,`30day Readmission Fraction` = mean(a_readm_30_dy=='TRUE')
  ) %>% 
  mutate(`Cumulative Count`=rev(cumsum(rev(`RAI-A Score N`)))) %>% 
  arrange(desc(rai_range)) 
) -> tables_01;


lapply(tables_01, write.table, paste0(outputpath, 'test.xlsx'), row.names=FALSE, append=TRUE, sep='\t')

