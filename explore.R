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
#if('clearenv'%in% ls()) clearenv():

#+ cache=TRUE
if(!'dat4' %in% ls()) source('run.R');
#+ echo=FALSE
cat('\nGit commit number:',gitstamp(),'\n');
cat('Data file:',inputdata,'\n');

#' #' Filter down to only NHW and hispanic
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
#' modvarstrata <-sapply(modelvars,function(ii) {
#'   try(
#'     eval(
#'       parse(text=sprintf("stratatable(dat3,modelvars,str=is.na(%s)|%s=='Unknown')",ii,ii))))
#'   });
#' modvarstrata <- modvarstrata[sapply(modvarstrata,class)=='matrix'];
#' 
#' #' Let's treat the variables with more than 6 distinct values as continuous and
#' #' the rest we tabulate whether or not they are factors. For this, 
#' #' `modelvars_iscont`, a named vector of T/F values, is created. We really ought
#' #' to just create a c_discrete column in dct0, but it might be out of date with 
#' #' the latest version so we'll clean it up later and use this vector for now.
#' modelvars_iscont<-sapply(dat3[,modelvars],function(xx) length(unique(xx))>6);
#' #' Now we just first do the continuous ones with summary and then the discrete
#' #' ones with table, and we `c()` them all together because when you do that to
#' #' lists, you get a list as the result.
#' modelvarsumtab <- c(sapply(dat3[,names(modelvars_iscont[modelvars_iscont])]
#'                          ,function(xx) cbind(summary(xx))),
#'                   sapply(dat3[,names(modelvars_iscont[!modelvars_iscont])]
#'                          ,function(xx) cbind(table(xx,useNA='always'))));
#' 
#' #modvarsumtab <- sapply(modelvars, function(ii){table(dat3[,ii],useNA = 'always')});
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
