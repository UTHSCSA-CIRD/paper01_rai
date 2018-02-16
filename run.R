#' ---
#' title: "RAI Main Analysis"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' ---
#' 
#' Please read this file through before trying to run it. The comments tell
#' you what you need to edit in order to proceed.
#' 
source('global.R');

#' ## Load data if it exists 
#' 
#' (useful later, right now don't bother saving sessions)
#'if(session %in% list.files()) load(session);
#' Load your data. Notice that we're using `read_csv()` from the readr library.
#' It is a little smarter than the built-in `read.csv()`
cost0 <- read_tsv(inputdata_cost,na=c('(null)',''));
dat0 <- read_tsv(inputdata,na=c('(null)',''));

#' Read in the data dictionary
dct0 <- read_csv(dctfile,na = '');
dct1 <- read_csv(cptfile,na='');
colnames(dat0) <- tolower(colnames(dat0));
colnames(cost0) <- tolower(colnames(cost0));

 
#' Create copy of original dataset
cost1 <- cost0;
dat1 <- dat0;
dropcol_dat <- c('idn_mrn', 'lmrn_visit', 'case_number');
dat1 <- dat0 %>% select(-one_of(dropcol_dat));
names(dat1)<-gsub('deid_patient','idn_mrn',names(dat1));
names(dat1)<-gsub('deid_visit','lmrn_visit',names(dat1));
names(dat1)<-gsub('deid_case','case_number',names(dat1));

#' Create synonyms for 'TRUE' for components of the Rockwood index
#' l_ is a variable storing labels for factors
l_truthy_default <- eval(formals(truthy.default)$truewords);
l_rockwood_true <- c("Insulin", "Non-Insulin"
                     , "At Rest", "Moderate Exertion"
                     , "Partially Dependent","Totally Dependent"
                     ,"From acute care hospital inpatient"
                     , "Nursing home - Chronic care - Intermediate care"
                     ,"Outside emergency department"
                     , "Transfer from other"
                     , l_truthy_default);

dropcol_cost <- c('idn_mrn', 'case_number');
cost1 <- cost0 %>% select(-one_of(dropcol_cost));
names(cost1)<-gsub('deid_patient','idn_mrn',names(cost1));
names(cost1)<-gsub('deid_case','case_number',names(cost1));

 
#' This is another departure from my not making code changes-- I think this is 
#' the only obstacle to using the version of the cost data you gave me, and this
#' code will be completely silent if your copy doesn't have spaces in the names
names(cost1)<-gsub('[()]','',names(cost1));
names(cost1) <- chartr(' ','_',names(cost1));


#' Fixing the date columns in the 'cost' dataset:
cost1$admission_date <- as.Date(cost1$admission_date, format = '%m/%d/%y')
cost1$discharge_date <- as.Date(cost1$discharge_date, format = '%m/%d/%Y')

#' Filtering out hopefully the indext cases. This needs work but this is a 
#' a quick dirty way to get the index cases:
cost2 <- cost1 %>% filter(admitdatediff < 20 & admitdatediff > -20)


#' Standardizing the weight units to kilograms
dat1[dat0$weight_unit=='lbs','weight'] <- dat1[dat0$weight_unit=='lbs','weight']*0.453592;
dat1[dat0$weight_unit=='lbs','weight_unit'] <- c('kg');
#' Standardizing height units to centimeters
dat1[dat0$height_unit=='in','height'] <- dat1[dat0$height_unit=='in','height']*2.54;
dat1[dat0$height_unit=='in','height_unit'] <- c('cm');


#' ## Column names of primary relevance
c_modelvars <- c(v(c_rai),v(c_postop),'income_final','hispanic_ethnicity');

#' Backup up the modified cnopatos column
#' ...because it's easier if patos-subtracted columns are modified in place
c_canbepatos <- v(c_canbepatos);
c_patos <- v(c_patos);
dat1[,paste0('bak_',c_canbepatos)] <- dat1[,c_canbepatos];
#' Since the postop & accompanying columns are counts, we just subtract patos
#' from their postop counterparts.
#dat1[,c_canbepatos] <- mapply(function(xx,yy){ifelse(xx,0,yy)}, dat1[,carepatos], dat1[,cnopatos]);
dat1[,c_canbepatos] <- dat1[,c_canbepatos] - dat1[,c_patos];


#' Create binned versions of certain numeric vars.
#' (commented out until we can put a c_num2bin or something into dct0)
# dat1[,paste0('bin_',cnum2bin)] <- sapply(dat1[,cnum2bin],function(ii){
#   qii <- c(0,quantile(ii,c(.25,.5,.75),na.rm=T),Inf);
#   cut(ii,breaks = qii);
# })

#' ## Create response variables
#' 
#' Create a column that is sum of all complications. Lets name analytically
#' created colums with an `a_` prefix. This way you could make it binary
#' via `dat1$a_allcomp > 0` or leave it as an integer and use number of 
#' different complications as a proxy for severity of outcome.
#dat1$a_allcomp <- rowSums(dat1[,csrscomp]);
c_postop_yesno <- setdiff(v(c_postop),v(c_count));
c_postop_count <- intersect(v(c_postop),v(c_count));
dat1$a_postop <- rowSums(dat1[,c_postop_count]) +
  apply(dat1[,c_postop_yesno],1,function(xx) sum(na.omit(xx %in% c('Yes','Positive'))));

#' -Hack the values of these variables to be binary for now.-
#dat1$sepsis_sirs_sepsis_sepshk_48h <- dat1$sepsis_sirs_sepsis_sepshk_48h != 'None';
#dat1$first_unp_ret_or <- dat1$first_unp_ret_or == 'Yes';
#dat1$hisp <- dat1$hispanic_ethnicity == 'Yes';
c_cd4_yesno <- setdiff(v(c_cd4),v(c_count));
c_cd4_count <- intersect(v(c_cd4),v(c_count));


#' Do the same as above but just for the `ccd4` complications
#dat1$a_cd4 <- rowSums(dat1[,c_cd4]);
dat1$a_cd4 <- rowSums(dat1[,c_cd4_count]) + 
  apply(dat1[,c_cd4_yesno],1,function(xx) sum(na.omit(xx=='Yes')));

#' TRUE/FALSE variables for postop and cd4 in both cases
#' indicating whether or not the patient had _any_ complications
dat1$a_any_cd4 <- factor(dat1$a_cd4>0);
dat1$a_any_postop <- factor(dat1$a_postop>0);

dat1$a_transfer <- dat1$origin_status!='Not transferred (admitted from home)';
dat1$a_readm_30_dy <- dat1$readm_30_dy>0; 

#' Time from surgery to adverse outcome if any
dat1$a_t <- with(dat1
                 ,pmin(
                   dt_death
                   ,dt_first_readm
                   ,dt_first_unpl_ret_or
                   ,dt_second_unp_ret,na.rm = T) %>% 
                   difftime(proc_surg_finish,units='days') %>%
                   as.numeric());
#' Censor the variables at 30 days
dat1$a_t[dat1$a_t>30] <- 30;
dat1$a_t[is.na(dat1$a_t)] <- 30;
dat1$a_c <- dat1$a_t!=30;


#' Obtain the RAI score
dat1$a_rai <- raiscore(dat1);
dat1$rai_range <- cut(dat1$a_rai, breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55));
dat1$a_discrete_rai <- cut(dat1$a_rai
                           ,c(0,15,21,Inf)
                           ,right = F
                           ,labels = c('Non Frail','Pre Frail','Frail'));
dat1$a_rai_hisp <- with(dat1,interaction(a_discrete_rai,hispanic_ethnicity));

temp_truewords <- formals(truthy.default)$truewords;

#' ## The Rockwood Scale
dat1$a_rockwood <- with(dat1,(
  as.numeric(bmi>=25)+
    as.numeric(origin_status!='Not transferred (admitted from home)')+
    as.numeric(diabetes_mellitus!='No')+
    as.numeric(current_smoker_within_1_year=='Yes')+
    as.numeric(dyspnea!='No')+
    as.numeric(!functnal_heath_status%in%c('Independent','Unknown'))+
    as.numeric(vent_dependent=='Yes')+
    as.numeric(history_severe_copd=='Yes')+
    as.numeric(ascites_30_dy_prior_surg=='Yes')+
    as.numeric(hypertensn_req_medicatn=='Yes')+
    as.numeric(acute_renal_failure=='Yes')+
    as.numeric(currently_dialysis=='Yes')+
    as.numeric(disseminated_cancer=='Yes')+
    as.numeric(open_wound=='Yes')+
    as.numeric(steroid_immunosupp=='Yes')+
    as.numeric(x_loss_bw_6_months_prior_surg=='Yes')+
    as.numeric(bleeding_disorder=='Yes')+
    as.numeric(chr_30_dy_prior_surg=='Yes')+
    as.numeric(isTRUE(serum_creatinine>3))
)/(
  19-
    as.numeric(functnal_heath_status=='Unknown')-
    as.numeric(is.na(serum_creatinine))
));

c_tabsievars <- c(v('c_tabsie')
                ,'a_postop','a_any_postop','a_cd4','a_any_cd4'
                ,'a_rai','a_discrete_rai','a_rockwood');
#' ## Transform Rows
#'
#' ### Sort the rows by patient ID and then by date of surgery, ascending
#' 
dat1 <- dat1[order(dat1$proc_surg_start),];
#' 
#' ### Drop patients without an income
#dat1 <- dat1[!is.na(dat1$income_final),];
#' 
#' ### Adding a column that aggregates all SSI cases together:
dat1$a_any_ssi <- rowSums(dat1[,c("postop_si_ssi", "postop_deep_incisnal_ssi")],na.rm=T) > 0;

#' Creating an object to use as the lookup argument for `mapnames()``
dat1namelookup <- with(dct0,setNames(dataset_column_names
                                     ,ifelse(is.na(NSQIP_NAMES)
                                             ,dataset_column_names
                                             ,NSQIP_NAMES)));
#'
#'
#' ### Make several subsets of dat1 all at once
#' 
#' for later use to make multiple versions of the same table and multiple
#' versions of the same graph, as for item #3 of the 10/13/2017 PKS email.

#identifying the colectomy patients that have multiple visits:
dup_mrn <- unlist(dat1 %>% filter(cpt_code %in% v(c_all_colon,dct1)) %>%
  filter(duplicated(idn_mrn)==TRUE) %>% select(idn_mrn)) 

#dropping visits after the index colectomy procedure for colectomy patients:
drop_case_num <- unlist(sapply(dup_mrn, function(themrn){
  mat0 <- dat1 %>% select(idn_mrn, case_number, hospital_admissn_dt) %>% 
    filter(idn_mrn %in% themrn) %>% arrange(desc(hospital_admissn_dt))
  drop_this <- mat0$case_number[-1]
}))

#' ### Create a version of the dataset that only has each patient's 1st encounter
#' 
#' (you need to have specified the name of the ID column in `metadata.R`)
dat2 <- group_by(dat1,idn_mrn) %>% summarise_all(first);

subs_criteria <- alist(
  y2016=hospital_admissn_dt<'2017-01-01' & hospital_admissn_dt>'2015-12-31'
  ,full=T
  ,all_elective=elective_surg=='Yes'
  ,all_urgent=elective_surg=='No' & emergency_case=='No'
  ,all_emergency=emergency_case=='Yes'
  ,all_colon_all=cpt_code %in% v(c_all_colon,dct1) & !(case_number %in% drop_case_num)
  # coming soon:
  #,all_colon_all_2017 = 
  ,all_colon_elective=cpt_code %in% v(c_all_colon,dct1) & elective_surg=='Yes' & 
    !(case_number %in% drop_case_num)
  ,all_colon_urgent=cpt_code %in% v(c_all_colon,dct1) & elective_surg=='No' & 
    emergency_case=='No' &
    !(case_number %in% drop_case_num)
  ,all_colon_emergency=cpt_code %in% v(c_all_colon,dct1) &
    emergency_case=='Yes' & 
    !(case_number %in% drop_case_num)
  ,open_colon_all=cpt_code %in% v(c_open_colon,dct1) &
    !case_number %in% drop_case_num
  ,open_colon_elective=cpt_code %in% v(c_open_colon,dct1) &
    elective_surg=='Yes' &
    !(case_number %in% drop_case_num)
  ,open_colon_urgent=cpt_code %in% v(c_open_colon,dct1) &
    elective_surg=='No' & emergency_case=='No' &
    !(case_number %in% drop_case_num)
  ,open_colon_emergency=cpt_code %in% v(c_open_colon,dct1) &
    emergency_case=='Yes' &
    !(case_number %in% drop_case_num)
  ,lapa_colon_all=cpt_code %in% v(c_lapa_colon,dct1) &
    !(case_number %in% drop_case_num)
  ,lapa_colon_elective=cpt_code %in% v(c_lapa_colon,dct1) &
    elective_surg=='Yes' &
    !(case_number %in% drop_case_num)
  ,lapa_colon_urgent=cpt_code %in% v(c_lapa_colon,dct1) &
    elective_surg=='No' & emergency_case=='No' &
    !(case_number %in% drop_case_num)
  ,lapa_colon_emergency=cpt_code %in% v(c_lapa_colon,dct1) &
    emergency_case=='Yes' & !(case_number %in% drop_case_num)
  ,ssi_all=a_any_ssi>0
);

sbs0 <- sapply(list(all=dat1,index=dat2),function(xx) do.call(ssply,c(list(dat=xx),subs_criteria[-1])),simplify=F);
sbs0$all2016 <- lapply(sbs0$all,subset,subset=eval(subs_criteria[['y2016']]));
comment(sbs0$all2016$all_colon_all) <- c(comment(sbs0$all2016),'These are only the index colon patients for 2016');
dat1subs <- sbs0$all; comment(dat1subs) <- c(comment(dat1subs),'This is deprecated, used sbs0$all instead');

#' Isolating the 2016 UHS colectomy data elements:
#col2016 <- sbs0$index[["all_colon_all"]] %>% 
#  filter(hospital_admissn_dt < '2017-01-01' & hospital_admissn_dt > '2015-12-31')

#' Merging the datasets:
# the first step in pipeline selects the index surgeries from cost1
subset(cost1,admission_date-1<=operatn_dt&discharge_date+1>=operatn_dt) %>%
  # now merge with all colectomy patients by MRN in order to avoid relying on admitdatediff
  # as a literal merge criterion (because it could permit mismatches as we saw)
  merge(sbs0$all$all_colon_all,.,by=c('idn_mrn','operatn_dt'),all.x=T,all.y=F,suffixes=c('','.junk')) %>%
  # now we have all colectomies one-to-one matched with cost data where available, and just need
  # to get rid of the out-of-range dates. This step has to come last because in future
  # datasets the admit/discharge window can span multiple time periods, and the time 
  # period of interest may vary while the fundamental structure of the merge remains the
  # same
  subset(eval(subs_criteria$y2016)) -> costdata;
#' We now have `r dim(subset(costdata,is.na(admitdatediff)))` patients missing cost 
#' data. This is one more missing than was previously calculated, because that one
#' has an `operatn_dt` that does not fall between the `admission_date` and `discharge_date`.
#' This is the only costdata patient for which this is the case, and the `proc_surg_start`
#' and `proc_surg_finish` fields argee with the `operatn_dt`. Though the patient is
#' eligible, and there is costdata for A visit by that patient, the costdata is not
#' for the index visit.
#' Below follows testing code for establishing that the above pipeling produces a 
#' unique set of index patients meeting the criteria, with a single patient missing 
#' from the earlier version of costdata because they do not have an index surgery
#' 
#' 
#costdata <-  merge(sbs0$all2016$all_colon_all, cost2, by = 'idn_mrn', all.x = TRUE);
#na_in_costdata<-subset(costdata,is.na(admission_date))$idn_mrn;
#' Useful columns
#ccs<-c('admitdatediff','admission_date','discharge_date','proc_surg_start','case_number','idn_mrn','hospital_admissn_dt')
#' Can we rely on the NSQIP variables operatn_dt and proc_surg_start, proc_surg_finish being in agreement
#' with each other, since only the former is in the costdata?
.debug_operatn_dt_mm0 <- subset(dat1,as.Date(proc_surg_start)!=as.Date(operatn_dt))[,c('idn_mrn','proc_surg_start','proc_surg_finish','operatn_dt')] %>% data.frame;
#' There are `r nrow(.debug_operatn_dt_mm)` rows in NSQIP that disagree:
.debug_operatn_dt_mm0[,c('idn_mrn','proc_surg_start','proc_surg_finish','operatn_dt')];
.debug_operatn_dt_mm1 <- subset(.debug_operatn_dt_mm0,as.Date(proc_surg_start)-as.Date(operatn_dt)>2);
#' `r nrow(.debug_operatn_dt_mm1)` of them by more than one day, none of which were in 2016
.debug_operatn_dt_mm1[,c('idn_mrn','proc_surg_start','proc_surg_finish','operatn_dt')];
#' merging all colonectomies (not limited by time) with all available cost data
#' to hopefully resolve a few more missing variables
#costdata0 <- merge(sbs0$all$all_colon_all,cost1,by='idn_mrn',all.x=TRUE,all.y=F,suffixes = c('','.junk'));
#' Dropping all records where the proc_surg_start does not fall between admission_date and discharge_date
#' First create the temporary filtering variables
#costdata1 <- mutate(costdata0,a_srg=as.Date(proc_surg_start),a_adm=admission_date-1,a_dis=discharge_date+1);
#' Then subset on them, keeping also the ones that are not in the costdata via is.na(...)
#costdata2 <- subset(costdata1,is.na(admitdatediff)|(a_srg>=a_adm&a_srg<=a_dis));
#costdata3 <- subset(costdata2
#                    ,(pmin(a_adm,as.Date(hospital_admissn_dt),na.rm=T)<'2017-01-01' &
#                      pmax(a_adm,as.Date(hospital_admissn_dt),na.rm=T)>'2015-12-31'));
#costdata3a <- subset(costdata2,as.Date(hospital_admissn_dt)<'2017-01-01' & 
#                       as.Date(hospital_admissn_dt)>'2015-12-31');
#costdata3b <- subset(costdata2,pmax(hospital_admissn_dt,hospital_admissn_dt.junk,na.rm = T)<'2017-01-01' & 
#                       pmin(hospital_admissn_dt,hospital_admissn_dt.junk,na.rm = T)>'2015-12-31');
#' Do the `operatn_dt` fields match up?
#dim(subset(costdata3,as.Date(proc_surg_start)==as.Date(operatn_dt)));
#dim(costdata3); dim(costdata3a); dim(costdata3b);
#' Yes
#' We have one patient in the original costdata that is not making it into the 
#' new version
#missing_from_costdata3<-setdiff(costdata$idn_mrn,costdata3$idn_mrn);
#missing_from_costdata3a<-setdiff(costdata$idn_mrn,costdata3a$idn_mrn);
# View(subset(dat1,idn_mrn==missing_from_costdata3)[,intersect(names(dat1),ccs)]);
# View(subset(cost1,idn_mrn==missing_from_costdata3)[,intersect(names(cost1),ccs)]);
#' Their admit/discharge dates are the only ones in cost1 that fail to span the surgery
#' date by more than one day
#' 
#' We have duplicates in costdata3:
# length(unique(costdata3$idn_mrn)); # 169
#costdata3dups <- table(costdata3$idn_mrn);
#costdata3dups <- names(costdata3dups[costdata3dups>1]);
#costdata3adups <- table(costdata3a$idn_mrn);
#costdata3adups <- names(costdata3adups[costdata3adups>1]);
#length(unique(costdata3b$idn_mrn)); #169
#setdiff(costdata3b$idn_mrn,costdata3$idn_mrn); # 0
#setdiff(costdata3$idn_mrn,costdata3b$idn_mrn); # 0
#' Conclusion: the costdata3b algorithm pulls non-duplicated patients

#' Filter down to only NHW and hispanic
dat3<-subset(dat2,hispanic_ethnicity!='Unknown'&(hispanic_ethnicity=='Yes'|race=='White'));
dat3$hispanic_ethnicity<-factor(dat3$hispanic_ethnicity);
#'  creating tables similar to the tables that Dan MacCarthy creates for the VASQIP data:


#' Both of our main non time-to-event responses
c_resps <- c('a_postop','a_cd4');

#' ### Create your random sample
source('random_seed.R');
pat_samp <- sample(dat3$idn_mrn,1000,rep=F);
dat4 <- subset(dat3,idn_mrn %in% pat_samp);

