#' ---
#' title: "RAI Code Tests"
#' author: "Wilson, Bokov, Shireman"
#' date: "02/16/2018"
#' ---
#' These are automated tests for checking the _code_ rather
#' than the data quality checks in the file currently named
#' `DataCleaning.R`. This is a self-contained file, and will
#' do the various tests immediately following the files they
#' are supposed to be testing. It may run for a rather long
#' time because at the moment it does not cache results, until
#' we are sure we have a way to do that without hiding errors.
source('global.R');
require(assertthat);
#' Test run.R
#+ echo=FALSE, message=FALSE
source('run.R');
#' Does the old (corrected) algorithm for TRUE/FALSE rockwood values give
#' the same results as the new one?
test00<-list(
  n_origrock=with(dat1,(
  #as.numeric(bmi>=25)+
    as.numeric(!origin_status%in%c('Unknown','Not transferred (admitted from home)'))+
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
    as.numeric(chr_30_dy_prior_surg=='Yes')
    #as.numeric(isTRUE(serum_creatinine>3))
    ))
  ,n_newrock = apply(dat1[,v(c_rock_tf)],1
                    ,function(xx) sum(truthy(xx,truewords=l_rockwood_true),na.rm = T)
                    )
);

test00$tab_rock<-unname(with(test00
                      ,as.matrix(as.data.frame.matrix(table(n_origrock,n_newrock)))));
dim(test00$tab_rock);
nrow(test00$tab_rock);
sum(test00$tab_rock!=0);
#test00$tab_rock <- diag(nrow(test00$tab_rock))*test00$tab_rock;
matrix(0,nrow=9,ncol=9);
class(test00$tab_rock);
test00$tab_rock;
sum(test00$tab_rock);
diag(test00$tab_rock);
sum(diag(test00$tab_rock));
#if(not(0==with(test00,sum(tab_rock)-sum(diag(tab_rock))))) 
#  stop('Hard-coded calculation of Rockwood index (for T/F items) does not match dynamic calculation.');

