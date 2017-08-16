#' ---
#' title: "RAI, Metadata"
#' author: "Wilson, Bokov, Shireman"
#' date: "08/15/2017"
#' ---
#' 
#' This file (`metadata.R`) is loaded before the data is loaded, so you should not 
#' rely on the data anywhere in this file. Please stick to variable assignments
#' that are not specific to your own computer (those go into `config.R`). Also
#' do not create functions here. Those go into `functions.R`
#' 
#' ## Non-analytic column names or regexps
#' 
#' Please create below as many lines as you need to for the the code you 
#' are using to find the columns which should be excluded from all plots, 
#' tables, and models. These columns include but are not limited to 
#' patient/visit IDs, free text, and anything that is a comma-separated list
#' of codes. These should be character vectors either of column names or of grep
#' patterns for selecting those names.  


dcols <- c("idn_mrn$", "case_number$", "lmrn_visit$",                    
	   "cpt_code$", "cpt_descriptn$", "elective_surg$", 
	   "principal_anesthesia_technique$", "height_unknown$", 
	   "weight_unknown$", "wound_class$", "surg_wound_s_closure$", 
	   "open_wound$", "steroid_immunosupp$", "type_c_diff_test_performed$",
	   "bleeding_disorder$", "preop_tnsf_rbc_72h_prior_surg$", 
	   "treatment_cdiff$", "all_pre_op_labs_unknown$", "other_proc$", 
	   "other_proc_cpt$", "concurrent_proc$", "concurrent_proc_cpt$", 
	   "postop_icd9_code$", "postop_icd9_descriptn$", "postop_icd10_code$",
	   "postop_icd10_descriptn$", "dt_death_unknown$", "unpl_readm$", 
	   "first_unpl_ret_proc$", "first_unpl_ret_cpt$", 
 	   "first_unpl_ret_rel_pr_proc$", "c_diff_loose_stool$", 
	   "second_unpl_ret_proc$", "second_unpl_ret_cpt$", "result_c_diff_test$",
	   "second_unpl_ret_rel_pr_proc$", "more_than_2_unp_ret_or$", 
	   "x30_dy_f_u_complete$", "fu_dy$", "robotic_assisted$"
	  );

dunitcol <- c("height_unit$", "weight_unit$") 


#' ## Column-names that may need to be transformed
#' 
#' Same idea as above section
#' 
#' ### Numeric
#' 
cnum <- c("income_final$", "age_at_time_surg$", "height$", "weight$", "bmi$",
	  "serum_sodium$", "bun$", "serum_creatinine$", "albumin$", 
	  "total_bilirubin$","ast_sgot$", "alkaline_phosphatase$", "wbc$", 
	  "hematocrit$", "platelet_count$", "inr$", "ptt$", 
	  "duratn_surg_proc_minutes$", "total_blood_transfused_units$", 
	  "dvt$", "hospital_length_stay$"
	 );

#' ### Numeric columns that it would make sense to bin
#' 
#' Such as BMI, age, and income
cnum2bin <- c("income_final$", "age_at_time_surg$", "bmi$");

#' ### Date
#' 
cdate <- c("dt_birth$", "hospital_admissn_dt$", "operatn_dt$", "serum_sodium_dt$",
	   "bun_dt$", "serum_creatinine_dt$", "albumin_dt$", "total_bilirubin_dt$",
	   "ast_sgot_dt$", "alkaline_phosphatase_dt$", "wbc_dt$", "hematocrit_dt$",
	   "platelet_count_dt$", "inr_dt$", "ptt_dt$", "proc_surg_start$", 
	   "proc_surg_finish$", "acute_hospital_discharge_dt$", "dt_death$",
	   "dt_death_unknown$", "dt_first_readm$", "dt_first_unpl_ret_or$",
	   "dt_second_unp_ret$"
	  );


#' ### Boolean/Logical
#' 
ctf <-  c("gender$", "in_out_patient_status$", "ascites_30_dy_prior_surg$",
	  "hypertensn_req_medicatn$", "emergency_case$","postop_si_ssi_patos$",
	  "postop_deep_incisnal_ssi$", "current_smoker_within_1_year$", 
	  "postop_deep_incisnal_ssi_patos$", "postop_organ_space_ssi$", 
	  "first_unp_ret_or$", "postop_organ_space_ssi_patos$",
	  "postop_wound_disruptn$", "postop_pneumonia$", "postop_pneumonia_patos$",
	  "postop_vent_48_hr$", "postop_vent_48_hr_patos$", "postop_prog_renal_ins$", 
	  "postop_acute_renal_failure$", "postop_uti$", "postop_uti_patos$", 
	  "postop_cva$", "postop_cardiac_arrest_req_cpr$", 
	  "postop_transf_intraop_72h_surg$", "dvt$", "postop_c_diff$", 
	  "postop_sepsis_patos$", "postop_sepshk$", "postop_sepshk_patos$"
	 );


#' ### Factor/Discrete
#' 
cfactr <- c("race$","hispanic_ethnicity$", "origin_status$", "diabetes_mellitus$",
	   "functnal_heath_status$", "sepsis_sirs_sepsis_sepshk_48h$",
	   "postop_si_ssi$", "postop_unpl_intubatn$", "postop_pulmonary_embolism$",
	   "asa_class$", "postop_sepsis$", "postop_mi$", "readm_30_dy$",
	   "unpl_readm$", "readm_likely_rel_pr_proc$", "readm_likely_unrel_pr_proc$",
	   "first_unpl_ret_proc$", "hospital_discharge_destinatn$", 
	   "second_unpl_ret_proc$", "still_hospital_30_dy$",
	   "postop_death_30_dy_proc$", "postop_death_gt_30_dy_proc$",
	   "end_life_withdrawal_care$", "second_unp_ret_or$"
	   );


#' ### Integer
#' 
cintgr <- c();


#' ## Outcomes
#'
#' ### Clavien-Dindo 4 column names
#' 
ccd4 <- c();


#' ### All Complications column names
#' 
ccomp <- c();


#' ### Complications to exclude column names (2)
#' 
ccompexc <- c();

#' ### Serious complications
#' 
csrscomp <- setdiff(ccomp,ccompexc);

#' ### PATOS
#' 
#' Columns that _have_ a corresponding PATOS (present at time of surgery) 
#' column
chavepatos <- c();
#' Columns that _are_ PATOS columns 
#' 
#' Note that this one is a regexp so let's name the variable that holds it
#' a name that begins with 'g'. Feel free to make whichever variables are
#' convenient into lists or regexps instead of literal names.
garepatos <- c('_patos$');


#' ### Time-to-event column names
#' 
ctime <- c();


#' ## Predictors
#' 
#' ### RAI components
crai <- c("dyspnea$", "functnal_heath_status$", "vent_dependent$", 
	  "history_severe_copd$", "acute_renal_failure$", "currently_dialysis$",
	  "disseminated_cancer$", "x_loss_bw_6_months_prior_surg$", 
 	  "chr_30_dy_prior_surg$");


#' ### Various individual predictors
#'
#' (replace the empty strings with actual column names of your data frame)
#' These start with v to indicate individual exact variable names rather than
#' vectors
#' 
#' Hispanic
vhisp <- '';
#' Income
vinc <- '';
#' Patient ID (for grouping)
#vid <- '';
#' Date of surgery
vdts <- '';
#' Date of birth
vdob <- '';
#' Sex
vsex <- '';
#' Race
vrace <- '';
#' InPatient vs Outpatient
vpatstat <- '';
#' Elective Surgery
velectsurg <- '';
#' Origin Status
vpatstat <- '';
#' Diabetes Status
vdiab <- '';
#' Smoking Status
vsmoke <- '';
#' Vent Dependent
vvent <- '';
#' COPD Status
vcopd <- '';
#' Living Status
vindepn <- '';
#' Hypertension Status
vhbld <- '';
#' Sepsis Status
vsepsis <- '';
