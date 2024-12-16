######I. CKB analyses:######
####Load data####
baseline_Q_all =read_csv("K:/ckb_data/Staff_Folders/bowen_liu/DAR-2023-00339-V1/data_baseline_questionnaires.csv")
combined_endpoints <- read_csv("K:/ckb_data/Staff_Folders/bowen_liu/DAR-2023-00339-V1/combined_endpoints.csv")
#to do analysis without DCOs:
#combined_endpoints <- read_csv("K:/ckb_data/Staff_Folders/bowen_liu/DAR-2023-00339-V1/combined_endpoints_noDCO.csv")
dat_cancer=merge(combined_endpoints[,c("csid","ep_CKB0014_combined_ep","ep_CKB0014_combined_datedeveloped",
                                       "ep_CKB0202_combined_ep","ep_CKB0202_combined_datedeveloped",
                                       "ep_CKB0020_combined_ep","ep_CKB0020_combined_datedeveloped",
                                       "ep_CKB0016_combined_ep","ep_CKB0016_combined_datedeveloped",
                                       "ep_CKB0018_combined_ep","ep_CKB0018_combined_datedeveloped",
                                       "ep_CKB0015_combined_ep","ep_CKB0015_combined_datedeveloped",
                                       "ep_CKB0201_combined_ep","ep_CKB0201_combined_datedeveloped",
                                       "ep_CKB0019_combined_ep","ep_CKB0019_combined_datedeveloped",
                                       "ep_CKB0027_combined_ep","ep_CKB0027_combined_datedeveloped",
                                       "ep_CKB0017_combined_ep","ep_CKB0017_combined_datedeveloped",
                                       "ep_CKB0021_combined_ep","ep_CKB0021_combined_datedeveloped",
                                       "ep_range_C82_to_C85_combined_ep", "ep_range_C82_to_C85_combined_datedeveloped",
                                       "ep_CKB0028_combined_ep","ep_CKB0028_combined_datedeveloped",
                                       "ep_CKB0022_combined_ep","ep_CKB0022_combined_datedeveloped",
                                       "ep_CKB0023_combined_ep","ep_CKB0023_combined_datedeveloped",
                                       "ep_CKB0024_combined_ep","ep_CKB0024_combined_datedeveloped",
                                       "ep_CKB0025_combined_ep","ep_CKB0025_combined_datedeveloped",
                                       "ep_CKB0026_combined_ep","ep_CKB0026_combined_datedeveloped",
                                       "ep_range_C23_to__combined_ep","ep_range_C23_to__combined_datedeveloped",
                                       "ep_range_C43.0_to_C43.9_combined_ep", "ep_range_C43.0_to_C43.9_combined_datedeveloped",
                                       "ep_range_C71.0_to_C71.9_combined_ep", "ep_range_C71.0_to_C71.9_combined_datedeveloped",
                                       "ep_range_C73_to__combined_ep", "ep_range_C73_to__combined_datedeveloped",
                                       "ep_range_C90_to__combined_ep","ep_range_C90_to__combined_datedeveloped", 
                                       "ep_range_C16.0_to__combined_ep", "ep_range_C16.0_to__combined_datedeveloped" ,
                                       "ep_range_C16.1_to_C16.6_combined_ep","ep_range_C16.1_to_C16.6_combined_datedeveloped",
                                       "ep_range_C18.0_to_C18.9_combined_ep","ep_range_C18.0_to_C18.9_combined_datedeveloped",
                                       "ep_range_C19_to_C20_combined_ep" ,"ep_range_C19_to_C20_combined_datedeveloped" ,
                                       "ep_CKB0030_combined_ep","ep_CKB0030_combined_datedeveloped",
                                       "ep_CKB0042_combined_ep","ep_CKB0042_combined_datedeveloped",
                                       "ep_CKB0048_combined_ep","ep_CKB0048_combined_datedeveloped",
                                       "ep_CKB0209_combined_ep","ep_CKB0209_combined_datedeveloped",
                                       "ep_CKB0210_combined_ep","ep_CKB0210_combined_datedeveloped",
                                       "ep_CKB0212_combined_ep","ep_CKB0212_combined_datedeveloped",
                                       "ep_CKB0211_combined_ep","ep_CKB0211_combined_datedeveloped")],
                 baseline_Q_all[,c("csid","is_female","age_at_study_date_x100",
                                   "region_code","region_is_urban","study_date","diabetes_diag","diabetes_diag_age","has_diabetes","dob_anon",
                                   "chd_diag","cancer_diag","stroke_or_tia_diag","cirrhosis_hep_diag",
                                   "smoking_category","cig_equiv_day","alcohol_category","total_alc_av_week_g","total_alc_typ_day_g","highest_education","met","bmi_calc","waist_mm","waist_hip_ratio",
                                   "rheum_arthritis_diag","random_glucose_x10","hours_since_last_ate_x10",
                                   "had_ovary_removed","had_hysterectomy","pill_use","first_period_age","menopause_status","hep_b","standing_height_mm",
                                   "sbp_mean","dbp_mean","hypertension_diag","kidney_dis_diag","peptic_ulcer_diag","mother_diabetes",
                                   "father_diabetes","siblings_diabetes","age_25_weight_jin","father_cancer","mother_cancer","siblings_cancer",
                                   "gall_diag","copd","live_birth_age01","live_birth_count","preg_count","menopause_age","siblings"
                 )],
                 by="csid")
names(dat_cancer)[2:69]=c("case_all","timeout_all",
                          "case_kid","timeout_kid",
                          "case_panc","timeout_panc",
                          "case_esop","timeout_esop",
                          "case_colc","timeout_colc",
                          "case_lop","timeout_lop",
                          "case_lary","timeout_lary",
                          "case_liv","timeout_liv",
                          "case_blad","timeout_blad",
                          "case_stom","timeout_stom",
                          "case_lung","timeout_lung",
                          "case_NHL","timeout_NHL",
                          "case_leuk","timeout_leuk",
                          "case_bres","timeout_bres",
                          "case_cerx","timeout_cerx",
                          "case_endo","timeout_endo",
                          "case_ovay","timeout_ovay",
                          "case_pros","timeout_pros",
                          "case_gall","timeout_gall",
                          "case_mela","timeout_mela",
                          "case_brain","timeout_brain",
                          "case_thy","timeout_thy",
                          "case_mmyl","timeout_mmyl",
                          "case_card","timeout_card",
                          "case_ncard","timeout_ncard",
                          "case_col","timeout_col",
                          "case_rect","timeout_rect",
                          "case_ill","timeout_ill",
                          "case_DM","timeout_DM",
                          "case_T2DM","timeout_T2DM",
                          "case_lsqcc","timeout_lsqcc",
                          "case_lsmcc","timeout_lsmcc",
                          "case_llcc","timeout_llcc",
                          "case_lac","timeout_lac"
)
####Baseline variables####
#Transform age at study data to years:
dat_cancer$age_at_study_date_x100=dat_cancer$age_at_study_date_x100/100
names(dat_cancer)[names(dat_cancer)=="age_at_study_date_x100"]="age_at_study_date"
dat_cancer$diabetes_scre=ifelse(dat_cancer$has_diabetes==1&dat_cancer$diabetes_diag==0,1,0)
#alcohol
dat_cancer$alc_amt=as.factor(cut(dat_cancer$total_alc_av_week_g/56,breaks=c(0,1,2,200),include.lowest=TRUE,right=TRUE,labels=FALSE))
levels(dat_cancer$alc_amt)=c("<1 unit","1-2",">2 units")
dat_cancer$alc_amt=as.character(dat_cancer$alc_amt)
dat_cancer$alc_amt[is.na(dat_cancer$alc_amt)]="unknown"
dat_cancer$alcohol_category[dat_cancer$alcohol_category==6]=dat_cancer$alc_amt[dat_cancer$alcohol_category==6]
dat_cancer$alcohol_category=as.factor(dat_cancer$alcohol_category)
#smoking
dat_cancer$smk_amt=as.factor(cut(dat_cancer$cig_equiv_day,breaks=c(0,15,200),include.lowest=TRUE,right=TRUE,labels=FALSE))
levels(dat_cancer$smk_amt)=c("current <15","current >=15")
dat_cancer$smk_amt=as.character(dat_cancer$smk_amt)
dat_cancer$smk_amt[is.na(dat_cancer$smk_amt)]="unknown"
dat_cancer$smoking_category[dat_cancer$smoking_category==4]=dat_cancer$smk_amt[dat_cancer$smoking_category==4]
dat_cancer$smoking_category=as.factor(dat_cancer$smoking_category)

#Female reproductive factors
dat_cancer$had_hysterectomy[is.na(dat_cancer$had_hysterectomy)]=999
dat_cancer$had_ovary_removed[is.na(dat_cancer$had_ovary_removed)]=999

dat_cancer$pill=dat_cancer$pill_use
dat_cancer$pill[dat_cancer$is_female==0]="not applicable"
dat_cancer$pill[is.na(dat_cancer$pill)]="not applicable"
dat_cancer$pill=as.factor(dat_cancer$pill)

dat_cancer$age_menarche=as.numeric(dat_cancer$first_period_age)
dat_cancer$age_menarche[dat_cancer$is_female==0]=NA


dat_cancer$meno=dat_cancer$menopause_status
dat_cancer$meno[dat_cancer$is_female==0]="not applicable"
dat_cancer$meno=as.factor(dat_cancer$meno)
dat_cancer$menopause_age[dat_cancer$is_female==0]=NA
dat_cancer$menopause_age[dat_cancer$meno==0|dat_cancer$meno==1]=NA

dat_cancer$live_birth_count[dat_cancer$is_female==0]=NA
dat_cancer$live_birth_age01[dat_cancer$is_female==0]=NA
dat_cancer$nullpar=ifelse(dat_cancer$live_birth_count==0,1,0)
dat_cancer$nullpar[is.na(dat_cancer$nullpar)]=0
dat_cancer$live_birth_count[dat_cancer$live_birth_count>=5]=5
dat_cancer$live_birth_count[dat_cancer$live_birth_count==1|dat_cancer$live_birth_count==2]="1-2"
dat_cancer$live_birth_count[dat_cancer$live_birth_count==3|dat_cancer$live_birth_count==4]="3-4"
dat_cancer$live_birth_count[is.na(dat_cancer$live_birth_count)]="not applicable"
dat_cancer$live_birth_count=as.factor(dat_cancer$live_birth_count)

dat_cancer$birthage=dat_cancer$live_birth_age01
dat_cancer$birthage[dat_cancer$is_female==0]=NA
save_dat_cancer=dat_cancer
####cox Analyses####
#allsex:
i=NULL
test_data=data.frame()
fit=list()
result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
CKB_allsex=data.frame()

for(i in c(1:23,26:28)){
#datset for certain cancer
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
#Site-specific cancer:exclude secondary cases
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
#Age at censor:
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
#Breast cancer: exclude those censored at age 50
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
#birth cohort and duration of follow up:
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy!=1&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed!=1&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
#analyses for both sexes: any diabetes and self-reported diabetes
  if(!(i %in% c(14:18))){
    #formalise covariates and label NA as not applicable
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    #two cox regression analyses for different diabetes definitions:
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_diag=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                       " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                       "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary=data.frame(rbind(result_summary_any,result_summary_diag))
    names(result_summary)=c("coef","se","p","HRCI")
    #calculate case numbers:
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")]))),
                           nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                "region_code","study_date","has_diabetes",
                                                                                                                "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")]))),
                             nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                  "region_code","study_date","has_diabetes",
                                                                                                                  "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                  "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("Any diabetes-all","Self-reported-all")
  }else{
    result_summary=NA
  }
  #for female:
  if(i!=18){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_diag=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                       " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                       "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any,result_summary_diag))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")]))),
                             nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","has_diabetes",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")]))),
                               nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","has_diabetes",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("Any diabetes-women","Self-reported-women")
    
    
  }else{
    result_summary_w=NA
  }
  #for male: no female reproductive factors
  if(!(i %in% c(14:17))){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    #regression of model 2 (with bmi adjustments) for any diabetes and self-reported diabetes  
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_diag=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                       " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                       "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any,result_summary_diag))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")]))),
                             nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","has_diabetes",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")]))),
                               nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","has_diabetes",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("Any diabetes-men","Self-reported-men")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}


write.csv(CKB_allsex,  "M:/Nov/CKB_all.csv")

######II. UKB analyses:######
####Load data####
UKB_baseline_general_nocan <- read_csv("UKB_baseline_general_noncancer_nonsexmismatch.csv")
UKB_cancer <- read_csv("UKB_cancer_230831.csv")#excluding baseline cancer (malignant) by icd9 and 10.
dat_cancer_UKB=merge(UKB_cancer[,c("eID","exit_date","allcan_inc",
                                   "kidney_inc",
                                   "pancreas_inc",
                                   "oesoph_inc",
                                   "colorectal_inc",
                                   "oral_inc",
                                   "larynx_inc",
                                   "liver_inc",
                                   "bladder_inc",
                                   "stomach_inc",
                                   "lung_inc",
                                   "nhl_inc",
                                   "leuk_inc",
                                   "breast_inc",
                                   "cervix_inc",
                                   "endometrial_inc",
                                   "ovary_inc",
                                   "prostate_inc",
                                   "gallbladder_inc",
                                   "melanoma_inc",
                                   "brain_inc",
                                   "cns_inc",
                                   "thyroid_inc",
                                   "multmyeloma_inc",
                                   "colon_inc",
                                   "rectum_inc",
                                   "cardia_inc",
                                   "ncardia_inc",
                                   "illdef_inc",
                                   
                                   "squamous_oesoph_car", "adeno_oesoph_car", "squamous_lung_car", 
                                   "adeno_lung_car", "small_lung_car", "large_lung_car",
                                   "prostate_inc_ca_death",
                                   "prostate_inc_ca_death_date"
)],

UKB_baseline_general_nocan[,c("eID","sex","age_recruit",
                              "region","ukb_asscen_0_0","recruit_date","diabetes_tscr_0_0","dob",
                              "depq","townsendDep","townsendG","education_0_0","qualifications3_2","ethnicity5",
                              "angina_dr", "stroke_dr", "highBP_dr", "num_cigs_curr_0_0","smoke_status_0_0",
                              "smokeG5","alcohol_0_0","alcohol_cat","drinkoverall","n_22040_0_0","n_22032_0_0","weight_0_0","bmi","waistcircumf_0_0","whr","bodysizeage10_0_0",
                              "n_30740_0_0","n_30750_0_0","n_74_0_0",
                              "ovaries","hysterectomy_cat","ocp_npc","age_menarche_0_0","hrt_npc","menopause",
                              "standheight_0_0","sbp_ave2","dbp_ave2",
                              "fh_dad_crc", "fh_mum_crc", "fh_sib_crc", "fh_crc",
                              "fh_mum_brc", "fh_sib_brc", "fh_brc", 
                              "fh_dad_lc", "fh_mum_lc", "fh_sib_lc", "fh_lc",
                              "fh_dad_pc", "fh_sib_pc", "fh_pc",
                              "age_menopause_0_0", "live_births_0_0", "n_2754_0_0", "n_3829_0_0", "n_3839_0_0", "n_3849_0_0",
                              "screen_breast_0_0", "screen_bowel_0_0", "screen_psa_0_0","n_2976_0_0"
)],
by.x="eID",by.y="eID")

#load in self-reported disease history data:
UKB_baseline_sr_disease <- read_csv("M:/UKB_prev_sf.csv")
i=NULL
GORD=rep(0,nrow(UKB_baseline_sr_disease))#GORD
BARR=rep(0,nrow(UKB_baseline_sr_disease))#Barretts oesophagus
HPLY=rep(0,nrow(UKB_baseline_sr_disease))#H.pylori infection
EBV=rep(0,nrow(UKB_baseline_sr_disease))#EBV infection
HEP=rep(0,nrow(UKB_baseline_sr_disease))#hepatitis
vHEP=rep(0,nrow(UKB_baseline_sr_disease))#viral hepapatis
HBV=rep(0,nrow(UKB_baseline_sr_disease))
HCV=rep(0,nrow(UKB_baseline_sr_disease))
CIRR=rep(0,nrow(UKB_baseline_sr_disease))#cirrhosis/liver failure
GALL=rep(0,nrow(UKB_baseline_sr_disease))#gallbladder disease
GALLstone=rep(0,nrow(UKB_baseline_sr_disease))
PANC=rep(0,nrow(UKB_baseline_sr_disease))#pancreatitis
DIAY=rep(0,nrow(UKB_baseline_sr_disease))#renal failure requiring dialysis
RENLstone=rep(0,nrow(UKB_baseline_sr_disease))#	kidney stone/ureter stone/bladder stone
COPD=rep(0,nrow(UKB_baseline_sr_disease))
CROHN=rep(0,nrow(UKB_baseline_sr_disease))
HYPER=rep(0,nrow(UKB_baseline_sr_disease))
HIV=rep(0,nrow(UKB_baseline_sr_disease))
CHD=rep(0,nrow(UKB_baseline_sr_disease))
STRK=rep(0,nrow(UKB_baseline_sr_disease))
for(i in 2:ncol(UKB_baseline_sr_disease)){
  UKB_baseline_sr_disease[,i][is.na(UKB_baseline_sr_disease[,i])]=-1
  GORD[UKB_baseline_sr_disease[,i]==1138]=1
  BARR[UKB_baseline_sr_disease[,i]==1139]=1
  HPLY[UKB_baseline_sr_disease[,i]==1442]=1
  EBV[UKB_baseline_sr_disease[,i]==1567]=1
  HEP[UKB_baseline_sr_disease[,i]==1155]=1
  vHEP[UKB_baseline_sr_disease[,i]==1156]=1
  HBV[UKB_baseline_sr_disease[,i]==1579]=1
  HCV[UKB_baseline_sr_disease[,i]==1580]=1
  CIRR[UKB_baseline_sr_disease[,i]==1158]=1
  GALL[UKB_baseline_sr_disease[,i]==1161]=1
  GALLstone[UKB_baseline_sr_disease[,i]==1162]=1
  PANC[UKB_baseline_sr_disease[,i]==1165]=1
  DIAY[UKB_baseline_sr_disease[,i]==1193]=1
  RENLstone[UKB_baseline_sr_disease[,i]==1197]=1
  COPD[UKB_baseline_sr_disease[,i]==1112]=1
  CROHN[UKB_baseline_sr_disease[,i]==1462]=1
  HYPER[UKB_baseline_sr_disease[,i]==1065]=1
  HIV[UKB_baseline_sr_disease[,i]==1439]=1
  CHD[UKB_baseline_sr_disease[,i]==1075]=1
  STRK[UKB_baseline_sr_disease[,i]==1081]=1
  
}
UKB_sf_med=data.frame(UKB_baseline_sr_disease$eID,GORD,BARR,HPLY,EBV,HEP,vHEP,HBV,HCV,CIRR,GALL,GALLstone,PANC,DIAY,
                      RENLstone,COPD,CROHN,HYPER,HIV,CHD,STRK)
names(UKB_sf_med)[1]="eID"
#load family history data:
UKB_fh <- read_csv("M:/UKB_fh.csv")
i=NULL
fh_dm=rep(0,nrow(UKB_fh))

for(i in 2:ncol(UKB_fh)){
  UKB_fh[,i][is.na(UKB_fh[,i])]="No records"
  fh_dm[UKB_fh[,i]=="Diabetes"]=1
}

UKB_fh_dm=data.frame(UKB_fh$eID,fh_dm)
names(UKB_fh_dm)[1]="eID"
#merge datasets together:
dat_cancer_UKB=merge(dat_cancer_UKB,UKB_fh_dm,by="eID")
dat_cancer_UKB=merge(dat_cancer_UKB,UKB_sf_med[,c("eID","HYPER")],by="eID")

#reformat exit date:
dat_cancer_UKB$exit_date=as.Date(dat_cancer_UKB$exit_date, format="%d%b%Y")
####baseline variables
#run this to censor the Welsh participants at the end of 2016:only apply to this version of UKB data
dat_cancer_UKB$exit_date_corr=dat_cancer_UKB$exit_date
dat_cancer_UKB$exit_date_corr[dat_cancer_UKB$region=="Wales"&dat_cancer_UKB$exit_date_corr>"2016-12-31"]="2016-12-31"
#Create variables related to age and follow up period/dates:
dat_cancer_UKB$dob=as.Date(dat_cancer_UKB$dob, format="%d%b%Y")
dat_cancer_UKB$recruit_date=as.Date(dat_cancer_UKB$recruit_date, format="%d%b%Y")
#names(dat_cancer_UKB)[names(dat_cancer_UKB)=="age_recruit"]="age_at_study_date"
dat_cancer_UKB$age_at_study_date=as.numeric((as.POSIXct(dat_cancer_UKB$recruit_date)-as.POSIXct(dat_cancer_UKB$dob)))/365.25
dat_cancer_UKB$age_at_censor=as.numeric((as.POSIXct(dat_cancer_UKB$exit_date_corr)-as.POSIXct(dat_cancer_UKB$dob)))/365.25
dat_cancer_UKB$dob_year=as.numeric(format(dat_cancer_UKB$dob,format="%Y"))
dat_cancer_UKB$birth_cohort=cut(dat_cancer_UKB$dob_year,breaks=c(seq(1934,1971,5),1972),right=FALSE)
dat_cancer_UKB$diabetes_diag=ifelse(dat_cancer_UKB$diabetes_tscr_0_0=="Yes",1,0)
dat_cancer_UKB$duration <- difftime(dat_cancer_UKB$exit_date_corr,dat_cancer_UKB$recruit_date,units="days")

#Exclusion criteria:
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$duration>0&!is.na(dat_cancer_UKB$diabetes_diag),]

#Create screen-detected diabetes based on glucose and A1C levels
names(dat_cancer_UKB)[names(dat_cancer_UKB)=="n_74_0_0"]="FT"
names(dat_cancer_UKB)[names(dat_cancer_UKB)=="n_30750_0_0"]="A1c"
names(dat_cancer_UKB)[names(dat_cancer_UKB)=="n_30740_0_0"]="RBG"
dat_cancer_UKB$byRBG=ifelse(c(dat_cancer_UKB$RBG>7&dat_cancer_UKB$FT>=8)|c(dat_cancer_UKB$RBG>11.1),TRUE,FALSE)
dat_cancer_UKB$byRBG[is.na(dat_cancer_UKB$RBG)]=NA
dat_cancer_UKB$byRBG[is.na(dat_cancer_UKB$FT)]=NA
dat_cancer_UKB$byA1c=ifelse(dat_cancer_UKB$A1c>48,TRUE,FALSE)

a=dat_cancer_UKB$byRBG
b=dat_cancer_UKB$byA1c
a[is.na(a)]=FALSE
b[is.na(b)]=FALSE

dat_cancer_UKB$byboth=a|b


#self-reported diabetes:
dat_cancer_UKB$has_diabetes=ifelse(dat_cancer_UKB$byboth|dat_cancer_UKB$diabetes_diag==1,1,0)
dat_cancer_UKB$diabetes_scre=ifelse(dat_cancer_UKB$byboth&dat_cancer_UKB$diabetes_diag==0,1,0)
dat_cancer_UKB$diabetes_scre[dat_cancer_UKB$byboth&is.na(dat_cancer_UKB$diabetes_diag)]=1
dat_cancer_UKB$diabetes_scre[is.na(dat_cancer_UKB$diabetes_scre)]=0


#alcohol
dat_cancer_UKB$alcohol_cat[is.na(dat_cancer_UKB$alcohol_cat)]="unknown"
dat_cancer_UKB$alcohol_cat[dat_cancer_UKB$alcohol_cat=="Unknown"&c(dat_cancer_UKB$alcohol_0_0=="Daily or almost daily"|
                                                                     dat_cancer_UKB$alcohol_0_0=="Once or twice a week"|
                                                                     dat_cancer_UKB$alcohol_0_0=="Three or four times a week")]="Weekly,amount unknown"
dat_cancer_UKB$alcohol_cat[dat_cancer_UKB$alcohol_cat=="Unknown"&c(dat_cancer_UKB$alcohol_0_0=="One to three times a month")]="Monthly,amount unknown"
dat_cancer_UKB$alcohol_cat[dat_cancer_UKB$alcohol_cat=="<1g/d"&dat_cancer_UKB$alcohol_0_0=="Never"]="Never"
dat_cancer_UKB$alcohol_cat[dat_cancer_UKB$alcohol_cat=="<1g/d"&dat_cancer_UKB$alcohol_0_0=="Special occasions only"]="Special occasions only"

#socio-economic factors:
dat_cancer_UKB$ethnicity5[is.na(dat_cancer_UKB$ethnicity5)]="unknown"
dat_cancer_UKB$education_0_0[is.na(dat_cancer_UKB$education_0_0)]="Prefer not to answer"

#generate two subtypes of oesophageal cancer
dat_cancer_UKB$oesophadeno_inc=ifelse(dat_cancer_UKB$oesoph_inc==1&dat_cancer_UKB$adeno_oesoph_car==1,1,0)
dat_cancer_UKB$oesophsquamous_inc=ifelse(dat_cancer_UKB$oesoph_inc==1&dat_cancer_UKB$squamous_oesoph_car==1,1,0)

#Exclusion criteria:
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$duration>0&!is.na(dat_cancer_UKB$has_diabetes),]


#female reproductive factors
dat_cancer_UKB$ovaries[is.na(dat_cancer_UKB$ovaries)]="unknown"
dat_cancer_UKB$hysterectomy_cat[is.na(dat_cancer_UKB$hysterectomy_cat)]="unknown"
#formalise covariates in the cox model
dat_cancer_UKB$smk=as.factor(dat_cancer_UKB$smokeG5)
dat_cancer_UKB$alc=as.factor(dat_cancer_UKB$alcohol_cat)
dat_cancer_UKB$edu=as.factor(dat_cancer_UKB$education_0_0)
dat_cancer_UKB$region=as.factor(dat_cancer_UKB$region)
dat_cancer_UKB$dep=as.factor(dat_cancer_UKB$townsendG)
dat_cancer_UKB$ethnicity5=as.factor(dat_cancer_UKB$ethnicity5)
dat_cancer_UKB$bodysizeage10_0_0=as.factor(dat_cancer_UKB$bodysizeage10_0_0)
dat_cancer_UKB$HYPER[dat_cancer_UKB$highBP_dr==1]=1
dat_cancer_UKB$HYPER[is.na(dat_cancer_UKB$HYPER)]=0
dat_cancer_UKB$HYPER=as.factor(dat_cancer_UKB$HYPER)

dat_cancer_UKB$pill=dat_cancer_UKB$ocp_npc
dat_cancer_UKB$pill[dat_cancer_UKB$sex=="Male"]="unknown"
dat_cancer_UKB$pill[is.na(dat_cancer_UKB$pill)]="unknown"
dat_cancer_UKB$pill=as.factor(dat_cancer_UKB$pill)

dat_cancer_UKB$age_menarche=as.numeric(dat_cancer_UKB$age_menarche_0_0)
dat_cancer_UKB$age_menarche[dat_cancer_UKB$sex=="Male"]=NA


dat_cancer_UKB$meno=dat_cancer_UKB$menopause
dat_cancer_UKB$meno[dat_cancer_UKB$sex=="Male"]="unknown/missing"
dat_cancer_UKB$meno=as.factor(dat_cancer_UKB$meno)
dat_cancer_UKB$age_meno=dat_cancer_UKB$age_menopause_0_0
dat_cancer_UKB$age_meno[dat_cancer_UKB$age_meno=="Do not know"|dat_cancer_UKB$age_meno=="Prefer not to answer"|dat_cancer_UKB$sex=="Male"]=NA

dat_cancer_UKB$hrt=dat_cancer_UKB$hrt_npc
dat_cancer_UKB$hrt[dat_cancer_UKB$sex=="Male"]="missing/unknown"
dat_cancer_UKB$hrt=as.factor(dat_cancer_UKB$hrt)

dat_cancer_UKB$live_births_0_0[dat_cancer_UKB$live_births_0_0=="Prefer not to answer"]=NA
dat_cancer_UKB$live_births_0_0[dat_cancer_UKB$sex=="Male"]=NA
dat_cancer_UKB$live_births_0_0=as.numeric(dat_cancer_UKB$live_births_0_0)
dat_cancer_UKB$live_births_0_0[dat_cancer_UKB$live_births_0_0>=5]=5
dat_cancer_UKB$live_births_0_0[dat_cancer_UKB$live_births_0_0==1|dat_cancer_UKB$live_births_0_0==2]="1-2"
dat_cancer_UKB$live_births_0_0[dat_cancer_UKB$live_births_0_0==3|dat_cancer_UKB$live_births_0_0==4]="3-4"
dat_cancer_UKB$live_births_0_0[is.na(dat_cancer_UKB$live_births_0_0)]="not applicable"
dat_cancer_UKB$live_births_0_0=as.factor(dat_cancer_UKB$live_births_0_0)


dat_cancer_UKB$nullpar=ifelse(dat_cancer_UKB$live_births_0_0==0,1,0)
dat_cancer_UKB$nullpar[is.na(dat_cancer_UKB$nullpar)]=0
dat_cancer_UKB$birthage=dat_cancer_UKB$n_2754_0_0
dat_cancer_UKB$birthage[dat_cancer_UKB$sex=="Male"]=NA
#load an extra variable about alcohol intake
UKB_alc_status <- read_csv("M:/UKB_alcohol_status.csv")
dat_cancer_UKB=merge(dat_cancer_UKB,UKB_alc_status,by="eID")
dat_cancer_UKB$alcohol_cat[dat_cancer_UKB$alcohol_status_0_0=="Previous"]="Previous"
dat_cancer_UKB$alc=as.factor(dat_cancer_UKB$alcohol_cat)

dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$alcohol_cat=="8-15g/d"|dat_cancer_UKB$alcohol_cat=="16+g/d",]
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$alcohol_cat=="1-7g/d"|dat_cancer_UKB$alcohol_cat=="<1g/d"| 
                                dat_cancer_UKB$alcohol_cat=="Monthly,amount unknown"|dat_cancer_UKB$alcohol_cat=="Weekly,amount unknown",]
#dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$alcohol_cat=="Never"|dat_cancer_UKB$alcohol_cat=="Previous"|dat_cancer_UKB$alcohol_cat=="Special occasions only",]
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$alcohol_cat=="Never"|dat_cancer_UKB$alcohol_cat=="Special occasions only",]

save_dat_cancer_UKB=dat_cancer_UKB
####Cox regression####
i=NULL
test_data=data.frame()
fit=list()
result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
UKB_allsex=data.frame()
for(i in c(3:5,7,10:20,27:28,119,120)){
#dataset for certain cancer:
  test_data=dat_cancer_UKB[,c(1,2,i,40:ncol(dat_cancer_UKB))]
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  #exclude those censored after age 50 for breast cancer, and those with hysterectomy /ovary removal for the relevant cancer:
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  #analysis for all sex combined: for two different diabetes' definitions
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_diag=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                       " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                       "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary=data.frame(rbind(result_summary_any,result_summary_diag))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
                                                                                                               ,
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")]))),
                           nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                "region","age_at_censor","has_diabetes",
                                                                                                                "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
                                                                                                                ,
                                                                                                                "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
                                                                                                                 ,
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")]))),
                             nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                  "region","age_at_censor","has_diabetes",
                                                                                                                  "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
                                                                                                                  ,
                                                                                                                  "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("Any diabetes-all","Self-reported-all")
  }else{
    result_summary=NA
  }
  #analyses for female:
  if(i!=20){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_diag=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                       " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                       "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any,result_summary_diag))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
                                                                                                                       ,
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")]))),
                             nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","has_diabetes",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
                                                                                                                        ,
                                                                                                                        "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
                                                                                                                         ,
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")]))),
                               nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","has_diabetes",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
                                                                                                                          ,
                                                                                                                          "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("Any diabetes-women","Self-reported-women")
    
    
  }else{
    result_summary_w=NA
  }
  #analyses for male:
  if(!(i %in% c(16:19))){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_diag=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                       " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                       "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any,result_summary_diag))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )]))),
    nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                               "region","age_at_censor","has_diabetes",
                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )]))),
    nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                               "region","age_at_censor","has_diabetes",
                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("Any diabetes-men","Self-reported-men")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

write.csv(UKB_allsex,"M:/Nov/UKB_all.csv")

####III. MWS analyses####
mws <- read_csv("M:/mws/bowen_mws_refresh_20240318.csv")

####Incidence:----
#incident cancer outcomes
mws$morph=as.numeric(substr(mws$cancer2_histology,1,4))


mws$allcan_inc=ifelse(grepl("C",mws$cancer2_icd4),1,0)
mws$kidney_inc=ifelse(grepl("C64",mws$cancer2_icd4),1,0)
mws$pancreas_inc=ifelse(grepl("C25",mws$cancer2_icd4),1,0)
mws$oesoph_inc=ifelse(grepl("C15",mws$cancer2_icd4),1,0)
mws$colorectal_inc=ifelse(grepl("C18",mws$cancer2_icd4)|
                            grepl("C19",mws$cancer2_icd4)|
                            grepl("C20",mws$cancer2_icd4),1,0)

mws$oral_inc=ifelse(grepl("C00",mws$cancer2_icd4)|grepl("C01",mws$cancer2_icd4)|
                      grepl("C02",mws$cancer2_icd4)|grepl("C03",mws$cancer2_icd4)|
                      grepl("C04",mws$cancer2_icd4)|grepl("C05",mws$cancer2_icd4)|
                      grepl("C06",mws$cancer2_icd4)|grepl("C07",mws$cancer2_icd4)|
                      grepl("C08",mws$cancer2_icd4)|grepl("C09",mws$cancer2_icd4)|
                      grepl("C10",mws$cancer2_icd4)|grepl("C11",mws$cancer2_icd4)|
                      grepl("C12",mws$cancer2_icd4)|grepl("C13",mws$cancer2_icd4)|
                      grepl("C14",mws$cancer2_icd4),1,0)
mws$Larynx_inc=ifelse(grepl("C32",mws$cancer2_icd4),1,0)
mws$liver_inc=ifelse(grepl("C22",mws$cancer2_icd4),1,0)
mws$bladder_inc=ifelse(grepl("C67",mws$cancer2_icd4),1,0)
mws$stomach_inc=ifelse(grepl("C16",mws$cancer2_icd4),1,0)
mws$lung_inc=ifelse(grepl("C34",mws$cancer2_icd4),1,0)
mws$nhl_inc=ifelse(grepl("C82",mws$cancer2_icd4)|
                     grepl("C83",mws$cancer2_icd4)|
                     grepl("C84",mws$cancer2_icd4)|
                     grepl("C85",mws$cancer2_icd4),1,0)
mws$leuk_inc=ifelse(grepl("C91",mws$cancer2_icd4)|
                      grepl("C92",mws$cancer2_icd4)|
                      grepl("C93",mws$cancer2_icd4)|
                      grepl("C94",mws$cancer2_icd4)|
                      grepl("C95",mws$cancer2_icd4),1,0)
mws$breast_inc=ifelse(grepl("C50",mws$cancer2_icd4),1,0)
mws$cervix_inc=ifelse(grepl("C53",mws$cancer2_icd4),1,0)
mws$endometrial_inc=ifelse(grepl("C541",mws$cancer2_icd4),1,0)
mws$ovary_inc=ifelse(grepl("C56",mws$cancer2_icd4),1,0)
mws$gallbladder_inc=ifelse(grepl("C23",mws$cancer2_icd4),1,0)
mws$melanoma_inc=ifelse(grepl("C43",mws$cancer2_icd4),1,0)
mws$brain_inc=ifelse(grepl("C71",mws$cancer2_icd4),1,0)

mws$thyroid_inc=ifelse(grepl("C73",mws$cancer2_icd4),1,0)
mws$multmyeloma_inc=ifelse(grepl("C90",mws$cancer2_icd4),1,0)
mws$oesophadeno_inc=ifelse(mws$oesoph_inc==1&mws$morph %in% c(8140,8144,8145,8480,8481,8490),1,0)
mws$oesophsquamous_inc=ifelse(mws$oesoph_inc==1&mws$morph %in% c(8070,8071,8072,8074),1,0)

mws$cardstom_inc=ifelse(grepl("C160",mws$cancer2_icd4),1,0)
mws$ncardstom_inc=ifelse(grepl("C161",mws$cancer2_icd4)|grepl("C162",mws$cancer2_icd4)|
                           grepl("C163",mws$cancer2_icd4)|grepl("C164",mws$cancer2_icd4)|
                           grepl("C165",mws$cancer2_icd4)|grepl("C166",mws$cancer2_icd4),1,0)

mws$colo_inc=ifelse(grepl("C18",mws$cancer2_icd4),1,0)
mws$rect_inc=ifelse(grepl("C19",mws$cancer2_icd4)|grepl("C20",mws$cancer2_icd4),1,0)
mws$ill_inc=ifelse(grepl("C76",mws$cancer2_icd4)|grepl("C77",mws$cancer2_icd4)|
                     grepl("C78",mws$cancer2_icd4)|grepl("C79",mws$cancer2_icd4)|
                     grepl("C80",mws$cancer2_icd4)|grepl("C97",mws$cancer2_icd4),1,0)

mws$lsqcc_inc=ifelse(mws$lung_inc==1&mws$morph %in% c(8070,8071,8072),1,0)
mws$lsmcc_inc=ifelse(mws$lung_inc==1&mws$morph %in% c(8041:8042),1,0)
mws$llcc_inc=ifelse(mws$lung_inc==1&mws$morph %in% c(8012),1,0)
mws$lac_inc=ifelse(mws$lung_inc==1&mws$morph %in% c(8140,8211,8250:8260,8310,8323,8480:8490),1,0)

###Time points for follow up
mws$age_at_study_date=as.numeric((as.POSIXct(mws$r_date_sign)-as.POSIXct(mws$r_date_birth)))/365.25
mws$age_at_censor=as.numeric((as.POSIXct(mws$cancer2_fudate)-as.POSIXct(mws$r_date_birth)))/365.25
mws$dob_year=as.numeric(format(as.POSIXct(mws$r_date_birth),format="%Y"))
mws$birth_cohort=cut(mws$dob_year,breaks=c(seq(1907,1953,5),1953),right=FALSE)
mws$duration <- as.numeric((as.numeric(as.POSIXct(mws$cancer2_fudate))-as.numeric(as.POSIXct(mws$r_date_sign)))/(365.25*24*60*60))

#Self-reported diabetes:
mws$has_diabetes=ifelse(mws$r_diabetes==1,1,0)
c=as.character(mws$hes_icd10_e11)
c[c==c[1]]=NA
mws$hes_T2DM=c

####Baseline variables####
#socio-economics
mws$r_region=as.factor(mws$r_region)
mws$r_dep_quintile[is.na(mws$r_dep_quintile)]="unknown"
mws$r_dep_quintile=as.factor(mws$r_dep_quintile)
mws$f1_ethnic[is.na(mws$f1_ethnic)]="unknown"
mws$f1_ethnic=as.factor(mws$f1_ethnic)
mws$edu=mws$r_educational_attainment
mws$edu[is.na(mws$edu)]="unknown"
mws$edu=as.factor(mws$edu)
#removal of uterus or ovary
mws$r_hyster_yn[is.na(mws$r_hyster_yn)]="unknown"
mws$r_bilat_yn[is.na(mws$r_bilat_yn)]="unknown"
#smoking
mws$r_smoke[is.na(mws$r_smoke)]="unknown"
mws$r_smoke[mws$r_cigs_per_day %in% c(2,3,4)]="<15"
mws$r_smoke[mws$r_cigs_per_day %in% c(5,6,7)]=">15"
mws$r_smoke=as.factor(mws$r_smoke)

#alcohol
mws$alc=as.character(cut(mws$r_units_alcohol,breaks=c(0,0.5,1,7,14,100),right=FALSE))
mws$alc[is.na(mws$alc)]="unknown"
mws$alc=as.factor(mws$alc)

#PA
mws$r_exercise[is.na(mws$r_exercise)]="unknown"
mws$r_exercise[mws$r_exercise=="-1"]="unknown"
mws$r_exercise=as.factor(mws$r_exercise)
mws$r_stren_exerc[is.na(mws$r_stren_exerc)]="unknown"
mws$r_stren_exerc[mws$r_stren_exerc=="-1"]="unknown"
mws$r_stren_exerc=as.factor(mws$r_stren_exerc)

#female reproductive factors
mws$r_hrtnpc[is.na(mws$r_hrtnpc)]="unknown"
mws$r_hrtnpc=as.factor(mws$r_hrtnpc)

mws$r_oc_yn[is.na(mws$r_oc_yn)]="unknown"
mws$r_oc_yn=as.factor(mws$r_oc_yn)

mws$r_menop_status=as.factor(mws$r_menop_status)
mws$age_menarche_cat=as.character(cut(as.numeric(mws$r_age_menarche),quantile(mws$r_age_menarche,probs=seq(0, 1, 1/6),na.rm=TRUE),right=FALSE))#quartiles of age at menarche
mws$age_menarche_cat[is.na(mws$age_menarche_cat)]="unknown"
mws$age_menarche_cat=as.factor(mws$age_menarche_cat)

dat_cancer_mws=mws[mws$duration>0&!is.na(mws$has_diabetes),]

dat_cancer_mws$r_num_children[is.na(dat_cancer_mws$r_num_children)]=-1
dat_cancer_mws$r_num_children[as.numeric(dat_cancer_mws$r_num_children)>=5]=5
dat_cancer_mws$r_num_children[dat_cancer_mws$r_num_children==1|dat_cancer_mws$r_num_children==2]="1-2"
dat_cancer_mws$r_num_children[dat_cancer_mws$r_num_children==3|dat_cancer_mws$r_num_children==4]="3-4"

dat_cancer_mws$r_age_fb[dat_cancer_mws$r_num_children==0]=NA
dat_cancer_mws$birthage=cut(dat_cancer_mws$r_age_fb,quantile(dat_cancer_mws$r_age_fb[dat_cancer_mws$r_num_children!="0"&dat_cancer_mws$r_num_children!="-1"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
dat_cancer_mws$birthage[is.na(dat_cancer_mws$birthage)]="unknown"
dat_cancer_mws$birthage[dat_cancer_mws$r_num_children=="-1"|dat_cancer_mws$r_num_children=="0"]="unknown"
dat_cancer_mws$r_num_children=as.factor(as.character(dat_cancer_mws$r_num_children))
dat_cancer_mws$birthage=as.factor(as.character(dat_cancer_mws$birthage))

dat_cancer_mws$r_menop_status[dat_cancer_mws$r_menop_status==6|dat_cancer_mws$r_menop_status==7]=6
dat_cancer_mws$age_meno=cut(as.numeric(dat_cancer_mws$r_est_age_menop),quantile(as.numeric(dat_cancer_mws$r_est_age_menop),probs=seq(0,1,1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
dat_cancer_mws$age_meno[is.na(dat_cancer_mws$age_meno)]=99
dat_cancer_mws$r_menop_status=as.factor(as.character(dat_cancer_mws$r_menop_status))
dat_cancer_mws$age_meno=as.factor(as.character(dat_cancer_mws$age_meno))

#hypertension and BPs
dat_cancer_mws$r_bp_in_preg[is.na(dat_cancer_mws$r_bp_in_preg)]=99
dat_cancer_mws$r_bp_not_preg[is.na(dat_cancer_mws$r_bp_not_preg)]=99
dat_cancer_mws$r_treat_bp[is.na(dat_cancer_mws$r_treat_bp)]=99
dat_cancer_mws$r_fam_hist_bc[is.na(dat_cancer_mws$r_fam_hist_bc)]=99

dat_cancer_mws$r_bp_in_preg[is.na(dat_cancer_mws$r_bp_in_preg)]=99
dat_cancer_mws$r_bp_not_preg[is.na(dat_cancer_mws$r_bp_not_preg)]=99
dat_cancer_mws$r_treat_bp[is.na(dat_cancer_mws$r_treat_bp)]=99

dat_cancer_mws$r_hyper=ifelse(dat_cancer_mws$r_bp_in_preg==1|dat_cancer_mws$r_bp_not_preg==1,1,0)
dat_cancer_mws$r_hyper[dat_cancer_mws$r_treat_bp==1]=1
dat_cancer_mws$r_hyper=as.factor(dat_cancer_mws$r_hyper)

save_dat_cancer_mws=dat_cancer_mws
i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
MWS_primary_adj_fully=data.frame()
result_summary_7=data.frame()

for(i in c(316:334,336:344) ){
#dataset for certain cancer:
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  #breast cancer: remove those censored <50 age
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #further Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper+r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  #event numbers:
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper","r_treat_bp",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper","r_treat_bp",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("Fully adjusted")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}
write.csv(MWS_primary_adj_fully,"M:/Nov/MWS_all.csv")

####IV. CKB subgroup and sensitivity analyses####
#1. byage at risk
dat_cancer=save_dat_cancer


i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
CKB_allsex=data.frame()

for(i in c(1:5,8:14,16:18,26:28)){
  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  test_data=test_data[test_data$age_at_study_date<65,]
  test_data$endpoint[test_data$age_at_censor>=65]=0
  test_data$age_at_censor[test_data$age_at_censor>=65]=65-0.00000001
  test_data$duration=test_data$age_at_censor-test_data$age_at_study_date
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                "region_code","study_date","diabetes_diag",
                                                                                                                "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                  "region_code","study_date","diabetes_diag",
                                                                                                                  "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                  "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("<65")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("<65")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("<65")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
dat_cancer=save_dat_cancer


i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()


for(i in c(1:5,8:14,16:18,26:28)){
  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  test_data=test_data[test_data$age_at_study_date<75&test_data$age_at_censor>=65,]      
  test_data$age_at_study_date[test_data$age_at_study_date<65]=65      
  test_data$endpoint[test_data$age_at_censor>=75]=0      
  test_data$age_at_censor[test_data$age_at_censor>=75]=75-0.000000001      
  test_data$duration=test_data$age_at_censor-test_data$age_at_study_date
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                "region_code","study_date","diabetes_diag",
                                                                                                                "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                  "region_code","study_date","diabetes_diag",
                                                                                                                  "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                  "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("65-75")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("65-75")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("65-75")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
dat_cancer=save_dat_cancer


i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()


for(i in c(1:5,8:14,16:18,26:28)){
  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  test_data=test_data[test_data$age_at_censor>=75,]
  test_data$age_at_study_date[test_data$age_at_study_date<75]=75
  test_data$duration=test_data$age_at_censor-test_data$age_at_study_date
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                "region_code","study_date","diabetes_diag",
                                                                                                                "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                  "region_code","study_date","diabetes_diag",
                                                                                                                  "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                  "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c(">=75")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c(">=75")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c(">=75")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
write.csv(CKB_allsex,  "M:/Nov/CKB_byage_3.csv")

#2.by bmi
dat_cancer=save_dat_cancer
dat_cancer=dat_cancer[dat_cancer$bmi_calc<25&!is.na(dat_cancer$bmi_calc),]

i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
CKB_allsex=data.frame()

for(i in c(1:5,8:14,16:18,26:28)){

  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("<25")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("<25")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("<25")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

dat_cancer=save_dat_cancer
dat_cancer=dat_cancer[dat_cancer$bmi_calc<30&dat_cancer$bmi_calc>=25&!is.na(dat_cancer$bmi_calc),]

i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()


for(i in c(1:5,8:14,16:18,26:28)){
  #for(i in c(5)){ 
  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("25-30")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("25-30")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("25-30")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
dat_cancer=save_dat_cancer
dat_cancer=dat_cancer[dat_cancer$bmi_calc>=30&!is.na(dat_cancer$bmi_calc),]

i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()


for(i in c(1:5,8:14,16:18,26:28)){

  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c(">=30")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c(">=30")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c(">=30")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

write.csv(CKB_allsex,  "M:/Nov/CKB_bybmi.csv")






#3.by alc:
dat_cancer=save_dat_cancer

dat_cancer=dat_cancer[dat_cancer$alcohol_category=="1-2"|dat_cancer$alcohol_category==">2 units"|
                        dat_cancer$alcohol_category=="<1 unit"|dat_cancer$alcohol_category=="4",]
i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
CKB_allsex=data.frame()

for(i in c(1:5,8:14,16:18,26:28)){
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("Regular drinker")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("Regular drinker")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("Regular drinker")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

dat_cancer=save_dat_cancer
dat_cancer=dat_cancer[dat_cancer$alcohol_category=="1"|dat_cancer$alcohol_category=="2"|
                        dat_cancer$alcohol_category=="3"|dat_cancer$alcohol_category=="5",]


i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()


for(i in c(1:5,8:14,16:18,26:28)){
  
  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("No regular alcohol")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("No regular alcohol")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("No regular alcohol")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

write.csv(CKB_allsex,  "M:/Nov/CKB_byalc.csv")


#4.by smk:
dat_cancer=save_dat_cancer
dat_cancer=dat_cancer[dat_cancer$smoking_category=="current <15"|dat_cancer$smoking_category=="current >=15"|dat_cancer$smoking_category==3,]

i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
CKB_allsex=data.frame()

for(i in c(1:5,8:14,16:18,26:28)){
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("Current smoker")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("Current smoker")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("Current smoker")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

dat_cancer=save_dat_cancer
dat_cancer=dat_cancer[dat_cancer$smoking_category==1|dat_cancer$smoking_category==2,]



i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
for(i in c(1:5,8:14,16:18,26:28)){
  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("Never smoke")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("Never smoke")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("Never smoke")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
write.csv(CKB_allsex,  "M:/Nov/CKB_bysmk_new.csv")


dat_cancer=save_dat_cancer
dat_cancer=dat_cancer[dat_cancer$smoking_category==3,]

i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
CKB_allsex=data.frame()

for(i in c(1:5,8:14,16:18,26:28)){
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy!=1&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed!=1&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("Current smoker")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("Current smoker")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("Past smoker")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
write.csv(CKB_allsex,  "M:/Nov/CKB_bysmk_past.csv")



#5. by duration since diabetes diagnosis, and analyses for screen-detected diabetes:

dat_cancer=save_dat_cancer[save_dat_cancer$diabetes_scre==0,]
dat_cancer$dm_duration=dat_cancer$age_at_study_date-dat_cancer$diabetes_diag_age
dat_cancer$dm_03=ifelse(dat_cancer$has_diabetes==1&dat_cancer$dm_duration<3&dat_cancer$dm_duration>=0,1,0)
dat_cancer$dm_36=ifelse(dat_cancer$has_diabetes==1&dat_cancer$dm_duration>=3&dat_cancer$dm_duration<6,1,0)
dat_cancer$dm_6=ifelse(dat_cancer$has_diabetes==1&dat_cancer$dm_duration>=6,1,0)

dat_cancer$dm_03[is.na(dat_cancer$dm_03)]=0
dat_cancer$dm_36[is.na(dat_cancer$dm_36)]=0
dat_cancer$dm_6[is.na(dat_cancer$dm_6)]=0

dat_cancer=dat_cancer[dat_cancer$dm_36!=1&dat_cancer$dm_6!=1,]



i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
CKB_allsex=data.frame()

for(i in c(1:5,8:14,16:18,26:28)){
  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                "region_code","study_date","diabetes_diag",
                                                                                                                "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                  "region_code","study_date","diabetes_diag",
                                                                                                                  "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                  "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("<3")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("<3")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("<3")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}



dat_cancer=save_dat_cancer[save_dat_cancer$diabetes_scre==0,]
dat_cancer$dm_duration=dat_cancer$age_at_study_date-dat_cancer$diabetes_diag_age
dat_cancer$dm_03=ifelse(dat_cancer$has_diabetes==1&dat_cancer$dm_duration<3&dat_cancer$dm_duration>=0,1,0)
dat_cancer$dm_36=ifelse(dat_cancer$has_diabetes==1&dat_cancer$dm_duration>=3&dat_cancer$dm_duration<6,1,0)
dat_cancer$dm_6=ifelse(dat_cancer$has_diabetes==1&dat_cancer$dm_duration>=6,1,0)

dat_cancer$dm_03[is.na(dat_cancer$dm_03)]=0
dat_cancer$dm_36[is.na(dat_cancer$dm_36)]=0
dat_cancer$dm_6[is.na(dat_cancer$dm_6)]=0

dat_cancer=dat_cancer[dat_cancer$dm_03!=1&dat_cancer$dm_6!=1,]


i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()


for(i in c(1:5,8:14,16:18,26:28)){
  
  
  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                "region_code","study_date","diabetes_diag",
                                                                                                                "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                  "region_code","study_date","diabetes_diag",
                                                                                                                  "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                  "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("3-6")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("3-6")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("3-6")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}



dat_cancer=save_dat_cancer[save_dat_cancer$diabetes_scre==0,]
dat_cancer$dm_duration=dat_cancer$age_at_study_date-dat_cancer$diabetes_diag_age
dat_cancer$dm_03=ifelse(dat_cancer$has_diabetes==1&dat_cancer$dm_duration<3&dat_cancer$dm_duration>=0,1,0)
dat_cancer$dm_36=ifelse(dat_cancer$has_diabetes==1&dat_cancer$dm_duration>=3&dat_cancer$dm_duration<6,1,0)
dat_cancer$dm_6=ifelse(dat_cancer$has_diabetes==1&dat_cancer$dm_duration>=6,1,0)

dat_cancer$dm_03[is.na(dat_cancer$dm_03)]=0
dat_cancer$dm_36[is.na(dat_cancer$dm_36)]=0
dat_cancer$dm_6[is.na(dat_cancer$dm_6)]=0

dat_cancer=dat_cancer[dat_cancer$dm_03!=1&dat_cancer$dm_36!=1,]


i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()


for(i in c(1:5,8:14,16:18,26:28)){
  
  
  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                "region_code","study_date","diabetes_diag",
                                                                                                                "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                  "region_code","study_date","diabetes_diag",
                                                                                                                  "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                  "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("6+")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("6+")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("6+")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
dat_cancer=save_dat_cancer[save_dat_cancer$diabetes_diag==0,]


i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()


for(i in c(1:5,8:14,16:18,26:28)){
  
  
  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("screened")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("screened")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("screened")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

write.csv(CKB_allsex,  "M:/Nov/CKB_bydura_3.csv")






#6.by follow up periods
dat_cancer=save_dat_cancer

i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
CKB_allsex=data.frame()

for(i in c(1:5,8:14,16:18,26:28)){

  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  test_data$had_hysterectomy[is.na(test_data$had_hysterectomy)]=0
  test_data$had_ovary_removed[is.na(test_data$had_ovary_removed)]=0
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  #first 5 years
  
  test_data$age_at_censor[test_data$duration>=1825.75]=test_data$age_at_study_date[test_data$duration>=1825.75]+5
  test_data$endpoint[test_data$duration>=1825.75]=0
  test_data=test_data[test_data$age_at_censor-test_data$age_at_study_date>0,]
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("<5")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("<5")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("<5")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

dat_cancer=save_dat_cancer



i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()


for(i in c(1:5,8:14,16:18,26:28)){
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  test_data$had_hysterectomy[is.na(test_data$had_hysterectomy)]=0
  test_data$had_ovary_removed[is.na(test_data$had_ovary_removed)]=0
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data=test_data[test_data$duration>=1825.75,]
  test_data$age_at_study_date=test_data$age_at_study_date+5
  test_data=test_data[test_data$age_at_censor-test_data$age_at_study_date>0,]
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("5+")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("5+")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("5+")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

write.csv(CKB_allsex,  "M:/Nov/CKB_byfollow.csv")

#7. by region:
dat_cancer=save_dat_cancer
dat_cancer=dat_cancer[dat_cancer$region_is_urban==1,]

i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
CKB_allsex=data.frame()

for(i in c(1:5,8:14,16:18,26:28)){
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("Urban")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("Urban")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("Urban")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

dat_cancer=save_dat_cancer
dat_cancer=dat_cancer[dat_cancer$region_is_urban==0,]



i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()


for(i in c(1:5,8:14,16:18,26:28)){

  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("Rural")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("Rural")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("Rural")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

write.csv(CKB_allsex,  "M:/Nov/CKB_byreg.csv")





#8.self-reported diabetes
dat_cancer=save_dat_cancer[save_dat_cancer$diabetes_scre==0,]

i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()

CKB_allsex=data.frame()

for(i in c(1:5,8:14,16:18,26:28)){
  
  
  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                "region_code","study_date","diabetes_diag",
                                                                                                                "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                  "region_code","study_date","diabetes_diag",
                                                                                                                  "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                  "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("self-reported")
  }else{
    result_summary=NA
  }
  if(i %in% c(14:17)){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag)+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("self-reported")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==18){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
      
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+strata(is_female)+smk+alc+edu+metcat+bmi_calc+waist_mm+weight_25_cat
                      +sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("is_female","age_at_study_date",
                                                                                                                        "region_code","study_date","diabetes_diag",
                                                                                                                        "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                        "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("is_female","age_at_study_date",
                                                                                                                          "region_code","study_date","diabetes_diag",
                                                                                                                          "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                          "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("self-reported")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex=rbind(CKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

write.csv(CKB_allsex,"M:/Nov/CKB_sf_DM.csv")

####V. UKB subgroup and sensitivity analyses####
#bt age at risk:
dat_cancer_UKB=save_dat_cancer_UKB
i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
UKB_allsex=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  #<65 years of age:
  test_data=test_data[test_data$age_at_study_date<65,]
  test_data$endpoint[test_data$age_at_censor>=65]=0
  test_data$age_at_censor[test_data$age_at_censor>=65]=65-0.00000001
  test_data$duration=test_data$age_at_censor-test_data$age_at_study_date
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                "region","age_at_censor","diabetes_diag",
                                                                                                                "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                  "region","age_at_censor","diabetes_diag",
                                                                                                                  "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                  "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("Age<65")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                        "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                          "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("Age<65")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("Age<65")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}


for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  #65-75 years of age:
  test_data=test_data[test_data$age_at_study_date<75&test_data$age_at_censor>=65,]      
  test_data$age_at_study_date[test_data$age_at_study_date<65]=65      
  test_data$endpoint[test_data$age_at_censor>=75]=0      
  test_data$age_at_censor[test_data$age_at_censor>=75]=75-0.000000001      
  test_data$duration=test_data$age_at_censor-test_data$age_at_study_date
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                "region","age_at_censor","diabetes_diag",
                                                                                                                "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                  "region","age_at_censor","diabetes_diag",
                                                                                                                  "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                  "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("65-75")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                        "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                          "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("65-75")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("65-75")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  #>=75 years of age:
  test_data=test_data[test_data$age_at_censor>=75,]
  test_data$age_at_study_date[test_data$age_at_study_date<75]=75
  test_data$duration=test_data$age_at_censor-test_data$age_at_study_date
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                "region","age_at_censor","diabetes_diag",
                                                                                                                "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                  "region","age_at_censor","diabetes_diag",
                                                                                                                  "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                  "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("Age>=75")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                        "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                          "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("Age>=75")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("Age>=75")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

write.csv(UKB_allsex,"M:/Nov/UKB_byage_3.csv")

#by BMI:
dat_cancer_UKB=save_dat_cancer_UKB
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$bmi<25,]
i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
UKB_allsex=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                 
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("BMI<25")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                       
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                         
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("BMI<25")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("BMI<25")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}


dat_cancer_UKB=save_dat_cancer_UKB
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$bmi>=25&dat_cancer_UKB$bmi<30,]
i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                               
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                 
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("BMI 25-30")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                       
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                         
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("BMI 25-30")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("BMI 25-30")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

dat_cancer_UKB=save_dat_cancer_UKB
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$bmi>=30,]
i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                               
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                 
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("BMI>=30")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                       
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                         
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("BMI>=30")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("BMI>=30")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
write.csv(UKB_allsex,"M:/Nov/UKB_bybmi_alcadj.csv")

#by alcohol intake:
dat_cancer_UKB=save_dat_cancer_UKB
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$alcohol_cat=="8-15g/d"|dat_cancer_UKB$alcohol_cat=="16+g/d"|
                                dat_cancer_UKB$alcohol_cat=="1-7g/d"|dat_cancer_UKB$alcohol_cat=="<1g/d"| 
                                dat_cancer_UKB$alcohol_cat=="Weekly,amount unknown"|dat_cancer_UKB$alcohol_cat=="Monthly,amount unknown",]
i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
UKB_allsex=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                               
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                 
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("Regular drinker")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                       
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                         
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("Regular drinker")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("Regular drinker")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}


dat_cancer_UKB=save_dat_cancer_UKB
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$alcohol_cat=="Never"|dat_cancer_UKB$alcohol_cat=="Special occasions only"|dat_cancer_UKB$alcohol_cat=="Previous",]


i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                               
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                 
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("No regular alcohol")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                       
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                         
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("No regular alcohol")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("No regular alcohol")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
write.csv(UKB_allsex,"M:/Nov/UKB_byalc_alcadj_relaxed_mont_as_occ.csv")
#by smoking:
dat_cancer_UKB=save_dat_cancer_UKB
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$smokeG5=="current & unknown no"|dat_cancer_UKB$smokeG5=="current smoker<15 cig/day"
                              |dat_cancer_UKB$smokeG5=="current smoker>=15 cig/day"|dat_cancer_UKB$smokeG5=="former",]
i=NULL
test_data=data.frame()
fit=list()
result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
UKB_allsex=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                               
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                 
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("Current smoker")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                       
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                         
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("Current smoker")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    #regression of model 1 (no bmi adjustments) for any diabetes and self-reported diabetes  
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("Current smoker")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}


dat_cancer_UKB=save_dat_cancer_UKB
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$smokeG5=="non smoker",]

i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                               
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                 
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("Never smoke")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                       
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                         
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("Never smoke")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("Never smoke")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
write.csv(UKB_allsex,"M:/Nov/UKB_bysmk_new.csv")




dat_cancer_UKB=save_dat_cancer_UKB
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$smokeG5=="former",]
i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
UKB_allsex=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                               
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                 
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("Past smoker")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                       
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                         
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("Past smoker")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    #regression of model 1 (no bmi adjustments) for any diabetes and self-reported diabetes  
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("Past smoker")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
write.csv(UKB_allsex,"M:/Nov/UKB_bysmk_past.csv")

#by follow up periods:
dat_cancer_UKB=save_dat_cancer_UKB

i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
UKB_allsex=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  #first 5 years
  test_data=test_data
  test_data$age_at_censor[test_data$duration>=1825.75]=test_data$age_at_study_date[test_data$duration>=1825.75]+5
  test_data$endpoint[test_data$duration>=1825.75]=0
  test_data=test_data[test_data$age_at_censor-test_data$age_at_study_date>0,]
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                               
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                 
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("<5")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                       
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                         
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("<5")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("<5")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}


dat_cancer_UKB=save_dat_cancer_UKB

i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  #5+
  test_data=test_data[test_data$duration>=1825.75,]
  test_data$age_at_study_date=test_data$age_at_study_date+5
  test_data=test_data[test_data$age_at_censor-test_data$age_at_study_date>0,]
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                               
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                 
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("5+")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                       
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                         
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("5+")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("5+")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

write.csv(UKB_allsex,"M:/Nov/UKB_byfollow_alcadj.csv")
#by diabetes duration:
dat_cancer_UKB=save_dat_cancer_UKB[save_dat_cancer_UKB$diabetes_scre==0,]
dat_cancer_UKB$n_2976_0_0[dat_cancer_UKB$n_2976_0_0=="Do not know"|dat_cancer_UKB$n_2976_0_0=="Prefer not to answer"]=NA
dat_cancer_UKB=dat_cancer_UKB[!c(dat_cancer_UKB$has_diabetes==1&is.na(dat_cancer_UKB$n_2976_0_0)),]
dat_cancer_UKB$dm_duration=dat_cancer_UKB$age_at_study_date-as.numeric(dat_cancer_UKB$n_2976_0_0)
dat_cancer_UKB$dm_03=ifelse(dat_cancer_UKB$has_diabetes==1&dat_cancer_UKB$dm_duration<3&dat_cancer_UKB$dm_duration>=0,1,0)
dat_cancer_UKB$dm_36=ifelse(dat_cancer_UKB$has_diabetes==1&dat_cancer_UKB$dm_duration<6&dat_cancer_UKB$dm_duration>=3,1,0)
dat_cancer_UKB$dm_6=ifelse(dat_cancer_UKB$has_diabetes==1&dat_cancer_UKB$dm_duration>=6,1,0)
dat_cancer_UKB$dm_03[is.na(dat_cancer_UKB$dm_03)]=0
dat_cancer_UKB$dm_36[is.na(dat_cancer_UKB$dm_36)]=0
dat_cancer_UKB$dm_6[is.na(dat_cancer_UKB$dm_6)]=0
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$dm_36!=1&dat_cancer_UKB$dm_6!=1,]
i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
UKB_allsex=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                "region","age_at_censor","diabetes_diag",
                                                                                                                "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                
                                                                                                                "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                  "region","age_at_censor","diabetes_diag",
                                                                                                                  "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                  
                                                                                                                  "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("<3")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                        
                                                                                                                        "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                          
                                                                                                                          "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("<3")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("<3")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}


dat_cancer_UKB=save_dat_cancer_UKB[save_dat_cancer_UKB$diabetes_scre==0,]
dat_cancer_UKB$n_2976_0_0[dat_cancer_UKB$n_2976_0_0=="Do not know"|dat_cancer_UKB$n_2976_0_0=="Prefer not to answer"]=NA
dat_cancer_UKB=dat_cancer_UKB[!c(dat_cancer_UKB$has_diabetes==1&is.na(dat_cancer_UKB$n_2976_0_0)),]
dat_cancer_UKB$dm_duration=dat_cancer_UKB$age_at_study_date-as.numeric(dat_cancer_UKB$n_2976_0_0)
dat_cancer_UKB$dm_03=ifelse(dat_cancer_UKB$has_diabetes==1&dat_cancer_UKB$dm_duration<3&dat_cancer_UKB$dm_duration>=0,1,0)
dat_cancer_UKB$dm_36=ifelse(dat_cancer_UKB$has_diabetes==1&dat_cancer_UKB$dm_duration<6&dat_cancer_UKB$dm_duration>=3,1,0)
dat_cancer_UKB$dm_6=ifelse(dat_cancer_UKB$has_diabetes==1&dat_cancer_UKB$dm_duration>=6,1,0)
dat_cancer_UKB$dm_03[is.na(dat_cancer_UKB$dm_03)]=0
dat_cancer_UKB$dm_36[is.na(dat_cancer_UKB$dm_36)]=0
dat_cancer_UKB$dm_6[is.na(dat_cancer_UKB$dm_6)]=0
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$dm_03!=1&dat_cancer_UKB$dm_6!=1,]


i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                "region","age_at_censor","diabetes_diag",
                                                                                                                "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                
                                                                                                                "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                  "region","age_at_censor","diabetes_diag",
                                                                                                                  "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                  
                                                                                                                  "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("3-6")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                        
                                                                                                                        "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                          
                                                                                                                          "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("3-6")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("3-6")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}


dat_cancer_UKB=save_dat_cancer_UKB[save_dat_cancer_UKB$diabetes_scre==0,]
dat_cancer_UKB$n_2976_0_0[dat_cancer_UKB$n_2976_0_0=="Do not know"|dat_cancer_UKB$n_2976_0_0=="Prefer not to answer"]=NA
dat_cancer_UKB=dat_cancer_UKB[!c(dat_cancer_UKB$has_diabetes==1&is.na(dat_cancer_UKB$n_2976_0_0)),]
dat_cancer_UKB$dm_duration=dat_cancer_UKB$age_at_study_date-as.numeric(dat_cancer_UKB$n_2976_0_0)
dat_cancer_UKB$dm_03=ifelse(dat_cancer_UKB$has_diabetes==1&dat_cancer_UKB$dm_duration<3&dat_cancer_UKB$dm_duration>=0,1,0)
dat_cancer_UKB$dm_36=ifelse(dat_cancer_UKB$has_diabetes==1&dat_cancer_UKB$dm_duration<6&dat_cancer_UKB$dm_duration>=3,1,0)
dat_cancer_UKB$dm_6=ifelse(dat_cancer_UKB$has_diabetes==1&dat_cancer_UKB$dm_duration>=6,1,0)
dat_cancer_UKB$dm_03[is.na(dat_cancer_UKB$dm_03)]=0
dat_cancer_UKB$dm_36[is.na(dat_cancer_UKB$dm_36)]=0
dat_cancer_UKB$dm_6[is.na(dat_cancer_UKB$dm_6)]=0
dat_cancer_UKB=dat_cancer_UKB[dat_cancer_UKB$dm_03!=1&dat_cancer_UKB$dm_36!=1,]


i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                "region","age_at_censor","diabetes_diag",
                                                                                                                "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                
                                                                                                                "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                  "region","age_at_censor","diabetes_diag",
                                                                                                                  "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                  
                                                                                                                  "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c(">=6")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                        
                                                                                                                        "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                          
                                                                                                                          "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c(">=6")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c(">=6")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
dat_cancer_UKB=save_dat_cancer_UKB[save_dat_cancer_UKB$diabetes_diag==0,]
i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                               
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                 
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("screened")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                       
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                         
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("screened")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("screened")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}

write.csv(UKB_allsex,"M:/Nov/UKB_bydura_3.csv")

#sefl-reported diabetes:
dat_cancer_UKB=save_dat_cancer_UKB[save_dat_cancer_UKB$diabetes_scre==0,]

i=NULL
test_data=data.frame()
fit=list()


result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_any=data.frame()
result_summary_diag=data.frame()
UKB_allsex=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  
  
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary=data.frame(rbind(result_summary_any))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                "region","age_at_censor","diabetes_diag",
                                                                                                                "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                
                                                                                                                "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                  "region","age_at_censor","diabetes_diag",
                                                                                                                  "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                  
                                                                                                                  "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("Self-reported")
  }else{
    result_summary=NA
  }
  if(i %in% c(16:19)){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    result_summary_w=data.frame(rbind(result_summary_any))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                        
                                                                                                                        "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                          
                                                                                                                          "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("Self-reported")
    
    
  }else{
    result_summary_w=NA
  }
  if(i==20){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~diabetes_diag+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk+alc+metcat+bmi+waistcircumf_0_0+bodysizeage10_0_0
                      +HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_any=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                      " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                      "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                         
                         
    )
    
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_any))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==1,c("sex","age_at_study_date",
                                                                                                                        "region","age_at_censor","diabetes_diag",
                                                                                                                        "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$diabetes_diag==0,c("sex","age_at_study_date",
                                                                                                                          "region","age_at_censor","diabetes_diag",
                                                                                                                          "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2"
    )])))
    ) 
    
    
    result_summary_m$model=c("Self-reported")
    
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  UKB_allsex=rbind(UKB_allsex,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
write.csv(UKB_allsex,"M:/Nov/UKB_sf_DM_alcadj.csv")
####VI. MWS subgroup and sensitivity analyses####
#by age at risk:

dat_cancer_mws=save_mws
i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
MWS_primary_adj_fully=data.frame()

result_summary=data.frame()
result_summary_7=data.frame()

for(i in c(316:334,336:344)){
  
  
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  test_data=test_data[test_data$age_at_study_date<65,]
  test_data$endpoint[test_data$age_at_censor>=65]=0
  test_data$age_at_censor[test_data$age_at_censor>=65]=65-0.00000001
  test_data$duration=test_data$age_at_censor-test_data$age_at_study_date
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("<65")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}
dat_cancer_mws=save_mws
i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()


result_summary=data.frame()
result_summary_7=data.frame()


for(i in c(316:334,336:344) ){
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  
  #65-75 years of age:
  test_data=test_data[test_data$age_at_study_date<75&test_data$age_at_censor>=65,]      
  test_data$age_at_study_date[test_data$age_at_study_date<65]=65      
  test_data$endpoint[test_data$age_at_censor>=75]=0      
  test_data$age_at_censor[test_data$age_at_censor>=75]=75-0.000000001      
  test_data$duration=test_data$age_at_censor-test_data$age_at_study_date
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("65-75")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}
dat_cancer_mws=save_mws
i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()

result_summary_7=data.frame()

for(i in c(316:334,336:344) ){
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  test_data=test_data[test_data$age_at_censor>=75,]
  test_data$age_at_study_date[test_data$age_at_study_date<75]=75
  test_data$duration=test_data$age_at_censor-test_data$age_at_study_date
  
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c(">=75")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}

write.csv(MWS_primary_adj_fully,"M:/Nov/MWS_byage_3.csv")


#by bmi:

dat_cancer_mws=save_mws
dat_cancer_mws=dat_cancer_mws[dat_cancer_mws$r_bmi<25,]
i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
MWS_primary_adj_fully=data.frame()

result_summary=data.frame()
result_summary_7=data.frame()


for(i in c(316:334,336:344) ){
  
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("<25")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}
dat_cancer_mws=save_mws
dat_cancer_mws=dat_cancer_mws[dat_cancer_mws$r_bmi<30&dat_cancer_mws$r_bmi>=25,]
i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
result_summary_7=data.frame()

for(i in c(316:334,336:344) ){
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("25-30")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}
dat_cancer_mws=save_mws
dat_cancer_mws=dat_cancer_mws[dat_cancer_mws$r_bmi>=30,]
i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
result_summary_7=data.frame()


for(i in c(316:334,336:344) ){
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  
  
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("30+")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}

write.csv(MWS_primary_adj_fully,"M:/Nov/MWS_bybmi.csv")




#by alcohol:
dat_cancer_mws=save_mws
dat_cancer_mws=dat_cancer_mws[dat_cancer_mws$r_units_alcohol>0,]

i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
MWS_primary_adj_fully=data.frame()

result_summary_7=data.frame()


for(i in c(316:334,336:344) ){
  
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("Regular drinker")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}
dat_cancer_mws=save_mws
dat_cancer_mws=dat_cancer_mws[dat_cancer_mws$r_units_alcohol==0,]

i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
result_summary_7=data.frame()


for(i in c(316:334,336:344) ){
  
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("No regular alcohol")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}

write.csv(MWS_primary_adj_fully,"M:/Nov/MWS_byalc.csv")

#by smoking

dat_cancer_mws=save_mws
dat_cancer_mws=dat_cancer_mws[dat_cancer_mws$r_smoke==1,]
i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
MWS_primary_adj_fully=data.frame()

result_summary_7=data.frame()

for(i in c(316:334,336:344) ){
  
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("Never smoke")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}
dat_cancer_mws=save_mws
dat_cancer_mws=dat_cancer_mws[dat_cancer_mws$r_smoke=="<15"|dat_cancer_mws$r_smoke==">15"|dat_cancer_mws$r_smoke=="2",]
i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()

result_summary_7=data.frame()

for(i in c(316:334,336:344) ){
  
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("Current smoker")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}

write.csv(MWS_primary_adj_fully,"M:/Nov/MWS_bysmk_new.csv")

dat_cancer_mws=save_mws
dat_cancer_mws=dat_cancer_mws[dat_cancer_mws$r_smoke=="2",]

i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
MWS_bysmk=data.frame()
result_summary_7=data.frame()


for(i in c(316:334,336:344) ){
  
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("Past smoker")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_bysmk=rbind(MWS_bysmk,data.frame(result_summary))
  result_summary=data.frame()
  
  
}

dat_cancer_mws=save_mws
dat_cancer_mws=dat_cancer_mws[dat_cancer_mws$r_smoke=="<15"|dat_cancer_mws$r_smoke==">15"|dat_cancer_mws$r_smoke=="2",]

i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
result_summary_7=data.frame()

for(i in c(316:334,336:344) ){
  
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("Ever regular")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_bysmk=rbind(MWS_bysmk,data.frame(result_summary))
  result_summary=data.frame()
  
  
}

write.csv(MWS_bysmk,"M:/Nov/MWS_bysmk_past_ever.csv")
#by follow up:

dat_cancer_mws=save_mws
i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
MWS_primary_adj_fully=data.frame()
result_summary_7=data.frame()

for(i in c(316:334,336:344) ){
  
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  test_data$age_at_censor[test_data$duration>=5]=test_data$age_at_study_date[test_data$duration>=5]+5
  test_data$endpoint[test_data$duration>=5]=0
  test_data=test_data[test_data$age_at_censor-test_data$age_at_study_date>0,]
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("<5")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}
dat_cancer_mws=save_mws
i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
result_summary_7=data.frame()

for(i in c(316:334,336:344) ){
  
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  test_data=test_data[test_data$duration>=5,]
  test_data$age_at_study_date=test_data$age_at_study_date+5
  test_data=test_data[test_data$age_at_censor-test_data$age_at_study_date>0,]
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu
                    +r_smoke+alc+r_exercise+r_stren_exerc+r_bmi+r_hyper
                    +r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_7=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  result_summary=data.frame(rbind(result_summary_7))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("5+")
  
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj_fully=rbind(MWS_primary_adj_fully,data.frame(result_summary))
  result_summary=data.frame()
  
  
}

write.csv(MWS_primary_adj_fully,"M:/Nov/MWS_byfollow.csv")

####VII. CKB analyses with adjustments for each covariates individually####
dat_cancer=save_dat_cancer

i=NULL
test_data_1=data.frame()
test_data_2=data.frame()
test_data=data.frame()
fit=list()

result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_1=data.frame()
result_summary_2a=data.frame()
result_summary_2b=data.frame()
result_summary_2c=data.frame()
result_summary_3a=data.frame()
result_summary_3b=data.frame()
result_summary_3c=data.frame()
result_summary_3=data.frame()
result_summary_4a=data.frame()
result_summary_4b=data.frame()
result_summary_4=data.frame()
result_summary_5=data.frame()
result_summary_6=data.frame()

CKB_allsex_adj=data.frame()

for(i in c(1:5,8:14,16:18,26:28)){
  
  
  test_data=dat_cancer[,c(1,2*i,(2*i+1),58:ncol(dat_cancer))]
  names(test_data)[c(2,3)]=c("endpoint","date_censor")
  if(i!=1){
    m=dat_cancer$case_all==1&dat_cancer$timeout_all<test_data$date_censor
    
    test_data$date_censor[m]=dat_cancer$timeout_all[m]
    test_data$endpoint[m]=0
  }
  
  test_data$age_at_censor=as.numeric((as.POSIXct(test_data$date_censor)-as.POSIXct(test_data$dob_anon)))/365.25
  if(i==14){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  test_data$dob_year=as.numeric(format(test_data$dob_anon,format="%Y"))
  test_data$birth_cohort=cut(test_data$dob_year,breaks=c(seq(1925,1980,5)),right=FALSE)
  test_data$duration <- difftime(test_data$date_censor,test_data$study_date,units="days")
  
  #Exclusion criteria:
  
  if (i==16){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_hysterectomy==0&!is.na(test_data$bmi_calc),]
    
  } else if(i==17){
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&test_data$had_ovary_removed==0&!is.na(test_data$bmi_calc),]
    
  } else {
    test_data=test_data[test_data$duration>0&test_data$cancer_diag==0&!is.na(test_data$bmi_calc),]
  }
  test_data=test_data[complete.cases(test_data[,c("is_female","age_at_study_date","region_code","study_date","has_diabetes",
                                                  "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm",
                                                  "sbp_mean","dbp_mean","hypertension_diag","pill","meno","live_birth_count")]),]
  
  
  test_data_1=test_data[test_data$is_female==1,]
  test_data_2=test_data[test_data$is_female==0,]
  
  if(!(i %in% c(14:18))){
    test_data$smk=as.factor(test_data$smoking_category)
    test_data$alc=as.factor(test_data$alcohol_category)
    test_data$edu=as.factor(test_data$highest_education)
    test_data$region=as.factor(test_data$region_code)
    test_data$metcat=as.factor(cut(test_data$met,quantile(test_data$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$weight_25_cat=cut(test_data$age_25_weight_jin,quantile(test_data$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$weight_25_cat[is.na(test_data$weight_25_cat)]="unknown"
    test_data$weight_25_cat=as.factor(test_data$weight_25_cat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$live_birth_age01[test_data$nullpar==0&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$menopause_age=cut(test_data$menopause_age,quantile(test_data$menopause_age[test_data$meno==2&test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$menopause_age[is.na(test_data$menopause_age)]="not applicable"
    test_data$menopause_age=as.factor(test_data$menopause_age)
    test_data$meno[is.na(test_data$meno)]="not applicable"
    
    test_data$age_menarche=cut(test_data$age_menarche,quantile(test_data$age_menarche[test_data$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+edu,data=test_data))
    
    result_summary_1=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+edu+smk,data=test_data))
    
    result_summary_2a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+edu+alc,data=test_data))
    
    result_summary_2b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+edu+metcat,data=test_data))
    
    result_summary_2c=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+edu+bmi_calc,data=test_data))
    
    result_summary_3a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+edu+waist_mm,data=test_data))
    
    result_summary_3b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+edu+weight_25_cat,data=test_data))
    
    result_summary_3c=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+edu+bmi_calc+waist_mm+weight_25_cat,data=test_data))
    
    result_summary_3=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+edu+as.factor(hypertension_diag),data=test_data))
    
    result_summary_4a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+edu+sbp_mean+dbp_mean,data=test_data))
    
    result_summary_4b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+edu+sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data))
    
    result_summary_4=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+strata(is_female)+edu+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data))
    
    result_summary_6=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    
    result_summary=data.frame(rbind(result_summary_1,result_summary_2a,result_summary_2b,result_summary_2c,
                                    result_summary_3a,result_summary_3b,result_summary_3c,result_summary_3,
                                    result_summary_4a,result_summary_4b,result_summary_4,result_summary_6))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                               "region_code","study_date","has_diabetes",
                                                                                                               "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                               "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                 "region_code","study_date","has_diabetes",
                                                                                                                 "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                 "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary$model=c("Basic","+ Smoking","+ Alcohol","+ Physical activity",
                           "+ BMI","+ Waist circumference","+ Body size at age 10","+ all Adiposity traits",
                           "+ Hypertension","+ Blood pressures","+ Hypertension and BP","+ Female reproductive factors")
    result_summary$sex="all"
  }else{
    result_summary=NA
  }
  if(i!=18){
    test_data_1$smk=as.factor(test_data_1$smoking_category)
    test_data_1$alc=as.factor(test_data_1$alcohol_category)
    test_data_1$edu=as.factor(test_data_1$highest_education)
    test_data_1$region=as.factor(test_data_1$region_code)
    test_data_1$metcat=as.factor(cut(test_data_1$met,quantile(test_data_1$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$weight_25_cat=cut(test_data_1$age_25_weight_jin,quantile(test_data_1$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$weight_25_cat[is.na(test_data_1$weight_25_cat)]="unknown"
    test_data_1$weight_25_cat=as.factor(test_data_1$weight_25_cat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$live_birth_age01[test_data_1$nullpar==0&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$menopause_age=cut(test_data_1$menopause_age,quantile(test_data_1$menopause_age[test_data_1$meno==2&test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$menopause_age[is.na(test_data_1$menopause_age)]="not applicable"
    test_data_1$menopause_age=as.factor(test_data_1$menopause_age)
    test_data_1$meno[is.na(test_data_1$meno)]="not applicable"
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,quantile(test_data_1$age_menarche[test_data_1$is_female==1],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu,data=test_data_1))
    
    result_summary_1=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+smk,data=test_data_1))
    
    result_summary_2a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+alc,data=test_data_1))
    
    result_summary_2b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+metcat,data=test_data_1))
    
    result_summary_2c=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+bmi_calc,data=test_data_1))
    
    result_summary_3a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+waist_mm,data=test_data_1))
    
    result_summary_3b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+weight_25_cat,data=test_data_1))
    
    result_summary_3c=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+bmi_calc+waist_mm+weight_25_cat,data=test_data_1))
    
    result_summary_3=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+as.factor(hypertension_diag),data=test_data_1))
    
    result_summary_4a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+sbp_mean+dbp_mean,data=test_data_1))
    
    result_summary_4b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_1))
    
    result_summary_4=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+pill+age_menarche+meno+menopause_age+live_birth_count+birthage,data=test_data_1))
    
    result_summary_6=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_w=data.frame(rbind(result_summary_1,result_summary_2a,result_summary_2b,result_summary_2c,
                                      result_summary_3a,result_summary_3b,result_summary_3c,result_summary_3,
                                      result_summary_4a,result_summary_4b,result_summary_4,result_summary_6))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag","pill","age_menarche","meno","menopause_age","live_birth_count","birthage")])))
    ) 
    
    result_summary_w$model=c("Basic","+ Smoking","+ Alcohol","+ Physical activity",
                             "+ BMI","+ Waist circumference","+ Body size at age 10","+ all Adiposity traits",
                             "+ Hypertension","+ Blood pressures","+ Hypertension and BP","+ Female reproductive factors")
    
    result_summary_w$sex="women"
  }else{
    result_summary_w=NA
  }
  if(!(i %in% c(14:17))){
    test_data_2$smk=as.factor(test_data_2$smoking_category)
    test_data_2$alc=as.factor(test_data_2$alcohol_category)
    test_data_2$edu=as.factor(test_data_2$highest_education)
    test_data_2$region=as.factor(test_data_2$region_code)
    test_data_2$metcat=as.factor(cut(test_data_2$met,quantile(test_data_2$met,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$weight_25_cat=cut(test_data_2$age_25_weight_jin,quantile(test_data_2$age_25_weight_jin,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_2$weight_25_cat[is.na(test_data_2$weight_25_cat)]="unknown"
    test_data_2$weight_25_cat=as.factor(test_data_2$weight_25_cat)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu,data=test_data_2))
    
    result_summary_1=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+smk,data=test_data_2))
    
    result_summary_2a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+alc,data=test_data_2))
    
    result_summary_2b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+metcat,data=test_data_2))
    
    result_summary_2c=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+bmi_calc,data=test_data_2))
    
    result_summary_3a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+waist_mm,data=test_data_2))
    
    result_summary_3b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+weight_25_cat,data=test_data_2))
    
    result_summary_3c=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+bmi_calc+waist_mm+weight_25_cat,data=test_data_2))
    
    result_summary_3=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_4a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+sbp_mean+dbp_mean,data=test_data_2))
    
    result_summary_4b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+edu+sbp_mean+dbp_mean+as.factor(hypertension_diag),data=test_data_2))
    
    result_summary_4=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
    )
    result_summary_6=result_summary_1
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_1,result_summary_2a,result_summary_2b,result_summary_2c,
                                      result_summary_3a,result_summary_3b,result_summary_3c,result_summary_3,
                                      result_summary_4a,result_summary_4b,result_summary_4,result_summary_6))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("is_female","age_at_study_date",
                                                                                                                       "region_code","study_date","has_diabetes",
                                                                                                                       "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                       "sbp_mean","dbp_mean","hypertension_diag")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("is_female","age_at_study_date",
                                                                                                                         "region_code","study_date","has_diabetes",
                                                                                                                         "smoking_category","alcohol_category","highest_education","met","bmi_calc","waist_mm","weight_25_cat",
                                                                                                                         "sbp_mean","dbp_mean","hypertension_diag")])))
    ) 
    result_summary_m$model=c("Basic","+ Smoking","+ Alcohol","+ Physical activity",
                             "+ BMI","+ Waist circumference","+ Body size at age 10","+ all Adiposity traits",
                             "+ Hypertension","+ Blood pressures","+ Hypertension and BP","+ Female reproductive factors")
    result_summary_m$sex="men"
  }else{
    result_summary_m=NA
  }
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  # result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer)[2*i],"_")[[1]][2]
  
  #add rows to the combined data frame of results
  CKB_allsex_adj=rbind(CKB_allsex_adj,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
write.csv(CKB_allsex_adj,"M:/Nov/CKB_adj.csv")

####VIII. UKB analyses with adjustments for each covariates individually####
dat_cancer_UKB=save_dat_cancer_UKB


i=NULL
test_data_1=data.frame()
test_data_2=data.frame()
test_data=data.frame()
fit=list()

result_summary=data.frame()
result_summary_w=data.frame()
result_summary_m=data.frame()
result_summary_1=data.frame()
result_summary_2a=data.frame()
result_summary_2b=data.frame()
result_summary_2c=data.frame()
result_summary_3a=data.frame()
result_summary_3b=data.frame()
result_summary_3c=data.frame()
result_summary_3=data.frame()
result_summary_4a=data.frame()
result_summary_4b=data.frame()
result_summary_4=data.frame()
result_summary_5=data.frame()
result_summary_6=data.frame()

UKB_allsex_adj=data.frame()
for(i in c(3:5,7,10:20,119,120)){
  test_data=dat_cancer_UKB[,c(1,2,i,29:ncol(dat_cancer_UKB))]
  
  names(test_data)[c(1,2,3)]=c("csid","date_censor","endpoint")
  #run this to censor the Welsh participants at the end of 2016:
  test_data$endpoint[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]=0
  test_data$date_censor[test_data$region=="Wales"&test_data$date_censor>"2016-12-31"]="2016-12-31"
  
  if(i==16){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  if (i==18){
    test_data=test_data[test_data$hysterectomy_cat==0,]
  } else if(i==19){
    test_data=test_data[test_data$ovaries=="No",]
  }
  test_data=test_data[complete.cases(test_data[,c("sex","age_at_study_date",
                                                  "region","age_at_censor","has_diabetes",
                                                  "smk","alc","edu","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                  "pill","meno","hrt","live_births_0_0")]),]
  test_data_1=test_data[test_data$sex=="Female",]
  test_data_2=test_data[test_data$sex=="Male",]
  
  if(!(i %in% c(16:20))){
    test_data$metcat=as.character(cut(test_data$n_22040_0_0,
                                      quantile(test_data$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data$metcat[is.na(test_data$metcat)]="unknown"    
    test_data$metcat=as.factor(test_data$metcat)
    
    test_data$birthage=cut(test_data$birthage,quantile(test_data$n_2754_0_0[test_data$nullpar==0&test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$birthage[test_data$nullpar==1]=0
    test_data$birthage[test_data$sex=="Male"]="not applicable"
    test_data$birthage[is.na(test_data$birthage)]="not applicable"
    test_data$birthage=as.factor(test_data$birthage)
    
    test_data$age_meno=cut(as.numeric(test_data$age_meno),quantile(as.numeric(test_data$age_meno)[test_data$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data$age_meno[is.na(test_data$age_meno)]="not applicable"
    test_data$age_meno=as.factor(test_data$age_meno)
    
    test_data$age_menarche=cut(test_data$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data$age_menarche[is.na(test_data$age_menarche)]="not applicable"
    test_data$age_menarche=as.factor(test_data$age_menarche)
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu,data=test_data))
    result_summary_1=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                       
                       
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+smk,data=test_data))
    result_summary_2a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+alc,data=test_data))
    result_summary_2b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+metcat,data=test_data))
    result_summary_2c=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+bmi,data=test_data))
    result_summary_3a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+waistcircumf_0_0,data=test_data))
    result_summary_3b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+bodysizeage10_0_0,data=test_data))
    result_summary_3c=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+bmi+waistcircumf_0_0+bodysizeage10_0_0,data=test_data))
    result_summary_3=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                       
                       
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+HYPER,data=test_data))
    result_summary_4a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+sbp_ave2+dbp_ave2,data=test_data))
    result_summary_4b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+HYPER+sbp_ave2+dbp_ave2,data=test_data))
    result_summary_4=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                       
                       
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(sex)+strata(region)+dep+ethnicity5+edu+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data))
    result_summary_6=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                       
                       
    )
    
    
    result_summary=data.frame(rbind(result_summary_1,result_summary_2a,result_summary_2b,result_summary_2c,
                                    result_summary_3a,result_summary_3b,result_summary_3c,result_summary_3,
                                    result_summary_4a,result_summary_4b,result_summary_4,result_summary_6))
    names(result_summary)=c("coef","se","p","HRCI")
    result_summary$N_exp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                               "region","age_at_censor","has_diabetes",
                                                                                                               "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                               "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary$N_unexp=c(nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                 "region","age_at_censor","has_diabetes",
                                                                                                                 "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                 "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    result_summary$model=c("Basic","+ Smoking","+ Alcohol","+ Physical activity",
                           "+ BMI","+ Waist circumference","+ Body size at age 10","+ all Adiposity traits",
                           "Hypertension","Blood pressures","+ Hypertension and BP","+ Female reproductive factors")
    result_summary$sex="all"
  }else{
    result_summary=NA
  }
  if(i!=20){
    test_data_1$metcat=as.character(cut(test_data_1$n_22040_0_0,
                                        quantile(test_data_1$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_1$metcat[is.na(test_data_1$metcat)]="unknown"    
    test_data_1$metcat=as.factor(test_data_1$metcat)
    
    test_data_1$birthage=cut(test_data_1$birthage,quantile(test_data_1$n_2754_0_0[test_data_1$nullpar==0&test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$birthage[test_data_1$nullpar==1]=0
    test_data_1$birthage[test_data_1$sex=="Male"]="not applicable"
    test_data_1$birthage[is.na(test_data_1$birthage)]="not applicable"
    test_data_1$birthage=as.factor(test_data_1$birthage)
    
    test_data_1$age_meno=cut(as.numeric(test_data_1$age_meno),quantile(as.numeric(test_data_1$age_meno)[test_data_1$sex=="Female"],probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_meno[is.na(test_data_1$age_meno)]="not applicable"
    test_data_1$age_meno=as.factor(test_data_1$age_meno)
    
    test_data_1$age_menarche=cut(test_data_1$age_menarche,c(5,11,12,13,14,25),include.lowest=TRUE,labels=FALSE)
    test_data_1$age_menarche[is.na(test_data_1$age_menarche)]="not applicable"
    test_data_1$age_menarche=as.factor(test_data_1$age_menarche)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu,data=test_data_1))
    result_summary_1=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                       
                       
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk,data=test_data_1))
    result_summary_2a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+alc,data=test_data_1))
    result_summary_2b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+metcat,data=test_data_1))
    result_summary_2c=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+bmi,data=test_data_1))
    result_summary_3a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+waistcircumf_0_0,data=test_data_1))
    result_summary_3b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+bodysizeage10_0_0,data=test_data_1))
    result_summary_3c=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+bmi+waistcircumf_0_0+bodysizeage10_0_0,data=test_data_1))
    result_summary_3=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                       
                       
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+HYPER,data=test_data_1))
    result_summary_4a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+sbp_ave2+dbp_ave2,data=test_data_1))
    result_summary_4b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+HYPER+sbp_ave2+dbp_ave2,data=test_data_1))
    result_summary_4=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                       
                       
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+pill+age_menarche+meno+hrt+age_meno+live_births_0_0+birthage,data=test_data_1))
    result_summary_6=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                       
                       
    )
    result_summary_w=data.frame(rbind(result_summary_1,result_summary_2a,result_summary_2b,result_summary_2c,
                                      result_summary_3a,result_summary_3b,result_summary_3c,result_summary_3,
                                      result_summary_4a,result_summary_4b,result_summary_4,result_summary_6))
    names(result_summary_w)=c("coef","se","p","HRCI")
    result_summary_w$N_exp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                       "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    )
    result_summary_w$N_unexp=c(nrow(na.omit(data.frame(test_data_1[test_data_1$endpoint==1&test_data_1$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2",
                                                                                                                         "pill","age_menarche","meno","hrt","age_meno","live_births_0_0","birthage")])))
    ) 
    
    
    result_summary_w$model=c("Basic","+ Smoking","+ Alcohol","+ Physical activity",
                             "+ BMI","+ Waist circumference","+ Body size at age 10","+ all Adiposity traits",
                             "Hypertension","Blood pressures","+ Hypertension and BP","+ Female reproductive factors")
    
    result_summary_w$sex="women"
  }else{
    result_summary_w=NA
  }
  if(!(i %in% c(16:19))){
    test_data_2$metcat=as.character(cut(test_data_2$n_22040_0_0,
                                        quantile(test_data_2$n_22040_0_0,probs = seq(0, 1, 1/5),na.rm=TRUE),include.lowest=TRUE,labels=FALSE))
    test_data_2$metcat[is.na(test_data_2$metcat)]="unknown"    
    test_data_2$metcat=as.factor(test_data_2$metcat)
    
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu,data=test_data_2))
    result_summary_1=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                       
                       
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+smk,data=test_data_2))
    result_summary_2a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+alc,data=test_data_2))
    result_summary_2b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+metcat,data=test_data_2))
    result_summary_2c=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+bmi,data=test_data_2))
    result_summary_3a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+waistcircumf_0_0,data=test_data_2))
    result_summary_3b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+bodysizeage10_0_0,data=test_data_2))
    result_summary_3c=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+bmi+waistcircumf_0_0+bodysizeage10_0_0,data=test_data_2))
    result_summary_3=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                       
                       
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+HYPER,data=test_data_2))
    result_summary_4a=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_4b=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                     " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                     "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                        
                        
    )
    fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(region)+dep+ethnicity5+edu+HYPER+sbp_ave2+dbp_ave2,data=test_data_2))
    result_summary_4=c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")"))
                       
                       
    )
    result_summary_6=result_summary_1
    #combine summary results for 4 models:
    result_summary_m=data.frame(rbind(result_summary_1,result_summary_2a,result_summary_2b,result_summary_2c,
                                      result_summary_3a,result_summary_3b,result_summary_3c,result_summary_3,
                                      result_summary_4a,result_summary_4b,result_summary_4,result_summary_6))
    names(result_summary_m)=c("coef","se","p","HRCI")
    #create variabels for case number in exposed and unexposed goups:
    result_summary_m$N_exp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==1,c("sex","age_at_study_date",
                                                                                                                       "region","age_at_censor","has_diabetes",
                                                                                                                       "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2")])))
    )
    result_summary_m$N_unexp=c(nrow(na.omit(data.frame(test_data_2[test_data_2$endpoint==1&test_data_2$has_diabetes==0,c("sex","age_at_study_date",
                                                                                                                         "region","age_at_censor","has_diabetes",
                                                                                                                         "smk","alc","edu","metcat","dep","ethnicity5","bmi","waistcircumf_0_0","bodysizeage10_0_0","HYPER","sbp_ave2","dbp_ave2")])))
    ) 
    
    
    result_summary_m$model=c("Basic","+ Smoking","+ Alcohol","+ Physical activity",
                             "+ BMI","+ Waist circumference","+ Body size at age 10","+ all Adiposity traits",
                             "Hypertension","Blood pressures","+ Hypertension and BP","+ Female reproductive factors")
    result_summary_m$sex="men"
  }else{
    result_summary_m=NA
  }
  
  
  
  
  result_summary=data.frame(rbind(result_summary,result_summary_w,result_summary_m))
  # result_summary=result_summary[!is.na(result_summary$coef),]
  result_summary$outcome=strsplit(names(dat_cancer_UKB)[i],"_")[[1]][1]
  
  
  #add rows to the combined data frame of results
  UKB_allsex_adj=rbind(UKB_allsex_adj,data.frame(result_summary))
  result_summary=data.frame()
  result_summary_m=data.frame()
  result_summary_w=data.frame()
  
}
write.csv(UKB_allsex_adj,"M:/Nov/UKB_adj_alcadj.csv")

####IX. MWS analyses with adjustments for each covariates individually####
dat_cancer_mws=save_dat_cancer_mws
i=NULL
test_data=data.frame()
fit=list()
fit_women=list()
fit_bmi=list()
result_summary=data.frame()
MWS_primary_adj=data.frame()

result_summary=data.frame()
result_summary_1=data.frame()
result_summary_2=data.frame()
result_summary_3=data.frame()
result_summary_4=data.frame()
result_summary_5=data.frame()
result_summary_6=data.frame()
result_summary_7=data.frame()


for(i in c(316:334,336:344) ){ 
  test_data=dat_cancer_mws[,c(i,1:ncol(dat_cancer_mws))]
  names(test_data)[1]="endpoint"
  if(i==329){
    test_data$endpoint[test_data$age_at_censor<=50]=0
  }
  #Exclusion criteria:
  if (i==331){
    test_data=test_data[test_data$r_hyster_yn==2,]
  } else if(i==332){
    test_data=test_data[test_data$r_bilat_yn==2,]
  }
  test_data=test_data[complete.cases(test_data[,c("r_bmi","r_hyper",
                                                  "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")]),]
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu,data=test_data))
  
  result_summary_1=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu+r_smoke,data=test_data))
  
  result_summary_2a=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                     
  ))
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu+alc,data=test_data))
  
  result_summary_2b=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                     
  ))
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu+r_exercise+r_stren_exerc,data=test_data))
  
  result_summary_2c=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                    " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                    "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                     
  ))
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu+r_bmi,data=test_data))
  
  result_summary_3=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu+r_hyper,data=test_data))
  
  result_summary_4=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  fit=summary(coxph(Surv(age_at_study_date,age_at_censor,endpoint)~has_diabetes+strata(birth_cohort)+strata(r_region)+r_dep_quintile+f1_ethnic+edu+r_hrtnpc+r_oc_yn+r_menop_status+age_menarche_cat+age_meno+r_num_children+birthage,data=test_data))
  
  result_summary_6=data.frame(rbind(c(as.numeric(fit$coefficients[1,c(1,3,5)]),as.character(paste0(sprintf("%.2f",fit$conf.int[1,1]),
                                                                                                   " (",sprintf("%.2f",fit$conf.int[1,3]),
                                                                                                   "-",sprintf("%.2f",fit$conf.int[1,4]),")")))
                                    
  ))
  
  result_summary=data.frame(rbind(result_summary_1,result_summary_2a,result_summary_2b,result_summary_2c,
                                  result_summary_3,result_summary_4,result_summary_6))
  names(result_summary)=c("coef","se","p","HRCI")
  result_summary$N_exp=NA
  result_summary$N_unexp=NA
  
  result_summary$N_exp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==1,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  result_summary$N_unexp=c(
    nrow(na.omit(data.frame(test_data[test_data$endpoint==1&test_data$has_diabetes==0,c("r_bmi","r_hyper",
                                                                                        "r_hrtnpc","r_oc_yn","r_menop_status","age_menarche_cat","age_meno","r_num_children","birthage")])))
  )
  
  
  result_summary$model=c("Basic","+ Smoking","+ Alcohol","+ Physical activity","+ BMI",
                         "Hypertension","+ Female reproductive factors")
  result_summary$sex="women"
  #Label cancer sites:                         
  result_summary$outcome=strsplit(names(dat_cancer_mws)[i],"_")[[1]][1]
  
  #add rows to the combined data frame of results
  MWS_primary_adj=rbind(MWS_primary_adj,data.frame(result_summary))
  result_summary=data.frame()
  
  
}
write.csv(MWS_primary_adj,"M:/Nov/MWS_adj.csv")

