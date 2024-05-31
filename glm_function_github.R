###########################
####### R script for repliating the causal inference analysis on the effect of 
####### vaccine on PASC

### Authors: Alaleh Azhir and Jingya Cheng 




library(dplyr)
library(readr)
library(cobalt)
library(marginaleffects)
library(WeightIt)
library(flextable)
library(ggplot2)
library(officer)
library(lme4)


study_data <- ###load study data with the variables used in the study


### causal inference function
  ## change variable names as needed to match your data
outglm <- function(outcome,#outcome of interest
                   dat.aoi,#data for modeling
                   group,
                   vax = F, # stratify based on this?
                   severity = T,
                   exposure = "vaccination_status_cat",
                   all = T
                   
){
  dat.aoi$label <- as.factor(dat.aoi[[outcome]])
  dat.aoi$label <- ifelse(dat.aoi$label == "0",0,1)#as.numeric(dat.aoi$label)
  
  if(all == T){
    if(severity == T){
      dat.aoi <- dat.aoi %>% dplyr::select("vaccination_status_cat","variant","hispanic","race","age","female","elixhauser_index","prior_infection","anti.viral","severity","Steroids","timeFromVax","label")
      
      W.out <- weightit(vaccination_status_cat ~age+female+elixhauser_index + severity  +variant+race +hispanic +prior_infection +anti.viral,
                        data = dat.aoi, estimand = "ATE" , method = "ebal")
    } else {
      dat.aoi <- dat.aoi %>% dplyr::select("vaccination_status_cat","variant","hispanic","race","age","female","elixhauser_index","prior_infection","anti.viral","Steroids","timeFromVax","label")
      print("hi")
      W.out <- weightit(vaccination_status_cat ~ age+female+elixhauser_index +variant +race+hispanic+prior_infection +anti.viral ,
                        data = dat.aoi, estimand = "ATE" , method = "ebal")
    }
  } 
  bal <- bal.tab(W.out, stats = c("m", "v", "ks"), m.threshold = .05, disp.v.ratio = TRUE,poly = 1)
  print(bal)
  fit = glm_weightit(label ~ vaccination_status_cat,
                     data = dat.aoi, weightit = W.out,family = binomial(logit) )
  output1 = avg_comparisons(fit,
                            variables = "vaccination_status_cat",
                            comparison = "lnoravg",
                            transform = "exp") 
  output2 = avg_comparisons(fit,
                            variables = "vaccination_status_cat",
                            comparison = "lnoravg") 
  
  output = data.frame(output2, output1, group)
  
  return(output)
  
  
}


###effects of vaccination on PASC by age group
vax_pasc_70 =outglm("PASC.any",subset(study_data,study_data$age > 70 ),group="over 70")
vax_pasc_35 =outglm("PASC.any",subset(study_data,study_data$age > 35 & study_data$age <= 70),group="35-70")
vax_pasc_under35 =outglm("PASC.any",subset(study_data, study_data$age <= 35),group="18-35")

vax_pasc_all = rbind(vax_pasc_under35, vax_pasc_35, vax_pasc_70)
vax_pasc_tbl = vax_pasc_all[c(2,3,12,13,15,16, 17)]
vax_pasc_tbl$Outcome = "PASC"


colnames(vax_pasc_tbl) = c("Group","Estimate", "OR", "p values", "LB", "UB", "Age.groups", "Outcome")

# **for PASC.symptom aggregate the number of rows in study_data
study_data = study_data %>%
  mutate(age_group_by10years = case_when(
    age<=35 ~ "18-35",
    age>35&age<=70 ~ "35-70",
    age>70 ~ "over 70"
  ))
study_data = study_data %>%
  mutate(age_group_by10years = case_when(
    age<=35 ~ "18-35",
    age>35&age<=70 ~ "35-70",
    age>70 ~ "over 70"
    
  )) 


### produce figure 1 data
count_by_age <- study_data %>%
  group_by(age_group_by10years) %>%
  dplyr::summarise(Distinct_patients= n_distinct(EMPI))

PASC_by_age <- study_data %>%
  filter(PASC.any == 1) %>%
  group_by(age_group_by10years) %>%
  dplyr::summarise(Distinct_PASC_patients= n_distinct(EMPI))

pasc_risk_age <- merge(count_by_age,PASC_by_age,by="age_group_by10years")
pasc_risk_age$absolute.PASC.risk <- round(pasc_risk_age$Distinct_PASC_patients/pasc_risk_age$Distinct_patients,3)*100

PASC_inc_by_age <- study_data %>%
  filter(PASC.any == 1) %>%
  group_by(age_group_by10years) %>%
  dplyr::summarise(PASC_incidents= length(PASC.any))

pasc_risk_age <- merge(pasc_risk_age,PASC_inc_by_age,by="age_group_by10years")

cov_inc_by_age <- study_data %>%
  group_by(age_group_by10years) %>%
  dplyr::summarise(COVID_incidents= length(EMPI))

pasc_risk_age <- merge(pasc_risk_age,cov_inc_by_age,by="age_group_by10years")


pasc_risk_age$absolute.PASC.incident.risk <- round(pasc_risk_age$PASC_incidents/pasc_risk_age$COVID_incidents,3)*100



severity_by_age1 <- study_data %>%
  filter(hospitalization == 1) %>%
  group_by(age_group_by10years) %>%
  dplyr::summarise(hospitalized_incidence= length(hospitalization))

pasc_risk_age <- merge(pasc_risk_age,severity_by_age1,by="age_group_by10years")
pasc_risk_age$absolute.hospitalization.risk <- round(pasc_risk_age$hospitalized_incidence/pasc_risk_age$COVID_incidents,3)*100

severity_by_age2 <- study_data %>%
  filter(icu_or_ventilation == 1) %>%
  group_by(age_group_by10years) %>%
  dplyr::summarise(icuventilation_incidence= length(icu_or_ventilation))

pasc_risk_age <- merge(pasc_risk_age,severity_by_age2,by="age_group_by10years")
pasc_risk_age$absolute.icuventilation.risk <- round(pasc_risk_age$icuventilation_incidence/pasc_risk_age$COVID_incidents,3)*100


####add vaccine within 6 months
vax_inc_by_age <- study_data %>% filter(vaccination_status_cat == 3) %>%
  group_by(age_group_by10years) %>%
  dplyr::summarise(recent_vaccine_incidence= length(EMPI))

pasc_risk_age <- merge(pasc_risk_age,vax_inc_by_age,by="age_group_by10years")
pasc_risk_age$percent.vaccine.within.6month.by.age <- round(pasc_risk_age$recent_vaccine_incidence/pasc_risk_age$COVID_incidents,3)*100

colnames(pasc_risk_age)[1] = "Age.groups"
t_pasc_risk_age = t(pasc_risk_age)
t_pasc_risk_age = data.frame(Group = row.names(t_pasc_risk_age), t_pasc_risk_age)
row.names(t_pasc_risk_age) = NULL
colnames(t_pasc_risk_age) = t_pasc_risk_age[1,]
t_pasc_risk_age = t_pasc_risk_age[-1,]
colnames(t_pasc_risk_age)[1] = "group"
#write.csv(pasc_risk_age, "pasc_risk_age.csv", row.names = F)

##absolute.PASC.risk
prop.test(x = pasc_risk_age$Distinct_PASC_patients[1:2], n = pasc_risk_age$Distinct_patients[1:2])
prop.test(x = pasc_risk_age$Distinct_PASC_patients[2:3], n = pasc_risk_age$Distinct_patients[2:3])

##absolute.PASC.incident.risk
prop.test(x = pasc_risk_age$PASC_incidents[1:2], n = pasc_risk_age$COVID_incidents[1:2])
prop.test(x = pasc_risk_age$PASC_incidents[2:3], n = pasc_risk_age$COVID_incidents[2:3])

##absolute.hospitalization.risk
prop.test(x = pasc_risk_age$hospitalized_incidence[1:2], n = pasc_risk_age$COVID_incidents[1:2])
prop.test(x = pasc_risk_age$hospitalized_incidence[2:3], n = pasc_risk_age$COVID_incidents[2:3])

##absolute.icuventilation.risk
prop.test(x = pasc_risk_age$icuventilation_incidence[1:2], n = pasc_risk_age$COVID_incidents[1:2])
prop.test(x = pasc_risk_age$icuventilation_incidence[2:3], n = pasc_risk_age$COVID_incidents[2:3])

##percent.patients
prop.test(x = pasc_risk_age$COVID_incidents[1:2], n = c(40246,40246))
prop.test(x = pasc_risk_age$COVID_incidents[2:3], n = c(40246,40246))

##PASC.percent.patients
prop.test(x = pasc_risk_age$PASC_incidents[1:2], n = c(11590,11590))
prop.test(x = pasc_risk_age$PASC_incidents[2:3], n =c(11590,11590))

##percent.severity
prop.test(x = pasc_risk_age$Severity_binary_incidence[1:2], n = pasc_risk_age$COVID_incidents[1:2])
prop.test(x = pasc_risk_age$Severity_binary_incidence[2:3], n = pasc_risk_age$COVID_incidents[2:3])


##percent.vaccine.within.6month.by.age
prop.test(x = pasc_risk_age$recent_vaccine_incidence[1:2], n = pasc_risk_age$COVID_incidents[1:2])
prop.test(x = pasc_risk_age$recent_vaccine_incidence[2:3], n = pasc_risk_age$COVID_incidents[2:3])






##effect of vaccine on severity

vax_sev_2_70over = outglm("hospitalization",subset(study_data,study_data$age > 70),group="over 70", severity = F)
vax_sev_3_70over = outglm("icu_or_ventilation",subset(study_data,study_data$age > 70),group="over 70", severity = F)


vax_sev_2_35 = outglm("hospitalization",subset(study_data,study_data$age > 35 & study_data$age <= 70),group="35-70", severity = F)
vax_sev_3_35 = outglm("icu_or_ventilation",subset(study_data,study_data$age > 35 & study_data$age <= 70),group="35-70", severity = F)

vax_sev_2_35under = outglm("hospitalization",subset(study_data, study_data$age <= 35),group="18-35", severity = F)
vax_sev_3_35under = outglm("icu_or_ventilation",subset(study_data, study_data$age <= 35 ),group="18-35", severity = F)

vax_sev_2 = rbind(vax_sev_2_35under, vax_sev_2_35, vax_sev_2_70over )
vax_sev_2$outcome = "hospitalization"
vax_sev_3 = rbind(vax_sev_3_35under,vax_sev_3_35,vax_sev_3_70over)
vax_sev_3$outcome = "ICU/Ventilation"
vax_sev = rbind(vax_sev_2, vax_sev_3)
vax_sev2 = vax_sev%>% filter(contrast != "ln(odds(2) / odds(0))")
vax_sev_tbl = vax_sev[c(2,3,12,13,15,16, 17, 18)]


colnames(vax_sev_tbl) = c("Group","Estimate", "OR", "p values", "LB", "UB", "Age groups", "Outcome")
#write.csv(vax_sev_tbl, "vax_sev_tbl.csv", row.names = F)


###function to estimate effects of severity on PASC
outglm_sev <- function(outcome,#outcome of interest
                       dat.aoi,#data for modeling
                       group
                       
){
  dat.aoi$label <- as.factor(dat.aoi[[outcome]])
  dat.aoi$label <- ifelse(dat.aoi$label == "0",0,1)#as.numeric(dat.aoi$label)
  
  dat.aoi <- dat.aoi %>% dplyr::select("hispanic","race","age","female",
                                "elixhauser_index","prior_infection",
                                "anti.viral","severity","Steroids",
                                "label" ,"yrqt", "vaccination_status_cat")
  
  W.out <- weightit(severity ~ age+female+elixhauser_index 
                    + yrqt +prior_infection + anti.viral+Steroids,
                    data = dat.aoi, estimand = "ATE" , method = "ebal")
  
  
  bal <- bal.tab(W.out, stats = c("m", "v", "ks"), m.threshold = .05, disp.v.ratio = TRUE,poly = 3)
  print(bal)
  fit = glm_weightit(label ~ severity,
                     data = dat.aoi, weightit = W.out,family = binomial(logit) )
  
  output1 = avg_comparisons(fit,
                            variables = "severity",
                            comparison = "lnoravg",
                            transform = "exp") 
  output2 = avg_comparisons(fit,
                            variables = "severity",
                            comparison = "lnoravg") 
  
  output = data.frame(output2, output1, group)
  
  return(output)
  
}
study_data2 = study_data
study_data2$severity = factor(study_data2$severity)
sev_pasc_70over = outglm_sev("PASC.any",subset(study_data2,study_data2$age > 70), group = "over 70")
sev_pasc_35 = outglm_sev("PASC.any",subset(study_data2,study_data2$age > 35 & study_data2$age <= 70), group = "35-70")
sev_pasc_35under = outglm_sev("PASC.any",subset(study_data, study_data$age <= 35  & study_data$severity !=2), group = "18-35")

sev_pasc_by_age = rbind(sev_pasc_35under[,-c(10:12, 20:22)], sev_pasc_35 ,sev_pasc_70over)

sev_pasc_tbl =sev_pasc_by_age2[c(2,3,12,13,15,16, 17)]
sev_pasc_tbl$Outcome = "PASC"
colnames(sev_pasc_tbl) = c("Group","Estimate", "OR", "p values", "LB", "UB", "Age groups", "Outcome")
