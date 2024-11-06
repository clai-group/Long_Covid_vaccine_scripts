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
## All age
vax_pasc_all = outglm("PASC.any",
                      glm_dat_cleaned_norgan,
                      group="all")
vax_pasc_all


## By age groups
results <- data.frame(age_start = integer(), age_end = integer(),
                      OR = numeric(), p_value = numeric(),
                      LB = numeric(), UB = numeric())

min_age <- 35
max_age <- 80
window_size <- 5


for (start_age in seq(min_age, max_age-window_size, by = window_size)) {
  end_age <- start_age + window_size
  subset_data <- glm_dat_cleaned_norgan[glm_dat_cleaned_norgan$age >= start_age & 
                                          glm_dat_cleaned_norgan$age < end_age, ]
  
  glm_model <- outglm("PASC.any",subset_data,group= start_age)
  
  results <- rbind(results, data.frame(age_start = start_age, age_end = end_age, 
                                       OR = round(glm_model$estimate,3), p_value = round(glm_model$p.value,3),
                                       LB = round(glm_model$conf.low,3), UB =round( glm_model$conf.high,3)))
}
vax_pasc_35 =outglm("PASC.any",subset(glm_dat_cleaned_norgan,glm_dat_cleaned_norgan$age < 35 ),group="under 35")
vax_pasc_35
results =  rbind(data.frame(age_start = 20, age_end = 35, 
                            OR = round(vax_pasc_35$estimate,3), p_value = round(vax_pasc_35$p.value,3),
                            LB = round(vax_pasc_35$conf.low,3), UB =round( vax_pasc_35$conf.high,3)),
                 results)
vax_pasc_85 =outglm("PASC.any",subset(glm_dat_cleaned_norgan,glm_dat_cleaned_norgan$age >= 80 ),group="over 85")
vax_pasc_85
results =  rbind(results, data.frame(age_start = 80, age_end = 105, 
                                     OR = round(vax_pasc_85$estimate,3), p_value = round(vax_pasc_85$p.value,3),
                                     LB = round(vax_pasc_85$conf.low,3), UB =round( vax_pasc_85$conf.high,3)))



