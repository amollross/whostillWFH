# =============================== #
#### Who still WFH? replication files ####
#### 3. Heterogeneity and sensitivity analysis ####
#### Aaron Mollross ####
# =============================== #


#### ALTERNATE OUTCOMES (APPENDIX A) ####
### Baseline LPMs for WFHmost
LPMmostpre <- feols(data = df %>% filter(PRE == 1), wfh.most ~ ..main_controls, 
                    cluster = "xwaveid")
LPMmostpand <- feols(data = df %>% filter(PAND == 1), wfh.most ~ ..main_controls, 
                     cluster = "xwaveid")
LPMmostpost <- feols(data = df %>% filter(POST == 1), wfh.most ~ ..main_controls, 
                     cluster = "xwaveid")
modelsummary(list(LPMmostpre, LPMmostpand, LPMmostpost), 
             stars = T, gof_omit = "AIC|BIC|RMSE", 
             output = "WFHmost LPM output - separated.docx")

LPMmost_comb <- feols(data = df, wfh.most ~ PAND + POST + ..main_controls + ..main_controls_PAND + ..main_controls_POST, 
                      cluster = "xwaveid")
modelsummary(LPMmost_comb, 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows,
             output = "WFHmost LPM output - combined.docx")

## OLS for WFHprop
obs <- df %>% group_by(wfh.any) %>% 
  summarise(obs = n()) %>% 
  mutate(share = obs/sum(obs))
df %>% 
  filter(wfh.any == 1) %>% 
  count(wfh.formalagree) %>% 
  mutate(share = n/sum(n))
df %>% 
  filter(wfh.any == 1) %>% 
  summarise(n=n(), n.ind=n_distinct(xwaveid))
OLSsharepre <- df %>%
  filter(wfh.any > 0, PRE == 1) %>%
  feols(wfh.prop ~ ..main_controls, cluster = "xwaveid")
OLSsharepand <- df %>%
  filter(wfh.any > 0, PAND == 1) %>%
  feols(wfh.prop ~ ..main_controls, cluster = "xwaveid")
OLSsharepost <- df %>%
  filter(wfh.any > 0, POST == 1) %>%
  feols(wfh.prop ~ ..main_controls, cluster = "xwaveid")
modelsummary(list(OLSsharepre, OLSsharepand, OLSsharepost), 
             stars = T, gof_omit = "AIC|BIC|RMSE",
             output = "WFHprop OLS output - separated.docx")
df %>% 
  filter(wfh.any == 1) %>% 
  summarise(mean=mean(wfh.prop, na.rm = T), 
            sd=sd(wfh.prop, na.rm = T))



#### EXTENSIONS TO GEOGRAPHY AND EMPLOYMENT STATUS (APPENDIX A) ####
# WFHany
LPManypre_geog <- feols(data = df %>% filter(PRE == 1), wfh.any ~ ..main_controls + ..geog_controls, 
                   cluster = "xwaveid")
LPManypand_geog <- feols(data = df %>% filter(PAND == 1), wfh.any ~ ..main_controls + ..geog_controls,
                    cluster = "xwaveid")
LPManypost_geog <- feols(data = df %>% filter(POST == 1), wfh.any ~ ..main_controls + ..geog_controls,
                    cluster = "xwaveid")
LPManypre_employ <- feols(data = df %>% filter(PRE == 1), wfh.any ~ ..main_controls + ..employ_controls, 
                   cluster = "xwaveid")
LPManypand_employ <- feols(data = df %>% filter(PAND == 1), wfh.any ~ ..main_controls + ..employ_controls,
                    cluster = "xwaveid")
LPManypost_employ <- feols(data = df %>% filter(POST == 1), wfh.any ~ ..main_controls + ..employ_controls,
                    cluster = "xwaveid")
modelsummary(list(LPManypre_geog, LPManypand_geog, LPManypost_geog, 
                  LPManypre_employ, LPManypand_employ, LPManypost_employ), 
             stars = T, gof_omit = "AIC|BIC|RMSE", 
             output = "WFHany LPM output - extensions.xlsx")

# WFHmost
LPMmostpre_geog <- feols(data = df %>% filter(PRE == 1), wfh.most ~ ..main_controls + ..geog_controls, 
                         cluster = "xwaveid")
LPMmostpand_geog <- feols(data = df %>% filter(PAND == 1), wfh.most ~ ..main_controls + ..geog_controls,
                          cluster = "xwaveid")
LPMmostpost_geog <- feols(data = df %>% filter(POST == 1), wfh.most ~ ..main_controls + ..geog_controls,
                          cluster = "xwaveid")
LPMmostpre_employ <- feols(data = df %>% filter(PRE == 1), wfh.most ~ ..main_controls + ..employ_controls, 
                           cluster = "xwaveid")
LPMmostpand_employ <- feols(data = df %>% filter(PAND == 1), wfh.most ~ ..main_controls + ..employ_controls,
                            cluster = "xwaveid")
LPMmostpost_employ <- feols(data = df %>% filter(POST == 1), wfh.most ~ ..main_controls + ..employ_controls,
                            cluster = "xwaveid")
modelsummary(list(LPMmostpre_geog, LPMmostpand_geog, LPMmostpost_geog, 
                  LPMmostpre_employ, LPMmostpand_employ, LPMmostpost_employ), 
             stars = T, gof_omit = "AIC|BIC|RMSE", 
             output = "WFHmost LPM output - extensions.xlsx")




#### HETEROGENEITY ANALYSIS (APPENDIX A) ####
### Gender splits
# Male LPMs for WFHany
LPManypre_male <- feols(data = df %>% filter(PRE == 1, female == 0), wfh.any ~ ..main_controls, 
                   cluster = "xwaveid")
LPManypand_male <- feols(data = df %>% filter(PAND == 1, female == 0), wfh.any ~ ..main_controls, 
                         cluster = "xwaveid")
LPManypost_male <- feols(data = df %>% filter(POST == 1, female == 0), wfh.any ~ ..main_controls, 
                         cluster = "xwaveid")

# Female LPMs for WFHany
LPManypre_female <- feols(data = df %>% filter(PRE == 1, female == 1), wfh.any ~ ..main_controls, 
                          cluster = "xwaveid")
LPManypand_female <- feols(data = df %>% filter(PAND == 1, female == 1), wfh.any ~ ..main_controls, 
                           cluster = "xwaveid")
LPManypost_female <- feols(data = df %>% filter(POST == 1, female == 1), wfh.any ~ ..main_controls, 
                           cluster = "xwaveid")

# WFHany gender split outputs
rows <- tribble(~"term", ~LPManypre_male, ~LPManypand_male, ~LPManypost_male, ~LPManypre_female, ~LPManypand_female, ~LPManypost_female,
                "Gender:","Male","Male","Male","Female","Female","Female",
                "Pandemic period:", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)")
attr(rows, "position") <- c(1,2)
modelsummary(list(LPManypre_male, LPManypand_male, LPManypost_male, LPManypre_female, LPManypand_female, LPManypost_female), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "WFHany output - gender split.docx")

# Male LPMs for WFHmost
LPMmostpre_male <- feols(data = df %>% filter(PRE == 1, female == 0), wfh.most ~ ..main_controls, 
                         cluster = "xwaveid")
LPMmostpand_male <- feols(data = df %>% filter(PAND == 1, female == 0), wfh.most ~ ..main_controls, 
                          cluster = "xwaveid")
LPMmostpost_male <- feols(data = df %>% filter(POST == 1, female == 0), wfh.most ~ ..main_controls, 
                          cluster = "xwaveid")

# Female LPMs for WFHmost
LPMmostpre_female <- feols(data = df %>% filter(PRE == 1, female == 1), wfh.most ~ ..main_controls, 
                           cluster = "xwaveid")
LPMmostpand_female <- feols(data = df %>% filter(PAND == 1, female == 1), wfh.most ~ ..main_controls, 
                            cluster = "xwaveid")
LPMmostpost_female <- feols(data = df %>% filter(POST == 1, female == 1), wfh.most ~ ..main_controls, 
                            cluster = "xwaveid")

# WFHmost gender split outputs
rows <- tribble(~"term", ~LPMmostpre_male, ~LPMmostpand_male, ~LPMmostpost_male, ~LPMmostpre_female, ~LPMmostpand_female, ~LPMmostpost_female,
                "Gender:","Male","Male","Male","Female","Female","Female",
                "Pandemic period:", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)")
attr(rows, "position") <- c(1,2)
modelsummary(list(LPMmostpre_male, LPMmostpand_male, LPMmostpost_male, LPMmostpre_female, LPMmostpand_female, LPMmostpost_female), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "WFHmost output - gender split.docx")

### Disability splits
# Non-disab LPMs for WFHany
LPManypre_nondisab <- feols(data = df %>% filter(PRE == 1, disab == 0), wfh.any ~ ..main_controls, 
                        cluster = "xwaveid")
LPManypand_nondisab <- feols(data = df %>% filter(PAND == 1, disab == 0), wfh.any ~ ..main_controls, 
                             cluster = "xwaveid")
LPManypost_nondisab <- feols(data = df %>% filter(POST == 1, disab == 0), wfh.any ~ ..main_controls, 
                             cluster = "xwaveid")

# Disab LPMs for WFHany
LPManypre_disab <- feols(data = df %>% filter(PRE == 1, disab == 1), wfh.any ~ ..main_controls - disab.lmtswk.scale - disab.type.cogni - disab.duration, 
                         cluster = "xwaveid")
LPManypand_disab <- feols(data = df %>% filter(PAND == 1, disab == 1), wfh.any ~ ..main_controls - disab.lmtswk.scale - disab.type.cogni - disab.duration, 
                          cluster = "xwaveid")
LPManypost_disab <- feols(data = df %>% filter(POST == 1, disab == 1), wfh.any ~ ..main_controls - disab.lmtswk.scale - disab.type.cogni - disab.duration, 
                          cluster = "xwaveid")

# WFHany disability split outputs
rows <- tribble(~"term", ~LPManypre_nondisab, ~LPManypand_nondisab, ~LPManypost_nondisab, ~LPManypre_disab, ~LPManypand_disab, ~LPManypost_disab,
                "Gender:","nondisab","nondisab","nondisab","disab","disab","disab",
                "Pandemic period:", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)")
attr(rows, "position") <- c(1,2)
modelsummary(list(LPManypre_nondisab, LPManypand_nondisab, LPManypost_nondisab, LPManypre_disab, LPManypand_disab, LPManypost_disab), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "WFHany output - disability split.docx")

# Non-disab LPMs for WFHmost
LPMmostpre_nondisab <- feols(data = df %>% filter(PRE == 1, disab == 0), wfh.most ~ ..main_controls, 
                             cluster = "xwaveid")
LPMmostpand_nondisab <- feols(data = df %>% filter(PAND == 1, disab == 0), wfh.most ~ ..main_controls, 
                              cluster = "xwaveid")
LPMmostpost_nondisab <- feols(data = df %>% filter(POST == 1, disab == 0), wfh.most ~ ..main_controls, 
                              cluster = "xwaveid")

# Disab LPMs for WFHmost
LPMmostpre_disab <- feols(data = df %>% filter(PRE == 1, disab == 1), wfh.most ~ ..main_controls - disab.lmtswk.scale - disab.type.cogni - disab.duration, 
                          cluster = "xwaveid")
LPMmostpand_disab <- feols(data = df %>% filter(PAND == 1, disab == 1), wfh.most ~ ..main_controls - disab.lmtswk.scale - disab.type.cogni - disab.duration, 
                           cluster = "xwaveid")
LPMmostpost_disab <- feols(data = df %>% filter(POST == 1, disab == 1), wfh.most ~ ..main_controls - disab.lmtswk.scale - disab.type.cogni - disab.duration, 
                           cluster = "xwaveid")

# WFHmost disability split outputs
rows <- tribble(~"term", ~LPMmostpre_nondisab, ~LPMmostpand_nondisab, ~LPMmostpost_nondisab, ~LPMmostpre_disab, ~LPMmostpand_disab, ~LPMmostpost_disab,
                "Gender:","nondisab","nondisab","nondisab","disab","disab","disab",
                "Pandemic period:", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)")
attr(rows, "position") <- c(1,2)
modelsummary(list(LPMmostpre_nondisab, LPMmostpand_nondisab, LPMmostpost_nondisab, LPMmostpre_disab, LPMmostpand_disab, LPMmostpost_disab), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "WFHmost output - disability split.docx")

### Employment status splits
# Full-time LPMs for WFHany
LPManypre_FT <- feols(data = df %>% filter(PRE == 1, job.casual == 0 & lfs.emp.ft == 1), wfh.any ~ ..main_controls, 
                            cluster = "xwaveid")
LPManypand_FT <- feols(data = df %>% filter(PAND == 1, job.casual == 0 & lfs.emp.ft == 1), wfh.any ~ ..main_controls, 
                             cluster = "xwaveid")
LPManypost_FT <- feols(data = df %>% filter(POST == 1, job.casual == 0 & lfs.emp.ft == 1), wfh.any ~ ..main_controls, 
                             cluster = "xwaveid")

# Part-time LPMs for WFHany
LPManypre_PT <- feols(data = df %>% filter(PRE == 1, job.casual == 1 | lfs.emp.pt == 1), wfh.any ~ ..main_controls, 
                         cluster = "xwaveid")
LPManypand_PT <- feols(data = df %>% filter(PAND == 1, job.casual == 1 | lfs.emp.pt == 1), wfh.any ~ ..main_controls, 
                          cluster = "xwaveid")
LPManypost_PT <- feols(data = df %>% filter(POST == 1, job.casual == 1 | lfs.emp.pt == 1), wfh.any ~ ..main_controls,
                          cluster = "xwaveid")

# WFHany employment split outputs
rows <- tribble(~"term", ~LPManypre_FT, ~LPManypand_FT, ~LPManypost_FT, ~LPManypre_PT, ~LPManypand_PT, ~LPManypost_PT,
                "Employ:","FT","FT","FT","PT","PT","PT",
                "Pandemic period:", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)")
attr(rows, "position") <- c(1,2)
modelsummary(list(LPManypre_FT, LPManypand_FT, LPManypost_FT, LPManypre_PT, LPManypand_PT, LPManypost_PT), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "WFHany output - employment split.docx")

# Full-time  LPMs for WFHmost
LPMmostpre_FT <- feols(data = df %>% filter(PRE == 1, job.casual == 0 & lfs.emp.ft == 1), wfh.most ~ ..main_controls, 
                             cluster = "xwaveid")
LPMmostpand_FT <- feols(data = df %>% filter(PAND == 1, job.casual == 0 & lfs.emp.ft == 1), wfh.most ~ ..main_controls, 
                              cluster = "xwaveid")
LPMmostpost_FT <- feols(data = df %>% filter(POST == 1, job.casual == 0 & lfs.emp.ft == 1), wfh.most ~ ..main_controls, 
                              cluster = "xwaveid")

# Part-time LPMs for WFHmost
LPMmostpre_PT <- feols(data = df %>% filter(PRE == 1, job.casual == 1 | lfs.emp.pt == 1), wfh.most ~ ..main_controls, 
                          cluster = "xwaveid")
LPMmostpand_PT <- feols(data = df %>% filter(PAND == 1, job.casual == 1 | lfs.emp.pt == 1), wfh.most ~ ..main_controls, 
                           cluster = "xwaveid")
LPMmostpost_PT <- feols(data = df %>% filter(POST == 1, job.casual == 1 | lfs.emp.pt == 1), wfh.most ~ ..main_controls, 
                           cluster = "xwaveid")

# WFHmost employment split outputs
rows <- tribble(~"term", ~LPMmostpre_FT, ~LPMmostpand_FT, ~LPMmostpost_FT, ~LPMmostpre_PT, ~LPMmostpand_PT, ~LPMmostpost_PT,
                "Employ:","FT","FT","FT","PT","PT","PT",
                "Pandemic period:", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)")
attr(rows, "position") <- c(1,2)
modelsummary(list(LPMmostpre_FT, LPMmostpand_FT, LPMmostpost_FT, LPMmostpre_PT, LPMmostpand_PT, LPMmostpost_PT), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "WFHmost output - employment split.docx")





### SENSITIVITY ANALYSIS (APPEDNIX C) ####
# Combined model with indiv FE
LPMany_comb_indFE <- feols(data = df, wfh.any ~ PAND + POST + ..main_controls + ..main_controls_PAND + ..main_controls_POST -
                             female - I(female*PAND) - I(female*POST) | xwaveid, cluster = "xwaveid")
LPMmost_comb_indFE <- feols(data = df, wfh.most ~ PAND + POST + ..main_controls + ..main_controls_PAND + ..main_controls_POST -
                              female - I(female*PAND) - I(female*POST) | xwaveid, cluster = "xwaveid")

rows <- tribble(~"term", ~LPMany_comb_indFE, ~LPMmost_comb_indFE,
                "Dependent variable:","WFHany","WFHmost")
attr(rows, "position") <- 1
modelsummary(list(LPMany_comb_indFE, LPMmost_comb_indFE), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "combined LPM output - indiv FE.docx")

# Check for individuals with no change in outcome (absorbed by FE)
no_change <- df %>%
  group_by(xwaveid) %>%
  summarise(
    any_obs = sum(!is.na(wfh.any)),
    any_min = min(wfh.any, na.rm = TRUE),
    any_max = max(wfh.any, na.rm = TRUE),
    most_obs = sum(!is.na(wfh.most)),
    most_min = min(wfh.most, na.rm = TRUE),
    most_max = max(wfh.most, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(any_changes = (any_min != any_max),
         any_constant = !any_changes,
         most_changes = (most_min != most_max),
         most_constant = !most_changes)
no_change %>% summarise(n_ids = n(),
                        n_any_constant = sum(any_constant),
                        n_most_constant = sum(most_constant),
                        share_any_constant = mean(any_constant),
                        share_most_constant = mean(most_constant))

## Logit models
# Share of predicted LPM outputs that are above 1 or below 0
models <- list(LPManypre, LPManypand, LPManypost, LPMmostpre, LPMmostpand, LPMmostpost)
LPM_errors <- map_dfr(models, ~{
  phat <- predict(.x)
  tibble(below_0 = sum(phat < 0),
         above_1 = sum(phat > 1),
         below_0_share = mean(phat < 0),
         above_1_share = mean(phat > 1))
}, .id = "model")

# WFHany
LOGanypre <- feglm(data = df %>% filter(PRE == 1), wfh.any ~ ..main_controls, 
                   cluster = "xwaveid", family = "logit")
LOGanypand <- feglm(data = df %>% filter(PAND == 1), wfh.any ~ ..main_controls, 
                    cluster = "xwaveid", family = "logit")
LOGanypost <- feglm(data = df %>% filter(POST == 1), wfh.any ~ ..main_controls, 
                    cluster = "xwaveid", family = "logit")
# WFHmost
LOGmostpre <- feglm(data = df %>% filter(PRE == 1), wfh.most ~ ..main_controls, 
                    cluster = "xwaveid", family = "logit")
LOGmostpand <- feglm(data = df %>% filter(PAND == 1), wfh.most ~ ..main_controls, 
                     cluster = "xwaveid", family = "logit")
LOGmostpost <- feglm(data = df %>% filter(POST == 1), wfh.most ~ ..main_controls, 
                     cluster = "xwaveid", family = "logit")

rows <- tribble(~"term", ~LOGanypre, ~LOGanypand, ~LOGanypost, ~LOGmostpre, ~LOGmostpand, ~LOGmostpost,
                "Dependent variable:","WFHany","WFHany","WFHany","WFHmost","WFHmost","WFHmost")
attr(rows, "position") <- 1
modelsummary(list(LOGanypre, LOGanypand, LOGanypost, LOGmostpre, LOGmostpand, LOGmostpost), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL, exponentiate = T,
             output = "separated LOGIT output - appendix.docx")
