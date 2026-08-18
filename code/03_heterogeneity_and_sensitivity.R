# =============================== #
#### Who still WFH? replication files ####
#### 3. Heterogeneity and sensitivity analysis ####
#### Aaron Mollross ####
# =============================== #


#### HETEROGENEITY ANALYSIS ####
### Gender splits
# Male LPMs for WFHany
LPManypre_male <- feols(data = df %>% filter(PRE == 1, female == 0), wfh.any ~ 
                          i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                          prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                          child.resid + child.pres + child.prim + child.high + 
                          disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                          wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                          DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                          urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                          i(state, ref = "[1] NSW") +
                          i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                          i(job.occ.1, ref = "[2] Professionals"), 
                        vcov = "hetero")
LPManypand_male <- feols(data = df %>% filter(PAND == 1, female == 0), wfh.any ~ 
                           i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                           prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                           child.resid + child.pres + child.prim + child.high + 
                           disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                           wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                           DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                           urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                           i(state, ref = "[1] NSW") +
                           i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                           i(job.occ.1, ref = "[2] Professionals"), 
                         vcov = "hetero")
LPManypost_male <- feols(data = df %>% filter(POST == 1, female == 0), wfh.any ~ 
                           i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                           prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                           child.resid + child.pres + child.prim + child.high + 
                           disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                           wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                           DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                           urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                           i(state, ref = "[1] NSW") +
                           i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                         vcov = "hetero")

# Female LPMs for WFHany
LPManypre_female <- feols(data = df %>% filter(PRE == 1, female == 1), wfh.any ~ 
                            i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                            prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                            child.resid + child.pres + child.prim + child.high + 
                            disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                            wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                            DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                            urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                            i(state, ref = "[1] NSW") +
                            i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                          vcov = "hetero")
LPManypand_female <- feols(data = df %>% filter(PAND == 1, female == 1), wfh.any ~ 
                             i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                             prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                             child.resid + child.pres + child.prim + child.high + 
                             disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                             wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                             DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                             urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                             i(state, ref = "[1] NSW") +
                             i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                           vcov = "hetero")
LPManypost_female <- feols(data = df %>% filter(POST == 1, female == 1), wfh.any ~ 
                             i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                             prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                             child.resid + child.pres + child.prim + child.high + 
                             disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                             wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                             DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                             urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                             i(state, ref = "[1] NSW") +
                             i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                           vcov = "hetero")

# WFHany gender split outputs
rows <- tribble(~"term", ~LPManypre_male, ~LPManypand_male, ~LPManypost_male, ~LPManypre_female, ~LPManypand_female, ~LPManypost_female,
                "Gender:","Male","Male","Male","Female","Female","Female",
                "Pandemic period:", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)")
attr(rows, "position") <- c(1,2)
modelsummary(list(LPManypre_male, LPManypand_male, LPManypost_male, LPManypre_female, LPManypand_female, LPManypost_female), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "regression outputs/separated WFHany output - gender split.docx")

# Male LPMs for WFHmost
LPMmostpre_male <- feols(data = df %>% filter(PRE == 1, female == 0), wfh.most ~ 
                           i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                           prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                           child.resid + child.pres + child.prim + child.high + 
                           disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                           wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                           DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                           urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                           i(state, ref = "[1] NSW") +
                           i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                         vcov = "hetero")
LPMmostpand_male <- feols(data = df %>% filter(PAND == 1, female == 0), wfh.most ~ 
                            i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                            prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                            child.resid + child.pres + child.prim + child.high + 
                            disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                            wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                            DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                            urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                            i(state, ref = "[1] NSW") +
                            i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                          vcov = "hetero")
LPMmostpost_male <- feols(data = df %>% filter(POST == 1, female == 0), wfh.most ~ 
                            i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                            prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                            child.resid + child.pres + child.prim + child.high + 
                            disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                            wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                            DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                            urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                            i(state, ref = "[1] NSW") +
                            i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                          vcov = "hetero")

# Female LPMs for WFHmost
LPMmostpre_female <- feols(data = df %>% filter(PRE == 1, female == 1), wfh.most ~ 
                             i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                             prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                             child.resid + child.pres + child.prim + child.high + 
                             disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                             wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                             DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                             urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                             i(state, ref = "[1] NSW") +
                             i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                           vcov = "hetero")
LPMmostpand_female <- feols(data = df %>% filter(PAND == 1, female == 1), wfh.most ~ 
                              i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                              prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                              child.resid + child.pres + child.prim + child.high + 
                              disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                              wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                              DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                              urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                              i(state, ref = "[1] NSW") +
                              i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                            vcov = "hetero")
LPMmostpost_female <- feols(data = df %>% filter(POST == 1, female == 1), wfh.most ~ 
                              i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                              prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                              child.resid + child.pres + child.prim + child.high + 
                              disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                              wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                              DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                              urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                              i(state, ref = "[1] NSW") +
                              i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                            vcov = "hetero")

# WFHmost gender split outputs
rows <- tribble(~"term", ~LPMmostpre_male, ~LPMmostpand_male, ~LPMmostpost_male, ~LPMmostpre_female, ~LPMmostpand_female, ~LPMmostpost_female,
                "Gender:","Male","Male","Male","Female","Female","Female",
                "Pandemic period:", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)")
attr(rows, "position") <- c(1,2)
modelsummary(list(LPMmostpre_male, LPMmostpand_male, LPMmostpost_male, LPMmostpre_female, LPMmostpand_female, LPMmostpost_female), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "regression outputs/separated WFHmost output - gender split.docx")


### Disability splits
# Non-disab LPMs for WFHany
LPManypre_nondisab <- feols(data = df %>% filter(PRE == 1, disab == 0), wfh.any ~ 
                              i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                              female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                              child.resid + child.pres + child.prim + child.high + 
                              wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                              DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                              urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                              i(state, ref = "[1] NSW") +
                              i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                            vcov = "hetero")
LPManypand_nondisab <- feols(data = df %>% filter(PAND == 1, disab == 0), wfh.any ~ 
                               i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                               female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                               child.resid + child.pres + child.prim + child.high + 
                               wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                               DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                               urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                               i(state, ref = "[1] NSW") +
                               i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                             vcov = "hetero")
LPManypost_nondisab <- feols(data = df %>% filter(POST == 1, disab == 0), wfh.any ~ 
                               i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                               female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                               child.resid + child.pres + child.prim + child.high + 
                               wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                               DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                               urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                               i(state, ref = "[1] NSW") +
                               i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                             vcov = "hetero")

# Disab LPMs for WFHany
LPManypre_disab <- feols(data = df %>% filter(PRE == 1, disab == 1), wfh.any ~ 
                           i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                           female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                           child.resid + child.pres + child.prim + child.high + 
                           wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                           DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                           urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                           i(state, ref = "[1] NSW") +
                           i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                         vcov = "hetero")
LPManypand_disab <- feols(data = df %>% filter(PAND == 1, disab == 1), wfh.any ~ 
                            i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                            female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                            child.resid + child.pres + child.prim + child.high + 
                            wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                            DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                            urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                            i(state, ref = "[1] NSW") +
                            i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                          vcov = "hetero")
LPManypost_disab <- feols(data = df %>% filter(POST == 1, disab == 1), wfh.any ~ 
                            i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                            female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                            child.resid + child.pres + child.prim + child.high + 
                            wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                            DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                            urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                            i(state, ref = "[1] NSW") +
                            i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                          vcov = "hetero")

# WFHany disability split outputs
rows <- tribble(~"term", ~LPManypre_nondisab, ~LPManypand_nondisab, ~LPManypost_nondisab, ~LPManypre_disab, ~LPManypand_disab, ~LPManypost_disab,
                "Gender:","nondisab","nondisab","nondisab","disab","disab","disab",
                "Pandemic period:", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)")
attr(rows, "position") <- c(1,2)
modelsummary(list(LPManypre_nondisab, LPManypand_nondisab, LPManypost_nondisab, LPManypre_disab, LPManypand_disab, LPManypost_disab), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "regression outputs/separated WFHany output - disability split.docx")

# Non-disab LPMs for WFHmost
LPMmostpre_nondisab <- feols(data = df %>% filter(PRE == 1, disab == 0), wfh.most ~ 
                               i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                               female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                               child.resid + child.pres + child.prim + child.high + 
                               wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                               DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                               urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                               i(state, ref = "[1] NSW") +
                               i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                             vcov = "hetero")
LPMmostpand_nondisab <- feols(data = df %>% filter(PAND == 1, disab == 0), wfh.most ~ 
                                i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                                female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                                child.resid + child.pres + child.prim + child.high + 
                                wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                                DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                                urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                                i(state, ref = "[1] NSW") +
                                i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                              vcov = "hetero")
LPMmostpost_nondisab <- feols(data = df %>% filter(POST == 1, disab == 0), wfh.most ~ 
                                i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                                female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                                child.resid + child.pres + child.prim + child.high + 
                                wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                                DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                                urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                                i(state, ref = "[1] NSW") +
                                i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                              vcov = "hetero")

# Disab LPMs for WFHmost
LPMmostpre_disab <- feols(data = df %>% filter(PRE == 1, disab == 1), wfh.most ~ 
                            i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                            female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                            child.resid + child.pres + child.prim + child.high + 
                            wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                            DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                            urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                            i(state, ref = "[1] NSW") +
                            i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                          vcov = "hetero")
LPMmostpand_disab <- feols(data = df %>% filter(PAND == 1, disab == 1), wfh.most ~ 
                             i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                             female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                             child.resid + child.pres + child.prim + child.high + 
                             wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                             DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                             urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                             i(state, ref = "[1] NSW") +
                             i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                           vcov = "hetero")
LPMmostpost_disab <- feols(data = df %>% filter(POST == 1, disab == 1), wfh.most ~ 
                             i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                             female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                             child.resid + child.pres + child.prim + child.high + 
                             wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                             DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                             urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                             i(state, ref = "[1] NSW") +
                             i(job.ind.1, ref = "[17] Health Care and Social Assistance") + i(job.occ.1, ref = "[2] Professionals"), 
                           vcov = "hetero")

# WFHmost disability split outputs
rows <- tribble(~"term", ~LPMmostpre_nondisab, ~LPMmostpand_nondisab, ~LPMmostpost_nondisab, ~LPMmostpre_disab, ~LPMmostpand_disab, ~LPMmostpost_disab,
                "Gender:","nondisab","nondisab","nondisab","disab","disab","disab",
                "Pandemic period:", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)")
attr(rows, "position") <- c(1,2)
modelsummary(list(LPMmostpre_nondisab, LPMmostpand_nondisab, LPMmostpost_nondisab, LPMmostpre_disab, LPMmostpand_disab, LPMmostpost_disab), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "regression outputs/separated WFHmost output - disability split.docx")









### ROBUSTNESS & SENSITIVITY ANALYSIS ####
# Combined model with indiv FE
LPMany_comb_indFE <- feols(data = df, wfh.any ~ PAND + POST +
                             i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc + 
                             prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all + 
                             child.resid + child.pres + child.prim + child.high + 
                             disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                             wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise + job.dist + 
                             DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                             urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                             state + i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                             i(job.occ.1, ref = "[2] Professionals") + 
                             
                             i(age.cat.5yr, PAND, ref = "41-45") + I(unieduc*PAND) + I(vocateduc*PAND) + 
                             I(prtnr_any*PAND) + I(prtnr_lfs.emp_all*PAND) + I(prtnr_wfh.any_all*PAND) + 
                             I(child.resid*PAND) + I(child.pres*PAND) + I(child.prim*PAND) + I(child.high*PAND) + 
                             I(disab*PAND) + I(disab.lmtswk.severe*PAND) + I(disab.type.cogni*PAND) + I(disab.duration*PAND) + 
                             I(wage.mn*PAND) + I(lfs.emp.pt*PAND) + I(job.casual*PAND) + I(job.tenure*PAND) + I(job.supervise*PAND) + 
                             I(job.dist*PAND) + I(DNscore_manual*PAND) + I(satisfact.life*PAND) + I(jobsat.ovrall*PAND) + 
                             I(satisfact.home*PAND) + I(home.bdrooms.pp*PAND) + 
                             I(urban*PAND) + I(innr.region*PAND) + I(dist.nearstcity*PAND) + I(urban*dist.nearstcity*PAND) + 
                             i(state, PAND, ref = "[1] NSW") + i(job.ind.1, PAND, ref = "[17] Health Care and Social Assistance") + 
                             i(job.occ.1, PAND, ref = "[2] Professionals") + 
                             
                             i(age.cat.5yr, POST, ref = "41-45") + I(unieduc*POST) + I(vocateduc*POST) + 
                             I(prtnr_any*POST) + I(prtnr_lfs.emp_all*POST) + I(prtnr_wfh.any_all*POST) + 
                             I(child.resid*POST) + I(child.pres*POST) + I(child.prim*POST) + I(child.high*POST) + 
                             I(disab*POST) + I(disab.lmtswk.severe*POST) + I(disab.type.cogni*POST) + I(disab.duration*POST) + 
                             I(wage.mn*POST) + I(lfs.emp.pt*POST) + I(job.casual*POST) + I(job.tenure*POST) + I(job.supervise*POST) + 
                             I(job.dist*POST) + I(DNscore_manual*POST) + I(satisfact.life*POST) + I(jobsat.ovrall*POST) + 
                             I(satisfact.home*POST) + I(home.bdrooms.pp*POST) + 
                             I(urban*POST) + I(innr.region*POST) + I(dist.nearstcity*POST) + I(urban*dist.nearstcity*POST) + 
                             i(state, POST, ref = "[1] NSW") + i(job.ind.1, POST, ref = "[17] Health Care and Social Assistance") + 
                             i(job.occ.1, POST, ref = "[2] Professionals")
                           | xwaveid, 
                           cluster = "xwaveid")
LPMmost_comb_indFE <- feols(data = df, wfh.most ~ PAND + POST +
                              i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc + 
                              prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all + 
                              child.resid + child.pres + child.prim + child.high + 
                              disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                              wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise + job.dist + 
                              DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                              urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                              state + i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                              i(job.occ.1, ref = "[2] Professionals") + 
                              
                              i(age.cat.5yr, PAND, ref = "41-45") + I(unieduc*PAND) + I(vocateduc*PAND) + 
                              I(prtnr_any*PAND) + I(prtnr_lfs.emp_all*PAND) + I(prtnr_wfh.any_all*PAND) + 
                              I(child.resid*PAND) + I(child.pres*PAND) + I(child.prim*PAND) + I(child.high*PAND) + 
                              I(disab*PAND) + I(disab.lmtswk.severe*PAND) + I(disab.type.cogni*PAND) + I(disab.duration*PAND) + 
                              I(wage.mn*PAND) + I(lfs.emp.pt*PAND) + I(job.casual*PAND) + I(job.tenure*PAND) + I(job.supervise*PAND) + 
                              I(job.dist*PAND) + I(DNscore_manual*PAND) + I(satisfact.life*PAND) + I(jobsat.ovrall*PAND) + 
                              I(satisfact.home*PAND) + I(home.bdrooms.pp*PAND) + 
                              I(urban*PAND) + I(innr.region*PAND) + I(dist.nearstcity*PAND) + I(urban*dist.nearstcity*PAND) + 
                              i(state, PAND, ref = "[1] NSW") + i(job.ind.1, PAND, ref = "[17] Health Care and Social Assistance") + 
                              i(job.occ.1, PAND, ref = "[2] Professionals") + 
                              
                              i(age.cat.5yr, POST, ref = "41-45") + I(unieduc*POST) + I(vocateduc*POST) + 
                              I(prtnr_any*POST) + I(prtnr_lfs.emp_all*POST) + I(prtnr_wfh.any_all*POST) + 
                              I(child.resid*POST) + I(child.pres*POST) + I(child.prim*POST) + I(child.high*POST) + 
                              I(disab*POST) + I(disab.lmtswk.severe*POST) + I(disab.type.cogni*POST) + I(disab.duration*POST) + 
                              I(wage.mn*POST) + I(lfs.emp.pt*POST) + I(job.casual*POST) + I(job.tenure*POST) + I(job.supervise*POST) + 
                              I(job.dist*POST) + I(DNscore_manual*POST) + I(satisfact.life*POST) + I(jobsat.ovrall*POST) + 
                              I(satisfact.home*POST) + I(home.bdrooms.pp*POST) + 
                              I(urban*POST) + I(innr.region*POST) + I(dist.nearstcity*POST) + I(urban*dist.nearstcity*POST) + 
                              i(state, POST, ref = "[1] NSW") + i(job.ind.1, POST, ref = "[17] Health Care and Social Assistance") + 
                              i(job.occ.1, POST, ref = "[2] Professionals")
                            | xwaveid, 
                            cluster = "xwaveid")

rows <- tribble(~"term", ~LPMany_comb_indFE, ~LPMmost_comb_indFE,
                "Dependent variable:","WFHany","WFHmost")
attr(rows, "position") <- 1
modelsummary(list(LPMany_comb_indFE, LPMmost_comb_indFE), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "regression outputs/combined LPM output - indiv FE.docx")

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
LOGanypre <- feglm(data = df %>% filter(PRE == 1), wfh.any ~ 
                     i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                     female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                     child.resid + child.pres + child.prim + child.high + 
                     disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                     wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                     DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                     urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                     i(state, ref = "[1] NSW") +
                     i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                     i(job.occ.1, ref = "[2] Professionals"), 
                   vcov = "hetero", family = "logit")
LOGanypand <- feglm(data = df %>% filter(PAND == 1), wfh.any ~ 
                      i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                      female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                      child.resid + child.pres + child.prim + child.high + 
                      disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                      wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                      DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                      urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                      i(state, ref = "[1] NSW") +
                      i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                      i(job.occ.1, ref = "[2] Professionals"), 
                    vcov = "hetero", family = "logit")
LOGanypost <- feglm(data = df %>% filter(POST == 1), wfh.any ~ 
                      i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                      female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                      child.resid + child.pres + child.prim + child.high + 
                      disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                      wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                      DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                      urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                      i(state, ref = "[1] NSW") +
                      i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                      i(job.occ.1, ref = "[2] Professionals"), 
                    vcov = "hetero", family = "logit")
# WFHmost
LOGmostpre <- feglm(data = df %>% filter(PRE == 1), wfh.most ~ 
                      i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                      female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                      child.resid + child.pres + child.prim + child.high + 
                      disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                      wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                      DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                      urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                      i(state, ref = "[1] NSW") +
                      i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                      i(job.occ.1, ref = "[2] Professionals"), 
                    vcov = "hetero", family = "logit")
LOGmostpand <- feglm(data = df %>% filter(PAND == 1), wfh.most ~ 
                       i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                       female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                       child.resid + child.pres + child.prim + child.high + 
                       disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                       wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                       DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                       urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                       i(state, ref = "[1] NSW") +
                       i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                       i(job.occ.1, ref = "[2] Professionals"), 
                     vcov = "hetero", family = "logit")
LOGmostpost <- feglm(data = df %>% filter(POST == 1), wfh.most ~ 
                       i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                       female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                       child.resid + child.pres + child.prim + child.high + 
                       disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                       wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                       DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                       urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                       i(state, ref = "[1] NSW") +
                       i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                       i(job.occ.1, ref = "[2] Professionals"), 
                     vcov = "hetero", family = "logit")
rows <- tribble(~"term", ~LOGanypre, ~LOGanypand, ~LOGanypost, ~LOGmostpre, ~LOGmostpand, ~LOGmostpost,
                "Dependent variable:","WFHany","WFHany","WFHany","WFHmost","WFHmost","WFHmost")
attr(rows, "position") <- 1
modelsummary(list(LOGanypre, LOGanypand, LOGanypost, LOGmostpre, LOGmostpand, LOGmostpost), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL, exponentiate = T,
             output = "regression outputs/separated LOGIT output - appendix.docx")



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
  filter(wfh.any > 0, PRE ==1) %>%
  drop_na(wfh.formalagree) %>%
  feols(wfh.prop ~ 
          wfh.formalagree + 
          i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
          female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
          child.resid + child.pres + child.prim + child.high + 
          disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
          wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
          DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
          urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
          i(state, ref = "[1] NSW") +
          i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
          i(job.occ.1, ref = "[2] Professionals"), 
        vcov = "hetero")
OLSsharepand <- df %>%
  filter(wfh.any > 0, PAND ==1) %>%
  drop_na(wfh.formalagree) %>%
  feols(wfh.prop ~ 
          wfh.formalagree + 
          i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
          female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
          child.resid + child.pres + child.prim + child.high + 
          disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
          wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
          DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
          urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
          i(state, ref = "[1] NSW") +
          i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
          i(job.occ.1, ref = "[2] Professionals"), 
        vcov = "hetero")
OLSsharepost <- df %>%
  filter(wfh.any > 0, POST ==1) %>%
  drop_na(wfh.formalagree) %>%
  feols(wfh.prop ~ 
          wfh.formalagree + 
          i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
          female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
          child.resid + child.pres + child.prim + child.high + 
          disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
          wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
          DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
          urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
          i(state, ref = "[1] NSW") +
          i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
          i(job.occ.1, ref = "[2] Professionals"), 
        vcov = "hetero")
modelsummary(list(OLSsharepre, OLSsharepand, OLSsharepost), 
             stars = T, gof_omit = "AIC|BIC|RMSE", statistic = NULL,
             output = "regression outputs/separated OLS output - WFHprop.docx")
df %>% 
  filter(wfh.any == 1) %>% 
  summarise(mean=mean(wfh.prop, na.rm = T), 
            sd=sd(wfh.prop, na.rm = T))


## Effect of firm size on WFH uptake
df %>% summarise(obs = n(),
                 no_firm.size = sum(is.na(emplyr.size))) %>%
  mutate(share_missing = no_firm.size/obs)

LPManypre_firmsize <- feols(data = df %>% filter(PRE == 1), wfh.any ~ i(emplyr.size, ref = 10) +
                              i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                              female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                              child.resid + child.pres + child.prim + child.high + 
                              disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                              wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                              DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                              urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                              i(state, ref = "[1] NSW") +
                              i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                              i(job.occ.1, ref = "[2] Professionals"), 
                            vcov = "hetero")
LPManypand_firmsize <- feols(data = df %>% filter(PAND == 1), wfh.any ~ i(emplyr.size, ref = 10) +
                               i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                               female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                               child.resid + child.pres + child.prim + child.high + 
                               disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                               wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                               DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                               urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                               i(state, ref = "[1] NSW") +
                               i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                               i(job.occ.1, ref = "[2] Professionals"), 
                             vcov = "hetero")
LPManypost_firmsize <- feols(data = df %>% filter(POST == 1), wfh.any ~ i(emplyr.size, ref = 10) +
                               i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                               female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                               child.resid + child.pres + child.prim + child.high + 
                               disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                               wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                               DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                               urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                               i(state, ref = "[1] NSW") +
                               i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                               i(job.occ.1, ref = "[2] Professionals"), 
                             vcov = "hetero")

LPMmostpre_firmsize <- feols(data = df %>% filter(PRE == 1), wfh.most ~ i(emplyr.size, ref = 10) +
                               i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                               female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                               child.resid + child.pres + child.prim + child.high + 
                               disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                               wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                               DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                               urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                               i(state, ref = "[1] NSW") +
                               i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                               i(job.occ.1, ref = "[2] Professionals"), 
                             vcov = "hetero")
LPMmostpand_firmsize <- feols(data = df %>% filter(PAND == 1), wfh.most ~ i(emplyr.size, ref = 10) +
                                i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                                female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                                child.resid + child.pres + child.prim + child.high + 
                                disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                                wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                                DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                                urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                                i(state, ref = "[1] NSW") +
                                i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                                i(job.occ.1, ref = "[2] Professionals"), 
                              vcov = "hetero")
LPMmostpost_firmsize <- feols(data = df %>% filter(POST == 1), wfh.most ~ i(emplyr.size, ref = 10) +
                                i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc +
                                female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all +
                                child.resid + child.pres + child.prim + child.high + 
                                disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                                wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise  + job.dist + 
                                DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                                urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                                i(state, ref = "[1] NSW") +
                                i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                                i(job.occ.1, ref = "[2] Professionals"), 
                              vcov = "hetero")

rows <- tribble(~"term", ~LPManypre_firmsize, ~LPManypand_firmsize, ~LPManypost_firmsize, ~LPMmostpre_firmsize, ~LPMmostpand_firmsize, ~LPMmostpost_firmsize,
                "Dependent variable:","WFHany","WFHany","WFHany","WFHmost","WFHmost","WFHmost",
                "Pandemic period:", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)")
attr(rows, "position") <- c(1,2)
label.emplyrsize <- df |> 
  select(emplyr.size) |>
  group_by(emplyr.size) |>
  slice_head(n = 1) |>
  mutate(emplyr.size = paste0("emplyr.size::", emplyr.size)) |>
  deframe()
label.firmsize <- tibble(emplyr.size = label.emplyrsize,
                         label = c(attr(attr(df$emplyr.size, "labels")[11:23], "names"), NA_character_)) |>
  mutate(label = str_remove(label, "^\\[[0-9]{2}\\]\\s*"),
         label = str_remove(label, "^\\[[0-9]{1}\\]\\s*"),
         label = str_remove(label, "Don.*but\\s*")) |>
  deframe()
modelsummary(list(LPManypre_firmsize, LPManypand_firmsize, LPManypost_firmsize, LPMmostpre_firmsize, LPMmostpand_firmsize, LPMmostpost_firmsize), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "regression outputs/separated LPM output - firm size robustness.docx",
             coef_map = label.firmsize)

