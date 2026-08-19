# =============================== #
#### Who still WFH? replication files ####
#### 2. Main results ####
#### Aaron Mollross ####
# =============================== #


#### MAIN RESULTS ####
### Baseline LPMs for WFHany
LPManypre <- feols(data = df %>% filter(PRE == 1), wfh.any ~ 
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
LPManypand <- feols(data = df %>% filter(PAND == 1), wfh.any ~ 
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
LPManypost <- feols(data = df %>% filter(POST == 1), wfh.any ~ 
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
modelsummary(list("Pre (2017-19)"=LPManypre, "During (2020-21)"=LPManypand, "Post (2022-24)"=LPManypost), 
             stars = T, gof_omit = "AIC|BIC|RMSE", 
             output = "WFHany LPM output - main.xlsx"
             )

### Baseline LPMs for WFHmost
LPMmostpre <- feols(data = df %>% filter(PRE == 1), wfh.most ~ 
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
LPMmostpand <- feols(data = df %>% filter(PAND == 1), wfh.most ~ 
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
LPMmostpost <- feols(data = df %>% filter(POST == 1), wfh.most ~ 
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
modelsummary(list("Pre (2017-19)"=LPMmostpre, "During (2020-21)"=LPMmostpand, "Post (2022-24)"=LPMmostpost), 
             stars = T, gof_omit = "AIC|BIC|RMSE", 
             output = "WFHmost LPM output - main.xlsx"
             )

# Main outputs
rows <- tribble(~"term", ~LPManypre, ~LPManypand, ~LPManypost, ~LPMmostpre, ~LPMmostpand, ~LPMmostpost,
                "Dependent variable:","WFHany","WFHany","WFHany","WFHmost","WFHmost","WFHmost",
                "Pandemic period:", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)", "Pre (2017-19)", "During (2020-21)", "Post(2022-24)")
attr(rows, "position") <- c(1,2)
modelsummary(list(LPManypre, LPManypand, LPManypost, LPMmostpre, LPMmostpand, LPMmostpost), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "separated LPM output - main.docx"
)


# Combined model for marginal effects
LPMany_comb <- feols(data = df, wfh.any ~ PAND + POST + 
                       i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc + 
                       female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all + 
                       child.resid + child.pres + child.prim + child.high + 
                       disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                       wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise + job.dist + 
                       DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                       urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                       state + i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                       i(job.occ.1, ref = "[2] Professionals") + 
                       
                       i(age.cat.5yr, PAND, ref = "41-45") + I(unieduc*PAND) + I(vocateduc*PAND) + 
                       I(female*PAND) + I(prtnr_any*PAND) + I(prtnr_lfs.emp_all*PAND) + I(prtnr_wfh.any_all*PAND) + 
                       I(child.resid*PAND) + I(child.pres*PAND) + I(child.prim*PAND) + I(child.high*PAND) + 
                       I(disab*PAND) + I(disab.lmtswk.severe*PAND) + I(disab.type.cogni*PAND) + I(disab.duration*PAND) + 
                       I(wage.mn*PAND) + I(lfs.emp.pt*PAND) + I(job.casual*PAND) + I(job.tenure*PAND) + I(job.supervise*PAND) + 
                       I(job.dist*PAND) + I(DNscore_manual*PAND) + I(satisfact.life*PAND) + I(jobsat.ovrall*PAND) + 
                       I(satisfact.home*PAND) + I(home.bdrooms.pp*PAND) + 
                       I(urban*PAND) + I(innr.region*PAND) + I(dist.nearstcity*PAND) + I(urban*dist.nearstcity*PAND) + 
                       i(state, PAND, ref = "[1] NSW") + i(job.ind.1, PAND, ref = "[17] Health Care and Social Assistance") + 
                       i(job.occ.1, PAND, ref = "[2] Professionals") + 
                       
                       i(age.cat.5yr, POST, ref = "41-45") + I(unieduc*POST) + I(vocateduc*POST) + 
                       I(female*POST) + I(prtnr_any*POST) + I(prtnr_lfs.emp_all*POST) + I(prtnr_wfh.any_all*POST) + 
                       I(child.resid*POST) + I(child.pres*POST) + I(child.prim*POST) + I(child.high*POST) + 
                       I(disab*POST) + I(disab.lmtswk.severe*POST) + I(disab.type.cogni*POST) + I(disab.duration*POST) + 
                       I(wage.mn*POST) + I(lfs.emp.pt*POST) + I(job.casual*POST) + I(job.tenure*POST) + I(job.supervise*POST) + 
                       I(job.dist*POST) + I(DNscore_manual*POST) + I(satisfact.life*POST) + I(jobsat.ovrall*POST) + 
                       I(satisfact.home*POST) + I(home.bdrooms.pp*POST) + 
                       I(urban*POST) + I(innr.region*POST) + I(dist.nearstcity*POST) + I(urban*dist.nearstcity*POST) + 
                       i(state, POST, ref = "[1] NSW") + i(job.ind.1, POST, ref = "[17] Health Care and Social Assistance") + 
                       i(job.occ.1, POST, ref = "[2] Professionals"), 
                       vcov = "hetero")
LPMmost_comb <- feols(data = df, wfh.most ~ PAND + POST + 
                        i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc + 
                        female + prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all + 
                        child.resid + child.pres + child.prim + child.high + 
                        disab + disab.lmtswk.severe + disab.type.cogni + disab.duration + 
                        wage.mn + lfs.emp.pt + job.casual + job.tenure + job.supervise + job.dist + 
                        DNscore_manual + satisfact.life + jobsat.ovrall + satisfact.home + home.bdrooms.pp + 
                        urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity) + 
                        state + i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                        i(job.occ.1, ref = "[2] Professionals") + 
                        
                        i(age.cat.5yr, PAND, ref = "41-45") + I(unieduc*PAND) + I(vocateduc*PAND) + 
                        I(female*PAND) + I(prtnr_any*PAND) + I(prtnr_lfs.emp_all*PAND) + I(prtnr_wfh.any_all*PAND) + 
                        I(child.resid*PAND) + I(child.pres*PAND) + I(child.prim*PAND) + I(child.high*PAND) + 
                        I(disab*PAND) + I(disab.lmtswk.severe*PAND) + I(disab.type.cogni*PAND) + I(disab.duration*PAND) + 
                        I(wage.mn*PAND) + I(lfs.emp.pt*PAND) + I(job.casual*PAND) + I(job.tenure*PAND) + I(job.supervise*PAND) + 
                        I(job.dist*PAND) + I(DNscore_manual*PAND) + I(satisfact.life*PAND) + I(jobsat.ovrall*PAND) + 
                        I(satisfact.home*PAND) + I(home.bdrooms.pp*PAND) + 
                        I(urban*PAND) + I(innr.region*PAND) + I(dist.nearstcity*PAND) + I(urban*dist.nearstcity*PAND) + 
                        i(state, PAND, ref = "[1] NSW") + i(job.ind.1, PAND, ref = "[17] Health Care and Social Assistance") + 
                        i(job.occ.1, PAND, ref = "[2] Professionals") + 
                        
                        i(age.cat.5yr, POST, ref = "41-45") + I(unieduc*POST) + I(vocateduc*POST) + 
                        I(female*POST) + I(prtnr_any*POST) + I(prtnr_lfs.emp_all*POST) + I(prtnr_wfh.any_all*POST) + 
                        I(child.resid*POST) + I(child.pres*POST) + I(child.prim*POST) + I(child.high*POST) + 
                        I(disab*POST) + I(disab.lmtswk.severe*POST) + I(disab.type.cogni*POST) + I(disab.duration*POST) + 
                        I(wage.mn*POST) + I(lfs.emp.pt*POST) + I(job.casual*POST) + I(job.tenure*POST) + I(job.supervise*POST) + 
                        I(job.dist*POST) + I(DNscore_manual*POST) + I(satisfact.life*POST) + I(jobsat.ovrall*POST) + 
                        I(satisfact.home*POST) + I(home.bdrooms.pp*POST) + 
                        I(urban*POST) + I(innr.region*POST) + I(dist.nearstcity*POST) + I(urban*dist.nearstcity*POST) + 
                        i(state, POST, ref = "[1] NSW") + i(job.ind.1, POST, ref = "[17] Health Care and Social Assistance") + 
                        i(job.occ.1, POST, ref = "[2] Professionals"), 
                      vcov = "hetero")

rows <- tribble(~"term", ~LPMany_comb, ~LPMmost_comb,
                "Dependent variable:","WFHany","WFHmost")
attr(rows, "position") <- 1
modelsummary(list(LPMany_comb, LPMmost_comb), 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "combined LPM output.docx"
)

# Extra stats for interpretation
wage.sd <- df %>% summarise(sd_wage = sd(wage.mn, na.rm = T)) #standard deviation of wages for economic significance 
wage.coef <- coef(LPManypost)["wage.mn"]
wage.sd*wage.coef
df %>%
  group_by(job.occ.1) %>%
  summarise(
    n = n(),
    n_occ4 = n_distinct(job.occ),
    n_vals = n_distinct(DNscore_manual),
    has_01 = all(c(0,1) %in% unique(DNscore_manual))
  ) %>%
  summarise(
    share_groups_with_2plus_vals = mean(n_vals >= 2),
    share_groups_with_01 = mean(has_01, na.rm = TRUE),
    median_n_occ4 = median(n_occ4))





#### COEFFICIENT PLOTS (APPENDIX A) ####
## Age
label.age <- c("age.cat.5yr::< 21"="Under 21",
               "age.cat.5yr::21-25"="21-25",
               "age.cat.5yr::26-30"="26-30",
               "age.cat.5yr::31-35"="31-35",
               "age.cat.5yr::36-40"="36-40",
               "age.cat.5yr::46-50"="46-50",
               "age.cat.5yr::51-55"="51-55",
               "age.cat.5yr::56-60"="56-60",
               "age.cat.5yr::61-65"="61-65",
               "age.cat.5yr::> 65"="Over 65")
brewer.pal(9, "Greens")
modelplot(list("After (2022-24)"=LPManypost, "During (2020-21)"=LPManypand,
               "Before (2017-19)"=LPManypre),
          coef_map = label.age) +
  labs(title = "WFHany", y = "Age category",
       colour = "Pandemic period", x = "Coeff. estimates (with 95% CI)") +
  geom_vline(xintercept = 0, colour = "black") +
  scale_colour_manual(values = c("#74C476","#238B45","#00441B")) +
  theme_classic() +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "bottom",  text = element_text(size = 7.5),
        axis.text = element_text(colour = "black"),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey")) +
  scale_x_continuous(limits = c(-0.11,0.06), breaks = seq(-0.2,0.2, by = 0.05)) +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHany_age_main_outputs.png"), width=9, height=12, units="cm")
brewer.pal(9, "Purples")
modelplot(list("After (2022-24)"=LPMmostpost, "During (2020-21)"=LPMmostpand,
               "Before (2017-19)"=LPMmostpre), 
          coef_map = label.age) +
  labs(title = "WFHmost", y = "Age category",
       colour = "Pandemic period", x = "Coeff. estimates (with 95% CI)") +
  geom_vline(xintercept = 0, colour = "black") +
  scale_colour_manual(values = c("#9E9AC8", "#6A51A3FF","#3F007DFF")) +
  theme_classic() +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "bottom", text = element_text(size = 7.5), 
        axis.text = element_text(colour = "black"), legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(-0.1,0.06), breaks = seq(-0.2,0.2, by = 0.05)) +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHmost_age_main_outputs.png"), width=7, height=12, units="cm")

## Occupation fixed effects 
label.occup <- LPManypre |> 
  tidy(conf.int = TRUE) |>                 
  filter(str_detect(term, "^job.occ.1")) |>
  select(term) |>
  mutate(occup = str_remove(term, "^job.occ.1::")) |>
  deframe()
modelplot(list("After (2022-24)"=LPManypost, "During (2020-21)"=LPManypand,
               "Before (2017-19)"=LPManypre),
          coef_map = label.occup) +
  labs(y = "Occupation", 
       x = "Coeff. estimates (with 95% CI)",
       colour = "Pandemic period",
       title = "WFHany") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.2, 0.1), breaks = seq(-1,1, by = 0.1)) +
  scale_colour_manual(values = c("#74C476","#238B45","#00441B")) +
  theme_classic() +
  theme(legend.position = "bottom", text = element_text(size = 7.5), 
        axis.text = element_text(colour = "black"), 
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        axis.title.y = element_blank()) +
  geom_vline(xintercept = 0, colour = "black") +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHany_occupFE_LPMoutputs.png"), width=10.5, height=9.5, units="cm")
modelplot(list("After (2022-24)"=LPMmostpost, "During (2020-21)"=LPMmostpand,
               "Before (2017-19)"=LPMmostpre),
          coef_map = label.occup) +
  labs(y = "Industry", 
       x = "Coeff. estimates (with 95% CI)",
       colour = "Pandemic period",
       title = "WFHmost") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.2, 0.1), breaks = seq(-1,1, by = 0.1)) +
  scale_colour_manual(values = c("#9E9AC8", "#6A51A3FF","#3F007DFF")) +
  theme_classic() +
  theme(legend.position = "bottom", text = element_text(size = 7.5), 
        axis.text = element_text(colour = "black"), legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  geom_vline(xintercept = 0, colour = "black") +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHmost_occupFE_LPMoutputs.png"), width=5.5, height=9.5, units="cm")

## Industry fixed effects
label.industry <- LPManypre |> 
  tidy(conf.int = TRUE) |>                 
  filter(str_detect(term, "^job.ind.1")) |>
  select(term) |>
  mutate(industry = str_remove(term, "^job.ind.1::")) |>
  deframe()
modelplot(list("After (2022-24)"=LPManypost, "During (2020-21)"=LPManypand,
               "Before (2017-19)"=LPManypre),
          coef_map = label.industry) +
  labs(y = "Industry", 
       x = "Coeff. estimates (with 95% CI)",
       colour = "Pandemic period",
       title = "WFHany") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.15, 0.4), breaks = seq(-1,1, by = 0.1)) +
  scale_colour_manual(values = c("#74C476","#238B45","#00441B")) +
  theme_classic() +
  theme(legend.position = "bottom", text = element_text(size = 8), 
        axis.text = element_text(colour = "black"), 
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        axis.title.y = element_blank()) +
  geom_vline(xintercept = 0, colour = "black") +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHany_industFE_LPMoutputs.png"), width=10.5, height=16, units="cm")
modelplot(list("After (2022-24)"=LPMmostpost, "During (2020-21)"=LPMmostpand,
               "Before (2017-19)"=LPMmostpre),
          coef_map = label.industry) +
  labs(y = "Industry", 
       x = "Coeff. estimates (with 95% CI)",
       colour = "Pandemic period",
       title = "WFHmost") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.15, 0.4), breaks = seq(-1,1, by = 0.1)) +
  scale_colour_manual(values = c("#9E9AC8", "#6A51A3FF","#3F007DFF")) +
  theme_classic() +
  theme(legend.position = "bottom", text = element_text(size = 8), 
        axis.text = element_text(colour = "black"), legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  geom_vline(xintercept = 0, colour = "black") +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHmost_industFE_LPMoutputs.png"), width=5.5, height=16, units="cm")

## State
label.state <- c("state::[2] VIC"="VIC",
                 "state::[3] QLD"="QLD",
                 "state::[4] SA"="SA",
                 "state::[5] WA"="WA",
                 "state::[6] TAS"="TAS",
                 "state::[7] NT"="NT",
                 "state::[8] ACT"="ACT")
modelplot(list("After (2022-24)"=LPManypost, "During (2020-21)"=LPManypand,
               "Before (2017-19)"=LPManypre),
          coef_map = label.state) +
  labs(title = "WFHany", y = "State",  
       colour = "Pandemic period", x = "Coeff. estimates (with 95% CI)") +
  geom_vline(xintercept = 0, colour = "black") +
  scale_colour_manual(values = c("#74C476","#238B45","#00441B")) +
  theme_classic() +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "bottom",  text = element_text(size = 8),
        axis.text = element_text(colour = "black"),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        axis.title.y = element_blank()) +
  scale_x_continuous(breaks = seq(-0.4,0.2, by = 0.1)) +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHany_state_outputs.png"), width=8.5, height=9.5, units="cm")
modelplot(list("After (2022-24)"=LPMmostpost, "During (2020-21)"=LPMmostpand,
               "Before (2017-19)"=LPMmostpre), 
          coef_map = label.state) +
  labs(title = "WFHmost", y = "State",  
       colour = "Pandemic period", x = "Coeff. estimates (with 95% CI)") +
  geom_vline(xintercept = 0, colour = "black") +
  scale_colour_manual(values = c("#9E9AC8", "#6A51A3FF","#3F007DFF")) +
  theme_classic() +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "bottom", text = element_text(size = 8), 
        axis.text = element_text(colour = "black"), legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  scale_x_continuous(breaks = seq(-0.4,0.2, by = 0.1)) +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHmost_state_outputs.png"), width=7.5, height=9.5, units="cm")


