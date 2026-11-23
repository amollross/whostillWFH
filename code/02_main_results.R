# =============================== #
#### Who still WFH? replication files ####
#### 2. Main results ####
#### Aaron Mollross ####
# =============================== #

### Covariate set-up
setFixest_fml(..main_controls = ~ i(age.cat.5yr, ref = "41-45") + unieduc + vocateduc + female + 
                prtnr_any + child.resid + child.pres + child.prim + child.high + 
                disab + disab.lmtswk.scale + disab.type.cogni + disab.duration + 
                DNscore_manual + i(state, ref = "[1] NSW") +
                i(job.ind.1, ref = "[17] Health Care and Social Assistance") + 
                i(job.occ.1, ref = "[2] Professionals"))
setFixest_fml(..main_controls_PAND = ~ i(age.cat.5yr, PAND, ref = "41-45") + I(unieduc*PAND) + I(vocateduc*PAND) + 
                I(female*PAND) + I(prtnr_any*PAND) + 
                I(child.resid*PAND) + I(child.pres*PAND) + I(child.prim*PAND) + I(child.high*PAND) + 
                I(disab*PAND) + I(disab.lmtswk.severe*PAND) + I(disab.type.cogni*PAND) + I(disab.duration*PAND) + 
                I(DNscore_manual*PAND) + i(state, PAND, ref = "[1] NSW") + 
                i(job.ind.1, PAND, ref = "[17] Health Care and Social Assistance") + 
                i(job.occ.1, PAND, ref = "[2] Professionals"))
setFixest_fml(..main_controls_POST = ~ i(age.cat.5yr, POST, ref = "41-45") + I(unieduc*POST) + I(vocateduc*POST) + 
                I(female*POST) + I(prtnr_any*POST) + 
                I(child.resid*POST) + I(child.pres*POST) + I(child.prim*POST) + I(child.high*POST) + 
                I(disab*POST) + I(disab.lmtswk.severe*POST) + I(disab.type.cogni*POST) + I(disab.duration*POST) + 
                I(DNscore_manual*POST) + i(state, POST, ref = "[1] NSW") + 
                i(job.ind.1, POST, ref = "[17] Health Care and Social Assistance") + 
                i(job.occ.1, POST, ref = "[2] Professionals"))
setFixest_fml(..geog_controls = ~ urban + innr.region + dist.nearstcity + I(urban*dist.nearstcity))
setFixest_fml(..employ_controls = ~ job.tenure + job.supervise)


#### MAIN RESULTS ####
### Baseline LPMs for WFHany
LPManypre <- feols(data = df %>% filter(PRE == 1), wfh.any ~ ..main_controls, 
                   cluster = "xwaveid")
LPManypand <- feols(data = df %>% filter(PAND == 1), wfh.any ~ ..main_controls,
                    cluster = "xwaveid")
LPManypost <- feols(data = df %>% filter(POST == 1), wfh.any ~ ..main_controls,
                    cluster = "xwaveid")
modelsummary(list("Pre (2017-19)"=LPManypre, "During (2020-21)"=LPManypand, "Post (2022-24)"=LPManypost), 
             stars = T, gof_omit = "AIC|BIC|RMSE", 
             output = "WFHany LPM output - separated.xlsx")

# Combined model for marginal effects
LPMany_comb <- feols(data = df, wfh.any ~ PAND + POST + ..main_controls + ..main_controls_PAND + ..main_controls_POST, 
                       cluster = "xwaveid")
modelsummary(LPMany_comb, 
             stars = T, gof_omit = "R2 Within|AIC|BIC|RMSE", add_rows = rows, statistic = NULL,
             output = "WFHany LPM output - combined.docx")


#### MAIN COEFFICIENT PLOTS ####
## State
label.state <- c("state::[2] VIC"="Victoria",
                 "state::[3] QLD"="Queensland",
                 "state::[4] SA"="South Ausralia",
                 "state::[5] WA"="Western Australia",
                 "state::[6] TAS"="Tasmania",
                 "state::[7] NT"="Northern Territory",
                 "state::[8] ACT"="Australian Capital Territory")
modelplot(list("After (2022-24)"=LPManypost, "During (2020-21)"=LPManypand,
               "Before (2017-19)"=LPManypre),
          coef_map = label.state) +
  labs(#title = "WFHany", 
    y = "State", colour = "Pandemic period", x = "Coeff. estimates (with 95% CI)") +
  geom_vline(xintercept = 0, colour = "black") +
  scale_colour_manual(values = c("#74C476","#238B45","#00441B")) +
  theme_classic() +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "right",  text = element_text(size = 8),
        axis.text = element_text(colour = "black"),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        #axis.title.y = element_blank()
  ) +
  scale_x_continuous(breaks = seq(-0.4,0.2, by = 0.05)) +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHany_state_outputs.png"), width=16, height=7, units="cm")

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
       colour = "Pandemic period") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.25, 0.1), breaks = seq(-1,1, by = 0.05)) +
  scale_colour_manual(values = c("#74C476","#238B45","#00441B")) +
  theme_classic() +
  theme(legend.position = "right", text = element_text(size = 8), 
        axis.text = element_text(colour = "black"), 
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        #axis.title.y = element_blank()
        ) +
  geom_vline(xintercept = 0, colour = "black") +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHany_occupFE_LPMoutputs.png"), width=16, height=7, units="cm")

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
       colour = "Pandemic period") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.15, 0.45), breaks = seq(-1,1, by = 0.1)) +
  scale_colour_manual(values = c("#74C476","#238B45","#00441B")) +
  theme_classic() +
  theme(legend.position = "right", text = element_text(size = 8), 
        axis.text = element_text(colour = "black"), 
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        #axis.title.y = element_blank()
        ) +
  geom_vline(xintercept = 0, colour = "black") +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHany_industFE_LPMoutputs.png"), width=16, height=12, units="cm")
