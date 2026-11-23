# =============================== #
#### Who still WFH? replication files ####
#### 4. Charts and descritpive statistics ####
#### Aaron Mollross ####
# =============================== #


#### SUPPORTING MATERIALS - APPENDIX A ####

# Occupation teleworkability v WFH uptake scatter plot
occup.scatter <- df %>%
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic (2017-2019)" = "Pre-pandemic period",
                                   "Pandemic (2020-2021)" = "Pandemic period",
                                   "Post-pandemic (2022-2024)" = "Post-pandemic period")) %>%
  group_by(job.occ, pandemic.cat) %>%
  summarise(share.WFHany = 100*mean(wfh.any),
            share.WFHrate = 100*mean(wfh.prop),
            telework = mean(DNscore_manual))
r2_df <- occup.scatter %>%
  filter(pandemic.cat != "Pandemic (2020-2021)") %>%
  group_by(pandemic.cat) %>%
  do(glance(lm(share.WFHany ~ telework, data = .))) %>%
  select(pandemic.cat, r.squared) %>%
  ungroup() %>%
  rename(r2 = r.squared) %>%
  mutate(
    x = c(0.55, 0.3),
    y = c(20, 22),
    label = paste0("R² = ", round(r2, 3)))
ggplot(occup.scatter %>% filter(pandemic.cat != "Pandemic (2020-2021)")) +
  geom_point(aes(y = share.WFHany, x = telework, colour = pandemic.cat)) +
  geom_smooth(aes(y = share.WFHany, x = telework), colour = "black",
              formula = y~poly(x,1), method = "glm", se = F) +
  labs(x = "Teleworkability scores",
       y = "Share with any hours of WFH (%)") +
  facet_grid(cols = vars(pandemic.cat)) +
  geom_text(data = r2_df, aes(x = x, y = y, label = label), 
            size = 2.8, fontface = "bold", hjust = 0) +
  scale_colour_manual(values = c("#6BAED6FF", "#08306BFF")) +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"), text = element_text(size =9),
        legend.position = "none") 
ggsave(paste0(chart_output, "occup_WFH_x_DNscore.png"), width = 16, height = 9.8, units = "cm")

# Age coefficients
label.age <- c("age.cat.5yr::< 21"="Under 21",
               "age.cat.5yr::21-25"="21-25",
               "age.cat.5yr::26-30"="26-30",
               "age.cat.5yr::31-35"="31-35",
               "age.cat.5yr::36-40"="36-40",
               "age.cat.5yr::46-50"="46-50",
               "age.cat.5yr::51-55"="51-55",
               "age.cat.5yr::56-60"="56-60",
               "age.cat.5yr::61-65"="61-64")
brewer.pal(9, "Greens")
modelplot(list("After (2022-24)"=LPManypost, "During (2020-21)"=LPManypand,
               "Before (2017-19)"=LPManypre),
          coef_map = label.age) +
  labs(y = "Age category", colour = "Pandemic period", x = "Coeff. estimates (with 95% CI)") +
  geom_vline(xintercept = 0, colour = "black") +
  scale_colour_manual(values = c("#74C476","#238B45","#00441B")) +
  theme_classic() +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "right",  text = element_text(size = 8),
        axis.text = element_text(colour = "black"),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey")) +
  scale_x_continuous(limits = c(-0.15,0.06), breaks = seq(-0.2,0.2, by = 0.05)) +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHany_age_main_outputs.png"), width=16, height=8, units="cm")
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

# State (WFHmost)
label.state <- c("state::[2] VIC"="Victoria",
                 "state::[3] QLD"="Queensland",
                 "state::[4] SA"="South Ausralia",
                 "state::[5] WA"="Western Australia",
                 "state::[6] TAS"="Tasmania",
                 "state::[7] NT"="Northern Territory",
                 "state::[8] ACT"="Australian Capital Territory")
modelplot(list("After (2022-24)"=LPMmostpost, "During (2020-21)"=LPMmostpand,
               "Before (2017-19)"=LPMmostpre), 
          coef_map = label.state) +
  labs(#title = "WFHmost", 
    y = "State", colour = "Pandemic period", x = "Coeff. estimates (with 95% CI)") +
  geom_vline(xintercept = 0, colour = "black") +
  scale_colour_manual(values = c("#9E9AC8", "#6A51A3FF","#3F007DFF")) +
  theme_classic() +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "right", text = element_text(size = 8), 
        axis.text = element_text(colour = "black"), #legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        #axis.title.y = element_blank(), #axis.text.y = element_blank()
  ) +
  scale_x_continuous(breaks = seq(-0.4,0.2, by = 0.05)) +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHmost_state_outputs.png"), width=16, height=7, units="cm")

# Occupation fixed effects (WFHmost)
label.occup <- LPManypre |> 
  tidy(conf.int = TRUE) |>                 
  filter(str_detect(term, "^job.occ.1")) |>
  select(term) |>
  mutate(occup = str_remove(term, "^job.occ.1::")) |>
  deframe()
modelplot(list("After (2022-24)"=LPMmostpost, "During (2020-21)"=LPMmostpand,
               "Before (2017-19)"=LPMmostpre),
          coef_map = label.occup) +
  labs(y = "Occupation", 
       x = "Coeff. estimates (with 95% CI)",
       colour = "Pandemic period") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.12, 0.07), breaks = seq(-1,1, by = 0.05)) +
  scale_colour_manual(values = c("#9E9AC8", "#6A51A3FF","#3F007DFF")) +
  theme_classic() +
  theme(legend.position = "right", text = element_text(size = 8), 
        axis.text = element_text(colour = "black"), #legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        #axis.title.y = element_blank(), axis.text.y = element_blank()
        ) +
  geom_vline(xintercept = 0, colour = "black") +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHmost_occupFE_LPMoutputs.png"), width=16, height=7, units="cm")

# Industry fixed effects (WFHmost)
label.industry <- LPManypre |> 
  tidy(conf.int = TRUE) |>                 
  filter(str_detect(term, "^job.ind.1")) |>
  select(term) |>
  mutate(industry = str_remove(term, "^job.ind.1::")) |>
  deframe()
modelplot(list("After (2022-24)"=LPMmostpost, "During (2020-21)"=LPMmostpand,
               "Before (2017-19)"=LPMmostpre),
          coef_map = label.industry) +
  labs(y = "Industry", 
       x = "Coeff. estimates (with 95% CI)",
       colour = "Pandemic period") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.15, 0.45), breaks = seq(-1,1, by = 0.1)) +
  scale_colour_manual(values = c("#9E9AC8", "#6A51A3FF","#3F007DFF")) +
  theme_classic() +
  theme(legend.position = "right", text = element_text(size = 8), 
        axis.text = element_text(colour = "black"), #legend.title = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey"),
        #axis.title.y = element_blank(), axis.text.y = element_blank()
        ) +
  geom_vline(xintercept = 0, colour = "black") +
  guides(colour = guide_legend(nrow = 3, reverse = T))
ggsave(paste0(chart_output, "WFHmost_industFE_LPMoutputs.png"), width=16, height=12, units="cm")



#### DESCRIPTIVE STATISTICS - APPENDIX B ####

# Summary of means by WFHany status and pandemic period
summ.stats.WFHany.pandemic <- df %>%
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic (2017-2019)" = "Pre-pandemic period",
                                   "Pandemic (2020-2021)" = "Pandemic period",
                                   "Post-pandemic (2022-2024)" = "Post-pandemic period")) %>%
  group_by(pandemic.cat, wfh.any) %>%
  summarise(obs = n(),
            wfh.prop = mean(wfh.prop, na.rm = T),
            usl.hrs = mean(job.uslhrs, na.rm = T),
            age = mean(age, na.rm = T),
            uni.educ = mean(unieduc, na.rm = T),
            female = mean(female, na.rm = T),
            prtnr = mean(prtnr_any, na.rm = T),
            prtnr.employ = mean(prtnr_lfs.emp_all, na.rm = T),
            prtnr.wfh = mean(prtnr_wfh.any_all, na.rm = T),
            child.resid = mean(child.resid, na.rm = T),
            child.yngst = mean(child.yngst, na.rm = T),
            disab = mean(disab, na.rm = T),
            disab.sever = mean(disab.lmtswk.severe, na.rm = T),
            cogni.disab = mean(disab.type.cogni, na.rm = T),
            wage = mean(wage.mn, na.rm = T),
            earnings = mean(earns.tot, na.rm = T),
            parttime = mean(lfs.emp.pt, na.rm = T),
            job.tenure = mean(job.tenure, na.rm = T),
            job.supervise = mean(job.supervise, na.rm = T),
            job.distance = mean(job.dist, na.rm = T),
            telework = mean(DNscore_manual),
            bedrooms = mean(home.bdrooms, na.rm = T),
            urban = mean(urban, na.rm = T),
            inner.regional = mean(innr.region, na.rm = T),
            dist.city = mean(dist.nearstcity, na.rm = T),
            .groups = "drop")%>%
  ungroup() %>%
  group_by(pandemic.cat) %>%
  mutate(sum.obs = sum(obs)) %>%
  ungroup() %>%
  mutate(share = obs/sum.obs)
temp <- summ.stats.WFHany.pandemic %>% 
  pivot_longer(cols = -c(pandemic.cat, wfh.any), names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = c(pandemic.cat, wfh.any), values_from = Value, names_sep = ", ")
write.csv(temp, "summ stats by WFHany + pand period.csv", row.names = F)
df %>% count(wfh.any) # counts by WFH status
df %>% summarise(sum.any = sum(wfh.any == 1),
                 sum = n()) %>%
  mutate(share.any = 100*sum.any/sum,
         share.none = 100-share.any)
df %>% group_by(pandemic.cat) %>% summarise(mean = 100*mean(wfh.any, na.rm = T)) # share with WFH hours
df %>% filter(wfh.any == 1) %>% summarise(mean = mean(wfh.prop, na.rm = T)) # mean prop of hours WFH, conditional on WFH

# Summary of means by WFHmost status and pandemic period
summ.stats.WFHmost.pandemic <- df %>%
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic (2017-2019)" = "Pre-pandemic period",
                                   "Pandemic (2020-2021)" = "Pandemic period",
                                   "Post-pandemic (2022-2024)" = "Post-pandemic period")) %>%
  group_by(pandemic.cat, wfh.most) %>%
  summarise(obs = n(),
            wfh.prop = mean(wfh.prop, na.rm = T),
            usl.hrs = mean(job.uslhrs, na.rm = T),
            age = mean(age, na.rm = T),
            uni.educ = mean(unieduc, na.rm = T),
            female = mean(female, na.rm = T),
            prtnr = mean(prtnr_any, na.rm = T),
            prtnr.employ = mean(prtnr_lfs.emp_all, na.rm = T),
            prtnr.wfh = mean(prtnr_wfh.any_all, na.rm = T),
            child.resid = mean(child.resid, na.rm = T),
            child.yngst = mean(child.yngst, na.rm = T),
            disab = mean(disab, na.rm = T),
            disab.sever = mean(disab.lmtswk.severe, na.rm = T),
            cogni.disab = mean(disab.type.cogni, na.rm = T),
            wage = mean(wage.mn, na.rm = T),
            earnings = mean(earns.tot, na.rm = T),
            parttime = mean(lfs.emp.pt, na.rm = T),
            job.tenure = mean(job.tenure, na.rm = T),
            job.supervise = mean(job.supervise, na.rm = T),
            job.distance = mean(job.dist, na.rm = T),
            telework = mean(DNscore_manual),
            bedrooms = mean(home.bdrooms, na.rm = T),
            urban = mean(urban, na.rm = T),
            inner.regional = mean(innr.region, na.rm = T),
            dist.city = mean(dist.nearstcity, na.rm = T),
            .groups = "drop")%>%
  ungroup() %>%
  group_by(pandemic.cat) %>%
  mutate(sum.obs = sum(obs)) %>%
  ungroup() %>%
  mutate(share = obs/sum.obs)
temp <- summ.stats.WFHmost.pandemic %>% 
  pivot_longer(-c(pandemic.cat, wfh.most), names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = c(pandemic.cat, wfh.most), values_from = Value, names_sep = ", ")
write.csv(temp, "summ stats by WFHmost + pand period.csv", row.names = F)
df %>% group_by(pandemic.cat) %>% summarise(mean = 100*mean(wfh.most, na.rm = T)) # share with majority WFH hours
df %>% group_by(pandemic.cat) %>% summarise(fully_remote = 100*mean(wfh.prop == 1, na.rm = T)) # share with 100% WFH
df %>% filter(wfh.any == 1) %>% group_by(pandemic.cat) %>% 
  summarise(fully_remote = 100*mean(wfh.prop == 1, na.rm = T)) # share with 100% WFH as proportion of home-workers

# Summary of averages against demographic and job details, split by any WFH and pandemic category
summ.stats.WFHmost.year <- df %>%
  group_by(year, wfh.most.cat) %>%
  summarise(n.obs = n(),
            .groups = "drop")%>%
  ungroup() %>%
  group_by(year) %>%
  mutate(sum.obs = sum(n.obs)) %>%
  ungroup() %>%
  mutate(share = n.obs/sum.obs)
# Comparison of WFHany shares
ggplot(summ.stats.WFHmost.year) +
  geom_col(aes(y = 100*share, x = year, fill = as.factor(wfh.most.cat)), 
           position = position_stack()) +
  scale_fill_manual(values = c("#6BAED6FF", "#2171B5FF","#08306BFF")) +
  labs(y = "Share of observations (%)", fill = "", x = "Year") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2017,2024, by = 1)) +
  theme_classic() +
  theme(axis.title.x = element_blank(), legend.position = "bottom",
        axis.text = element_text(colour = "black"),text = element_text(size = 8))
ggsave(paste0(chart_output, "WFHmost_year.png"), width = 16, height = 10, units = "cm")

# Density plot of WFH rates
df %>% filter(wfh.any == 1) %>%
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic (2017-2019)" = "Pre-pandemic period",
                                   "Pandemic (2020-2021)" = "Pandemic period",
                                   "Post-pandemic (2022-2024)" = "Post-pandemic period")) %>%
  ggplot() +
  geom_histogram(aes(x=100*wfh.prop), linewidth = 1, fill = "#08306BFF", bins = 15) +
  facet_grid(cols = vars(pandemic.cat)) +
  theme_bw() +
  labs(x = "Usual WFH hours as share of total hours worked (%)", y = "Number of observations") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text = element_text(colour = "black"),text = element_text(size = 8))
ggsave(paste0(chart_output, "WFHdensity_pandemicera.png"), width = 16, height = 10, units = "cm")
df %>% count(wfh.prop ==1, pandemic.cat) #basic summaries of distribution for fully remote

### Part-time summary stats ###
summ.WFH.pandemic <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         part.time.cat = factor(lfs.emp.pt, labels = c("Full-time", "Part-time"))) %>%
  group_by(pandemic.cat, lfs.emp.pt, part.time.cat) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any==1, na.rm=T),
            se.wfh.any = sd(wfh.any==1, na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(summ.WFH.pandemic) + # Chart prop WFHany by lfs.emp.pt
  geom_col(aes(y=100*p.wfh.any, x=pandemic.cat, fill=part.time.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.any-1.96*se.wfh.any), ymax = 100*(p.wfh.any+1.96*se.wfh.any),
                    colour = part.time.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="", fill = "") +
  scale_fill_manual(values = c("#238B45FF","#00441BFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,45), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "parttime_pandemicera_wfhany.png"), width = 7.9, height = 7, units = "cm")
ggplot(summ.WFH.pandemic) + # Chart prop WFHmost by lfs.emp.pt
  geom_col(aes(y=100*p.wfh.most, x=pandemic.cat, fill=part.time.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.most-1.96*se.wfh.most), ymax = 100*(p.wfh.most+1.96*se.wfh.most),
                    colour = part.time.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with majority WFH hours (%)", x="", fill = "") +
  scale_fill_manual(values = c("#6A51A3FF","#3F007DFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "parttime_pandemicera_wfhmost.png"), width = 7.9, height = 7, units = "cm")

### Casual summary stats ###
summ.WFH.pandemic <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         casual.cat = factor(job.casual, labels = c("Permanent", "Casual"))) %>%
  group_by(pandemic.cat, job.casual, casual.cat) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any==1, na.rm=T),
            se.wfh.any = sd(wfh.any==1, na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop") %>%
  filter(!is.na(job.casual))
ggplot(summ.WFH.pandemic) + # Chart prop WFHany by job.casual
  geom_col(aes(y=100*p.wfh.any, x=pandemic.cat, fill=casual.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.any-1.96*se.wfh.any), ymax = 100*(p.wfh.any+1.96*se.wfh.any),
                    colour = casual.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="", fill = "") +
  scale_fill_manual(values = c("#238B45FF","#00441BFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,45), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "casual_pandemicera_wfhany.png"), width = 7.9, height = 7, units = "cm")
ggplot(summ.WFH.pandemic) + # Chart prop WFHmost by job.casual
  geom_col(aes(y=100*p.wfh.most, x=pandemic.cat, fill=casual.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.most-1.96*se.wfh.most), ymax = 100*(p.wfh.most+1.96*se.wfh.most),
                    colour = casual.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with majority WFH hours (%)", x="", fill = "") +
  scale_fill_manual(values = c("#6A51A3FF","#3F007DFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "casual_pandemicera_wfhmost.png"), width = 7.9, height = 7, units = "cm")

### Supervisor summary stats ###
summ.WFH.pandemic <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         job.supervise.cat = factor(job.supervise, labels = c("Non-supervisor", "Supervisor"))) %>%
  group_by(pandemic.cat, job.supervise, job.supervise.cat) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any==1, na.rm=T),
            se.wfh.any = sd(wfh.any==1, na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(summ.WFH.pandemic) + # Chart prop WFHany by job.supervise
  geom_col(aes(y=100*p.wfh.any, x=pandemic.cat, fill=job.supervise.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.any-1.96*se.wfh.any), ymax = 100*(p.wfh.any+1.96*se.wfh.any),
                    colour = job.supervise.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="", fill = "") +
  scale_fill_manual(values = c("#238B45FF","#00441BFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,45), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "superv_pandemicera_wfhany.png"), width = 7.9, height = 7, units = "cm")
ggplot(summ.WFH.pandemic) + # Chart prop WFHmost by job.supervise
  geom_col(aes(y=100*p.wfh.most, x=pandemic.cat, fill=job.supervise.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.most-1.96*se.wfh.most), ymax = 100*(p.wfh.most+1.96*se.wfh.most),
                    colour = job.supervise.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with majority WFH hours (%)", x="", fill = "") +
  scale_fill_manual(values = c("#6A51A3FF","#3F007DFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "superv_pandemicera_wfhmost.png"), width = 7.9, height = 7, units = "cm")

### Urban summary stats ###
summ.WFH.pandemic <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         urban.cat = factor(urban, labels = c("Regional/remote resident", "Major city resident"))) %>%
  group_by(pandemic.cat, urban, urban.cat) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any==1, na.rm=T),
            se.wfh.any = sd(wfh.any==1, na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(summ.WFH.pandemic) + # Chart prop WFHany by urban
  geom_col(aes(y=100*p.wfh.any, x=pandemic.cat, fill=urban.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.any-1.96*se.wfh.any), ymax = 100*(p.wfh.any+1.96*se.wfh.any),
                    colour = urban.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="", fill = "") +
  scale_fill_manual(values = c("#238B45FF","#00441BFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,45), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "urban_pandemicera_wfhany.png"), width = 7.9, height = 7, units = "cm")
ggplot(summ.WFH.pandemic) + # Chart prop WFHmost by urban
  geom_col(aes(y=100*p.wfh.most, x=pandemic.cat, fill=urban.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.most-1.96*se.wfh.most), ymax = 100*(p.wfh.most+1.96*se.wfh.most),
                    colour = urban.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with majority WFH hours (%)", x="", fill = "") +
  scale_fill_manual(values = c("#6A51A3FF","#3F007DFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "urban_pandemicera_wfhmost.png"), width = 7.9, height = 7, units = "cm")

### Partner summary stats ###
summ.WFH.pandemic <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         prtnr.wfh.cat = prtnr_any + prtnr_lfs.emp_all + prtnr_wfh.any_all,
         prtnr.wfh.cat = factor(prtnr.wfh.cat,
                                labels = c("No partner", "Non-employed partner", "Non-WFH partner", "WFH partner"))) %>%
  group_by(pandemic.cat, prtnr.wfh.cat, prtnr_any, prtnr_lfs.emp_all, prtnr_wfh.any_all) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any==1, na.rm=T),
            se.wfh.any = sd(wfh.any==1, na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(summ.WFH.pandemic) + # Chart prop WFHany by job.supervise
  geom_col(aes(y=100*p.wfh.any, x=pandemic.cat, fill=prtnr.wfh.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.any-1.96*se.wfh.any), ymax = 100*(p.wfh.any+1.96*se.wfh.any),
                    colour = prtnr.wfh.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="", fill = "Partner status") +
  scale_fill_manual(values = c("#00441B","#238B45","#74C476","#C7E9c0")) +
  #scale_fill_brewer(palette = "Greens", direction = -1) +
  scale_color_manual(values = rep("darkgray",12), guide = "none") +
  scale_y_continuous(limits = c(0,60), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "partner_pandemicera_wfhany.png"), width = 16, height = 7, units = "cm")
ggplot(summ.WFH.pandemic) + # Chart prop WFHmost by job.supervise
  geom_col(aes(y=100*p.wfh.most, x=pandemic.cat, fill=prtnr.wfh.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.most-1.96*se.wfh.most), ymax = 100*(p.wfh.most+1.96*se.wfh.most),
                    colour = prtnr.wfh.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with majority WFH hours (%)", x="", fill = "Partner status") +
  scale_fill_manual(values = c("#3F007D","#6A51A3","#9e9ac8","#dadaeb")) +
  scale_color_manual(values = rep("darkgray",12), guide = "none") +
  scale_y_continuous(limits = c(0,40), breaks = seq(0,60,5)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "partner_pandemicera_wfhmost.png"), width = 16, height = 7, units = "cm")

### House bedrooms summary stats ###
summ.WFH.pandemic <- df %>%
  filter(!is.na(home.bdrooms)) %>%
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         bedrooms.cat = if_else(home.bdrooms > 6, 6, home.bdrooms),
         bedrooms.cat = if_else(bedrooms.cat < 1, 1, bedrooms.cat),
         bedrooms.cat = factor(bedrooms.cat, labels = c("1 bedroom","2 bedrooms","3 bedrooms",
                                                        "4 bedrooms","5 bedrooms","6+ bedrooms"))) %>%
  group_by(pandemic.cat, bedrooms.cat) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any==1, na.rm=T),
            se.wfh.any = sd(wfh.any==1, na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(summ.WFH.pandemic) + # Chart prop WFHany by home.bdrooms
  geom_col(aes(y=100*p.wfh.any, x=pandemic.cat, fill=bedrooms.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.any-1.96*se.wfh.any), ymax = 100*(p.wfh.any+1.96*se.wfh.any),
                    colour = bedrooms.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="", fill = "Home bedrooms") +
  scale_fill_brewer(palette = "Greens", direction = -1) +
  scale_color_manual(values = rep("darkgray",12), guide = "none") +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "bedrooms_pandemicera_wfhany.png"), width = 16, height = 9, units = "cm")
ggplot(summ.WFH.pandemic) + # Chart prop WFHmost by home.bdrooms
  geom_col(aes(y=100*p.wfh.most, x=pandemic.cat, fill=bedrooms.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.most-1.96*se.wfh.most), ymax = 100*(p.wfh.most+1.96*se.wfh.most),
                    colour = bedrooms.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with majority WFH hours (%)", x="", fill = "Home bedrooms") +
  scale_fill_brewer(palette = "Purples", direction = -1) +
  scale_color_manual(values = rep("darkgray",12), guide = "none") +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,60,5)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "bedrooms_pandemicera_wfhmost.png"), width = 16, height = 9, units = "cm")


### Firm size summary stats ###
label.emplyrsize <- df %>%
  select(emplyr.size) %>%
  filter(!is.na(emplyr.size), emplyr.size != 12, emplyr.size != 13) %>%
  mutate(emplyr.size = if_else(emplyr.size < 5, 11, emplyr.size)) %>%
  group_by(emplyr.size) %>%
  slice_head(n = 1) %>%
  mutate(emplyr.size = paste0("emplyr.size::", emplyr.size)) %>%
  deframe()
label.firmsize <- tibble(emplyr.size = label.emplyrsize,
                         label = c(attr(attr(df$emplyr.size, "labels")[15:21], "names"))) %>%
  mutate(label = str_remove(label, "^\\[[0-9]{2}\\]\\s*"),
         label = str_remove(label, "^\\[[0-9]{1}\\]\\s*"),
         label = str_replace(label, "Don.*fewer\\s*", "Fewer ")) %>%
  deframe()
summ.WFH.pandemic <- df %>% 
  filter(!is.na(emplyr.size), emplyr.size != 12, emplyr.size != 13) %>%
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         emplyr.size = if_else(emplyr.size < 5, 11, emplyr.size),
         emplyr.size = factor(emplyr.size, labels = label.firmsize),
         emplyr.size = fct_relevel(emplyr.size, "Fewer than 20",after = 0)) %>%
  group_by(pandemic.cat, emplyr.size) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any==1, na.rm=T),
            se.wfh.any = sd(wfh.any==1, na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(summ.WFH.pandemic) + # Chart prop WFHany by firm.size
  geom_col(aes(y=100*p.wfh.any, x=pandemic.cat, fill=emplyr.size), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.any-1.96*se.wfh.any), ymax = 100*(p.wfh.any+1.96*se.wfh.any),
                    colour = emplyr.size),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="", fill = "Employer size (employees)") +
  scale_fill_brewer(palette = "Greens", direction = -1) +
  scale_color_manual(values = rep("darkgray",7), guide = "none") +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "firmsize_pandemicera_wfhany.png"), width = 16, height = 9, units = "cm")
ggplot(summ.WFH.pandemic) + # Chart prop WFHmost by firm.size
  geom_col(aes(y=100*p.wfh.most, x=pandemic.cat, fill=emplyr.size), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.most-1.96*se.wfh.most), ymax = 100*(p.wfh.most+1.96*se.wfh.most),
                    colour = emplyr.size),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with majority WFH hours (%)", x="", fill = "Employer size (employees)") +
  scale_fill_brewer(palette = "Purples", direction = -1) +
  scale_color_manual(values = rep("darkgray",7), guide = "none") +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,60,5)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave(paste0(chart_output, "firmsize_pandemicera_wfhmost.png"), width = 16, height = 9, units = "cm")

## Job tenure
job.tenure.centile <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         x_centile = ntile(job.tenure, 15)) %>%
  group_by(x_centile, pandemic.cat) %>%
  summarise(
    mean_x = mean(job.tenure, na.rm = TRUE),
    mean_any = mean(wfh.any==1, na.rm = TRUE),
    mean_most = mean(wfh.most==1, na.rm = TRUE),
    .groups = "drop") %>%
  group_by(pandemic.cat)
ggplot(job.tenure.centile) + # Chart prop WFHany by job.tenure
  geom_line(aes(y=100*mean_any, x=mean_x, colour=pandemic.cat), size = 1.2) +
  geom_point(aes(y=100*mean_any, x=mean_x, colour=pandemic.cat), size = 1.5) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="Job tenure (years)", colour = "") +
  scale_colour_manual(values = c("#74C476","#238B45","#00441B")) +
  scale_y_continuous(limits = c(0,40), breaks = seq(0,60,10)) +
  theme(legend.position = "none", axis.text = element_text(colour = "black"),
        text = element_text(size = 8)) 
ggsave(paste0(chart_output, "jobtenure_pandemicera_wfhany.png"), width = 7.9, height = 7, units = "cm")
ggplot(job.tenure.centile) + # Chart prop WFHmost by job.tenure
  geom_line(aes(y=100*mean_most, x=mean_x, colour=pandemic.cat), size = 1.2) +
  geom_point(aes(y=100*mean_most, x=mean_x, colour=pandemic.cat), size = 1.5) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="Job tenure (years)", colour = "") +
  scale_colour_manual(values = c("#9e9ac8","#6A51A3","#3F007D")) +
  scale_y_continuous(limits = c(0,25), breaks = seq(0,60,5)) +
  theme(legend.position = "none", axis.text = element_text(colour = "black"),
        text = element_text(size = 8)) 
ggsave(paste0(chart_output, "jobtenure_pandemicera_wfhmost.png"), width = 7.9, height = 7, units = "cm")

## wages
wages.centile <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         x_centile = ntile(wage.mn, 15)) %>%
  group_by(x_centile, pandemic.cat) %>%
  summarise(
    mean_x = mean(wage.mn, na.rm = TRUE),
    mean_any = mean(wfh.any==1, na.rm = TRUE),
    mean_most = mean(wfh.most==1, na.rm = TRUE),
    .groups = "drop") %>%
  group_by(pandemic.cat)
ggplot(wages.centile) + # Chart prop WFHany by wage.mn
  geom_line(aes(y=100*mean_any, x=mean_x, colour=pandemic.cat), size = 1.2) +
  geom_point(aes(y=100*mean_any, x=mean_x, colour=pandemic.cat), size = 1.5) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="Wages ($)", colour = "") +
  scale_colour_manual(values = c("#74C476","#238B45","#00441B")) +
  scale_y_continuous(limits = c(0,70), breaks = seq(0,70,10)) +
  scale_x_continuous(breaks = seq(0,150,20)) +
  theme(legend.position = "none", axis.text = element_text(colour = "black"),
        text = element_text(size = 8)) 
ggsave(paste0(chart_output, "wage_pandemicera_wfhany.png"), width = 7.9, height = 7, units = "cm")
ggplot(wages.centile) + # Chart prop WFHmost by wage.mn
  geom_line(aes(y=100*mean_most, x=mean_x, colour=pandemic.cat), size = 1.2) +
  geom_point(aes(y=100*mean_most, x=mean_x, colour=pandemic.cat), size = 1.5) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="Wages ($)", colour = "") +
  scale_colour_manual(values = c("#9e9ac8","#6A51A3","#3F007D")) +
  scale_y_continuous(limits = c(0,45), breaks = seq(0,60,10)) +
  theme(legend.position = "none", axis.text = element_text(colour = "black"),
        text = element_text(size = 8)) 
ggsave(paste0(chart_output, "wage_pandemicera_wfhmost.png"), width = 7.9, height = 7, units = "cm")

## Distance to work
job.dist.centile <- df %>%
  filter(job.dist > 0 & !is.na(job.dist)) %>%
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         x_centile = ntile(job.dist, 15)) %>%
  group_by(x_centile, pandemic.cat) %>%
  summarise(
    mean_x = mean(job.dist, na.rm = TRUE),
    mean_any = mean(wfh.any==1, na.rm = TRUE),
    mean_most = mean(wfh.most==1, na.rm = TRUE),
    .groups = "drop") %>%
  group_by(pandemic.cat)
ggplot(job.dist.centile) + # Chart prop WFHany by job.dist
  geom_line(aes(y=100*mean_any, x=mean_x, colour=pandemic.cat), size = 1.2) +
  geom_point(aes(y=100*mean_any, x=mean_x, colour=pandemic.cat), size = 1.5) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="Distance to work (km, log scale)", colour = "") +
  scale_colour_manual(values = c("#74C476","#238B45","#00441B")) +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,70,10)) +
  scale_x_log10() +
  theme(legend.position = "none", axis.text = element_text(colour = "black"),
        text = element_text(size = 8)) 
ggsave(paste0(chart_output, "jobdist_pandemicera_wfhany.png"), width = 7.9, height = 7, units = "cm")
ggplot(job.dist.centile) + # Chart prop WFHmost by job.dist
  geom_line(aes(y=100*mean_most, x=mean_x, colour=pandemic.cat), size = 1.2) +
  geom_point(aes(y=100*mean_most, x=mean_x, colour=pandemic.cat), size = 1.5) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="Distance to work (km, log scale)", colour = "") +
  scale_colour_manual(values = c("#9e9ac8","#6A51A3","#3F007D")) +
  scale_y_continuous(limits = c(0,25), breaks = seq(0,60,5)) +
  scale_x_log10() +
  theme(legend.position = "none", axis.text = element_text(colour = "black"),
        text = element_text(size = 8)) 
ggsave(paste0(chart_output, "jobdist_pandemicera_wfhmost.png"), width = 7.9, height = 7, units = "cm")

## Distance to nearest city
city.dist.centile <- df %>%
  filter(dist.nearstcity > 0 & !is.na(dist.nearstcity)) %>%
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         x_centile = ntile(dist.nearstcity, 15)) %>%
  group_by(x_centile, pandemic.cat) %>%
  summarise(
    mean_x = mean(dist.nearstcity, na.rm = TRUE),
    mean_any = mean(wfh.any==1, na.rm = TRUE),
    mean_most = mean(wfh.most==1, na.rm = TRUE),
    .groups = "drop") %>%
  group_by(pandemic.cat)
ggplot(city.dist.centile) + # Chart prop WFHany by dist.nearstcity
  geom_line(aes(y=100*mean_any, x=mean_x, colour=pandemic.cat), size = 1.2) +
  geom_point(aes(y=100*mean_any, x=mean_x, colour=pandemic.cat), size = 1.5) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="Distance to nearest city (km, log scale)", colour = "") +
  scale_colour_manual(values = c("#74C476","#238B45","#00441B")) +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,70,10)) +
  scale_x_log10() +
  theme(legend.position = "none", axis.text = element_text(colour = "black"),
        text = element_text(size = 8)) 
ggsave(paste0(chart_output, "citydist_pandemicera_wfhany.png"), width = 7.9, height = 7, units = "cm")
ggplot(city.dist.centile) + # Chart prop WFHmost by dist.nearstcity
  geom_line(aes(y=100*mean_most, x=mean_x, colour=pandemic.cat), size = 1.2) +
  geom_point(aes(y=100*mean_most, x=mean_x, colour=pandemic.cat), size = 1.5) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="Distance to nearest city (km, log scale)", colour = "") +
  scale_colour_manual(values = c("#9e9ac8","#6A51A3","#3F007D")) +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,60,5)) +
  scale_x_log10() +
  theme(legend.position = "none", axis.text = element_text(colour = "black"),
        text = element_text(size = 8)) 
ggsave(paste0(chart_output, "citydist_pandemicera_wfhmost.png"), width = 7.9, height = 7, units = "cm")
