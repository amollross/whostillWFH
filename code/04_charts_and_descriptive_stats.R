# =============================== #
#### Who still WFH? replication files ####
#### 4. Charts and descritpive statistics ####
#### Aaron Mollross ####
# =============================== #


#### SUPPORTING CHARTS AND SUMMARY STATISTICS - APPENDIX A ####

# Missing job distance data comparison
my_vars <- c("wfh.any", "wfh.prop", "job.uslhrs", "age", "unieduc", "female","child.resid", "child.yngst",
             "disab", "disab.type.cogni", "wage.mn", "earns.tot", "lfs.emp.ft", "job.tenure", "job.supervise", 
             "DNscore_manual", "home.bdrooms", "urban", "innr.region", "dist.nearstcity")
wfhdata.travel <- wfhdata.employees %>%
  filter(year >= 2017,
         age >=15,
         age <=69) %>%
  mutate(job.dist.na = if_else(is.na(job.dist), 1, 0),
         job.dist.na = factor(job.dist.na, labels = c("Not missing", "Missing")),
         wfh.any = if_else(wfh.any=="WFH",1,0), # recode wfh.any
         pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic (2017-2019)" = "Pre-pandemic period",
                                   "Pandemic (2020-2021)" = "Pandemic period",
                                   "Post-pandemic (2022-2024)" = "Post-pandemic period"))
wfhdata.travel <- wfhdata.travel %>%
  select(all_of(my_vars), job.dist.na)  %>%    
  mutate(across(where(haven::is.labelled), haven::zap_label),
         across(where(haven::is.labelled), haven::zap_labels))
datasummary_balance(data = wfhdata.travel,
                    wfh.any + wfh.prop + job.uslhrs + age + unieduc + female + child.resid + child.yngst +
                      disab + disab.type.cogni + wage.mn + earns.tot + lfs.emp.ft + job.tenure + job.supervise + 
                      DNscore_manual + home.bdrooms + urban + innr.region + dist.nearstcity 
                    ~ job.dist.na,
                    fmt = 3,
                    dinm = T,
                    dinm_statistic = "p.value",
                    output = "missing jobdist summary stats.xlsx"
)   

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
ggsave("charts/occup_WFH_x_DNscore.png", width = 16, height = 9.8, units = "cm")

# Job distance v distance to nearest city centre scatter plot
dist.scatter <- df %>%
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic (2017-2019)" = "Pre-pandemic period",
                                   "Pandemic (2020-2021)" = "Pandemic period",
                                   "Post-pandemic (2022-2024)" = "Post-pandemic period"),
         citydist.centile = ntile(dist.nearstcity, 100)) %>%
  group_by(pandemic.cat, citydist.centile) %>%
  summarise(
    mean_citydist = mean(10*dist.nearstcity),
    mean_jobdist = mean(10*job.dist),
    med_citydist = median(10*dist.nearstcity),
    med_jobdist = median(10*job.dist),
    .groups = "drop")

r2_dist.mean <- dist.scatter %>%
  filter(pandemic.cat != "Pandemic (2020-2021)") %>%
  group_by(pandemic.cat) %>%
  do(glance(lm(mean_jobdist ~ mean_citydist, data = .))) %>%
  select(pandemic.cat, r.squared) %>%
  ungroup() %>%
  rename(r2 = r.squared) %>%
  mutate(
    x = c(760, 600),
    y = c(45, 42),
    label = paste0("R² = ", round(r2, 3)))

ggplot(dist.scatter %>% filter(pandemic.cat != "Pandemic (2020-2021)")) +
  geom_point(aes(y = mean_jobdist, x = mean_citydist, colour = pandemic.cat)) +
  geom_smooth(aes(y = mean_jobdist, x = mean_citydist), colour = "black",
              formula = y~poly(x,1), method = "glm", se = F) +
  labs(x = "Mean distance to nearest city centre (km)",
       y = "Mean distance to work (km)") +
  facet_grid(cols = vars(pandemic.cat)) +
  geom_text(data = r2_dist.mean, aes(x = x, y = y, label = label), 
            size = 2.8, fontface = "bold", hjust = 0) +
  scale_colour_manual(values = c("#6BAED6FF", "#08306BFF")) +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"), text = element_text(size =9),
        legend.position = "none") 
ggsave("charts/dist_work_x_city.png", width = 16, height = 9, units = "cm")



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
            life.satisfact = mean(satisfact.life, na.rm = T),
            job.satisfact = mean(jobsat.ovrall, na.rm = T),
            home.satisfact = mean(satisfact.home),
            bedrooms = mean(home.bdrooms, na.rm = T),
            urban = mean(urban, na.rm = T),
            inner.regional = mean(innr.region, na.rm = T),
            dist.city = mean(dist.nearstcity, na.rm = T),
            bedrooms.pp = mean(home.bdrooms.pp, na.rm = T),
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
summ.stats.WFHany.pandemic %>% group_by(wfh.any) %>% summarise(sum = sum(obs)) # counts by WFH status
df %>% filter(wfh.any == "WFH") %>% summarise(mean = mean(wfh.prop)) # mean prop of hours WFH, conditional on WFH

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
            life.satisfact = mean(satisfact.life, na.rm = T),
            job.satisfact = mean(jobsat.ovrall, na.rm = T),
            home.satisfact = mean(satisfact.home),
            bedrooms = mean(home.bdrooms, na.rm = T),
            urban = mean(urban, na.rm = T),
            inner.regional = mean(innr.region, na.rm = T),
            dist.city = mean(dist.nearstcity, na.rm = T),
            bedrooms.pp = mean(home.bdrooms.pp, na.rm = T),
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
ggsave("charts/WSWFH descript charts/WFHmost_year.png", width = 16, height = 10, units = "cm")

# Density plot of WFH rates
df %>% filter(wfh.any == "WFH") %>%
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
ggsave("charts/WSWFH descript charts/WFHdensity_pandemicera.png", width = 16, height = 10, units = "cm")
df %>% count(wfh.prop ==1, pandemic.cat) #basic summaries of distribution for fully remote

# Sankey diagram
sec_years <- c(2019,2021,2023)
# Summarise the transitions for every second year
transition.sec <- df %>%
  mutate(wfh.category.yr = paste(wfh.category, year, sep = " ")) %>% # adding a new var to distinguish categories over time
  filter(year %in% sec_years) %>% # filter for sec_years data only
  arrange(xwaveid, year) %>%
  group_by(xwaveid) %>%
  filter(all(sec_years %in% year)) %>% # filter out observations that do not cover the full period
  mutate(next.wfh.category = dplyr::lead(wfh.category.yr)) %>% 
  filter(!is.na(next.wfh.category)) %>%
  ungroup() %>%
  count(wfh.category.yr, next.wfh.category, name = "Count")
# Build prepared sankey dataset
nodes <- data.frame(name = unique(c(transition.sec$wfh.category.yr, transition.sec$next.wfh.category))) ## edit for relevant dataset ##
links <- transition.sec %>% ## edit for relevant dataset ##
  mutate(source = match(wfh.category.yr, nodes$name) - 1,
         target = match(next.wfh.category, nodes$name) - 1) %>%
  select(source, target, value = Count)
# Build custom colour scale to align with palettes in desc stats
# from the MapPalettes::Bruiser palette: https://github.com/disarm-platform/MapPalettes/blob/master/R/mappalettes_function.R
base.colors <- c(
  "Never" = "#1E313E",
  "Low hybrid" = "#43435A", 
  "High hybrid" = "#725370",
  "Fully remote" = "#A4607C") 
nodes <- nodes %>%
  mutate(base.category = gsub(" \\d+$", "", name)) %>% # Remove year suffix
  mutate(color = base.colors[base.category]) # Assign colors based on the base.category
colourScale <- paste0(
  'd3.scaleOrdinal()',
  '.domain(["', paste(unique(nodes$base.category), collapse = '", "'), '"])',
  '.range(["', paste(unique(nodes$color), collapse = '", "'), '"])') # Define a colourScale using the nodes color column
# Create sankey diagram
sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  units = "Transitions",
  fontSize = 18,
  fontFamily = "Calibri",
  nodeWidth = 50,
  colourScale = colourScale)
# Check on quantity figures using Never as baseline (transition.sec df only)
transition.sec %>% 
  summarise(never19 = sum(Count[wfh.category.yr == "Never 2019"], na.rm=T),
            never21 = sum(Count[wfh.category.yr == "Never 2021"], na.rm=T),
            never23 = sum(Count[next.wfh.category == "Never 2023"], na.rm=T))
df %>%
  group_by(xwaveid) %>%
  filter(all(sec_years %in% year)) %>%
  ungroup() %>%
  count(wfh.category=="Never" & year==2019,
        wfh.category=="Never" & year==2021,
        wfh.category=="Never" & year==2023)
# Clean the environment
rm(list = c("links","nodes","transition.sec","base.colors","colourScale","sec_years"))

# Age summary stats (5yr bins)
age.summ.WFH.pandemic.5yr <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic (2017-2019)" = "Pre-pandemic period",
                                   "Pandemic (2020-2021)" = "Pandemic period",
                                   "Post-pandemic (2022-2024)" = "Post-pandemic period")) %>%
  group_by(pandemic.cat, age.cat.5yr) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any=="WFH", na.rm=T),
            se.wfh.any = sd(wfh.any=="WFH", na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(age.summ.WFH.pandemic.5yr) + # Chart prop WFHany by age (5 yr bins)
  geom_col(aes(y=100*p.wfh.any, x=pandemic.cat, fill=age.cat.5yr), 
           colour = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin=100*(p.wfh.any-1.96*se.wfh.any), ymax=100*(p.wfh.any+1.96*se.wfh.any), x = pandemic.cat, colour = age.cat.5yr), 
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", fill = "Age category") +
  scale_fill_paletteer_d(palette = "ggsci::green_material", direction = -1) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray","darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.title.x = element_blank(), 
        text = element_text(size = 8), axis.text = element_text(colour = "black")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) 
ggsave("charts/age5yr_pandemicera_wfhany.png", width = 16, height = 8, units = "cm")
ggplot(age.summ.WFH.pandemic.5yr) + # Chart mean WFHmost by age (5 yr bins)
  geom_col(aes(y=100*p.wfh.most, x=pandemic.cat, fill=age.cat.5yr), 
           colour = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin=100*(p.wfh.most-1.96*se.wfh.most), ymax=100*(p.wfh.most+1.96*se.wfh.most), 
                    x = pandemic.cat, colour = age.cat.5yr), 
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with majority WFH hours (%)", fill = "Age category") +
  scale_fill_paletteer_d(palette = "ggsci::purple_material", direction = -1) +
  scale_color_manual(values = c("darkgray", "darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.title.x = element_blank(), 
        text = element_text(size = 8), axis.text = element_text(colour = "black")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 70, by = 5)) 
ggsave("charts/age5yr_pandemicera_wfhmost.png", width = 16, height = 8, units = "cm")

# Gender summary stats
fem.summ.WFH.pandemic <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         female.cat = factor(female, labels = c("Male", "Female"))) %>%
  group_by(pandemic.cat, female, female.cat) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any=="WFH", na.rm=T),
            se.wfh.any = sd(wfh.any=="WFH", na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(fem.summ.WFH.pandemic) + # Chart prop WFHany by female
  geom_col(aes(y=100*p.wfh.any, x=pandemic.cat, fill=female.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.any-1.96*se.wfh.any), ymax = 100*(p.wfh.any+1.96*se.wfh.any),
                    colour = female.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="", fill = "") +
  scale_fill_manual(values = c("#238B45FF","#00441BFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,55), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave("charts/female_pandemicera_wfhany.png", width = 7.9, height = 7, units = "cm")
ggplot(fem.summ.WFH.pandemic) + # Chart prop WFHmost by female
  geom_col(aes(y=100*p.wfh.most, x=pandemic.cat, fill=female.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.most-1.96*se.wfh.most), ymax = 100*(p.wfh.most+1.96*se.wfh.most),
                    colour = female.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with majority WFH hours (%)", x="", fill = "") +
  scale_fill_manual(values = c("#6A51A3FF","#3F007DFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave("charts/female_pandemicera_wfhmost.png", width = 7.9, height = 7, units = "cm")

# Disability summary stats
disab.summ.WFH.pandemic <- df %>% 
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         disab.cat = factor(disab, labels = c("No disability", "Has a disability"))) %>%
  group_by(pandemic.cat, disab, disab.cat) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any=="WFH", na.rm=T),
            se.wfh.any = sd(wfh.any=="WFH", na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(disab.summ.WFH.pandemic) + # Chart prop WFHany by disab
  geom_col(aes(y=100*p.wfh.any, x=pandemic.cat, fill=disab.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.any-1.96*se.wfh.any), ymax = 100*(p.wfh.any+1.96*se.wfh.any),
                    colour = disab.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="", fill = "") +
  scale_fill_manual(values = c("#238B45FF","#00441BFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,55), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave("charts/disab_pandemicera_wfhany.png", width = 7.9, height = 7, units = "cm")
ggplot(disab.summ.WFH.pandemic) + # Chart prop WFHmost by disab
  geom_col(aes(y=100*p.wfh.most, x=pandemic.cat, fill=disab.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.most-1.96*se.wfh.most), ymax = 100*(p.wfh.most+1.96*se.wfh.most),
                    colour = disab.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with majority WFH hours (%)", x="", fill = "") +
  scale_fill_manual(values = c("#6A51A3FF","#3F007DFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave("charts/disab_pandemicera_wfhmost.png", width = 7.9, height = 7, units = "cm")

# Education summary stats
uni.summ.WFH.pandemic <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         unieduc.cat = factor(unieduc, labels = c("No degree", "University degree"))) %>%
  group_by(pandemic.cat, unieduc, unieduc.cat) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any=="WFH", na.rm=T),
            se.wfh.any = sd(wfh.any=="WFH", na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(uni.summ.WFH.pandemic) + # Chart prop WFHany by unieduc
  geom_col(aes(y=100*p.wfh.any, x=pandemic.cat, fill=unieduc.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.any-1.96*se.wfh.any), ymax = 100*(p.wfh.any+1.96*se.wfh.any),
                    colour = unieduc.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="", fill = "") +
  scale_fill_manual(values = c("#238B45FF","#00441BFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,55), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave("charts/unieduc_pandemicera_wfhany.png", width = 7.9, height = 7, units = "cm")
ggplot(uni.summ.WFH.pandemic) + # Chart prop WFHmost by unieduc
  geom_col(aes(y=100*p.wfh.most, x=pandemic.cat, fill=unieduc.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.most-1.96*se.wfh.most), ymax = 100*(p.wfh.most+1.96*se.wfh.most),
                    colour = unieduc.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with majority WFH hours (%)", x="", fill = "") +
  scale_fill_manual(values = c("#6A51A3FF","#3F007DFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave("charts/unieduc_pandemicera_wfhmost.png", width = 7.9, height = 7, units = "cm")

# Married summary stats
marr.summ.WFH.pandemic <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         ismarr.cat = factor(ismarr, labels = c("Not married", "Married"))) %>%
  group_by(pandemic.cat, ismarr, ismarr.cat) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any=="WFH", na.rm=T),
            se.wfh.any = sd(wfh.any=="WFH", na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(marr.summ.WFH.pandemic) + # Chart prop WFHany by ismarr
  geom_col(aes(y=100*p.wfh.any, x=pandemic.cat, fill=ismarr.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.any-1.96*se.wfh.any), ymax = 100*(p.wfh.any+1.96*se.wfh.any),
                    colour = ismarr.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with any usual WFH (%)", x="", fill = "") +
  scale_fill_manual(values = c("#238B45FF","#00441BFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,55), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave("charts/marr_pandemicera_wfhany.png", width = 7.9, height = 7, units = "cm")
ggplot(marr.summ.WFH.pandemic) + # Chart prop WFHmost by marr
  geom_col(aes(y=100*p.wfh.most, x=pandemic.cat, fill=ismarr.cat), 
           position = position_dodge(), colour = "black") +
  geom_errorbar(aes(x=pandemic.cat, ymin=100*(p.wfh.most-1.96*se.wfh.most), ymax = 100*(p.wfh.most+1.96*se.wfh.most),
                    colour = ismarr.cat),
                position = position_dodge(width = 0.9), width = 0.3, size = 1) +
  theme_classic() +
  labs(y="Proportion with majority WFH hours (%)", x="", fill = "") +
  scale_fill_manual(values = c("#6A51A3FF","#3F007DFF")) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray","darkgray","darkgray","darkgray"), 
                     guide = "none") +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave("charts/marr_pandemicera_wfhmost.png", width = 7.9, height = 7, units = "cm")

# Supervisor summary stats
sup.summ.WFH.pandemic <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         job.supervise.cat = factor(job.supervise, labels = c("Non-supervisor", "Supervisor"))) %>%
  group_by(pandemic.cat, job.supervise, job.supervise.cat) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any=="WFH", na.rm=T),
            se.wfh.any = sd(wfh.any=="WFH", na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(sup.summ.WFH.pandemic) + # Chart prop WFHany by job.supervise
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
  scale_y_continuous(limits = c(0,55), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave("charts/superv_pandemicera_wfhany.png", width = 7.9, height = 7, units = "cm")
ggplot(sup.summ.WFH.pandemic) + # Chart prop WFHmost by job.supervise
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
  scale_y_continuous(limits = c(0,35), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave("charts/superv_pandemicera_wfhmost.png", width = 7.9, height = 7, units = "cm")

# Urban summary stats
urban.summ.WFH.pandemic <- df %>%  
  mutate(pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period"),
         urban.cat = factor(urban, labels = c("Regional/remote resident", "Major city resident"))) %>%
  group_by(pandemic.cat, urban, urban.cat) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any=="WFH", na.rm=T),
            se.wfh.any = sd(wfh.any=="WFH", na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(urban.summ.WFH.pandemic) + # Chart prop WFHany by urban
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
  scale_y_continuous(limits = c(0,55), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave("charts/urban_pandemicera_wfhany.png", width = 7.9, height = 7, units = "cm")
ggplot(urban.summ.WFH.pandemic) + # Chart prop WFHmost by urban
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
  scale_y_continuous(limits = c(0,35), breaks = seq(0,60,10)) +
  theme(legend.position = "bottom", axis.text = element_text(colour = "black"),
        text = element_text(size = 8), axis.title.x = element_blank()) 
ggsave("charts/urban_pandemicera_wfhmost.png", width = 7.9, height = 7, units = "cm")

# Occupation summary stats
occ.summ.WFH.pandemic <- df %>% 
  mutate(job.occ.1 = fct_na_value_to_level(job.occ.1, level = "Unknown"),
         pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period")) %>%
  group_by(pandemic.cat, job.occ.1) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any=="WFH", na.rm=T),
            se.wfh.any = sd(wfh.any=="WFH", na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(occ.summ.WFH.pandemic) + # Chart prop WFHany by occup
  geom_col(aes(y=job.occ.1, x=100*p.wfh.any, fill=pandemic.cat), 
           position = position_dodge(width = 1.2), colour = "black") +
  facet_grid(cols = vars(pandemic.cat)) +
  theme_bw() +
  labs(x="Proportion with any usual WFH (%)", fill = "") +
  scale_fill_manual(values = c("#74C476FF","#238B45FF","#00441BFF")) +
  scale_x_continuous(limits = c(0,60), breaks = seq(0,100,20)) +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "none", axis.text = element_text(colour = "black"),
        text = element_text(size = 9), axis.title.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey")) 
ggsave("charts/occ_pandemicera_wfhany.png", width = 16, height = 8, units = "cm")
ggplot(occ.summ.WFH.pandemic) + # Chart prop WFHmost by occup
  geom_col(aes(y=job.occ.1, x=100*p.wfh.most, fill=pandemic.cat), 
           position = position_dodge(width = 1.2), colour = "black") +
  facet_grid(cols = vars(pandemic.cat)) +
  theme_bw() +
  labs(x="Proportion with majority WFH hours (%)", fill = "") +
  scale_fill_manual(values = c("#9E9AC8FF","#6A51A3FF","#3F007DFF")) +
  scale_x_continuous(limits = c(0,40), breaks = seq(0,60,20)) +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "none", axis.text = element_text(colour = "black"),
        text = element_text(size = 9), axis.title.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey")) 
ggsave("charts/occ_pandemicera_wfhmost.png", width = 16, height = 8, units = "cm")

### Industry summary stats ###
ind.summ.WFH.pandemic <- df %>% 
  mutate(job.ind.1 = fct_na_value_to_level(job.ind.1, level = "Unknown"),
         pandemic.cat = fct_recode(pandemic.cat,
                                   "Pre-pandemic\n(2017-2019)" = "Pre-pandemic period",
                                   "Pandemic\n(2020-2021)" = "Pandemic period",
                                   "Post-pandemic\n(2022-2024)" = "Post-pandemic period")) %>%
  group_by(pandemic.cat, job.ind.1) %>%
  summarise(obs = n(),
            p.wfh.any = mean(wfh.any=="WFH", na.rm=T),
            se.wfh.any = sd(wfh.any=="WFH", na.rm=T)/sqrt(obs),
            p.wfh.most = mean(wfh.most==1, na.rm=T),
            se.wfh.most = sd(wfh.most==1, na.rm=T)/sqrt(obs),
            .groups = "drop")
ggplot(ind.summ.WFH.pandemic) + # Chart prop WFHany by urban
  geom_col(aes(y=job.ind.1, x=100*p.wfh.any, fill=pandemic.cat), 
           position = position_dodge(width = 1.2), colour = "black") +
  facet_grid(cols = vars(pandemic.cat)) +
  theme_bw() +
  labs(x="Proportion with any usual WFH (%)", fill = "") +
  scale_fill_manual(values = c("#74C476FF","#238B45FF","#00441BFF")) +
  scale_x_continuous(limits = c(0,90), breaks = seq(0,100,20)) +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "none", axis.text = element_text(colour = "black"),
        text = element_text(size = 9), axis.title.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey")) 
ggsave("charts/indust_pandemicera_wfhany.png", width = 16, height = 11.5, units = "cm")
ggplot(ind.summ.WFH.pandemic) + # Chart prop WFHany by urban
  geom_col(aes(y=job.ind.1, x=100*p.wfh.most, fill=pandemic.cat), 
           position = position_dodge(width = 1.2), colour = "black") +
  facet_grid(cols = vars(pandemic.cat)) +
  theme_bw() +
  labs(x="Proportion with majority WFH hours (%)", fill = "") +
  scale_fill_manual(values = c("#9E9AC8FF","#6A51A3FF","#3F007DFF")) +
  scale_x_continuous(limits = c(0,70), breaks = seq(0,100,20)) +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "none", axis.text = element_text(colour = "black"),
        text = element_text(size = 9), axis.title.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "darkgrey")) 
ggsave("charts/indust_pandemicera_wfhmost.png", width = 16, height = 11.5, units = "cm")
