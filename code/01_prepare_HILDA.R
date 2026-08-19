# =============================== #
#### Who still WFH? replication files ####
#### 1. Combining HILDA files ####
#### Aaron Mollross ####
# =============================== #

install.packages('haven'); install.packages('tidyverse'); install.packages('plm'); 
install.packages('fixest'); install.packages('sf'); install.packages('absmapsdata')
install.packages('modelsummary'); install.packages('car'); install.packages('broom')
library(haven); library(tidyverse); library(plm); 
library(fixest); library(sf); library(absmapsdata)
library(modelsummary); library(car); library(broom)
options(scipen = 999)

setwd(output)

#### HILDA COMBINATION CODE ####
# Melb Institute program for combining longitudinal files, derived from:
# https://melbourneinstitute.unimelb.edu.au/hilda/for-data-users/program-library

wave <- 24     # Number of wave data files to extract.
maxwave <- 24  # Latest wave.
rls <- 240     # Latest release.

# SECTION 1: Creating an unbalanced dataset (long-format)
# Adjust for personal needs.
var <- c("hhrhid", "hhrpid", "hhpxid",  #file ID
         "hgni", # interview flag
         "hhtup", # top-up cohort flag
         "hhstate", "hhs3ra", "hhs3gcc", "hhpcode", "hhssa1", #geography
         "hgsex", "hgage", "edhigh1", "hhs3add", #demographic
         "mrcurr", "tcr", "rcyng", #family
         "esdtl", "esempst", "jbindc", #employment status
         "jbmh", "jbmhrh", "jbmhrhw", "jbmhrha", "jbmagh", "jbmhdy", #WFH characteristics
         "jbmhruc", "jbhruc", #hours worked
         "jbmday","jbmmth","jbmday1","jbmday2","jbmday3","jbmday4","jbmday5","jbmday6","jbmday7", #working patterns
         "jbmo06", "jbmo62", "jbmo61", "jbcmocc", "jbocct", "jbmi06", "jbmi62", "jbmi61", #occupation & industry
         "jbmsvsr", "jbempt", "jbmplej", "jbcasab", #other work characteristics
         "jbmmply", "jbmlpc", "jbmems2", "jbmemsz", #employer characteristics
         "jbmsall", "jbmsflx", "jbmshrs", "jbmspay", "jbmssec", "jbmswrk", #job satisfaction
         "jbmlkm", "lscom", # job distance and travel time
         "wsce","wscei", "wscef", "wscme", "wscmei", "wscmef", "wscoe", "wscoei", "wscoef", #work earnings
         "helth", "helthdg", "helthwk", #health status
         "bncdsp", "bnfdspf", "hgndi", #disability support pension + NDIS
         "hespnc", "hehear", "hespch", "hebflc", "heslu", "heluaf", "hedgt", "helufl", "henec", "hemig", #type of disability 1
         "hecrph", "hedisf", "hemirh", "hesbdb", "hecrp", "hehibd", "hemed", "heoth", "herf", "hedk", #type of disability 2
         "losat", "losathl", "losatft", #life satisfaction
         "hsbedrm", "hhpers", "hsvalui", "dodtyp", #housing characteristics
         "lebth", "lefrd", "leinf", "leins", "lejob", "lemvd", "leprm", "lertr", "lemar", "leprg", #life events
         "hges", # e-person employment
         "hglth", "hgndi") # e-person disability

for( i in 1:wave) {
  file_list <- paste0(hilda_data, "Combined_", letters[i], rls, "u.dta")
  temp <- read_dta(file_list)
  var_add <- paste0(letters[i], var) # Add wave letter onto the variable names
  temp <- temp %>% dplyr::select(xwaveid, any_of(var_add)) 
  # any_of() # lets the program avoid selecting the variable not included in a specific wave and set NA to that variable. eg: "hwhmhl" not included in wave 1
  names(temp)[-1] <- substring(names(temp)[-1], 2) # Remove wave letter from variable names except for xwaveid
  temp$wave <- i
  if (i == 1 ){
    unbalanced_long <- temp
  } else {
    unbalanced_long <- bind_rows(unbalanced_long, temp) # Append the data file from each wave
  }
}

# Reorder columns
unbalanced_long <- unbalanced_long %>%
  select(xwaveid, hhrhid, hhrpid, hhpxid, hgni, hhtup, wave, hhstate, hhpcode, hhssa1, everything())

# Save new data set
save(unbalanced_long, file = "long-file-unbalanced.Rdata")

# Clean the environment
remove(temp, master, master_long, master_long20XX, comp, file_list, i, intvw_pattern, intvw_pattern20XX, master_file, maxwave, 
       newdatadir, origdatdir, rls, var, var_add, wave)



#### CLEANING UNBALANCED DATA ####

wfhdata <- unbalanced_long 

# Convert missing values to NA
wfhdata[wfhdata < 0] <- NA

# Remove observations that were not interviewed (enumerated, but substantial missing data)
wfhdata <- wfhdata %>%
  filter(hgni == 0) %>%
  select(!hgni)

# Filter for observations from wave 4 (2004) onwards
wfhdata <- wfhdata %>%
  filter(wave >= 2)

# Rename variables for ease of use
wfhdata <- wfhdata %>%
  arrange(xwaveid, wave) %>%
  select(xwaveid, wave, everything()) %>%
  rename(state = hhstate,
         SA1 = hhssa1,
         pcode = hhpcode,
         RA.2021 = hhs3ra,
         GCC.2021 = hhs3gcc,
         SEIFA.2021 = hhs3add,
         female = hgsex,
         age = hgage,
         educ = edhigh1,
         child.resid = tcr,
         child.yngst = rcyng,
         wfh.any = jbmh, # binary flag for usual WFH
         wfh.formalagree = jbmagh,
         wfh.days.count = jbmhdy, # new var in wave 23
         job.uslhrs = jbhruc,
         job.uslhrs.mn = jbmhruc,
         job.tenure = jbempt,
         job.supervise = jbmsvsr,
         job.ind.contractor = jbindc,
         job.casual = jbcasab,
         job.leave.intent = jbmplej,
         job.traveltime = lscom,
         job.dist = jbmlkm,
         job.occ = jbmo06, # 4-digit occupation
         job.occ.2 = jbmo62, # 2-digit occupation
         job.occ.1 = jbmo61, # 1-digit occupation
         job.occ.tenure = jbocct, # time in occupation
         job.ind = jbmi06, # 4-digit industry
         job.ind.2 = jbmi62, # 2-digit industry
         job.ind.1 = jbmi61, # 1-digit industry
         jobsat.ovrall = jbmsall,
         jobsat.flex = jbmsflx,
         jobsat.hrs = jbmshrs,
         jobsat.pay = jbmspay,
         jobsat.sec = jbmssec,
         jobsat.work = jbmswrk,
         emplyr.pcode = jbmlpc,
         earns.tot = wscei, #using imputed earnings values
         earns.mn = wscmei,
         earns.oth = wscoei,
         disab = helth,
         disab.lmtswk.scale = helthdg,
         disab.type.sight = hespnc,
         disab.type.hear = hehear,
         disab.type.spch = hespch,
         disab.type.blkout = hebflc,
         disab.type.learn = heslu,
         disab.type.arms = heluaf,
         disab.type.grip = hedgt,
         disab.type.legs = helufl,
         disab.type.emotn = henec,
         disab.type.hdache = hemig,
         disab.type.restrict = hecrph,
         disab.type.disf = hedisf,
         disab.type.mental = hemirh,
         disab.type.breath = hesbdb,
         disab.type.pain = hecrp,
         disab.type.brninj = hehibd,
         disab.type.other1 = hemed,
         disab.type.other2 = heoth,
         disab.type.refused = herf,
         disab.type.dknow = hedk,
         disab.DSP = bncdsp,
         disab.DSP.amt = bnfdspf,
         disab.NDISrecpt = hgndi,
         satisfact.home = losathl,
         satisfact.life = losat,
         satisfact.ftime = losatft,
         home.bdrooms = hsbedrm,
         home.resids = hhpers,
         home.value = hsvalui,
         newchild = lebth,
         fired = lefrd,
         famill = leinf,
         injury.illness = leins,
         newjob = lejob,
         moved = lemvd,
         promote = leprm,
         retired = lertr,
         pregnancy = leprg,
         gotmarr = lemar, # married in past year
         ismarr = mrcurr) # is currently married

# Generate new variables from original data 
wfhdata <- wfhdata %>%
  mutate(year = wave + 2000,
         job.uslhrs = pmax(job.uslhrs, job.uslhrs.mn, na.rm = TRUE),
         job.uslhrs = if_else(is.na(job.uslhrs), 0, job.uslhrs),
         job.uslhrs.mn = if_else(is.na(job.uslhrs.mn), 0, job.uslhrs.mn),
         jbmhrhw = if_else(jbmhrhw == 997, jbmhrha, jbmhrhw), # replace varying hours code with average hours variable
         wfh.hrs = if_else(wave == 1, jbmhrh, jbmhrhw), # collate changed variable names
         wfh.hrs = if_else(is.na(wfh.hrs), 0, wfh.hrs), # only 30 obs with 0 wfh.hrs otherwise
         wfh.prop = wfh.hrs/job.uslhrs,
         wfh.prop = if_else(is.nan(wfh.prop), 0, wfh.prop),
         wfh.any = if_else(wfh.any == 1, 1, 0),
         wfh.formalagree = if_else(wfh.formalagree == 1, 1, 0),
         COVID = if_else(wave >= 20 & wave <= 21, 1, 0), # a flag for COVID-era waves (2020 & 2021)
         post.pandemic = if_else(wave >= 22, 1, 0), # flag for post-COVID waves (2022-24 so far)
         female = female - 1,
         defacto = if_else(ismarr == 2, 1, 0),
         ismarr = if_else(ismarr == 1, 1, 0),
         unieduc = if_else(educ <= 3, 1, 0), # binary flag for university education or not
         vocateduc = if_else(educ >= 4 & educ <= 5, 1, 0),
         lfs.unemp = if_else(esdtl == 3 | esdtl == 4, 1, 0), # binaries for labour force participation category
         lfs.emp.ft = if_else(esdtl == 1, 1, 0),
         lfs.emp.pt = if_else(esdtl == 2 | esdtl == 7, 1, 0),
         lfs.emp = lfs.emp.ft + lfs.emp.pt,
         lfs.nilf = if_else(esdtl == 5 | esdtl == 6, 1, 0),
         lfs.emp.self = if_else(esempst == 2 | esempst == 3, 1, 0),
         jbmemsz = if_else(jbmemsz >= 8, jbmemsz + 1, jbmemsz),
         jbmemsz = if_else(jbmemsz == 1, 8, jbmemsz),
         emplyr.size = if_else(wave > 21, jbmems2, jbmemsz + 3), # roughly equivalising between changed variables in wave 22
         emplyr.large = if_else(emplyr.size == 8 | emplyr.size == 9 | emplyr.size == 10, 1, 0), # more than 1000 employees across Aus
         emplyr.priv.type = if_else(jbmmply == 1, 1, 0),
         emplyr.govt.type = if_else(jbmmply == 2 | jbmmply == 5, 1, 0),
         emplyr.othr.type = if_else(jbmmply == 3 | jbmmply == 4 | jbmmply == 6, 1, 0),
         job.days.pwk = if_else(jbmday == 1,5, # build a composite variable for days worked from work schedule question
                                if_else(jbmday == 2,4.5,
                                        if_else(jbmday == 3 | jbmday == 4, jbmmth/4,
                                                if_else(jbmday == 8, jbmday1+jbmday2+jbmday3+jbmday4+jbmday5+jbmday6+jbmday7, NA)))),
         job.hrs.pday = job.uslhrs/job.days.pwk,
         wfh.days.est = wfh.hrs/job.hrs.pday,
         wfh.days.count = if_else(wave >= 23 & wfh.any == 0, 0, wfh.days.count),
         job.supervise = if_else(job.supervise == 1, 1, 0),
         job.ind.contractor = if_else(job.ind.contractor == 1, 1, 0),
         job.casual = if_else(job.casual == 1, 1, 0),
         job.overtime.prop = if_else(job.uslhrs-40 > 0,(job.uslhrs-40)/job.uslhrs,0),
         earns.tot = if_else(earns.tot == 0, NA, earns.tot),
         earns.mn = if_else(earns.mn == 0, NA, earns.mn),
         earns.oth = if_else(earns.oth == 0, NA, earns.oth),
         wage.tot = earns.tot/job.uslhrs, # not based on 'usual' earnings 
         wage.mn = earns.mn/job.uslhrs.mn, # based on 'usual' earnings from main job
         newchild = newchild - 1,
         fired = fired - 1,
         famill = famill - 1,
         injury.illness = injury.illness - 1,
         newjob = newjob - 1,
         moved = moved - 1,
         promote = promote - 1,
         retired = retired - 1,
         pregnancy = pregnancy -1,
         gotmarr = gotmarr - 1,
         occup.chg = NA,
         occup.chg = if_else(jbcmocc == 2, 0,
                             if_else(jbcmocc == 3, NA,
                                     if_else(jbcmocc == 1, 1, occup.chg))),
         disab = if_else(disab == 2, 0, disab),
         disab.lmtswk = if_else(helthwk == 1 | helthwk == 3, 1, 0),
         disab.lmtswk = if_else(is.na(disab.lmtswk),0,disab.lmtswk),
         disab.lmtswk.scale = if_else(helthwk == 2, 0, disab.lmtswk.scale), 
         disab.lmtswk.scale = if_else(helthwk == 3, 10, disab.lmtswk.scale),
         disab.cantwk = if_else(helthwk == 3, 1, 0),
         disab.cantwk = if_else(disab == 0, -1, disab.cantwk),
         disab.type.cogni = if_else(disab.type.mental==1 | disab.type.emotn==1, 1, 0),
         disab.type.cogni = if_else(is.na(disab.type.cogni) | disab.type.cogni==0,0,1),
         disab.type.sensory = if_else(disab.type.spch==1 | disab.type.sight==1 | disab.type.hear==1,1,0),
         disab.type.sensory = if_else(is.na(disab.type.sensory) | disab.type.sensory==0,0,1),
         disab.type.physical = if_else(disab.type.blkout==1 | disab.type.arms==1 | disab.type.legs==1 |
                                         disab.type.grip==1 | disab.type.disf==1 | disab.type.breath==1 | 
                                         disab.type.pain==1 | disab.type.restrict==1,1,0),
         disab.type.physical = if_else(is.na(disab.type.physical) | disab.type.physical==0,0,1),
         disab.type.other = if_else(disab.type.other1==1 | disab.type.other2==1 | disab.type.brninj==1 | 
                                      disab.type.refused==1 | disab.type.learn==1 | disab.type.sensory==1 |
                                      disab.type.hdache==1 | disab.type.dknow==1,1,0),
         disab.type.other = if_else(is.na(disab.type.other) | disab.type.other==0,0,1),
         disab.NDISrecpt = if_else(is.na(disab.NDISrecpt) | disab.NDISrecpt==2,0,1),
         home.bdrooms.pp = home.bdrooms / home.resids,
         home.house = if_else(dodtyp == 3, 1, 0),
         home.unit = if_else(dodtyp %in% 8:14, 1, 0),
         home.thouse = if_else(dodtyp %in% 5:7, 1, 0),
         home.other = if_else(home.house ==0 & home.unit ==0 & home.thouse ==0, 1, 0),
         SA1 = as.numeric(SA1)
  )

# Drop transformed variables
wfhdata <- wfhdata %>%
  select(!c("jbmhrhw", "jbmhrha", "jbmhrh", # original WFH variables
            "esdtl", "esempst", # labour force variables
            "jbmems2", "jbmemsz", # firm size
            "jbmmth", grep("^jbmday",colnames(wfhdata)), # working patterns
            "jbcmocc", "jbmmply", # job characteristics
            "wscef", "wscmef", "wscoef", # wage imputation flags
            "wsce", "wscme", "wscoe", # non-imputed wage variables (total, main, other)
            "helthwk", # health conditions preventing work
            "dodtyp" # dwelling type 
  )) 

# Turn binary WFH flag into a factor variable
wfhdata <- wfhdata %>%
  mutate(wfh.any = if_else(is.na(wfh.any), 0, wfh.any),
         wfh.any = factor(wfh.any,
                          labels = c("No WFH", "WFH"),
                          exclude = NULL))

# Build WFH category variables ancross never, always, hybrid split
wfhdata <- wfhdata %>%
  mutate(wfh.fullremote = if_else(wfh.prop == 1, 1, 0),
         wfh.hybrid.high = if_else(wfh.prop < 1 & wfh.prop >= 0.5, 1, 0),
         wfh.hybrid.low = if_else(wfh.prop < 0.5 & wfh.prop > 0, 1, 0),
         wfh.never = if_else(wfh.prop == 0, 1, 0),
         wfh.check = wfh.fullremote + wfh.hybrid.high + wfh.hybrid.low + wfh.never) # check variable
wfhdata %>% 
  count(wfh.check > 1)
wfhdata <- wfhdata %>%
  select(!wfh.check)
# Convert WFH dummies to single factor (categorical) variable
wfhdata <- wfhdata %>%
  mutate(wfh.category = if_else(wfh.never == 1, 0,
                                if_else(wfh.hybrid.low == 1, 1,
                                        if_else(wfh.hybrid.high == 1, 2,
                                                if_else(wfh.fullremote == 1, 3, NA)))),
         wfh.category = factor(wfh.category,
                               labels = c("Never", "Low hybrid", "High hybrid", "Fully remote", NA),
                               exclude = NULL))


# Translate 1-digit, 2-digit and 4-digit occupation codes to usable labels
tally <- wfhdata %>%
  count(job.occ.1, job.occ.2, job.occ)
attr(wfhdata$job.occ.1, "labels")
occ.labels <- attr(attr(wfhdata$job.occ.1, "labels")[11:18], "names")
wfhdata <- wfhdata %>%
  mutate(job.occ.1 = factor(job.occ.1, 
                            levels = attr(wfhdata$job.occ.1, "labels")[11:18],
                            labels = occ.labels,
                            exclude = NULL))
occ.labels <- attr(attr(wfhdata$job.occ.2, "labels")[11:61], "names")
wfhdata <- wfhdata %>%
  mutate(job.occ.2 = factor(job.occ.2, 
                            levels = attr(wfhdata$job.occ.2, "labels")[11:61],
                            labels = occ.labels,
                            exclude = NULL))
occ.labels <- attr(attr(wfhdata$job.occ, "labels")[11:516], "names")
wfhdata <- wfhdata %>%
  mutate(job.occ = factor(job.occ, 
                          levels = attr(wfhdata$job.occ, "labels")[11:516],
                          labels = occ.labels,
                          exclude = NULL))

tally <- wfhdata %>%
  count(job.occ.1, job.occ.2, job.occ)
remove(occ.labels)

# Translate 1-digit and 2-digit industry codes to usable labels
tally <- wfhdata %>%
  count(job.ind.1, job.ind.2)
attributes(wfhdata$job.ind.1)
ind.labels <- attr(attr(wfhdata$job.ind.1, "labels")[11:29], "names")
wfhdata <- wfhdata %>%
  mutate(job.ind.1 = factor(job.ind.1, 
                            levels = attr(wfhdata$job.ind.1, "labels")[11:29],
                            labels = ind.labels,
                            exclude = NULL))
ind.labels <- attr(attr(wfhdata$job.ind.2, "labels")[11:96], "names")
wfhdata <- wfhdata %>%
  mutate(job.ind.2 = factor(job.ind.2, 
                            levels = attr(wfhdata$job.ind.2, "labels")[11:96],
                            labels = ind.labels,
                            exclude = NULL))
tally <- wfhdata %>%
  count(job.ind.1,job.ind.2)
wfhdata <- wfhdata %>% # replace missing 2-dig industry with 1-dig labels
  mutate(job.ind.2 = if_else(is.na(job.ind.2),job.ind.1,job.ind.2))
remove(ind.labels)

# Translate remoteness area codes to usable labels and create binary regional/urban variable
tally <- wfhdata %>%
  count(RA.2021)
attributes(wfhdata$RA.2021)
RA.labels <- attr(attr(wfhdata$RA.2021, "labels")[11:15], "names")
wfhdata <- wfhdata %>%
  mutate(RA.2021 = factor(RA.2021, 
                          levels = attr(wfhdata$RA.2021, "labels")[11:15],
                          labels = RA.labels,
                          exclude = NULL),
         urban = if_else(RA.2021 == "[0] Major Cities of Australia", 1, 0),
         innr.region = if_else(RA.2021 == "[1] Inner Regional Australia",1,0))
tally <- wfhdata %>%
  count(RA.2021, urban)
remove(RA.labels)

# Translate GCCSA area codes to usable labels
tally <- wfhdata %>%
  count(GCC.2021)
attributes(wfhdata$GCC.2021)
GCC.labels <- attr(attr(wfhdata$GCC.2021, "labels")[11:23], "names")
wfhdata <- wfhdata %>%
  mutate(GCC.2021 = factor(GCC.2021, 
                           levels = attr(wfhdata$GCC.2021, "labels")[11:23],
                           labels = GCC.labels,
                           exclude = NULL))
tally <- wfhdata %>%
  count(GCC.2021)
remove(GCC.labels, tally)

# Translate state variable into labels
tally <- wfhdata %>%
  count(state)
attributes(wfhdata$state)
state.labels <- attr(attr(wfhdata$state, "labels")[11:18], "names")
wfhdata <- wfhdata %>%
  mutate(state = factor(state, 
                        levels = attr(wfhdata$state, "labels")[11:18],
                        labels = state.labels,
                        exclude = NULL))
tally <- wfhdata %>%
  count(state)
remove(state.labels, tally)

# Build variable for distance from SA1 to nearest city centre
sa1mapdata <- absmapsdata::sa12011 # Import SA1 map data
glimpse(sa1mapdata)
sa1mapdata <- as.data.frame(sa1mapdata) %>%
  filter(!is.na(cent_lat)) %>% # Remove offshore shipping and no usual address codes 
  select(sa1_7dig_2011, cent_long, cent_lat)
sa1mapdata <- st_as_sf(sa1mapdata, coords = c("cent_long","cent_lat"), crs = 4326)
cities <- st_as_sf(read.csv(paste0(base_url, "nonHILDA_data_sources/city_coords.csv")), coords = c("X","Y"), crs = 4326) # Import city centre coordinates (separate file)
sa1mapdata <- sa1mapdata %>% # Calculate distances to each city centre (divide by 1000 for kms)
  mutate(dist.Syd = as.numeric(st_distance(sa1mapdata, cities[1,])/1000),
         dist.Melb = as.numeric(st_distance(sa1mapdata, cities[2,])/1000),
         dist.Bris = as.numeric(st_distance(sa1mapdata, cities[3,])/1000),
         dist.Per = as.numeric(st_distance(sa1mapdata, cities[4,])/1000),
         dist.Adel = as.numeric(st_distance(sa1mapdata, cities[5,])/1000),
         dist.Hob = as.numeric(st_distance(sa1mapdata, cities[6,])/1000),
         dist.Canb = as.numeric(st_distance(sa1mapdata, cities[7,])/1000),
         dist.Dar = as.numeric(st_distance(sa1mapdata, cities[8,])/1000))
sa1mapdata <- as.data.frame(sa1mapdata) %>%
  group_by(sa1_7dig_2011) %>%
  mutate(dist.nearstcity = min(dist.Syd,dist.Melb,dist.Bris,dist.Per,dist.Adel,dist.Hob,dist.Canb,dist.Dar),
         sa1_7dig_2011 = as.numeric(sa1_7dig_2011)) %>%
  select(sa1_7dig_2011,dist.nearstcity)
wfhdata <- wfhdata %>%
  left_join(sa1mapdata, by = c("SA1" = "sa1_7dig_2011"))
rm(sa1mapdata, cities)

# Generate bins for youngest child's age
wfhdata <- wfhdata %>%
  mutate(yng.child = if_else(is.na(child.yngst) | child.yngst > 12,0,1),
         child.yngst.bin = if_else(is.na(child.yngst) & child.resid == 0, "No resident children", 
                                   if_else(child.yngst >= 18, "Resident adult child", 
                                           if_else(child.yngst >= 13 & child.yngst <=17, "High school (13-17)",
                                                   if_else(child.yngst >= 6 & child.yngst <= 12, "Primary school (6-12)",
                                                           if_else(child.yngst >=0 & child.yngst <= 5, "Pre-school (0-5)", NA))))),
         child.yngst.bin = factor(child.yngst.bin, 
                                  levels = c("No resident children", "Pre-school (0-5)", "Primary school (6-12)", "High school (13-17)", "Resident adult child"),
                                  exclude = NULL))

# Generate bins for parent category
wfhdata <- wfhdata %>%
  mutate(parent.cat = if_else(female==0 & (is.na(child.yngst) | child.yngst>=18), "Childless men",
                              if_else(female==1 & (is.na(child.yngst) | child.yngst>=18), "Childless women", 
                                      if_else(child.yngst<18 & child.resid>0 & female==0, "Fathers", 
                                              if_else(child.yngst<18 & child.resid>0 & female==1, "Mothers", NA)))),
         parent.cat = factor(parent.cat, 
                             levels = c("Childless men", "Childless women", "Fathers", "Mothers"),
                             exclude = NA))

# Build categorical variable for pandemic periods
wfhdata <- wfhdata %>%
  mutate(pandemic.cat = if_else(year <= 2019, "Pre-pandemic period",
                                if_else(COVID == 1, "Pandemic period",
                                        if_else(post.pandemic == 1, "Post-pandemic period", NA))),
         pandemic.cat = factor(pandemic.cat, 
                               levels = c("Pre-pandemic period", "Pandemic period", "Post-pandemic period"),
                               exclude = NULL))

# Code in categorical labels for disab.lmtswk.cat and build disability spell duration variable
wfhdata <- wfhdata %>%
  mutate(disab.lmtswk.scale = if_else(disab == 0, -1, disab.lmtswk.scale),
         disab.lmtswk.cat = if_else(disab==0,-1,disab.lmtswk),
         disab.lmtswk.cat = factor(disab.lmtswk.cat,
                                   labels = c("No disability", "Disability does not limit work", "Disability limits work"),
                                   exclude = NA),
         disab.type = if_else(disab==0,0,
                              if_else(disab.type.cogni==1 & disab.type.physical==1 & disab.type.other==1,1,
                                      if_else(disab.type.cogni==1 & disab.type.physical==1 & disab.type.other==0,2,
                                              if_else(disab.type.cogni==1 & disab.type.physical==0 & disab.type.other==1,3,
                                                      if_else(disab.type.cogni==0 & disab.type.physical==1 & disab.type.other==1,4,
                                                              if_else(disab.type.cogni==1 & disab.type.physical==0 & disab.type.other==0,5,
                                                                      if_else(disab.type.cogni==0 & disab.type.physical==1 & disab.type.other==0,6,
                                                                              if_else(disab.type.cogni==0 & disab.type.physical==0 & disab.type.other==1,7,NA)))))))),
         disab.type = factor(disab.type, labels = c("No disability", "Psychosocial, physical & other", "Psychosocial & physical",
                                                    "Psychosocial & other", "Physical & other", "Psychosocial only", "Physical only", "Other only")))
wfhdata <- wfhdata %>% 
  arrange(xwaveid, year) %>% 
  group_by(xwaveid) %>% 
  mutate(disab.onset = disab == 1 & dplyr:::lag(disab, default = 0) == 0, # mark each NEW spell
         disab.ceased = disab == 0 & dplyr:::lag(disab, default = 0) == 1, # mark end of each spell
         disab.spell.no = cumsum(disab.onset)) %>%         # running ID of spells
  group_by(xwaveid, disab.spell.no) %>%                    # work within each spell
  mutate(disab.duration = case_when(disab == 1 ~ row_number() - 1L, disab == 0 ~ 0L)) %>%
  ungroup() ### lag function makes it slow here ###

# Generate 10-year bins for age
wfhdata <- wfhdata %>%
  mutate(age.cat = if_else(age<=25, 0,
                           if_else(age>25 & age <=35, 1, 
                                   if_else(age>35 & age <=45, 2, 
                                           if_else(age>45 & age <=55, 3, 
                                                   if_else(age>55 & age <=65, 4, 
                                                           if_else(age >65, 5, NA)))))),
         age.cat = factor(age.cat, 
                          labels = c("25 or under", "26 to 35", "36 to 45", "46 to 55", "56 to 65", "Over 65"),
                          exclude = NA))

# Generate 5-year bins for age
wfhdata <- wfhdata %>%
  mutate(age.cat.5yr = if_else(age<=20, 0,
                               if_else(age>20 & age <=25, 1, 
                                       if_else(age>25 & age <=30, 2, 
                                               if_else(age>30 & age <=35, 3, 
                                                       if_else(age>35 & age <=40, 4, 
                                                               if_else(age>40 & age <=45, 5, 
                                                                       if_else(age>45 & age <=50, 6, 
                                                                               if_else(age>50 & age <=55, 7, 
                                                                                       if_else(age>55 & age <=60, 8, 
                                                                                               if_else(age>60 & age <=65, 9, 
                                                                                                       if_else(age>65, 10, NA))))))))))),
         age.cat.5yr = factor(age.cat.5yr, 
                              labels = c("< 21","21-25","26-30","31-35","36-40","41-45","46-50",
                                         "51-55","56-60","61-65","> 65"),
                              exclude = NA))

# Generate bins for tenure
wfhdata <- wfhdata %>%
  mutate(tenure.cat = if_else(job.tenure<1, 0,
                              if_else(job.tenure>=1 & job.tenure <2, 1, 
                                      if_else(job.tenure>=2 & job.tenure <3, 2, 
                                              if_else(job.tenure>=3 & job.tenure <5, 3, 
                                                      if_else(job.tenure>=5 & job.tenure <10, 4, 
                                                              if_else(job.tenure >=10, 5, NA)))))),
         tenure.cat = factor(tenure.cat, 
                             labels = c("< 1 year", "1-2 years", "2-3 years", "3-5 years", "5-10 years", "10+ years"),
                             exclude = NA))

### Data check for impossible outcomes
# Proportion of time spent WFH is greater than 1
problemwfhdata <- wfhdata %>%
  filter(wfh.prop > 1)
# Fix: drop those obs
wfhdata <- wfhdata %>%
  mutate(wfh.prop = if_else(wfh.prop > 1, 1, wfh.prop),
         wfh.hrs = if_else(wfh.hrs > job.uslhrs, job.uslhrs, wfh.hrs))
remove(problemwfhdata)

# disability that can't work, but usl hrs > 0
problemdisabdata <- wfhdata %>%
  filter(disab.cantwk == 1 & job.uslhrs > 0)
# Fix: none
remove(problemdisabdata)

# Infinite wage outcomes
problemwagedata <- wfhdata %>%
  filter(is.infinite(wage.tot))
wfhdata %>%
  count(is.infinite(wage.tot), is.infinite(wage.mn)) # not a total earning v main job issue
problemwagedata %>%
  count(wfh.hrs, job.uslhrs) # all problematic data has zero work hours and zero WFH hours
# Fix: over-ride wages to zero, as it must be an error in the current wages field (probably mis-classified income?)
wfhdata <- wfhdata %>%
  mutate(wage.tot = if_else(is.infinite(wage.tot), 0, wage.tot),
         wage.mn = if_else(is.infinite(wage.mn), 0, wage.mn))
remove(problemwagedata)





### Link Dingel-Neiman scores to HILDA occupations and obtain 2016 occupation weightings
# Import Dingel-Neiman scores pre-crosswalked to Australian occupations
Occup_DN <- read.csv(paste0(base_url, "nonHILDA_data_sources/DN_score_ANZSCO_4dig_output.csv"))

# Sum total workers at the 4-digit ANZSCO level
Occup_DN <- Occup_DN %>%
  rename(ANZSCO_4digcat = ANZSCO_4dig) %>%
  mutate(ANZSCO_subgroup = substr(as.character(ANZSCO_4digcat),1,2)) %>%
  group_by(ANZSCO_4digcat) %>%
  mutate(ANZSCO_4dig_total = sum(Census16_total_working)) %>%
  ungroup() %>%
  mutate(Census_occ_weights = ANZSCO_4dig_total/sum(Census16_total_working))

# Collapse the dataframe to the first entry for each 4-digit level
Occup_DN <- Occup_DN[,c(1,3,7:13)] %>%
  group_by(ANZSCO_4digcat) %>%
  slice(1) %>%
  ungroup()  

# Summarise the occupation data in HILDA 
HILDA.occ <- wfhdata %>% 
  filter(!is.na(job.occ)) %>%
  count(job.occ) %>%
  mutate(ANZSCO_4digcat = as.numeric(substr(as.character(job.occ),2,5))) %>% 
  left_join(Occup_DN[,c(2,5:9)], by = join_by(ANZSCO_4digcat), keep = F) %>%
  mutate(Census_occ_weights = if_else(is.na(Census_occ_weights), 0, Census_occ_weights),
         HILDA_occ_weights = n/sum(n),
         occ.weights = Census_occ_weights/HILDA_occ_weights) %>%
  fill(everything(), .direction = "up")

# Join back to main dataset
wfhdata <- wfhdata %>%
  left_join(HILDA.occ[,c(1,3:9)], by = join_by(job.occ), keep = F)

# Clean the data environment
rm(list = c("HILDA.occ", "Occup_DN"))




### Add partner occupation, employment status, gender, WFHany flag and WFH prop
prtnrdata <- wfhdata %>%
  select(xwaveid, wave,
         prtnr_job.occ = job.occ.2, prtnr_lfs.emp.ft = lfs.emp.ft, prtnr_lfs.emp.pt = lfs.emp.pt, 
         prtnr_lfs.emp.self = lfs.emp.self,prtnr_lfs.nilf = lfs.nilf, prtnr_lfs.unemp = lfs.unemp, 
         prtnr_wfh.any = wfh.any, prtnr_wfh.prop = wfh.prop, prtnr_DNscore.man = DNscore_manual, prtnr_DNscore.onet = DNscore_onet,
         prtnr_female = female, prtnr_disab = disab, prtnr_disab.lmtswk = disab.lmtswk, prtnr_unieduc = unieduc)
wfhdata <- wfhdata %>%
  left_join(prtnrdata, by = c("hhpxid" = "xwaveid", "wave" = "wave")) %>%
  mutate(prtnr_any = if_else(!is.na(hhpxid), 1, 0),
         prtnr_wfh.any_all = if_else(prtnr_wfh.any == "No WFH" | is.na(prtnr_wfh.any), 0, 1),
         prtnr_lfs.emp_all = if_else(prtnr_any == 1, prtnr_lfs.emp.ft + prtnr_lfs.emp.pt,0),
         prtnr_lfs.emp_all = if_else(is.na(prtnr_lfs.emp_all),0,prtnr_lfs.emp_all),
         prtnr_female_all = if_else(prtnr_female == "0" | is.na(prtnr_female), 0, 1),
         prtnr_lfs.emp = prtnr_lfs.emp.ft + prtnr_lfs.emp.pt
  )
rm(prtnrdata)



# Save the result
save(wfhdata, file = "wfh_workingdata_unbalanced_complete.Rdata")




### Drop data out of scope for employee analysis
wfhdata.employees <- wfhdata 

## Self-employed :
# Examine self-employed split
wfhdata.employees %>% 
  count(lfs.emp.self)
selfemp.obs <- wfhdata.employees %>%
  filter(lfs.emp.self == 1)
# Drop self-employed 
wfhdata.employees <- wfhdata.employees %>% 
  filter(lfs.emp.self == 0 | is.na(lfs.emp.self))
remove(selfemp.obs)

## Not in labour force (NILF) and unemployed:
# Examine NILF, unemployed and NA split
wfhdata.employees %>%
  count(lfs.nilf, lfs.unemp, wfh.prop == 0)
NILF.obs <- wfhdata.employees %>%
  filter(lfs.nilf == 1 | lfs.unemp == 1)
# Drop NILF, unemployed and unknown 
wfhdata.employees <- wfhdata.employees %>% 
  filter(lfs.nilf == 0 & lfs.unemp == 0)
remove(NILF.obs)


# Save the result
save(wfhdata.employees, file = "wfh_workingdata_unbalanced_employees.Rdata")




#### FINAL SAMPLE CONSTRUCTION AND DROPPED OBSERVATION COUNTS ####
covars <- c("wfh.any", "wfh.prop", "age.cat.5yr", "unieduc", "vocateduc", "female", "prtnr_any", 
            "prtnr_lfs.emp_all", "prtnr_wfh.any_all", "child.resid", "child.yngst.bin", "disab", 
            "disab.lmtswk", "disab.lmtswk.scale", "disab.type.cogni", "disab.duration", "wage.mn", 
            "lfs.emp.pt", "job.casual", "job.tenure", "job.supervise", "job.dist", "DNscore_manual",
            "satisfact.life", "jobsat.ovrall", "satisfact.home", "home.bdrooms.pp", "urban",
            "innr.region", "dist.nearstcity", "state")

# Basic obs counts and missingness checks
wfhdata %>% filter(year >= 2017) %>% 
  summarise(n=n(), #total obs in HILDA 2017-2024
            n.ind=n_distinct(xwaveid)) #individuals in HILDA 2017-2024
wfhdata %>% filter(year >= 2017,lfs.nilf==1 | lfs.unemp==1) %>% 
  count() #obs that are not employed
wfhdata %>% filter(year >= 2017) %>% count(lfs.emp) #obs that are employed
wfhdata %>% filter(year >= 2017,lfs.emp.self==1) %>% 
  count() #obs that are self-employed
wfhdata %>% filter(year >= 2017) %>% count(lfs.emp==1 & lfs.emp.self==0) #obs that are employees
wfhdata %>% filter(year >= 2017, lfs.nilf == 0 & lfs.unemp == 0) %>%
  group_by(pandemic.cat, lfs.emp.self) %>%
  summarise(obs = n(),
            wfh.any = sum(wfh.any == "WFH")/obs, 
            wfh.prop = mean(wfh.prop, na.rm = T)) # Summary of self-employed WFH uptake by pandemic period
wfhdata %>% filter(year >= 2017,lfs.nilf==0 & lfs.unemp==0 & lfs.emp.self==0) %>% 
  count(age<15, age >69) #obs outside 15-69 age bracket
wfhdata %>% filter(year >= 2017, lfs.emp==1 & lfs.emp.self==0) %>% count(age<15, age>69) #obs that are working-age employees

### Build baseline df of full sample for analysis
df <- wfhdata.employees %>% 
  filter(year >= 2017, age >=15, age <=69)

# Further obs counts and missingness checks
missing <- df %>% filter(if_any(all_of(covars), ~is.na(.x))) %>% nrow() #count rows with any missing obs
missing/(df %>% count()) #proportion missing
missing <- df %>% summarise(across(covars, ~ sum(is.na(.x)), .names = "n_miss_{.col}")) #summary of missing obs by variable
df %>% count(is.na(wage.mn), is.na(job.dist)) #counts for main missing variables and overlaps
(df %>% filter(is.na(job.traveltime)) %>% nrow())/(df %>% count()) # share with missing travel time
# df %>% drop_na(covars) %>% count(is.na(wfh.formalagree), wfh.any) #counts for missing WFH formal agreement variable
# df %>% count(is.na(emplyr.large)) #count for firm size missingness
df %>% drop_na(covars) %>% count(is.na(job.ind.1),is.na(job.occ.1)) #count missing industry and occupation FE
df %>% drop_na(covars) %>% summarise(n=n(), #total obs in final sample
                                     n.ind=n_distinct(xwaveid)) #individuals in final sample

### Save baseline df with listwise deletion of missing variables and new WFHmost variable
df <- wfhdata.employees %>% 
  filter(year >= 2017, age >=15, age <=69) %>% 
  drop_na(all_of(covars)) %>%
  mutate(wfh.any = if_else(wfh.any=="WFH",1,0),
         wfh.most = if_else(wfh.prop >= 0.5,1,0),
         wfh.most.cat = if_else(wfh.most == 1, 1,
                                if_else(wfh.any == 1, 0, -1)),
         wfh.most.cat = factor(wfh.most.cat, labels = c("No WFH", "Some WFH", "Mostly WFH")),
         POST = if_else(pandemic.cat == "Post-pandemic period",1,0),
         PAND = if_else(pandemic.cat == "Pandemic period",1,0),
         PRE = if_else(pandemic.cat == "Pre-pandemic period",1,0),
         dist.nearstcity = dist.nearstcity/10,
         job.dist = job.dist/10,
         disab.lmtswk = if_else(disab.lmtswk == "Disability limits work",1,0),
         disab.lmtswk.severe = if_else(disab.lmtswk.scale > 5, 1, 0),
         child.pres = if_else(child.yngst.bin == "Pre-school (0-5)",1,0),
         child.prim = if_else(child.yngst.bin == "Primary school (6-12)",1,0),
         child.high = if_else(child.yngst.bin == "High school (13-17)",1,0),
         job.ind.1 = fct_na_value_to_level(job.ind.1, level = "Unknown"),
         job.ind.2 = fct_na_value_to_level(job.ind.2, level = "Unknown"),
         job.occ = fct_na_value_to_level(job.occ, level = "Unknown"),
         job.occ.1 = fct_na_value_to_level(job.occ.1, level = "Unknown"))

