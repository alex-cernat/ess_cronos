

# Project looking at how measurement error is different
# in Cronos and ESS8
#
# RQ1: Are there differences in data quality between face to face and Web
# RQ2: Are differences explained by mode?
# RW3: Are differences explained by cognitive ability or experience?
#


# clear working space
rm(list = ls ())



# Admin -------------------------------------------------------------------

# folders for installed packages

# .libPaths(c(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox (The University of Manchester)/R/package")))


# create folders and unzip
# dir.create("./data")
# dir.create("./output")
# dir.create("./functions")


# install packages and load

# use packrat to install packages localy

pkg <- c("tidyverse", "ggthemes", "haven", "reshape2",
         "MplusAutomation", "lavaan", "semTools")

#install.packages(pkg)
sapply(pkg, library, character.only = T)

# load local functions
map(str_c("./functions/",
          list.files("./functions/")),
    source)



# Import data -------------------------------------------------------------

data0 <- read_dta("./data/CRONOS_ESS8_e01_1.dta")
data0_admin <- read_dta("./data/CRONOS_Administrative_data_e01_1.dta")
data0_paradata <- read_dta("./data/CRONOS_Paradata_Basic_e01.dta")
ess8 <- read_dta("./data/ESS8MTMMe02.dta")



data1 <- data0 %>%
  left_join(data0_admin, by = "idno") %>%
  left_join(data0_paradata, by = "idno") %>%
  select(-cntry.x, -cntry.y)

# get immigration quesiton from mtmm experiment

ess_small <- ess8 %>%
  select(idno, cntry, testgc33:testgc35)

data2 <- left_join(data1, ess_small)





# Clean data --------------------------------------------------------------

# cogitive ability
desc_tab(data2$agea)
desc_tab(data2$eduyrs)
desc_tab(data2$w0q44)



# internet and PC use
desc_tab(data2$w0q40)
desc_tab(data2$w0q41)

desc_tab(data2$w0q43)





data2 %>%
  mutate(survey = ifelse(w0q43 > 20, 20, w0q43)) %>%
  ggplot(aes(survey)) + geom_density()

# device

desc_tab(data2$w5main_device)
desc_tab(data2$w6main_device)


desc_tab(data2$tablet)
table(data2$tablet, data2$w5main_device)


# social desirability
desc_tab(data2$w0q48a)



# attitudes towards immigration
desc_tab(data2$w5q44)
desc_tab(data2$w5q46)
desc_tab(data2$w5q48)

# clean ess vars

data3 <- data2 %>%
  mutate_at(vars(matches("testgc")),
            funs(ifelse(. > 10, NA, .)))

desc_tab(data3$testgc33)
desc_tab(data3$testgc34)
desc_tab(data3$testgc35)

# correlation
vars_int1 <- c(str_c("w5q4", c(4, 6, 8)),
               str_c("testgc3", 3:5))
round(cor(data3[vars_int1],
          use = "pairwise.complete.obs"), 2)



# Political ability

desc_tab(data3$w5q38)
desc_tab(data3$w5q39)
desc_tab(data3$w5q40)
desc_tab(data3$w5q41)




desc_tab(data3$psppsgva)
desc_tab(data3$actrolga)
desc_tab(data3$psppipla)
desc_tab(data3$cptppola)


rtab <- function(data, var1, var2) {

  print(attributes(data[[var1]])$label)
  print(attributes(data[[var1]])$labels)
  print(attributes(data[[var2]])$label)
  print(attributes(data[[var2]])$labels)

  round(
    prop.table(
      table(data[[var1]], data[[var2]], useNA = "always"),
      1),
    2)
}

print(attributes(data3$psppsgva)$labels)


var_int5 <- c(str_c("w5q", 38:41),
              "psppsgva", "actrolga", "psppipla", "cptppola")

# cross-tab
rtab(data3, "psppsgva", "w5q38")
rtab(data3, "actrolga", "w5q39")
rtab(data3, "psppipla", "w5q40")
rtab(data3, "cptppola", "w5q41")






# Political participation
desc_tab(data3$w5q32)
desc_tab(data3$w5q33)
desc_tab(data3$w5q34)
desc_tab(data3$w5q35)
desc_tab(data3$w5q36)
desc_tab(data3$w5q37)

desc_tab(data3$wrkprty)
desc_tab(data3$wrkorg)
desc_tab(data3$badge)
desc_tab(data3$sgnptit)
desc_tab(data3$pbldmn)
desc_tab(data3$pstplonl)


# object
var_int6 <- c(str_c("w5q3", 2:7),
              "wrkprty", "wrkorg", "badge", "sgnptit", "pbldmn", "pstplonl")

# cross-tab
rtab(data3, "wrkprty", "w5q32")
rtab(data3, "wrkorg", "w5q33")
rtab(data3, "badge", "w5q34")
rtab(data3, "sgnptit", "w5q35")
rtab(data3, "pbldmn", "w5q36")
rtab(data3, "pstplonl", "w5q37")


# Social trust
desc_tab(data3$w5q57)
desc_tab(data3$w5q58)
desc_tab(data3$w5q59)



data3 <- data3 %>%
  mutate_at(vars(w6q5, w6q6, w6q7),
            funs(ifelse(. < 0, NA, .)))

desc_tab(data3$w6q5)
desc_tab(data3$w6q6)
desc_tab(data3$w6q7)

desc_tab(data3$ppltrst)
desc_tab(data3$pplfair)
desc_tab(data3$pplhlp)


# correlation
vars_int2 <- c(str_c("w5q5", 7:9),
               str_c("w6q", 5:7),
               "ppltrst", "pplfair", "pplhlp")
round(cor(data3[vars_int2],
          use = "pairwise.complete.obs"), 2)


# Social benefits
desc_tab(data3$w5q63)
desc_tab(data3$w5q64)
desc_tab(data3$w5q65)
desc_tab(data3$w5q66)


desc_tab(data3$sbstrec)
desc_tab(data3$sbprvpv)
desc_tab(data3$sbeqsoc)
desc_tab(data3$sbbsntx)

vars_int3 <- c(str_c("w5q6", 3:6),
               "sbstrec", "sbprvpv", "sbeqsoc", "sbbsntx")
round(cor(data3[vars_int3],
          use = "pairwise.complete.obs"), 2)





# Trust institutions

data3 <- data3 %>%
  mutate_at(vars(str_c("w6q", 9:14)),
            funs(ifelse(. < 0, NA, .)))


desc_tab(data3$w6q9)
desc_tab(data3$w6q10)
desc_tab(data3$w6q11)
desc_tab(data3$w6q12)
desc_tab(data3$w6q13)
desc_tab(data3$w6q14)


desc_tab(data3$trstlgl)
desc_tab(data3$trstplc)
desc_tab(data3$trstplt)
desc_tab(data3$trstprt)
desc_tab(data3$trstep)
desc_tab(data3$trstun)


# corr
vars_int4 <- c(str_c("w6q", 9:14),
               "trstlgl", "trstplc", "trstplt", "trstprt", "trstep", "trstun")
round(cor(data3[vars_int4],
          use = "pairwise.complete.obs"), 2)


# selection variables

desc_tab(data3$w2disp_status)

data3 <- data3 %>%
  mutate(w5part = ifelse(w5disp_status > 16, 1, 0),
         w6part = ifelse(w6disp_status > 16, 1, 0))

rtab(data3, "w5disp_status", "w6disp_status")


# Data quality ------------------------------------------------------------


# Missing
vars_int <- c(var_int5, var_int6, vars_int1, vars_int2, vars_int3, vars_int4)


vars_int_ess <- vars_int[!str_detect(vars_int, "^w[0-9]q")]



vars_int_cronos5 <- vars_int[str_detect(vars_int, "^w5q")]
vars_int_cronos6 <- vars_int[str_detect(vars_int, "^w6q")]


# exclude mtmm variables
vars_int_ess <- vars_int_ess[!str_detect(vars_int_ess, "testg")]
vars_int_cronos5 <- vars_int_cronos5[!str_detect(vars_int_cronos5, "w5q4[468]")]

vars_int_ess5 <- vars_int_ess[!str_detect(vars_int_ess, "^trst")]
vars_int_ess6 <- vars_int_ess[str_detect(vars_int_ess, "^ppl|^trst")]


# compare wave 5 missing
data3 %>%
  filter(w5part == 1) %>%
  select(idno, cntry, vars_int_ess5) %>%
  mutate_at(vars(vars_int_ess5),
               funs(m = is.na(.))) %>%
  select(-vars_int_ess5) %>%
  mutate(sum_m = rowSums(.[3:19])) %>%
  group_by(cntry) %>%
  summarise(mean = mean(sum_m))


data3 %>%
  filter(w5part == 1) %>%
  select(idno, cntry, vars_int_cronos5) %>%
  mutate_at(vars(vars_int_cronos5),
            funs(m = is.na(.))) %>%
  select(-vars_int_cronos5) %>%
  mutate(sum_m = rowSums(.[3:19])) %>%
  group_by(cntry) %>%
  summarise(mean = mean(sum_m))


# compare wave 6 missing
data3 %>%
  filter(w6part == 1) %>%
  select(idno, cntry, vars_int_ess6) %>%
  mutate_at(vars(vars_int_ess6),
            funs(m = is.na(.))) %>%
  select(-vars_int_ess6) %>%
  mutate(sum_m = rowSums(.[3:11])) %>%
  group_by(cntry) %>%
  summarise(mean = mean(sum_m))


data3 %>%
  filter(w6part == 1) %>%
  select(idno, cntry, vars_int_cronos6) %>%
  mutate_at(vars(vars_int_cronos6),
            funs(m = is.na(.))) %>%
  select(-vars_int_cronos6) %>%
  mutate(sum_m = rowSums(.[3:11])) %>%
  group_by(cntry) %>%
  summarise(mean = mean(sum_m))


# save individual scores in new data


# compare wave 5 missing
miss1 <- data3 %>%
  filter(w5part == 1) %>%
  select(idno, cntry, vars_int_ess5) %>%
  mutate_at(vars(vars_int_ess5),
            funs(m = is.na(.))) %>%
  select(-vars_int_ess5) %>%
  mutate(miss_ess5 = rowSums(.[3:19])) %>%
  select(idno, cntry, miss_ess5)

miss2 <- data3 %>%
  filter(w5part == 1) %>%
  select(idno, cntry, vars_int_cronos5) %>%
  mutate_at(vars(vars_int_cronos5),
            funs(m = is.na(.))) %>%
  select(-vars_int_cronos5) %>%
  mutate(miss_cro5 = rowSums(.[3:19])) %>%
  select(idno, cntry, miss_cro5)


# compare wave 6 missing
miss3 <- data3 %>%
  filter(w6part == 1) %>%
  select(idno, cntry, vars_int_ess6) %>%
  mutate_at(vars(vars_int_ess6),
            funs(m = is.na(.))) %>%
  select(-vars_int_ess6) %>%
  mutate(miss_ess6 = rowSums(.[3:11])) %>%
  select(idno, cntry, miss_ess6)



miss4 <- data3 %>%
  filter(w6part == 1) %>%
  select(idno, cntry, vars_int_cronos6) %>%
  mutate_at(vars(vars_int_cronos6),
            funs(m = is.na(.))) %>%
  select(-vars_int_cronos6) %>%
  mutate(miss_cro6 = rowSums(.[3:11])) %>%
  select(idno, cntry, miss_cro6)


miss_data <- full_join(miss1, miss2, by = c("idno", "cntry")) %>%
  full_join(miss3) %>%
  full_join(miss4)



# resp style political ability --------------------------------------------

summ_data <- data3 %>%
  filter(w5part == 1) %>%
  select(idno, cntry, var_int5) %>%
  mutate(polab_prim_ess8 = ifelse(psppsgva == 1, 1, 0) +
           ifelse(actrolga == 1, 1, 0) +
           ifelse(psppipla == 1, 1, 0) +
           ifelse(cptppola == 1, 1, 0),
         polab_rec_ess8 = ifelse(psppsgva == 5, 1, 0) +
           ifelse(actrolga == 5, 1, 0) +
           ifelse(psppipla == 5, 1, 0) +
           ifelse(cptppola == 5, 1, 0),
         polab_prim_cro = ifelse(w5q38 == 1, 1, 0) +
           ifelse(w5q39 == 1, 1, 0) +
           ifelse(w5q40 == 1, 1, 0) +
           ifelse(w5q41 == 1, 1, 0),
         polab_rec_cro = ifelse(w5q38 == 5, 1, 0) +
           ifelse(w5q39 == 5, 1, 0) +
           ifelse(w5q40 == 5, 1, 0) +
           ifelse(w5q41 == 5, 1, 0))



summ_data %>%
  group_by(cntry) %>%
  summarise(round(mean(polab_prim_ess8, na.rm = T), 2))

summ_data %>%
  group_by(cntry) %>%
  summarise(round(mean(polab_prim_cro, na.rm = T), 2))


summ_data %>%
  group_by(cntry) %>%
  summarise(round(mean(polab_rec_ess8, na.rm = T), 2))

summ_data %>%
  group_by(cntry) %>%
  summarise(round(mean(polab_rec_cro, na.rm = T), 2))





# summ_data %>%
#   group_by(cntry) %>%
#   select(matches("polab")) %>%
#   do(desc_tab2(.$polab_prim_ess8, useNA_opt = "no")) %>%
#   filter(Code == 4)
#
# summ_data %>%
#   group_by(cntry) %>%
#   select(matches("polab")) %>%
#   do(desc_tab2(.$polab_prim_cro, useNA_opt = "no")) %>%
#   filter(Code == 4)
#
# summ_data %>%
#   group_by(cntry) %>%
#   select(matches("polab")) %>%
#   do(desc_tab2(.$polab_rec_ess8, useNA_opt = "no")) %>%
#   filter(Code == 0) %>%
#   mutate(dif = 100 - Perc.)
#
# summ_data %>%
#   group_by(cntry) %>%
#   select(matches("polab")) %>%
#   do(desc_tab2(.$polab_rec_cro, useNA_opt = "no")) %>%
#   filter(Code == 0) %>%
#   mutate(dif = 100 - Perc.)






# variance for each person

summ_data_var <- data3 %>%
  filter(w5part == 1) %>%
  select(idno, cntry, var_int5) %>%
  rowwise() %>%
  mutate(var_ess = var(c(psppsgva, actrolga, psppipla, cptppola), na.rm = T),
         var_cro = var(c(w5q38, w5q39, w5q40, w5q41), na.rm = T))


summ_data_var %>%
  group_by(cntry) %>%
  summarise(mean_var_ess = round(mean(var_ess, na.rm = T), 2),
            mean_var_cro = round(mean(var_cro, na.rm = T), 2))



# political aprticipation straightlining ----------------------------------


desc_tab(data3$wrkprty)

summ_datab <- data3 %>%
  filter(w5part == 1) %>%
  select(idno, cntry, var_int6) %>%
  mutate(polpart_prim_ess8 = ifelse(wrkprty == 1, 1, 0) +
           ifelse(wrkorg == 1, 1, 0) +
           ifelse(badge == 1, 1, 0) +
           ifelse(sgnptit == 1, 1, 0) +
           ifelse(pbldmn == 1, 1, 0) +
           ifelse(pstplonl == 1, 1, 0),
         polpart_prim_cro = ifelse(w5q32 == 1, 1, 0) +
           ifelse(w5q33 == 1, 1, 0) +
           ifelse(w5q34 == 1, 1, 0) +
           ifelse(w5q35 == 1, 1, 0) +
           ifelse(w5q36 == 1, 1, 0) +
           ifelse(w5q37 == 1, 1, 0),
         polpart_rec_ess8 = ifelse(wrkprty == 2, 1, 0) +
           ifelse(wrkorg == 2, 1, 0) +
           ifelse(badge == 2, 1, 0) +
           ifelse(sgnptit == 2, 1, 0) +
           ifelse(pbldmn == 2, 1, 0) +
           ifelse(pstplonl == 2, 1, 0),
         polpart_rec_cro = ifelse(w5q32 == 2, 1, 0) +
           ifelse(w5q33 == 2, 1, 0) +
           ifelse(w5q34 == 2, 1, 0) +
           ifelse(w5q35 == 2, 1, 0) +
           ifelse(w5q36 == 2, 1, 0) +
           ifelse(w5q37 == 2, 1, 0))




summ_datab %>%
  group_by(cntry) %>%
  summarise(round(mean(polpart_prim_ess8, na.rm = T), 2))

summ_datab %>%
  group_by(cntry) %>%
  summarise(round(mean(polpart_prim_cro, na.rm = T), 2))



summ_datab %>%
  group_by(cntry) %>%
  summarise(round(mean(polpart_rec_ess8, na.rm = T), 2))

summ_datab %>%
  group_by(cntry) %>%
  summarise(round(mean(polpart_rec_cro, na.rm = T), 2))


# Social trust response style ---------------------------------------------




summ_data2 <- data3 %>%
  filter(w5part == 1) %>%
  select(idno, cntry,
         vars_int2[!str_detect(vars_int2, "w6")]) %>%
  mutate(trust_prim_ess8 = ifelse(ppltrst == 0, 1, 0) +
           ifelse(pplfair == 0, 1, 0) +
           ifelse(pplhlp == 0, 1, 0),
         trust_rec_ess8 = ifelse(ppltrst == 10, 1, 0) +
           ifelse(pplfair == 10, 1, 0) +
           ifelse(pplhlp == 10, 1, 0),
         trust_prim_cro = ifelse(w5q57 == 0, 1, 0) +
           ifelse(w5q58 == 0, 1, 0) +
           ifelse(w5q59 == 0, 1, 0),
         trust_rec_cro = ifelse(w5q57 == 10, 1, 0) +
           ifelse(w5q58 == 10, 1, 0) +
           ifelse(w5q59 == 10, 1, 0)) %>%
  rowwise() %>%
  mutate(var_ess = var(c(ppltrst, pplfair, pplhlp), na.rm = T),
         var_cro = var(c(w5q57, w5q58, w5q59), na.rm = T))



summ_data2 %>%
  group_by(cntry) %>%
  summarise(round(mean(trust_prim_ess8, na.rm = T), 2))

summ_data2 %>%
  group_by(cntry) %>%
  summarise(round(mean(trust_prim_cro, na.rm = T), 2))

summ_data2 %>%
  group_by(cntry) %>%
  summarise(round(mean(trust_rec_ess8, na.rm = T), 2))

summ_data2 %>%
  group_by(cntry) %>%
  summarise(round(mean(trust_rec_cro, na.rm = T), 2))



summ_data3 <- data3 %>%
  filter(w6part == 1) %>%
  select(idno, cntry, ppltrst, pplfair, pplhlp,
         vars_int2[str_detect(vars_int2, "w6")]) %>%
  mutate(trust_prim_ess8 = ifelse(ppltrst == 0, 1, 0) +
           ifelse(pplfair == 0, 1, 0) +
           ifelse(pplhlp == 0, 1, 0),
         trust_rec_ess8 = ifelse(ppltrst == 10, 1, 0) +
           ifelse(pplfair == 10, 1, 0) +
           ifelse(pplhlp == 10, 1, 0),
         trust_prim_cro = ifelse(w6q5 == 0, 1, 0) +
           ifelse(w6q6 == 0, 1, 0) +
           ifelse(w6q7 == 0, 1, 0),
         trust_rec_cro = ifelse(w6q5 == 10, 1, 0) +
           ifelse(w6q6 == 10, 1, 0) +
           ifelse(w6q7 == 10, 1, 0)) %>%
  rowwise() %>%
  mutate(var_ess = var(c(ppltrst, pplfair, pplhlp), na.rm = T),
         var_cro = var(c(w6q5, w6q6, w6q7), na.rm = T))


summ_data3 %>%
  group_by(cntry) %>%
  summarise(round(mean(trust_prim_ess8, na.rm = T), 2))

summ_data3 %>%
  group_by(cntry) %>%
  summarise(round(mean(trust_prim_cro, na.rm = T), 2))

summ_data3 %>%
  group_by(cntry) %>%
  summarise(round(mean(trust_rec_ess8, na.rm = T), 2))

summ_data3 %>%
  group_by(cntry) %>%
  summarise(round(mean(trust_rec_cro, na.rm = T), 2))




desc_tab(summ_data2$trust_prim_ess8)
desc_tab(summ_data2$trust_prim_cro)
desc_tab(summ_data3$trust_prim_cro)

desc_tab(summ_data2$trust_rec_ess8)
desc_tab(summ_data2$trust_rec_cro)
desc_tab(summ_data3$trust_rec_cro)

summ_data2 %>%
group_by(cntry) %>%
  summarise(round(mean(var_ess, na.rm = T), 2),
            round(mean(var_cro, na.rm = T), 2))

summ_data3 %>%
  group_by(cntry) %>%
  summarise(round(mean(var_ess, na.rm = T), 2),
            round(mean(var_cro, na.rm = T), 2))




# Social benefit style ----------------------------------------------------




summ_data4 <- data3 %>%
  filter(w5part == 1) %>%
  select(idno, cntry, vars_int3) %>%
  mutate(ben_prim_ess8 = ifelse(sbstrec == 1, 1, 0) +
           ifelse(sbprvpv == 1, 1, 0) +
           ifelse(sbeqsoc == 1, 1, 0) +
           ifelse(sbbsntx == 1, 1, 0),
         ben_rec_ess8 = ifelse(sbstrec == 5, 1, 0) +
           ifelse(sbprvpv == 5, 1, 0) +
           ifelse(sbeqsoc == 5, 1, 0) +
           ifelse(sbbsntx == 5, 1, 0),
         ben_prim_cro = ifelse(w5q63 == 1, 1, 0) +
           ifelse(w5q64 == 1, 1, 0) +
           ifelse(w5q65 == 1, 1, 0) +
           ifelse(w5q66 == 1, 1, 0),
         ben_rec_cro = ifelse(w5q63 == 5, 1, 0) +
           ifelse(w5q64 == 5, 1, 0) +
           ifelse(w5q65 == 5, 1, 0) +
           ifelse(w5q66 == 5, 1, 0)) %>%
  rowwise() %>%
  mutate(var_ess = var(c(sbstrec, sbprvpv, sbeqsoc, sbbsntx), na.rm = T),
         var_cro = var(c(w5q63, w5q64, w5q65, w5q66), na.rm = T))




summ_data4 %>%
  group_by(cntry) %>%
  summarise(round(mean(ben_prim_ess8, na.rm = T), 2))

summ_data4 %>%
  group_by(cntry) %>%
  summarise(round(mean(ben_prim_cro, na.rm = T), 2))

summ_data4 %>%
  group_by(cntry) %>%
  summarise(round(mean(ben_rec_ess8, na.rm = T), 2))

summ_data4 %>%
  group_by(cntry) %>%
  summarise(round(mean(ben_rec_cro, na.rm = T), 2))


desc_tab(summ_data4$ben_prim_ess8)
desc_tab(summ_data4$ben_prim_cro)


desc_tab(summ_data4$ben_rec_ess8)
desc_tab(summ_data4$ben_rec_cro)


summ_data4 %>%
  group_by(cntry) %>%
  summarise(round(mean(var_ess, na.rm = T), 2),
            round(mean(var_cro, na.rm = T), 2))





# Trust in institution style ----------------------------------------------



summ_data5 <- data3 %>%
  filter(w6part == 1) %>%
  select(idno, cntry, vars_int4) %>%
  mutate(inst_prim_ess8 = ifelse(trstlgl == 0, 1, 0) +
           ifelse(trstplc == 0, 1, 0) +
           ifelse(trstplt == 0, 1, 0) +
           ifelse(trstprt == 0, 1, 0) +
           ifelse(trstep == 0, 1, 0) +
           ifelse(trstun == 0, 1, 0),
         inst_rec_ess8 = ifelse(trstlgl == 10, 1, 0) +
           ifelse(trstplc == 10, 1, 0) +
           ifelse(trstplt == 10, 1, 0) +
           ifelse(trstprt == 10, 1, 0) +
           ifelse(trstep == 10, 1, 0) +
           ifelse(trstun == 10, 1, 0),
         inst_prim_cro = ifelse(w6q9 == 0, 1, 0) +
           ifelse(w6q10 == 0, 1, 0) +
           ifelse(w6q11 == 0, 1, 0) +
           ifelse(w6q12 == 0, 1, 0) +
           ifelse(w6q13 == 0, 1, 0) +
           ifelse(w6q14 == 0, 1, 0),
         inst_rec_cro = ifelse(w6q9 == 10, 1, 0) +
           ifelse(w6q10 == 10, 1, 0) +
           ifelse(w6q11 == 10, 1, 0) +
           ifelse(w6q12 == 10, 1, 0) +
           ifelse(w6q13 == 10, 1, 0) +
           ifelse(w6q14 == 10, 1, 0),
         inst_str_ess = identical(trstlgl, trstplc, trstplt,
                                 trstprt, trstep, trstun),
         inst_str_cro = identical(w6q9, w6q10, w6q11,
                                 w6q12, w6q13, w6q14)) %>%
  rowwise() %>%
  mutate(var_ess = var(c(trstlgl, trstplc, trstplt, trstprt, trstep, trstun), na.rm = T),
         var_cro = var(c(w6q9, w6q10, w6q11, w6q12, w6q13, w6q14), na.rm = T))






summ_data5 %>%
  group_by(cntry) %>%
  summarise(round(mean(inst_prim_ess8, na.rm = T), 2))

summ_data5 %>%
  group_by(cntry) %>%
  summarise(round(mean(inst_prim_cro, na.rm = T), 2))

summ_data5 %>%
  group_by(cntry) %>%
  summarise(round(mean(inst_rec_ess8, na.rm = T), 2))

summ_data5 %>%
  group_by(cntry) %>%
  summarise(round(mean(inst_rec_cro, na.rm = T), 2))






desc_tab(summ_data5$inst_prim_ess8)
desc_tab(summ_data5$inst_prim_cro)


desc_tab(summ_data5$inst_rec_ess8)
desc_tab(summ_data5$inst_rec_cro)


mean(summ_data5$inst_str_ess, na.rm = T)
mean(summ_data5$inst_str_cro, na.rm = T)




summ_data5 %>%
  group_by(cntry) %>%
  summarise(round(mean(var_ess, na.rm = T), 2),
            round(mean(var_cro, na.rm = T), 2))




# Boostrap coefficients ---------------------------------------------------






my_boot <- function(x, times = 1000) {
  # Bootstrap 95% CI
  cis <- quantile(replicate(times,
                            mean(sample(x, replace = TRUE),
                                 na.rm = T)),
                  probs = c(0.025, 0.975))

  # Return results as a data frame
  data.frame(
    mean = round(mean(x, na.rm = T), 2),
    lower.ci = round(cis[1], 2),
    upper.ci = round(cis[2], 2)
  )
}










boot_ess_cro <- function(data) {
  # Bootstraping SE

  data %>%
    ungroup() %>%
    mutate(cntry = as.numeric(as.factor(cntry))) %>%
    group_by(cntry) %>%
    select(cntry, matches("_prim_"), matches("_rec_")) %>%
    do(as.data.frame(apply(., 2, my_boot))) %>%
    melt(id.var = "cntry") %>%
    separate(
      variable,
      sep = "\\.",
      extra = "merge",
      into = c("col", "stat")
    ) %>%
    dcast(cntry + col ~ stat, value.var = "value") %>%
    filter(col != "cntry") %>%
    mutate(
      Survey = case_when(
        str_detect(col, "_cro") ~ "Cronos",
        str_detect(col, "_ess8") ~ "ESS8"
      ),
      Measure = case_when(
        str_detect(col, "_prim") ~ "Primacy",
        str_detect(col, "_rec") ~ "Recency"
      ),
      cntry = factor(cntry, labels = c("EE", "GB", "SI"))
    ) %>%
    select(cntry, Measure, Survey, everything()) %>%
    arrange(cntry, Measure, desc(Survey))
}


boot_ess_cro(summ_data)
boot_ess_cro(summ_datab)
boot_ess_cro(summ_data2)
boot_ess_cro(summ_data3)
boot_ess_cro(summ_data4)
boot_ess_cro(summ_data5)





# version for variance





boot_ess_cro2 <- function(data, match = "var_") {


  data %>%
    ungroup() %>%
    mutate(cntry = as.numeric(as.factor(cntry))) %>%
    group_by(cntry) %>%
    select(cntry, matches(match)) %>%
    do(as.data.frame(apply(., 2, my_boot))) %>%
    melt(id.var = "cntry") %>%
    separate(
      variable,
      sep = "\\.",
      extra = "merge",
      into = c("col", "stat")
    ) %>%
    dcast(cntry + col ~ stat, value.var = "value") %>%
    filter(col != "cntry") %>%
    mutate(
      Survey = case_when(
        str_detect(col, "_cro") ~ "Cronos",
        str_detect(col, "_ess") ~ "ESS8"
      ),
        cntry = factor(cntry, labels = c("EE", "GB", "SI"))
    ) %>%
    select(cntry, Survey, everything()) %>%
    arrange(cntry, desc(Survey))
}


boot_ess_cro2(miss_data, match = "miss_")


boot_ess_cro2(summ_data_var)
boot_ess_cro2(summ_data2)
boot_ess_cro2(summ_data3)
boot_ess_cro2(summ_data4)
boot_ess_cro2(summ_data5)




# Equivalence -------------------------------------------------------------


data4 <- data3 %>%
  select(idno, cntry, w6part, w5part, vars_int, w0q40) %>%
  mutate(cntry = as.numeric(as.factor(cntry)),
         comp = case_when(w0q40 < 4 ~ 0,
                          w0q40 > 3 ~ 1)) %>%
  select(-w0q40) %>%
  mutate_all(funs(as.numeric))


table(data3$cntry, data4$cntry)


# export to Mplus

prepareMplusData(data4, "./Mplus/data4.dat")



# import Mplus

mplus_models <- list.files("./Mplus/",
                           pattern = ".out",
                           full.names = T)


read_fit <- function(model) {
  indices <- c("Filename", "Observations", "ChiSqM_Value", "ChiSqM_DF",
               "CFI", "RMSEA_Estimate")

  readModels(model)$summaries[indices]

}

# not avilable for categorical "LL", "BIC", "AIC"

fit_table <- map(mplus_models, read_fit) %>%
  reduce(rbind)


fit_table <- fit_table %>%
  tbl_df() %>%
  mutate(Country = case_when(str_detect(Filename, "ee") ~ "Estonia",
                             str_detect(Filename, "gb") ~ "UK",
                             str_detect(Filename, "si") ~ "Slovenia"),
         Model = case_when(str_detect(Filename, "config") ~ "Configural",
                             str_detect(Filename, "metric") ~ "Metric",
                             str_detect(Filename, "scalar") ~ "Scalar")) %>%
  filter(!str_detect(Filename, "[0-9]"))


write.csv(fit_table, file = "./output/fit_table_equivalence.csv")




# regression explaining data quality differences --------------------------


# select main variables

clean_data1 <- data3 %>%
  select(idno, cntry, agea, eduyrs, w0q44,
         gndr, w0q40, w0q41, w0q43, tablet,
         w5main_device, w6main_device)


# clean ind vars


# bring data quality

miss_data


summ_data_varb <- summ_data_var %>%
  rename(polab_var_ess = var_ess,
         polab_var_cro = var_cro)

summ_data2b <- summ_data2 %>%
  rename_all(funs(str_replace(., "trust_", "trust5_"))) %>%
  rename(trust5_var_ess = var_ess,
         trust5_var_cro = var_cro)

summ_data3b <- summ_data3 %>%
  rename_all(funs(str_replace(., "trust_", "trust6_"))) %>%
  rename(trust6_var_ess = var_ess,
         trust6_var_cro = var_cro)


summ_data4b <- summ_data4 %>%
  rename(ben_var_ess = var_ess,
         ben_var_cro = var_cro)

summ_data5b <- summ_data5 %>%
  rename(inst_var_ess = var_ess,
         inst_var_cro = var_cro)


summ_list <- list(summ_data, summ_datab, summ_data_varb,
                  summ_data2b, summ_data3b,
                  summ_data4b, summ_data5b)


qual_data <- map(summ_list, function(x) {
  x %>%
    select(idno, cntry, matches("_prim_"),
           matches("_rec_"), matches("var_"))
}) %>%
  reduce(full_join, by = c("idno", "cntry"))


clean_data2 <- full_join(clean_data1, miss_data) %>%
  full_join(qual_data)

# make dependent variable

clean_data3 <- clean_data2 %>%
  mutate(diff_miss5 = miss_cro5 - miss_ess5,
         diff_miss6 = miss_cro6 - miss_ess6,

         diff_var_polab = polab_var_cro - polab_var_ess,
         diff_var_trust5 = trust5_var_cro - trust5_var_ess,
         diff_var_trust6 = trust6_var_cro - trust6_var_ess,
         diff_var_ben = ben_var_cro - ben_var_ess,
         diff_var_inst = inst_var_cro - inst_var_ess,

         diff_prim_polab = polab_prim_cro - polab_prim_ess8,
         diff_prim_polpart = polpart_prim_cro - polpart_prim_ess8,
         diff_prim_trust5 = trust5_prim_cro - trust5_prim_ess8,
         diff_prim_trust6 = trust6_prim_cro - trust6_prim_ess8,
         diff_prim_ben = ben_prim_cro - ben_prim_ess8,
         diff_prim_inst = inst_prim_cro - inst_prim_ess8,

         diff_rec_polab = polab_rec_cro - polab_rec_ess8,
         diff_rec_trust5 = trust5_rec_cro - trust5_rec_ess8,
         diff_rec_trust6 = trust6_rec_cro - trust6_rec_ess8,
         diff_rec_ben = ben_rec_cro - ben_rec_ess8,
         diff_rec_inst = inst_rec_cro - inst_rec_ess8)


map(select(clean_data3, matches("diff_")), function(x) qplot(x))


# clean independent vars

clean_data4 <- clean_data3 %>%
  mutate(und_diff = ifelse(w0q44 > 1, 1, 0),
         int_comf = w0q41,
         nr_surv = ifelse(w0q43 > 20, 20, w0q43),
         gndr = as.factor(gndr),
         w5main_device = as.factor(w5main_device),
         w6main_device = as.factor(w6main_device),
         cntry = as.factor(cntry))


desc_tab(clean_data4$int_comf)

ind_var <- c("gndr", "agea", "eduyrs", "cntry", "und_diff",
             "int_comf", "nr_surv", "tablet")

dev <- c("w5main_device", "w6main_device")


dep <- str_subset(names(clean_data4), "diff_")

dev_index <- dev[str_detect(dep, "6") + 1]


# Regression models -------------------------------------------------------

i <- 1
reg_res <- map(dep, function(x) {

  out <- lm(data = clean_data4,
     str_c(x, " ~ ",
           str_c(ind_var, collapse = " + "),
           " + ", dev_index[i]))

  i <<- i + 1

  out

})


sig_res <- map(reg_res, tidy) %>%
  reduce(rbind) %>%
  mutate(dep = rep(dep, each = 12)) %>%
  filter(p.value < 0.05)

write.csv(sig_res, file = "./output/sig_res.csv")


model_fit_reg <- map(reg_res, glance) %>%
  reduce(rbind) %>%
  mutate(dep = dep)
write.csv(model_fit_reg, file = "./output/model_fit_reg.csv")

desc_tab(clean_data3$gndr)

