setwd("C:/Users/rusla/OneDrive/GSE_520/RD_Project/114827-V1/Data_20110196")
library(haven)
library(dplyr)
library(AER)
library(clubSandwich)

births <- read_dta(file = "data_births_20110196.dta")
abortions <- read_dta(file = "data_abortions_20110196.dta")

births <- births %>%
  mutate(m = case_when(year == 2010 ~ mesp + 29,
                       year == 2009 ~ mesp + 17,
                       year == 2008 ~ mesp + 5,
                       year == 2007 ~ mesp - 7,
                       year == 2006 ~ mesp - 19,
                       year == 2005 ~ mesp - 31,
                       year == 2004 ~ mesp - 43,
                       year == 2003 ~ mesp - 55,
                       year == 2002 ~ mesp - 67,
                       year == 2001 ~ mesp - 79,
                       year == 2000 ~ mesp - 91
                       ),
         mc1 = m - 9,
         mc2 = case_when(((semanas > 0 & semanas < 38) | prem == 2) ~ m - 8,
                         TRUE ~ m - 9),
         mc3 = case_when(((semanas != 0 & semanas < 39) | prem == 2) ~ m - 8,  
                          (semanas > 43 & !is.na(semanas)) ~ m - 10,
                          TRUE ~ m - 9))

n <- births %>% group_by(mc3) %>% tally() %>% rename(mc = "mc3") %>%
  mutate(month = case_when(mc %in% c(-30,-18,-6,6,18,30,-42,-54,-66,-87,-99) ~ 1,
                           mc %in% c(-29,-17,-5,7,19,31,-41,-53,-65,-86,-98) ~ 2,
                           mc %in% c(-28,-16,-4,8,20,32,-40,-52,-64,-85,-97) ~ 3,
                           mc %in% c(-27,-15,-3,9,21,-39,-51,-63,-75,-84,-96) ~ 4,
                           mc %in% c(-26,-14,-2,10,22,-38,-50,-62,-74,-83,-95) ~ 5,
                           mc %in% c(-25,-13,-1,11,23,-37,-49,-61,-73,-82,-94) ~ 6,
                           mc %in% c(-24,-12,0,12,24,-36,-48,-60,-72,-81,-93) ~ 7,
                           mc %in% c(-23,-11,1,13,25,-35,-47,-59,-71,-80,-92) ~ 8,
                           mc %in% c(-22,-10,2,14,26,-34,-46,-58,-70,-79,-91) ~ 9,
                           mc %in% c(-21,-9,3,15,27,-33,-45,-57,-69,-78,-90) ~ 10,
                           mc %in% c(-20,-8,4,16,28,-32,-44,-56,-68,-77,-89) ~ 11,
                           mc %in% c(-19,-7,5,17,29,-31,-43,-55,-67,-76,-88) ~ 12),
         july = case_when(month == 7 ~ n),
         days = case_when(month == 2 ~ 28,
                          mc == 7 ~ 29,
                          month %in% c(4,6,9,11) ~ 30,
                          TRUE ~ 31),
         post = case_when(mc >= 0 ~ 1,
                          TRUE ~ 0),
         mc2 = mc*mc,
         mc3 = mc*mc*mc,
         ln = log(n))

first_subset <- n %>% filter(mc > -91 & mc < 30)
mod1 = lm(ln ~ mc + mc2 + mc3 + post + days, data = first_subset)
round(coeftest(mod1, vcov = vcovHC(mod1, "HC0")),5)

second_subset = n %>% filter(mc > -31 & mc < 30)
mod2 = lm(ln ~ mc + mc2 + mc3 + post + days, data = second_subset)
round(coeftest(mod2, vcov = vcovHC(mod2, "HC0")),5)

third_subset = n %>% filter(mc > -13 & mc < 12)
mod3 = lm(ln ~ mc + mc2 + mc3 + post + days, data = third_subset)
round(coeftest(mod3, vcov = vcovHC(mod3, "HC0")),5)

fourth_subset = n %>% filter(mc > -10 & mc < 9)
mod4 = lm(ln ~ mc + mc2 + mc3 + post + days, data = fourth_subset)
round(coeftest(mod4, vcov = vcovHC(mod4, "HC0")),5)

fifth_subset = n %>% filter(mc > -4 & mc < 3)
mod5 = lm(ln ~ mc + mc2 + mc3 + post + days, data = fifth_subset)
round(coeftest(mod5, vcov = vcovHC(mod5, "HC0")),5)
################################################################################
vars <- c("n_ive_and","n_ive_val","n_ive_rioja","n_ive_cat","n_ive_can",
          "n_ive_mad","n_ive_gal","n_ive_bal","n_ive_pv","n_ive_castlm",
          "n_ive_ast","n_ive_arag")
abortions <- abortions %>%
  mutate(n_tot = rowSums(abortions[, vars], na.rm = T),
         m = seq(1:nrow(abortions)), 
         m = m - 103,
         days = case_when(month == 2 ~ 28,
                         (month == 2 & year %in% c(2000,2004,2008)) ~ 29,
                          month %in% c(4,6,9,11) ~ 30,
                          TRUE ~ 31),
         log_ive = log(n_tot),
         m2 = m * m,
         m3 = m * m * m,
         post = case_when(m >= 0 ~ 1,
                          TRUE ~ 0)) %>%
  filter(m <= 29 & m >= -90)

options(scipen = 999)
mod1 = lm(log_ive ~ post + m + m2 + m3 + days, data = abortions)
coeftest(mod1, vcov. = vcovHC(mod1, type = "HC0"))

second_set = abortions %>% filter(m > -31)
mod2 = lm(log_ive ~ post + m + m2 + days, data = second_set)
coeftest(mod2, vcov. = vcovHC(mod2, type = "HC0"))

third_set = abortions %>% filter(m > -13 & m < 12)
mod3 = lm(log_ive ~ post + m + m2 + days, data = third_set)
coeftest(mod3, vcov. = vcovHC(mod3, type = "HC0"))

fourth_set = abortions %>% filter(m > -10 & m < 9)
mod4 = lm(log_ive ~ post + m + days, data = fourth_set)
coeftest(mod4, vcov. = vcovHC(mod4, type = "HC0"))

fifth_set = abortions %>% filter(m > -4 & m < 3)
mod5 = lm(log_ive ~ post + days, data = fifth_set)
coeftest(mod5, vcov. = vcovHC(mod5, type = "HC0"))

save(n, file = 'births.Rdata')
save(abortions, file = 'abortions.Rdata')

#load('births.Rdata')
#load('abortions.Rdata')
