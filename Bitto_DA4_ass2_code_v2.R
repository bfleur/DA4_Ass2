# Virag Bitto (ID: 1903164)
# DA4 Assignment 2
# ------------------------------------------------------------------------------------------------------
# setup
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
#install.packages("modelsummary")
library(modelsummary)
#install.packages("haven")
library(haven)
library(stargazer)
library(car)
#install.packages("huxtable")
library(huxtable)
#install.packages("estimatr")
library(estimatr)
#install.packages("lmtest")
library(lmtest)
library(modelsummary)
#install.packages("fixest")
library(fixest)
#install.packages("urca")
library(urca)
#install.packages("tidyr")
library(tidyr)

# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")
getwd()
setwd("/Users/Virág/Documents/CEU/2nd year/2nd trimester/Data4/Assignment2")


# set data dir, data used
#source("set-data-directory.R")             # data_dir must be first defined 
# alternative: give full path here, 
#            example data_dir="C:/Users/bekes.gabor/Dropbox (MTA KRTK)/bekes_kezdi_textbook/da_data_repo"

data_dir= "/Users/Virág/Documents/CEU/2nd year/2nd trimester/Data4/Assignment2/WB_data/"

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

use_case_dir <- "DA4_Ass2/"
data_in <- data_dir
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-------------------------------------------------------
# Import data

data_raw <- read.csv(paste(data_in, "WB_DA4_data_raw.csv", sep = ""))

summarize(data_raw)

#2. Cleaning: nas, dividing by populations where necessary, check for extreme values
# check data format, transform if necessary, delete rows which dont denote countries
# transform stuff into logs, calculate first differences

sum(duplicated(data_raw))
# there seems to be 2 duplicate elements
data_raw <- data_raw %>% distinct()


# converting from long to tidy format
#reshape(data_raw, idvar = "Country.Code", timevar = "numbers", direction = "wide")

colnames(data_raw)


c_names = c("country", "country_code", "series", "series_code", "1992", "1993", "1994", "1995",
            "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007",
            "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

i <- 1
while (i < 32) {
  names(data_raw)[i] <- c_names[i]
  
  i = i+1
}

colnames(data_raw)

# first we want to convert the data into long format
#data_raw_wide <- reshape(data_raw, idvar = "country", timevar = "series", direction = "wide")

year_columns = c('1992', '1993', '1994', '1995',
                 '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007',
                 '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')

data_raw_pivot <- data_raw %>% 
  pivot_longer(year_columns, names_to = "year", values_to = "series_p")

#data_raw_tidy <- reshape(data_raw_pivot, idvar = ("country", "year"), timevar = "series", direction = "wide")

#install.packages("dplyr")
library(dplyr)

#install.packages("broom")
library(broom)

#install.packages("broom.mixed")
library(broom.mixed)

#data_raw_tidy <- data_raw_pivot %>%
#  group_by(country) %>%
#  mutate(row = row_number()) %>%
#  tidyr::pivot_wider(names_from = series, values_from = series_p) %>%
#  select(-row)


#data_raw_tidy <- data_raw_pivot %>%
#  pivot_wider(., names_from = series, values_from = series_p)

#then we make it wide

data_raw_pivot <- data_raw_pivot %>%
  group_by(country) %>%
  mutate(row = row_number()) %>%
  #tidyr::pivot_wider(names_from = series, values_from = series_p) %>%
  select(-row)

#data_raw_tidy <- spread(data_raw_pivot, series, series_p)

library(reshape2)

data_raw_tidy2 <- dcast(data_raw_pivot, country + year ~ series, value.var="series_p")
write.csv(data_raw_tidy2,paste(data_dir, 'WB_wideform.csv', sep = "/"), row.names = FALSE)

# check for missing values
is.na(data_raw_tidy2)
apply(is.na(data_raw_tidy2), 2, which)

cn_tidy = colnames(data_raw_tidy2)
cn_tidy

length(colnames(data_raw_tidy2))
sum(is.na(cn_tidy[1]))
cn_tidy[1]
print(na)

i<- 1
while (i<10) {
  na <- sum(is.na(cn_tidy[i]))
  print(cn_tidy[i])
  print(na)

  i= i+1
}

# drop obs which cant't be used due to nas
# subset(dataframe, A==B & E!=0)
data_raw_tidy2 <- subset(data_raw_tidy2, country != "")



# drop column with name "Var.3"
data_raw_tidy2 <- subset(data_raw_tidy2, select = -c(Var.3))

# give proper names again
c_names2 = c("country", "year", "electr", "co2_tot", "ff_c", "GDP_tot", "pop", 
             "fuel_pr")
length(c_names2)

i <- 1
while (i < 9) {
  names(data_raw_tidy2)[i] <- c_names2[i]
  
  i = i+1
}

colnames(data_raw_tidy2)


# create "per capita" variables
data_raw_tidy2$co2_em_pc <- 

# 3. Visualization, checking the data, extreme values?

# 4. cross-section OLS 1999/2000

# 5. cross-section OLS year of choice

# 6. first diff, time trend, no lags

# 7. first diff, time trend, 2-y lags

# 8. first diff, time trend, 6-y lags

# 9. Fixed effects with time+country FEs

# 10. long diff model

####################################################################################
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
install.packages("modelsummary")
library(modelsummary)
install.packages("haven")
library(haven)
library(stargazer)
library(car)
install.packages("huxtable")
library(huxtable)
install.packages("estimatr")
library(estimatr)
install.packages("lmtest")
library(lmtest)
library(modelsummary)
install.packages("fixest")
library(fixest)
install.packages("urca")
library(urca)

# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")
getwd()
setwd("/Users/Virág/Documents/CEU/2nd year/2nd trimester/Data4/da_case_studies")


# set data dir, data used
source("set-data-directory.R")             # data_dir must be first defined 
# alternative: give full path here, 
#            example data_dir="C:/Users/bekes.gabor/Dropbox (MTA KRTK)/bekes_kezdi_textbook/da_data_repo"

data_dir= "/Users/Virág/Documents/CEU/2nd year/2nd trimester/Data4/da_data_repo"

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

use_case_dir <- "ch23-immunization-life/"
data_in <- paste(data_dir,"worldbank-immunization","clean/", sep = "/")
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-------------------------------------------------------
# Import data

data <- read_dta(paste(data_in, "worldbank-immunization-continents.dta", sep = ""))

# **************************************************
# * info graph on measles vaccination continent aggregates

p1 <- ggplot(data, aes(x = year, y = imm6)) +
  geom_line(color = color[1], size = 0.7) +
  geom_line(aes(x = year, y = imm7), color = color[2], size = 0.7) +
  geom_text(data = data[12,], aes(label = "South Asia"), hjust = 1.2, vjust = 1, size=2) +
  geom_text(data = data[16,], aes(y = imm7, label = "Sub-Saharan Africa"), hjust = 0.4, vjust = 1.5, size=2) +
  labs(y = "Immunization rate (percent)", x="Date (year)") + 
  scale_y_continuous(expand=c(0,0), breaks = seq(50, 100, by = 10), limits = c(50, 100)) +
  scale_x_continuous(expand=c(0,0), breaks = seq(1998, 2018, by = 5), limits = c(1998, 2018)) +
  theme_bg()

for (i in seq(1,5)) {
  p1 <- p1 + geom_line(aes_string(x = "year", y = paste0("imm",i)), color = "grey", size=0.5)
}
p1
save_fig("ch23-figure-2a-tsimmun", output, size = "small")

p2 <- ggplot(data, aes(x = year, y = surv6)) +
  geom_line(color = color[1], size = 0.7) +
  geom_line(aes(x = year, y = surv7), color = color[2], size = 0.7) +
  geom_text(data = data[11,], aes(label = "South Asia"), hjust = 0, vjust = 1.5, size=2) +
  geom_text(data = data[15,], aes(y = surv7, label = "Sub-Saharan Africa"), hjust = 0.2, vjust = 1.5, size=2) +
  labs(y = "Child survival rate (percent)", x="Date (year)") + 
  scale_y_continuous(expand=c(0,0), breaks = seq(80, 100, by = 5), limits = c(80, 100)) +
  scale_x_continuous(expand=c(0,0), breaks = seq(1998, 2018, by = 5), limits = c(1998, 2018)) +
  theme_bg()
for (i in seq(1,5)) {
  p2 <- p2 + geom_line(aes_string(x = "year", y = paste0("surv",i)), color = "grey", size=0.5)
}
p2
save_fig("ch23-figure-2b-tssurvival", output, size = "small")

# **************************************************
# * regressions on countries

data_panel <- read_dta(paste(data_in, "worldbank-immunization-panel.dta", sep = "/"))

data_panel <- data_panel %>%
  filter(!(is.na(imm) | is.na(gdppc))) %>%
  mutate(c = factor(c)) %>%
  group_by(c) %>%
  mutate(balanced = min(year) == 1998 & max(year) == 2017 & length(unique(year)) == 20) %>%
  ungroup() 

data_balanced <- data_panel %>%
  filter(balanced == TRUE)

data_balanced <- data_balanced %>%
  arrange(c, year) %>%
  group_by(c) %>%
  mutate(
    lnpop=log(pop),
    d_surv = surv- lag(surv),
    d_imm = imm - lag(imm),
    d2_imm = d_imm - lag(d_imm), 
    d_lngdppc= lngdppc- lag(lngdppc),
    d_lnpop = lnpop - lag(lnpop),
    avgpop = mean(pop), #for weights in xtreg fe
    year = factor(year)
  ) %>%
  ungroup()



# *****************************************************
# * FE REGRESSSIONS

fe_lm <- lm_robust(surv ~ imm + year,
                   data = data_balanced, 
                   weights = avgpop, 
                   se_type = "stata", 
                   fixed_effect =  ~ c ,
                   clusters = c)

fe_lm2 <- lm_robust(surv ~ imm + lngdppc + lnpop + year,
                    data = data_balanced, 
                    weights = avgpop, 
                    se_type = "stata", 
                    fixed_effect =  ~ c ,
                    clusters = c)


# ch23-table-2-immun-fe
huxreg(fe_lm, fe_lm2, 
       statistics = c(N = "nobs", R2 = "r.squared"), 
       coefs = c("Immunization rate"= "imm", "ln GDP per capita"= "lngdppc","ln population"= "lnpop"))



# *************************
# ** CLUSTER SE VS BIASED SE 

fe_lm3 <- lm_robust(surv ~ imm + lngdppc + lnpop + year,
                    data = data_balanced, 
                    weights = avgpop, 
                    se_type = "stata", 
                    fixed_effect =  ~ c )

# ch23-table-3-immun-fese
huxreg(list("Clustered SE" = fe_lm2, "Simple SE" = fe_lm3), 
       statistics = c(N = "nobs", R2 = "r.squared"), 
       coefs = c("Immunization rate"= "imm", "ln GDP per capita"= "lngdppc","ln population"= "lnpop"))

# *************************
# * FD REGRESSIONS 

# * basic FD 
fd_lm <- lm_robust(d_surv ~ d_imm,
                   data = data_balanced, 
                   weights = pop,
                   se_type = "stata", 
                   clusters = c)

# * FD, 5 lags
lags_helper <- paste(paste0("lag(d_imm,", c(0:5), ")"), collapse = " + ")
fd_lm_5_formula <- as.formula(paste0("d_surv ~ ", lags_helper))

fd_lm_5 <- lm_robust(fd_lm_5_formula,
                     data = data_balanced, 
                     weights = pop,
                     se_type = "stata", 
                     clusters = c
)


# * FD, 5 lags, cumul
lags_helper <- paste(paste0("lag(d2_imm,", c(0:4), ")"), collapse = " + ")
fd_lm_5_cumul_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", lags_helper))
fd_lm_5_cumul <- lm_robust(fd_lm_5_cumul_formula,
                           data = data_balanced, 
                           weights = pop,
                           se_type = "stata", 
                           clusters = c
)

# * FD, 5 lags, cumul, lead
lags_helper <- paste(paste0("lag(d2_imm,", c(0:4), ")"), collapse = " + ")
lead_helper <- paste(paste0("lead(d_imm,", c(1:3), ")"), collapse = " + ")

fd_lm_5_cumul_lead_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", lags_helper, " + ", lead_helper))
fd_lm_5_cumul_lead <- lm_robust(fd_lm_5_cumul_lead_formula,
                                data = data_balanced, 
                                weights = pop,
                                se_type = "stata", 
                                clusters = c
)


# h23-table-4-immun-fd1
# 1st, 2nd column
huxreg(fd_lm, fd_lm_5,
       coefs = c("d_imm"="lag(d_imm, 0)", 
                 "d_imm"="d_imm", 
                 "d_imm lag1"="lag(d_imm, 1)",
                 "d_imm lag2"="lag(d_imm, 2)",
                 "d_imm lag3"="lag(d_imm, 3)",
                 "d_imm lag4"="lag(d_imm, 4)",
                 "d_imm lag5"="lag(d_imm, 5)",
                 "Constant"= "(Intercept)" 
       ),
       statistics = c(N = "nobs", R2 = "r.squared")
)

# thrid, fourth column
huxreg("(3)"=fd_lm_5_cumul, "(4)"=fd_lm_5_cumul_lead,
       coefs = c("d_imm cumulative"="lag(d_imm, 5)",
                 "d_imm lead 1"="lead(d_imm, 1)",
                 "d_imm lead 2"="lead(d_imm, 2)",
                 "d_imm lead 3"="lead(d_imm, 3)",
                 "Constant"= "(Intercept)" ),
       statistics = c(N = "nobs", R2 = "r.squared")
)



# *************************
# * AGGREG TREND, CONFOUNDERS, CTRY TRENDS
# * FD, 5 lags, cumul, aggreg trend

lags_helper <- paste(paste0("lag(d2_imm,", c(0:4), ")"), collapse = " + ")
fd_lm_5_cumul_trend_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", lags_helper, "+ year"))
fd_lm_5_cumul_trend <- lm_robust(fd_lm_5_cumul_trend_formula,
                                 data = data_balanced, 
                                 weights = pop,
                                 se_type = "stata", 
                                 clusters = c
) 

# * FD, 5 lags, cumul, aggreg trend, confounders 
lags_helper <- paste(paste0("lag(d2_imm, ", c(0:4), ")"), collapse = " + ")
lags_helper2 <- paste(paste0("lag(d_lngdppc, ", c(0:5), ")"), collapse = " + ")
lags_helper3 <- paste(paste0("lag(d_lnpop, ", c(0:5), ")"), collapse = " + ")

fd_lm_5_cumul_trend_c_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", 
                                                   lags_helper, "+",
                                                   lags_helper2, "+",
                                                   lags_helper3, "+",
                                                   "+ year"))
fd_lm_5_cumul_trend_c <- lm_robust(fd_lm_5_cumul_trend_c_formula,
                                   data = data_balanced, 
                                   weights = pop,
                                   se_type = "stata", 
                                   clusters = c
) 

# * check: cumulative coeffs on the confounders
linearHypothesis(fd_lm_5_cumul_trend_c, paste0(lags_helper2," =0"))
linearHypothesis(fd_lm_5_cumul_trend_c, paste0(lags_helper3," =0"))

# * check: it's not the number of obsrevations
data_balanced_filtered <- data_balanced %>%
  filter(!is.na(d_lngdppc))
fd_lm_5_cumul_trend2 <- lm_robust(formula = fd_lm_5_cumul_trend_formula,
                                  data = data_balanced_filtered, 
                                  weights = pop,
                                  se_type = "stata", 
                                  clusters = c
)

# * FD, 5 lags, cumul, aggreg trend, cofounders, country linear trend
fd_lm_5_cumul_trend_c_country_formula <- as.formula(paste0("d_surv ~ lag(d_imm, 5) + ", 
                                                           lags_helper, "+",
                                                           lags_helper2, "+",
                                                           lags_helper3, "+",
                                                           "+ year + c"))

fd_lm_5_cumul_trend_c_country <- lm_robust(fd_lm_5_cumul_trend_c_country_formula,
                                           data = data_balanced, 
                                           weights = pop,
                                           se_type = "stata", 
                                           clusters = c
) 

# ch23-table-5-immun-fd2
# Just keeping cumul, but if delete last row, you can see details
tab5<-huxreg(fd_lm_5_cumul_trend, fd_lm_5_cumul_trend_c, fd_lm_5_cumul_trend_c_country,
             statistics = c(N = "nobs", R2 = "r.squared"), 
             #omit_coefs = c(paste("year", levels(data_balanced$year), sep= ""), paste("c", levels(data_balanced$c), sep= "")),
             coefs = c("d_imm cumulative" = "lag(d_imm, 5)")
)

# produce table 5
tab5 %>%
  insert_row(c("Year dummies", "Yes", "Yes", "Yes"), after = 3) %>%
  insert_row(c("Confounder variables", "No", "Yes", "Yes"), after = 4) %>%
  insert_row(c("Country-specific trends", "No", "No", "Yes"), after = 5)

