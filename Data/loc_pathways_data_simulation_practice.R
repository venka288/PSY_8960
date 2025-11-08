#######################################
#### Locus of Control Pathways Project
#### Data Simulation Practice
#### Varsha Venkatesh
#### 2025-11-06
#######################################

# workspace setup
library(groundhog)
groundhog.library(dplyr, "2025-11-04")
groundhog.library(faux, "2025-11-04")
groundhog.library(summarytools, "2025-11-04")
groundhog.library(missMethods, "2025-11-04")

sessionInfo()
# R version 4.5.1
# dplyr_1.1.4
# faux_1.2.3
# summarytools_1.1.4
# missMethods_0.4.0

# setting seed
set.seed(1978)

#### using rnorm()

# raw simulation
rnorm(n = 10, mean = 3.5, sd = 0.75)

# save as dataframe
outcome <- rnorm(n = 100000, mean = 3.5, sd = 0.75)
dat_rnorm <- data.frame(outcome)

mean(dat_rnorm$outcome)
sd(dat_rnorm$outcome)

# rnorm(), setting up a function
data_generate <- function(n, m, sd) {
  set.seed(1978)
  g <- rnorm(n = n, mean = m, sd = sd)
  data <- data.frame(
    outcome = g
  )
  return(data)
}

dat_rnorm_func <- data_generate(n = 10, m = 3.5, sd = 0.75)
dat_rnorm_func
mean(dat_rnorm_func$outcome)
sd(dat_rnorm_func$outcome)

dat_rnorm_func_again <- data_generate(n = 10000, m = 4.2, sd = 0.2)
dat_rnorm_func_again
mean(dat_rnorm_func_again$outcome)
sd(dat_rnorm_func_again$outcome)

# rnorm() with two groups
data_generate <- function(n1, n2, m1, m2, sd1, sd2) {
  set.seed(1978)
  g1 <- rnorm(n = n1, mean = m1, sd = sd1)
  g2 <- rnorm(n = n2, mean = m2, sd = sd2)
  data <- data.frame(
    group = rep(c("1", "2"), times = c(n1, n2)),
    outcome = c(g1, g2)
  )
  return(data)
}

dat_rnorm_func_two <- data_generate(n1 = 500, n2 = 50,
                                    m1 = 0.39, m2 = -0.42,
                                    sd1 = 0.93, sd2 = 0.80)

# checking data
head(dat_rnorm_func_two)

dat_rnorm_func_two %>% 
  group_by(group) %>% 
  summarize(mean = mean(outcome, na.rm = TRUE), sd = sd(outcome, na.rm = TRUE))

#### using rnorm_multi() from faux package

# set parameters
n <- 100
m1 <- 0.39
sd1 <- 0.93
m2 <- -0.42
sd2 <- 0.80

# create data file
dat_faux <- rnorm_multi(n = n,
                        mu = c(m1, m2),
                        sd = c(sd1, sd2),
                        varnames = c("g1", "g2")
                        )

# checking data
head(dat_faux)
mean(dat_faux$g1)
mean(dat_faux$g2)
sd(dat_faux$g1)
sd(dat_faux$g2)

### using sample() to simulate data

# get info on data from dictionary
dict <- read.csv("Data/loc_pathways_dictionary_2025-10-30.csv")
dict

# create empty data frame and populate with id variable based on N (N = 96)
dat_sim <- data.frame(participant_id = c(1:96))
head(dat_sim)

### simulate individual items based on properties of interest

# wave 1 loc
dat_sim$w1_loc_1_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w1_loc_1_sim)

dat_sim$w1_loc_2_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w1_loc_2_sim)

dat_sim$w1_loc_3_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w1_loc_3_sim)

dat_sim$w1_loc_4_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w1_loc_4_sim)

dat_sim$w1_loc_5_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w1_loc_5_sim)

# wave 2 loc
dat_sim$w2_loc_1_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w2_loc_1_sim)

dat_sim$w2_loc_2_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w2_loc_2_sim)

dat_sim$w2_loc_3_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w2_loc_3_sim)

dat_sim$w2_loc_4_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w2_loc_4_sim)

dat_sim$w2_loc_5_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w2_loc_5_sim)

# wave 3 loc
dat_sim$w3_loc_1_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w3_loc_1_sim)

dat_sim$w3_loc_2_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w3_loc_2_sim)

dat_sim$w3_loc_3_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w3_loc_3_sim)

dat_sim$w3_loc_4_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w3_loc_4_sim)

dat_sim$w3_loc_5_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w3_loc_5_sim)

# wave 4 loc
dat_sim$w4_loc_1_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w4_loc_1_sim)

dat_sim$w4_loc_2_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w4_loc_2_sim)

dat_sim$w4_loc_3_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w4_loc_3_sim)

dat_sim$w4_loc_4_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w4_loc_4_sim)

dat_sim$w4_loc_5_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$w4_loc_5_sim)

# pathway
dat_sim$pathway_sim <- sample(1:5, size = 96, replace = TRUE, prob = c(.20, .20, .20, .20, .20))
table(dat_sim$pathway_sim)

# recode pathway
dat_sim$pathway_sim <- as.factor(dat_sim$pathway_sim)

dat_sim$pathway_sim <-  recode(dat_sim$pathway_sim,
                               "1" = "Uncertain",
                               "2" = "Discovery",
                               "3" = "Redirect",
                               "4" = "Solidification",
                               "5" = "Certain")

# age
dat_sim$age_sim <- sample(18:22, size = 96, replace = TRUE, prob = c(.50, .30, .10, .05, .05))
table(dat_sim$age)

# gender
dat_sim$gender_sim <- sample(1:3, size = 96, replace = TRUE, prob = c(.45, .50, .05))
table(dat_sim$gender)

# recode gender
dat_sim$gender_sim <- as.factor(dat_sim$gender_sim)

dat_sim$gender_sim <-  recode(dat_sim$gender_sim,
                              "1" = "Male",
                              "2" = "Female",
                              "3" = "Nonbinary")

# race
dat_sim$race_sim <- sample(1:7, size = 96, replace = TRUE, prob = c(.65, .12, .08, .07, .04, .02, .02))

# recode race
dat_sim$race_sim <-  recode(dat_sim$race_sim,
                            "1" = "White",
                            "2" = "Asian/Pacific Islander",
                            "3" = "Black/African American",
                            "4" = "Hispanic/Latine",
                            "5" = "Multiracial",
                            "6" = "Native American",
                            "7" = "Other"
                            )
table(dat_sim$race_sim)

# transfer
dat_sim$transfer_sim <- sample(0:1, size = 96, replace = TRUE, prob = c(.70, .30))
table(dat_sim$transfer_sim)

# international
dat_sim$international_sim <- sample(0:1, size = 96, replace = TRUE, prob = c(.94, .06))
table(dat_sim$international_sim)

# us born
dat_sim$us_born_sim <- sample(0:1, size = 96, replace = TRUE, prob = c(.05, .95))
table(dat_sim$us_born_sim)

# parent ed 1
dat_sim$parent_ed_1_sim <- sample(1:9, size = 96, replace = TRUE, prob = c(.01, .01, .01, .06, .29, .24, .25, .10, .03))
table(dat_sim$parent_ed_1_sim)

# recode parent ed 1
dat_sim$parent_ed_1_sim <- recode(dat_sim$parent_ed_1_sim,
                                  "1" = "No school",
                                  "2" = "Elementary school",
                                  "3" = "Middle school/Junior high",
                                  "4" = "Some high school",
                                  "5" = "High school diploma",
                                  "6" = "Associates degree or equivalent (2 year degree)",
                                  "7" = "Bachelor's degree (4 year degree)",
                                  "8" = "Master's degree",
                                  "9" = "Doctorate or professional degree")

# parent ed 2
dat_sim$parent_ed_2_sim <- sample(1:9, size = 96, replace = TRUE, prob = c(.01, .01, .01, .06, .29, .24, .25, .10, .03))
table(dat_sim$parent_ed_2_sim)

# recode parent ed 2
dat_sim$parent_ed_2_sim <- recode(dat_sim$parent_ed_2_sim,
                                  "1" = "No school",
                                  "2" = "Elementary school",
                                  "3" = "Middle school/Junior high",
                                  "4" = "Some high school",
                                  "5" = "High school diploma",
                                  "6" = "Associates degree or equivalent (2 year degree)",
                                  "7" = "Bachelor's degree (4 year degree)",
                                  "8" = "Master's degree",
                                  "9" = "Doctorate or professional degree")

# missingness
dat_sim <- delete_MCAR(dat_sim, .04, c("w1_loc_1_sim", "w1_loc_2_sim", "w1_loc_3_sim", "w1_loc_4_sim", "w1_loc_5_sim"))
dat_sim <- delete_MCAR(dat_sim, .08, c("w2_loc_1_sim", "w2_loc_2_sim", "w2_loc_3_sim", "w2_loc_4_sim", "w2_loc_5_sim"))
dat_sim <- delete_MCAR(dat_sim, .12, c("w3_loc_1_sim", "w3_loc_2_sim", "w3_loc_3_sim", "w3_loc_4_sim", "w3_loc_5_sim"))
dat_sim <- delete_MCAR(dat_sim, .16, c("w4_loc_1_sim", "w4_loc_2_sim", "w4_loc_3_sim", "w4_loc_4_sim", "w4_loc_5_sim"))

# check
dat_sim %>% 
  select(w1_loc_1_sim:w4_loc_5_sim) %>% 
  freq()
dat_sim

# save new file
write.csv(dat_sim, "./Data/loc_pathways_simulated_data_2025-11-08.csv", row.names = FALSE)