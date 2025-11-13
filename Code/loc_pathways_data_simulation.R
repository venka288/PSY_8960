#######################################
#### Locus of Control Pathways Project
#### Data Simulation
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
dat_sim$race_sim <- sample(1:6, size = 96, replace = TRUE, prob = c(.34, .23, .20, .11, .06, .06))

# recode race
dat_sim$race_sim <-  recode(dat_sim$race_sim,
                            "1" = "Asian/Pacific Islander",
                            "2" = "Black/African American",
                            "3" = "Hispanic/Latine",
                            "4" = "Multiracial",
                            "5" = "Native American",
                            "6" = "Other"
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