library(dplyr)
library(cmdstanr)
library(posterior)

setwd("/home/control/yimzha/Documents")

###########################################################
# Read data
data_pd <- read.csv("modeling.csv")
data_pd <- data_pd[, -((ncol(data_pd) - 1):ncol(data_pd))]
data_pd$group <- 1
data_hc <- read.csv("modeling_hc.csv")
data_hc$group <- 0
Behav <- rbind(data_pd, data_hc)

# Number of participants
subject_list <- unique(Behav$subno)
N <- length(subject_list)

# Number of trials for each participant
trials_per_subject <- Behav %>%
  group_by(subno) %>%
  summarise(n_trials = n()) %>%
  pull(n_trials)
# Maximal number of trials
Tmax <- max(trials_per_subject)
# Group (hc or pd)
group_vec <- sapply(subject_list, function(sid) {
  Behav$group[which(Behav$subno == sid)[1]]
})

# Initialize matrices
choice_mat <- matrix(0, nrow = N, ncol = Tmax)
outcome_mat <- matrix(0, nrow = N, ncol = Tmax)
mask_mat <- matrix(0, nrow = N, ncol = Tmax)

# Fill in real data
for (i in 1:N) {
  subj_id <- subject_list[i]
  subj_data <- Behav %>% filter(subno == subj_id)
  
  Ti <- nrow(subj_data) # Number of trials for the current participant
  
  choice_mat[i, 1:Ti] <- subj_data$choice
  outcome_mat[i, 1:Ti] <- subj_data$correct
  mask_mat[i, 1:Ti] <- 1 # Real trial is coded as 1
}

stan_data <- list(
  N = N,
  Tmax = Tmax,
  choice = choice_mat,
  outcome = outcome_mat,
  mask = mask_mat,
  group = group_vec
)

#################################################
model <- cmdstan_model("RLWM/Modeling/Models/RL.stan")
fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 5000,
  save_warmup = FALSE
)
draws_subset <- fit$draws(variables = c("mu_pd", "mu_hc", "sigma", "alpha", "beta"))
saveRDS(draws_subset, file = "RL_draws.rds")

print("Success!!!!!")
