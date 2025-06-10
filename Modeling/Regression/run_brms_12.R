library(parallel)
library(brms)
library(bayesplot)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(dplyr)
library(rstan)

options(mc.cores = parallel::detectCores())
options(brms.backend = "rstan")
setwd("/home/control/yimzha/Documents")

data_pd <- read.csv("clean_pd_mds_med.csv")
data_pd <- data_pd %>% select(-mds)
data_pd <- na.omit(data_pd)
data_pd$group <- ifelse(data_pd$med_motor_PAL < 2, 'PD_Under', 'PD_OK')
data_pd <- data_pd[, !grepl("med", names(data_pd))] # remove all columns containing "med"

data_hc <- read.csv("clean_hc.csv")
data_hc$group <- 'HC'
data <- rbind(data_pd, data_hc)
data$group <- factor(data$group, levels = c("HC", "PD_Under", "PD_OK"))

data <- data[data$pcor != 0, ]
data <- data[data$learning != -1, ]

data$learning <- factor(data$learning, levels = c(0, 1), labels = c("Early", "Late"))
data$ns <- ifelse(data$ns %in% c(2, 3), "Low", "High")
data$ns <- factor(data$ns, levels = c("Low", "High"))

# Scale
data <- data %>%
  mutate(
    pcor = as.vector(scale(pcor, scale = TRUE, center = TRUE)),
    iterseq = as.vector(scale(iterseq, scale = TRUE, center = TRUE)),
    delay = as.vector(scale(delay, scale = TRUE, center = TRUE)),
  )

######################################################
fit_12 <- brm(
  formula = correct ~ group*learning*ns + (1 + group*learning*ns | subno),
  data = data,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0, 0.2), class = "sd")
  ),
  control = list(adapt_delta = 0.99),
  chains = 4,
  iter = 3000,
  warmup = 1000,
  seed = 123
)
save(fit_12, file = "fits/fit_12.RData")

print("Success!!!!!")