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

data_pd <- read.csv("modeling.csv")
data_pd <- data_pd[, -((ncol(data_pd) - 1):ncol(data_pd))]
data_pd$group <- 1
data_hc <- read.csv("modeling_hc.csv")
data_hc$group <- 0
data <- rbind(data_pd, data_hc)

# Scale
data <- data %>%
  group_by(subno)%>%
  mutate(
    ns = as.vector(scale(ns, scale = TRUE, center = TRUE)),
    pcor = as.vector(scale(pcor, scale = TRUE, center = TRUE)),
  ) %>%
  ungroup()

# Specify priors
priors <- set_prior("normal(0, 2)", class = "b")

######################################################
fit_0 <- brm(
  correct ~ group + (1 | subno),
  data = data,
  family = bernoulli(),
  chains = 4,
  iter = 8000,
  warmup = 3000,
  control = list(adapt_delta = 0.99),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0, 0.2), class = "sd")
  )
)
save(fit_0, file = "fits/fit_0_norandomslope.RData")

print("Success!!!!!")