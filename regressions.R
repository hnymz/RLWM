library(lme4)
library(lmerTest)
library(lmtest)
library(sjPlot)
library(gridExtra)
library(dplyr)
library(ggplot2)

data <-read.csv("prep_all.csv")
# Remove the first block
data <- data[data$block != 0, ]
# Drop blocks where proportions of no responses exceed 33%
data <- data %>%
  group_by(subno, block) %>%  # Group by 'subno' and 'block'
  mutate(proportion_no_response = mean(correct == -1)) %>%  # Calculate the proportion of -1 responses
  filter(proportion_no_response <= 0.33) %>%  # Keep only groups with no response rate <= 33%
  select(-proportion_no_response)  # Remove the auxiliary column
# Remove trials without responses
data <- data[data$correct != -1, ]
df <- data

#####################
# Log rt
df$logrt = log(df$rt)
ggplot(data = df, aes(x = logrt)) +
  geom_histogram() 

# Center and z-score continuous variables within subject
df <- df %>%
  group_by(subno) %>%
  mutate(
    ns = as.vector(scale(ns, scale = TRUE, center = TRUE)),
    delay_corr = as.vector(scale(delay_corr, scale = TRUE, center = TRUE)),
    corr_count = as.vector(scale(corr_count, scale = TRUE, center = TRUE))
  ) %>%
  ungroup()

#####################
# Logistic regression on accuracy
model <- glmer(correct ~ ns + delay_corr + corr_count + (1 | subno), data = df, family = binomial(link="logit"))
summary(model)
tab_model(model, title = "Accuracy")
which(residuals(model) < -100)

plot_model(model,type = "pred", terms = c("ns"))
plot_model(model,type = "pred", terms = c("delay_corr"))
plot_model(model,type = "pred", terms = c("corr_count"))
#grid.arrange(p1, p2, p3)

# Test
model_fixed <- glm(
  correct ~ ns + delay_corr + corr_count,
  data = df,
  family = binomial
)
lrtest(model, model_fixed)

pred_prob_conditional <- predict(model, newdata = df, type = "response", re.form = NULL)
threshold <- 0.5
pred_class <- ifelse(pred_prob_conditional > threshold, 1, 0)
summary(pred_prob_conditional)

data$predicted_correct <- pred_class
write.csv(data, file = "predicted_all.csv", row.names = FALSE)

#####################
# RT regression
model_rt = lmer(logrt ~ ns + delay_corr + corr_count + (1 | subno), data = df, REML = F)
summary(model_rt)
tab_model(model_rt, title = "Reaction time")
plot_model(model_rt, type = "pred", terms = c("ns"))
plot_model(model_rt, type = "pred", terms = c("delay_corr"))
plot_model(model_rt, type = "pred", terms = c("corr_count"))

