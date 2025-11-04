library(tidyverse)
library(broom)
library(MASS)

set.seed(3100)
theme_set(theme_bw())

# problem 1 ---------------------------------------------------------------

# read data
fertility <- read.csv("data/fertility_data.csv", header = TRUE) %>%
  as_tibble() %>%
  mutate(
    educ0    = as.factor(educ0),
    usemeth  = as.factor(usemeth),
    urban    = as.factor(urban),
    electric = as.factor(electric),
    radio    = as.factor(radio),
    tv       = as.factor(tv),
    bicycle  = as.factor(bicycle)
  )

# a)
cat("The response variable ceb is a count (number of children).
A Poisson model with log-link is natural when births are modeled
as independent events with constant rate within covariate groups.\n")

# b) full Poisson GLM
fit_full <- glm(
  ceb ~ educ0 + usemeth + urban + electric + radio + tv + bicycle,
  data = fertility,
  family = poisson(link = "log")
)

summary(fit_full)
tidy(fit_full, exponentiate = TRUE, conf.int = TRUE)

# diagnostic plot: Pearson residuals vs fitted
resid_plot <- fertility %>%
  mutate(
    fitted = fitted(fit_full),
    resid = residuals(fit_full, type = "pearson")
  ) %>%
  ggplot(aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Fitted mean", y = "Pearson residuals") +
  theme_bw()

ggsave("plots/problem1_residuals_vs_fitted.pdf", resid_plot, width = 7, height = 4)

# c) model selection
drop1(fit_full, test = "Chisq")

fit_no_tv <- update(fit_full, . ~ . - tv)
fit_no_bicycle <- update(fit_no_tv, . ~ . - bicycle)

anova(fit_full, fit_no_tv, test = "Chisq")
anova(fit_no_tv, fit_no_bicycle, test = "Chisq")

AIC(fit_full, fit_no_tv, fit_no_bicycle)

# choose best model
fit_best <- fit_no_tv
summary(fit_best)

# d) rate ratios
rate_ratios <- tidy(fit_best, exponentiate = TRUE, conf.int = TRUE) %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)

rate_ratios

# e) prediction for given covariate combination
newdata <- tibble(
  educ0   = factor(0, levels = c(0,1)),
  usemeth = factor(0, levels = c(0,1)),
  urban   = factor(0, levels = c(0,1)),
  electric = factor(1, levels = c(0,1)),
  radio    = factor(1, levels = c(0,1)),
  bicycle  = factor(1, levels = c(0,1))
)

pred <- predict(fit_best, newdata = newdata, type = "link", se.fit = TRUE)

rate_est <- exp(pred$fit)
rate_low <- exp(pred$fit - 1.96 * pred$se.fit)
rate_high <- exp(pred$fit + 1.96 * pred$se.fit)

cbind(newdata, rate_est, rate_low, rate_high)
