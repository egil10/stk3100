library(tidyverse)
library(broom)
library(MASS)

set.seed(3100)
theme_set(theme_bw())

# problem 2 ---------------------------------------------------------------

# read data (same as P1)
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

# f) Fit NB-GLM for the preferred model from Problem 1 (no TV)
# Poisson reference (from P1)
fit_pois <- glm(
  ceb ~ educ0 + usemeth + urban + electric + radio + bicycle,
  data = fertility,
  family = poisson(link = "log")
)

# Negative binomial (MASS::glm.nb uses log link by default)
fit_nb <- glm.nb(
  ceb ~ educ0 + usemeth + urban + electric + radio + bicycle,
  data = fertility, link = log
)

summary(fit_nb)
tidy(fit_nb, exponentiate = TRUE, conf.int = TRUE)

# Theta (k) estimate and SE
theta_hat <- fit_nb$theta
theta_se  <- fit_nb$SE.theta
c(theta_hat = theta_hat, theta_se = theta_se)

# Compare Poisson vs NB
comp_aic <- AIC(fit_pois, fit_nb)
comp_ll  <- tibble(
  model = c("Poisson", "NegBin"),
  logLik = c(as.numeric(logLik(fit_pois)), as.numeric(logLik(fit_nb))),
  df = c(attr(logLik(fit_pois), "df"), attr(logLik(fit_nb), "df"))
)

comp_aic
comp_ll

# LR-style comparison (treat NB as Poisson with extra parameter k)
lr_stat <- 2 * (as.numeric(logLik(fit_nb)) - as.numeric(logLik(fit_pois)))
lr_pval <- pchisq(lr_stat, df = 1, lower.tail = FALSE)
c(lr_stat = lr_stat, df = 1, p_value = lr_pval)

# Pearson dispersion check
disp_pois <- sum(residuals(fit_pois, type = "pearson")^2) / df.residual(fit_pois)
disp_nb   <- sum(residuals(fit_nb,   type = "pearson")^2) / df.residual(fit_nb)
c(pearson_overdisp_poisson = disp_pois, pearson_overdisp_negbin = disp_nb)

# Residuals vs fitted for NB (diagnostic)
resid_nb_plot <- fertility %>%
  mutate(
    fitted = fitted(fit_nb),
    resid  = residuals(fit_nb, type = "pearson")
  ) %>%
  ggplot(aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Fitted values (NB-GLM)", y = "Pearson residuals") +
  theme_bw()

ggsave("plots/problem2_nb_residuals_vs_fitted.pdf", resid_nb_plot, width = 7, height = 4)

# Rate ratios from NB model (for interpretation in the report)
nb_rate_ratios <- tidy(fit_nb, exponentiate = TRUE, conf.int = TRUE) %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)

nb_rate_ratios

# Prediction for the same covariate combo as in P1(e)
newdata <- tibble(
  educ0   = factor(0, levels = c(0,1)),
  usemeth = factor(0, levels = c(0,1)),
  urban   = factor(0, levels = c(0,1)),
  electric = factor(1, levels = c(0,1)),
  radio    = factor(1, levels = c(0,1)),
  bicycle  = factor(1, levels = c(0,1))
)

pred_nb <- predict(fit_nb, newdata = newdata, type = "link", se.fit = TRUE)

nb_rate_est  <- exp(pred_nb$fit)
nb_rate_low  <- exp(pred_nb$fit - 1.96 * pred_nb$se.fit)
nb_rate_high <- exp(pred_nb$fit + 1.96 * pred_nb$se.fit)

cbind(newdata, nb_rate_est, nb_rate_low, nb_rate_high)
