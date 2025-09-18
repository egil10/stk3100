

# 2

library(tidyverse)

# a)

titanic <- read.table("data/titanic.txt",
                      header = TRUE)

titanic

glm(survived~pclass,
    data = titanic,
    family = binomial) %>% 
  summary()

# c)

glm(survived~pclass+age,
    data = titanic,
    family = binomial) %>% 
  summary()

# d)

M0 <- glm(survived~pclass, data = titanic, family = binomial)
M1 <- glm(survived~pclass+age, data = titanic, family = binomial)

wald <- summary(M1)$coefficients["age",]
wald

lrt <- anova(M0,M1,test = "Chisq")
lrt

# e)

M0 <- glm(survived~pclass, data = titanic, family = binomial)
M1 <- glm(survived~pclass+age, data = titanic, family = binomial)

M0 %>% confint()
M1 %>% confint()
M0 %>% confint() %>% exp()
M1 %>% confint() %>% exp()





