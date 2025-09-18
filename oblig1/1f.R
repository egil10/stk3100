
# f)

library(tidyverse)

wingspan <- read.table("data/wingspan.txt",
                       header = TRUE)
wingspan

M0 <- lm(Wingspan~0+Height,
        data = wingspan)

M1 = lm(Wingspan~Height,
        data = wingspan)

anova(M0,M1)




