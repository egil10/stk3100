

library(tidyverse)

fertility = read.csv("data/fertility_data.csv",
         header = TRUE)


fertility %>% as_tibble()
