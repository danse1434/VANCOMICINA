

require(tidyverse)

data <- read_csv('./data/data_TAD.csv', na = '.')

data1 <- data %>%
  mutate(OUT = if_else((ID == 1 & !is.na(YTYPE)), 1, 0))

write_csv(data1, './data/processed/data_TAD_mod.csv', na = '.')
