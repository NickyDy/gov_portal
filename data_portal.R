library(tidyverse)
library(scales)

obsh_poruchki <- read_csv("https://data.egov.bg/resource/download/0809e6c3-90b2-4fcd-889c-9130f8dabfbe/csv") %>% 
  janitor::clean_names()
glimpse(obsh_poruchki)

obsh_poruchki %>% 
  count(obekt_na_por_ckata, data_na_dogovor, izp_lnitel, v_zlozitel, wt = stojnost_pri_sklucvane, sort = T) %>%
  filter(v_zlozitel == "ОБЩИНА ЯМБОЛ") %>% count(obekt_na_por_ckata, izp_lnitel, wt = n, sort = T)