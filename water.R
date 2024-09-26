library(tidyverse)
library(scales)

zaustvane <- read_csv("https://data.egov.bg/resource/download/becf1f8f-14d2-44c6-87ce-28d35389e2ee/csv")
vodovzemane <- read_csv("https://data.egov.bg/resource/download/00f66760-2ac9-42b9-bfa9-33f7c7d9d924/csv")
glimpse(migr)

vod <- vodovzemane %>% 
  select(name = `Титуляр Име`, lat = `Десетични координати N`, long = `Десетични координати E`) %>% 
  mutate(lat = as.numeric(lat), long = as.numeric(long)) %>% drop_na()

surf_water_east <- read_csv("https://data.egov.bg/resource/download/4adad97f-0859-43fb-8c66-6100d7656dad/csv",
                            col_names = F)
surf_water_west <- read_csv("https://data.egov.bg/resource/download/11c55246-122e-43d7-a731-7ace1a6a231c/csv",
                            col_names = F)
surf_water_black <- read_csv("https://data.egov.bg/resource/download/8f5979d8-1447-41c4-b677-3981976f987e/csv",
                             col_names = F)
surf_water_dun <- read_csv("https://data.egov.bg/resource/download/50a4d414-a388-49e1-ba90-24ec61e8bf07/csv",
                           col_names = F)
surf_water <- bind_rows(surf_water_east, surf_water_west, surf_water_black, surf_water_dun)

under_water <- read_csv("data/under_water.csv",
                        col_names = F) %>% mutate(across(is.double, as.character))

und_water <- under_water %>%
  #slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:5, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  #mutate(`1` = str_replace(`1`, "__", "obshtina")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(14:105, names_to = "name", values_to = "value") %>%
  separate(name, c("pokazatel", "m_edinica", "standart", "izmervane"), sep = "_") %>%
  filter(!izmervane == "Метод") %>% 
  mutate(value = parse_number(value)) %>% 
  drop_na(value) %>% 
  select(basin = `Басейнов район_NA_NA_NA`, code = `Код на ПВТ_NA_NA_NA`,
         name = `Име на ПВТ_NA_NA_NA`, oblast = Област_NA_NA_NA,
         obshtina = Община_NA_NA_NA, sett = `Населено място_NA_NA_NA`,
         site_code = `Код на пункт_NA_NA_NA`, old_code = `Стар код_NA_NA_NA`,
         site_name = `Име на пункт_NA_NA_NA`, lat = Геогр.дължина_NA_NA_NA,
         long = Геогр.ширина_NA_NA_NA, date = `Дата на пробовземане_NA_NA_NA`,
         lab = Лаборатория_NA_NA_NA, everything()) %>% 
  mutate(long = parse_number(long),
         lat = parse_number(lat),
         standart = parse_number(standart),
         date = ymd(date))

underground_water <- und_water %>% 
  mutate(m_edinica = str_replace(m_edinica, "\\?C", "\u00B0C")) %>%
  filter(value > 0, izmervane == "Изм.ст-ст") %>% 
  select(basin, oblast, obshtina, sett, site_name, lat, long, date,
         pokazatel, m_edinica, standart, izmervane, value) %>% 
  drop_na(standart)

write_parquet(underground_water, "shiny/und_water/underground_water.parquet")

und_water %>% filter(value > standart * 10 & izmervane == "Изм.ст-ст", date > "2023-07-01") %>%
  mutate(site_name = fct_reorder(site_name, value)) %>% 
  ggplot(aes(value, site_name, fill = pokazatel)) +
  geom_col(show.legend = F) +
  scale_x_continuous(expand = expansion(mult = c(.01, .2))) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = 1, size = 10, size.unit = "pt") +
  geom_vline(aes(xintercept = standart), linewidth = 0.7, lty = 2, color = "red") +
  theme(text = element_text(size = 14)) +
  facet_grid(pokazatel ~ date, scales = "free_x")

und_water %>%
  mutate(over = standart - value, sign = over > 0) %>%
  filter(oblast == "Ямбол", izmervane == 'Изм.ст-ст') %>% drop_na() %>%
  count(oblast, site_name, pokazatel, sign) %>% 
  mutate(sign = as.factor(sign)) %>% 
  mutate(sign = fct_recode(sign, 
                           "Над нормата" = "FALSE", "В нормата" = "TRUE")) %>% 
  ggplot(aes(n, pokazatel, fill = sign)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  geom_text(aes(label = n), 
            position = position_dodge(width = 1), hjust = -0.05, size = 8, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .2))) +
  scale_fill_manual(values = c("Над нормата" = "red", "В нормата" = "green")) +
  theme(text = element_text(size = 14)) +
  labs(x = "Брой измервания", y = NULL, fill = "Легенда:") +
  facet_wrap(vars(site_name), ncol = 6, labeller = labeller(site_name = label_wrap_gen(35)))


surf_water <- surf_water %>%
  #slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  #mutate(`1` = str_replace(`1`, "__", "obshtina")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(8:177, names_to = "name", values_to = "value") %>%
  #separate(name, c("year", "settlement", "sex"), sep = "_") %>%
  #mutate(value = parse_number(value)) %>% 
  drop_na(value)

surf_water <- surf_water %>%
  select(basin = `Басейнова дирекция_NA`, site_code_iaos = `Код на пункта в ИАОС_NA`,
         site_code = `Код на пункта_NA`, site_name = `Име на пункта_NA`,
         water_source = `Воден ресурс_NA`, date = `Дата на пробовземане_NA`,
         hour = `Час на пробовземане_NA`, index = name, value) %>% 
  mutate(value = parse_number(value), date = dmy(date)) %>% 
  filter(value > 0) %>% drop_na(value)

glimpse(und_water)
surf_waters %>% count(basin) %>% view
surf_waters %>% count(site_name, sort = T) %>% view

surf_water <- surf_water %>%
  mutate(pdk = case_when(
    index == "Активна реакция рН - pH_-" ~ 7.5,
    index == "Електропроводимост_µS/cm" ~ 900,
    index == "Разтворен кислород_mg/l" ~ 5,
    index == "БПК5 - BOD5_mg/l" ~ 5,
    index == "Азот амониев - N-NH4_mg/l" ~ 0.65,
    index == "Азот нитритен - N-NO2_mg/l" ~ 0.06,
    index == "Азот нитратен - N-NO3_mg/l" ~ 2.5,
    index == "Ортофосфати (като Р) - PO4-P_mg/l" ~ 0.15)) %>% 
  mutate(col = case_when(
    index == "Активна реакция рН - pH_-" & value < pdk & value > pdk ~ "1",
    index == "Електропроводимост_µS/cm" & value > pdk ~ "1",
    index == "Разтворен кислород_mg/l" & value < pdk ~ "1",
    index == "БПК5 - BOD5_mg/l" & value > pdk ~ "1",
    index == "Азот амониев - N-NH4_mg/l" & value > pdk ~ "1",
    index == "Азот нитритен - N-NO2_mg/l" & value > pdk ~ "1",
    index == "Азот нитратен - N-NO3_mg/l" & value > pdk ~ "1",
    index == "Ортофосфати (като Р) - PO4-P_mg/l" & value > pdk ~ "1", .default = "0"))

surf_water %>% count(basin) %>% view
surf_water <- surf_water %>% filter(!basin == "Басейнова дирекция") %>% 
  select(basin, site_name, date, index, pdk, col, value) %>% 
  drop_na(pdk)
write_parquet(surf_water, "shiny/und_water/surf_water.parquet")

surf_water %>% 
  filter(site_name %in% c('р. Тунджа на моста за с. Срем'),
         index %in% c(
           "Активна реакция рН - pH_-",
           "Разтворен кислород_mg/l",
           "БПК5 - BOD5_mg/l",
           "Азот амониев - N-NH4_mg/l",
           "Азот нитратен - N-NO3_mg/l")) %>% 
  mutate(date = as.factor(date)) %>% 
  ggplot(aes(date, value, fill = col)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("0" = "green", "1" = "red")) +
  geom_hline(aes(yintercept = pdk), linewidth = 0.7, lty = 2, color = "red") +
  theme(text = element_text(size = 16), legend.position = "none") +
  labs(title = "р. Тунджа на моста за с. Срем", 
       x = "Месец", y = "Измерена стойност") +
  facet_wrap(vars(index))
df %>% 
  filter(site_name %in% c('р. Тунджа на моста за с. Срем'),
         index %in% c(
           "Електропроводимост_µS/cm")) %>% 
  ggplot(aes(month, value, fill = col)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("0" = "green", "1" = "red")) +
  geom_hline(aes(yintercept = pdk), linewidth = 0.7, lty = 2, color = "red") +
  theme(text = element_text(size = 16), legend.position = "none") +
  labs(title = "р. Тунджа на моста за с. Срем", 
       x = "Месец", y = "Измерена стойност") +
  facet_wrap(vars(index))
df %>% 
  filter(site_name %in% c('р. Тунджа на моста за с. Срем'),
         index %in% c(
           "Азот нитритен - N-NO2_mg/l",
           "Ортофосфати (като Р) - PO4-P_mg/l"
         )) %>% 
  ggplot(aes(month, value, fill = col)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("0" = "green", "1" = "red")) +
  geom_hline(aes(yintercept = pdk), linewidth = 0.7, lty = 2, color = "red") +
  theme(text = element_text(size = 16), legend.position = "none") +
  labs(title = "р. Тунджа на моста за с. Срем", 
       x = "Месец", y = "Измерена стойност") +
  facet_wrap(vars(index))

surf_waters %>%
  filter(str_detect(site_name, "Ханово"), str_detect(index, "µg")) %>% 
  ggplot(aes(date, value, color = index)) +
  geom_line(show.legend = F, size = 1) +
  geom_point(show.legend = F) +
  facet_wrap(vars(index), scales = "free_y")