library(tidyverse)
library(scales)

und_2024 <- read_csv("2024 г. - Подземни води - Обща физикохимия.csv", col_names = F) %>% 
  mutate(across(is.numeric, as.character))

dun_2024 <- read_csv("Басейнова дирекция - Дунавски район.csv", col_names = F)
black_2024 <- read_csv("Басейнова дирекция - Черноморски район.csv", col_names = F)
west_2024 <- read_csv("Басейнова дирекция - Западнобеломорски район.csv", col_names = F)
east_2024 <- read_csv("Басейнова дирекция - Източнобеломорски район.csv", col_names = F)

surf <- bind_rows(dun_2024, black_2024, west_2024, east_2024)

under_new <- und_2024 %>%
  #slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:5, sep = "_") %>%
  filter(!str_detect(united, "^NA")) %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(15:37, names_to = "name", values_to = "value") %>%
  separate(name, c("pokazatel", "m_edinica", "standart", "izmervane"), sep = "_") %>%
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
         date = ymd(date),
         year = year(date))

under <- under_new %>% 
  mutate(m_edinica = str_replace(m_edinica, "\\?C", "\u00B0C")) %>%
  filter(value > 0, izmervane == "Изм.ст-ст") %>% 
  select(basin, oblast, obshtina, sett, site_name, lat, long, date, year,
         pokazatel, m_edinica, standart, izmervane, value) %>% 
  drop_na(standart)

under_old <- read_rds("shiny/und_water/underground_water.rds") %>% 
  mutate(year = year(date))
under_df <- bind_rows(under, under_old)

write_rds(under_df, "shiny/und_water/under_df.rds")

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


surf_water <- surf %>%
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
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(8:198, names_to = "name", values_to = "value") %>%
  separate(name, c("index", "m_edinica"), sep = "_") %>%
  #mutate(value = parse_number(value)) %>% 
  drop_na(value)

surf_water <- surf_water %>%
  select(basin = `Басейнова дирекция_NA`, site_code_iaos = `Код на пункта в ИАОС_NA`,
         site_code = `Код на пункта_NA`, site_name = `Име на пункта_NA`,
         water_source = `Воден ресурс_NA`, date = `Дата на пробовземане_NA`,
         hour = `Час на пробовземане_NA`, index, m_edinica, value) %>% 
  mutate(value = parse_number(value), date = dmy(date)) %>% 
  filter(value > 0) %>% drop_na(value)

surf_water <- surf_water %>%
  mutate(pdk = case_when(
    index == "Активна реакция рН - pH" ~ 7.5,
    index == "Електропроводимост" ~ 900,
    index == "Разтворен кислород" ~ 5,
    index == "БПК5 - BOD5" ~ 5,
    index == "Азот амониев - N-NH4" ~ 0.65,
    index == "Азот нитритен - N-NO2" ~ 0.06,
    index == "Азот нитратен - N-NO3" ~ 2.5,
    index == "Ортофосфати (като Р) - PO4-P" ~ 0.15)) %>% 
  mutate(col = case_when(
    index == "Активна реакция рН - pH" & value < pdk & value > pdk ~ "1",
    index == "Електропроводимост" & value > pdk ~ "1",
    index == "Разтворен кислород" & value < pdk ~ "1",
    index == "БПК5 - BOD5" & value > pdk ~ "1",
    index == "Азот амониев - N-NH4" & value > pdk ~ "1",
    index == "Азот нитритен - N-NO2" & value > pdk ~ "1",
    index == "Азот нитратен - N-NO3" & value > pdk ~ "1",
    index == "Ортофосфати (като Р) - PO4-P" & value > pdk ~ "1", .default = "0"))

surf_water <- surf_water %>% filter(!basin == "Басейнова дирекция") %>% 
  select(basin, site_name, date, index, m_edinica, pdk, col, value) %>% 
  drop_na(pdk)

surf_df <- bind_rows(surf_water, surf_old)

surf_old <- read_rds("shiny/und_water/surf_water.rds")
write_rds(surf_df, "shiny/und_water/surf_df.rds")

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