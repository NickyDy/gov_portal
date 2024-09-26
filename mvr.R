library(tidyverse)

ptp <- read_csv("https://data.egov.bg/resource/download/b0ef6d47-def9-4573-902e-c25170defd4f/csv")
ptp2 <- read_csv("https://data.egov.bg/resource/download/a2bd53fb-d6e9-496c-b36b-2fa30e2a0944/csv")
ptp_age <- read_csv("https://data.egov.bg/resource/download/fec9a876-500c-43c5-a269-2e059cf549e3/csv")
ptp_road <- read_csv("https://data.egov.bg/resource/download/1b9c6b24-af8f-4e7b-beac-ffb0e6b20737/csv")

age_jan_june_2024 <- read_csv("https://data.egov.bg/resource/download/0460f4f2-5644-4d9d-b460-ff9cdbab6172/csv",
                              col_names = c("age_group", "Загинали_d", "Ранени_d", "Загинали_p", "Ранени_p", "Загинали_t", "Ранени_t",
                                            "Ранени_w", "Загинали", "Ранени"), skip = 1)
road_type_jan_june_2024 <- read_csv("https://data.egov.bg/resource/download/7ab2b0be-5aea-46f5-b593-9d803b33d2a2/csv",
                                    col_names = c("road_type", "ПТП", "ПТП (%)", "Загинали", "Загинали (%)", "Ранени", "Ранени (%)",
                                                  "ptp_23", "ptp_perc_23", "Загинали_23", "Загинали_perc_23", "Ранени_23", "Ранени_perc_23"), 
                                    skip = 1)
obl_2024 <- read_csv("https://data.egov.bg/resource/download/4387455d-fd50-4022-9ad2-78ba46547c00/csv",
                     col_names = c("obl", "ПТП", "Загинали", "Ранени", "ptp_23", "Загинали_23", "Ранени_23",
                                   "ПТП (разлика)", "diff_ptp_perc", "Загинали (разлика)", "diff_Загинали_perc",
                                   "Ранени (разлика)", "diff_Ранени_perc"), skip = 1)
weekdays_2024 <- read_csv("https://data.egov.bg/resource/download/b1f62ff6-b1e1-4585-8b5c-de0075d14ab0/csv",
                          col_names = c("hour", "ПТП (понеделник)", "Загинали (понеделник)", "Ранени (понеделник)", 
                                        "ПТП (вторник)", "Загинали (вторник)", "Ранени (вторник)",
                                        "ПТП (сряда)", "Загинали (сряда)", "Ранени (сряда)", 
                                        "ПТП (четвъртък)", "Загинали (четвъртък)", "Ранени (четвъртък)",
                                        "ПТП (петък)", "Загинали (петък)", "Ранени (петък)", 
                                        "ПТП (събота)", "Загинали (събота)", "Ранени (събота)",
                                        "ПТП (наделя)", "Загинали (наделя)", "Ранени (наделя)", 
                                        "ПТП (общо)", "Загинали (общо)", "Ранени (общо)"), 
                          skip = 1)
obl_months_2024 <- read_csv("https://data.egov.bg/resource/download/1584ca0c-7936-4389-88a5-81a304ae8e03/csv",
                            col_names = c("obl", "Загинали (януари)", "Ранени (януари)", "Загинали (февруари)", "Ранени (февруари)",
                                          "Загинали (март)", "Ранени (март)", "Загинали (април)", "Ранени (април)",
                                          "Загинали (май)", "Ранени (май)", "Загинали (юни)", "Ранени (юни)",
                                          "Загинали (общо)", "Ранени (общо)"), skip = 1)
ptp_months_2024 <- read_csv("https://data.egov.bg/resource/download/b04e3068-dfcd-4cc9-87d2-05adbadb0841/csv",
                            col_names = c("obl", "ПТП (януари)", "ПТП (февруари)", "ПТП (март)", "ПТП (април)",
                                          "ПТП (май)", "ПТП (юни)", "ПТП (общо)"), skip = 1)
glimpse(age_jan_june_2024)

age_jan_june_2024 %>% 
  pivot_longer(9:10) %>% 
  filter(!age_group == "Общо") %>% 
  mutate(age_group = fct_inorder(age_group)) %>% 
  ggplot(aes(value, age_group, fill = name)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.05, size = 14, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  scale_fill_manual(values = c("Загинали" = "black", "Ранени" = "red")) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой", y = NULL) +
  facet_wrap(vars(name))

road_type_jan_june_2024 %>% 
  pivot_longer(2:7) %>% 
  filter(!str_detect(name, "%"), !road_type == "Общо") %>%
  mutate(road_type = fct_reorder(road_type, value),
         name = fct_relevel(name, "ПТП", "Ранени", "Загинали")) %>% 
  ggplot(aes(value, road_type, fill = name)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.05, size = 14, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  scale_fill_manual(values = c("Загинали" = "black", "Ранени" = "red", "ПТП" = "orange")) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой", y = NULL) +
  facet_wrap(vars(name))

obl_2024 %>% 
  pivot_longer(2:4) %>%
  filter(!obl == "Общо") %>%
  mutate(name = fct_relevel(name, "ПТП", "Ранени", "Загинали"),
         obl = fct_reorder(obl, value)) %>% 
  ggplot(aes(value, obl, fill = name)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.05, size = 14, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  scale_fill_manual(values = c("Загинали" = "black", "Ранени" = "red", "ПТП" = "orange")) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой", y = NULL) +
  facet_wrap(vars(name))

obl_2024 %>% 
  pivot_longer(c(8, 10, 12)) %>%
  filter(!obl == "Общо") %>%
  mutate(name = fct_relevel(name, "ПТП (разлика)", "Ранени (разлика)", "Загинали (разлика)"),
         obl = fct_reorder(obl, value),
         col = value > 0) %>% 
  ggplot(aes(value, obl, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.05, size = 14, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  #scale_fill_manual(values = c("Загинали (разлика)" = "black", "Ранени (разлика)" = "red", "ПТП (разлика)" = "orange")) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой", y = NULL) +
  facet_wrap(vars(name))

weekdays_2024 %>% 
  pivot_longer(-hour) %>% 
  filter(str_detect(name, "^ПТП"), !str_detect(name, "общо"), !str_detect(hour, "Общо")) %>%
  mutate(hour = fct_rev(hour), name = fct_inorder(name),
         col = case_when(value > 40 ~ "2",
                         value > 20 & value <= 40 ~ "1",
                         .default = "0")) %>%
  ggplot(aes(value, hour, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.05, size = 14, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  scale_fill_manual(values = c("2" = "black", "0" = "orange", "1" = "red")) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой", y = NULL) +
  facet_wrap(vars(name), nrow = 1)

weekdays_2024 %>% 
  pivot_longer(-hour) %>% 
  filter(str_detect(name, "^Загинали"), !str_detect(name, "общо"), !str_detect(hour, "Общо")) %>%
  mutate(hour = fct_rev(hour), name = fct_inorder(name),
         col = case_when(value > 3 ~ "2",
                         value >= 2 & value <= 3 ~ "1",
                         .default = "0")) %>%
  ggplot(aes(value, hour, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.05, size = 14, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  scale_fill_manual(values = c("2" = "black", "0" = "orange", "1" = "red")) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой", y = NULL) +
  facet_wrap(vars(name), nrow = 1)

weekdays_2024 %>% 
  pivot_longer(-hour) %>% 
  filter(str_detect(name, "^Ранени"), !str_detect(name, "общо"), !str_detect(hour, "Общо")) %>%
  mutate(hour = fct_rev(hour), name = fct_inorder(name),
         col = case_when(value > 40 ~ "2",
                         value > 20 & value <= 40 ~ "1",
                         .default = "0")) %>%
  ggplot(aes(value, hour, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.05, size = 14, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  scale_fill_manual(values = c("2" = "black", "0" = "orange", "1" = "red")) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой", y = NULL) +
  facet_wrap(vars(name), nrow = 1)

weekdays_2024 %>% 
  pivot_longer(-hour) %>% 
  filter(str_detect(name, "общо"), !str_detect(hour, "Общо")) %>%
  mutate(hour = fct_rev(hour), 
         name = fct_relevel(name, "ПТП (общо)", "Ранени (общо)", "Загинали (общо)")) %>%
  ggplot(aes(value, hour, fill = name)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.05, size = 14, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  scale_fill_manual(values = c("Загинали (общо)" = "black", "Ранени (общо)" = "red", "ПТП (общо)" = "orange")) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой", y = NULL) +
  facet_wrap(vars(name), nrow = 1)

df_months <- inner_join(obl_months_2024, ptp_months_2024)

text_months <- df_months %>% 
  pivot_longer(-obl) %>% 
  filter(!str_detect(obl, "Общо"), !str_detect(name, "общо")) %>%
  mutate(obl = fct_reorder(obl, value), 
         name = fct_relevel(name, "ПТП (януари)", "ПТП (февруари)", "ПТП (март)", "ПТП (април)", "ПТП (май)", "ПТП (юни)",
                            "Ранени (януари)", "Ранени (февруари)", "Ранени (март)", "Ранени (април)", "Ранени (май)", 
                            "Ранени (юни)", "Загинали (януари)", "Загинали (февруари)", "Загинали (март)", "Загинали (април)", "Загинали (май)",
                            "Загинали (юни)"),
         col = case_when(str_detect(name, "Ранени") ~ "0", 
                         str_detect(name, "Загинали") ~ "1", 
                         .default = "2")) %>% summarise(s = sum(value), .by = name)

df_months %>% 
  pivot_longer(-obl) %>% 
  filter(!str_detect(obl, "Общо"), !str_detect(name, "общо")) %>%
  mutate(obl = fct_reorder(obl, value), 
         name = fct_relevel(name, "ПТП (януари)", "ПТП (февруари)", "ПТП (март)", "ПТП (април)", "ПТП (май)", "ПТП (юни)",
                            "Ранени (януари)", "Ранени (февруари)", "Ранени (март)", "Ранени (април)", "Ранени (май)", 
                            "Ранени (юни)", "Загинали (януари)", "Загинали (февруари)", "Загинали (март)", "Загинали (април)", "Загинали (май)",
                            "Загинали (юни)"),
         col = case_when(str_detect(name, "Ранени") ~ "0", 
                         str_detect(name, "Загинали") ~ "1", 
                         .default = "2")) %>%
  ggplot(aes(value, obl)) +
  geom_col(aes(fill = col), show.legend = F) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.05, size = 10, size.unit = "pt") +
  geom_text(data = text_months, aes(label = paste("Общо: ", s)), size = 5, vjust = -0.2, x = 60, y = 10) +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  scale_fill_manual(values = c("1" = "black", "0" = "red", "2" = "orange")) +
  theme(text = element_text(size = 10)) +
  labs(x = "Брой", y = NULL) +
  facet_wrap(vars(name), nrow = 3)
#---------------------------------
ptp %>% 
  select(1:4) %>%
  filter(!Области == "Общо") %>% 
  pivot_longer(-Области) %>% 
  mutate(name = fct_relevel(name, "ПТП бр., 2023 г.", "Ранени бр., 2023 г.", "Загинали бр., 2023 г."),
         Области = fct_reorder(Области, value)) %>% 
  ggplot(aes(value, Области, fill = name)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.05, size = 14, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
  scale_fill_manual(values = c("ПТП бр., 2023 г." = "orange", 
                               "Ранени бр., 2023 г." = "red",
                               "Загинали бр., 2023 г." = "black")) +
  theme(text = element_text(size = 14)) +
  labs(x = "Брой", y = NULL) +
  facet_wrap(vars(name))

ptp2 %>% 
  select(1:22) %>% 
  filter(!`Часови интервали` == "Общо") %>% 
  pivot_longer(-`Часови интервали`) %>% 
  mutate(`Часови интервали` = fct_collapse(`Часови интервали`,
                                           "От 00:00 До 06:59" = c("От 00 До 00:59", "От 01 До 01:59", "От 02 До 02:59", "От 03 До 03:59",
                                                                   "От 04 До 04:59", "От 05 До 05:59", "От 06 До 06:59"),
                                           "От 07:00 До 11:59" = c("От 07 До 07:59", "От 08 До 08:59", "От 09 До 09:59", "От 10 До 10:59",
                                                                   "От 11 До 11:59"),
                                           "От 12:00 До 17:59" = c("От 12 До 12:59", "От 13 До 13:59", "От 14 До 14:59", "От 15 До 15:59",
                                                                   "От 16 До 16:59", "От 17 До 17:59"),
                                           "От 18:00 До 23:59" = c("От 18 До 18:59", "От 19 До 19:59", "От 20 До 20:59", "От 21 До 21:59",
                                                                   "От 22 До 22:59", "От 23 До 23:59"))) %>% 
  group_by(`Часови интервали`, name) %>% summarise(value = sum(value)) %>% ungroup() %>% 
  mutate(type = str_match(name, "ПТП|Ранени|Загинали"),
         name = str_match(name, "понеделник|вторник|сряда|четвъртък|петък|събота|неделя")) %>% 
  mutate(name = fct_relevel(name, "понеделник", "вторник", "сряда",
                            "четвъртък", "петък", "събота", "неделя"),
         type = fct_relevel(type, "ПТП", "Ранени", "Загинали")) %>%
  ggplot(aes(value, `Часови интервали`, fill = type)) +
  geom_col(show.legend = F, position = "dodge") +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.1, size = 10, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .15))) +
  scale_fill_manual(values = c("ПТП" = "orange", 
                               "Ранени" = "red",
                               "Загинали" = "black")) +
  theme(text = element_text(size = 11)) +
  labs(x = "Брой", y = NULL) +
  facet_grid(type ~ name)

ptp_age %>% 
  pivot_longer(-`Възрастови групи`) %>% 
  filter(!str_detect(name, "общо"),
         !str_detect(`Възрастови групи`, "Общо")) %>% 
  mutate(`Възрастови групи` = fct_inorder(`Възрастови групи`),
         `Възрастови групи` = fct_rev(`Възрастови групи`)) %>% 
  ggplot(aes(value, `Възрастови групи`, fill = name)) +
  geom_col(show.legend = F, position = "dodge") +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.1, size = 16, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .15))) +
  scale_fill_manual(values = c("Ранени пътници" = "red",
                               "Ранени пешеходци" = "red",
                               "Ранени водачи" = "red",
                               "Загинали пътници" = "black",
                               "Загинали пешеходци" = "black",
                               "Загинали водачи" = "black",
                               "Ранени работници на пътя" = "red")) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой", y = NULL) +
  facet_wrap(vars(name))

ptp_road %>% 
  pivot_longer(-`Вид на пътя`) %>% 
  filter(!str_detect(name, "%"),
         !str_detect(name, "2022"),
         !str_detect(`Вид на пътя`, "Общо")) %>% 
  mutate(name = fct_relevel(name, "ПТП бр., 2023 г.", "Ранени бр., 2023 г.", "Загинали бр., 2023 г."),
         `Вид на пътя` = fct_reorder(`Вид на пътя`, value)) %>% 
  ggplot(aes(value, `Вид на пътя`, fill = name)) +
  geom_col(show.legend = F, position = "dodge") +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), hjust = -0.1, size = 16, size.unit = "pt") +
  scale_x_continuous(expand = expansion(mult = c(.01, .15))) +
  scale_fill_manual(values = c("ПТП бр., 2023 г." = "orange", 
                               "Ранени бр., 2023 г." = "red",
                               "Загинали бр., 2023 г." = "black")) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой", y = NULL) +
  facet_wrap(vars(name))

library(tidytext)

mvr_stats <- read_csv("data/pol_stats.csv") %>% mutate(crimes_100000_people = round(crimes_100000_people, 1),
                                                       percent_solved = round(percent_solved, 1))
glimpse(mvr_stats)

mvr_stats %>% 
  filter(!location == "Общо за Р България", 
         #str_detect(crime_types, "убийство|Убийство"),
         crime_types == "Убийство (чл.115-127 НК)") %>%
  mutate(location = reorder_within(location, percent_solved, crime_types)) %>%
  filter(percent_solved > 0) %>% 
  ggplot(aes(percent_solved, location, fill = percent_solved)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = percent_solved), 
            position = position_dodge(width = 1), 
            hjust = -0.1, size = 14, size.unit = "pt") +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(.01, .2))) +
  scale_fill_gradient(low = "red", high = "white") +
  labs(y = NULL, x = "Процент разкрити престъпления", 
       title = "Полицейска статистика за 2023 година!",
       caption = "Източник на данните: МВР") +
  theme(text = element_text(size = 18)) +
  facet_wrap(vars(crime_types), scales = "free_y", nrow = 2, 
             #labeller = labeller(crime_types = label_wrap_gen(40))
  )