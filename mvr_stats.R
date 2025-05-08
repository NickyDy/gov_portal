library(tidyverse)
library(tidytext)

stats_2023 <- read_csv("data/pol_stats.csv") %>% mutate(across(where(is.numeric), round, 1))
stats_2024 <- read_csv("https://data.egov.bg/resource/download/9d2cc270-faf4-4427-a18b-ce950a51da7b/csv", skip = 12,
                       col_names = c("no", "crime_types", "registered_crimes.2024", "crimes_100000_people.2024",
                                     "solved_crimes_from_all_registered.2024", "percent_solved.2024",
                                     "criminals_found.2024", "registered_crimes_women.2024",
                                     "minor_aged_14_17_years.2024", "foreign_citizens.2024",
                                     "past_crimes_solved_current_year.2024", "past_crimes_criminals_found.2024")) %>% 
  mutate(across(where(is.numeric), round, 1)) %>% select(-no)
df <- read_csv("https://data.egov.bg/resource/download/7d075b6c-21ca-46c0-be4a-3be4586384a4/csv") %>% drop_na()

glimpse(stats_2023)

mvr_stats %>% 
  filter(!location == "Общо за Р България", 
         #str_detect(crime_types, "убийство|Убийство"),
         crime_types == "Убийство (чл.115-127 НК)") %>%
  mutate(location = reorder_within(location, percent_solved.2023, crime_types)) %>%
  filter(percent_solved.2023 > 0) %>% 
  ggplot(aes(percent_solved.2023, location, fill = percent_solved.2023)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = percent_solved.2023), 
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
             #labeller = labeller(crime_types = label_wrap_gen(40)))









