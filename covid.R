library(tidyverse)

covid <- read_csv("https://data.egov.bg/resource/download/e59f95dd-afde-43af-83c8-ea2916badd19/csv",
                  col_names = c("Дата", "Направени тестове", "Направени тестове за денонощие", 
                                "Потвърдени случаи", "Активни случаи", "Нови случаи", "Хоспитализирани",
                                "Нови хоспитализирани", "В интензивното", "Излекувани", "Излекувани за денонощие", 
                                "Починали", "Починали за денонощие")) %>% 
  slice(-c(1:1)) %>% mutate(across(-Дата, as.double)) %>% mutate(Дата = as.Date(Дата))
glimpse(covid)

covid %>% 
  pivot_longer(-Дата) %>% 
  ggplot(aes(Дата, value)) +
  geom_line() +
  scale_y_continuous(labels = label_number()) +
  facet_wrap(vars(name), scales = "free_y") +
  labs(x = "Година", y = "Брой") +
  theme(text = element_text(size = 16))