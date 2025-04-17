library(tidyverse)
library(jsonlite)
library(scales)

space_s <- function (x, accuracy = NULL, scale = 1, prefix = "", suffix = "", 
                     big.mark = " ", decimal.mark = ".", trim = TRUE, digits, 
                     ...)
{
  if (!missing(digits)) {
    lifecycle::deprecate_stop(when = "1.0.0", what = "comma(digits)", 
                              with = "comma(accuracy)")
  }
  number(x = x, accuracy = accuracy, scale = scale, prefix = prefix, 
         suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark, 
         trim = trim, ...)
}

obsh_poruchki <- read_csv("https://data.egov.bg/resource/download/0809e6c3-90b2-4fcd-889c-9130f8dabfbe/csv") %>% 
  janitor::clean_names()
glimpse(obsh_poruchki)

obsh_poruchki %>% 
  count(obekt_na_por_ckata, data_na_dogovor, izp_lnitel, v_zlozitel, wt = stojnost_pri_sklucvane, sort = T) %>%
  filter(v_zlozitel == "ОБЩИНА ЯМБОЛ") %>% count(obekt_na_por_ckata, izp_lnitel, wt = n, sort = T)

subs_2023 <- fromJSON("https://data.egov.bg/resource/download/d1031bc1-7cdf-4a27-8716-899a4877ad76/json") %>% as_tibble()

colnames(subs_2023) <- c('row_n','fin_year','eik', "benef", 
                      "oblast", "ЕФГЗ-ДП", "ЕФГЗ", "ЕЗФРСР-НБ", "total", "pub_sklad", "desc", "mqrka")

subs_2023 <- subs_2023 %>%
  select(-row_n) %>% slice(-c(1:1)) %>% 
  mutate(across(5:8, parse_number))

sub_2023 %>% mutate(benef = str_replace_all(benef, '\\"', ""), 
                    benef = str_replace_all(benef, "\\'", ""),
                    benef = str_squish(benef)) %>% 
  summarise(t_sum = sum(total), .by = benef) %>% filter(t_sum > 50000) %>% arrange(-t_sum) %>% view
  mutate(benef = fct_recode(benef, "ЕТ Зоров 91 Димитър Зоров" = "ЕТ Зоров 91Димитър Зоров"),
         benef = str_squish(benef)) %>% 
  summarise(tot_sub = sum(total), .by = c(oblast, mqrka, benef)) %>%
  filter(tot_sub > 1500000) %>%
  mutate(benef = fct_reorder(benef, tot_sub),
         mqrka = str_wrap(mqrka, 20)) %>%
  ggplot(aes(tot_sub, benef, fill = oblast)) +
  geom_col() +
  geom_text(aes(label = space_s(tot_sub)), hjust = -0.05) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.5))) +
  theme(text = element_text(size = 16), legend.position = "top",
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  labs(x = "Обща сума (лв)", y = NULL, fill = "Област:") +
  facet_wrap(vars(mqrka), nrow = 1) +
  guides(fill = guide_legend(nrow = 2))

sub_2023 %>% filter(oblast == "Ямбол") %>% 
  mutate(benef = str_squish(benef)) %>% 
  summarise(tot_sub = sum(total), .by = c(oblast, mqrka, benef)) %>%
  filter(tot_sub > 200000) %>%
  mutate(benef = fct_reorder(benef, tot_sub),
         mqrka = str_wrap(mqrka, 20)) %>%
  ggplot(aes(tot_sub, benef, fill = mqrka)) +
  geom_col() +
  geom_text(aes(label = space_s(tot_sub)), hjust = -0.05) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.5))) +
  theme(text = element_text(size = 16), legend.position = "none",
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  labs(x = "Обща сума (лв)", y = NULL, fill = "Област:") +
  facet_wrap(vars(mqrka), nrow = 1) +
  guides(fill = guide_legend(nrow = 2))

lekari <- fromJSON("https://data.egov.bg/resource/download/65bdde36-4f0e-4b7a-b2ca-77bd47a022a4/json") %>% as_tibble() %>% 
  janitor::row_to_names(row_number = 1)

koncesii <- read_csv("gov_portal/konces.csv")





