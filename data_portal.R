library(tidyverse)
library(jsonlite)
library(scales)
library(nanoparquet)

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
#-------------------------------------------
stock_market0 <- read_csv("https://data.egov.bg/resource/download/407f3577-dbd3-40a5-96f3-513427354c7f/csv", 
                         col_names = c("product", "unit", "base", 
                                       "2025-06-23", "change_perc_07_07", "change_lv_07_07",
                                       "2025-06-24", "change_perc_07_08", "change_lv_07_08",
                                       "2025-06-25", "change_perc_07_09", "change_lv_07_09",
                                       "2025-06-26", "change_perc_07_10", "change_lv_07_10",
                                       "2025-06-27", "change_perc_07_11", "change_lv_07_11"), skip = 1) %>% 
  select(1:2, contains("2025")) %>% pivot_longer(3:7, names_to = "date", values_to = "price") %>% 
  mutate(unit = str_remove(unit, ","), date = ymd(date))

stock_market1 <- read_csv("https://data.egov.bg/resource/download/0442d438-2a74-4210-ac16-e9ed76436de0/csv",
                          col_names = c("product", "unit", "base", 
                                       "2025-07-07", "change_perc_07_07", "change_lv_07_07",
                                       "2025-07-08", "change_perc_07_08", "change_lv_07_08",
                                       "2025-07-09", "change_perc_07_09", "change_lv_07_09",
                                       "2025-07-10", "change_perc_07_10", "change_lv_07_10",
                                       "2025-07-11", "change_perc_07_11", "change_lv_07_11"), skip = 3) %>% 
  select(1:2, contains("2025")) %>% pivot_longer(3:7, names_to = "date", values_to = "price") %>% 
  mutate(unit = str_remove(unit, ","), date = ymd(date))

stock_market2 <- read_csv("https://data.egov.bg/resource/download/6f60f738-f47e-454b-ab49-593c3089b0bd/csv",
                          col_names = c("product", "unit", "base", 
                                        "2025-07-21", "change_perc_07_07", "change_lv_07_07",
                                        "2025-07-22", "change_perc_07_08", "change_lv_07_08",
                                        "2025-07-23", "change_perc_07_09", "change_lv_07_09",
                                        "2025-07-24", "change_perc_07_10", "change_lv_07_10",
                                        "2025-07-25", "change_perc_07_11", "change_lv_07_11"), skip = 3) %>% 
  select(1:2, contains("2025")) %>% pivot_longer(3:7, names_to = "date", values_to = "price") %>% 
  mutate(unit = str_remove(unit, ","), date = ymd(date))

stock_market3 <- read_csv("https://data.egov.bg/resource/download/c49e482a-bb0c-4ebe-aaba-19a02cab78fc/csv",
                          col_names = c("product", "unit", "base", 
                                        "2025-08-04", "change_perc_07_07", "change_lv_07_07",
                                        "2025-08-05", "change_perc_07_08", "change_lv_07_08",
                                        "2025-08-06", "change_perc_07_09", "change_lv_07_09",
                                        "2025-08-07", "change_perc_07_10", "change_lv_07_10",
                                        "2025-08-08", "change_perc_07_11", "change_lv_07_11"), skip = 3) %>% 
  select(1:2, contains("2025")) %>% slice(1:32) %>% 
  pivot_longer(3:7, names_to = "date", values_to = "price") %>% 
  mutate(unit = str_remove(unit, ","), date = ymd(date), price = as.numeric(price))



df <- read_parquet("shiny/bgprices/df_market.parquet")

df <- bind_rows(df, stock_market3)

write_parquet(df, "shiny/bgprices/df_market.parquet")

glimpse(stock_market3)

df %>% 
  filter(date %in% c("2025-06-23", "2025-07-11")) %>%
  summarise(price_change = (last(price, na_rm = T) - first(price, na_rm = T)) / first(price, na_rm = T), 
            .by = c(unit, product)) %>% 
  filter(price_change != 0) %>%
  mutate(product = fct_reorder(product, price_change)) %>% 
  ggplot(aes(price_change, product, fill = price_change > 0)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = paste0(round(price_change * 100, 2), "%")), hjust = -0.03) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.1))) +
  theme(text = element_text(size = 16), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  labs(x = NULL, y = NULL)

df %>% 
  filter(product == 'Кашкавал "Витоша"') %>% 
  ggplot(aes(date, price)) +
  geom_line(linetype = 2, linewidth = 0.3) +
  geom_point(size = 2) +
  theme(text = element_text(size = 16)) +
  labs(x = "Дата", y = "Цена (лв)")

write_parquet(df, "shiny/stock_market/df.parquet")






