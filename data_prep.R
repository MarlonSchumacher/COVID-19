
# Packages ----
library(magrittr)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gghighlight)
library(patchwork)

# Raw Data ----
df_confirmed <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
                         stringsAsFactors = F)
df_death <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv",
                     stringsAsFactors = F)
df_recover <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv",
                       stringsAsFactors = F)

list_df <- list(df_confirmed = df_confirmed,
                df_death = df_death,
                df_recover = df_recover)

# Setup ----
start <- 5
end <- ncol(df_confirmed)
relevant_countries <- c("Germany", "US", "Italy", "France", "Spain", "United Kingdom")

get_type <- function(x){
  if(stringr::str_detect(x, "confirmed")){
    return("Confirmed")
  }
  if(stringr::str_detect(x, "death")){
    return("Death")
  }
  if(stringr::str_detect(x, "recover")){
    return("Recovered")
  }
}

na_handler <- function(x){
  if(is.infinite(x)|is.na(x)){
    return(NaN)
  } else{
    return(x)
    }
}



# Data Preparation ----
prepared_list_df <- purrr::map(.x = list_df,
                               .f = ~tidyr::gather(.x, key = "date", value = "cases", start:end) %>% 
                                 dplyr::mutate(date = stringr::str_remove_all(date, "X"),
                                               date = stringr::str_replace_all(date, "[.]", "-"),
                                               date = lubridate::mdy(date)) %>% 
                                 dplyr::filter(Country.Region %in% relevant_countries & date >= lubridate::ymd("2020-01-01")) %>% 
                                 dplyr::select(Country.Region, date, cases))

prepared_list_df <- purrr::map2(.x = prepared_list_df,
                                .y = names(prepared_list_df),
                                .f = ~dplyr::mutate(.x, type = get_type(.y)))


df_prepared <- dplyr::bind_rows(prepared_list_df) %>% 
  dplyr::group_by(date, Country.Region, type) %>% 
  dplyr::summarise(cases = sum(cases, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(Country.Region, type) %>% 
  dplyr::arrange(date) %>% 
  dplyr::mutate(rate = (cases-lag(cases))/lag(cases),
                rate = round(rate, 2),
                rate_ma_5 = (lag(rate, 4) + lag(rate, 3) + lag(rate, 2) + lag(rate, 1) + rate)/5,
                cases_ma_5 = (lag(cases, 4) + lag(cases, 3) + lag(cases, 2) + lag(cases, 1) + cases)/5,
                rate_string = paste0(as.character(rate), "%"),
                rate_string = ifelse(stringr::str_detect(rate_string, "NaN"), "", rate_string)) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(country = Country.Region) 


plot1 <- df_prepared %>% 
  dplyr::filter(type == "Confirmed" & date >= lubridate::ymd("2020-03-01")) %>% 
  ggplot(aes(x = date, y = rate_ma_5, color = country)) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("Germany" =  "#D55E00",
                                "US" = "#E69F00", 
                                "Italy" = "#56B4E9",
                                "France" = "#009E73",
                                "Spain" = "grey50",
                                "United Kingdom" = "#0072B2")) +
  gghighlight(use_direct_label = FALSE)+ 
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, color = NULL, title = "Covid-19: Change Of Confirmed Cases",
       subtitle = "Moving Avegerage (5)") +
  facet_wrap(~country, scale = "free", ncol = 3)

max_date <- df_prepared$date %>% max()

df_prepared_spread <- df_prepared %>% 
  dplyr::filter(date == max_date) %>%
  dplyr::select(country, type, cases) %>% 
  tidyr::spread(key = "type", value = "cases")

df_prepared_scenario <- purrr::map(.x = c(0.0035, 0.005, 0.01, 0.015, 0.02, 0),
                                   .f = ~df_prepared_spread %>% 
                                     dplyr::mutate(death_rate = Death/Confirmed,
                                                   dark_cases = Death/ifelse(.x == 0, death_rate, .x),
                                                   assumption = .x,
                                                   ass_string = paste0(
                                                     as.character(assumption*100),
                                                     "%"),
                                                   ass_string = ifelse(ass_string == "0%", "Official", ass_string))
) %>% 
  dplyr::bind_rows()

df_prepared_scenario$ass_string <- factor(df_prepared_scenario$ass_string, 
                                          levels = c("0.35%", "0.5%", "1%", "1.5%", "2%", "Official"))

plot2 <- df_prepared_scenario %>% 
  ggplot(aes(x = country, y = dark_cases, fill = ass_string, label = dark_cases)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("0.35%" =  "#D55E00",
                               "0.5%" = "#E69F00", 
                               "1%" = "#56B4E9",
                               "1.5%" = "#009E73",
                               "2%" = "grey50",
                               "Official" = "#0072B2")) +
  theme_minimal() +
  scale_y_continuous(labels = function(x) format(x, scientific = F)) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, fill = NULL, title = "Covid-19: Estimation Of Actual Cases",
       subtitle = "Based On Different Fatality Rates",
       caption = "Source: Johns Hopkins CSSE") +
  facet_wrap(~country, scale = "free", ncol = 3)

p <- plot1/plot2 +plot_annotation(tag_levels = 'A')


ggsave("Test.png", width = 25, height = 30, units = "cm")


df_prepared %>% 
  dplyr::filter(type == "Confirmed" & date >= lubridate::ymd("2020-03-01")) %>% 
  ggplot(aes(x = date, y = cases, color = country)) +
  geom_line() +
  theme_minimal() +
  scale_y_log10() +
  scale_color_manual(values = c("Germany" =  "#D55E00",
                                "US" = "#E69F00", 
                                "Italy" = "#56B4E9",
                                "France" = "#009E73",
                                "Spain" = "grey50",
                                "United Kingdom" = "#0072B2")) +
  gghighlight(use_direct_label = FALSE)+ 
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, color = NULL, title = "Covid-19: Change Of Confirmed Cases",
       subtitle = "Moving Avegerage (5)") +
  facet_wrap(~country, scale = "free", ncol = 3)




df_prepared %>% 
  dplyr::filter(type == "Death" & date >= lubridate::ymd("2020-03-01")) %>% 
  ggplot(aes(x = date, y = rate_ma_5, color = country)) +
  geom_line() +
  gghighlight(use_direct_label = FALSE) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("Germany" =  "#D55E00",
                                "US" = "#E69F00", 
                                "Italy" = "#56B4E9",
                                "France" = "#009E73",
                                "Spain" = "grey50",
                                "United Kingdom" = "#0072B2")) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, color = NULL, title = "Change of Death Cases",
       subtitle = "Moving Avegerage (5)") +
  facet_wrap(~country, scale = "free", ncol = 3)


  dplyr::mutate(death_rate = Death/Confirmed) %>% 
  dplyr::mutate(death_rate_a1 = 0.002,
                diff_rate = death_rate - death_rate_a1)
