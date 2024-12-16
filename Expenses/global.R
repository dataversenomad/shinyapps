# Load libraries
library(dplyr)
library(magrittr)
library(tidyverse)
library(moments)
library(skimr)
library(scales)
library(ggthemes)
library(readxl)
library(lubridate)
library(plotly)
library(knitr)
library(repr)
library(GGally)
library(scales)
library(purrr)
library(gridExtra)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(RColorBrewer)
library(shinyWidgets)
library(timetk)
library(data.table)
library(ggridges)
library(DT)
library(fuzzyjoin)



# Color palette
colfunc <- colorRampPalette(c("#eecac1", "#8b0000"))
pal_ <- colfunc(25)

######## 1. DATA 

# Load datasets
expenses_df <- read_csv("/srv/shiny-server/Expenses/expenses.csv")

expenses_df <- expenses_df %>%
  mutate(
    `TIPO DE MOVMIENTO` = as_factor(`TIPO DE MOVMIENTO`),
    COMERCIO = as_factor(COMERCIO),
    CARD_ = as_factor(CARD),
    LIMIT = as_factor(LIMIT),
    month_name = as_factor(month_name),
    day_name = as_factor(day_name),
    year_ = as_factor(year),
    month_ = as_factor(month),
    week_ = as_factor(week),
    CATEGORY = as_factor(CATEGORY),
    CATEGORY_NEW = as_factor(CATEGORY_NEW),
    CURRENCY_ = as_factor(CURRENCY),
    CATEGORY_GROUP = case_when(
      str_detect(CATEGORY_NEW, "debito_elec") ~ "Controlled",
      str_detect(CATEGORY_NEW, "entertaining") ~ "Controlled",
      str_detect(CATEGORY_NEW, "other") ~ "Controlled",
      str_detect(CATEGORY_NEW, "pets") ~ "Controlled",
      str_detect(CATEGORY_NEW, "restaurant") ~ "Controlled",
      str_detect(CATEGORY_NEW, "groceries") ~ "Controlled",
      str_detect(CATEGORY_NEW, "health") ~ "Health",
      str_detect(CATEGORY_NEW, "fixed") ~ "Fixed",
      str_detect(CATEGORY_NEW, "finan_val") ~ "Finance",
      str_detect(CATEGORY_NEW, "finan_int") ~ "Finance",
      str_detect(CATEGORY_NEW, "pagos") ~ "Pagos",
      str_detect(CATEGORY_NEW, "interes_iva") ~ "Cuotas",
      str_detect(CATEGORY_NEW, "coutas_cobros") ~ "Cuotas",
      str_detect(CATEGORY_NEW, "comissiones") ~ "Cuotas",
      TRUE ~ "CHECK"
    )
  )

visa_pay <- read_csv("/srv/shiny-server/Expenses/payment_days_visa.csv", col_names = FALSE) %>%
  rename(FECHA = X1) %>%
  mutate(
    FECHA = str_replace_all(FECHA, "/", "-"),
    FECHA = dmy(FECHA),
    Period = case_when(
      day(FECHA) <= 15 ~ paste("P1_", month(FECHA), substr(year(FECHA), 3, 4), sep = ""),
      TRUE ~ paste("P2_", month(FECHA), substr(year(FECHA), 3, 4), sep = "")
    ),
    RANK = row_number()
  )

payments_df <- expenses_df %>%
  filter(CATEGORY %in% c("PAGO")) %>%
  select(FECHA, VALUE, SALDO, `TIPO DE MOVMIENTO`, CARD, CURRENCY_, CATEGORY_NEW, CATEGORY_GROUP, year_, month_) %>%
  left_join(visa_pay, by = "FECHA") %>%
  arrange(-desc(FECHA)) %>%
  group_by(FECHA, year_, month_, Period, RANK) %>%
  summarize(VALUE = sum(VALUE), .groups = "drop") %>%
  filter(FECHA >= '2023-09-13') %>%
  fill(Period, RANK, .direction = "down") %>%
  filter(!is.na(FECHA)) %>%
  mutate(
    Type = "Payment",
    CATEGORY_NEW = "Payment",
    CATEGORY_GROUP = "Payment"
  ) %>%
  select(FECHA, year_, month_, Period, RANK, VALUE, CATEGORY_NEW, CATEGORY_GROUP, Type)

debits <- expenses_df %>%
  filter(CATEGORY %in% c("DEBITO")) %>%
  select(FECHA, VALUE, SALDO, `TIPO DE MOVMIENTO`, CARD, CURRENCY_, CATEGORY_NEW, CATEGORY_GROUP, year_, month_) %>%
  arrange(-desc(FECHA))

map_period <- function(date) {
  period_index <- findInterval(date, visa_pay$FECHA)
  if (period_index > 0 && period_index <= nrow(visa_pay)) {
    return(visa_pay$Period[period_index])
  } else {
    return(NA_character_)
  }
}

debits_df <- debits %>%
  mutate(Period = sapply(FECHA, map_period)) %>%
  left_join(visa_pay %>% select(Period, RANK), by = "Period") %>%
  mutate(Type = "Expense") %>%
  filter(RANK >= 1) %>%
  select(FECHA, year_, month_, Period, RANK, VALUE, CATEGORY_NEW, CATEGORY_GROUP, Type) %>%
  arrange(desc(VALUE)) %>%
  mutate(VALUE = VALUE * -1)

expense_tracker <- rbind(debits_df, payments_df)

expense_tr_sum <- expense_tracker %>%
  select(Period, RANK, CATEGORY_NEW, Type, VALUE) %>%
  group_by(Period, RANK, CATEGORY_NEW, Type) %>%
  summarize(VALUE = sum(VALUE), .groups = "drop") %>%
  arrange(-desc(VALUE))

expenses_df <- expenses_df %>%
  mutate(Period = sapply(FECHA, map_period)) %>%
  mutate(Period = if_else(is.na(Period), "Other", Period))

intervals <- c(0, 500, 1000, 3000, Inf)
labels <- c("low", "medium", "high", "ultra high")

expenses_df <- expenses_df %>%
  mutate(VALUE_CAT = cut(VALUE, breaks = intervals, labels = labels, include.lowest = TRUE))

expenses_period_c1 <- expenses_df %>%
  mutate(Period = sapply(FECHA, map_period)) %>%
  left_join(visa_pay %>% select(Period, RANK), by = "Period") %>%
  filter(RANK >= 1) %>%
  group_by(CARD, CURRENCY, Period, RANK) %>%
  summarize(BALANCE_LAST = last(BALANCE), .groups = "drop") %>%
  arrange(-desc(RANK)) %>%
  filter(CARD == '4487-XXXX-XXXX-5978') %>%
  mutate(CARD = "VISA")

custom_order <- c(unique(visa_pay$Period ))
expenses_period_c1$Period <- as.character(expenses_period_c1$Period)
expenses_period_c1$Period <- factor(expenses_period_c1$Period, levels = custom_order)


expenses_period_c2 <- expenses_df %>%
  mutate(Period = sapply(FECHA, map_period)) %>%
  left_join(visa_pay %>% select(Period, RANK), by = "Period") %>%
  filter(RANK >= 1) %>%
  group_by(CARD, CURRENCY, Period, RANK) %>%
  summarize(BALANCE_LAST = last(BALANCE), .groups = "drop") %>%
  arrange(-desc(RANK)) %>%
  filter(CARD == '5524-XXXX-XXXX-1119') %>%
  mutate(CARD = "MASTER")

expenses_period_c2$Period <- as.character(expenses_period_c2$Period)
expenses_period_c2$Period <- factor(expenses_period_c2$Period, levels = custom_order)

# Define utility functions
collect_1 <- function(data, card, curr, ys, mnt, wk) {
  data <- data %>% filter(
    CARD %in% card &
      CURRENCY %in% curr &
      year %in% ys &
      month %in% mnt &
      week %in% wk
  )
  return(data)
}

collect_2 <- function(data, card, curr) {
  data <- data %>% filter(
    CARD %in% card &
      CURRENCY %in% curr
  )
  return(data)
}

collect_3 <- function(period) {
  df <- expense_tr_sum %>% filter(Period == period)
  df <- df[order(df$CATEGORY_NEW != "Payment", decreasing = FALSE), ]
  tot_e <- df %>% filter(Type == "Expense") %>% pull(VALUE) %>% sum()
  tot_p <- df %>% filter(Type == "Payment") %>% pull(VALUE) %>% sum()
  net <- df[nrow(df), ]
  net$VALUE <- tot_p + tot_e
  net$CATEGORY_NEW <- "Net Income"
  net$Type <- "Net"
  df <- rbind(df, net) 
  df <- df %>% mutate(colour = case_when(
    str_detect(Type, "Expense") ~ "darkred",
    str_detect(Type, "Payment") ~ "darkgreen",
    str_detect(Type, "Net") ~ "darkblue",
    TRUE ~ "CHECK"
  ))
  df$CATEGORY_NEW_ <- as.character(df$CATEGORY_NEW)
  custom_order <- c("Payment", unique(df$CATEGORY_NEW_[df$CATEGORY_NEW_ != "Payment"]))
  df$CATEGORY_NEW <- factor(df$CATEGORY_NEW, levels = custom_order)
  df$text <- as.character(format(round(df$VALUE, 0), big.mark = ","))
  df$Factor <- as.numeric(df$CATEGORY_NEW)
  df <- df %>% 
    mutate(measure = case_when(
      str_detect(Type, "Expense") ~ "relative",
      str_detect(Type, "Payment") ~ "relative",
      str_detect(Type, "Net") ~ "total",
      TRUE ~ "CHECK"
    ))
  df$id <- seq_along(df$VALUE)
  df$end <- cumsum(df$VALUE)
  df$start <- c(0, head(df$end, -1))
  colors <- c("Payment" = "deepskyblue3", "Expense" = "darkorange", 
              "Net" = ifelse(df %>% filter(Type == "Net") %>% pull(VALUE) < 0, 
                             "firebrick1", "green3"))
  total_expenses <- df %>% filter(Type == "Expense") %>% pull(VALUE) %>% sum()
  total_expenses <- total_expenses * -1
  p <- ggplot(df, aes(CATEGORY_NEW, fill = Type)) + 
    geom_rect(aes(x = CATEGORY_NEW, xmin = id - 0.45, xmax = id + 0.45, ymin = start, ymax = end), fill = colors[df$Type]) +
    geom_text(aes(x = CATEGORY_NEW, y = 0.5 * (start + end), 
                  label = ifelse(Type == 'Expense',
                                 paste0(as.character(format(round(VALUE, 0), big.mark = ",")),
                                        "(", paste0(round(100 * VALUE / total_expenses, 0),"%"), ")"),
                                 as.character(format(round(VALUE, 0), big.mark = ",")))),
              vjust = 0.5, 
              size = 3,  
              color = "black",  
              fontface = "bold") +
    scale_fill_manual(values = colors) + 
    theme_minimal() +
    labs(x = "", y = "Amount") + 
    theme(legend.position = 'none')
  return(p)
}
