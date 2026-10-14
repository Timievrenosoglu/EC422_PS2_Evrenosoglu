library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")


# 4

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"    ~ "Oregon",
      symbol == "CAUR"    ~ "California",
      symbol == "TXUR"    ~ "Texas",
      symbol == "NYUR"    ~ "New York",
    )
  )
# 4.2
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

# 4.3
unemp_top_increases <- unemp |>
  group_by(region) |>
  slice_max(order_by = unemployment_rate_change, n = 5) |>
  ungroup()

unemp_top_decreases <- unemp |>
  group_by(region) |>
  slice_min(order_by = unemployment_rate_change, n = 5) |>
  ungroup()

# 4.4
unemp_top_increases_no_covid <- unemp |>
  group_by(region) |>
  filter(year(date) != 2020) |>
  slice_max(order_by = unemployment_rate_change, n = 5) |>
  ungroup()

unemp_top_decreases_no_covid <- unemp |>
  group_by(region) |>
  filter(year(date) != 2020) |>
  slice_min(order_by = unemployment_rate_change, n = 5) |>
  ungroup()

# 5.1

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +        
  labs(title = "Unemployment Rate Over Time", x = "Date", y = "Unemployment Rate") +
  theme_minimal()

# 5.2 
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +   facet_wrap(~ region)     
  labs(title = "Unemployment Rate Over Time", x = "Date", y = "Unemployment Rate") +
  theme_minimal()
  
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate, fill = region)) +
  geom_col() +        
  labs(title = "Mean Unemployment Rate By Region", x = "Region", y = "Unemployment Rate") +
  theme_minimal()

# 5.3 - Personal Graph Choice
ggplot(unemp_summary, aes(x = fct_reorder(region, max_rate), y = max_rate, fill = region)) +
  geom_col() +        
  labs(title = "Max Unemployment Rate By Region", x = "Region", y = "Unemployment Rate") +
  theme_minimal()

# It is interesting to see that the order of regions differs between the mean and max values. For example, California has the highest mean, but only the second highest max. Also Oregon has the second highest mean, but the 4th largest max.


