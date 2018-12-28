# install.packages("quantmod")
library(quantmod)
# 
# install.packages("geofacet")
library(geofacet)
# 
# install.packages("hrbrthemes")
library(hrbrthemes)
# 
# install.packages("rvest")
library(rvest)
# 
# install.packages("lucr")
library(lucr)
# 
# install.packages("tidyverse")
library(tidyverse)
# 
# install.packages("anytime")
library(anytime)
# 
# install.packages("tibble")
library(tibble)

get(getSymbols("CPIAUCSL", src='FRED')) %>%
  as.data.frame() %>%
  rownames_to_column("date") %>%
  set_names(c("date", "cpi")) %>%
  as_tibble() -> cpi

rm("CPIAUCSL")

get(getSymbols("CES4200000008", src="FRED")) %>%
  as.data.frame() %>%
  rownames_to_column("date") %>%
  set_names(c("date", "hourly_wage")) %>%
  as_tibble() -> earnings

rm("CES4200000008")

filter(cpi, date == "2017-01-01") %>%
  pull(cpi) -> base_cpi # 2017-01-01

left_join(earnings, cpi) %>%
  mutate(adj = cpi / base_cpi) %>%
  mutate(hourly_wage_adj = hourly_wage / adj) -> earnings_adj_2017

select(earnings_adj_2017, -cpi, -adj) %>%
  gather(measure, value, -date) %>%
  mutate(date = as.Date(date)) %>%
  ggplot(aes(date, value, group=measure)) +
  geom_line(aes(color=measure))

select(earnings_adj_2017, -cpi, -adj) %>%
  mutate(year = substr(date, 1, 4)) %>%
  group_by(year) %>%
  summarise(
    annual_mean_wage = mean(hourly_wage) * 34 * 40, # 34 hours worked per week, 40 weeks worked per year
    annual_mean_wage_adj = mean(hourly_wage_adj) * 34 * 40
  ) -> annual_wage_adj_2017

annual_wage_adj_2017 %>%
  gather(measure, value, -year) %>%
  mutate(date = as.Date(sprintf("%s-01-01", year))) %>%
  ggplot(aes(date, value, group=measure)) +
  geom_line(aes(color=measure))


housing <- read_lines("https://www.census.gov/hhes/www/housing/census/historic/values.html")
housing[
  (which(str_detect(housing, "Median Home Values: Unadjusted"))+7):
    (which(str_detect(housing, "Source:  U.S. Census Bureau"))-3)
  ] %>%
  str_split("\\$|NA", simplify = TRUE) %>%
  trimws() %>%
  gsub(",", "", .) %>%
  as_data_frame() %>%
  mutate_at(as.numeric, .vars=sprintf("V%d",2:8)) %>%
  set_names(c("state", seq(2000, 1940, -10))) %>%
  gather(year, value, -state) -> housing_by_year_unadjusted

mutate(cpi, year = substr(date, 1, 4)) %>%
  group_by(year) %>%
  summarise(mean_cpi = mean(cpi)) -> mean_annual_cpi

filter(mean_annual_cpi, year == "2017") %>%
  pull(mean_cpi) -> base_mean_cpi # 2017

left_join(housing_by_year_unadjusted, mean_annual_cpi) %>%
  mutate(adj = mean_cpi / base_mean_cpi) %>%
  mutate(value_adj = value / adj) -> median_raw_housing_by_decade_and_state

median_raw_housing_by_decade_and_state %>%
  mutate(year = as.Date(sprintf("%s-01-01", year))) %>%
  select(-mean_cpi, -adj) %>%
  gather(measure, value, -state, -year) %>%
  ggplot(aes(year, value, group=measure)) +
  geom_line(aes(color=measure)) +
  scale_x_date(date_labels="%y") +
  scale_y_comma(name="Media Home Price") +
  facet_geo(~state) +
  labs(x=NULL) +
  theme_ipsum_rc(grid="XY", axis_text_size = 6, strip_text_size = 8) +
  theme(panel.spacing=unit(0, "lines"))


median_raw_housing_by_decade_and_state %>%
  left_join(
    annual_wage_adj_2017 %>%
      mutate(year = sprintf("%s0", substr(year, 1, 3))) %>%
      group_by(year) %>%
      summarise(mean_wage_adj = mean(annual_mean_wage_adj, na.rm=TRUE))
  ) %>%
  select(-value, -mean_cpi, -adj) %>%
  gather(measure, value, -state, -year) %>%
  mutate(year = as.Date(sprintf("%s-01-01", year))) %>%
  ggplot(aes(year, value, group=measure)) +
  geom_line(aes(color=measure)) +
  scale_x_date(date_labels="%y") +
  scale_y_comma(name="Media Home Price") +
  geofacet::facet_geo(~state) +
  labs(x=NULL) +
  theme_ipsum_rc(grid="XY", axis_text_size = 6, strip_text_size = 8) +
  theme(panel.spacing=unit(0, "lines"))


read_html("https://college-education.procon.org/view.resource.php?resourceID=005532") %>%
  html_nodes("table") %>%
  .[3] %>%
  html_table(fill=TRUE) %>%
  .[[1]] %>%
  select(year=1, private=4, public=5) %>%
  slice(17:61) %>%
  mutate_at(lucr::from_currency, .vars=c("private", "public")) -> tuition

tuition[38, "private"] <- 24818

left_join(tuition, mean_annual_cpi) %>%
  mutate(adj = mean_cpi / base_mean_cpi) %>%
  mutate(private_adj = private / adj) %>%
  mutate(public_adj = public / adj) %>%
  select(-private, -public, -mean_cpi, -adj) %>%
  gather(measure, value, -year) %>%
  mutate(date = as.Date(sprintf("%s-01-01", year))) %>%
  ggplot(aes(date, value, group=measure)) +
  geom_line(aes(color=measure))

left_join(tuition, mean_annual_cpi) %>%
  left_join(annual_wage_adj_2017) %>%
  filter(!is.na(annual_mean_wage)) %>%
  mutate(adj = mean_cpi / base_mean_cpi) %>%
  mutate(private_adj = private / adj) %>%
  mutate(public_adj = public / adj) %>%
  select(-private, -public, -mean_cpi, -adj, -annual_mean_wage) %>%
  gather(measure, value, -year) %>%
  mutate(date = as.Date(sprintf("%s-01-01", year))) %>%
  ggplot(aes(date, value, group=measure)) +
  geom_line(aes(color=measure))


left_join(tuition, mean_annual_cpi) %>%
  left_join(annual_wage_adj_2017) %>%
  filter(!is.na(annual_mean_wage)) %>%
  mutate(adj = mean_cpi / base_mean_cpi) %>%
  mutate(private_adj = private / adj) %>%
  mutate(public_adj = public / adj) %>%
  select(-private, -public, -mean_cpi, -adj, -annual_mean_wage) %>%
  mutate(date = as.Date(sprintf("%s-01-01", year))) %>%
  ggplot(aes(annual_mean_wage_adj)) +
  geom_path(aes(y=private_adj, color="4yr Private College")) +
  geom_label(aes(y=private_adj, color="4yr Private College", label=year), show.legend = FALSE) +
  geom_path(aes(y=public_adj, color="4yr Public College")) +
  geom_label(aes(y=public_adj, color="4yr Public College", label=year), show.legend = FALSE) +
  scale_x_comma(limits=c(19000,25000), labels=scales::dollar) +
  scale_y_comma(limits=c(0, 40000), labels=scales::dollar) +
  scale_color_ipsum(name=NULL) +
  labs(
    x="Mean Annual Wage (Production and Nonsupervisory Employees: Retail Trade)",
    y="Mean Annual Tutition Cost",
    title="Inflation Adjusted (2017) Earnings vs Tuition: 1979-2015",
    subtitle="How affordable is a year of college if you work in an entry-level retail job?",
    caption="Data: BLS & https://college-education.procon.org/view.resource.php?resourceID=005532"
  ) +
  theme_ipsum(grid="XY") +
  theme(legend.position=c(0.9, 0.85))

gas <- read_csv("https://www.eia.gov/totalenergy/data/browser/csv.php?tbl=T09.04")

filter(gas, Description == "Unleaded Regular Gasoline, U.S. City Average Retail Price") %>%
  select(year = YYYYMM, value = Value) %>%
  mutate(date = anytime::anydate(sprintf("%s01", year))) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value)) %>%
  left_join(mutate(cpi, date=as.Date(date))) %>%
  mutate(adj = cpi / base_cpi) %>%
  mutate(value_adj = value / adj)