# =============================================================================
# R/covid_data.R — 팀 공식 data/covid_daily.csv 를 앱에서 쓰기 좋게 읽어 오기
# load_covid_daily()   -> 탭 1에서 쓰는 일별 표
# make_covid_monthly() -> 탭 3에서 카드 월별 자료(common_panel.csv)와 합칠 때 쓰는 월별 표
# =============================================================================

library(dplyr)
library(readr)

# 인구 100만 명당 환산용 인구 (OWID population estimates, 2022 기준 근사값)
covid_population <- c("South Korea" = 51815810, "Taiwan" = 23893396)

load_covid_daily <- function(path = "data/covid_daily.csv") {
  read_csv(path, show_col_types = FALSE) |>
    transmute(
      country,
      date        = as.Date(date),
      cum_cases   = jhu_total_confirmed,
      cum_deaths  = jhu_total_deaths,
      new_cases   = jhu_new_confirmed,
      new_deaths  = jhu_new_deaths,
      new_cases_pm  = new_cases  / unname(covid_population[country]) * 1e6,
      new_deaths_pm = new_deaths / unname(covid_population[country]) * 1e6,
      stringency        = owid_stringency_index,
      vax_one           = owid_people_vaccinated_per_hundred,
      vax_full          = owid_people_fully_vaccinated_per_hundred,
      boosters          = owid_total_boosters_per_hundred,
      reproduction_rate = owid_reproduction_rate
    )
}

# 월별 요약: 확진·사망은 월 합계, 방역지수는 월 평균, 접종률은 월말 값
# common_panel.csv 의 month(예: 202101) 형식에 맞춘 month_key 열도 만든다.
# 주의: common_panel.csv 의 country 는 "Korea"/"Taiwan" 이므로 합칠 때 이름을 맞출 것.
make_covid_monthly <- function(covid_daily) {
  last_obs <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) > 0) x[length(x)] else NA_real_
  }
  covid_daily |>
    mutate(month = as.Date(format(date, "%Y-%m-01"))) |>
    group_by(country, month) |>
    summarise(
      n_days          = n(),
      cases           = sum(new_cases, na.rm = TRUE),
      deaths          = sum(new_deaths, na.rm = TRUE),
      cases_pm        = sum(new_cases_pm, na.rm = TRUE),
      deaths_pm       = sum(new_deaths_pm, na.rm = TRUE),
      stringency_mean = mean(stringency, na.rm = TRUE),
      vax_full_end    = last_obs(vax_full),
      boosters_end    = last_obs(boosters),
      .groups = "drop"
    ) |>
    mutate(month_key = as.integer(format(month, "%Y%m")))
}
