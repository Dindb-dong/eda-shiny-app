# =============================================================================
# app.R — COVID-19 and Consumer Spending: South Korea vs Taiwan (2021–2022)
# 실행: RStudio에서 이 파일을 열고 오른쪽 위 "Run App" 클릭
# 각 탭의 코드는 R/ 폴더에 따로 있다. (탭 1: R/tab_covid.R)
# =============================================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

source("R/covid_data.R")
source("R/tab_covid.R")

covid_daily <- load_covid_daily("data/covid_daily.csv")

ui <- navbarPage(
  title = "COVID-19 & Card Spending: South Korea vs Taiwan (2021–2022)",

  tabPanel("01 · COVID-19 Timeline", covidTabUI("covid")),

  # ---- 팀원 담당: R/tab_spending.R 만들고 아래 줄 교체. 데이터: data/common_panel.csv ----
  tabPanel("02 · Card Spending",
           h3("Coming soon"), p("Card spending comparison (teammate).")),

  # ---- 결론 패널: make_covid_monthly(covid_daily) 를 common_panel.csv 와 월별 join ----
  tabPanel("03 · COVID-19 × Spending",
           h3("Coming soon"), p("Joint analysis and conclusions."))
)

server <- function(input, output, session) {
  covidTabServer("covid", covid_daily)
}

shinyApp(ui, server)
