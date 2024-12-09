library(tidycensus)
library(dplyr)

# Function to get median income
get_median_income_data <- function(year) {
  url <- paste0("https://fred.stlouisfed.org/release/tables?eid=259515&rid=249&eid=259515&od=", year, "-01-01#")
  
  income_data <- url |>
    rvest::read_html() |>
    rvest::html_table(fill = TRUE) |>
    purrr::pluck(1) |>
    dplyr::rename(Index = 1, Name = 2, Income = 3, PrecedingPeriod = 4, YearAgo = 5) |>
    dplyr::filter(!is.na(Name) & Name != "" & Name != "Name" & Name != "The United States") |>
    dplyr::transmute(
      STATE = Name,
      Median_Income = stringr::str_replace_all(Income, ",", "") |> as.numeric(),
      Year = as.character(year)
    )
  
  return(income_data)
}

# Pull the data for 2024, 2020, 2016, 2012, and 2008
state_income_2024 <- get_median_income_data(2024)
state_income_2020 <- get_median_income_data(2020)
state_income_2016 <- get_median_income_data(2016)
state_income_2012 <- get_median_income_data(2012)
state_income_2008 <- get_median_income_data(2008)

# Combine into one dataset
combined_income_data <- dplyr::bind_rows(state_income_2024, state_income_2020, state_income_2016, state_income_2012, state_income_2008)
wide_income_data <- combined_income_data |>
  dplyr::select(STATE, Median_Income, Year) |>
  tidyr::pivot_wider(
    names_from = Year, 
    values_from = Median_Income,
    names_glue = "Median_Income_{Year}"
  )

# Function to  get unemployment data
get_unemployment_data <- function(year) {
  url <- paste0("https://fred.stlouisfed.org/release/tables?eid=840687&rid=116&eid=840687&od=", year, "-01-01#")
  
  unemployment_data <- url |>
    rvest::read_html() |>
    rvest::html_table(fill = TRUE) |>
    purrr::pluck(1) |>
    dplyr::rename(Index = 1, Name = 2, Unemployment_Rate = 3, PrecedingPeriod = 4, YearAgo = 5) |>
    dplyr::filter(!is.na(Name) & Name != "" & Name != "Name" & Name != "The United States") |>
    dplyr::transmute(
      STATE = Name,
      Unemployment_Rate = stringr::str_replace_all(Unemployment_Rate, ",", "") |> as.numeric(),
      Year = as.character(year)
    )
  
  return(unemployment_data)
}

# Pull the data for 2024, 2020, 2016, 2012, and 2008
state_unemployment_2024 <- get_unemployment_data(2024)
state_unemployment_2020 <- get_unemployment_data(2020)
state_unemployment_2016 <- get_unemployment_data(2016)
state_unemployment_2012 <- get_unemployment_data(2012)
state_unemployment_2008 <- get_unemployment_data(2008)

# Combine into one dataset
combined_unemployment_data <- dplyr::bind_rows(state_unemployment_2024, state_unemployment_2020, state_unemployment_2016, state_unemployment_2012, state_unemployment_2008)

wide_unemployment_data <- combined_unemployment_data |>
  dplyr::select(STATE, Unemployment_Rate, Year) |>
  tidyr::pivot_wider(
    names_from = Year, 
    values_from = Unemployment_Rate,
    names_glue = "Unemployment_Rate_{Year}"
  )

# Pull education data using API
api_key="c4339645f76126c40cef741688a743274902d024"
education_data_2024 <- tidycensus::get_acs(
  geography = "state",
  variables = c("DP02_0068PE"),
  year = 2023,
  survey = "acs1" ,
  key = api_key
)
education_data_2024 <- education_data_2024 |>
  dplyr::select(STATE = NAME, Education_Percent_2024 = estimate)

education_data_2020 <- tidycensus::get_acs(
  geography = "state",
  variables = c("DP02_0068PE"),
  year = 2020,
  survey = "acs5" ,
  key = api_key
)
education_data_2020 <- education_data_2020 |>
  dplyr::select(STATE = NAME, Education_Percent_2020 = estimate)

education_data_2016 <- tidycensus::get_acs(
  geography = "state",
  variables = c("DP02_0067PE"),
  year = 2016,
  survey = "acs5" ,
  key = api_key
)
education_data_2016 <- education_data_2016 |>
  dplyr::select(STATE = NAME, Education_Percent_2016 = estimate)

education_data_2012 <- tidycensus::get_acs(
  geography = "state",
  variables = c("DP02_0067PE"),
  year = 2012,
  survey = "acs5" ,
  key = api_key
)
education_data_2012 <- education_data_2012 |>
  dplyr::select(STATE = NAME, Education_Percent_2012 = estimate)

education_data_2008 <- tidycensus::get_acs(
  geography = "state",
  variables = c("DP02_0067E"),
  year = 2008,
  survey = "acs1" ,
  key = api_key
)
education_data_2008 <- education_data_2008 |>
  dplyr::select(STATE = NAME, Education_Percent_2008 = estimate)

# Pull voter data from csv files
voter_data_2024 <- readr::read_csv("M:/ISA 401/data/Turnout_2024G_v0.3.csv") |>
  dplyr::select(STATE = STATE, Turnout_Rate_2024 = VEP_TURNOUT_RATE)
voter_data_old <- readr::read_csv("M:/ISA 401/data/Turnout_1980_2022_v1.2.csv") |>
  dplyr::filter(YEAR %in% c(2020, 2016, 2012, 2008)) |>
  dplyr::select(STATE = STATE, YEAR, VEP_TURNOUT_RATE) |>
  dplyr::mutate(STATE = stringr::str_remove_all(STATE, "\\*")) 
# Reshape data
voter_data_wide <- voter_data_old |>
  tidyr::pivot_wider(
    names_from = YEAR, 
    values_from = VEP_TURNOUT_RATE, 
    names_prefix = "Turnout_Rate_"
  )


# Create full dataset combining all variables
state_data <- wide_income_data |>
  dplyr::inner_join(voter_data_2024, by = "STATE") |>
  dplyr::inner_join(voter_data_wide, by = "STATE") |>
  dplyr::inner_join(wide_unemployment_data, by = "STATE") |>
  dplyr::inner_join(education_data_2024, by = "STATE") |>
  dplyr::inner_join(education_data_2020, by = "STATE") |>
  dplyr::inner_join(education_data_2016, by = "STATE") |>
  dplyr::inner_join(education_data_2012, by = "STATE") |>
  dplyr::inner_join(education_data_2008, by = "STATE") |>
  dplyr::select(STATE, Median_Income_2024, Unemployment_Rate_2024, Education_Percent_2024, Turnout_Rate_2024,
                Median_Income_2020, Unemployment_Rate_2020, Education_Percent_2020, Turnout_Rate_2020,
                Median_Income_2016, Unemployment_Rate_2016, Education_Percent_2016, Turnout_Rate_2016,
                Median_Income_2012, Unemployment_Rate_2012, Education_Percent_2012, Turnout_Rate_2012,
                Median_Income_2008, Unemployment_Rate_2008, Education_Percent_2008, Turnout_Rate_2008) |>
  dplyr::filter(!is.na(Turnout_Rate_2008))|>
  dplyr::mutate(
    Turnout_Rate_2024 = stringr::str_remove(Turnout_Rate_2024, "%") |> as.numeric() / 100,
    Unemployment_Rate_2024 = Unemployment_Rate_2024 / 100,
    Education_Percent_2024 = Education_Percent_2024/100,
    Turnout_Rate_2020 = stringr::str_remove(Turnout_Rate_2020, "%") |> as.numeric() / 100,
    Unemployment_Rate_2020 = Unemployment_Rate_2020 / 100,
    Education_Percent_2020 = Education_Percent_2020/100,
    Turnout_Rate_2016 = stringr::str_remove(Turnout_Rate_2016, "%") |> as.numeric() / 100,
    Unemployment_Rate_2016 = Unemployment_Rate_2016 / 100,
    Education_Percent_2016 = Education_Percent_2016/100,
    Turnout_Rate_2012 = stringr::str_remove(Turnout_Rate_2012, "%") |> as.numeric() / 100,
    Unemployment_Rate_2012 = Unemployment_Rate_2012 / 100,
    Education_Percent_2012 = Education_Percent_2012/100,
    Turnout_Rate_2008 = stringr::str_remove(Turnout_Rate_2008, "%") |> as.numeric() / 100,
    Unemployment_Rate_2008 = Unemployment_Rate_2008 / 100,
    Education_Percent_2008 = Education_Percent_2008/100
  )

# Create long version of dataset for analysis
state_data_long <- state_data |>
  tidyr::pivot_longer(
    cols = starts_with("Median_Income_") | starts_with("Unemployment_Rate_") | starts_with("Education_Percent_") | starts_with("Turnout_Rate_"),
    names_to = c(".value", "Year"),
    names_pattern = "(.*)_(\\d{4})"
  ) |>
  dplyr::mutate(
    Year = as.integer(Year)
  )

readr::write_csv(x = state_data_long, file = "M:/ISA 401/data/final_project.csv")


