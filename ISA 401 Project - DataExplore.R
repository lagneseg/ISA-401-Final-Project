library(DataExplorer)
library(psych)

final_dataset <- read.csv("M:/ISA 401/data/final_project.csv")
final_dataset$STATE <- as.factor(final_dataset$STATE)
final_dataset$Year <- as.factor(final_dataset$Year)
create_report(final_dataset)

descriptive_stats <- describe(final_dataset[, c("Median_Income", "Unemployment_Rate", "Education_Percent", "Turnout_Rate")])
descriptive_stats
