library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(here)

# Load data
nba <- read_excel(here("data", "nba_data.xlsx"))

# Clean data
nba_clean <- nba %>%
  mutate(
    Player = str_trim(Player),
    Pos = str_trim(Pos),
    Awards = str_trim(as.character(Awards))
  ) %>%
  filter(
    !is.na(Player), Player != "",
    !is.na(Pos), Pos != ""
  ) %>%
  mutate(
    all_star = if_else(Awards == "AS", "Yes", "No", missing = "No")
  )

# Summary table
summary_table <- nba_clean %>%
  group_by(Pos) %>%
  summarise(
    total_players = n(),
    num_players_with_award = sum(all_star == "Yes", na.rm = TRUE),
    proportion_with_award = num_players_with_award / total_players,
    .groups = "drop"
  ) %>%
  arrange(Pos)

print(summary_table)

# Create output folder
dir.create(here("output", "em_output"), recursive = TRUE, showWarnings = FALSE)

# Save summary table as CSV
write.csv(
  summary_table,
  here("output", "em_output", "table1_summary.csv"),
  row.names = FALSE
)

# Data for bar chart
award_by_position <- nba_clean %>%
  filter(all_star == "Yes") %>%
  count(Pos, name = "num_players_with_award") %>%
  arrange(Pos)

print(award_by_position)

# Create plot object
fig1 <- ggplot(award_by_position, aes(x = Pos, y = num_players_with_award)) +
  geom_col() +
  labs(
    title = "Number of Players by Position Who Won an All-Star Award",
    x = "Position",
    y = "Number of Players"
  ) +
  theme_minimal()

# Show plot in RStudio
print(fig1)

# Save plot
ggsave(
  filename = here("output", "em_output", "Position_AS_barchart.png"),
  plot = fig1,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Chi-square test
chi_table <- table(nba_clean$Pos, nba_clean$all_star)
print(chi_table)
print(dim(chi_table))
chi_result <- chisq.test(chi_table)

print(chi_result)

# Save chi-square test results as CSV
chi_square_output <- data.frame(
  statistic = unname(chi_result$statistic),
  df = unname(chi_result$parameter),
  p_value = chi_result$p.value
)

write.csv(
  chi_square_output,
  here("output", "em_output", "chi_square_results.csv"),
  row.names = FALSE
)

# Save contingency table as CSV in long format
chi_table_output <- as.data.frame(chi_table)
colnames(chi_table_output) <- c("Position", "All_Star", "Count")

write.csv(
  chi_table_output,
  here("output", "em_output", "chi_square_table.csv"),
  row.names = FALSE
)

