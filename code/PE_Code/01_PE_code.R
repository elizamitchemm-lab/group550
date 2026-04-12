library(readxl)
library(ggplot2)
library(rstatix)
library(dplyr)
library(broom)

here::i_am(
  "code/PE_code/01_PE_code.R"
)

data <- read_excel(here::here("data/nba_data.xlsx"))

#Frequency counts for personal use
table(data$Team, useNA = "ifany")

#ANOVA test
PFbT_anova <- oneway.test(PF ~ Team, 
                          data = data, 
                          var.equal = FALSE)

PFbT_anova_tbl <- data.frame(
  Test = "Welch One-Way ANOVA",
  F_statistic = round(PFbT_anova$statistic, 2),
  df1 = round(PFbT_anova$parameter[1], 2),
  df2 = round(PFbT_anova$parameter[2], 2),
  p_value = signif(PFbT_anova$p.value, 3)
)


saveRDS(PFbT_anova_tbl, 
        here::here("output/PE_output/PFbT_anova_tbl.rds"))


#Summary table - unsure if necessary

PFbT_table <- data %>%
  group_by(Team) %>%
  summarise(
    mean_PF = mean(PF, na.rm = TRUE),
    sd_PF = sd(PF, na.rm = TRUE),
    n = n()
  )

saveRDS(PFbT_table,
        here::here("output/PE_output/PFbT_sum_tbl.rds"))

#Boxplot
PFbT_boxplot <- ggplot(data, 
               aes(x = Team, 
                   y = PF)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 10)) +
  theme(axis.text = element_text(angle = 45, hjust = 1))

saveRDS(PFbT_boxplot,
        here::here("output/PE_output/PFbT_boxplot.rds"))

#Bar Graph
PFbT_bar <- ggplot(PFbT_table, 
                   aes(x = reorder(Team, mean_PF),
                       y = mean_PF)) +
  geom_col() +
  coord_flip()

saveRDS(PFbT_bar,
        here::here("output/PE_output/PFbT_bar.rds"))
