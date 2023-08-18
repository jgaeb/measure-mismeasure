options(tidyverse.quiet = TRUE)

library(groundhog)
groundhog.library(tidyverse, "2023-08-01")

# Set seed
set.seed(31371648)

data_url <- "https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-two-years.csv"

p <- suppressMessages(read_csv(data_url, show_col_types = FALSE)) %>%
  filter(
    days_b_screening_arrest <= 30,
    days_b_screening_arrest >= -30,
    is_recid != -1,
    c_charge_degree != "O",
    score_text != 'N/A',
  ) %>%
  select(gender = sex, score = decile_score...12, recidivated = is_recid) %>%
  filter(score > 0, recidivated >= 0) %>%
  mutate(
    gender = str_to_title(gender),
    gender = factor(gender, c("Male", "Female"))
  ) %>%
  group_by(gender, score) %>%
  summarize(est = mean(recidivated), .groups = "drop") %>%
  ggplot(aes(
    x = score,
    y = est,
    color = gender
  )) +
  geom_point(show.legend = FALSE) +
  geom_line() +
  geom_vline(xintercept = 7) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(
    name = "Recidivism rate",
    label = scales::label_percent()
  ) +
  scale_x_continuous(
    name = "Gender-blind COMPAS score",
    n.breaks = 10
  ) +
  labs(color = NULL) +
  theme_bw() +
  theme(
    legend.position = c(0.01, 1.05),
    legend.justification = c(0,1),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 9),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(10, 10, 10, 10, "pt"),
    legend.background = element_rect(fill = alpha("white", 0))
  )

ggsave("figures/compas.pdf", plot = p, width = 2.5, height = 2.5)
