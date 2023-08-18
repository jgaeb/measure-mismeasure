options(tidyverse.quiet = TRUE)

library(groundhog)
groundhog.library(progress, "2023-08-01")
groundhog.library(fs, "2023-08-01")
groundhog.library(tidyverse, "2023-08-01")

# Set the ggplot theme
theme_set(theme_bw(base_size = 20))

# Set seed
set.seed(31371648)


# Define the simulation parameters
POP_SIZE_TRAIN = 10000000
POP_SIZE_TEST = 1000000
FRAC_MINORITY = 0.33
DISCRETE = TRUE
E_0 = 1
E_R = -1
E_NOISE = 1
M_0 = -1.0
M_E = 1.0
M_NOISE = 1.0
T_0 = 50
T_M = 4
T_E = 4
T_E_M = 1.0
T_NOISE = 7
Y_REJECT_NOISE_SD = 0.3
Y_REJECT_NOISE_MEAN = -0.5
DIVERSITY_UTILITY = 0.25
FRAC_ADMIT = 0.25

# Load the test data
print("Loading test data...")
df_test <- read_csv(path("data", "test.csv"), show_col_types = FALSE)

############################## PARETO FRONTIER #################################

POP_SIZE_TEST_ADMIT <- FRAC_ADMIT * POP_SIZE_TEST
N <- round(FRAC_ADMIT * POP_SIZE_TEST)  * FRAC_MINORITY * 2.0

MAX_MINORITY_ADMIT <- POP_SIZE_TEST_ADMIT * FRAC_MINORITY * 2.0
pareto_ideal <- NULL
pareto_grid <- seq(from = 1, to = FRAC_ADMIT * POP_SIZE_TEST,
                   by = as.integer(POP_SIZE_TEST_ADMIT / 1000))
# Create a progress bar
pb <- progress_bar$new(
  total = length(pareto_grid),
  format = "Generating Pareto Frontier: [:bar] :percent :eta",
  clear = FALSE
)

for (n_minority in pareto_grid) {
  df_test_minority <- df_test %>%
    arrange(desc(T)) %>%
    filter(R == 1)
  df_test_majority <- df_test %>%
    arrange(desc(T)) %>%
    filter(R == 0)
  agg_ac_index <- with(df_test_minority, sum(Y[1:n_minority])) +
    with(df_test_majority, sum(Y[1:(POP_SIZE_TEST_ADMIT - n_minority)]))
  pareto_ideal <- rbind(pareto_ideal, tibble(n_minority, agg_ac_index))
  pb$tick()
}

max_graduation <- pareto_ideal[which.max(pareto_ideal$agg_ac_index), ]
MAX_GRADUATION <- list(
  "Max Graduation",
  0,
  max_graduation$n_minority,
  max_graduation$agg_ac_index
)

pareto_ideal$cutoff <- pareto_ideal$n_minority >=
  as.double(MAX_GRADUATION[[3]])
lo <- loess(
  agg_ac_index ~ n_minority,
  pareto_ideal,
  span = 0.5
)

pareto_ideal$smooth_y <- predict(lo, pareto_ideal$n_minority, se = FALSE)

#################################### PLOT ######################################

# Counterfactual fairness
print("Plotting counterfactual fairness...")
grid <- read_csv(path("data", "outcomes_grid_cf_fairness.csv"),
                 show_col_types = FALSE)

grid_ <- grid %>%
  group_by(min_minority_admits) %>%
  summarize( 
    y_max = max(max_graduates),
    y_min = min(min_graduates)
  )

p_cff <- ggplot() +
  ggtitle('Counterfactual Fairness / Path-Specific Fairness') +
  geom_line(
    data = filter(pareto_ideal, cutoff),
    mapping = aes(x = n_minority, y = smooth_y),
    linewidth = 1.5,
    color = "violet"
  ) + 
  geom_line(
    data = filter(pareto_ideal, !cutoff), 
    mapping = aes(x = n_minority, y = smooth_y),
    linetype = "dotted",
    linewidth = 1.5,
    color = "violet"
  ) + 
  geom_ribbon(
    data = grid_,
    aes(x = min_minority_admits, ymin = y_min, ymax = y_max),
    color = "#C04BFF50",
    fill = "#C04BFF50",
    linetype = 0.5
  ) +
  geom_point(aes(x = 82250, y = 111000), color = "#C77CFF", size = 2) +
  annotate("text",
    size = 5, 
    lineheight = 0.85, 
    hjust = 1,
    x = 82250 + 40000,
    y = 111000,
    label ='Random'
  ) +
  coord_cartesian(
    xlim = c(0.0, FRAC_ADMIT * POP_SIZE_TEST + 10000),
    ylim = c(90000, 195000),
    expand = FALSE
  ) + 
  scale_x_continuous("Admitted Applicants from Target Group", labels = scales::comma) + 
  scale_y_continuous("Academic Index", labels = scales::comma) +
  theme(
    legend.title = element_blank(),
    plot.margin = margin(0.25, 1, 0.25, 0.5, "cm"), 
    legend.position = "none",
    axis.text = element_text(size = 18), 
    plot.title = element_text(size = 22, hjust = 0.5),
    axis.title = element_text(size = 22),
    panel.grid = element_line(
      color = rgb(235, 235, 235, 100, maxColorValue = 255)
    )
  )

ggsave(
  plot = p_cff,
  filename = path("figures", 'frontier_cf_fairness.pdf'),
  height = 6,
  width = 8
)

# Counterfactual equalized odds
print("Plotting counterfactual equalized odds...")
grid <- read_csv(path("data", "outcomes_grid_cf_eo.csv"),
                 show_col_types = FALSE)

grid_ <- grid %>%
  group_by(min_minority_admits) %>%
  summarize( 
    y_max = max(max_graduates),
    y_min = min(min_graduates)
  )

p_cfeo <- ggplot() +
  ggtitle('Counterfactual Equalized Odds') +
  geom_line(
    data = filter(pareto_ideal, cutoff),
    mapping = aes(x = n_minority, y = smooth_y),
    linewidth = 1.5,
    color = "violet"
  ) + 
  geom_line(
    data = filter(pareto_ideal, !cutoff), 
    mapping = aes(x = n_minority, y = smooth_y),
    linetype = "dotted",
    linewidth = 1.5,
    color = "violet"
  ) + 
  geom_ribbon(
    data = grid_,
    aes(x = min_minority_admits, ymin = y_min, ymax = y_max),
    color = NA,
    fill = "#BED05650",
    linetype = 0.5
  ) +
  coord_cartesian(
    xlim = c(0.0, FRAC_ADMIT * POP_SIZE_TEST + 10000),
    ylim = c(90000, 195000),
    expand = FALSE
  ) + 
  scale_x_continuous("Admitted Applicants from Target Group", labels = scales::comma) + 
  scale_y_continuous("Academic Index", labels = scales::comma) +
  theme(
    legend.title = element_blank(),
    plot.margin = margin(0.25, 1, 0.25, 0.5, "cm"), 
    legend.position = "none",
    axis.text = element_text(size = 18), 
    plot.title = element_text(size = 22, hjust = 0.5),
    axis.title = element_text(size = 22),
    panel.grid = element_line(
      color = rgb(235, 235, 235, 100, maxColorValue = 255)
    )
  )

ggsave(
  plot = p_cfeo,
  filename = path("figures", 'frontier_cf_eo.pdf'),
  height = 6,
  width = 8
)

# Principal fairness
print("Plotting principal fairness...")
grid <- read_csv(path("data", "outcomes_grid_principal_fairness.csv"),
                 show_col_types = FALSE)

grid_ <- grid %>%
  group_by(min_minority_admits) %>%
  summarize( 
    y_max = max(max_graduates),
    y_min = min(min_graduates)
  )

p_pf <- ggplot() +
  ggtitle('Principal Fairness') +
  geom_line(
    data = filter(pareto_ideal, cutoff),
    mapping = aes(x = n_minority, y = smooth_y),
    linewidth = 1.5,
    color = "violet"
  ) + 
  geom_line(
    data = filter(pareto_ideal, !cutoff), 
    mapping = aes(x = n_minority, y = smooth_y),
    linetype = "dotted",
    linewidth = 1.5,
    color = "violet"
  ) + 
  geom_ribbon(
    data = grid_,
    aes(x = min_minority_admits, ymin = y_min, ymax = y_max),
    color = NA,
    fill = "#E9A35950",
    linetype = 0.5
  ) +
  coord_cartesian(
    xlim = c(0.0, FRAC_ADMIT * POP_SIZE_TEST + 10000),
    ylim = c(90000, 195000),
    expand = FALSE
  ) + 
  scale_x_continuous("Admitted Applicants from Target Group", labels = scales::comma) + 
  scale_y_continuous("Academic Index", labels = scales::comma) +
  theme(
    legend.title = element_blank(),
    plot.margin = margin(0.25, 1, 0.25, 0.5, "cm"), 
    legend.position = "none",
    axis.text = element_text(size = 18), 
    plot.title = element_text(size = 22, hjust = 0.5),
    axis.title = element_text(size = 22),
    panel.grid = element_line(
      color = rgb(235, 235, 235, 100, maxColorValue = 255)
    )
  )

ggsave(
  plot = p_pf,
  filename = path("figures", 'frontier_principal_fairness.pdf'),
  height = 6,
  width = 8
)

# Counterfactual predictive parity
print("Plotting counterfactual predictive parity...")
grid <- read_csv(path("data", "outcomes_grid_cf_pred_par.csv"),
                 show_col_types = FALSE)

grid_ <- grid %>%
  group_by(min_minority_admits) %>%
  summarize( 
    y_max = max(max_graduates),
    y_min = min(min_graduates)
  )

p_cfpp <- ggplot() +
  ggtitle('Counterfactual Predictive Parity') +
  geom_line(
    data = filter(pareto_ideal, cutoff),
    mapping = aes(x = n_minority, y = smooth_y),
    linewidth = 1.5,
    color = "violet"
  ) + 
  geom_line(
    data = filter(pareto_ideal, !cutoff), 
    mapping = aes(x = n_minority, y = smooth_y),
    linetype = "dotted",
    linewidth = 1.5,
    color = "violet"
  ) + 
  geom_ribbon(
    data = grid_,
    aes(x = min_minority_admits, ymin = y_min, ymax = y_max),
    color = NA,
    fill = "#69FF4B50",
    linetype = 0.5
  ) +
  coord_cartesian(
    xlim = c(0.0, FRAC_ADMIT * POP_SIZE_TEST + 10000),
    ylim = c(90000, 195000),
    expand = FALSE
  ) + 
  scale_x_continuous("Admitted Applicants from Target Group", labels = scales::comma) + 
  scale_y_continuous("Academic Index", labels = scales::comma) +
  theme(
    legend.title = element_blank(),
    plot.margin = margin(0.25, 1, 0.25, 0.5, "cm"), 
    legend.position = "none",
    axis.text = element_text(size = 18), 
    plot.title = element_text(size = 22, hjust = 0.5),
    axis.title = element_text(size = 22),
    panel.grid = element_line(
      color = rgb(235, 235, 235, 100, maxColorValue = 255)
    )
  )

ggsave(
  plot = p_cfpp,
  filename = path("figures", 'frontier_cf_pred_par.pdf'),
  height = 6,
  width = 8
)
