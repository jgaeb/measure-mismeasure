options(tidyverse.quiet = TRUE)

library(groundhog)
groundhog.library(tidyverse, "2023-08-01")

# Set seed
set.seed(31371648)

#################################### LOAD ######################################
# NOTE: Thanks to Madison Coots for the data loading code.

print("Loading data...")

# 2011-2012
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DEMO_G.XPT",
              demo <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DIQ_G.XPT",
              diq <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/BMX_G.XPT",
              bmx <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/GHB_G.XPT",
              ghb <- tempfile(), mode="wb", quiet = TRUE)
raw_demographics_11_12 <- foreign::read.xport(demo) %>% 
  janitor::clean_names()
raw_survey_responses_11_12 <- foreign::read.xport(diq) %>% 
  janitor::clean_names()
raw_body_measurements_11_12 <- foreign::read.xport(bmx) %>% 
  janitor::clean_names()
raw_glycohemoglobin_11_12 <- foreign::read.xport(ghb) %>% 
  janitor::clean_names()

# 2013-2014
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DEMO_H.XPT",
              demo <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DIQ_H.XPT",
              diq <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BMX_H.XPT",
              bmx <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/GHB_H.XPT",
              ghb <- tempfile(), mode="wb", quiet = TRUE)
raw_demographics_13_14 <- foreign::read.xport(demo) %>% 
  janitor::clean_names()
raw_survey_responses_13_14 <- foreign::read.xport(diq) %>% 
  janitor::clean_names()
raw_body_measurements_13_14 <- foreign::read.xport(bmx) %>% 
  janitor::clean_names()
raw_glycohemoglobin_13_14 <- foreign::read.xport(ghb) %>% 
  janitor::clean_names()

# 2015-2016
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.XPT",
              demo <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DIQ_I.XPT",
              diq <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/BMX_I.XPT",
              bmx <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/GHB_I.XPT",
              ghb <- tempfile(), mode="wb", quiet = TRUE)
raw_demographics_15_16 <- foreign::read.xport(demo) %>% 
  janitor::clean_names()
raw_survey_responses_15_16 <- foreign::read.xport(diq) %>% 
  janitor::clean_names()
raw_body_measurements_15_16 <- foreign::read.xport(bmx) %>% 
  janitor::clean_names()
raw_glycohemoglobin_15_16 <- foreign::read.xport(ghb) %>% 
  janitor::clean_names()

# 2017-2018
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DEMO_J.XPT",
              demo <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DIQ_J.XPT",
              diq <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/BMX_J.XPT",
              bmx <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/GHB_J.XPT",
              ghb <- tempfile(), mode="wb", quiet = TRUE)
raw_demographics_17_18 <- foreign::read.xport(demo) %>% 
  janitor::clean_names()
raw_survey_responses_17_18 <- foreign::read.xport(diq) %>% 
  janitor::clean_names()
raw_body_measurements_17_18 <- foreign::read.xport(bmx) %>% 
  janitor::clean_names()
raw_glycohemoglobin_17_18 <- foreign::read.xport(ghb) %>% 
  janitor::clean_names()

# Demographics data
raw_demographics_all <- bind_rows(
    raw_demographics_11_12,
    raw_demographics_13_14,
    raw_demographics_15_16,
    raw_demographics_17_18
  ) %>%
  as_tibble()

# Survey data
raw_survey_responses_all <- bind_rows(
    raw_survey_responses_11_12,
    raw_survey_responses_13_14,
    raw_survey_responses_15_16,
    raw_survey_responses_17_18
  ) %>%
  as_tibble()

# Body measurements data
raw_body_measurements_all <- bind_rows(
    raw_body_measurements_11_12,
    raw_body_measurements_13_14,
    raw_body_measurements_15_16,
    raw_body_measurements_17_18
  ) %>%
  as_tibble()

# Glycohemoglobin data
raw_glycohemoglobin_all <- bind_rows(
    raw_glycohemoglobin_11_12,
    raw_glycohemoglobin_13_14,
    raw_glycohemoglobin_15_16,
    raw_glycohemoglobin_17_18
  ) %>%
  as_tibble()

# Join into one dataset and add outcome label
df <- raw_demographics_all %>%
  full_join(raw_survey_responses_all, by = "seqn") %>%
  full_join(raw_body_measurements_all, by = "seqn") %>%
  full_join(raw_glycohemoglobin_all, by = "seqn") %>%
  mutate(
    lbxgh = as.numeric(as.character((lbxgh))),
    diq010 = as.numeric(as.character((diq010))),
    a1c = cut(lbxgh,breaks=c(0,5.7,6.5,1000),right=FALSE),
    diabetes_diagnosis = case_when(
      diq010 %in% 1 ~ 1,
      diq010 %in% c(2,3,9) ~ 0,
      diq010 %in% 7 ~ as.numeric(NA),
    ),
    diabetes = diabetes_diagnosis,
    diabetes = if_else(a1c=="[6.5,1e+03)" &!is.na(a1c), 1, diabetes),
    diabetes = as.integer(diabetes),
    diabetes = if_else(diabetes == 1, TRUE, FALSE),
    # Normalize weights
    weights = wtmec2yr / sum(wtmec2yr),
    # Normalize race
    race = case_when(
      ridreth3 == 1 ~ "Hispanic",
      ridreth3 == 2 ~ "Hispanic",
      ridreth3 == 3 ~ "White",
      ridreth3 == 4 ~ "Black",
      ridreth3 == 6 ~ "Asian",
      ridreth3 == 7 ~ "Other"
    )
  )

# Simplify the dataframe
df_simp <- df %>%
  select(diabetes, age = ridageyr, bmi = bmxbmi, race = race, weights) %>%
  filter(!is.na(diabetes), !is.na(bmi))

#################################### TRAIN #####################################

# Fit logistic regression
m_full <- df_simp %>%
  glm(diabetes ~ age + bmi + race, data = ., family = binomial(),
      weights = weights)

risk <- df_simp %>%
  predict(m_full, newdata = ., type = "response")

df_simp$risk <- risk

################################### ANALYZE ####################################
  
# Calculate false-positive rates
fpr <- df_simp %>%
  filter(!diabetes) %>%
  group_by(race) %>%
  mutate(weights = weights / sum(weights)) %>%
  summarize(fpr = weighted.mean(risk > 0.015, weights), .groups = "drop")

# Calculate means
means <- df_simp %>%
  group_by(race) %>%
  summarize(mean = weighted.mean(risk, weights), .groups = "drop") %>%
  mutate(density_type = "Distribution of risk")

# Calculate proportion of positive decisions
print("Proportion of positive decisions")
df_simp %>%
  group_by(race) %>%
  summarize(prop_above = weighted.mean(risk > 0.015, weights), .groups = "drop")

# Find a threshold that satisfies demographic parity
print("Proportion of positive decisions at threshold")
race_specific_thresholds <- tribble(
  ~race,    ~threshold,
  "Asian",  0.022,
  "White",  0.01
)
print(race_specific_thresholds)
df_simp %>%
  right_join(race_specific_thresholds, by = "race") %>%
  group_by(race) %>%
  summarize(prop = weighted.mean(risk > threshold, weights), .groups = "drop")

# Plot the risk distributions
p_density <- df_simp %>%
  bind_rows(df_simp) %>%
  mutate(
    density_type = c(rep("U", nrow(df_simp)), rep("C", nrow(df_simp))),
    race = fct_relevel(race, "White", "Asian")
  ) %>%
  filter(
    race %in% c("Asian", "White"),
    density_type == "U" | density_type == "C" & !diabetes
  ) %>%
  mutate(density_type = fct_recode(
    density_type,
    `Distribution of risk` = "U",
    `Conditional distribution of risk` = "C"
  )) %>%
  group_by(race) %>%
  mutate(weights = weights / sum(weights)) %>%
  ggplot(aes(x = risk, color = race)) +
  geom_density(aes(weight = weights), bw = 0.01) +
  labs(
    x = "Probability of having diabetes",
    y = "Density",
    color = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(
    label = scales::label_percent(),
    limits = c(0, 0.2),
    expand = c(0,0)
  ) +
  scale_y_continuous(label = NULL, expand = c(0,1)) +
  geom_vline(
    aes(xintercept = mean, color = race),
    data = filter(means, race %in% c("Asian", "White")),
    linetype = "dashed",
    show.legend = FALSE
  ) +
  geom_vline(xintercept = 0.015, show.legend = FALSE) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 9),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(10, 10, 10, 10, "pt")
  ) +
  facet_wrap(vars(fct_rev(density_type)))

suppressWarnings(ggsave(
  "figures/density.pdf",
  plot = p_density,
  width = 5,
  height = 2.5
))

# Fit logistic regression (but without race)
m_red <- df_simp %>%
  glm(diabetes ~ age + bmi, data = ., family = binomial(), weights = weights)

risk_red <- df_simp %>%
  predict(m_red, newdata = ., type = "response")

df_simp$risk_red <- risk_red

# Calculate false-positive rates
print("False positive rates")
df_simp %>%
  filter(!diabetes) %>%
  group_by(race) %>%
  summarize(fpr = mean(risk_red > 0.01), .groups = "drop")

# Compare risk from full and reduced model
p_diabetes <- df_simp %>%
  filter(race %in% c("Asian", "White")) %>%
  group_by(race) %>%
  mutate(
    weights = weights / sum(weights),
    risk_red = round(2 * risk_red, digits = 2) / 2
  ) %>%
  group_by(race, risk_red) %>%
  summarize(est = weighted.mean(risk, weights), .groups = "drop") %>%
  ggplot(aes(x = risk_red, y = est, color = race)) +
  geom_point(show.legend = FALSE) +
  geom_line() +
  geom_vline(xintercept = 0.015) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(label = scales::label_percent()) +
  scale_y_continuous(label = scales::label_percent()) +
  coord_fixed(xlim = c(0, 0.05), ylim = c(0, 0.05), expand = FALSE) +
  labs(
    color = NULL,
    x = "Race-blind risk score",
    y = "Diabetes rate"
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.99, 1.05),
    legend.justification = c(1, 1),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 9),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(10, 10, 10, 10, "pt"),
    legend.background = element_rect(fill = alpha("white", 0))
  )
    
ggsave("figures/diabetes.pdf", plot = p_diabetes, height = 2.5, width = 2.5)

# Show how calibration could be maliciously manipulated
df_asian <- df_simp %>%
  filter(race == "Asian") %>%
  mutate(state = "Original")

df_noise <- df_asian %>%
  mutate(
    bmi = bmi + rnorm(nrow(df_asian), 0, 10),
    age = age + rnorm(nrow(df_asian), 0, 20),
    state = "Noised"
  )

m_noise <- glm(diabetes ~ bmi + age, data = df_noise, family = binomial,
               weights = weights)

df_noise$risk <- predict(m_noise, type = "response")

# Calculate the percentage screened
print("Percentage screened under 'red-lined' model")
with(df_asian, glue::glue("Asian: {weighted.mean(risk > 0.015, weights)}"))
with(df_noise, glue::glue("White: {weighted.mean(risk > 0.015, weights)}"))

# Plot risk distributions
p_calibration_risk <- bind_rows(df_asian, df_noise) %>%
  mutate(state = fct_relevel(state, "Original")) %>%
  ggplot(aes(x = risk, fill = state, weight = weights)) +
  geom_histogram(
    binwidth = 0.005,
    center = 0.0025,
    position = "identity",
    alpha = 1/2
  ) +
  labs(
    x = "Risk score",
    y = "Num. patients",
    fill = NULL
  ) +
  scale_fill_manual(values = scales::brewer_pal(palette = "Set1")(4)[3:4]) +
  scale_x_continuous(
    label = scales::label_percent(),
    limits = c(0, 0.2),
    expand = c(0,0)
  ) +
  geom_vline(
    xintercept = with(means, mean[race == "Asian"]),
    linetype = "dashed"
  ) +
  geom_vline(xintercept = 0.015, show.legend = FALSE) +
  theme_bw() +
  theme(
    legend.position = c(0.075, 0.99),
    legend.justification = c(0, 1),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(10, 10, 10, 10, "pt"),
    legend.background = element_rect(fill = alpha("white", 0))
  )

suppressWarnings(ggsave(
  "figures/calibration_risk_dist.pdf",
  plot = p_calibration_risk,
  height = 2.5,
  width = 2.5
))

# Show calibration plot with original white risk scores and asian noised risk
# scores.
p_diabetes_calibration <- df_simp %>%
  filter(race == "White") %>%
  bind_rows(select(df_noise, -state)) %>%
  ggplot(aes(x = risk, y = diabetes + 0.0, color = race, weight = weights)) +
  geom_smooth(method = "lm", linewidth = 1/2, formula = y ~ x) +
  coord_cartesian(xlim = c(0,1/2), ylim = c(0,1/2), expand = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous("Risk estimate", labels = scales::label_percent()) +
  scale_y_continuous(
    "Diabetes rate",
    labels = scales::label_percent()
  ) +
  labs(color = NULL) +
  theme_bw() +
  theme(
    legend.position = c(0.99, 0.01),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = alpha("white", 0)),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 9),
    plot.margin = margin(10, 10, 10, 10)
  )

suppressWarnings(ggsave(
  "figures/diabetes_calibration.pdf",
  plot = p_diabetes_calibration,
  height = 2.5,
  width = 2.5
))
