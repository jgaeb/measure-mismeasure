options(tidyverse.quiet = TRUE)

library(groundhog)
groundhog.library(sigmoid, "2023-08-01")
groundhog.library(fs, "2023-08-01")
groundhog.library(tidyverse, "2023-08-01")

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

################################ BOOK OF LIFE ##################################

# Define the structural equations
gen_book_of_life <- function(
  pop_size,
  frac_minority,
  discrete,
  e_0,
  e_r, 
  e_noise,
  m_0,
  m_e,
  m_noise,
  t_0,
  t_m,
  t_e,
  t_e_m,
  t_noise,
  y_reject_noise,
  y_reject_noise_mean,
  ...
) {
  
  # Define the structural equations.
  f_E <- function(r_i, noise){
    return(e_0 + e_r*r_i + noise)
  }
  
  f_M <- function(e_i, noise){
    return(m_0 + m_e*e_i + noise)
  }

  f_T <- function(m_i, e_i, noise){
    return(round(t_0 + t_m * m_i + t_e * e_i + t_e_m * e_i * m_i + noise,
                 digits = ifelse(discrete,0,3)))
  }
  
  # Generate the book of life.
  book_of_life <- tibble(
      # Race
      # NOTE: We code "minority" as 1 and "majority" as 0.
      R = as.numeric(as.integer(runif(pop_size) <= frac_minority)),
      
      # Educational opportunities
      E_noise = rnorm(pop_size, mean = 0, sd = e_noise),
      E_minority = f_E(1,noise = E_noise),
      E_majority = f_E(0,noise = E_noise),
      
      # Preparation
      M_noise = rnorm(pop_size, mean = 0, sd = m_noise),
      M_minority = f_M(E_minority, noise = M_noise),
      M_majority = f_M(E_majority, noise = M_noise),

      # Test score
      T_noise = rnorm(pop_size, mean = 0, sd = t_noise),
      T_minority = f_T(M_minority, E_minority, noise = T_noise),
      T_majority = f_T(M_majority, E_majority, noise = T_noise),
      
      T_majority_star = f_T(M_minority, E_majority, noise = T_noise),
      T_minority_star = f_T(M_majority, E_minority, noise = T_noise),

      # Realize potential outcomes of covariates
      E = if_else(R == 1, E_minority, E_majority),
      M = if_else(R == 1, M_minority, M_majority),
      T = if_else(R == 1, T_minority, T_majority),
      
      # Graduation
      Y_unif = runif(pop_size, min=0, max=1),
      Y_reject_noise = rnorm(pop_size, mean = y_reject_noise_mean, sd = y_reject_noise),
      Y = as.integer(sigmoid(M) >= Y_unif),
      Y_raw = sigmoid(M),
      Y_reject = as.integer(sigmoid(M-0.5) >= Y_unif),
      Y_reject_raw = sigmoid(M - 0.5)
  )
}

################################## GENERATE ####################################

df_train <-gen_book_of_life(
  pop_size = POP_SIZE_TRAIN,
  frac_minority = FRAC_MINORITY,
  discrete = DISCRETE,
  e_0 = E_0,
  e_r = E_R,
  e_noise = E_NOISE,
  m_0 = M_0,
  m_e = M_E,
  m_noise = M_NOISE,
  t_0 = T_0,
  t_m = T_M,
  t_e = T_E,
  t_e_m = T_E_M,
  t_noise = T_NOISE,
  y_reject_noise = Y_REJECT_NOISE_SD,
  y_reject_noise_mean = Y_REJECT_NOISE_MEAN
)

df_test <-gen_book_of_life(
  pop_size = POP_SIZE_TEST,
  frac_minority = FRAC_MINORITY,
  discrete = DISCRETE,
  e_0 = E_0,
  e_r = E_R,
  e_noise = E_NOISE,
  m_0 = M_0,
  m_e = M_E,
  m_noise = M_NOISE,
  t_0 = T_0,
  t_m = T_M,
  t_e = T_E,
  t_e_m = T_E_M,
  t_noise = T_NOISE,
  y_reject_noise = Y_REJECT_NOISE_SD,
  y_reject_noise_mean = Y_REJECT_NOISE_MEAN
)

# Write the simulated data to disk
write_csv(df_train, path('data', 'train.csv'))
write_csv(df_test, path('data', 'test.csv'))
