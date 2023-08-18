#!/usr/bin/env python
# coding: utf-8
import os
import random
from collections import Counter

import numpy as np
import pandas as pd
from tqdm import tqdm
from ortools.linear_solver import pywraplp


DIVERSITY_UTILITY = 0.25
FRAC_ADMIT = 0.25

#################################### LOAD ######################################
print("Loading data...")

df = pd.read_csv('data/test.csv')
df['key'] = df['R'].astype(str) + "_"  + df['T'].astype(str)

# Estimate E[Y(1)|T, A]
df_train = pd.read_csv('data/train.csv')
df_stratum_utility = (df_train[['R','T','Y']]
                      .groupby(['R','T'])
                      .mean()
                      .reset_index())
df_stratum_utility['utility_grad'] = (df_stratum_utility['Y']).round(2)
df_stratum_utility['stratum_utility'] = (df_stratum_utility['Y'] 
                                         + DIVERSITY_UTILITY 
                                         * df_stratum_utility['R']).round(2)
df_stratum_utility['key'] = df_stratum_utility['R'].astype(str) + "_" \
        + df_stratum_utility['T'].astype(str)

df = df.merge(df_stratum_utility[['stratum_utility','utility_grad','key']], on='key')
df['ml_outcomes'] = df['stratum_utility']


TOTAL_ADMITS = int(len(df) * FRAC_ADMIT)


MAX_MINORITY_ADMITS = TOTAL_ADMITS
MIN_GRADUATES = 75000
GRID_SIZE = 500

outcomes_grid = []
Xs = []
Ys = []

for N_minority_admits in tqdm(range(0, MAX_MINORITY_ADMITS, GRID_SIZE),
                              "Generating Pareto frontier"):
    
    minority_admits = (df[df['R']==1]
                       .sort_values(by='T',ascending=False)
                       .head(n=N_minority_admits))
    majority_admits = (df[df['R']==0]
                       .sort_values(by='T',ascending=False)
                       .head(n=TOTAL_ADMITS - N_minority_admits))
    
    Y = minority_admits['Y'].sum() + majority_admits['Y'].sum()
    
    Xs.append(N_minority_admits)
    Ys.append(Y)
    
    for y_iter in np.arange(MIN_GRADUATES, Y, GRID_SIZE):
        outcomes_grid.append({'min_minority_admits': N_minority_admits
                                                     - GRID_SIZE / 2,
                              'max_minority_admits': N_minority_admits 
                                                     + GRID_SIZE/2,
                              'max_graduates': y_iter + GRID_SIZE,
                              'min_graduates': y_iter,
                              'policy_exists':'Unknown'})

df_pareto = pd.DataFrame({'# Minority Admits': Xs, '# Graduates': Ys})


dff = df[['R','T','ml_outcomes','Y']].groupby(['R','T','Y']).count().reset_index()
dff.columns = ['R','T','Y','N']

################################### SETUP #####################################

solver = pywraplp.Solver.CreateSolver('GLOP')

applicant_stratum = []
vars_cache = {}

# Objective: Maximize the expected utility of the admitted students
objective = solver.Objective()

# For each stratum
for ix, row in dff.iterrows():
    # probability of admission
    numvar = solver.NumVar(0.0, 1.0, str(ix))
    
    # store variable by index, and also by stratum R, T
    applicant_stratum.append(numvar)
    vars_cache[(row['R'],row['T'],row['Y'])] = numvar
    
    # Benefit of admitting people is simply the total number of people admitted
    objective.SetCoefficient(applicant_stratum[ix], float(row['N']))

objective.SetMaximization()

# Constraint: At most K applicants
K = int(len(df) * FRAC_ADMIT)
admit_quota = solver.Constraint(0, K)

# Total applicants cannot exceed K 
for ix, row in dff.iterrows():
    admit_quota.SetCoefficient(applicant_stratum[ix], float(row['N']))
    

# Add the CF equalized odds constraints

# Make sure that you have to add all people in Y stratum or none, i.e., you
# can't add only people who graduate and reject those who don't from same T, R
# stratum
for ix, row in dff.iterrows():
    constrain_bp = solver.Constraint(0.0, 0.0)
    
    var1 = vars_cache[(row['R'],row['T'],row['Y'])]
    key2 = (row['R'],row['T'], 1-row['Y'])
    
    if key2 not in vars_cache:
        continue
        
    var2 = vars_cache[key2]
    
    constrain_bp.SetCoefficient(var1, -1.0)
    constrain_bp.SetCoefficient(var2, 1.0)

majority_grad = []
majority_no_grad = []
minority_grad = []
minority_no_grad = []

for key in vars_cache:
    r, t, Y = key
    if Y == 1 and r == 0:
        majority_grad.append(key)
    elif Y == 0 and r == 0:
        majority_no_grad.append(key)
    elif Y == 1 and r == 1:
        minority_grad.append(key)
    elif Y == 0 and r == 1:
        minority_no_grad.append(key)

NUM_TOTALS = {}
df_totals = dff[['N','R','Y']].groupby(['R','Y']).sum().reset_index()
for ix, row in df_totals.iterrows():
    NUM_TOTALS[(row['R'],row['Y'])] = row['N']
    
N_IN_STRATAS = {}
for ix, row in dff.iterrows():
    N_IN_STRATAS[(row['R'],row['T'],row['Y'])] = row['N']

# Of those who graduate, fraction majority admitted and fraction minority
# admitted should be the same.

constrain_grad = solver.Constraint(0.0, 0.0)

for key in majority_grad:
    r, t, Y = key
    N_IN_STRATUM = N_IN_STRATAS[(r,t,Y)]
    N_TOTAL = NUM_TOTALS[(r,Y)]
    
    constrain_grad.SetCoefficient(vars_cache[key],
                                  float(N_IN_STRATUM) / float(N_TOTAL))

for key in minority_grad:
    r, t, Y = key
    N_IN_STRATUM = N_IN_STRATAS[(r,t,Y)]
    N_TOTAL = NUM_TOTALS[(r,Y)]
    
    constrain_grad.SetCoefficient(
            vars_cache[key], -1.0 * (float(N_IN_STRATUM) / float(N_TOTAL)))

# Of those who do not graduation, fraction majority admitted and fraction
# minority admitted should be the same.

constrain_no_grad = solver.Constraint(0.0, 0.0)

for key in majority_no_grad:
    r, t, Y = key
    N_IN_STRATUM = N_IN_STRATAS[(r,t,Y)]
    N_TOTAL = NUM_TOTALS[(r,Y)]
    
    constrain_no_grad.SetCoefficient(vars_cache[key],
                                     float(N_IN_STRATUM) / float(N_TOTAL))

for key in minority_no_grad:
    r, t, Y = key
    N_IN_STRATUM = N_IN_STRATAS[(r,t,Y)]
    N_TOTAL = NUM_TOTALS[(r,Y)]
    
    constrain_no_grad.SetCoefficient(
            vars_cache[key], -1.0 * (float(N_IN_STRATUM) / float(N_TOTAL)))

constrain_graduate = solver.Constraint(0, 0)

for ix, row in dff.iterrows():
    
    key = (row['R'],row['T'],row['Y'])
    n_graduate = row['Y'] * row['N']
    constrain_graduate.SetCoefficient(vars_cache[key], float(n_graduate))

constrain_minority_admit = solver.Constraint(0, 0)

for ix, row in dff.iterrows():
    key = (row['R'],row['T'],row['Y'])
    n_minority = row['R'] * row['N']
    constrain_minority_admit.SetCoefficient(vars_cache[key], float(n_minority))

#################################### SOLVE #####################################

for region in tqdm(outcomes_grid, desc="Checking grid cells"):
    constrain_graduate.SetBounds(float(region['min_graduates']),
                                 float(region['max_graduates']))
    constrain_minority_admit.SetBounds(float(region['min_minority_admits']),
                                    float(region['max_minority_admits']))
    status = solver.Solve()

    if status == 0:
        region['policy_exists'] = 1
        
        row = []
        admit = []

        for i in applicant_stratum:
            row.append(int(str(i)))
            admit.append(i.solution_value())
        region['policy'] = (row,admit)    
    else:
        continue
        
outcomes_grid = pd.DataFrame(outcomes_grid)
outcomes_grid = outcomes_grid[outcomes_grid['policy_exists']==1]
outcomes_grid.to_csv('data/outcomes_grid_cf_eo.csv', index=False)
