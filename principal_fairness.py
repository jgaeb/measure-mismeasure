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

df['Y_stratum'] = df['Y'].astype(str) + df['Y_reject'].astype(str)

# Get number of people in each stratum
dff = (df[['R','T','key','Y','Y_reject']]
       .groupby(['R','T','Y','Y_reject'])
       .count()
       .reset_index())
dff.columns = ['R','T','Y','Y_reject','N']

TOTAL_ADMITS = int(len(df)*FRAC_ADMIT)
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
                                                     + GRID_SIZE / 2,
                              'max_graduates': y_iter + GRID_SIZE,
                              'min_graduates': y_iter,
                              'policy_exists': 'Unknown'})

df_pareto = pd.DataFrame({'# Minority Admits': Xs, '# Graduates': Ys})

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
    
    # store variable by index, and also by stratum R, T, Y, Y_reject
    applicant_stratum.append(numvar)
    vars_cache[(row['R'],row['T'],row['Y'], row['Y_reject'])] = numvar
    
    # Benefit of admitting people is total utility in that stratum
    objective.SetCoefficient(applicant_stratum[ix], float(row['N']))

objective.SetMaximization()

# Constraint: At most K applicants
K = int(len(df) * FRAC_ADMIT)
admit_quota = solver.Constraint(0, K)

# Total admits cannot exceed K 
for ix, row in dff.iterrows():
    admit_quota.SetCoefficient(applicant_stratum[ix], float(row['N']))

# Add Equalized Odds Constraints

# Make sure that you have to add all people in Y stratum or none, i.e., you
# can't add only people who graduate and reject those who don't from same T, R
# stratum
for ix, row in dff.iterrows():
    var1 = vars_cache[(row['R'],row['T'],row['Y'], row['Y_reject'])]
    key2s = [(row['R'],row['T'], 1-row['Y'], 1-row['Y_reject']),
             (row['R'],row['T'], row['Y'], 1-row['Y_reject']),
             (row['R'],row['T'], 1-row['Y'], row['Y_reject'])]

    for key2 in key2s:
        constrain_bp = solver.Constraint(0.0, 0.0)
        
        if key2 not in vars_cache:
            continue
            
        var2 = vars_cache[key2]
        
        constrain_bp.SetCoefficient(var1, -1.0)
        constrain_bp.SetCoefficient(var2, 1.0)

majority_pass_pass_reject = []
majority_fail_pass_reject = []
minority_pass_pass_reject = []
minority_fail_pass_reject = []
majority_pass_fail_reject = []
majority_fail_fail_reject = []
minority_pass_fail_reject = []
minority_fail_fail_reject = []

for key in vars_cache:
    r, t, Y, Y_reject = key
    if Y == 1 and r==0 and Y_reject==1:
        majority_pass_pass_reject.append(key)
    elif Y == 0 and r==0 and Y_reject==1:
        majority_fail_pass_reject.append(key)
    elif Y == 1 and r==1 and Y_reject==1:
        minority_pass_pass_reject.append(key)
    elif Y == 0 and r==1 and Y_reject==1:
        minority_fail_pass_reject.append(key)
        
    if Y == 1 and r==0 and Y_reject==0:
        majority_pass_fail_reject.append(key)
    elif Y == 0 and r==0 and Y_reject==0:
        majority_fail_fail_reject.append(key)
    elif Y == 1 and r==1 and Y_reject==0:
        minority_pass_fail_reject.append(key)
    elif Y == 0 and r==1 and Y_reject==0:
        minority_fail_fail_reject.append(key)

NUM_TOTALS = {}
df_totals = (dff[['N','R','Y','Y_reject']]
             .groupby(['R','Y','Y_reject'])
             .sum()
             .reset_index())
for ix, row in df_totals.iterrows():
    NUM_TOTALS[(row['R'],row['Y'],row['Y_reject'])] = row['N']
    
N_IN_STRATAS = {}
for ix, row in dff.iterrows():
    N_IN_STRATAS[(row['R'],row['T'],row['Y'],row['Y_reject'])] = row['N']

# Of those who graduate if accepted and if rejected, fraction majority admitted
# and fraction minority admitted should be the same

constrain_pass_pass_reject = solver.Constraint(0.0, 0.0)

for key in majority_pass_pass_reject:
    r, t, Y, Y_reject = key
    N_IN_STRATUM = N_IN_STRATAS[(r,t,Y, Y_reject)]
    N_TOTAL = NUM_TOTALS[(r,Y, Y_reject)]
    
    constrain_pass_pass_reject.SetCoefficient(
            vars_cache[key], float(N_IN_STRATUM) / float(N_TOTAL))

for key in minority_pass_pass_reject:
    r, t, Y, Y_reject = key
    N_IN_STRATUM = N_IN_STRATAS[(r,t,Y, Y_reject)]
    N_TOTAL = NUM_TOTALS[(r,Y, Y_reject)]
    
    constrain_pass_pass_reject.SetCoefficient(
            vars_cache[key], -1.0 * (float(N_IN_STRATUM) / float(N_TOTAL)))

# Of those who fail if accepted and graduate if rejected, fraction majority
# admitted and fraction minority admitted should be the same

constrain_fail_boards_pass_boards_reject = solver.Constraint(0.0, 0.0)

for key in majority_fail_pass_reject:
    r, t, Y, Y_reject = key
    N_IN_STRATUM = N_IN_STRATAS[(r,t,Y, Y_reject)]
    N_TOTAL = NUM_TOTALS[(r,Y, Y_reject)]
    
    constrain_fail_pass_reject.SetCoefficient(
            vars_cache[key], float(N_IN_STRATUM) / float(N_TOTAL))

for key in minority_fail_pass_reject:
    r, t, Y, Y_reject = key
    N_IN_STRATUM = N_IN_STRATAS[(r,t,Y, Y_reject)]
    N_TOTAL = NUM_TOTALS[(r,Y, Y_reject)]
    
    constrain_fail_pass_reject.SetCoefficient(
            vars_cache[key], -1.0 * (float(N_IN_STRATUM) / float(N_TOTAL)))

# Of those who graduate if accepted and fail if rejected, fraction majority
# admitted and fraction minority admitted should be the same

constrain_pass_fail_reject = solver.Constraint(0.0, 0.0)

for key in majority_pass_fail_reject:
    r, t, Y, Y_reject = key
    N_IN_STRATUM = N_IN_STRATAS[(r,t,Y, Y_reject)]
    N_TOTAL = NUM_TOTALS[(r,Y, Y_reject)]
    
    constrain_pass_fail_reject.SetCoefficient(
            vars_cache[key], float(N_IN_STRATUM) / float(N_TOTAL))

for key in minority_pass_fail_reject:
    r, t, Y, Y_reject = key
    N_IN_STRATUM = N_IN_STRATAS[(r,t,Y, Y_reject)]
    N_TOTAL = NUM_TOTALS[(r,Y, Y_reject)]
    
    constrain_pass_fail_reject.SetCoefficient(
            vars_cache[key], -1.0 * (float(N_IN_STRATUM) / float(N_TOTAL)))

    
# Of those who fail if admitted an if rejected, fraction majority admitted and
# fraction minority admitted should be the same

constrain_fail_fail_reject = solver.Constraint(0.0, 0.0)

for key in majority_fail_fail_reject:
    r, t, Y, Y_reject = key
    N_IN_STRATUM = N_IN_STRATAS[(r,t,Y, Y_reject)]
    N_TOTAL = NUM_TOTALS[(r,Y, Y_reject)]
    
    constrain_fail_fail_reject.SetCoefficient(
            vars_cache[key], float(N_IN_STRATUM) / float(N_TOTAL))

for key in minority_fail_fail_reject:
    r, t, Y, Y_reject = key
    N_IN_STRATUM = N_IN_STRATAS[(r,t,Y, Y_reject)]
    N_TOTAL = NUM_TOTALS[(r,Y, Y_reject)]
    
    constrain_fail_fail_reject.SetCoefficient(
            vars_cache[key], -1.0 * (float(N_IN_STRATUM) / float(N_TOTAL)))


# Add outcome constraints

constrain_graduate = solver.Constraint(0, 0)

for ix, row in dff.iterrows():
    
    key = (row['R'],row['T'],row['Y'],row['Y_reject'])
    n_graduate = row['Y'] * row['N']
    constrain_graduate.SetCoefficient(vars_cache[key], float(n_graduate))

constrain_minority_admit = solver.Constraint(0, 0)

for ix, row in dff.iterrows():
    key = (row['R'],row['T'],row['Y'],row['Y_reject'])
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
outcomes_grid.to_csv('data/outcomes_grid_principal_fairness.csv', index=False)
