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
df_stratum_utility['key'] = df_stratum_utility['R'].astype(str) + "_" \
        + df_stratum_utility['T'].astype(str)

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
                                                     + GRID_SIZE / 2,
                              'max_graduates': y_iter + GRID_SIZE,
                              'min_graduates': y_iter,
                              'policy_exists': 'Unknown'})

df_pareto = pd.DataFrame({'# Minority Admits': Xs, '# Graduates': Ys})

dff = df[['R','T','Y']].groupby(['R','T']).count().reset_index()
dff.columns = ['R','T','N']
dff['key'] = dff['R'].astype(str) + "_" + dff['T'].astype(str)


dff2 = df[['R','T','Y']].groupby(['R','T']).mean().reset_index()
dff2.columns = ['R','T','Y']
dff2['key'] = dff['R'].astype(str) + "_" + dff['T'].astype(str)

dff = dff.merge(dff2[['key','Y']],on='key')

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
    vars_cache[(row['R'],row['T'])] = numvar
    
    # Benefit of admitting people is total utility in that stratum
    objective.SetCoefficient(applicant_stratum[ix], float(row['N']))

objective.SetMaximization()

# Constraint: At most K applicants
K = int(len(df) * FRAC_ADMIT)
admit_quota = solver.Constraint(0, K)

# Total admits cannot exceed K 
for ix, row in dff.iterrows():
    admit_quota.SetCoefficient(applicant_stratum[ix], float(row['N']))

# Add counterfactual fairness constraints

def convertListToProb(raw_list):
    counts = dict(Counter(raw_list))
    probs = {}
    for test_score in counts:
        probs[test_score]  = counts[test_score]/float(len(raw_list))
    return [(probs[t], t) for t in probs]


T_minoritys_list = (df[df['R']==0][['T','T_minority']]
                    .groupby('T')['T_minority']
                    .apply(list)
                    .reset_index(name='T_minoritys'))
T_minoritys_list['probs'] = (T_minoritys_list['T_minoritys']
                             .apply(convertListToProb))

for ix, row in T_minoritys_list.iterrows():
    cf_fair_stratum = solver.Constraint(0.0, 0.0)

    majority_T = row['T']
    minoritys_Ts = row['probs']        
    cf_fair_stratum.SetCoefficient(vars_cache[(0.0, majority_T)], -1.0)
    for prob in minoritys_Ts:
        if (1.0, prob[1]) not in vars_cache:
            vars_cache[(1.0, prob[1])] = solver.NumVar(0.0, 1.0,
                                                       str((1.0, prob[1])))
        cf_fair_stratum.SetCoefficient(vars_cache[(1.0, prob[1])], prob[0])

T_majoritys_list = (df[df['R']==1][['T','T_majority']]
                    .groupby('T')['T_majority']
                    .apply(list)
                    .reset_index(name='T_majoritys'))
T_majoritys_list['probs'] = (T_majoritys_list['T_majoritys']
                             .apply(convertListToProb))

for ix, row in T_majoritys_list.iterrows():
    cf_fair_stratum = solver.Constraint(0.0, 0.0)

    minority_T = row['T']
    majority_Ts = row['probs']
    
    cf_fair_stratum.SetCoefficient(vars_cache[(1.0, minority_T)], -1.0)
    for prob in majority_Ts:
        if (0.0, prob[1]) not in vars_cache:
            vars_cache[(0.0, prob[1])] = solver.NumVar(0.0, 1.0,
                                                       str((0.0, prob[1])))
        cf_fair_stratum.SetCoefficient(vars_cache[(0.0, prob[1])], prob[0])

constrain_graduate = solver.Constraint(0, 0)

for ix, row in dff.iterrows():
    key = (row['R'],row['T'])
    n_graduate = row['Y'] * row['N']
    constrain_graduate.SetCoefficient(vars_cache[key], float(n_graduate))

constrain_minority_admit = solver.Constraint(0, 0)

for ix, row in dff.iterrows():
    key = (row['R'],row['T'])
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
outcomes_grid.to_csv('data/outcomes_grid_cf_fairness.csv', index=False)
