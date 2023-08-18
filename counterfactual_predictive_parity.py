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

df_train = pd.read_csv('data/train.csv')
df_stratum_utility = (df_train[['R','T','Y']]
                      .groupby(['R','T'])
                      .mean()
                      .reset_index())
df_stratum_utility['stratum_utility'] = df_stratum_utility['Y'] 
df_stratum_utility['key'] = df_stratum_utility['R'].astype(str) + "_" \
        + df_stratum_utility['T'].astype(str)

df = df.merge(df_stratum_utility[['stratum_utility','key']], on='key')
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


# Get graduation probability in each stratum

df_prob = df[['R','T','Y']].groupby(['R','T']).mean().reset_index()
df_prob.columns = ['R','T','ml_outcomes_prob']
dff = dff.merge(df_prob[['ml_outcomes_prob']],left_index=True,right_index=True)

dff['Pr(X=X_j)'] = dff['N']/(dff['N']).sum()

df_decisions = None
max_utility = -1.0
solutions = 0

for C_y in tqdm(np.linspace(0.0,1.0,1001), desc="Iterating over C_y"):

    dff['alpha_j'] = dff['Pr(X=X_j)']* (dff['ml_outcomes_prob'] - C_y)
    solver = pywraplp.Solver.CreateSolver('GLOP')
    
    applicant_stratum = []
    vars_cache = {}
    alpha_j = {}
    
    # Objective: Maximize the expected utility of the admitted students
    objective = solver.Objective()
    
    # For each stratum
    for ix, row in dff.iterrows():
        # probability of admission
        numvar = solver.NumVar(0.0, 1.0, str(ix))
        
        # store variable by index, and also by stratum R, T
        applicant_stratum.append(numvar)
        vars_cache[(row['R'],row['T'])] = numvar
        
        alpha_j[(row['R'],row['T'])] = row['alpha_j']
        
        # Benefit of admitting people is total utility in that stratum
        objective.SetCoefficient(applicant_stratum[ix], float(row['N']))
        
    objective.SetMaximization()

    # Constraint: At most K applicants
    K = int(len(df) * FRAC_ADMIT)
    admit_quota = solver.Constraint(0, K)
    
    # Total admits cannot exceed K 
    for ix, row in dff.iterrows():
        admit_quota.SetCoefficient(applicant_stratum[ix], float(row['N']))
    

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
    
    sum_alpha = 0
    R = 1 
    
    for T in list(dff[dff['R']==R]['T']):
        sum_alpha += alpha_j[(R,T)] 
        
    cf_minority = solver.Constraint(sum_alpha, sum_alpha)
    
    for T in list(dff[dff['R']==R]['T']):    
        cf_minority.SetCoefficient(vars_cache[(R,T)], alpha_j[(R,T)] )    
    
    sum_alpha = 0
    R = 0
    
    for T in list(dff[dff['R']==R]['T']):
        sum_alpha += alpha_j[(R,T)] 
        
    cf_majority = solver.Constraint(sum_alpha, sum_alpha)
    
    for T in list(dff[dff['R']==R]['T']):    
        cf_majority.SetCoefficient(vars_cache[(R,T)], alpha_j[(R,T)] )
        
    count = 0 
    
    for region in tqdm(outcomes_grid, desc="Checking grid cells", leave=False):
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
outcomes_grid.to_csv('data/outcomes_grid_cf_pred_par.csv', index=False)
