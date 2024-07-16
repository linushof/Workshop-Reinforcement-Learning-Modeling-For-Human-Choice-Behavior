import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Clear the workspace
# In Python, you typically don't need to clear the workspace as in R
# Just make sure variables from previous runs don't interfere

# Pre-allocation
subject = 1
rndwlk = pd.read_csv('sequence_learning/Python/data/rndwlk_depth3_100trials.csv', header=None)

# Load the simulated data (assuming the file is a CSV or similar)
df = pd.read_pickle('sequence_learning/Python/data/sequenceLearning_simulatedData.pkl')

Narms = 2
Nstages = 3
Nblocks = 1
Nstates = 8
Ntrials_perblock = 100
expvalues = rndwlk.values
expvalues_row_names = ['ev1', 'ev2', 'ev3', 'ev4', 'ev5', 'ev6', 'ev7', 'ev8']

Qval = np.tile(0.5, (Narms, Nstates, Nstages))
alpha = 0.5
beta = 3
lambdas = np.arange(0.1, 1.0, 0.1) # array([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9])

log_likelihood_1 = np.zeros(9)
log_likelihood_2 = np.zeros(9)
log_likelihood_3 = np.zeros(9)

for idx, l in enumerate(lambdas):
    Qval.fill(0.5)
    
    log_p1 = np.zeros(Ntrials_perblock)
    log_p2 = np.zeros(Ntrials_perblock)
    log_p3 = np.zeros(Ntrials_perblock)
    
    for t in range(Ntrials_perblock):
        state1 = 1
        p_1 = np.exp(beta * Qval[:, state1 - 1, 0]) / np.sum(np.exp(beta * Qval[:, state1 - 1, 0]))
        choice1 = int(df.iloc[t]["choice1"]) - 1
        p_choice1 = p_1[choice1]
        log_p1[t] = np.log(p_choice1)
        
        state2 = choice1 + 1
        p_2 = np.exp(beta * Qval[:, state2 - 1, 1]) / np.sum(np.exp(beta * Qval[:, state2 - 1, 1]))
        choice2 = int(df.iloc[t]["choice2"]) - 1
        p_choice2 = p_2[choice2]
        log_p2[t] = np.log(p_choice2)
        
        state3 = -2 + 2 * (choice1 + 1) + (choice2 + 1)
        p_3 = np.exp(beta * Qval[:, state3 - 1, 2]) / np.sum(np.exp(beta * Qval[:, state3 - 1, 2]))
        choice3 = int(df.iloc[t]["choice3"]) - 1
        p_choice3 = p_3[choice3]
        log_p3[t] = np.log(p_choice3)
        
        reward = int(df.iloc[t]["reward"])
        
        PE_1 = Qval[choice2, state2 - 1, 1] - Qval[choice1, state1 - 1, 0]
        PE_2 = Qval[choice3, state3 - 1, 2] - Qval[choice2, state2 - 1, 1]
        PE_3 = reward - Qval[choice3, state3 - 1, 2]
        
        Qval[choice1, state1 - 1, 0] += alpha * PE_1 + alpha * l * PE_2 + alpha * (l ** 2) * PE_3
        Qval[choice2, state2 - 1, 1] += alpha * PE_2 + alpha * l * PE_3
        Qval[choice3, state3 - 1, 2] += alpha * PE_3
    
    log_likelihood_1[idx] = np.sum(log_p1)
    log_likelihood_2[idx] = np.sum(log_p2)
    log_likelihood_3[idx] = np.sum(log_p3)

log_likelihood = log_likelihood_1 + log_likelihood_2 + log_likelihood_3
max_index = np.argmax(log_likelihood)
print(f"Max log likelihood index: {max_index}")
print(f"Max log likelihood value: {log_likelihood[max_index]}")

plt.plot(lambdas, log_likelihood)
plt.xlabel('Lambda')
plt.ylabel('Log Likelihood')
plt.title('Log Likelihood vs. Lambda')
plt.show()
