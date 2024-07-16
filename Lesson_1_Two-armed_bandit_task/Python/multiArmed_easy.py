import pandas as pd
import numpy as np


#function to generate random walk for reward expected values
def generate_random_walk(Narms=2, Ntrials=100, step_size=0.01):
    """
    Generates a random walk for the expected values of multiple arms.

    Parameters:
    Narms (int): Number of arms.
    Ntrials (int): Number of trials.
    step_size (float): Maximum change in expected value per step.

    Returns:
    np.ndarray: A matrix of size (num_arms, num_trials) with the random walk values.
    """
    # Initialize the expected values to random values between 0 and 1
    expvalues = np.random.rand(Narms, Ntrials)
    
    for trial in range(1, Ntrials):
        # Generate random steps for each arm
        steps = np.random.uniform(-step_size, step_size, Narms)
        # Update the expected values
        expvalues[:, trial] = expvalues[:, trial - 1] + steps
        # Ensure values remain between 0 and 1
        expvalues[:, trial] = np.clip(expvalues[:, trial], 0, 1)
    
    return expvalues


#set parameters
alpha = 0.3
beta  = 3
  
#set initial var
Narms              = 2
Nblocks            = 4
Ntrials            = 100
expvalues          = generate_random_walk()

data = {
    'block':[],
    'trial':[],
    'choice':[],
    'expval_ch':[],
    'reward':[],
    'Qval_1':[],
    'Qval_2':[],
    'expval_1':[],
    'expval_2':[],
    }

for block in range(Nblocks):

    Qval = np.repeat(0.5, ____)

    for trial in range(Ntrials):

        #players choice
        p = np.exp(____ * Qval) / np.sum(np.exp(____ * Qval))
        choice = np.random.choice(____, p=p)

        #outcome
        p = [1-expvalues[____,trial], expvalues[____,trial]]
        reward = np.random.choice([0,1], p=____)

        #save trial's data
      
        #create data for current trial
        data['block'].append(block + 1)
        data['trial'].append(trial + 1)
        data['choice'].append(choice + 1)
        data['expval_ch'].append(____)
        data['reward'].append(reward)
        data['Qval_1'].append(Qval[0])
        data['Qval_2'].append(Qval[1])
        data['expval_1'].append(expvalues[0,trial])
        data['expval_2'].append(expvalues[1,trial])

        #updating Qvalues
        Qval[choice] = ____ + ____*(reward - ____)

#create data frame for all data and save
df = pd.DataFrame(data).reset_index().drop(columns=['index'])
df.to_csv('multi_armed_bandit/Python/data/multiArmedSimulationData.csv',index=False)
