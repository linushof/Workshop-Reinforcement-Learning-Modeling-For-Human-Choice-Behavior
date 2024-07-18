import numpy as np
import pandas as pd
import pickle

# Load data
rndwlk = pd.read_csv('Lesson_2_Sequence_learning_in_a_Tree_task/Python/data/rndwlk_depth3_100trials.csv', header=None)

# Set parameters
alpha = 0.3
beta = 3
lambda_ = 0.2

# Set initial variables
Narms = 2
Nstages = 3
Nblocks = 2
Nstates = 8
Ntrials_perblock = 100
expvalues = rndwlk.values
Qval = np.zeros((Narms, Nstates, Nstages))
df = pd.DataFrame()

subject = 1

for block in range(1, Nblocks + 1):
    Qval.fill(0.5)
    
    for trial in range(1, Ntrials_perblock + 1):
        # Simulate agent's actions according to a softmax policy
        state1 = 1
        p_1 = np.exp(______ * Qval[:, state1 - 1, 0]) / np.sum(np.exp(______ * Qval[:, state1 - 1, 0]))
        choice1 = np.random.choice(np.arange(1, 3), p=p_1)
        
        state2 = choice1
        p_2 = np.exp(beta * Qval[:, ______ - 1, 1]) / np.sum(np.exp(______ * Qval[:, state2 - 1, 1]))
        choice2 = np.random.choice(np.arange(1, 3), p=p_2)
        
        state3 = -2 + 2 * choice1 + choice2
        p_3 = np.exp(beta * Qval[:, state3 - 1, 2]) / np.sum(np.exp(beta * Qval[:, state3 - 1, 2]))
        choice3 = np.random.choice(np.arange(1, 3), p=______)
        
        # Simulate outcome
        reward_prob = expvalues[-2 + 2 * state3 + choice3 - 1, trial - 1]
        reward = np.random.choice([0, 1], p=[1 - reward_prob, reward_prob])
        
        # Save trial's data
        dfnew = pd.DataFrame({
            'subject': [subject],
            'block': [block],
            'trial': [trial],
            'first_trial_in_block': [int(trial == 1)],
            'choice1': [choice1],
            'choice2': [choice2],
            'choice3': [choice3],
            'state1': [state1],
            'state2': [state2],
            'state3': [state3],
            'expval_ch': [reward_prob],
            'reward': [reward]
        })
        
        dfnew = pd.concat([dfnew, pd.DataFrame(expvalues[:, trial - 1]).T], axis=1)
        df = pd.concat([df, dfnew], ignore_index=True)
        
        # Update Q-values
        PE_1 = Qval[choice2 - 1, state2 - 1, 1] - Qval[choice1 - 1, state1 - 1, 0]
        PE_2 = Qval[choice3 - 1, state3 - 1, 2] - Qval[______, ______, 1]
        PE_3 = ______ - Qval[choice3 - 1, state3 - 1, 2]
        
        Qval[choice1 - 1, state1 - 1, 0] += ______ * PE_1 + alpha * ______ * PE_2 + alpha * (______ ** 2) * PE_3
        Qval[choice2 - 1, state2 - 1, 1] += ______ * PE_2 + alpha * ______ * PE_3
        Qval[choice3 - 1, state3 - 1, 2] += ______ * PE_3

# Save data
df.to_pickle("Lesson_2_Sequence_learning_in_a_Tree_task/Python/data/sequenceLearning_simulatedData.pkl")

#### Save in Stan format ####
def make_mystandata(data, subject_column, block_column, var_toinclude, var_tobenamed=None, additional_arguments=None):
    subjects_list = data[subject_column].unique()
    blocks_list = data[block_column].unique()
    
    Ntrials_per_subject = [sum(data[subject_column] == subj) for subj in subjects_list]
    
    Ntrials_per_subject_per_block = [[sum((data[subject_column] == subj) & (data[block_column] == blk)) for subj in subjects_list] for blk in blocks_list]
    
    max_trials_per_subject = max(Ntrials_per_subject)
    
    mydata = {var: np.array([list(data.loc[data[subject_column] == subj, var]) + [9999] * (max_trials_per_subject - sum(data[subject_column] == subj)) for subj in subjects_list]) for var in var_toinclude}
    
    if var_tobenamed:
        mydata = dict(zip(var_tobenamed, mydata.values()))
    
    mydata.update({
        'Nsubjects': len(subjects_list),
        'Nblocks': len(blocks_list),
        'Ntrials': max_trials_per_subject,
        'Ntrials_per_subject': Ntrials_per_subject,
        'Ntrials_per_subject_per_block': Ntrials_per_subject_per_block
    })
    
    if additional_arguments:
        mydata.update(additional_arguments)
    
    return mydata

data_for_stan = make_mystandata(
    data=df, 
    subject_column='subject',
    block_column='block',
    var_toinclude=[
        'first_trial_in_block',
        'trial',
        'state1',
        'state2',
        'state3',
        'choice1',
        'choice2',
        'choice3',
        'reward'
    ],
    additional_arguments={'Nstages': 3, 'Nstates': 4}
)

# Save data for Stan
with open("Lesson_2_Sequence_learning_in_a_Tree_task/Python/data/sequenceLearning_data_stan_format.pkl", "wb") as f:
    pickle.dump(data_for_stan, f)
