### This folder has all the files with projected data being used in RL models.

The action space of the RL model is 1000 action buckets and the range of bucket sizes used for Contextual Bandit model is 200-500. The Batch size of total number of vaccines is taken as 10 lakh. This folder contains the outputs of the RL models, CB model and the Naive approach adopted for distributing vaccines as described in our paper.

**projection.csv** has projected data generated from SEIR model. It has future context information of five states for future dates.
This folder contains the action values for different corresponding bucket sizes used for our Reinforcement Learning models (ACKTR & DQN)
