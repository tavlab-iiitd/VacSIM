# Stable Baselines only supports tensorflow 1.x for now
# %tensorflow_version 1.x
!apt install swig cmake libopenmpi-dev zlib1g-dev
!pip install stable-baselines[mpi]==2.10.0 box2d box2d-kengz

import gym
import numpy as np
import pandas as pd
from stable_baselines import DQN, DDPG, ACKTR
from gym import spaces 
import copy
from collections import Counter
import math
import random
class StatesEnv(gym.Env):
    metadata = {'render.modes':['human']}
    def __init__(self, s, episodes, total, confirmed, dr, rr, population, susceptible_ratio, bed, icu, ventilator, old, buckets, total_susceptible):  #susceptible ratio is ratio of susceptible people in that state/total susceptible people combining all states 
        self.states = s #no of states
        self.observation_space = spaces.Box(np.array([0,0,0,0,0,0,0,0,0]), np.array([1,1,1,1,1,1,1,1,1]), shape=(9,), dtype = np.float32)
        self.action_space = spaces.Discrete(buckets)
        self.curr_step = 1
        self.done = False
        self.valueMap = np.zeros((self.states, 100))
        self.total = total #total number of vials available in 1 batch = batch size 
        self.episodes = episodes
        self.received = [0]*self.states
        self.states_cond = []
        self.action_list = None
        self.susc = [0]*self.states
        self.confirmed=confirmed
        self.dr=dr
        self.rr=rr 
        self.population=population
        self.susceptible_ratio=susceptible_ratio
        self.bed=bed
        self.icu=icu
        self.ventilator=ventilator
        self.old=old
        self.buckets=buckets
        self.total_susceptible=total_susceptible
        self.reset()
    def get_discrete_int(self, n):
        discrete_int = int(n)
        return discrete_int

    def reset(self):
        self.curr_step = 1
        self.done = False
        self.total = 1000000          #10 lakh vaccines
        self.states_cond =  np.array([self.confirmed,self.dr,self.rr,self.population,self.susceptible_ratio,self.bed,self.icu,self.ventilator,self.old])
        return copy.deepcopy(self.states_cond)
        
    def step(self, action):
    	self.action_list = action
        reward = self.get_reward()
        vaccine_allotted = (action/self.buckets)*self.total
        new_susceptible_ratio = ((self.susceptible_ratio*total_susceptible)-vaccine_allotted)/(total_susceptible-vaccine_allotted)
        self.states_cond[4] = new_susceptible_ratio
        # increment episode
        if self.curr_step == self.episodes:
            self.done=True
        else:
            self.done=False
            self.curr_step+=1
        
        return self.states_cond, reward, self.done, {'action_list': self.action_list, 'distribution':self.action_list, 'episode_number': self.curr_step}
    
    def get_reward(self):
        reward = 0              
        A=(self.action_list*(1/self.buckets))
        S=self.states_cond[4]                          
        reward = (100 * math.exp((-(A-S)**2)/0.0001))
        return reward 

    def close(self):
        pass


df=pd.read_csv('../data/projections.csv',skipinitialspace=True,index_col=None)
action_list=[]
reward_list=[]
for i in range(0,75,5):   # change according to how many days the model needs to be trained
    arr=df[i:i+5].to_numpy()    #i to i+5 for 5 states on single date
    total_confirmed=sum(arr[:,2])
    total_population=sum(arr[:,5])
    total_susceptible=sum(arr[:,6])
    total_beds=sum(arr[:,7])
    total_icu=sum(arr[:,8])
    total_venti=sum(arr[:,9])
    total_old=sum(arr[:,10])
    for s in range(5):          # looping for 5 states
        confirmed_ratio=arr[s][2]/total_confirmed
        dr=arr[s][3]; rr=arr[s][4]
        population_ratio=arr[s][5]/total_population
        susceptible_ratio = arr[s][6]/total_susceptible
        bed_ratio=arr[s][7]/total_beds
        icu_ratio=arr[s][8]/total_icu
        venti_ratio=arr[s][9]/total_venti
        old_ratio=arr[s][10]/total_old
        env=StatesEnv(1,1,100000,confirmed_ratio,dr,rr,population_ratio,susceptible_ratio,bed_ratio,icu_ratio,venti_ratio,old_ratio,1000,total_susceptible)
        model = ACKTR('MlpPolicy', env, verbose=0,n_steps=1, seed=1, learning_rate=1e-3)
        model.learn(total_timesteps=int(2e3), log_interval=100)
        obs = env.reset()
        action, _states = model.predict(obs)
        obs, reward, done, info = env.step(action)
        action_list.append(action)
        reward_list.append(reward)
        
#get actions and rewards
print(action_list,reward_list)
