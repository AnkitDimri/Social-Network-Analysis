#!/usr/bin/env python
# coding: utf-8

# In[1]:


import igraph as ig
import cairocffi as cairo
import numpy as np
import matplotlib.pyplot as plt
import random


# In[ ]:





# In[ ]:





# In[2]:


def connected (v, p):

    G = ig.Graph (n=v)

    for i in range (0, v-1):
        for j in range (i+1, v):
            r = toss (p)

            if r is 1:
                G.add_edges ([(i,j)])
            
    #print (G)
    return G.is_connected ()

        


# In[3]:


def toss (p):
    
    r = random.random ()
    
    return 1 if r < p else 0


# In[4]:


def plot_nVSp (n):
    
    v_size = []
    prob = []
    
    for i in range (n):
        
        v = i+1
        for j in range (100):
            
            p = (j+1)/100
            
            check = connected (v, p)
            
            if connected (v, p):
                v_size.append (v)
                prob.append (p)
                break
            
        
    
    plt.plot (v_size, prob)
            
            


# In[ ]:





# In[ ]:





# In[5]:


# plot for n values from 1-100
plt.xlabel ("n")
plt.ylabel ("Probablity")

plot_nVSp (100)


# In[6]:


plt.savefig ("n_p_plot.png")


# In[ ]:




