#!/usr/bin/env python
# coding: utf-8

# In[1]:


import igraph as ig
import cairocffi as cairo


# In[ ]:





# In[ ]:





# In[2]:


tt = ig.Graph (n=100)

vis = set({})

tt.add_edges ([(0,1), (0,2), (0,3)])
vis.add (0)
vis.add (1)
vis.add (2)
vis.add (3)

i = 1

while (len (vis) < 100):
    
    j = i+1
    while j in vis and j < 101:
        j = j+1
        
    tt.add_edges ([(i, j)])
    tt.add_edges ([(i, j+1)])
    vis.add (j)
    vis.add (j+1)
    i = i+1

tt.vs ["color"] = "yellow"
tt.es ["color"] = "blue"
tt.vs ["label"] = [i for i in range (1, 101)]
ig.plot(tt, layout = tt.layout ("kk"))


# In[ ]:





# In[ ]:





# In[3]:


print (tt)


# In[ ]:




