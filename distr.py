import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os
import shutil
from scipy.integrate import simps
from joblib import Parallel, delayed
import sys
###########################################################
def get_prob(counts,bins):
    NORM = simps(counts, x=bins[:-1], even='avg')
    slice_x = []
    slice_y = []
    for j in range(len(counts)):
      en_j = bins[:-1][j]
      if en_j >= 0.3:
        slice_x.append(en_j)
        slice_y.append(counts[j])
    prob_above = simps(slice_y, x=slice_x, even='avg')/NORM
    return prob_above
def get_ensemble_distr(N):
    path_dist = 'DIST'
    os.system('rm -rf DIST')
    os.system('mkdir DIST')
    #'''
    Nbin = 200
    ff = [fold for fold in os.listdir() if os.path.isdir(fold) and path_dist not in fold ]  
    os.chdir(ff[0]+'/WDIST')
    unique_ws = [fold for fold in os.listdir() if 'w_' in fold]
    unique_ws = sorted(unique_ws,key=lambda x: float(x.split('_')[-1].split('.txt')[0]))  
    unique_ws_numbers = [ float(x.split('_')[-1].split('.txt')[0]) for x in unique_ws]
    os.chdir('../..')
    
    Nens = len(ff)
    bin_array = np.linspace(0,3,Nbin)
    probs  = []
    for w in unique_ws:
      w_list = []
      number = float(w.split('_')[-1].split('.txt')[0])
      for ens in ff:
        path=ens+'/WDIST/'+w
        w_ens = np.loadtxt(path)
        w_list = w_list + w_ens.tolist()
      plt.clf()
      w_list = np.array(w_list)#/np.mean(w_list)
      counts, bin_edges, _ = plt.hist(w_list, bins=bin_array, color='red', alpha=0.3, edgecolor='black',density=True)
       
      prob = get_prob(counts,bin_edges)
      probs.append(prob)
      plt.savefig(path_dist+'/fig_'+str(number)+'.pdf')
      np.savetxt(path_dist+'/dist_'+str(number)+'.txt', np.column_stack((bin_edges[:-1], counts)), fmt='%.5f', delimiter='\t')
    np.savetxt(path_dist+'/prob.txt', np.column_stack((unique_ws_numbers, probs)), fmt='%.5f', delimiter='\t')      
    
    with open(path_dist+'/dist_dat.txt','w') as f:
      f.write('prob={:}\nstd={:}\nmean={:}'.format(probs[-1], np.std(w_list),np.mean(w_list)))
    #'''
######################################################    
def wrapper(fjob,N):
    os.chdir(fjob)
    fpuns = [fold for fold in os.listdir() if os.path.isdir(fold) and "PUN_" in fold]
    fpuns = sorted(fpuns)
    for pun in fpuns:
      os.chdir(pun)
      get_ensemble_distr(N)
      print(fjob,pun)
      os.chdir('..')
    os.chdir('..')
def wrapper2(pun,N):
      os.chdir(pun)
      get_ensemble_distr(N)
      os.chdir('..')

def wrapper3(path,N):
      os.chdir(path)
      get_ensemble_distr(N)


N       = 101 #number of points written during dynamics

wrapper3(sys.argv[-1],N)

'''
folders = [fold for fold in os.listdir() if os.path.isdir(fold) and "JOB_" in fold]
folders = sorted(folders)
wrapp   = lambda x: wrapper(x,N)
wrapp2  = lambda x: wrapper2(x,N)

for fjob in folders:
    print(fjob)
    os.chdir(fjob)
    fpuns = [fold for fold in os.listdir() if os.path.isdir(fold) and "PUN_" in fold]
    fpuns = sorted(fpuns)
    Parallel(n_jobs=10)(delayed(wrapp2)(pun) for pun in fpuns)
'''    
    
#Parallel(n_jobs=10)(delayed(wrapp)(fjob) for fjob in folders)
'''
for fjob in folders:
    print(fjob)
    os.chdir(fjob)
    fpuns = [fold for fold in os.listdir() if os.path.isdir(fold) and "PUN_" in fold]
    fpuns = sorted(fpuns)
    for pun in fpuns:
      os.chdir(pun)
      get_ensemble_distr(N)
      os.chdir('..')
    os.chdir('..')
'''



