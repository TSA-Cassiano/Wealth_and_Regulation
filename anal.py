import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os
import shutil
from matplotlib.ticker import MaxNLocator
letter_size = 20
axis_size   = 18
tics_size   = 15
plt.rcParams['font.family'] = 'DeJavu Serif'
plt.rcParams['font.serif'] = ['Times New Roman']
plt.rcParams['xtick.labelsize'] = tics_size 
plt.rcParams['ytick.labelsize'] = tics_size 
plt.rcParams['lines.linewidth'] = 2.5
axis_dict={'ecoact':r"""$\alpha_{econ}$""", 'instab' : r"""$\alpha_{inst}$""", 'wmax': r"""$ w_{max}/w_{tot}$""", 'wmin':r"""$ w_{min}/w_{tot}$""",'harmony':r"""$\alpha_{harm}$""",'coop':r"""$\alpha_{coop}$"""}
###############################################
def read_single_run():
  ecoact      = np.loadtxt('ecoact.txt')[:,1]
  harmony     = np.loadtxt('harmony.txt')[:,1]  
  instab      = np.loadtxt('instability.txt')[:,1]  
  wmax        = np.loadtxt('wmax.txt')[:,1]  
  wmin        = np.loadtxt('wmin.txt')[:,1]    
  wmean       = np.loadtxt('w.txt')[:,1]
  coop        = np.loadtxt('cooperation.txt')[:,1]
  its         = np.loadtxt('ecoact.txt')[:,0]
  return its, ecoact, harmony, instab, wmax, wmin, wmean, coop

def print_lastavgs(N_ens,data_dict,outname):
  plt.clf()
  letter_arr = ['(a)','(b)','(c)','(d)','(e)','(f)']
  fig = plt.figure(figsize=(12, 8))
  index = 0
  for dat in data_dict:
      index = index + 1
      plt.subplot(2, 3, index)
      data = data_dict[dat][1]
      mat  = data_dict[dat][2]
      avg = []
      for i in range(N_ens):
          avg.append(mat[-1,i])
      cum_mean = [ np.mean(avg[0:i]) for i in range (1,N_ens) ]
      plt.plot(np.arange(1,N_ens),cum_mean)
      last_mean = np.mean(avg)
      plt.plot([0,N_ens],[last_mean,last_mean])
      plt.xlabel('Ensemble Size',fontsize=axis_size)
      plt.ylabel(axis_dict[dat],fontsize=axis_size)
      plt.xticks([1,125,250])
      plt.text(-0.2, 1.2, letter_arr[index-1], transform=plt.gca().transAxes, fontsize=letter_size, va='top', ha='right')
  plt.tight_layout()
  plt.savefig(outname)
  plt.close()  
def print_avgs(N_ens,data_dict,outname):
  plt.clf()
  letter_arr = ['(a)','(b)','(c)','(d)','(e)','(f)']
  fig = plt.figure(figsize=(12, 8))
  index = 0
  for dat in data_dict:
      index = index + 1
      plt.subplot(2, 3, index)
      data = data_dict[dat][1]
      mat  = data_dict[dat][2]
      avg = []
      for i in range(1,N_ens):
          avg.append(np.mean(data[0:i]))
      plt.plot(np.arange(1,N_ens),avg)
      last_mean = np.mean(data)
      plt.plot([0,N_ens],[last_mean,last_mean])
      plt.xlabel('Ensemble Size',fontsize=axis_size)
      plt.xticks([1,125,250])
      plt.ylabel(axis_dict[dat],fontsize=axis_size)
      #plt.gca().yaxis.set_major_locator(MaxNLocator(nbins=6))
      plt.text(-0.2, 1.2, letter_arr[index-1], transform=plt.gca().transAxes, fontsize=letter_size, va='top', ha='right')
  plt.tight_layout()
  plt.savefig(outname)
  plt.close()
def print_tavgs(N_ens,data_dict,outname,its):
  plt.clf()
  letter_arr = ['(a)','(b)','(c)','(d)','(e)','(f)']
  fig = plt.figure(figsize=(12, 8))
  index = 0
  for dat in data_dict:
      index = index + 1
      plt.subplot(2, 3, index)
      data = data_dict[dat][0]
      mat  = data_dict[dat][2]
      for i in range(1,N_ens,int(N_ens/3)):
        slice_vec = mat[:,0:i]
        plt.plot(its,np.mean(slice_vec, axis=1),label=str(i))
        #plt.legend()
      plt.plot(its,data,label=str(N_ens))
      plt.xlabel('Iterations',fontsize=axis_size)
      plt.ylabel(axis_dict[dat],fontsize=axis_size)
      plt.text(-0.2, 1.2, letter_arr[index-1], transform=plt.gca().transAxes, fontsize=letter_size, va='top', ha='right')
  plt.legend()
  plt.tight_layout()
  plt.savefig(outname)
  plt.close()
def ensemble_anal():
    jobs = [fold for fold in os.listdir() if os.path.isdir(fold) and "DIST" not in fold]
    jobs = sorted(jobs,key=lambda x: float(x))  
    N_ens = len(jobs)
    mat_ecoact  = np.zeros([N,N_ens])
    mat_harmony = np.zeros([N,N_ens])
    mat_instab  = np.zeros([N,N_ens])
    mat_wmax    = np.zeros([N,N_ens])
    mat_wmin    = np.zeros([N,N_ens])
    mat_wmean   = np.zeros([N,N_ens])
    mat_coop    = np.zeros([N,N_ens])
    for i in range(N_ens):
      job = jobs[i]
      os.chdir(job)
      #os.system('pwd')
      its, ecoact, harmony, instab, wmax, wmin, wmean, coop = read_single_run()
      mat_ecoact[:,i]  = ecoact 
      mat_harmony[:,i] = harmony
      mat_instab[:,i]  = instab
      mat_wmax[:,i]    = wmax
      mat_wmin[:,i]    = wmin
      mat_wmean[:,i]   = wmean
      mat_coop[:,i]    = coop  
      os.chdir('../')
      
    ecoact_eavg  = np.mean(mat_ecoact, axis=1)
    instab_eavg  = np.mean(mat_instab, axis=1)
    wmax_eavg  = np.mean(mat_wmax, axis=1)
    wmin_eavg  = np.mean(mat_wmin, axis=1)
    wmean_eavg  = np.mean(mat_wmean, axis=1)
    coop_eavg  = np.mean(mat_coop, axis=1)
    harmony_eavg  = np.mean(mat_harmony, axis=1)
    
    ecoact_tavg  = np.mean(mat_ecoact, axis=0)
    instab_tavg  = np.mean(mat_instab, axis=0)
    wmax_tavg    = np.mean(mat_wmax, axis=0)
    wmin_tavg    = np.mean(mat_wmin, axis=0)
    wmean_tavg   = np.mean(mat_wmean, axis=0)
    coop_tavg    = np.mean(mat_coop, axis=0)
    harmony_tavg = np.mean(mat_harmony, axis=0)
  
    data_dict = {'ecoact':[ecoact_eavg,ecoact_tavg,mat_ecoact],\
                 'instab':[instab_eavg,instab_tavg,mat_instab],\
                 'wmax':[wmax_eavg,wmax_tavg,mat_wmax],\
                 'wmin':[wmin_eavg,wmin_tavg,mat_wmin],\
                 #'wmean':[wmean_eavg,wmean_tavg,mat_wmean],\
                 'harmony':[harmony_eavg,harmony_tavg,mat_harmony],\
                 'coop':[coop_eavg,coop_tavg,mat_coop],}
    
    print_avgs(N_ens,data_dict,'ensembleavgs.pdf')      
    print_tavgs(N_ens,data_dict,'timeavgs.pdf',its)
    print_lastavgs(N_ens,data_dict,'latspoints.pdf')
    ecoact_lp  = ecoact_eavg[-1]
    instab_lp  = instab_eavg[-1]
    wmax_lp    = wmax_eavg[-1]
    wmin_lp    = wmin_eavg[-1]
    wmean_lp   = wmean_eavg[-1]
    coop_lp    = coop_eavg[-1]
    harmony_lp = harmony_eavg[-1]

    lplist = [ecoact_lp, instab_lp, wmax_lp, wmin_lp, wmean_lp, coop_lp, harmony_lp]
    return lplist

###########################################################

N=101 #number of points written during dynamics
folders = [fold for fold in os.listdir() if os.path.isdir(fold) and "JOB_" in fold]
folders = sorted(folders)
#folders = sorted(folders,key=lambda x: float(x.split('_')[-1]))
#folders = ["JOB_25_5000_10_2_1_0.1"]
for fjob in folders:
    os.chdir(fjob)
    
    fpuns = [fold for fold in os.listdir() if os.path.isdir(fold) and "PUN_" in fold]
    fpuns = sorted(fpuns)
    data = []
    ppun_unique=np.unique([float(x.split('_')[1]) for x in fpuns])
    fpun_unique=np.unique([float(x.split('_')[2]) for x in fpuns])
    i_ref = 0
   
    #ppun_unique = [ppun_unique[0]]
    #fpun_unique = [fpun_unique[0]]
    
    
    #'''
    hmname="hm.txt"
    os.system('pwd')
    with open(hmname,'w') as f:
        for i in range(len(ppun_unique)):
            for j in range(len(fpun_unique)):
                ppun=ppun_unique[i]
                fpun=fpun_unique[j]
                dirname="PUN_"+str(ppun)+"_"+str(fpun)
                os.chdir(dirname)
                os.system('pwd')
                lplist  = ensemble_anal()
                linedat = [str(round(x,5)) for x in [ppun,fpun]+lplist]
                linedat = ' '.join(linedat)+'\n'
                os.chdir('..')
                if i_ref != i:
                    i_ref = i
                    f.write('\n')
                f.write(linedat) 
                
    #'''
    '''
    for i in range(len(ppun_unique)):
        for j in range(len(fpun_unique)):
            ppun=ppun_unique[i]
            fpun=fpun_unique[j]
            dirname="PUN_"+str(ppun)+"_"+str(fpun)
            os.chdir(dirname)
            os.system('pwd')
            lplist = ensemble_anal()
            os.chdir('..')
    '''
    os.chdir('..')

