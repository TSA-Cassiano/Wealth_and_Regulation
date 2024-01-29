<h1 align="center">Wealth and Regulation: An agent-based simulation code for wealth dynamics with variable regulation policy</h1
  
<p align="center">This code simulates economic dynamics as an agent-based model in a graph formalism. Agents are nodes of a graph while the links connecting them represent economic bond. </p>

The first version was written by Prof. Leonardo Luiz e Castro. I adapted his code for parallel computing and fixed some bugs. Additionally, the current version writes the dynamical evolution of network-based quantities.

## Setup
To run in ubuntu, one has to install some libraries. Here are the steps:

# steps to run this script in ubuntu:
```bash
sudo apt install r-base r-base-dev -y
# in the terminal, enter "R", then:
install.packages("igraph") 
# answer: yes yes...
install.packages("stringr")
install.packages("minpack.lm")
```

# Function of each file:
1. main.r is the main program where the magic happens
2. make_stat.sh is a bash script that creates a set of jobs varying the main.r parameters
3. run_slurm.sh is the main code to submit the jobs created by make_stat.sh in a SLURM-base cluster
4. wrapper.sh is a aux. code to help the submission
5. anal.py and dist.py are data processing scripts to extract information from the simulations


## Authors
---
<table>
  <tr>	  
    <td align="center"><a href="https://github.com/llcastro-unb"><img style="border-radius: 50%;" src="https://avatars.githubusercontent.com/u/27786496?v=4" width="100px;" alt=""/><br /><sub><b>Prof. Leonardo Luiz e Castro</b></sub></a><br />Professor at the  <br> University of Brasília (UnB)<br />
    <td align="center"><a href="https://github.com/TSA-Cassiano"><img style="border-radius: 50%;" src="https://avatars.githubusercontent.com/u/26448170?s=400&u=b0820613fd46515f0cfe1806f7251a414d4c249b&v=4" width="100px;" alt=""/><br /><sub><b>MSc Tiago de Sousa Araújo Cassiano</b></sub></a><br />Phd student at the  <br> University of Brasília (UnB)<br />
  </tr>
</table>
