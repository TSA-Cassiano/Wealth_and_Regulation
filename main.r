# Reproduction of the simulation presented in
# "Wealth redistribution in our small world" (Iglesias et al., 2003, Physica A)
# with additional binary variables representing type of economic activity
# and the legality or repression of it.

########################################
# steps to run this script in ubuntu:
# (1) sudo apt install r-base r-base-dev -y
# (2) in the terminal, enter "R", then:
#     a) install.packages("igraph") 
#     answer: yes yes...
#     b) install.packages("stringr")
#     c) install.packages("minpack.lm")
#########################################

# Graph theory library
# https://igraph.org/r/
library(igraph)

# A consistent, simple and easy to use set of wrappers around the fantastic 'stringi' package
# https://www.rdocumentation.org/packages/stringr/versions/1.4.0
library(stringr)

# Provides the nlsLM function, which works better than nls
# https://cran.r-project.org/web/packages/minpack.lm/index.html
library(minpack.lm)



#########################################
# WR binary simulation results analysis:
system("rm *.txt")
system("rm -r WDIST")
system("mkdir WDIST")

wr_analysis <- function(x_str, y_str, aa0, x0, dx0, lowerlimits, upperlimits)
{

erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

x = as.numeric(unlist(str_extract_all(x_str, "[\\.0-9e-]+")))
y = as.numeric(unlist(str_extract_all(y_str, "[\\.0-9e-]+")))
    
#cat("\n")
#cat("\n In wr_analysis:")
#cat("\n x = ",x)
#cat("\n y = ",y)
    
plot(x,y,type="h",xlim=c(0,1))

hdataframe = data.frame(x = x, y = y)

# Estimate some starting values:

aa.0 = aa0
dx.0 = dx0
x0.0 = x0

#cat("\n")
#cat("\n Initial parameters for curve fitting:")
#cat("\n aa = ",aa.0,"\n dx = ",dx.0,"\n x0 = ",x0.0)

# Plot initial fit:
xfit.0 = seq(from=0.0, to=1.0, length.out=100)
yfit.0 = aa.0 * dlnorm((xfit.0-x0.0)/dx.0)
    
lines(xfit.0, yfit.0, col="green", lwd=3)

# Define limits:
    
lowerlimits = c(0.1*aa.0, 0.1*dx.0, 0.1*x0.0)
upperlimits = c(5.0*aa.0, 5.0*dx.0, 5.0*x0.0)
    
# Do the fit:

#try
#(

    fit <- nlsLM( y ~ aa * dlnorm((x-x0)/dx), data = hdataframe,
             start = list(aa = aa.0, dx = dx.0, x0 = x0.0),
             lower = lowerlimits,
             upper = upperlimits,
             alg = "plinear")
        
    xfit.1 = xfit.0
    yfit.1 = predict(fit, data.frame(x=xfit.0, y=yfit.0))
    
    lines(xfit.1, yfit.1, col="blue", lwd=2)

    flush.console()

    aa = coef(fit)[[1]]
    dx = coef(fit)[[2]]
    x0 = coef(fit)[[3]]


    #cat("\n Still in wr_analysis:")
    #cat("\n xfit.1 = ",xfit.1)
    #cat("\n yfit.1 = ",yfit.1)
    #cat("\n")
    #cat("\n Final adjusted parameters:")
    #cat("\n aa = ",aa,"\n x0 = ",x0,"\n dx = ",dx)
    #cat("\n")
    
#)

par(mar=c(5,6,4,1)+.1) # margins

plot(x, y, type="h", col="#00AAFF", lend=1, xlim=c(0,1), lwd=20, xlab="Wealth", ylab="Number of agents", cex=5, cex.lab=2, cex.axis=2)

f_sol = function(x) ( aa * dlnorm((x-x0)/dx) )  # divided by y_sum to normalize number of agents to 1
curve(f_sol, 0, 1, type="l", col="black", lwd=5, add=TRUE)


flush.console()
}
###########################################

# MAIN

# WR binary simulation:

wr_binary_simulation <- function(n_agents, n_iter, n_bits, k, tol_n_irreg_bits, p_pun, f_pun, even_redist, wealth_prod, wealth_prod_per_con_per_iter, hist_breaks, debug, stop)
{
    # Printing input parameters:
    input_file     <- 'input.txt'
    file.create(input_file, showWarnings = TRUE)
    cat("\n Simulation initiated with the following parameters:",file=input_file,append=TRUE)
    cat("\n Number of agents: n_agents = ",n_agents,file=input_file,append=TRUE)
    cat("\n Number of iterations: n_iter = ",n_iter,file=input_file,append=TRUE)
    cat("\n Number of bits of activity representation: n_bits = ",n_bits,file=input_file,append=TRUE)
    cat("\n Connection range (half the vertex degree) k = ",k,file=input_file,append=TRUE)
    cat("\n Tolerance of number of irregular bits: tol_n_irreg_bits = ", tol_n_irreg_bits,file=input_file,append=TRUE)
    cat("\n Punishment probability: p_pun = ",p_pun,file=input_file,append=TRUE)
    cat("\n Punishment force: f_pun = ",f_pun,file=input_file,append=TRUE)
    cat("\n Even wealth redistribution: even_redist = ",even_redist,file=input_file,append=TRUE)
    cat("\n Wealth production: wealth_prod = ", wealth_prod,file=input_file,append=TRUE)
    cat("\n Wealth produced per connection per iteration: wealth_prod_per_con_per_iter = ", wealth_prod_per_con_per_iter,file=input_file,append=TRUE)
    cat("\n Number of histogram bars: hist_breaks = ",hist_breaks,file=input_file,append=TRUE)
    cat("\n Program debug information: debug = ",debug,file=input_file,append=TRUE)
    cat("\n Program stops: stop = ",stop,file=input_file,append=TRUE)
    cat("\n",file=input_file,append=TRUE)
    
    # Variable definition:
    
    w = vector(length = n_agents) # wealth of the agents
    u = matrix(sample(c(-1, 1), n_agents*n_bits, replace=TRUE), nrow = n_agents, ncol = n_bits) # activity orientation of the agents
    varmax_n_bits = vector(length = n_agents) # maximum u variation in bits
    A = matrix(0, nrow = n_agents, ncol = n_agents) # actual connections (adjacency matrix)
    P_con = matrix(0, nrow = n_agents, ncol = n_agents) # probability of connection
    
    n_neighbors = vector(length = n_agents) # number of neighbors of the agents
    total_n_neighbors = 0 # number of neighbors
    
    # Variable initiation:
    
    w = runif(n_agents, 0.0, 1.0) # generates n numbers between 0.0 and 1.0
    # u already initiated
    # A already initiated with zeros
    varmax_n_bits = replicate(n_agents, trunc(0.25*n_bits))
    # P_con already initiated with zeros
    d_varmax_n_bits = replicate(n_agents, trunc(0.1*n_bits))
       
    if (debug)
    {
        cat("\n u: ")
        print(u)
        cat("\n varmax_n_bits: ", varmax_n_bits)
        flush.console()
    }
    
    # Creating initial network:
    cat("\n Creating initial network...")
    
    ## Connecting each agent to its 2k closest neighbors:

    for (i in 1:n_agents-1)
    {
        for (j in (i-k):(i+k))
        {
            # Cyclic boundary conditions:
            
            j1 = j
            if(j1 < 1){j1 = j + n_agents}
            if(j1 > n_agents){j1 = j - n_agents}
            
            # Connecting i to the neighbors:
            
            # cat(i, j1, "\n") # debug
            if(i != j1)
            {
                A[i,j1] = 1
                A[j1,i] = 1
            }
            
        }
    }
    
    if(debug)
    {
        cat("\n")
        print(A)
        cat("\n")
    }
    
    ## Creating a graph from adjacency matrix A:

    A_graph = graph_from_adjacency_matrix(A, mode = "undirected")
    
    plot(A_graph, main = "agent graph before simulation")
    
    ## Creating sequence of vertices of the graph:

    v_A_graph = V(A_graph)
        
    # Creating files before main loop:
    
    file.create("varmax_n_bits-vs-iter.txt", showWarnings = TRUE)
   
    # Creating out files
    outindex <- 'index.txt'
    outwmin  <- 'wmin.txt'
    outwmax  <- 'wmax.txt'
    outinst  <- 'instability.txt'
    outcoact <- 'ecoact.txt'
    outw     <- 'w.txt'
    outcoop  <- 'cooperation.txt'
    outharm  <- 'harmony.txt'
    tots     <- 'totsum.txt'
    file.create(outwmin, showWarnings = TRUE)
    file.create(outwmax, showWarnings = TRUE)
    file.create(outinst, showWarnings = TRUE)
    file.create(outcoact, showWarnings = TRUE)
    file.create(outw, showWarnings = TRUE)
    file.create(outcoop, showWarnings = TRUE)
    file.create(outharm, showWarnings = TRUE)
    file.create(tots, showWarnings = TRUE)    
    file.create(outindex, showWarnings = TRUE)

    # MAIN LOOP:
    
    cat("\n Performing iterations...")
    cat("\n")
    
    for (iter in (1:n_iter))
    {
        if (debug)
        {
            cat("\n")
            cat("\n sum(w) = ", sum(w))
        }
        
        # Printing progress information:
        
        if ( ( iter %% (n_iter/10) ) == 0 )
        {
            cat("\n")
            cat("\n Now on iteration ",iter," of ",n_iter,".")
            cat("\n")
            flush.console()
        }

        # Generating wealth proportional to each agent's number of connections:
        
        if (wealth_prod)
        {
            for (i_agent in (1:n_agents))
            {
                n_neighbors_iagent = neighbors(A_graph, i_agent)
                
                if ( is.numeric(n_neighbors_iagent[[1]]) )
                {
                    w[i_agent] = w[i_agent] + 1.0*wealth_prod_per_con_per_iter*n_neighbors_iagent[[1]]
                }
                else
                {
                    cat("\n Variable with invalid value:")
                    cat("\n n_neighbors_iagent[[1]] = ", n_neighbors_iagent[[1]])
                    quit()
                }
            }
        }                  
        # Initialization of total loss of society wealth due to punishments:
        
        dw_pun = 0.0
               
        # Punishment of agents that do not conform to the rules of society:
        
        for (i_agent in (1:n_agents))
        {        
            # Counting number of irregular bits of the agent:
            
            n_irreg_bits = 0
            
            for (i_bit in (1:n_bits))
            {
                if (u[i_agent,i_bit] == -1)
                {
                    n_irreg_bits = n_irreg_bits + 1
                }
            }

            # Drawing a random number to define if the agent will be punished:
            
            rand1 = runif(1, 0.0, 1.0)
            
            # Checking if agent "i_agent" deserves to be punished,
            # based on nonconformity and chance:
            
            if( (n_irreg_bits > tol_n_irreg_bits) & (rand1 <= p_pun)  )
            {
                # Defining punishment of the agent:
                
                #w_pun_iagent = w[i_agent]*f_pun
                w_pun_iagent = abs(w[i_agent])*f_pun

		            if (w[i_agent]<0) # gov chooses to not beat the poor agent
	              {
		            w_pun_iagent = 0.0
		            }		
                # Performing the punishment:
                
                w[i_agent] = w[i_agent] - w_pun_iagent
                              
                # Incrementing the total loss of society wealth due to punishments:

                dw_pun = dw_pun +  w_pun_iagent

                if (debug)
                {
                    cat("\n")
                    cat("\n i_agent, dw_pun ", i_agent, dw_pun)
                }
                
                # Changing maximum orientation variation accordingly:
                
                varmax_n_bits[i_agent] = varmax_n_bits[i_agent]*(1.0+f_pun)
                
                if (debug)
                {
                    cat("\n")
                    cat("\n f_pun ", f_pun)
                    cat("\n i_agent ", i_agent)
                    cat("\n varmax_n_bits[i_agent] ", varmax_n_bits[i_agent])
                    cat("\n n_bits ", n_bits)
                    cat("\n varmax_n_bits[i_agent] > n_bits ", varmax_n_bits[i_agent] > n_bits)
                    cat("\n")
                }
                
                ## Applying boundary conditions:

                if ( varmax_n_bits[i_agent] > n_bits )
                {
                        varmax_n_bits[i_agent] = n_bits
                }
                else
                {
                    if ( varmax_n_bits[i_agent] < 1 )
                    {
                        varmax_n_bits[i_agent] = 1
                    }
                }
                
                # Redefining a number varmax_n_bits of bits of the activity of each agent
                # (the redefinition is random and some bits will get the same value as before;
                # also, the same agent may be changed more than once):
                
                for (i_varied_bit in 1:varmax_n_bits[i_agent])
                {
                    irand = ceiling(runif(1,0,n_bits))
                    u[i_agent,irand] = sample(c(-1,1), 1, replace=TRUE)
                }
                
            } #if(rand1 < p_pun)

        } #for (i_agent in (1:n))
        
        # Redistributing the value raised in punishments:

        if(even_redist)
        {   
            # Even redistribution:
            
            for (i_agent in (1:n_agents))
            {
                if ( is.numeric(n_agents) && is.numeric(dw_pun) )
                {
                    if (debug)
                    {
                        cat("\n")
                        cat("\n i_agent ", i_agent)
                        cat("\n w[i_agent] antes: ", w[i_agent])
                    }
                    
                    w[i_agent] = w[i_agent] + dw_pun/n_agents
                    
                    if (debug)
                    {
                        cat("\n w[i_agent] depois: ", w[i_agent])
                        cat("\n")
                        cat("\n n_agents, dw_pun/n_agents ", n_agents, dw_pun/n_agents)
                    }
                    
                    #if (stop)
                    #{
                    #    invisible(readline(prompt="Press any key to continue"))
                    #}

                }
                else
                {
                    cat("\n Invalid variable value:")
                    cat("\n n_agents = ", n_agents)
                    cat("\n dw_pun = ", dw_pun)
                    quit()
                }
            }
        }
        else
        {
            # Redistribution proportional to each agent number of connections:

            total_n_neighbors = 0
            for (i_agent in (1:n_agents))
            {
                n_neighbors[[i_agent]] = length(neighbors(A_graph, i_agent))
                total_n_neighbors = total_n_neighbors + n_neighbors[[i_agent]]
            }
            
            for (i_agent in (1:n_agents))
            {                
                if ( is.numeric(n_neighbors[[i_agent]]) && is.numeric(total_n_neighbors) && is.numeric(dw_pun) )
                {
                    w[[i_agent]] = w[[i_agent]] + dw_pun*(n_neighbors[[i_agent]]/total_n_neighbors)
                }
                else
                {
                    cat("\n Invalid variable value:")
                    cat("\n n_neighbors_iagent[[1]] = ", n_neighbors[[i_agent]])
                    cat("\n gsize(A_graph) = ", gsize(A_graph))
                    cat("\n dw_pun = ", dw_pun)
                    # invisible(readline(prompt="Press any key to continue"))
                    quit()
                }
            }
        }
        
        # Printing iteration number:
        if (debug)
        {
            cat("\n")
            cat("\n Iteration ",iter)
            cat("\n")
            flush.console()
        }
        
        # Agent with minimum wealth will take an action...
        # Let's identify this agent:
        
        w_min = min(w) # minimum wealth
        i_w_min = which.min(w) # index of minimum wealth agent

        # Reporting:
        
        if (debug)
        {
            cat("\n w_min = ", w_min)
            cat("\n i_w_min = ", i_w_min)
            flush.console()
        }
        
        if (stop) {invisible(readline(prompt="\n Press any key to continue."))}
        
        # Calculating maximum wealth:
        
        w_max = max(w)
        
        
        # Changing the wealth of mininum wealth agent:
        
        if (wealth_prod)
        {
            w_new = runif(1, 0.0, w_max)
        }
        else
        {
            w_new = runif(1, 0.0, 1.0) # new random wealth
        }
        
        d_w = w_new - w_min # wealth variation of agent i_w_min
        
        # Calculating mean wealth:
        
        w_mean = mean(w)
        
        # Adjusting maximum variation of economic orientation:
               
        if ( w_min < 0.1*abs(w_mean) )
        {
            varmax_n_bits[i_w_min] = n_bits
        }
        else
        {
            varmax_n_bits[i_w_min] = varmax_n_bits[i_w_min] - ceiling(n_bits*d_w/w_min) # /n_iter
            # cat("\n ceiling(n_bits*d_w/w_min) = ",ceiling(n_bits*d_w/w_min))
            # invisible(readline(prompt="\n Press any key to continue."))
        }
        
        # Applying boundary conditions:
        
        if (debug)
        {
            cat("\n i_w_min = ", i_w_min)
            cat("\n w_min = ", w_min)
            cat("\n n_bits = ", n_bits)
            cat("\n varmax_n_bits[i_w_min] = ", varmax_n_bits[i_w_min])
            cat("\n varmax_n_bits[i_w_min] > n_bits = ", varmax_n_bits[i_w_min] > n_bits)
            cat("\n")
            cat("\n varmax_n_bits = ", varmax_n_bits)
            cat("\n")
        }
        
        if ( varmax_n_bits[i_w_min] > n_bits )
        {
            varmax_n_bits[i_w_min] = n_bits
        }
        else
        {
            if ( varmax_n_bits[i_w_min] < 1 )
            {
                varmax_n_bits[i_w_min] = 1
            }
        }
        
        # Actually changing the wealth of agent i_w_min:
        
        w[[i_w_min]] = w_new

        # Reporting:
        
        if (debug)
        {
            cat("\n w_new = ", w_new)
            cat("\n d_w = ", d_w)
            flush.console()
        }
        
        # Simplification of the graph to avoid multiple connections and loops:
        
        A_graph <- A_graph %>% simplify(remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = igraph_opt("edge.attr.comb"))

        # The wealth variation of agent i_w_min will be compensated by its neighbors:
        
        neighbors_i_w_min = neighbors(A_graph, i_w_min) # neighbors of agent i_w_min
        n_neigh = length(neighbors_i_w_min) # number of these neighbors
        
        if( is.numeric(n_neigh) & n_neigh > 0 )
        {
            d_w_neighbors = -d_w/n_neigh # variation of wealth of each neighbor
        }
        else
        {
            # If i_w_min is not connected, it won't be able to get w_new:
            w[[i_w_min]] = w_min
        }
        
        # Reporting:
        
        if (debug)
        {
            cat("\n neighbors_i_w_min = ")
            print(neighbors_i_w_min)
            cat("\n")
            cat("\n n_neighbors[[1]] = ", n_neighbors[[1]])
            cat("\n d_w_neighbors = ", d_w_neighbors)
            flush.console()
        }
                
        if (stop)
        {
            invisible(readline(prompt="\n Press any key to continue."))
        }
        # Actually changing the wealth of the neighbors:   
        for (i_neighbor in neighbors_i_w_min)
        {
            # Reporting:
            
            if (debug)
            {
                cat("\n Wealth of agent ",i_neighbor," before:")
                cat("\n",w[i_neighbor])
                flush.console()
            }
            
            # Changing neighbor i wealth:
            
            w[i_neighbor] = w[i_neighbor] + d_w_neighbors
            
            # Reporting:
            
            if (debug)
            {
                cat("\n Wealth of agent ",i_neighbor," after:")
                cat("\n",w[i_neighbor])
                flush.console()
            }

            if (stop)
            {
                invisible(readline(prompt="\n Press any key to continue."))
            }
            
        }
  
        # aqui unir laços
        for (i_neighbor in neighbors_i_w_min)
        {     
        
            # Changing maximum number of varied bits accordingly:

            if (debug)
            {
                cat("\n i_neighbor = ", i_neighbor)
                cat("\n w[i_neighbor] = ", w[i_neighbor])
                cat("\n abs(w_mean) = ", abs(w_mean))
                cat("\n w[i_neighbor] < 0.1*abs(w_mean) = ", w[i_neighbor] < 0.1*abs(w_mean))
                cat("\n")
            }
            
            if ( w[i_neighbor] < 0.1*abs(w_mean) )
            {
                varmax_n_bits[i_neighbor] = n_bits
            }
            else
            {
                if ( is.numeric(d_w_neighbors) && is.numeric(w[i_neighbor]) )
                {
                    d_varmax_n_bits[i_neighbor] = - ceiling(n_bits*d_w_neighbors/w[i_neighbor]) # /n_iter
                    
                    if ( abs(d_varmax_n_bits[i_neighbor]) > n_bits )
                    {
                        d_varmax_n_bits[i_neighbor] = sign(d_varmax_n_bits[i_neighbor])*n_bits
                    }
                    
                    varmax_n_bits[i_neighbor] = varmax_n_bits[i_neighbor] + d_varmax_n_bits[i_neighbor]
                }
            }
            
            # Applying boundary conditions in varmax_n_bits:
                       
            if ( varmax_n_bits[i_neighbor] > n_bits )
            {
                varmax_n_bits[i_neighbor] = n_bits
            }
            else
            {
                if ( varmax_n_bits[i_neighbor] < 1 )
                {
                    varmax_n_bits[i_neighbor] = 1
                }
            }
            
            
            # Changing the economic activity quality of the agents:
            for (i_varied_bit in 1:varmax_n_bits[i_neighbor])
            {
                irand = ceiling(runif(1,0,n_bits))
                u[i_neighbor,irand] = sample(c(-1,1), 1, replace=TRUE)
            }
                        
            if (debug)
            {
                cat("\n i_neighbor, u[i_neighbor,] = ", i_neighbor, u[i_neighbor,],"\n")
            }
                        
            # Calculating probability of connection between i_w_min and i_neighbor:
                      
            P_con[i_w_min,i_neighbor] = (((u[i_w_min,]%*%u[i_neighbor,])/n_bits)+1)/2
            P_con[i_neighbor,i_w_min] = P_con[i_w_min,i_neighbor]
                        
            if (debug)
            {
                cat("\n u[i_w_min,] = ",u[i_w_min,])
                cat("\n u[i_neighbor,] = ",u[i_neighbor,])
                cat("\n u[i_w_min,]%*%u[i_neighbor,] = ", u[i_w_min,]%*%u[i_neighbor,])
                cat("\n n_bits = ",n_bits)
                cat("\n (((u[i_w_min,]%*%u[i_neighbor,])/n_bits)+1)/2 = ", (((u[i_w_min,]%*%u[i_neighbor,])/n_bits)+1)/2)
                cat("\n P_con[[i_w_min,i_neighbor]] = ",P_con[i_w_min,i_neighbor])
                cat("\n P_con[[i_neighbor,i_w_min]] = ",P_con[i_neighbor,i_w_min])
                cat("\n")
            }
            
            # Generating random number between 0 and 1:
            
            ran_con = runif(1, 0.0, 1.0)

            if (debug)
            {
                cat("\n ran_con = ",ran_con)
                cat("\n i_neighbor, i_w_min = ",i_neighbor,i_w_min)
                cat("\n P_con[i_neighbor,i_w_min] = ",P_con[i_neighbor,i_w_min])
                cat("\n (ran_con > P_con[i_neighbor,i_w_min])",(ran_con > P_con[i_neighbor,i_w_min]))
                flush.console()
            }
            
            if(stop)
            {
                invisible(readline(prompt="\n Press any key to continue."))
            }            
            
            # If the probability is less than the random number,
            # the connection between i_w_min and i_neighbor is broken
            # and they form new connections with random agents:
            
            if (P_con[[i_neighbor,i_w_min]] < ran_con)
            {
                # Breaking the connection between i_w_min and i_neighbor (in both direction if existing): 
                A_graph = A_graph - E(A_graph, P = c(i_w_min,i_neighbor, i_neighbor,i_w_min))
                
                # Picking two random agents:
                i_ran_1 <- floor(runif(1, 1, n_agents+1))
                i_ran_2 <- floor(runif(1, 1, n_agents+1))
                
                # Reporting:
                if (debug)
                {
                    cat("\n E(A_graph) = ",E(A_graph))
                    cat("\n i_w_min,i_ran_1, i_ran_1,i_w_min, i_ran_2,i_neighbor, i_neighbor,i_ran_2 =",
                        i_w_min,i_ran_1, i_ran_1,i_w_min, i_ran_2,i_neighbor, i_neighbor,i_ran_2)
                    flush.console()
                }
                
                # Forming the new connections (indices must be read in pairs):
                A_graph = A_graph %>% add_edges(c(i_w_min,i_ran_1, i_ran_1,i_w_min, i_ran_2,i_neighbor, i_neighbor,i_ran_2))
            }
        } #for (i_neighbor in neighbors_i_w_min)
        
        A_graph <- A_graph %>% simplify(remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = igraph_opt("edge.attr.comb"))
        
        
        # Printing the evolution of some quantities:
        
        cat(iter," ",mean(varmax_n_bits),file="varmax_n_bits-vs-iter.txt",append=TRUE)
    
        ###################################################
	# Printing dynamics
	
	if ( ( iter %% (n_iter/100)) == 0  | iter == 1 | iter == n_iter )
        {
	index_inst = mean(abs(varmax_n_bits))
	index_inst = index_inst/n_bits
	w_max = max(w)
	w_min = min(w)
	totsum      <- sum(w)
	
	index_econAct = vertex_connectivity(A_graph)

	index_wealth = mean(w)

	index_coop = 0

	for ( i_agent in 1:(gorder(A_graph)-1) )
	{
	    for ( j_agent in (i_agent+1):gorder(A_graph) )
	    {
		if (debug)
		{
		    cat("n_agents, i_agent, j_agent : ", n_agents, i_agent, j_agent)
		}

		if ( are.connected(A_graph, i_agent, j_agent) )
		{
		    index_coop = index_coop + 1/(gorder(A_graph)*(gorder(A_graph)-1)/2)
		}
	    }
	}
	index_harmony = 0.0
	count_index_harmony = 0
	
  agent_connect = numeric(n_agents)
	for ( i_agent in 1:(gorder(A_graph)-1) )
	{
	    for ( j_agent in (i_agent+1):gorder(A_graph) )
	    {
		if ( are.connected(A_graph, i_agent, j_agent) )
		{
		    index_harmony = index_harmony + u[i_agent,] %*% u[j_agent,] # %*% means the dot product
		    count_index_harmony = count_index_harmony + 1
		    agent_connect[i_agent] <- agent_connect[i_agent] +1
		}
	    }
	}
	
	index_harmony = index_harmony/count_index_harmony   

	cat(iter," ",w_min/totsum,"\n",file=outwmin,append=TRUE)
	cat(iter," ",w_max/totsum,"\n",file=outwmax,append=TRUE)
	cat(iter," ",index_inst,"\n",file=outinst,append=TRUE)
	cat(iter," ",index_econAct,"\n",file=outcoact,append=TRUE)
	cat(iter," ",index_wealth,"\n",file=outw,append=TRUE)    
	cat(iter," ",index_coop,"\n",file=outcoop,append=TRUE)
	cat(iter," ",index_harmony,"\n",file=outharm,append=TRUE)
	cat(iter," ",totsum,"\n",file=tots,append=TRUE)
	
	
	###### PRINTING HIST ##############

    #dumping hist
    wfile <- paste("WDIST/w_",toString(iter),".txt")
    file.create(wfile, showWarnings = TRUE)
    write.table(w, file = wfile, sep = "\t", row.names = FALSE, col.names = FALSE)
    
    wfile <- paste("WDIST/connect_",toString(iter),".txt")
    file.create(wfile, showWarnings = TRUE)
    write.table(agent_connect, file = wfile, sep = "\t", row.names = FALSE, col.names = FALSE)    
    
	##################################
	} # if ( ( iter %% (n_iter/100)) == 0  | iter == 1)

    } # for (iter in (1:n_iter))

    # Plots:
    plot(A_graph, main = "agent graph after simulation")

    flush.console()
    
    cat("\n")
    cat("\n Wealth of each agent:")
    
    w_max = max(w)
    
    if (wealth_prod)
    {
        plot(w, ylim=c(0,w_max))
    }
    else
    {
        plot(w, ylim=c(0,1))
    }
    
    flush.console()
    
    cat("\n")
    cat("\n Agent wealth histogram:")
    
    w_max = max(w)
    
    if (wealth_prod)
    {
        hdata <- hist(w, breaks=hist_breaks, xlim=c(0,w_max), col="#00AAFF", xlab="Wealth", ylab="Number of agents")
    }
    else
    {
        hdata <- hist(w, breaks=hist_breaks, xlim=c(0,1), col="#00AAFF", xlab="Wealth", ylab="Number of agents")
    }
    
    flush.console()
    
    cat("\n")
    cat("\n Histogram data frame:")
    
    hdataframe = data.frame(x = hdata$mids, y = hdata$counts)
    print(hdataframe)

    x = hdata$mids
    y = hdata$counts
    
    #cat("\n x = ",x)
    #cat("\n")
    #cat("\n y = ",y)
    #cat("\n")

    aa0 = 80   # max(y)
    x0 = 0.16   # x[which.max(y)] # (max(x)-min(x))/2.0
    dx0 = 0.26   # 3.0*max(x) # 2.0*(max(x)-min(x)) 

    x_str = toString(hdataframe$x)
    y_str = toString(hdataframe$y)

    wr_analysis(x_str, y_str, aa0, x0, dx0)
    
}

# Standard parameter values:
IN_n = XXX1 # 500 #agent number
IN_n_iter = XXX2 # iterations
IN_n_bits = XXX3 # number of bits (size of the vector)
IN_k = XXX4 #initial lattice degree (num of initial neighs)
IN_tol_n_irreg_bits = XXX5 #num of tolerated irregular bits
IN_p_pun = XXX6 #prob of punish
IN_f_pun = XXX7 # weaths proportion punish
IN_even_redist = TRUE # uniform redistribution
IN_wealth_prod = FALSE # weath production during simul
IN_wealth_prod_per_con_per_iter = XXX8 #rate of weath produced 
IN_hist_breaks = 20 # n bins
IN_debug = FALSE #verbosity
IN_stop = FALSE





# Current working directory:
cwd <- getwd()


wr_binary_simulation(IN_n, IN_n_iter, IN_n_bits, IN_k, IN_tol_n_irreg_bits, IN_p_pun, IN_f_pun, IN_even_redist, IN_wealth_prod, IN_wealth_prod_per_con_per_iter, IN_hist_breaks, IN_debug, IN_stop)
