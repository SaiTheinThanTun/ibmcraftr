#' Run state_trans function over a given number of timesteps.
#'
#' Organize population data and transition parameters to run state_trans function over the given number of timesteps.
#'
#' @param states A numeric vector with each element representing the number of individuals in a particular state
#'     its index corresponds to.
#' @param shuffle A logical value to enable shuffling of the individuals (rows) in the resulting matrix.
#' @return A matrix of 0s, and 1s. The rows representing the individuals and the columns representing the states
#'     the individuals are in
#' @examples
#' syn_pop(c(3,2,1))
#' syn_pop(c(0,0,1,5), shuffle=TRUE)
#'
#' @export
#' 


source("D:\\Dropbox\\IBM project_Sai\\generic functions\\syn_pop.R")
source("D:\\Dropbox\\IBM project_Sai\\generic functions\\state_trans.R")
pop <- syn_pop(c(19,1,0,0)) #synthesizing population
beta <- 2 #effective contact rate
#lambda = beta*sum(pop[,2],pop[,3])/sum(pop) #force of infection

#prameters for transitions
param <- list(
  list(1,2,NA), #transition from state 1 to 2 using FOI lambda
  list(2,3,100), #transition from state 2 to 3, the 3rd term 100 ensures the near 100% transition to the next stage
  list(3,4,100)
)
timesteps <- 10
transient <- c("param[[1]][[3]] <- beta*sum(pop[,2],pop[,3])/sum(pop)")

eval(parse(text=transient))

run_state_trans <- function(timesteps, param, pop, transient){
  Matrix.List <- list() #master matrix list initiazilation, to store the transition values each timestep
  sim.table <- matrix(NA, timesteps, ncol(pop))  #table to record the summaries each time step
  for(i in 1:timesteps){
    for(j in 1:length(param)){
      Matrix.List[[j]] <- state_trans(param[[j]][[1]], param[[j]][[2]], param[[j]][[3]], pop)
    }
    pop <- Reduce('+', Matrix.List) + pop #Population after transition
    
    eval(parse(text=transient))
    sim.table[i,] <- colSums(pop) #getting summaries of the population
  }
  sim.table
}

run_state_trans(timesteps,param,pop,transient)

