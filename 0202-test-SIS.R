
#' title: "Run step_deterministic_SIS function"
#' author: "Juma Minya"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'
#'#' File: 0202-test-SIS.r
#' ========================
#' 
#' 
#' Objective: running the SIS model with variable time steps 
#' and plot the SIS model graphs to see the effect of time step.
#' 
#Loading Packages
library(RPiR)
#' 
# Read the function
source("0201-step-SIS.R")
#'
#' First we set up the simulation parameters for every experiment,
#' incorporating time steps in ecoli transmission and recovery rates.
#Set the simulation parameters
pop.size<-100
initial.infecteds<-2
initial.susceptibles<-pop.size - initial.infecteds
timestep<-1
ecoli.transmission<- 1*timestep
ecoli.recovery<- 1/3*timestep
start.time<-0
end.time<-100
#'
# Set up the population starting size (at the first time step)
population1.df<-data.frame(susceptibles=initial.susceptibles,
                           infecteds=initial.infecteds)
#'
# The time steps that the simulation will run through
timesteps <- seq(from = start.time+timestep, to = end.time, by = timestep)
#'
#Run the simulation
# Now we loop through the time itself (starting at the second time step)
for (new.time in timesteps) 
{
# calling the new step function with the population at the next time step:
  next.population1<-step_deterministic_SIS(latest = tail(population1.df,1), 
                                           transmission.rate = ecoli.transmission*timestep,
                                           recovery.rate = ecoli.recovery*timestep)
# Add new element onto end of population vector
  population1.df <- rbind(population1.df, next.population1)
}
#'Add time to updated data frame so as to see how the population changes with time.
population1.df$time<-c(start.time,timesteps)
#'
# Plot the results
#' ----------------
# And finally we output the results.
plot_populations(population1.df, col=c("green", "red"))
#'
#'
#'
#Calculating Ro
R0.1 <- ecoli.transmission / ecoli.recovery
print(R0.1)
#'
#calculating the inverse R0.1
inverse.R0.1 <- 1/ R0.1
print(inverse.R0.1)
#'
#'
#'##Set the second time step to 0.1
timestep.2<-0.1
#'
#'
#'# Set up the population starting size (at the first time step)
population2.df<-data.frame(susceptibles=initial.susceptibles,
                           infecteds=initial.infecteds)
#'
# The time steps that the simulation will run through
timesteps.2 <- seq(from = start.time + timestep.2, to = end.time,by = timestep.2)
#'
# Now we loop through the time itself (starting at the second time step)
for (new.time in timesteps.2) 
{
# calling the new step function with the population at the next time step:
  next.population2<-step_deterministic_SIS(latest = tail(population2.df,1), 
                                           transmission.rate = ecoli.transmission*timestep.2,
                                           recovery.rate = ecoli.recovery*timestep.2)
# Add new element onto end of population vector
  population2.df <- rbind(population2.df, next.population2)
}

#'Add time to updated data frame so as to see how the population changes with time.
population2.df$time<-c(start.time,timesteps.2)
#'
#' comparing the two time steps
#' Plot the results
#' ----------------
#' And finally we output of the timesteps against the population
plot_populations(population1.df, col=c("green", "red"))
plot_populations(population2.df, new.graph = FALSE, col=c("green", "red"))
#'
# The graphs seems has changed hence showing that the time step was enough 
#to produce an accurate output that  makes a visible difference. 
#'
#'
#'##Set the third time step to 3
timestep.3<-3
#'
#'
#'# Set up the population starting size (at the first time step)
population3.df<-data.frame(susceptibles=initial.susceptibles,
                           infecteds=initial.infecteds)
#'
# The time steps that the simulation will run through
timesteps.3 <- seq(from = start.time + timestep.3, to = end.time,by = timestep.3)
#'
# Now we loop through the time itself (starting at the second time step)
for (new.time in timesteps.3) 
{
# calling the new step function with the population at the next time step:
  next.population3<-step_deterministic_SIS(latest = tail(population3.df,1), 
                                           transmission.rate = ecoli.transmission*timestep.3,
                                           recovery.rate = ecoli.recovery*timestep.3)
# Add new element onto end of population vector
  population3.df <- rbind(population3.df, next.population3)
}
#'
#'Add time to updated data frame so as to see how the population changes with time.
population3.df$time<-c(start.time,timesteps.3)
#' comparing the two time steps
#' Plot the results
#' ----------------
#' And finally we output the results.
plot_populations(population1.df, col=c("green", "red"))
plot_populations(population3.df, new.graph = FALSE, col=c("green", "red"))
#'
#'##Set the third time step to 3
timestep.4<-5
#'
#'
#'# Set up the population starting size (at the first time step)
population4.df<-data.frame(susceptibles=initial.susceptibles,
                           infecteds=initial.infecteds)
#'
# The time steps that the simulation will run through
timesteps.4 <- seq(from = start.time + timestep.4, to = end.time,by = timestep.4)
#'
# Now we loop through the time itself (starting at the second time step)
for (new.time in timesteps.4) 
{
# calling the new step function with the population at the next time step:
  next.population4<-step_deterministic_SIS(latest = tail(population4.df,1), 
                                           transmission.rate = ecoli.transmission*timestep.4,
                                           recovery.rate = ecoli.recovery*timestep.4)
# Add new element onto end of population vector
  population4.df <- rbind(population4.df, next.population4)
}
#'
#'Add time to updated data frame so as to see how the population changes with time.
population4.df$time<-c(start.time,timesteps.4)
#'
#' comparing the two time steps
#' Plot the results
#' ----------------
#' And finally we output the results.
plot_populations(population1.df, col=c("green", "red"))
plot_populations(population4.df, new.graph = FALSE, col=c("green", "red"))
#'
# The graphs have clearly demonstrated that for smaller time steps (less or equal to 1),
# the model predicted the disease accurately where are beyond 1 (ie 3 and 5),
# the model misbehaved. This shows that time step is an important parameter to consider
# when diseases in SIS models
#'
