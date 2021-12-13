#' ---
#' title: "Run timestep_deterministic_SIS function"
#' author: "Juma Minya"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'
#'#' File: 0203-run-SIS.r
#' ========================
#' 
#' 
#' #Objective: to use the created code to run the epidemiology SIS model, 
#' plotting the SIS model graphs with timestep specified in the function 
#' 
# Comparing the output between the model with timestep manually created with
# the one added within the function
#' 
#Loading Packages
library(RPiR)
library(knitr)
#' 
# Read the function
source("0203-deterministic-SIS.R")
#'
#' First we set up the simulation parameters for timestep_deteministic_SIS.
# Set the simulation parameters
#'
pop.size <- 100 # total number of cattle
initial.infecteds <- 2 #number of infected cattle
initial.susceptibles<-pop.size - initial.infecteds
ecoli.transmission <- 4/3
ecoli.recovery <- 1/3
start.time <- 0 #time in weeks
end.time <- 100 #time in weeks
this.timestep <- 1 #time in weeks
#'
#population = herd
# Set up the population starting size data frame
population.df<-data.frame(time = start.time,
                          susceptibles = initial.susceptibles,
                          infecteds = initial.infecteds)

#Run the simulation
latest.population <- population.df
while (latest.population$time < end.time) 
{
  # calling the new timestep function with the population at the next time step:
  latest.population <- timestep_deterministic_SIS(latest = latest.population, 
                                                  transmission.rate = ecoli.transmission,
                                                  recovery.rate = ecoli.recovery,
                                                  timestep = this.timestep)
  # Add new element onto end of population vector
  population.df <- rbind(population.df, latest.population)
}
#'
#' ##Plot the results
#'
#' And finally we output the results of the timesteps against population vector.
plot_populations(population.df, col=c("green", "red"))
#'
#'
#'# **Using step_deterministic_function** 
#'
#'
# Read the function
source("0201-step-SIS.R")
#'
#'
ecoli.transmission <- 4/3
ecoli.recovery <- 1/3
timesteps <- 1 #time in weeks
# Set up the population starting size data frame
population1.df <- data.frame(susceptibles = initial.susceptibles, 
                             infecteds = initial.infecteds)

# The timesteps that the simulation will run through
timesteps <- seq(from = start.time + 1, to = end.time, by = this.timestep)

#Run the simulation
for(new.time in timesteps)
{
  next.population1 <- step_deterministic_SIS(latest = tail(population1.df,1), 
                                             transmission.rate = ecoli.transmission*this.timestep,
                                             recovery.rate = ecoli.recovery*this.timestep)
  
  population1.df <- rbind(population1.df, next.population1)
}
#Adding the time vector into the population
population1.df$time <- c(start.time,timesteps)
#'
# And finally we output the results.
plot_populations(population1.df, col=c("green", "red"))
#'
#'
# This shows that the graphs are similar in appearance with just an extension
# (from time = 1.0) as there is no changes for the time below 1
#'
#'
#' ### Simulation 2: timestep_deterministic_SIS
# loading the source function
source("0203-deterministic-SIS.R")
#'
# Setting population parameters
pop.size <- 100 # total number of cattle
initial.infecteds <- 2 #number of infected cattle
initial.susceptibles<-pop.size - initial.infecteds
ecoli.transmission <- 4/3
ecoli.recovery <- 1/3
start.time <- 0 #time in weeks
end.time <- 100 #time in weeks
this.timestep.2 <- 5
#'
# Set up the population starting size data frame
population2.df<-data.frame(time = start.time,
                           susceptibles = initial.susceptibles,
                           infecteds = initial.infecteds)

#Run the simulation
latest.population2 <- population2.df
while (latest.population2$time < end.time) 
{
  # calling the new timestep function with the population at the next time step:
  latest.population2 <- timestep_deterministic_SIS(latest = latest.population2, 
                                                   transmission.rate = ecoli.transmission,
                                                   recovery.rate = ecoli.recovery,
                                                   timestep = this.timestep)
  # Add new element onto end of population vector
  population2.df <- rbind(population2.df, latest.population2)
}
#'
#' ##Plot the results
#'
#' And finally we output the results of the timesteps against population vector.
plot_populations(population2.df, col=c("green", "red"))

#'
#'
#'
#'Using the step_deterministic function
#'
# loading the step_deterministic_SIS function
source("0201-step-SIS.R")
#'
# setting up the parameters
ecoli.transmission <- 4/3
ecoli.recovery <- 1/3
timestep <- 5
#'
# Set up the population starting size data frame
population3.df <- data.frame(susceptibles = initial.susceptibles, 
                             infecteds = initial.infecteds)

# The timesteps that the simulation will run through
timesteps <- seq(from = start.time + 1, to = end.time, by = timestep)

#Run the simulation
for(new.time in timesteps)
{
  next.population3 <- step_deterministic_SIS(latest = tail(population3.df,1), 
                                             transmission.rate = ecoli.transmission*this.timestep,
                                             recovery.rate = ecoli.recovery*this.timestep)
  
  population3.df <- rbind(population3.df, next.population3)
}
#Adding the time vector into the population
population3.df$time <- c(start.time,timesteps)
#'
# And finally we output the results.
plot_populations(population3.df, col=c("green", "red"))
#'

