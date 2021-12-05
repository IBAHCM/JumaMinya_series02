#'
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
#' 
# Read the function
source("0203-deterministic-SIS.R")
source("0201-step-SIS.R")
#'
#' First we set up the simulation parameters for every experiment.
#Set the simulation parameters
#'
pop.size<-100
initial.infecteds<-2
initial.susceptibles<-pop.size - initial.infecteds
ecoli.transmission<- 4/3
ecoli.recovery<- 1/3
start.time<-0
end.time<-100
this.timestep<-1
#'
# Set up the population starting size data frame
population.df<-data.frame(time = start.time,
                           susceptibles = initial.susceptibles,
                           infecteds = initial.infecteds)
#'
# The time steps that the simulation will run through
timesteps <- seq(from = start.time+1, to = end.time)

#Run the simulation
latest.population <- population.df

while (latest.population$time < end.time) 
{
# calling the new timestep function with the population at the next time step:
latest.population<-timestep_deterministic_SIS(latest = latest.population, 
                                           transmission.rate = ecoli.transmission,
                                           recovery.rate = ecoli.recovery,
                                           timestep = this.timestep)
# Add new element onto end of population vector
  population.df <- rbind(population.df, latest.population)
}
#'
# Set up the population starting size (at the first time step)
population1.df<-data.frame(time=start.time, susceptibles=initial.susceptibles,
                           infecteds=initial.infecteds)
#'
#'
##specify the sequence of time steps, in this case we will use "while" function
# and timestep will be updated with the data frame rather than making an 
# independent vector for timestep
timesteps <- seq(from = start.time + 1, to = end.time)
#'
latest.population<-population.df
#'
while(latest.population$time < end.time)
{
#Run the simulation
next.population<-timestep_deterministic_SIS(latest = tail(population.df,1),
                                            transmission.rate = ecoli.transmission,
                                             recovery.rate = ecoli.recovery,
                                                timestep = this.timestep)
#'
#'
population.df<-rbind(population.df, next.population)
}
#'
#' Plot the results
#' ----------------
#' And finally we output the results of the timesteps against population vector.
plot_populations(population.df, col=c("green", "red"))
#
#'
#'# **Using step_deterministic_function** 
#'
# Read the function
source("0201-step-SIS.R")
#'
# Set up the population starting size data frame
population1.df<-data.frame(susceptibles = initial.susceptibles, 
                           infecteds = initial.infecteds)

# The timesteps that the simulation will run through
timesteps<-seq(from = start.time + 1, to = end.time, by = this.timestep)

#Run the simulation
for(new.time in timesteps)
{
next.population1<-step_deterministic_SIS(latest = tail(population1.df,1), 
                                           transmission.rate = ecoli.transmission*this.timestep,
                                           recovery.rate = ecoli.recovery*this.timestep)
  
population1.df<-rbind(population1.df, next.population1)
}
#Adding the time vector into the population
population1.df$time<-c(start.time,timesteps)
#'
#' And finally we output the results.
plot_populations(population1.df, col=c("green", "red"))
#'
#comparing the two graphs
plot_populations(population.df, col=c("green", "red"))
plot_populations(population1.df, new.graph = FALSE, col=c("green", "red"))
#'
#'
# This shows that the graphs are similar in appearance with just an extension
# (from time = 1.0) as there is no changes for the time below 1
#'
# This shows that the graphs are similar in appearance.
#'
#'
#Setting a new timestep of 5
this.timestep.4<-5
#'
# Set up the population starting size data frame
#'
population.df<-data.frame(time = start.time,
                           susceptibles = initial.susceptibles,
                           infecteds = initial.infecteds)
#'
#'
# The time steps that the simulation will run through
timesteps <- seq(from = start.time+1, to = end.time)
#'
#Run the simulation using timestep_deterministic function
latest.population <- population.df
#'
while (latest.population$time < end.time) 
{
# calling the new step function with the population at the next time step:
latest.population<-timestep_deterministic_SIS(latest = latest.population, 
                                                transmission.rate = ecoli.transmission,
                                                recovery.rate = ecoli.recovery,
                                                timestep = this.timestep.4)
# Add new element onto end of population vector
population.df <- rbind(population.df, latest.population)
}
#'
#Using the step_deterministic function
#'
#' # Set up the population starting size data frame
#'
population1.df<-data.frame(susceptibles = initial.susceptibles, 
                           infecteds = initial.infecteds)
#'
# The time steps that the simulation will run through
timesteps<-seq(from = start.time + 1, to = end.time, by = this.timestep.4)
#'
#Run the simulation
for(new.time in timesteps)
{
next.population1<-step_deterministic_SIS(latest = tail(population1.df,1), 
                                           transmission.rate = ecoli.transmission*this.timestep.4,
                                           recovery.rate = ecoli.recovery*this.timestep.4)
  
population1.df<-rbind(population1.df, next.population1)
}
#Adding the time vector into the population
population1.df$time<-c(start.time,timesteps)
#'
#'
#And finally we output the results of the timesteps against population vector.
plot_populations(population1.df, col=c("green", "red"))
#'
#' comparing the two graphs
plot_populations(population.df, col=c("green", "red"))
plot_populations(population1.df, new.graph = FALSE, col=c("green", "red"))
#'
#'
#'



