#' ---
#' title: "Run timestep_deterministic_SIR function"
#' author: "Juma Minya"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'
#'#' File: 0204-run-SIS.r
#' ========================
#' 
#' 
# Objective: to use the created code to run the epidemiology SIR model for Foot
#' and mouth disease (FMD), with R0 of 2. 
#' 
# Comparing the output between the model with timestep manually created with
# the one added within the function

#Loading Packages
library(RPiR)
library(knitr)
 
# Read the function
source("0204-deterministic-SIR.R")


# First we set up the simulation parameters for every experiment.
# Set the simulation parameters

pop.size <-100
initial.infecteds <-2
initial.recovereds <-0
initial.susceptibles <-pop.size - initial.infecteds - initial.recovereds
farm.transmission <- 1/2
farm.recovery <- 1/4
start.time <-0
end.time <-100
this.timestep <- 0.75

# Set up the population starting size data frame
population.SIR.df<-data.frame(time = start.time,
                              susceptibles = initial.susceptibles,
                              infecteds = initial.infecteds,
                              recovereds = initial.recovereds)

# Run the simulation
latest.population.SIR <- population.SIR.df

while (latest.population.SIR$time < end.time) 
{
  # calling the new timestep function with the population at the next time step:
  latest.population.SIR<-timestep_deterministic_SIR(latest = latest.population.SIR, 
                                                    transmission.rate = farm.transmission,
                                                    recovery.rate = farm.recovery,
                                                    timestep = this.timestep)
  # Add new element onto end of population vector
  population.SIR.df <- rbind(population.SIR.df, latest.population.SIR)
}

# Plot the results
plot_populations(population.SIR.df, col=c("green","red", "black"))

# calculating R0
R0 <- farm.transmission/farm.recovery
print(R0)

#'### **Using SIS function with the same R0 of 2**
source("0203-deterministic-SIS.R")

# Set up the population starting size (at the first time step)
population.SIS.df <- data.frame(time = start.time, 
                                susceptibles = initial.susceptibles,
                                infecteds = initial.infecteds)

latest.population.SIS <- population.SIS.df

while(latest.population.SIS$time < end.time)
{
  #Run the simulation
  latest.population.SIS <- timestep_deterministic_SIS(latest = latest.population.SIS,
                                                      transmission.rate = farm.transmission,
                                                      recovery.rate = farm.recovery,
                                                      timestep = this.timestep)

  population.SIS.df<-rbind(population.SIS.df, latest.population.SIS)
}

# Plot the results
# And finally we output the results of the timesteps against population vector.
plot_populations(population.SIS.df, col=c("green","red"))

#'The difference between the two models is that in SIR models,the population 
#' balances with recovereds and susceptibles at equilibrium state, while in SIS model,
#'the population balances infecteds and susceptibles. This is because in SIR model,
#'the recovereds are removed from susceptibles group while in SIS model, they are
# also part of the susceptibles group (can be re-infected)
#'
#'---
#' 
#' **comparison of SIR model with different R0 values**
#'
# Setted parameters for comparison for larger R0

farm.transmission1 <- 2
farm.recovery1 <- 1/2
#'
# Set up the population starting size data frame
population1.SIR.df<-data.frame(time = start.time,
                               susceptibles = initial.susceptibles,
                               infecteds = initial.infecteds,
                               recovereds = initial.recovereds)

#Run the simulation
latest.population1.SIR <- population1.SIR.df

while (latest.population1.SIR$time < end.time) 
{
  # calling the new timestep function with the population at the next time step:
  latest.population1.SIR<-timestep_deterministic_SIR(latest = latest.population1.SIR, 
                                                     transmission.rate = farm.transmission1,
                                                     recovery.rate = farm.recovery1,
                                                     timestep = this.timestep)
  # Add new element onto end of population vector
  population1.SIR.df <- rbind(population1.SIR.df, latest.population1.SIR)
}

# Plot the results
plot_populations(population1.SIR.df, col=c("green","red", "black"))

# Set the parameters for comparison for larger R0

farm.transmission2 <- 0.02
farm.recovery2 <- 0.1

# Set up the population starting size data frame
population2.SIR.df<-data.frame(time = start.time,
                               susceptibles = initial.susceptibles,
                               infecteds = initial.infecteds,
                               recovereds = initial.recovereds)
# Run the simulation
latest.population2.SIR <- population2.SIR.df

while (latest.population2.SIR$time < end.time) 
{
  # calling the new timestep function with the population at the next time step:
  latest.population2.SIR<-timestep_deterministic_SIR(latest = latest.population2.SIR, 
                                                     transmission.rate = farm.transmission2,
                                                     recovery.rate = farm.recovery2,
                                                     timestep = this.timestep)
  # Add new element onto end of population vector
  population2.SIR.df <- rbind(population2.SIR.df, latest.population2.SIR)
}

# Plot the results
plot_populations(population2.SIR.df, col=c("green","red", "black"))

#'With small R0, the population remain unchanged as the entire population is 
#'susceptible to the disease, but there is no outbreak yet. This is because the
#'R0 is too small for the disease to spread.
