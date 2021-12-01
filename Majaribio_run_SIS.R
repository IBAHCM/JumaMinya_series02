
#' title: "the SIS epidemiological model"
#' author: "Juma Minya"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'
source("Majaribio-step-SIS.R")
#'step_deterministic_susceptible_infected
#'

library (RPiR)
#'
#'#' First we set up the simulation parameters for every experiment.

# Set the initial population, infected and initial susceptible

# initial number of cattle
num.cattle <- 100

#initial cattle infected 
initial.infecteds <- 2

#number of initial susceptible
initial.susceptibles <- num.cattle - initial.infecteds

# Compute Ro - Reproductive number.
Ro = beta / gamma


#number of transmission for the ecoli
ecoli.transmission <- 3

#number of recovery for ecoli
ecoli.recovery <- 1


# And setting times
start.time <- 0
end.time <- 120

#' Run the simplest possible simulation
#' ------------------------------------
#' Then run it so that we can get the output we need

# Set up the herd starting size (at the first time step)

herd.df <- data.frame(susceptibles = initial.susceptibles,
                      infecteds = initial.infecteds)

# The time steps that the simulation will run through
timesteps <- seq(from = start.time + 1, to = end.time, by =1)


# Now we loop through the time itself (starting at the second timestep)
for (new.time in timesteps) {
  
  # calling the new step function with the population at the next time step:
  next.population <- step_deterministic_SIS(latest = tail(herd.df, 1), 
                                            transmission.rate = ecoli.transmission,
                                            recovery.rate = ecoli.recovery)
  # Add new element onto end of population vector
  herd.df <- rbind(herd.df, next.population)
  
}

#' Plot the results
#' ----------------
#' And finally we output the results.
herd.df$time <- c(start.time, timesteps)
plot_populations(herd.df)

#' Set up a second population
#' #' --------------------------
#' 


#next cattle infected
next.infecteds <- 5

#number of next susceptible
next.susceptibles <- num.cattle - initial.infecteds

#number of transmission for the ecoli
ecoli.transmission <- 3

#number of recovery for ecoli
ecoli.recovery <- 1


# And setting times
start.time <- 0
end.time <- 120

#' Run the simplest possible simulation
#' ------------------------------------
#' Then run it so that we can get the output we need

# Set up the population starting size (at the first timestep)
herd2.df <- data.frame(susceptibles = next.susceptibles,
                       infecteds = next.infecteds)

# The time steps that the simulation will run through
timesteps <- seq(from = start.time + 1, to = end.time)

# Now we loop through the time itself (starting at the second timestep)
for (new.time in timesteps) {
  
  # calling the new step function with the population at the next time step:
  next.population <- step_deterministic_SIS(latest = tail(herd2.df, 1), 
                                            transmission.rate = ecoli.transmission,
                                            recovery.rate = ecoli.recovery)
  # Add new element onto end of population vector
  herd2.df <- rbind(herd2.df, next.population)
  
}


#' Plot the results
#' ----------------
#' And finally we output the results.
# Now we can plot the time steps against the population vector
herd2.df$time <- c(start.time, timesteps)
plot_populations(herd2.df,  new.graph = FALSE, col = "red")

