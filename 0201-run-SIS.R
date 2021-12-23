#' ---
#' title: "Run step_deterministic_SIS model"
#' author: "Juma Minya"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' Objective: running the SIS model and plot the SIS model graphs
#' 
#Loading Packages
library(RPiR)
library(knitr)
library(codetools)

# Read the function
source("0201-step-SIS.R")

#Set the simulation parameters
pop.size <- 100 # cattle number
initial.infecteds <- 2
initial.susceptibles <- pop.size - initial.infecteds
ecoli.transmission <- 4/3
ecoli.recovery <- 1/3
start.time <- 0
end.time <- 100

# Set up the population starting size data frame (at the first time step)
population1.df <- data.frame(susceptibles=initial.susceptibles,
                             infecteds=initial.infecteds)

# The time steps that the simulation will run through
timesteps <- seq(from = start.time + 1, to = end.time)

# Now we loop through the time itself (starting at the second time step)
for (new.time in timesteps) 
{
  # calling the new step function with the population at the next time step:
  next.population1 <- step_deterministic_SIS(latest = tail(population1.df,1), 
                                             transmission.rate = ecoli.transmission,
                                             recovery.rate = ecoli.recovery)
  
  # Add new element onto end of population vector
  population1.df <- rbind(population1.df, next.population1)
}

# Add time to updated data frame to see how the population changes with time.
population1.df$time<-c(start.time,timesteps)

# And finally we output the results.
plot_populations(population1.df, col=c("green", "red"))

#'Interpretation of the graph: 
#'In this population, the number of infecteds is exponentially increasing 
#'while the number of susceptibles is decreasing until equilibrium is reached. 
#'This shows that with high basic reproduction ratio i.e. R0 > 1, there is high
#'disease spread in a population.

# Add, the column for proportion of susceptibles in a population
population1.df$prop.susceptibles <- c(population1.df$susceptibles/pop.size)
print(tail(population1.df,1))

#' ##### Setting up a second transmission rate
# But when Ro < 1...

## Set the simulation with only ecoli. transmission changed
ecoli.transmission2 <- 1

# Set up the population starting size (at the first time step)
population2.df <- data.frame(susceptibles = initial.susceptibles,
                             infecteds = initial.infecteds)

# The time steps that the simulation will run through
timesteps <- seq(from = start.time + 1, to = end.time)

# Now we loop through the time itself (starting at the second time step)
for (new.time in timesteps) 
{
  # calling the new step function with the population at the next time step:
  next.population2 <- step_deterministic_SIS(latest = tail(population2.df,1), 
                                             transmission.rate = ecoli.transmission2,
                                             recovery.rate = ecoli.recovery)
  
  # Add new element onto end of population vector
  population2.df <- rbind(population2.df, next.population2)
}

#Add time to updated data frame so as to see how the population changes with time.
population2.df$time<-c(start.time,timesteps)


# And finally we output the results.
plot_populations(population2.df, col=c("green", "red"))

#' Then we can see no changes between number of infecteds and susceptibles, 
#' which means disease spread in a population is too low to be detected.
#'Therefore understanding basic reproduction number is very important in  
#'making decision on the intervention mechanisms, e.g. vaccination or treatment.

#'
#'---
#'

#Add, the column for proportion of susceptibles in a population
population2.df$prop.susceptibles<-c(population2.df$susceptibles/pop.size)
print(tail(population2.df,1))

#' By using different values of beta (transmission rate), we see how the proportional
#' of susceptibles in a population varies with basic reproduction number as:

# changing the Transmission rate to 2/3
ecoli.transmission3 <- 2/3

population3.df <- data.frame(susceptibles = initial.susceptibles, 
                             infecteds = initial.infecteds)

timesteps <- seq(from = start.time + 1, to=end.time)

for(new.time in timesteps)
{
  next.population3 <- step_deterministic_SIS(latest = tail(population3.df,1), 
                                             transmission.rate = ecoli.transmission3,
                                             recovery.rate = ecoli.recovery)
  
  population3.df <- rbind(population3.df, next.population3)
}
population3.df$time <- c(start.time,timesteps)

# And finally we output the results.
plot_populations(population3.df, col=c("green", "red"))

#Add, the column for proportion of susceptibles in a population
population3.df$prop.susceptibles <- c(population3.df$susceptibles/pop.size)
print(tail(population3.df,1))

#'
#'---
#'

# using transmission rate of 0.5

ecoli.transmission4 <- 0.5
timesteps <- seq(from=start.time + 1, to=end.time)

population4.df <- data.frame(susceptibles = initial.susceptibles, 
                             infecteds = initial.infecteds)

timesteps <- seq(from=start.time+1, to = end.time)

for(new.time in timesteps)
{
  next.population4 < -step_deterministic_SIS(latest = tail(population4.df,1), 
                                             transmission.rate = ecoli.transmission4,
                                             recovery.rate = ecoli.recovery)
  
  population4.df <- rbind(population4.df, next.population4)
}
population4.df$time <- c(start.time,timesteps)

# And finally we output the results.
plot_populations(population4.df, col=c("green", "red"))

#Add the column for proportion of susceptibles in a population
population4.df$prop.susceptibles <- c(population4.df$susceptibles/pop.size)
print(tail(population4.df,1))

# Creating a data frame to report how the values of R0 and proportion of 
#susceptibles vary with different transmission rate as well as how the 
#proportion of the susceptibles is related to reciprocal of R0 (i.e.1/R0).  

# Create the vectors to be included in the data frame
transmission.rate<-c(1/2,2/3,1,4/3)
recovery.rate<-c(1/3,1/3,1/3,1/3)

#Then create a dataframe
population <- data.frame(transmission.rate,recovery.rate)

# Add the columns of total population size and latest susceptibles in a data frame, so as to get the proportion of susceptibles individuals.
population$pop.size<-c(pop.size)

population$susceptibles <- c((tail(population1.df$susceptibles,1)), 
                             tail(population2.df$susceptibles,1), 
                             tail(population3.df$susceptibles,1), 
                             tail(population4.df$susceptibles,1))

#Add column of R0 
population$R0 <- c(transmission.rate/recovery.rate)

# Add the column for 1/R0
population$reciprocal.R0 <- c(population$susceptibles/pop.size)

# Report results in a table as follows;
kable(population, caption="how transmission rate alters proportional of 
      susceptibles in a population")

#' In general, the values of 1/R0 and proportion of susceptibles in a population 
#' are similar which means higher the basic reproduction ratio in a population, 
#'the larger the number of the population will be infecteds.









