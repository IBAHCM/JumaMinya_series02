
#' title: "Run step_deterministic_SIS function"
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
#' # Comparing the output between the model with timestep manually created with
#' # the one added within the function
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

pop.size<-100
initial.infecteds<-2
initial.susceptibles<-pop.size - initial.infecteds
timestep<-1
ecoli.transmission<- 4/3
ecoli.recovery<- 1/3
start.time<-0
end.time<-100
#'

# Set up the population starting size (at the first time step)
population1.df<-data.frame(time=start.time, susceptibles=initial.susceptibles,
                           infecteds=initial.infecteds)

##specify the sequence of time steps, in this case we will use "while" function
# and timestep will be updated with the data frame rather than making an 
# independent vector for timestep

latest.population<-population1.df

while(latest.population$time < end.time)
{
#Run the simulation
next.population1<-timestep_deterministic_SIS(latest = tail(population1.df,1),
                                                effective.transmission.rate = ecoli.transmission,
                                             effective.recovery.rate = ecoli.recovery,
                                                timestep = this.timestep)


  population1.df<-rbind(population1.df, next.population1)
}

#' Plot the results
#' ----------------
#' And finally we output the results.
plot_populations(population1.df, col=c("green", "red"))











