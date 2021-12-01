#' title: "Library containing a step_deterministic_SIS function"
#' author: "Juma Minya"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#Load Package
library(RPiR)
library(codetools)

#' #' File: 0203-deterministic-SIS.r
#' ========================
#' #'
#' ### Function: step_deterministic_SIS() 
#' Run one step of a step_deterministic_SIS model. 
#'
#' Arguments: 
#' 
#'  - **latest** -- latest population count
#'  
#'  - **transmission.rate** -- ecoli transmission
#'                   
#'  - **recovery.rate** -- ecoli recovery
#'  
#'  - **timestep** -- series of a time frame
#'
#' Returns:
#' - the next count for updated population count of infected and susceptibles

#'##The function
timestep_deterministic_SIS <- function(latest,
                                       effective.transmission.rate,
                                         effective.recovery.rate,
                                         timestep)   
{

# Calculate the population size   
pop.size<-latest$susceptible + latest$infected

# Calculate changes to the population
# Calculate the effective transmission rate
effective.transmission.rate<-transmission.rate*timestep

# Calculate the effective recovery rate
effective.recovery.rate<-recovery.rate*timestep

# other parameters
new.susceptible<-effective.recovery.rate*latest$infected 
new.infected<-effective.transmission.rate*latest$susceptible*(latest$infected/pop.size)
next.susceptible<-latest$susceptible + new.susceptible-new.infected
next.infected<-latest$infected + new.infected - new.susceptible

# create a data frame with updated population and return    
return(data.frame(susceptibles=next.susceptible, infecteds=next.infected, time=time))
}

# Does the function work without any external (global) information?

if (length(findGlobals(step_deterministic_SIS,
                       merge = FALSE)$variables) != 0) {
  stop(
    "Function step_deterministic_SIS() may not use global variable(s): ",
    findGlobals(step_deterministic_SIS, merge = FALSE)$variables
  )
}


  
