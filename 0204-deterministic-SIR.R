#' ---
#' title: "Practical 2-4: Library containing a step_deterministic_SIR function"
#' author: "Juma Minya"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#Load Package
library(RPiR)
library(codetools)

#' #' File: 0204-deterministic-SIR.r
#' ========================
#' #'
#' ### Function: timestep_deterministic_SIR() 
#'
#' Run a timestep_deterministic_SIR model. 
#'
#' Arguments: 
#' 
#'  - **latest** -- current population count
#'
#'  - **transmission.rate** -- farm transmission
#'                   
#'  - **recovery.rate** -- farm recovery
#'  
#'  - **timestep** -- series of a time frame
#'
#' Returns:
#' - the next count of recovereds,infecteds and susceptibles in the population
#'
#'##The function
timestep_deterministic_SIR <- function(latest,transmission.rate,
                                       recovery.rate,timestep)
{
  
  # Calculate the population size   
  pop.size<-latest$susceptibles + latest$infecteds + latest$recovereds
  
  # Calculate changes to the population
  # Calculate the transmission rate
  effective.transmission.rate<-transmission.rate*timestep
  
  # Calculate the recovery rate
  effective.recovery.rate<-recovery.rate*timestep
  
  # other parameters
  new.infecteds <- effective.transmission.rate * latest$susceptibles *
    (latest$infecteds/pop.size)
  
  new.recovereds <- effective.recovery.rate * latest$infecteds
  
  next.susceptibles <- latest$susceptibles - new.infecteds
  
  next.infecteds <- latest$infecteds + new.infecteds - new.recovereds
  
  next.recovereds <- latest$recovereds + new.recovereds
  # create a data frame with updated population and return    
  
  return(data.frame(time = latest$time + timestep,
                    susceptibles = next.susceptibles,
                    infecteds = next.infecteds,
                    recovereds = next.recovereds))
}

# checking if the function does not work without any external (global) information
if (length(findGlobals(timestep_deterministic_SIR,
                       merge = FALSE)$variables) != 0) 
{
  stop(
    "Function timestep_deterministic_SIR() may not use global variable(s): ",
    findGlobals(timestep_deterministic_SIR, merge = FALSE)$variables
  )
}