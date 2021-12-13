#' ---
#' title: "Practical 2-3: Library containing a timestep_deterministic_SIS function"
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
#' ### Function: timestep_deterministic_SIS() 
#'
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
#' - the next count for updated population of infecteds and susceptibles
#'
#'##The function
timestep_deterministic_SIS <- function(latest,transmission.rate,
                                       recovery.rate,timestep)
{
  # Calculate the population size   
  pop.size<-latest$susceptibles + latest$infecteds
  
  # Calculate the effective transmission rate for timesteps
  effective.transmission.rate <- transmission.rate*timestep
  
  # Calculate the effective recovery rate for timesteps
  effective.recovery.rate <- recovery.rate*timestep
  
  # other population parameters changes
  new.recovereds <- effective.recovery.rate * latest$infecteds 
  new.infecteds <- effective.transmission.rate * latest$susceptibles *
    (latest$infecteds/pop.size)
  next.susceptibles <- latest$susceptibles + new.recovereds - new.infecteds
  next.infecteds <- latest$infecteds + new.infecteds - new.recovereds
  
  # create a data frame with updated population and return    
  return(data.frame(time = latest$time + timestep,
                    susceptibles = next.susceptibles,
                    infecteds = next.infecteds))
}

# Does the function work without any external (global) information?
if (length(findGlobals(timestep_deterministic_SIS,
                       merge = FALSE)$variables) != 0) {
  stop(
    "Function timestep_deterministic_SIS() may not use global variable(s): ",
    findGlobals(timestep_deterministic_SIS, merge = FALSE)$variables
  )
}



