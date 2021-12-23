#' ---
#' title: "Library containing a step_deterministic_SIS function"
#' author: "Juma Minya"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

#Load Package
library(RPiR)
library(codetools)

#' File: 0201-step-SIS.r
#' ========================

#' ### Function: step_deterministic_SIS() 
#' Run one step of a step_deterministic_SIS model. 
#'
#' Arguments: 
#' 
#'  - **latest** -- data frame containing latest population count
#'  
#'  - **transmission.rate** -- ecoli transmission
#'                   
#'  - **recovery.rate** -- ecoli recovery
#'
#' Returns:
#' - the next count for updated population count
#'
#'##The function

step_deterministic_SIS <- function(latest, transmission.rate, recovery.rate)   
{
  
  # Calculate the population size   
  pop.size <- latest$susceptibles + latest$infecteds
  
  #calculate changes to the population
  new.recovereds <- recovery.rate * latest$infecteds
  new.infecteds <- transmission.rate * latest$susceptibles*(latest$infecteds/pop.size)
  next.susceptibles <- latest$susceptibles + new.recovereds-new.infecteds
  next.infecteds <- latest$infecteds + new.infecteds - new.recovereds
  
  # create a data frame with updated population and return    
  data.frame(susceptibles = next.susceptibles, infecteds = next.infecteds)
}
##### Does the function work without any external (global) information?

if (length(findGlobals(step_deterministic_SIS,
                       merge = FALSE)$variables) != 0) {
  stop(
    "Function step_deterministic_SIS() may not use global variable(s): ",
    findGlobals(step_deterministic_SIS, merge = FALSE)$variables
  )
}



