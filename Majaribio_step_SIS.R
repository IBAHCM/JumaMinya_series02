
#' title: "simple epidemiological model – the Susceptible-Infected-Susceptible "
#' author: "Juma Minya"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#' 
#' #'
#' ### Function: step_deterministic_susceptible_infected() 
#' Run one step of a deterministic exponential growth model. 
#'
#' Arguments: 
#' - num.cattle -- the population count now
#' - initial.infected -- the population of infected cattle
#'
#' Returns:
#' - the next count

step_deterministic_SIS <- function(latest, transmission.rate, recovery.rate)   {
  
  # create state variables (local variables)
  S = state_values [1]        # susceptibles
  I = state_values [2]        # infectious
  
  
  new.infecteds <- ( beta * S * I) - (gamma * I)
  
  # Calculate the new susceptibles
  new.susceptibles <- (-beta * S * I) + (gamma * I)
  
  
  # create a data frame with updated population and return
  data.frame(susceptibles = new.susceptibles, infecteds = new.infecteds)
}
#' 