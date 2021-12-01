#' title: "Library containing a step_deterministic_SIS function"
#' author: "Juma Minya"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#Load Package
library(RPiR)
library(codetools)

#' #' File: 0201-step-SIS.r
#' ========================
#' #'
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

#'##The function
step_deterministic_SIS <- function(latest, transmission.rate, recovery.rate)   
{

# Calculate the population size   
pop.size<-latest$susceptible + latest$infected

#calculate changes to the population
new.recovereds<-recovery.rate*latest$infected
new.infected<-transmission.rate*latest$susceptible*(latest$infected/pop.size)
next.susceptible<-latest$susceptible + new.recovereds-new.infected
next.infected<-latest$infected + new.infected - new.recovereds

# create a data frame with updated population and return    
data.frame(susceptibles=next.susceptible, infecteds=next.infected)
}


  
