#!/usr/bin/env Rscript

#' Run the CoMo model using a template
#'
#' @param list_template A list of a parameter template
#' @keywords como covid19
#' @export
#' list_template <- load_template(
#'     template = "tests/data/Template_CoMoCOVID-19App_v17.xlsx",
#'     country_name = "United Kingdom of Great Britain")
#' run_model(list_template)
run_model <- function(list_template){
  
    l <- list_template
    list_output <- multi_runs(
      l$Y, 
      l$times, 
      l$parameters, 
      input = l$input, 
      A = l$A,
      l$ihr, 
      l$ifr, 
      l$mort, 
      popstruc = l$popstruc, 
      popbirth = l$popbirth, 
      l$ageing,
      contact_home = l$contact_home, 
      contact_school = l$contact_school, 
      contact_work = l$contact_work, 
      contact_other = l$contact_other, 
      age_group_vectors = l$age_group_vectors, 
      use_cpp = l$use_cpp)
  
  return(list_output)
}

#' Process output from running CoMo model into dataframe
#'
#' @param list_output Output from the CoMo model (output from running run_model())
#' @param list_template CoMo parameter template (list)
#' @keywords como covid19
#' @export
#' @examples
#' list_template <- load_template(
#'     template = "tests/data/Template_CoMoCOVID-19App_v17.xlsx",
#'     country_name = "United Kingdom of Great Britain")
#' list_output <- run_model(list_template)
#' df <- process_outputs(list_output)
process_outputs <- function(list_output, list_template){
  l <- list_template
  
  simul_baseline <- list(results = NULL)
  
  simul_baseline$results <- process_ode_outcome(
        out = list_output, 
        param_used = l$parameters, 
        l$startdate, 
        l$times, 
        l$ihr, 
        l$ifr, 
        l$mort, 
        l$popstruc, 
        intv_vector = l$input)

  simul_baseline$baseline_available <- TRUE

  dta <- model_outputs(l$input, simul_baseline, l$cases_rv, l$interventions, l)
  return( dta )
}

