model_outputs <- function(input, simul_baseline, cases_rv, interventions, list_template){
# Outputs of the model ----
  dta <- tibble(
    date = simul_baseline$results$time, 

    # Baseline
    baseline_predicted_reported_min = simul_baseline$results$min$daily_incidence,
    baseline_predicted_reported_and_unreported_min = simul_baseline$results$min$daily_total_cases,
    baseline_normal_bed_occupancy_min = simul_baseline$results$min$hospital_surge_beds,
    baseline_icu_bed_occupancy_min = simul_baseline$results$min$icu_beds,
    baseline_icu_ventilator_occupancy_min = simul_baseline$results$min$ventilators,
    baseline_normal_bed_requirement_min = simul_baseline$results$min$normal_bed_requirement,
    baseline_icu_bed_requirement_min = simul_baseline$results$min$icu_bed_requirement,
    baseline_icu_ventilator_requirement_min = simul_baseline$results$min$icu_ventilator_requirement,
    baseline_death_natural_non_exposed_min = simul_baseline$results$min$death_natural_non_exposed,
    baseline_death_natural_exposed_min = simul_baseline$results$min$death_natural_exposed,
    baseline_death_treated_hospital_min = simul_baseline$results$min$death_treated_hospital,
    baseline_death_treated_icu_min = simul_baseline$results$min$death_treated_icu,
    baseline_death_treated_ventilator_min = simul_baseline$results$min$death_treated_ventilator,
    baseline_death_untreated_hospital_min = simul_baseline$results$min$death_untreated_hospital,
    baseline_death_untreated_icu_min = simul_baseline$results$min$death_untreated_icu,
    baseline_death_untreated_ventilator_min = simul_baseline$results$min$death_untreated_ventilator,
    baseline_death_cum_mortality_min = simul_baseline$results$min$cum_mortality,
    baseline_death_deaths_from_covid_min = simul_baseline$results$min$deaths_from_covid,
    baseline_death_deaths_with_covid_min = simul_baseline$results$min$deaths_with_covid,


    baseline_predicted_reported_med = simul_baseline$results$med$daily_incidence,
    baseline_predicted_reported_and_unreported_med = simul_baseline$results$med$daily_total_cases,
    baseline_normal_bed_occupancy_med = simul_baseline$results$med$hospital_surge_beds,
    baseline_icu_bed_occupancy_med = simul_baseline$results$med$icu_beds,
    baseline_icu_ventilator_occupancy_med = simul_baseline$results$med$ventilators,
    baseline_normal_bed_requirement_med = simul_baseline$results$med$normal_bed_requirement,
    baseline_icu_bed_requirement_med = simul_baseline$results$med$icu_bed_requirement,
    baseline_icu_ventilator_requirement_med = simul_baseline$results$med$icu_ventilator_requirement,
    baseline_death_natural_non_exposed_med = simul_baseline$results$med$death_natural_non_exposed,
    baseline_death_natural_exposed_med = simul_baseline$results$med$death_natural_exposed,
    baseline_death_treated_hospital_med = simul_baseline$results$med$death_treated_hospital,
    baseline_death_treated_icu_med = simul_baseline$results$med$death_treated_icu,
    baseline_death_treated_ventilator_med = simul_baseline$results$med$death_treated_ventilator,
    baseline_death_untreated_hospital_med = simul_baseline$results$med$death_untreated_hospital,
    baseline_death_untreated_icu_med = simul_baseline$results$med$death_untreated_icu,
    baseline_death_untreated_ventilator_med = simul_baseline$results$med$death_untreated_ventilator,
    baseline_death_cum_mortality_med = simul_baseline$results$med$cum_mortality,
    baseline_death_deaths_from_covid_med = simul_baseline$results$med$deaths_from_covid,
    baseline_death_deaths_with_covid_med = simul_baseline$results$med$deaths_with_covid,


    baseline_predicted_reported_max = simul_baseline$results$max$daily_incidence,
    baseline_predicted_reported_and_unreported_max = simul_baseline$results$max$daily_total_cases,
    baseline_normal_bed_occupancy_max = simul_baseline$results$max$hospital_surge_beds,
    baseline_icu_bed_occupancy_max = simul_baseline$results$max$icu_beds,
    baseline_icu_ventilator_occupancy_max = simul_baseline$results$max$ventilators,
    baseline_normal_bed_requirement_max = simul_baseline$results$max$normal_bed_requirement,
    baseline_icu_bed_requirement_max = simul_baseline$results$max$icu_bed_requirement,
    baseline_icu_ventilator_requirement_max = simul_baseline$results$max$icu_ventilator_requirement,
    baseline_death_natural_non_exposed_max = simul_baseline$results$max$death_natural_non_exposed,
    baseline_death_natural_exposed_max = simul_baseline$results$max$death_natural_exposed,
    baseline_death_treated_hospital_max = simul_baseline$results$max$death_treated_hospital,
    baseline_death_treated_icu_max = simul_baseline$results$max$death_treated_icu,
    baseline_death_treated_ventilator_max = simul_baseline$results$max$death_treated_ventilator,
    baseline_death_untreated_hospital_max = simul_baseline$results$max$death_untreated_hospital,
    baseline_death_untreated_icu_max = simul_baseline$results$max$death_untreated_icu,
    baseline_death_untreated_ventilator_max = simul_baseline$results$max$death_untreated_ventilator,
    baseline_death_cum_mortality_max = simul_baseline$results$max$cum_mortality,
    baseline_death_deaths_from_covid_max = simul_baseline$results$max$deaths_from_covid,
    baseline_death_deaths_with_covid_max = simul_baseline$results$max$deaths_with_covid)#,


#     # Hypothetical scenario
#     hypothetical_predicted_reported_min = simul_interventions$results$min$daily_incidence,
#     hypothetical_predicted_reported_and_unreported_min = simul_interventions$results$min$daily_total_cases,
#     hypothetical_normal_bed_occupancy_min = simul_interventions$results$min$hospital_surge_beds,
#     hypothetical_icu_bed_occupancy_min = simul_interventions$results$min$icu_beds,
#     hypothetical_icu_ventilator_occupancy_min = simul_interventions$results$min$ventilators,
#     hypothetical_normal_bed_requirement_min = simul_interventions$results$min$normal_bed_requirement,
#     hypothetical_icu_bed_requirement_min = simul_interventions$results$min$icu_bed_requirement,
#     hypothetical_icu_ventilator_requirement_min = simul_interventions$results$min$icu_ventilator_requirement,
#     hypothetical_death_natural_non_exposed_min = simul_interventions$results$min$death_natural_non_exposed,
#     hypothetical_death_natural_exposed_min = simul_interventions$results$min$death_natural_exposed,
#     hypothetical_death_treated_hospital_min = simul_interventions$results$min$death_treated_hospital,
#     hypothetical_death_treated_icu_min = simul_interventions$results$min$death_treated_icu,
#     hypothetical_death_treated_ventilator_min = simul_interventions$results$min$death_treated_ventilator,
#     hypothetical_death_untreated_hospital_min = simul_interventions$results$min$death_untreated_hospital,
#     hypothetical_death_untreated_icu_min = simul_interventions$results$min$death_untreated_icu,
#     hypothetical_death_untreated_ventilator_min = simul_interventions$results$min$death_untreated_ventilator,
#     hypothetical_death_cum_mortality_min = simul_interventions$results$min$cum_mortality,
#     hypothetical_death_deaths_from_covid_min = simul_interventions$results$min$deaths_from_covid,
#     hypothetical_death_deaths_with_covid_min = simul_interventions$results$min$deaths_with_covid,

#     hypothetical_predicted_reported_med = simul_interventions$results$med$daily_incidence,
#     hypothetical_predicted_reported_and_unreported_med = simul_interventions$results$med$daily_total_cases,
#     hypothetical_normal_bed_occupancy_med = simul_interventions$results$med$hospital_surge_beds,
#     hypothetical_icu_bed_occupancy_med = simul_interventions$results$med$icu_beds,
#     hypothetical_icu_ventilator_occupancy_med = simul_interventions$results$med$ventilators,
#     hypothetical_normal_bed_requirement_med = simul_interventions$results$med$normal_bed_requirement,
#     hypothetical_icu_bed_requirement_med = simul_interventions$results$med$icu_bed_requirement,
#     hypothetical_icu_ventilator_requirement_med = simul_interventions$results$med$icu_ventilator_requirement,
#     hypothetical_death_natural_non_exposed_med = simul_interventions$results$med$death_natural_non_exposed,
#     hypothetical_death_natural_exposed_med = simul_interventions$results$med$death_natural_exposed,
#     hypothetical_death_treated_hospital_med = simul_interventions$results$med$death_treated_hospital,
#     hypothetical_death_treated_icu_med = simul_interventions$results$med$death_treated_icu,
#     hypothetical_death_treated_ventilator_med = simul_interventions$results$med$death_treated_ventilator,
#     hypothetical_death_untreated_hospital_med = simul_interventions$results$med$death_untreated_hospital,
#     hypothetical_death_untreated_icu_med = simul_interventions$results$med$death_untreated_icu,
#     hypothetical_death_untreated_ventilator_med = simul_interventions$results$med$death_untreated_ventilator,
#     hypothetical_death_cum_mortality_med = simul_interventions$results$med$cum_mortality,
#     hypothetical_death_deaths_from_covid_med = simul_interventions$results$med$deaths_from_covid,
#     hypothetical_death_deaths_with_covid_med = simul_interventions$results$med$deaths_with_covid,

#     hypothetical_predicted_reported_max = simul_interventions$results$max$daily_incidence,
#     hypothetical_predicted_reported_and_unreported_max = simul_interventions$results$max$daily_total_cases,
#     hypothetical_normal_bed_occupancy_max = simul_interventions$results$max$hospital_surge_beds,
#     hypothetical_icu_bed_occupancy_max = simul_interventions$results$max$icu_beds,
#     hypothetical_icu_ventilator_occupancy_max = simul_interventions$results$max$ventilators,
#     hypothetical_normal_bed_requirement_max = simul_interventions$results$max$normal_bed_requirement,
#     hypothetical_icu_bed_requirement_max = simul_interventions$results$max$icu_bed_requirement,
#     hypothetical_icu_ventilator_requirement_max = simul_interventions$results$max$icu_ventilator_requirement,
#     hypothetical_death_natural_non_exposed_max = simul_interventions$results$max$death_natural_non_exposed,
#     hypothetical_death_natural_exposed_max = simul_interventions$results$max$death_natural_exposed,
#     hypothetical_death_treated_hospital_max = simul_interventions$results$max$death_treated_hospital,
#     hypothetical_death_treated_icu_max = simul_interventions$results$max$death_treated_icu,
#     hypothetical_death_treated_ventilator_max = simul_interventions$results$max$death_treated_ventilator,
#     hypothetical_death_untreated_hospital_max = simul_interventions$results$max$death_untreated_hospital,
#     hypothetical_death_untreated_icu_max = simul_interventions$results$max$death_untreated_icu,
#     hypothetical_death_untreated_ventilator_max = simul_interventions$results$max$death_untreated_ventilator,
#     hypothetical_death_cum_mortality_max = simul_interventions$results$max$cum_mortality,
#     hypothetical_death_deaths_from_covid_max = simul_interventions$results$max$deaths_from_covid,
#     hypothetical_death_deaths_with_covid_max = simul_interventions$results$max$deaths_with_covid,
#  )
  
  # Cases Data ----
  dta <- left_join(
    dta, 
    cases_rv$data %>% rename(input_cases = cases, input_deaths = deaths, input_cumulative_death = cumulative_death), 
    by = "date")

  # Interventions ----
  startdate <- list_template$startdate #input$date_range[1]
  stopdate <- list_template$stopdate #input$date_range[2]
  
  times <- seq(0, as.numeric(stopdate - startdate))
  inp <- bind_rows(interventions$baseline_mat %>% mutate(apply_to = "Baseline (Calibration)"),
                   interventions$future_mat %>% mutate(apply_to = "Hypothetical Scenario"))

  vectors0 <- inputs(inp, 'Baseline (Calibration)', times, startdate, stopdate)
  vectors0_cbind <- do.call(cbind, vectors0)
  vectors0_reduced <- vectors0_cbind[seq(from=0,to=nrow(vectors0_cbind),by=20),]
  vectors0_reduced <- as.data.frame(rbind(rep(0,ncol(vectors0_reduced)),vectors0_reduced))
  vectors0_reduced <- vectors0_reduced[,1:12] #subsetting only the coverages - total of 12 different interventions
  names(vectors0_reduced) <- paste0("interventions_baseline_",names(vectors0_reduced))

  vectors <- inputs(inp, 'Hypothetical Scenario', times, startdate, stopdate)
  vectors_cbind <- do.call(cbind, vectors)
  vectors_reduced <- vectors_cbind[seq(from=0,to=nrow(vectors_cbind),by=20),]
  vectors_reduced <- as.data.frame(rbind(rep(0,ncol(vectors_reduced)),vectors_reduced))
  vectors_reduced <- vectors_reduced[,1:12] #subsetting only the coverages - total of 12 different interventions
  names(vectors_reduced) <- paste0("interventions_hypothetical_",names(vectors_reduced))

  intv_vectors <- as_tibble(cbind(date=simul_baseline$results$time, vectors0_reduced, vectors_reduced))
  intv_vectors$date <- as.Date(intv_vectors$date)
  dta <- left_join(dta,intv_vectors, by="date")

 return(dta)
}