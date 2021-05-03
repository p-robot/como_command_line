#!/usr/bin/env Rscript

#' Load a CoMo template file to list
#'
#' @param template Path to the template
#' @param country_name Name of the country of interest
#' @param USE_CPP Should the CPP solver be used?  
#' @keywords como covid19
#' @export
#' @examples
#' load_template(
#'     template = "tests/data/Template_CoMoCOVID-19App_v17.xlsx",
#'     country_name = "United Kingdom of Great Britain")
load_template <- function(template, country_name, USE_CPP = FALSE){
  print("load_template() start")
  
  file_path <- template
  source("como_read_data.R")
  
  list_template <- list(
      Y = Y, 
      times = times, 
      startdate = startdate,
      stopdate = stopdate,
      parameters = parameters, 
      input = vectors, 
      A = A,  
      ihr = ihr, 
      ifr = ifr, 
      mort = mort, 
      popstruc = popstruc, 
      popbirth = popbirth, 
      ageing = ageing,
      contact_home = contact_home, 
      contact_school = contact_school, 
      contact_work = contact_work, 
      contact_other = contact_other, 
      age_group_vectors = interventions$baseline_age_groups, 
      cases_rv = cases_rv,
      interventions = interventions,
      use_cpp = USE_CPP
  )
  print("load_template() complete")
  return(list_template)
}


# Function to run the CoMo model using 
# an input template
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
      l$popstruc, 
      l$popbirth, 
      l$ageing,
      contact_home = l$contact_home, 
      contact_school = l$contact_school, 
      contact_work = l$contact_work, 
      contact_other = l$contact_other, 
      age_group_vectors = l$age_group_vectors, 
      use_cpp = l$use_cpp)
  
  return(list_output)
}


# Function to turn output from run_model into dataframe
process_outputs <- function(list_output, list_template){
  print("process_outputs start")
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
  print("process_outputs complete")
  return( dta )
}


# inputs1<-function(inp, run){
#   # cap intervention start and end dates with simulation end date

#   inp[["Date Start"]] = pmin(stopdate, as.Date(inp[["Date Start"]]))
#   inp[["Date End"]] = pmin(stopdate, as.Date(inp[["Date End"]]))

#   inp[["Date Start"]] = pmax(startdate, as.Date(inp[["Date Start"]]))
#   inp[["Date End"]] = pmax(startdate, as.Date(inp[["Date End"]]))

#   inp <- inp %>% arrange(`Date Start`)
#   # print(inp)
#   tv<-which(inp$`Apply to`==run)

#   si<-intersect(which(inp$Intervention=="Self-isolation if Symptomatic"),tv)
#   scr<-intersect(which(inp$Intervention=="(*Self-isolation) Screening"),tv)
#   sd<-intersect(which(inp$Intervention=="Social Distancing"),tv)
#   hw<-intersect(which(inp$Intervention=="Handwashing"),tv)
#   msk<-intersect(which(inp$Intervention=="Mask Wearing"),tv)
#   wah<-intersect(which(inp$Intervention=="Working at Home"),tv)
#   sc<-intersect(which(inp$Intervention=="School Closures"),tv)
#   scp<-intersect(which(inp$Intervention=="Partial School Closures"),tv)
#   # scc<-intersect(which(inp$Intervention=="School Group Code"),tv)
#   cte<-intersect(which(inp$Intervention=="Shielding the Elderly"),tv)
#   q<-intersect(which(inp$Intervention=="(*Self-isolation) Household Isolation"),tv)
#   tb<-intersect(which(inp$Intervention=="International Travel Ban"),tv)
#   vc<-intersect(which(inp$Intervention=="Vaccination"),tv)
#   vcp<-intersect(which(inp$Intervention=="Partial Vaccination"),tv)
#   mt<-intersect(which(inp$Intervention=="Mass Testing"),tv)
#   dx<-intersect(which(inp$Intervention=="Dexamethasone"),tv)
  
#   v<-(format(as.POSIXct(inp$`Date Start`,format='%Y/%m/%d %H:%M:%S'),format="%d/%m/%y"))
#   v2<-as.Date(v,format="%d/%m/%y")
#   inp$`Date Start`<-v2
  
#   v<-(format(as.POSIXct(inp$`Date End`,format='%Y/%m/%d %H:%M:%S'),format="%d/%m/%y"))
#   v2<-as.Date(v,format="%d/%m/%y")
#   inp$`Date End`<-v2
  
#   ##  self isolation
#   f<-c()
#   si_vector<-c()
#   isolation<-c()
#   if (length(si)>=1){
#     for (i in 1:length(si)){
#       f<-c(f,as.numeric(inp$`Date Start`[si[i]]-startdate),as.numeric(inp$`Date End`[si[i]]-startdate))
#       # print(f)
#       if(i==1){
#         if (inp$`Date Start`[si[i]]>startdate){
#           si_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[si[i]],(f[i+1]-f[i])*20))
#           isolation<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#         }
#         else{
#           si_vector<-c(rep(inp$`Value`[si[i]],(f[i+1])*20))
#           isolation<-c(rep(1,(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           si_vector<-c(si_vector,rep(inp$`Value`[si[i]],20))
#           isolation<-c(isolation,rep(1,20))
#         }else{
#           si_vector<-c(si_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           isolation<-c(isolation,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         si_vector<-c(si_vector,rep(inp$`Value`[si[i]],(f[i*2]-f[i*2-1])*20))
#         isolation<-c(isolation,rep(1,(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(si) && f[i*2]<tail(times,1)){
#        si_vector<-c(si_vector,rep(0,(tail(times,1)-f[i*2])*20))
#        isolation<-c(isolation,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     si_vector<-rep(0,tail(times,1)*20)
#     isolation<-rep(0,tail(times,1)*20)
#   }
#   ## social distancing
#   f<-c()
#   sd_vector<-c()
#   distancing<-c()
#   if (length(sd)>=1){
#     for (i in 1:length(sd)){
      
#       f<-c(f,as.numeric(inp$`Date Start`[sd[i]]-startdate),as.numeric(inp$`Date End`[sd[i]]-startdate))
      
#       if(i==1){
#         if (inp$`Date Start`[sd[i]]>startdate){
#           sd_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[sd[i]],(f[i+1]-f[i])*20))
#           distancing<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#         }
#         else{
#           sd_vector<-c(rep(inp$`Value`[sd[i]],(f[i+1])*20))
#           distancing<-c(rep(1,(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           sd_vector<-c(sd_vector,rep(inp$`Value`[sd[i]],20))
#           distancing<-c(distancing,rep(1,20))
#         }else{
#           sd_vector<-c(sd_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           distancing<-c(distancing,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         sd_vector<-c(sd_vector,rep(inp$`Value`[sd[i]],(f[i*2]-f[i*2-1])*20))
#         distancing<-c(distancing,rep(1,(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(sd)&& f[i*2]<tail(times,1)){
#         sd_vector<-c(sd_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         distancing<-c(distancing,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     sd_vector<-rep(0,tail(times,1)*20)
#     distancing<-rep(0,tail(times,1)*20)
#   }
#   ## screening
#   f<-c()
#   scr_vector<-c()
#   screen<-c()
#   if (length(scr)>=1){
#     for (i in 1:length(scr)){
#       f<-c(f,as.numeric(inp$`Date Start`[scr[i]]-startdate),as.numeric(inp$`Date End`[scr[i]]-startdate))
      
#       if(i==1){
#         if (inp$`Date Start`[scr[i]]>startdate){
#           scr_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[scr[i]],(f[i+1]-f[i])*20))
#           screen<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#         }
#         else{
#           scr_vector<-c(rep(inp$`Value`[scr[i]],(f[i+1])*20))
#           screen<-c(rep(1,(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           scr_vector<-c(scr_vector,rep(inp$`Value`[scr[i]],20))
#           screen<-c(screen,rep(1,20))
#         }else{
#           scr_vector<-c(scr_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           screen<-c(screen,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         scr_vector<-c(scr_vector,rep(inp$`Value`[scr[i]],(f[i*2]-f[i*2-1])*20))
#         screen<-c(screen,rep(1,(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(scr)&& f[i*2]<tail(times,1)){
#         scr_vector<-c(scr_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         screen<-c(screen,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     scr_vector<-rep(0,tail(times,1)*20)
#     screen<-rep(0,tail(times,1)*20)
#   }
#   ## handwashing
#   f<-c()
#   hw_vector<-c()
#   handwash<-c()
#   if (length(hw)>=1){
#     for (i in 1:length(hw)){
      
#       f<-c(f,as.numeric(inp$`Date Start`[hw[i]]-startdate),as.numeric(inp$`Date End`[hw[i]]-startdate))
      
#       if(i==1){
#         if (inp$`Date Start`[hw[i]]>startdate){
#           hw_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[hw[i]],(f[i+1]-f[i])*20))
#           handwash<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#         }
#         else{
#           hw_vector<-c(rep(inp$`Value`[hw[i]],(f[i+1])*20))
#           handwash<-c(rep(1,(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           hw_vector<-c(hw_vector,rep(inp$`Value`[hw[i]],20))
#           handwash<-c(handwash,rep(1,20))
#         }else{
#           hw_vector<-c(hw_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           handwash<-c(handwash,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         hw_vector<-c(hw_vector,rep(inp$`Value`[hw[i]],(f[i*2]-f[i*2-1])*20))
#         handwash<-c(handwash,rep(1,(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(hw)&& f[i*2]<tail(times,1)){
#         hw_vector<-c(hw_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         handwash<-c(handwash,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     hw_vector<-rep(0,tail(times,1)*20)
#     handwash<-rep(0,tail(times,1)*20)
#   }
#   ## masking
#   f<-c()
#   msk_vector<-c()
#   masking<-c()
#   if (length(msk)>=1){
#     for (i in 1:length(msk)){
      
#       f<-c(f,as.numeric(inp$`Date Start`[msk[i]]-startdate),as.numeric(inp$`Date End`[msk[i]]-startdate))
#       if(i==1){
#         if (inp$`Date Start`[msk[i]]>startdate){
#           msk_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[msk[i]],(f[i+1]-f[i])*20))
#           masking<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#         }
#         else{
#           msk_vector<-c(rep(inp$`Value`[msk[i]],(f[i+1])*20))
#           masking<-c(rep(1,(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           msk_vector<-c(msk_vector,rep(inp$`Value`[msk[i]],20))
#           masking<-c(masking,rep(1,20))
#         }else{
#           msk_vector<-c(msk_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           masking<-c(masking,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         msk_vector<-c(msk_vector,rep(inp$`Value`[msk[i]],(f[i*2]-f[i*2-1])*20))
#         masking<-c(masking,rep(1,(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(msk)&& f[i*2]<tail(times,1)){
#         msk_vector<-c(msk_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         masking<-c(masking,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     msk_vector<-rep(0,tail(times,1)*20)
#     masking<-rep(0,tail(times,1)*20)
#   }
#   ## dexamethasone
#   f<-c()
#   dex<-c()
#   if (length(dx)>=1){
#     for (i in 1:length(dx)){
#       f<-c(f,as.numeric(inp$`Date Start`[dx[i]]-startdate),as.numeric(inp$`Date End`[dx[i]]-startdate))
      
#       if(i==1){
#         if (inp$`Date Start`[dx[i]]>startdate){
#           dex<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#         }
#         else{
#           dex<-c(rep(1,(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           dex<-c(dex,rep(1,20))
#         }else{
#           dex<-c(dex,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         dex<-c(dex,rep(1,(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(dx)&& f[i*2]<tail(times,1)){
#         dex<-c(dex,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     dex<-rep(0,tail(times,1)*20)
#   }
#   ## working at home
#   f<-c()
#   wah_vector<-c()
#   workhome<-c()
#   if (length(wah)>=1){
#     for (i in 1:length(wah)){
      
#       f<-c(f,as.numeric(inp$`Date Start`[wah[i]]-startdate),as.numeric(inp$`Date End`[wah[i]]-startdate))
#       if(i==1){
#         if (inp$`Date Start`[wah[i]]>startdate){
#           wah_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[wah[i]],(f[i+1]-f[i])*20))
#           workhome<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#         }
#         else{
#           wah_vector<-c(rep(inp$`Value`[wah[i]],(f[i+1])*20))
#           workhome<-c(rep(1,(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           wah_vector<-c(wah_vector,rep(inp$`Value`[wah[i]],20))
#           workhome<-c(workhome,rep(1,20))
#         }else{
#           wah_vector<-c(wah_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           workhome<-c(workhome,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         wah_vector<-c(wah_vector,rep(inp$`Value`[wah[i]],(f[i*2]-f[i*2-1])*20))
#         workhome<-c(workhome,rep(1,(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(wah)&& f[i*2]<tail(times,1)){
#         wah_vector<-c(wah_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         workhome<-c(workhome,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     wah_vector<-rep(0,tail(times,1)*20)
#     workhome<-rep(0,tail(times,1)*20)
#   }
#   ## school closure
#   f<-c()
#   sc_vector<-c()
#   schoolclose<-c()
#   if (length(sc)>=1){
#     for (i in 1:length(sc)){
#       f<-c(f,as.numeric(inp$`Date Start`[sc[i]]-startdate),as.numeric(inp$`Date End`[sc[i]]-startdate))
      
#       if(i==1){
#         if (inp$`Date Start`[sc[i]]>startdate){
#           sc_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[sc[i]],(f[i+1]-f[i])*20))
#           schoolclose<-c(rep(0,f[i]*20),rep(inp$Target[sc[i]],(f[i+1]-f[i])*20))
#         }
#         else{
#           sc_vector<-c(rep(inp$`Value`[sc[i]],(f[i+1])*20))
#           schoolclose<-c(rep(inp$Target[sc[i]],(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           sc_vector<-c(sc_vector,rep(inp$`Value`[sc[i]],20))
#           schoolclose<-c(schoolclose,rep(inp$Target[sc[i]],20))
#         }else{
#           sc_vector<-c(sc_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           schoolclose<-c(schoolclose,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         sc_vector<-c(sc_vector,rep(inp$`Value`[sc[i]],(f[i*2]-f[i*2-1])*20))
#         schoolclose<-c(schoolclose,rep(inp$Target[sc[i]],(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(sc)&& f[i*2]<tail(times,1)){
#         sc_vector<-c(sc_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         schoolclose<-c(schoolclose,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     sc_vector<-rep(0,tail(times,1)*20)
#     schoolclose<-rep(0,tail(times,1)*20)
#   }
#   schoolclose[is.na(schoolclose)]<-1
#   ## partial school closure
#   f<-c()
#   scp_vector<-c()
#   schoolclosepartial<-c()
#   if (length(scp)>=1){
#     for (i in 1:length(scp)){
#       f<-c(f,as.numeric(inp$`Date Start`[sc[i]]-startdate),as.numeric(inp$`Date End`[scp[i]]-startdate))
#       if(i==1){
#         if (inp$`Date Start`[scp[i]]>startdate){
#           scp_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[scp[i]],(f[i+1]-f[i])*20))
#           schoolclosepartial<-c(rep(0,f[i]*20),rep(inp$Target[scp[i]],(f[i+1]-f[i])*20))
#         }
#         else{
#           scp_vector<-c(rep(inp$`Value`[scp[i]],(f[i+1])*20))
#           schoolclosepartial<-c(rep(inp$Target[scp[i]],(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           scp_vector<-c(scp_vector,rep(inp$`Value`[scp[i]],20))
#           schoolclosepartial<-c(schoolclosepartial,rep(inp$Target[scp[i]],20))
#         }else{
#           scp_vector<-c(scp_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           schoolclosepartial<-c(schoolclosepartial,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         scp_vector<-c(scp_vector,rep(inp$`Value`[scp[i]],(f[i*2]-f[i*2-1])*20))
#         schoolclosepartial<-c(schoolclosepartial,rep(inp$Target[scp[i]],(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(scp)&& f[i*2]<tail(times,1)){
#         scp_vector<-c(scp_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         schoolclosepartial<-c(schoolclosepartial,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     scp_vector<-rep(0,tail(times,1)*20)
#     schoolclosepartial<-rep(0,tail(times,1)*20)
#   }
#   schoolclosepartial[is.na(schoolclosepartial)]<-1
#   ## cocooning the elderly
#   f<-c()
#   cte_vector<-c()
#   cocoon<-c()
#   if (length(cte)>=1){
#     for (i in 1:length(cte)){
      
#       f<-c(f,as.numeric(inp$`Date Start`[cte[i]]-startdate),as.numeric(inp$`Date End`[cte[i]]-startdate))
#       if(i==1){
#         if (inp$`Date Start`[cte[i]]>startdate){
#           cte_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[cte[i]],(f[i+1]-f[i])*20))
#           cocoon<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#         }
#         else{
#           cte_vector<-c(rep(inp$`Value`[cte[i]],(f[i+1])*20))
#           cocoon<-c(rep(1,(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           cte_vector<-c(cte_vector,rep(inp$`Value`[cte[i]],20))
#           cocoon<-c(cocoon,rep(1,20))
#         }else{
#           cte_vector<-c(cte_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           cocoon<-c(cocoon,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         cte_vector<-c(cte_vector,rep(inp$`Value`[cte[i]],(f[i*2]-f[i*2-1])*20))
#         cocoon<-c(cocoon,rep(1,(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(cte)&& f[i*2]<tail(times,1)){
#         cte_vector<-c(cte_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         cocoon<-c(cocoon,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     cte_vector<-rep(0,tail(times,1)*20)
#     cocoon<-rep(0,tail(times,1)*20)
#   }
#   ## quarantine
#   f<-c()
#   q_vector<-c()
#   quarantine<-c()
#   if (length(q)>=1){
#     for (i in 1:length(q)){
      
#       f<-c(f,as.numeric(inp$`Date Start`[q[i]]-startdate),as.numeric(inp$`Date End`[q[i]]-startdate))
#       if(i==1){
#         if (inp$`Date Start`[q[i]]>startdate){
#           q_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[q[i]],(f[i+1]-f[i])*20))
#           quarantine<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#         }
#         else{
#           q_vector<-c(rep(inp$`Value`[q[i]],(f[i+1])*20))
#           quarantine<-c(rep(1,(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           q_vector<-c(q_vector,rep(inp$`Value`[q[i]],20))
#           quarantine<-c(quarantine,rep(1,20))
#         }else{
#           q_vector<-c(q_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           quarantine<-c(quarantine,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         q_vector<-c(q_vector,rep(inp$`Value`[q[i]],(f[i*2]-f[i*2-1])*20))
#         quarantine<-c(quarantine,rep(1,(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(q)&& f[i*2]<tail(times,1)){
#         q_vector<-c(q_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         quarantine<-c(quarantine,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     q_vector<-rep(0,tail(times,1)*20)
#     quarantine<-rep(0,tail(times,1)*20)
#   }
#   ## travel ban
#   f<-c()
#   tb_vector<-c()
#   travelban<-c()
#   if (length(tb)>=1){
#     for (i in 1:length(tb)){
#       f<-c(f,as.numeric(inp$`Date Start`[tb[i]]-startdate),as.numeric(inp$`Date End`[tb[i]]-startdate))
#       if(i==1){
#         if (inp$`Date Start`[tb[i]]>startdate){
#           tb_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[tb[i]],(f[i+1]-f[i])*20))
#           travelban<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#         }
#         else{
#           tb_vector<-c(rep(inp$`Value`[tb[i]],(f[i+1])*20))
#           travelban<-c(rep(1,(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           tb_vector<-c(tb_vector,rep(inp$`Value`[tb[i]],20))
#           travelban<-c(travelban,rep(1,20))
#         }else{
#           tb_vector<-c(tb_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           travelban<-c(travelban,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         tb_vector<-c(tb_vector,rep(inp$`Value`[tb[i]],(f[i*2]-f[i*2-1])*20))
#         travelban<-c(travelban,rep(1,(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(tb)&& f[i*2]<tail(times,1)){
#         tb_vector<-c(tb_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         travelban<-c(travelban,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     tb_vector<-rep(0,tail(times,1)*20)
#     travelban<-rep(0,tail(times,1)*20)
#   }
#   ## mass testing
#   f<-c()
#   mt_vector<-c()
#   masstesting<-c()
#   testage<-c()
#   if (length(mt)>=1){
#     for (i in 1:length(mt)){
#       f<-c(f,as.numeric(inp$`Date Start`[mt[i]]-startdate),as.numeric(inp$`Date End`[mt[i]]-startdate))
      
#       if(i==1){
#         if (inp$`Date Start`[mt[i]]>startdate){
#           mt_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[mt[i]],(f[i+1]-f[i])*20))
#           masstesting<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#           testage<-c(rep(0,f[i]*20),rep(inp$`Target`[mt[i]],(f[i+1]-f[i])*20))
#         }
#         else{
#           mt_vector<-c(rep(inp$`Value`[mt[i]],(f[i+1])*20))
#           masstesting<-c(rep(1,(f[i+1])*20))
#           testage<-c(rep(inp$`Target`[mt[i]],(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           mt_vector<-c(mt_vector,rep(inp$`Value`[mt[i]],20))
#           masstesting<-c(masstesting,rep(1,20))
#           testage<-c(testage,rep(inp$`Target`[mt[i]],20))
#         }else{
#           mt_vector<-c(mt_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           masstesting<-c(masstesting,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           testage<-c(testage,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         mt_vector<-c(mt_vector,rep(inp$`Value`[mt[i]],(f[i*2]-f[i*2-1])*20))
#         masstesting<-c(masstesting,rep(1,(f[i*2]-f[i*2-1])*20))
#         testage<-c(testage,rep(inp$`Target`[mt[i]],(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(mt)&& f[i*2]<tail(times,1)){
#         mt_vector<-c(mt_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         masstesting<-c(masstesting,rep(0,(tail(times,1)-f[i*2])*20))
#         testage<-c(testage,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     mt_vector<-rep(0,tail(times,1)*20)
#     masstesting<-rep(0,tail(times,1)*20)
#     testage<-rep(0,tail(times,1)*20)
#   }
#   ## vaccine
#   f<-c()
#   vc_vector<-c()
#   vaccine<-c()
#   vaccineage<-c()
#   if (length(vc)>=1){
#     for (i in 1:length(vc)){
#       f<-c(f,as.numeric(inp$`Date Start`[vc[i]]-startdate),as.numeric(inp$`Date End`[vc[i]]-startdate))
#       if(i==1){
#         if (inp$`Date Start`[vc[i]]>startdate){
#           vc_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[vc[i]],(f[i+1]-f[i])*20))
#           vaccine<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#           vaccineage<-c(rep(0,f[i]*20),rep(inp$`Target`[vc[i]],(f[i+1]-f[i])*20))
#         }
#         else{
#           vc_vector<-c(rep(inp$`Value`[vc[i]],(f[i+1])*20))
#           vaccine<-c(rep(1,(f[i+1])*20))
#           vaccineage<-c(rep(inp$`Target`[vc[i]],(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           vc_vector<-c(vc_vector,rep(inp$`Value`[vc[i]],20))
#           vaccine<-c(vaccine,rep(1,20))
#           vaccineage<-c(vaccineage,rep(inp$`Target`[vc[i]],20))
#         }else{
#           vc_vector<-c(vc_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           vaccine<-c(vaccine,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           vaccineage<-c(vaccineage,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         vc_vector<-c(vc_vector,rep(inp$`Value`[vc[i]],(f[i*2]-f[i*2-1])*20))
#         vaccine<-c(vaccine,rep(1,(f[i*2]-f[i*2-1])*20))
#         vaccineage<-c(vaccineage,rep(inp$`Target`[vc[i]],(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(vc)&& f[i*2]<tail(times,1)){
#         vc_vector<-c(vc_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         vaccine<-c(vaccine,rep(0,(tail(times,1)-f[i*2])*20))
#         vaccineage<-c(vaccineage,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     vc_vector<-rep(0,tail(times,1)*20)
#     vaccine<-rep(0,tail(times,1)*20)
#     vaccineage<-rep(0,tail(times,1)*20)
#   }
#   ## vaccine partial
#   f<-c()
#   vcp_vector<-c()
#   vaccinep<-c()
#   vaccineagepartial<-c()
#   if (length(vcp)>=1){
#     for (i in 1:length(vcp)){
#       f<-c(f,as.numeric(inp$`Date Start`[vcp[i]]-startdate),as.numeric(inp$`Date End`[vcp[i]]-startdate))
#       if(i==1){
#         if (inp$`Date Start`[vcp[i]]>startdate){
#           vcp_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[vcp[i]],(f[i+1]-f[i])*20))
#           vaccinep<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
#           vaccineagepartial<-c(rep(0,f[i]*20),rep(inp$`Target`[vcp[i]],(f[i+1]-f[i])*20))
#         }
#         else{
#           vcp_vector<-c(rep(inp$`Value`[vcp[i]],(f[i+1])*20))
#           vaccinep<-c(rep(1,(f[i+1])*20))
#           vaccineagepartial<-c(rep(inp$`Target`[vcp[i]],(f[i+1])*20))
#         }
#       }
#       else{
#         if (f[(i-1)*2+1]-f[(i-1)*2]==1){
#           vcp_vector<-c(vcp_vector,rep(inp$`Value`[vcp[i]],20))
#           vaccinep<-c(vaccinep,rep(1,20))
#           vaccineagepartial<-c(vaccineagepartial,rep(inp$`Target`[vcp[i]],20))
#         }else{
#           vcp_vector<-c(vcp_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           vaccinep<-c(vaccinep,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#           vaccineagepartial<-c(vaccineagepartial,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
#         }
#         vcp_vector<-c(vcp_vector,rep(inp$`Value`[vcp[i]],(f[i*2]-f[i*2-1])*20))
#         vaccinep<-c(vaccinep,rep(1,(f[i*2]-f[i*2-1])*20))
#         vaccineagepartial<-c(vaccineagepartial,rep(inp$`Target`[vcp[i]],(f[i*2]-f[i*2-1])*20))
#       }
#       if(i==length(vcp)&& f[i*2]<tail(times,1)){
#         vcp_vector<-c(vcp_vector,rep(0,(tail(times,1)-f[i*2])*20))
#         vaccinep<-c(vaccinep,rep(0,(tail(times,1)-f[i*2])*20))
#         vaccineagepartial<-c(vaccineagepartial,rep(0,(tail(times,1)-f[i*2])*20))
#       }
#     }
#   }else{
#     vcp_vector<-rep(0,tail(times,1)*20)
#     vaccinep<-rep(0,tail(times,1)*20)
#     vaccineagepartial<-rep(0,tail(times,1)*20)
#   }
 
#   return(list(si_vector=si_vector,sd_vector=sd_vector,scr_vector=scr_vector,hw_vector=hw_vector,msk_vector=msk_vector,
#               wah_vector=wah_vector,sc_vector=sc_vector,scp_vector=scp_vector,tb_vector=tb_vector,mt_vector=mt_vector*1000,
#               cte_vector=cte_vector,q_vector=q_vector,vc_vector=vc_vector,vcp_vector=vcp_vector,isolation=isolation,
#               screen=screen,cocoon=cocoon,schoolclose=schoolclose,schoolclosepartial=schoolclosepartial,
#               workhome=workhome,handwash=handwash,masking=masking,
#               quarantine=quarantine,vaccine=vaccine,vaccinep=vaccinep,travelban=travelban,distancing=distancing,
#               masstesting=masstesting,testage=testage,vaccineage=vaccineage,vaccineagepartial=vaccineagepartial,dex=dex))
# }




# # set up a function to solve the equations
# covid<-function(t, Y, parameters,input) 
# {
#   with(as.list(c(Y, parameters)),
#        {
#          S <- Y[Sindex]
#          E <- Y[Eindex]
#          I <- Y[Iindex]
#          R <- Y[Rindex]
#          X <- Y[Xindex]
#          Z <- Y[Zindex]
#          H <- Y[Hindex]
#          HC <- Y[HCindex]
#          C <- Y[Cindex]
#          CM <- Y[CMindex]
#          V <- Y[Vindex]
#          QS <- Y[QSindex]
#          QE <- Y[QEindex]
#          QI <- Y[QIindex]
#          QR <- Y[QRindex]
#          CL <- Y[CLindex]
#          QC <- Y[QCindex]
#          ICU <- Y[ICUindex]
#          ICUC <- Y[ICUCindex]
#          ICUCV <-Y[ICUCVindex]
#          Vent <- Y[Ventindex]
#          VentC <- Y[VentCindex]
#          CMC <- Y[CMCindex]
#          EV <- Y[EVindex]
#          ER <- Y[ERindex]
#          EVR <- Y[EVRindex]
#          VR <- Y[VRindex]
#          QV <- Y[QVindex]
#          QEV <- Y[QEVindex]
#          QER <- Y[QERindex]
#          QEVR <- Y[QEVRindex]
#          QVR <- Y[QVRindex]
#          HCICU <- Y[HCICUindex]
#          HCV <- Y[HCVindex]
#          Ab <- Y[Abindex]
         
#          P <- (S+E+I+R+X+Z+V+H+HC+ICU+ICUC+ICUCV+Vent+VentC+EV+ER+EVR+VR+HCICU+HCV+
#                  QS+QE+QI+QR+CL+QC+QEV+QV+QER+QEVR+QVR)
#          Q <- (sum(QS)+sum(QE)+sum(QI)+sum(QC)+sum(QR)+sum(QV)+sum(QER)+sum(QEVR)+sum(QEV)+sum(QVR))/sum(P)
#          # print(HCICUindex)
#          # print(sum(ER))
#          # print(t)
#          # print(paste("EVR",sum(QE)))
#          # print(paste("ER",sum(QR)))
#          # print(paste("QER",sum(QER)))
#          # print(paste("QEVR",sum(QEVR)))
#          # print(paste("QV",sum(QV)))
#          # print(paste("QC",sum(QC)))
#          # print(paste("QI",sum(QI)))
#          # print(paste("QEV",sum(QV)))
#          # print(paste("QE",sum(QC)))
#          # print(paste("QR",sum(QR)))
#          # health system performance
#          critH<-min(1-fH(sum(H)+sum(ICUC)+sum(ICUCV)),1)
#          crit<-min(1-fICU(sum(ICU)+sum(Vent)+sum(VentC)),1)
#          critV<-min(1-fVent(sum(Vent)),1)

# to_keep = 10000000
# critH <- round(critH * to_keep) / to_keep
# crit  <- round(crit  * to_keep) / to_keep
# critV <- round(critV * to_keep) / to_keep
         
#          # interventions
#          isolation<-input$isolation[t*20+1]
#          distancing<-input$distancing[t*20+1]
#          handwash<-input$handwash[t*20+1]
#          masking<-input$masking[t*20+1]
#          workhome<-input$workhome[t*20+1]
#          schoolclose<-input$schoolclose[t*20+1]
#          schoolclosep<-input$schoolclosepartial[t*20+1]
#          cocoon<-input$cocoon[t*20+1]
#          vaccine<-input$vaccine[t*20+1]
#          travelban<-input$travelban[t*20+1]
#          screen<-input$screen[t*20+1]
#          quarantine<-input$quarantine[t*20+1]
#          masstesting<-input$masstesting[t*20+1]
#          dexamethasone<-input$dex[t*20+1]
         
#          screen_eff<-0
#          selfis<-0
#          school<-1
#          dist<-1
#          hand<-0
#          mask<-0
#          vaccinate<-0
#          trvban_eff<-0
#          quarantine_rate<-0
#          tests_per_day<-0
         
#          selfis_cov<-(input$si_vector[t*20+1])/100
#          screen_contacts<-(input$scr_vector[t*20+1])/10
#          school_eff<-(input$sc_vector[t*20+1])/100
#          school_effp<-(input$scp_vector[t*20+1])/100
#          dist_cov<-(input$sd_vector[t*20+1])/100
#          hand_cov<-(input$hw_vector[t*20+1])/100
#          mask_cov<-(input$msk_vector[t*20+1])/100
#          cocoon<-(input$cte_vector[t*20+1])/100
#          work_cov<-(input$wah_vector[t*20+1])/100
#          travelban_eff<-(input$tb_vector[t*20+1])/100
#          vaccine_cov<-(input$vc_vector[t*20+1])/100
#          vaccine_covp<-(input$vcp_vector[t*20+1])/100
#          quarantine_cov<-(input$q_vector[t*20+1])/100
#          tests_per_day<-(input$mt_vector[t*20+1])
#          vaccineage<-input$vaccineage[t*20+1]
#          vaccineagep<-input$vaccineagepartial[t*20+1]
#          testage<-input$testage[t*20+1]
         
#          # if (vaccine && !vaccinep){
#          #   age_vaccine_vector<-as.numeric(age_group_vectors[[vaccineage]])
#          #   vac_rate<-(-log(1-vaccine_cov)/vac_campaign)
#          #   vaccinate<-vac_rate
#          # }
#          # if(vaccine && vaccinep){
#          #   age_vaccine_vector<-max(as.numeric(age_group_vectors[[vaccineage]])as.numeric(age_group_vectors[[vaccineagepartial]])
#          #   vac_rate<-(-log(1-vaccine_cov)/vac_campaign)
#          #   vaccinate<-vac_rate
#          # }
#          if (vaccine){
#            age_vaccine_vector<-as.numeric(age_group_vectors[[vaccineage]])
#            vac_rate<-(-log(1-vaccine_cov)/vac_campaign)
#            vaccinate<-vac_rate
#          }
#          else{age_vaccine_vector<-rep(0,A)}
#          # print(vaccinate*age_vaccine_vector)
#          if (masstesting){
#            age_testing_vector<-as.numeric(age_group_vectors[[testage]])
#          }else{age_testing_vector<-rep(0,A)}
#          if (workhome){
#            work<-work_cov*work_eff
#          }else{work<-1}
#          if (isolation){
#            selfis<-selfis_cov
#            if(screen){
#              screen_eff<-min(sum(report*I+reportc*(CL)+H+ICU+Vent+reporth*(HC+ICUC+ICUCV+VentC+HCICU+HCV))*screen_contacts*(screen_overdispersion*I/P)*screen_test_sens/P,1) 
#            }
#          }
#          if (schoolclose>=1 && schoolclosep>=1){
#            school<-school_eff
#            school2<-school_effp
#            # print(dim(schoolclose))
#            schoolclose2<-as.numeric(age_group_vectors[[schoolclose]])
#            schoolclose2p<-as.numeric(age_group_vectors[[schoolclosep]])
#            schoolclose3<-pmax(school*schoolclose2,school2*schoolclose2p)
#            schoolclose4<-pmax((1-school)*schoolclose2,(1-school2)*schoolclose2p)
#          }
#          if (schoolclose>=1 && schoolclosep==0){
#            school<-school_eff
#            schoolclose2<-as.numeric(age_group_vectors[[schoolclose]])
#            schoolclose3<-school*schoolclose2
#            schoolclose4<-(1-school)*schoolclose2
#          }
#          if (schoolclose==0 && schoolclosep>=1){
#            school<-school_effp
#            schoolclose2<-as.numeric(age_group_vectors[[schoolclosep]])
#            schoolclose3<-school*schoolclose2
#            schoolclose4<-(1-school)*schoolclose2
#          }
#          if (schoolclose==0 && schoolclosep==0){
#            schoolclose3<-rep(0,A);schoolclose4<-rep(0,A)
#          }
#          # print(schoolclose3)
#          # print(schoolclose)
#          if(distancing){
#            dist<-dist_cov*dist_eff
#          }
#          if(handwash){
#            hand<-hand_eff*hand_cov
#          }
#          if(masking){
#            mask<-mask_eff*mask_cov
#          }
#          if(travelban){
#            trvban_eff<-travelban_eff
#          }
#          if(quarantine){
#            rate_q<-min((min(sum((CL+H+ICU+Vent+HC+ICUC+ICUCV+VentC+HCV+HCICU))*(household_size-1)/sum(P),1)*quarantine_effort),quarantine_cov/2)
#            quarantine_rate<-rate_q/(1+exp(-10*(quarantine_cov/2-Q)))
#          }
#          if(dexamethasone){
#            prob_v<-prob_vent*vent_dex
#            dexo2<-parameters["dexo2"];dexo2c<-parameters["dexo2c"];dexv<-parameters["dexv"];dexvc<-parameters["dexvc"];
#          }else{
#            dexo2<-1;dexo2c<-1;dexv<-1;dexvc<-1;prob_v<-prob_vent;
#          }
#          # print(paste(dexo2,dexo2c,dexv,dexvc,prob_v))
#          # print(paste("quarantine_rate",quarantine_rate))
#          # print(quarantine_rate*sum(R))
#          # print(quarantine_rate*ER)
#          # 
#          # print(sum(vaccinate*age_vaccine_vector*R))
#          # print((1/quarantine_days)*sum(QR))
         
#          # testing rates
#          propI<-sum(I)/sum(P)
#          propC<-sum(CL)/sum(P)
#          propE<-sum(E)/sum(P)
#          propEV<-sum(EV)/sum(P)
#          propER<-sum(ER)/sum(P)
#          propEVR<-sum(EVR)/sum(P)
#          propHC<-sum(HC)/sum(P)
#          propHCICU<-sum(HCICU)/sum(P)
#          propHCV<-sum(HCV)/sum(P)
#          testE<-tests_per_day*propE
#          testEV<-tests_per_day*propEV
#          testER<-tests_per_day*propER
#          testEVR<-tests_per_day*propEVR
#          testI<-tests_per_day*propI
#          testC<-tests_per_day*propC
#          testHC<-tests_per_day*propHC
#          testHCICU<-tests_per_day*propHCICU
#          testHCV<-tests_per_day*propHCV
         
#          if(sum(I)>1){
#            ratetestI<-mass_test_sens*testI/sum(I)
#            # print(paste('rateI: ',ratetestI))
#          }else{ratetestI<-0}
#          if(sum(CL)>1){
#            ratetestC<-mass_test_sens*testC/sum(CL)
#            # print(paste('rateC: ',ratetestC))
#          }else{ratetestC<-0}
#          # print(sum(E))
#          if(sum(E)>1){
#            ratetestE<-mass_test_sens*testE/sum(E)
#          }else{ratetestE<-0}
#          if(sum(EV)>1){
#            ratetestEV<-mass_test_sens*testEV/sum(EV)
#            # print(paste('rateEV: ',ratetestEV))
#          }else{ratetestEV<-0}
#          if(sum(ER)>1){
#            ratetestER<-mass_test_sens*testER/sum(ER)
#            # print(paste('rateER: ',ratetestER))
#          }else{ratetestER<-0}
#          if(sum(EVR)>1){
#            ratetestEVR<-mass_test_sens*testEVR/sum(EVR)
#          }else{ratetestEVR<-0}
#          if(sum(HC)>1){
#            ratetestHC<-mass_test_sens*testHC/sum(HC)
#          }else{ratetestHC<-0}
#          if(sum(HCICU)>1){
#            ratetestHCICU<-mass_test_sens*testHCICU/sum(HCICU)
#          }else{ratetestHCICU<-0}
#          if(sum(HCV)>1){
#            ratetestHCV<-mass_test_sens*testHCV/sum(HCV)
#          }else{ratetestHCV<-0}
         
#          # print(mass_test_sens)
#          # print(ratetestI*sum(I) + ratetestC*sum(CL) - (1/isolation_days)*sum(Z) )
#          # print(propC)
#          # print(testI)
#          # print(testC)
#          # 
#          # cocooning the elderly
#          cocoon_mat<-matrix((1-cocoon_eff),nrow = length(popstruc$pop),ncol = length(popstruc$pop))
#          cocoon_mat[1:(age_cocoon-1),1:(age_cocoon-1)]<-1
         
#          # contact matrices
#          cts<-(contact_home+distancing*(1-dist)*contact_other+(1-distancing)*contact_other+
#                +(1-schoolclose3)*contact_school
#                +schoolclose4* contact_school
#                +schoolclose3*contact_home*s2h
#                # +sweep(contact_school, MARGIN=2, (1-schoolclose3), `*`) # school on
#                # +sweep(contact_school, MARGIN=2, schoolclose4, `*`)     # school close
#                # +sweep(contact_home*s2h, MARGIN=2, schoolclose3, `*`)   # inflating contacts at home when school closes
#                +(1-workhome)*contact_work  # normal work
#                +workhome*(1-work)*contact_work # people not working from home when homework is active
#                +contact_home*workhome*work*w2h # inflating contacts at home when working from home
#          )
#          # print(sum(rowSums(cts)))
         
#          # print(paste("time",t,"  cts: ",length(cts)))
#          # Final transmission related parameters
#          contacts <- (1-cocoon)*cts+cocoon*cts*cocoon_mat+cocoon*(1+schoolclose3*(1-school_eff)+workhome*(1-work_eff))*contact_home*(1-cocoon_mat)
#          seas <- 1+amp*cos(2*3.14*(t-(phi*365.25/12))/365.25)
#          importation <- mean_imports*(1-trvban_eff)
#          HH<-H+ICU+Vent+ICUC+ICUCV+VentC
#          HHC<-HC+HCICU+HCV
#          lam <- (1-max(hand,mask))*p*seas*(contacts%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC)+rhos*(HH))/P))+
#            (1-max(hand,mask))*p*seas*(1-quarantine*quarantine_eff_other)*(contact_other%*%((rho*QE+QI+QC+QEV+QEVR+QER)/P))
#          # contacts under home quarantine
#          lamq<-(1-max(hand,mask))*p*seas*((1-quarantine_eff_home)*contact_home%*%(((1-selfis_eff)*(X+HHC+rho*QE+QI+QC++QEV+QEVR+QER))/P))+
#            (1-max(hand,mask))*p*seas*(1-quarantine_eff_other)*(contact_other%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC+rho*QE+QI+QC++QEV+QEVR+QER)+rhos*(HH))/P))
#          # lamq<-0
#          # print(paste("lamq",lamq))
#          # print(paste("lamq",(1-vaccine_eff_r)*lamq*sum(QVR) ))
#          # print(paste("quarantine evr",quarantine_rate*sum(EVR) ))
         
#          # birth/death
#          b1<-sum(popbirth[,2]*popstruc[,2])
#          birth<-0*popbirth[,2]
#          birth[1]<-b1
         
#          # ODE system
#          dSdt <- -S*lam-vaccinate*age_vaccine_vector*S+omega*R+vac_dur*V-
#            quarantine_rate*S +(1/quarantine_days)*QS+ageing%*%S-mort*S+birth
#          dEdt <- S*lam-gamma*E+ageing%*%E- vaccinate*age_vaccine_vector*E - mort*E -
#            quarantine_rate*E+(1/quarantine_days)*QE
#          dIdt <- gamma*(1-pclin)*(1-age_testing_vector*ratetestE)*(1-screen_eff)*(1-ihr[,2])*E+
#            gamma*(1-pclin_v)*(1-age_testing_vector*ratetestEV)*(1-screen_eff)*(1-sigmaEV*ihr[,2])*EV+
#            gamma*(1-pclin_vr)*(1-age_testing_vector*ratetestEVR)*(1-screen_eff)*(1-sigmaEVR*ihr[,2])*EVR+
#            gamma*(1-pclin_r)*(1-age_testing_vector*ratetestER)*(1-screen_eff)*(1-sigmaER*ihr[,2])*ER-
#            vaccinate*age_vaccine_vector*I - nui*I+ageing%*%I-mort*I + 
#            (1/quarantine_days)*QI - quarantine_rate*I - ratetestI*age_testing_vector*I
#          dCLdt<- gamma*pclin*(1-age_testing_vector*ratetestE)*(1-selfis)*(1-ihr[,2])*(1-quarantine_rate)*E+
#            gamma*pclin_v*(1-age_testing_vector*ratetestEV)*(1-selfis)*(1-sigmaEV*ihr[,2])*(1-quarantine_rate)*EV+
#            gamma*pclin_vr*(1-age_testing_vector*ratetestEVR)*(1-selfis)*(1-sigmaEVR*ihr[,2])*(1-quarantine_rate)*EVR+
#            gamma*pclin_r*(1-age_testing_vector*ratetestER)*(1-selfis)*(1-sigmaER*ihr[,2])*(1-quarantine_rate)*ER-
#            nui*CL+ageing%*%CL-mort*CL  + (1/quarantine_days)*QC - ratetestC*age_testing_vector*CL
#          dRdt <- vac_dur_r*VR-omega*R-vaccinate*age_vaccine_vector*R-lam*sigmaR*R - quarantine_rate*R+
#            nui*I+nui*X+nui*CL+ageing%*%R-mort*R + (1/isolation_days)*Z+(1/quarantine_days)*QR+ 
#            nus*propo2*(1-dexo2*pdeath_ho)*ifr[,2]*H+nus*(1-propo2)*(1-pdeath_h)*ifr[,2]*H+
#            nusc*propo2*(1-pdeath_hco)*ifr[,2]*HC+nusc*(1-propo2)*(1-pdeath_hc)*ifr[,2]*HC+  
#            nusc*propo2*(1-pdeath_icu_hco)*ifr[,2]*HCICU+nusc*(1-propo2)*(1-pdeath_icu_hc)*ifr[,2]*HCICU+
#            nu_icu*propo2*(1-dexo2*pdeath_icuo)*ifr[,2]*ICU+nu_icu*(1-propo2)*(1-pdeath_icu)*ifr[,2]*ICU+
#            nu_icuc*propo2*(1-dexo2c*pdeath_icuco)*ifr[,2]*ICUC+nu_icuc*(1-propo2)*(1-pdeath_icuc)*ifr[,2]*ICUC+
#            nu_vent*(1-dexv*pdeath_vent)*ifr[,2]*Vent+
#            nu_ventc*(1-pdeath_vent_hc)*ifr[,2]*HCV+
#            nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*VentC+nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*ICUCV 
#          dXdt <- gamma*selfis*(1-age_testing_vector*ratetestE)*pclin*(1-ihr[,2])*E+
#            gamma*(1-pclin)*(1-age_testing_vector*ratetestE)*screen_eff*(1-ihr[,2])*E+
#            gamma*selfis*(1-age_testing_vector*ratetestEV)*pclin_v*(1-sigmaEV*ihr[,2])*EV+
#            gamma*(1-pclin_v)*(1-age_testing_vector*ratetestEV)*screen_eff*(1-sigmaEV*ihr[,2])*EV+
#            gamma*selfis*(1-age_testing_vector*ratetestEVR)*pclin_v*(1-sigmaEVR*ihr[,2])*EVR+
#            gamma*(1-pclin_vr)*(1-age_testing_vector*ratetestEVR)*screen_eff*(1-sigmaEVR*ihr[,2])*EVR+
#            gamma*selfis*(1-age_testing_vector*ratetestER)*pclin_r*(1-sigmaER*ihr[,2])*ER+
#            gamma*(1-pclin_r)*(1-age_testing_vector*ratetestER)*screen_eff*(1-sigmaER*ihr[,2])*ER+
#            -nui*X+ageing%*%X-mort*X 
#          dVdt <- vaccinate*age_vaccine_vector*S + omega*VR - (1-vaccine_eff)*lam*V - vac_dur*V + ageing%*%V-mort*V - quarantine_rate*V
#          dEVdt<- (1-vaccine_eff)*lam*V - gamma*EV + ageing%*%EV - mort*EV - quarantine_rate*EV +(1/quarantine_days)*QEV
#          dERdt<- lam*sigmaR*R - gamma*ER + ageing%*%ER - mort*ER - quarantine_rate*ER +(1/quarantine_days)*QER
#          dVRdt <- vaccinate*age_vaccine_vector*E + vaccinate*age_vaccine_vector*I + vaccinate*age_vaccine_vector*R -
#            (1-vaccine_eff_r)*lam*VR - vac_dur_r*VR + ageing%*%VR - mort*VR - omega*VR - quarantine_rate*VR + (1/quarantine_days)*QVR
#          dEVRdt<- (1-vaccine_eff_r)*lam*VR - gamma*EVR + ageing%*%EVR-mort*EVR - quarantine_rate*EVR +
#            (1/quarantine_days)*QEVR
         
         
#          dQSdt <- quarantine_rate*S + ageing%*%QS-mort*QS - (1/quarantine_days)*QS - lamq*QS
#          dQEdt <- quarantine_rate*E - gamma*QE + ageing%*%QE-mort*QE - (1/quarantine_days)*QE + lamq*QS 
#          dQIdt <- quarantine_rate*I + gamma*(1-ihr[,2])*(1-pclin)*QE+
#            gamma*(1-sigmaEV*ihr[,2])*(1-pclin_v)*QEV+
#            gamma*(1-sigmaER*ihr[,2])*(1-pclin_r)*QER+           
#            gamma*(1-sigmaEVR*ihr[,2])*(1-pclin_vr)*QEVR-
#            nui*QI+ageing%*%QI-mort*QI - (1/quarantine_days)*QI
#          dQCdt <- gamma*pclin*(1-selfis)*(1-age_testing_vector*ratetestE)*(1-ihr[,2])*quarantine_rate*E+
#            gamma*pclin_v*(1-age_testing_vector*ratetestEV)*(1-selfis)*(1-sigmaEV*ihr[,2])*quarantine_rate*EV+
#            gamma*pclin_vr*(1-age_testing_vector*ratetestEVR)*(1-selfis)*(1-sigmaEVR*ihr[,2])*quarantine_rate*EVR+
#            gamma*pclin_r*(1-age_testing_vector*ratetestER)*(1-selfis)*(1-sigmaER*ihr[,2])*quarantine_rate*ER+
#            gamma*(1-ihr[,2])*pclin*QE + 
#            gamma*(1-sigmaEV*ihr[,2])*pclin_v*QEV + 
#            gamma*(1-sigmaER*ihr[,2])*pclin_r*QER + 
#            gamma*(1-sigmaEVR*ihr[,2])*pclin_vr*QEVR -
#            nui*QC+ageing%*%QC-mort*QC - (1/quarantine_days)*QC
#          dQRdt <- quarantine_rate*R + nui*QI + nui*QC + ageing%*%QR-mort*QR - (1/quarantine_days)*QR + vac_dur_r*QVR
#          dQVdt <- quarantine_rate*V + ageing%*%QV-mort*QV - (1/quarantine_days)*QV - (1-vaccine_eff)*lamq*QV + omega*QVR 
#          dQEVdt <- quarantine_rate*EV - gamma*QEV + ageing%*%QEV-mort*QEV - (1/quarantine_days)*QEV + (1-vaccine_eff)*lamq*QV 
#          dQERdt <- quarantine_rate*ER - gamma*QER + ageing%*%QER-mort*QER - (1/quarantine_days)*QER + sigmaR*lamq*QR 
#          dQVRdt <- quarantine_rate*VR - (1-vaccine_eff_r)*lam*QVR - vac_dur_r*QVR - omega*QVR + ageing%*%QVR - mort*QVR 
#          dQEVRdt <- quarantine_rate*EVR - gamma*QEVR +ageing%*%QEVR-mort*QEVR -
#            (1/quarantine_days)*QEVR +(1-vaccine_eff_r)*lamq*QVR 
            
         
#          dHdt <- gamma*ihr[,2]*(1-prob_icu)*(1-critH)*reporth*E+ 
#            gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*(1-critH)*reporth*EV + 
#            gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*(1-critH)*reporth*EVR + 
#            gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*(1-critH)*reporth*ER + 
#            gamma*ihr[,2]*(1-prob_icu)*(1-critH)*reporth*QE +
#            gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*(1-critH)*reporth*QEV + 
#            gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*(1-critH)*reporth*QEVR + 
#            gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*(1-critH)*reporth*QER - 
#            nus*H + ageing%*%H-mort*H 
#          dHCdt <- gamma*ihr[,2]*(1-prob_icu)*(1-reporth)*E+gamma*ihr[,2]*(1-prob_icu)*critH*reporth*E + 
#            gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*(1-reporth)*EV+gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*critH*reporth*EV+
#            gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*(1-reporth)*EVR+gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*critH*reporth*EVR+
#            gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*(1-reporth)*ER+gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*critH*reporth*ER +
#            gamma*ihr[,2]*(1-prob_icu)*(1-reporth)*QE+gamma*ihr[,2]*(1-prob_icu)*critH*reporth*QE+
#            gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*(1-reporth)*QEV+gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*critH*reporth*QEV+
#            gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*(1-reporth)*QEVR+gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*critH*reporth*QEVR+
#            gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*(1-reporth)*QER+gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*critH*reporth*QER - 
#            nusc*HC + ageing%*%HC-mort*HC - ratetestHC*age_testing_vector*HC
#          dHCICUdt <- gamma*(1-reporth_ICU)*ihr[,2]*prob_icu*(1-prob_v)*E+
#            gamma*(1-reporth_ICU)*sigmaEV*ihr[,2]*prob_icu_v*(1-prob_v_v)*EV+
#            gamma*(1-reporth_ICU)*sigmaEVR*ihr[,2]*prob_icu_vr*(1-prob_v_vr)*EVR+
#            gamma*(1-reporth_ICU)*sigmaER*ihr[,2]*prob_icu_r*(1-prob_v_r)*ER+
#            gamma*(1-reporth_ICU)*ihr[,2]*prob_icu*(1-prob_v)*QE+
#            gamma*(1-reporth_ICU)*sigmaEV*ihr[,2]*prob_icu_v*(1-prob_v_v)*QEV+
#            gamma*(1-reporth_ICU)*sigmaEVR*ihr[,2]*prob_icu_vr*(1-prob_v_vr)*QEVR+
#            gamma*(1-reporth_ICU)*sigmaER*ihr[,2]*prob_icu_r*(1-prob_v_r)*QER-
#            nusc*HCICU + ageing%*%HCICU-mort*HCICU - ratetestHCICU*age_testing_vector*HCICU
#          dHCVdt <- gamma*(1-reporth_ICU)*ihr[,2]*prob_icu*prob_v*E+
#            gamma*(1-reporth_ICU)*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*EV+
#            gamma*(1-reporth_ICU)*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*EVR+
#            gamma*(1-reporth_ICU)*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*ER+
#            gamma*(1-reporth_ICU)*ihr[,2]*prob_icu*prob_v*QE+
#            gamma*(1-reporth_ICU)*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*QEV+
#            gamma*(1-reporth_ICU)*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*QEVR+
#            gamma*(1-reporth_ICU)*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*QER-
#            nu_ventc*HCV + ageing%*%HCV-mort*HCV - ratetestHCV*age_testing_vector*HCV 
#          dICUdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*(1-crit)*(1-prob_v)*E+ 
#            gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*(1-crit)*(1-prob_v_v)*EV+
#            gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*(1-crit)*(1-prob_v_vr)*EVR+
#            gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*(1-crit)*(1-prob_v_r)*ER+
#            gamma*reporth_ICU*ihr[,2]*prob_icu*(1-crit)*(1-prob_v)*QE+ 
#            gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*(1-crit)*(1-prob_v_v)*QEV+
#            gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*(1-crit)*(1-prob_v_vr)*QEVR+
#            gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*(1-crit)*(1-prob_v_r)*QER - 
#            nu_icu*ICU +ageing%*%ICU - mort*ICU + (1-crit)*ICUC*1/2
#          dICUCdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*crit*(1-prob_v)*E+
#            gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*crit*(1-prob_v_v)*EV+
#            gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*crit*(1-prob_v_vr)*EVR+
#            gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*crit*(1-prob_v_r)*ER+
#            gamma*reporth_ICU*ihr[,2]*prob_icu*crit*(1-prob_v)*QE+
#            gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*crit*(1-prob_v_v)*QEV+
#            gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*crit*(1-prob_v_vr)*QEVR+
#            gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*crit*(1-prob_v_r)*QER - 
#            nu_icuc*ICUC -(1-crit)*ICUC*1/2 +ageing%*%ICUC - mort*ICUC 
#          dICUCVdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*prob_v*crit*E+
#            gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*crit*EV+
#            gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*crit*EVR+
#            gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*crit*ER+
#            gamma*reporth_ICU*ihr[,2]*prob_icu*prob_v*crit*QE+
#            gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*crit*QEV+
#            gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*crit*QEVR+
#            gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*crit*QER -
#            nu_ventc*ICUCV +ageing%*%ICUCV - mort*ICUCV - (1-critV)*ICUCV*1/2
#          dVentdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_v*E+
#            gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*(1-crit)*(1-critV)*prob_v_v*EV+
#            gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*(1-crit)*(1-critV)*prob_v_vr*EVR+
#            gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*(1-crit)*(1-critV)*prob_v_r*ER+
#            gamma*reporth_ICU*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_v*QE+
#            gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*(1-crit)*(1-critV)*prob_v_v*QEV+
#            gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*(1-crit)*(1-critV)*prob_v_vr*QEVR+
#            gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*(1-crit)*(1-critV)*prob_v_r*QER +
#            (1-critV)*VentC*1/2 +(1-critV)*ICUCV*1/2 - nu_vent*Vent +ageing%*%Vent - mort*Vent 
#          dVentCdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*prob_v*(1-crit)*critV*E+
#            gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*(1-crit)*critV*EV+
#            gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*(1-crit)*critV*EVR+
#            gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*(1-crit)*critV*ER+
#            gamma*reporth_ICU*ihr[,2]*prob_icu*prob_v*(1-crit)*critV*QE+
#            gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*(1-crit)*critV*QEV+
#            gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*(1-crit)*critV*QEVR+
#            gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*(1-crit)*critV*QER - 
#            (1-critV)*VentC*1/2 -nu_ventc*VentC +ageing%*%VentC - mort*VentC 
         
#          # print(paste(t,dexo2,propo2))
#          # print(sum(nus*propo2*dexo2*pdeath_ho*ifr[,2]*H))
#          dCdt <- report*gamma*(1-age_testing_vector*ratetestE)*(1-pclin)*(1-ihr[,2])*(E+QE)+reportc*gamma*pclin*(1-age_testing_vector*ratetestE)*(1-ihr[,2])*(E+QE)+
#            gamma*ihr[,2]*(1-critH)*(1-prob_icu)*(E+QE)+gamma*ihr[,2]*critH*reporth*(1-prob_icu)*(E+QE)+
#            gamma*ihr[,2]*prob_icu*(E+QE)+ratetestI*age_testing_vector*I+ratetestC*age_testing_vector*CL+gamma*age_testing_vector*ratetestE*(1-ihr[,2])*E
#          dCMdt<- nus*propo2*dexo2*pdeath_ho*ifr[,2]*H+nus*(1-propo2)*pdeath_h*ifr[,2]*H+
#            nusc*report_death_HC*propo2*pdeath_hco*ifr[,2]*HC+nusc*report_death_HC*(1-propo2)*pdeath_hc*ifr[,2]*HC+
#            nu_icu*propo2*dexo2*pdeath_icuo*ifr[,2]*ICU+nu_icu*(1-propo2)*pdeath_icu*ifr[,2]*ICU+
#            nu_icuc*propo2*dexo2c*pdeath_icuco*ifr[,2]*ICUC+nu_icuc*(1-propo2)*pdeath_icuc*ifr[,2]*ICUC+
#            nu_vent*dexv*pdeath_vent*ifr[,2]*Vent+nu_ventc*dexvc*pdeath_ventc*ifr[,2]*VentC +
#            nu_ventc*dexvc*pdeath_ventc*ifr[,2]*ICUCV+ nu_ventc*report_death_HC*pdeath_vent_hc*ifr[,2]*HCV+
#            nusc*report_death_HC*propo2*pdeath_icu_hco*ifr[,2]*HCICU+
#            nusc*report_death_HC*(1-propo2)*pdeath_icu_hc*ifr[,2]*HCICU +
#            mort*H + mort*ICU + mort*ICUC + mort*ICUCV + mort*Vent + mort*VentC + mort*Z + 
#            mort*report_death_HC*HC +mort*report_death_HC*HCICU + mort*report_death_HC*HCV +
#            report_natdeathI*mort*I + report_natdeathI*mort*QI+ report_natdeathI*mort*E+
#            report_natdeathI*mort*QE + report_natdeathI*mort*EV+ report_natdeathI*mort*EVR+
#            report_natdeathI*mort*ER + report_natdeathI*mort*QEV+
#            report_natdeathI*mort*QEVR + report_natdeathI*mort*QER+
#            report_natdeathCL*mort*CL + report_natdeathCL*mort*QC + report_natdeathCL*mort*X
#          dCMCdt <- nusc*propo2*pdeath_hco*ifr[,2]*HC+nusc*(1-propo2)*pdeath_hc*ifr[,2]*HC+
#            nu_icuc*propo2*dexo2c*pdeath_icuco*ifr[,2]*ICUC+nu_icuc*(1-propo2)*pdeath_icuc*ifr[,2]*ICUC+
#            nu_ventc*dexvc*pdeath_ventc*ifr[,2]*VentC+nu_ventc*dexvc*pdeath_ventc*ifr[,2]*ICUCV+
#            mort*HC + mort*ICUC + mort*VentC + mort*ICUCV 
         
#          dZdt <- gamma*ratetestE*age_testing_vector*(1-ihr[,2])*E+
#            ratetestI*age_testing_vector*I+
#            ratetestC*age_testing_vector*CL+
#            gamma*(1-ihr[,2])*ratetestEV*age_testing_vector*EV+
#            gamma*(1-ihr[,2])*ratetestEVR*age_testing_vector*EVR+
#            gamma*(1-ihr[,2])*ratetestER*age_testing_vector*ER+
#            ratetestHC*age_testing_vector*HC+
#            ratetestHCICU*age_testing_vector*HCICU+
#            ratetestHCV*age_testing_vector*HCV-
#            (1/isolation_days)*Z-mort*Z
         
#          dAbdt <- nui*I+nui*X+nui*CL+ 
#            nus*propo2*(1-dexo2*pdeath_ho)*ifr[,2]*H+nus*(1-propo2)*(1-pdeath_h)*ifr[,2]*H+
#            nusc*propo2*(1-pdeath_hco)*ifr[,2]*HC+nusc*(1-propo2)*(1-pdeath_hc)*ifr[,2]*HC+  
#            nusc*propo2*(1-pdeath_icu_hco)*ifr[,2]*HCICU+nusc*(1-propo2)*(1-pdeath_icu_hc)*ifr[,2]*HCICU+
#            nu_ventc*(1-pdeath_vent_hc)*ifr[,2]*HCV+
#            nu_icu*propo2*(1-dexo2*pdeath_icuo)*ifr[,2]*ICU+nu_icu*(1-propo2)*(1-pdeath_icu)*ifr[,2]*ICU+
#            nu_icuc*propo2*(1-dexo2c*pdeath_icuco)*ifr[,2]*ICUC+nu_icuc*(1-propo2)*(1-pdeath_icuc)*ifr[,2]*ICUC+
#            nu_vent*(1-dexv*pdeath_vent)*ifr[,2]*Vent+
#            nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*VentC+
#            nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*ICUCV - 
#            seroneg*Ab - mort*Ab + ageing%*%Ab
         
#          # print(paste("QEVR",sum(QEVR)))
         
#          # return the rate of change
#          list(c(S=dSdt,dEdt,dIdt,dRdt,dXdt,dHdt,dHCdt,dCdt,dCMdt,dVdt,dQSdt,dQEdt,dQIdt,dQRdt,dCLdt,dQCdt,dICUdt,dICUCdt,dICUCVdt,
#                 dVentdt,dVentCdt,dCMCdt,dZdt,dEVdt,dERdt,dEVRdt,dVRdt,dQVdt,dQEVdt,dQEVRdt,dQERdt,dQVRdt,dHCICUdt,dHCVdt,dAbdt))
#        }
#   ) 
# }



# # process_ode_outcome <- function(out_mean, intv_vector, param_used, iterations = 1){

# #   out_min<-out_mean
# #   out_max<-out_mean
# #   # out_mean<-out$mean
# #   parameters <- param_used

# #   critH<-c()
# #   crit<-c()
# #   critV<-c()
  
# #   for (i in 1:length(times)){
# #     critH[i]<-min(1-fH((sum(out_mean[i,(Hindex+1)]))+sum(out_mean[i,(ICUCindex+1)])+sum(out_mean[i,(ICUCVindex+1)])),1)
# #     crit[i]<-min(1-fICU((sum(out_mean[i,(ICUindex+1)]))+(sum(out_mean[i,(Ventindex+1)]))+(sum(out_mean[i,(VentCindex+1)]))))
# #     critV[i]<-min(1-fVent((sum(out_mean[i,(Ventindex+1)]))),1)
# #   }
  
# #   # total population
# #   pop1<-out_mean[,(Sindex+1)]+out_mean[,(Eindex+1)]+out_mean[,(Iindex+1)]+out_mean[,(CLindex+1)]+out_mean[,(Rindex+1)]+
# #     out_mean[,(Xindex+1)]+out_mean[,(Vindex+1)]+out_mean[,(Zindex+1)]+out_mean[,(EVindex+1)]+out_mean[,(ERindex+1)]+out_mean[,(EVRindex+1)]+
# #     out_mean[,(QSindex+1)]+out_mean[,(QEindex+1)]+out_mean[,(QIindex+1)]+out_mean[,(QCindex+1)]+out_mean[,(QRindex+1)]+
# #     out_mean[,(QVindex+1)]+out_mean[,(QEVindex+1)]+out_mean[,(QERindex+1)]+out_mean[,(QVRindex+1)]+out_mean[,(QEVRindex+1)]+
# #     out_mean[,(Hindex+1)]+out_mean[,(HCindex+1)]+out_mean[,(ICUindex+1)]+out_mean[,(ICUCindex+1)]+out_mean[,(ICUCVindex+1)]+
# #     out_mean[,(Ventindex+1)]+out_mean[,(VentCindex+1)]+out_mean[,(HCICUindex+1)]+out_mean[,(HCVindex+1)]
# #   tpop1<-rowSums(pop1)
  

# #   ##########################    AB prevalence
# #   ab_age<-out_mean[,(Abindex+1)]
# #   ab_all_ages<-rowSums(out_mean[,(Abindex+1)])
  
# #   ##########################    CALCULATE MORTALITY 
# #   dexo2_hist <- rep(0,length(times))
# #   dexo2c_hist <- rep(0,length(times))
# #   dexv_hist <- rep(0,length(times))
# #   dexvc_hist <- rep(0,length(times))
# #   for (tt in times) {
# #     if(tt < max(times)){
# #       if(intv_vector$dex[tt*20+1]) {
# #         dexo2_hist[tt+1] <- parameters["dexo2"]
# #         dexo2c_hist[tt+1] <- parameters["dexo2c"]
# #         dexv_hist[tt+1] <- parameters["dexv"]
# #         dexvc_hist[tt+1] <- parameters["dexvc"]
# #       } else {
# #         dexo2_hist[tt+1] <- 1
# #         dexo2c_hist[tt+1] <- 1
# #         dexv_hist[tt+1] <- 1
# #         dexvc_hist[tt+1] <- 1
# #       }
# #     } else {
# #       dexo2_hist[tt+1] <- dexo2_hist[tt]
# #       dexo2c_hist[tt+1] <- dexo2c_hist[tt]
# #       dexv_hist[tt+1] <- dexv_hist[tt]
# #       dexvc_hist[tt+1] <- dexvc_hist[tt]
# #     }
# #   }
  
# #   cinc_mort_1 <- cumsum(rowSums(parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out_mean[,(Hindex+1)]%*%ifr[,2])))
# #   cinc_mort_2 <- cumsum(rowSums(parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out_mean[,(Hindex+1)]%*%ifr[,2])))
  
# #   cinc_mort_3 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out_mean[,(HCindex+1)]%*%ifr[,2])))
# #   cinc_mort_4 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out_mean[,(HCindex+1)]%*%ifr[,2])))
 
# #   cinc_mort_5 <- cumsum(rowSums(parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out_mean[,(ICUindex+1)]%*%ifr[,2])))
# #   cinc_mort_6 <- cumsum(rowSums(parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out_mean[,(ICUindex+1)]%*%ifr[,2])))
# #   cinc_mort_7 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out_mean[,(ICUCindex+1)]%*%ifr[,2])))
# #   cinc_mort_8 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out_mean[,(ICUCindex+1)]%*%ifr[,2])))
  
# #   cinc_mort_9 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out_mean[,(Ventindex+1)]%*%ifr[,2])))
# #   cinc_mort_10 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_mean[,(VentCindex+1)]%*%ifr[,2])))
# #   cinc_mort_11 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_mean[,(ICUCVindex+1)]%*%ifr[,2])))
  
# #   cinc_mort_12 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])))
# #   cinc_mort_13 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])))
# #   cinc_mort_14 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["report_death_HC"]*parameters["pdeath_vent_hc"]*(out_mean[,(HCVindex+1)]%*%ifr[,2])))
  
# #   cinc_mort_121 <- cumsum(rowSums(parameters["nusc"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])))
# #   cinc_mort_131 <- cumsum(rowSums(parameters["nusc"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_mean[,(HCICUindex+1)]%*%ifr[,2])))
# #   cinc_mort_141 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_vent_hc"]*(out_mean[,(HCVindex+1)]%*%ifr[,2])))
  
  
# #   cinc_mort_H1 <- cinc_mort_1 + cinc_mort_2
# #   cinc_mort_HC1 <- cinc_mort_3 + cinc_mort_4 + cinc_mort_12 + cinc_mort_13 + cinc_mort_14
# #   cinc_mort_ICU1 <- cinc_mort_5 + cinc_mort_6
# #   cinc_mort_ICUC1 <- cinc_mort_7 + cinc_mort_8
# #   cinc_mort_Vent1 <- cinc_mort_9
# #   cinc_mort_VentC1 <- cinc_mort_10
# #   cinc_mort_ICUCV1 <- cinc_mort_11
  
# #   # all deaths due to covid19 disease - reported + unreported
# #   cinc_mort_all<-cinc_mort_1+cinc_mort_2+cinc_mort_3+cinc_mort_4+cinc_mort_5+cinc_mort_6+
# #     cinc_mort_7+cinc_mort_8+cinc_mort_9+cinc_mort_10+cinc_mort_11+cinc_mort_121+cinc_mort_131+cinc_mort_141
  
# #   base_mort_H1 <- cumsum(rowSums(out_mean[,(Hindex+1)]%*%mort))
# #   base_mort_HC1 <- cumsum(rowSums(parameters["report_death_HC"]*out_mean[,(HCindex+1)]%*%mort))
# #   base_mort_ICU1 <- cumsum(rowSums(out_mean[,(ICUindex+1)]%*%mort))
# #   base_mort_ICUC1 <- cumsum(rowSums(out_mean[,(ICUCindex+1)]%*%mort))
# #   base_mort_ICUCV1 <- cumsum(rowSums(out_mean[,(ICUCVindex+1)]%*%mort))
# #   base_mort_Vent1 <- cumsum(rowSums(out_mean[,(Ventindex+1)]%*%mort))
# #   base_mort_VentC1 <- cumsum(rowSums(out_mean[,(VentCindex+1)]%*%mort))
# #   base_mort_Z1 <- cumsum(rowSums(out_mean[,(Zindex+1)]%*%mort))
# #   base_mort_HCICU1 <- cumsum(rowSums(parameters["report_death_HC"]*out_mean[,(HCICUindex+1)]%*%mort))
# #   base_mort_HCV1 <- cumsum(rowSums(parameters["report_death_HC"]*out_mean[,(HCVindex+1)]%*%mort))
  
# #   base_mort_V1 <- cumsum(rowSums(out_mean[,(Vindex+1)]%*%mort))
# #   base_mort_S1 <- cumsum(rowSums(out_mean[,(Sindex+1)]%*%mort))
# #   base_mort_QS1 <- cumsum(rowSums(out_mean[,(QSindex+1)]%*%mort))
# #   base_mort_QR1 <- cumsum(rowSums(out_mean[,(QRindex+1)]%*%mort))
# #   base_mort_R1 <- cumsum(rowSums(out_mean[,(Rindex+1)]%*%mort))
# #   base_mort_QVR1 <- cumsum(rowSums(out_mean[,(QVRindex+1)]%*%mort))
# #   base_mort_VR1 <- cumsum(rowSums(out_mean[,(VRindex+1)]%*%mort))
# #   base_mort_QV1 <- cumsum(rowSums(out_mean[,(QVindex+1)]%*%mort))
  
# #   base_mort_E1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(Eindex+1)]%*%mort))
# #   base_mort_I1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(Iindex+1)]%*%mort))
# #   base_mort_CL1 <- cumsum(rowSums(parameters["report_natdeathCL"]*out_mean[,(CLindex+1)]%*%mort))
# #   base_mort_X1 <- cumsum(rowSums(parameters["report_natdeathCL"]*out_mean[,(Xindex+1)]%*%mort))
# #   base_mort_QE1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QEindex+1)]%*%mort))
# #   base_mort_QI1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QIindex+1)]%*%mort))
# #   base_mort_QC1 <- cumsum(rowSums(parameters["report_natdeathCL"]*out_mean[,(QCindex+1)]%*%mort))
# #   base_mort_ER1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(ERindex+1)]%*%mort))
# #   base_mort_EV1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(EVindex+1)]%*%mort))
# #   base_mort_EVR1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(EVRindex+1)]%*%mort))
# #   base_mort_QEV1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QEVindex+1)]%*%mort))
# #   base_mort_QER1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QERindex+1)]%*%mort))
# #   base_mort_QEVR1 <- cumsum(rowSums(parameters["report_natdeathI"]*out_mean[,(QEVRindex+1)]%*%mort))

  
# #   base_mort_HC11 <- cumsum(rowSums(out_mean[,(HCindex+1)]%*%mort))
# #   base_mort_HCICU11 <- cumsum(rowSums(out_mean[,(HCICUindex+1)]%*%mort))
# #   base_mort_HCV11 <- cumsum(rowSums(out_mean[,(HCVindex+1)]%*%mort))
# #   base_mort_E11 <- cumsum(rowSums(out_mean[,(Eindex+1)]%*%mort))
# #   base_mort_I11 <- cumsum(rowSums(out_mean[,(Iindex+1)]%*%mort))
# #   base_mort_CL11 <- cumsum(rowSums(out_mean[,(CLindex+1)]%*%mort))
# #   base_mort_X11 <- cumsum(rowSums(out_mean[,(Xindex+1)]%*%mort))
# #   base_mort_QE11 <- cumsum(rowSums(out_mean[,(QEindex+1)]%*%mort))
# #   base_mort_QI11 <- cumsum(rowSums(out_mean[,(QIindex+1)]%*%mort))
# #   base_mort_QC11 <- cumsum(rowSums(out_mean[,(QCindex+1)]%*%mort))
# #   base_mort_ER11 <- cumsum(rowSums(out_mean[,(ERindex+1)]%*%mort))
# #   base_mort_EV11 <- cumsum(rowSums(out_mean[,(EVindex+1)]%*%mort))
# #   base_mort_EVR11 <- cumsum(rowSums(out_mean[,(EVRindex+1)]%*%mort))
# #   base_mort_QEV11 <- cumsum(rowSums(out_mean[,(QEVindex+1)]%*%mort))
# #   base_mort_QER11 <- cumsum(rowSums(out_mean[,(QERindex+1)]%*%mort))
# #   base_mort_QEVR11 <- cumsum(rowSums(out_mean[,(QEVRindex+1)]%*%mort))
  
# #   # all deaths of infected with sars-cov-2 virus - reported + unreported
# #   nat_deaths_inf <- round(base_mort_E11 + base_mort_I11 + base_mort_CL11 + base_mort_X11 + 
# #                                            base_mort_ER11 + base_mort_EV11+  base_mort_EVR11+   
# #                                            base_mort_QE11 + base_mort_QI11 + base_mort_QC11 +  
# #                                            base_mort_QEV11 + base_mort_QER11 + base_mort_QEVR11 + base_mort_Z1+
# #                                            base_mort_H1+base_mort_HC11+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
# #                                            base_mort_Vent1+base_mort_VentC1+base_mort_HCICU11+base_mort_HCV11)
  
# #   # Export in a cohesive format ----
# #   results <- list()
# #   results$time <- startdate + times  # dates
# #   results$N <- tpop1
  
# #   ## Ab
# #   results$ab_all_ages<-ab_all_ages
# #   results$ab<-ab_age
  
# #   # Rt/ FOI
# #   # dailyinc1<-out$mean_cases         # daily incidence
# #   # results$Rt <- out$mean_Rt
# #   # results$pct_total_pop_infected <- out$mean_infections
# #   # results$doubling_time <- round(log(2)*7 / (log(dailyinc1[2+7] / dailyinc1[2])), 2)  # (Baseline only) to double the number of infections at inception
# #   # results$daily_incidence <- round(dailyinc1)  # daily incidence (Reported)
# #   # results$daily_total_cases <- round(out$mean_daily_infection) # daily incidence (Reported + Unreported)  # daily incidence (Reported + Unreported)
  
# #   # Hospital requirements
# #   previcureq1<-rowSums(out_mean[,(Hindex+1)])+ rowSums(out_mean[,(ICUCindex+1)])+rowSums(out_mean[,(ICUCVindex+1)]) # surge beds occupancy
# #   previcureq21<-rowSums(out_mean[,(ICUindex+1)])+rowSums(out_mean[,(VentCindex+1)])   # icu beds occupancy
# #   previcureq31<-rowSums(out_mean[,(Ventindex+1)])   # ventilator occupancy
# #   overloadH1<-rowSums(out_mean[,(HCindex+1)])       # requirement for beds
# #   overloadICU1<-rowSums(out_mean[,(ICUCindex+1)])+rowSums(out_mean[,(HCICUindex+1)])   # requirement for icu beds
# #   overloadICUV1<-rowSums(out_mean[,(ICUCVindex+1)]) # requirement for ventilators
# #   overloadVent1<-rowSums(out_mean[,(VentCindex+1)])+rowSums(out_mean[,(HCVindex+1)]) # requirement for ventilators
  
# #   results$required_beds <- round(previcureq1)  # required beds
# #   results$saturation <- parameters["beds_available"]  # saturation
# #   results$hospital_surge_beds <- round(previcureq1)
# #   results$icu_beds <- round(previcureq21)
# #   results$ventilators <- round(previcureq31)
# #   results$normal_bed_requirement <- round(rowSums(out_mean[,(Hindex+1)])+overloadH1)   #real required beds. previcureq1 above is the occupancy
# #   results$icu_bed_requirement <- round(rowSums(out_mean[,(ICUindex+1)])+overloadICU1)
# #   results$icu_ventilator_requirement <- round(rowSums(out_mean[,(Ventindex+1)])+overloadICUV1+overloadVent1)
  
# #   ### MORTALITY
# #   results$cum_mortality <- round(rowSums(out_mean[,(CMindex+1)]))       # cumulative mortality
# #   results$deaths_from_covid<-last(cinc_mort_all)
# #   results$deaths_with_covid<-last(nat_deaths_inf)
# #   results$death_natural_non_exposed <- round(base_mort_S1+base_mort_V1+base_mort_QS1)
# #   results$death_natural_exposed <- round(base_mort_E1 + base_mort_I1 + base_mort_CL1 + base_mort_X1 + 
# #                                            base_mort_R1+ base_mort_ER1 + base_mort_EV1+  base_mort_EVR1+   
# #                                            base_mort_QE1 + base_mort_QI1 + base_mort_QC1 + base_mort_QR1 + 
# #                                            base_mort_QEV1 + base_mort_QER1 + base_mort_QEVR1 + base_mort_QVR1 +
# #                                            base_mort_H1+base_mort_HC1+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
# #                                            base_mort_Vent1+base_mort_VentC1+base_mort_HCICU1+base_mort_HCV1)
# #   results$death_treated_hospital <- round(cinc_mort_H1)
# #   results$death_treated_icu <- round(cinc_mort_ICU1)
# #   results$death_treated_ventilator <- round(cinc_mort_Vent1)
# #   results$death_untreated_hospital <- round(cinc_mort_HC1)
# #   results$death_untreated_icu <- round(cinc_mort_ICUC1)
# #   results$death_untreated_ventilator <- round(cinc_mort_VentC1)+round(cinc_mort_ICUCV1)
# #   results$attributable_deaths <- results$death_treated_hospital + results$death_treated_icu + results$death_treated_ventilator +
# #     results$death_untreated_hospital + results$death_untreated_icu + results$death_untreated_ventilator
# #   results$attributable_deaths_end <- last(results$attributable_deaths)
# #   results$total_deaths <- results$attributable_deaths + results$death_natural_non_exposed + results$death_natural_exposed
# #   results$total_deaths_end <- last(results$total_deaths)
# #   results$total_reported_deaths_end <- last(results$cum_mortality)
  
  
# #   ## AGE DEPENDENT MORTALITY
# #   # cinc_mort_H1 <- parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out$mean[,(Hindex+1)])+
# #   #   parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out$mean[,(Hindex+1)])
# #   # cinc_mort_HC1 <- parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out$mean[,(HCindex+1)])+
# #   #   parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out$mean[,(HCindex+1)])
# #   # cinc_mort_ICU1 <- parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out$mean[,(ICUindex+1)])+
# #   #   parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out$mean[,(ICUindex+1)])
# #   # cinc_mort_ICUC1 <- parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out$mean[,(ICUCindex+1)] )+
# #   #   parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out$mean[,(ICUCindex+1)] )
# #   # cinc_mort_Vent1  <- parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out$mean[,(Ventindex+1)] )
# #   # cinc_mort_VentC1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out$mean[,(VentCindex+1)] )
# #   # cinc_mort_ICUCV1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out$mean[,(ICUCVindex+1)] )
# #   # cinc_mort_HCICU1 <- parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out$mean[,(HCICUindex+1)] )+
# #   #   parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out$mean[,(HCICUindex+1)] )
# #   # cinc_mort_HCV1 <- parameters["nu_ventc"]*parameters["report_death_HC"]*parameters["pdeath_vent_hc"]*(out$mean[,(HCVindex+1)] )
  
# #   # totage1<-as.data.frame(cinc_mort_H1+cinc_mort_HC1+cinc_mort_ICU1+cinc_mort_ICUC1+
# #   #                          cinc_mort_Vent1+cinc_mort_VentC1+cinc_mort_ICUCV1+cinc_mort_HCICU1+cinc_mort_HCV1)
  
# #   # basemort_H1<-(out$mean[,(Hindex+1)])
# #   # basemort_HC1<-parameters["report_death_HC"]*(out$mean[,(HCindex+1)])
# #   # basemort_ICU1<-(out$mean[,(ICUindex+1)])
# #   # basemort_ICUC1<-(out$mean[,(ICUCindex+1)])
# #   # basemort_ICUCV1<-(out$mean[,(ICUCVindex+1)])
# #   # basemort_Vent1<-(out$mean[,(Ventindex+1)])
# #   # basemort_VentC1<-(out$mean[,(VentCindex+1)])
# #   # basemort_HCICU1<-parameters["report_death_HC"]*(out$mean[,(HCICUindex+1)])
# #   # basemort_HCV1<-parameters["report_death_HC"]*(out$mean[,(HCVindex+1)])
# #   # basemort_I<-parameters["report_natdeathI"]*(out$mean[,(Iindex+1)])
# #   # basemort_QI<-parameters["report_natdeathI"]*(out$mean[,(QIindex+1)])
# #   # basemort_E<-parameters["report_natdeathI"]*(out$mean[,(Eindex+1)])
# #   # basemort_QE<-parameters["report_natdeathI"]*(out$mean[,(QEindex+1)])
# #   # basemort_EV<-parameters["report_natdeathI"]*(out$mean[,(EVindex+1)])
# #   # basemort_EVR<-parameters["report_natdeathI"]*(out$mean[,(EVRindex+1)])
# #   # basemort_ER<-parameters["report_natdeathI"]*(out$mean[,(ERindex+1)])
# #   # basemort_QEV<-parameters["report_natdeathI"]*(out$mean[,(QEVindex+1)])
# #   # basemort_QEVR<-parameters["report_natdeathI"]*(out$mean[,(QEVRindex+1)])
# #   # basemort_QER<-parameters["report_natdeathI"]*(out$mean[,(QERindex+1)])
# #   # basemort_CL<-parameters["report_natdeathCL"]*(out$mean[,(CLindex+1)])
# #   # basemort_QC<-parameters["report_natdeathCL"]*(out$mean[,(QCindex+1)])
# #   # basemort_X<-parameters["report_natdeathCL"]*(out$mean[,(Xindex+1)])
  
# #   # totbase1<-as.data.frame(basemort_H1+basemort_HC1+basemort_ICU1+basemort_ICUC1+basemort_ICUCV1+
# #   #                           basemort_Vent1+basemort_VentC1+basemort_HCICU1+basemort_HCV1+ 
# #   #                           basemort_I+basemort_QI+basemort_E+basemort_QE+basemort_EV+basemort_EVR+
# #   #                           basemort_ER+basemort_QEV+basemort_QEVR+basemort_QER+basemort_CL+basemort_QC+basemort_X)
  
# #   # tc<-c()
# #   # for (i in 1:dim(cinc_mort_H1)[1]) {
# #   #   for (j in 1:dim(cinc_mort_H1)[2]) {
# #   #     # print(totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j])
# #   #     tc<-rbind(tc,c(i, j, totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j]))
# #   #   }
# #   # }
# #   # tc<-as.data.frame(tc)
# #   # colnames(tc)<-c("Day","Age","value")

# #   # results$tc <- tc %>%
# #   #   mutate(Date = startdate + Day,
# #   #          age_cat = case_when(
# #   #            Age >=  1 & Age <= 6   ~ "<= 30 y.o.",
# #   #            Age >  6 & Age <= 8    ~ "30-40 y.o.",
# #   #            Age >  8 & Age <= 10    ~ "40-50 y.o.",
# #   #            Age >  10 & Age <= 12    ~ "50-60 y.o.",
# #   #            Age >  12 & Age <= 14    ~ "60-70 y.o.",
# #   #            Age >=  15  ~ ">= 70 y.o.")) %>%
# #   #   mutate(age_cat = factor(age_cat, levels = rev(c("<= 30 y.o.", "30-40 y.o.",
# #   #                                                   "40-50 y.o.", "50-60 y.o.", "60-70 y.o.", ">= 70 y.o."))))
  

# #   # mortality_lag <- data.frame(Age = popstruc$agefloor)
# #   # if(nrow(out_mean) >= 30)  mortality_lag <- bind_cols(mortality_lag,
# #   #                                                      data.frame(day30 = out_mean[30,CMindex+1]/out_mean[30,Cindex+1]) %>%
# #   #                                                        mutate(day30 = ifelse(is.infinite(day30), 0, day30)))
# #   # if(nrow(out_mean) >= 60)  mortality_lag <- bind_cols(mortality_lag,
# #   #                                                      data.frame(day60 = out_mean[60,CMindex+1]/out_mean[60,Cindex+1]) %>%
# #   #                                                        mutate(day60 = ifelse(is.infinite(day60), 0, day60)))
# #   # if(nrow(out_mean) >= 90)  mortality_lag <- bind_cols(mortality_lag,
# #   #                                                      data.frame(day90 = out_mean[90,CMindex+1]/out_mean[90,Cindex+1]) %>%
# #   #                                                        mutate(day90 = ifelse(is.infinite(day90), 0, day90))) 
# #   # if(nrow(out_mean) >= 120)  mortality_lag <- bind_cols(mortality_lag,
# #   #                                                       data.frame(day120 = out_mean[120,CMindex+1]/out_mean[120,Cindex+1]) %>%
# #   #                                                         mutate(day120 = ifelse(is.infinite(day120), 0, day120)))

# #   # results$mortality_lag <- mortality_lag

  
# #   if(iterations>1){
    
# #     cinc_mort_1 <- cumsum(rowSums(parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out_min[,(Hindex+1)]%*%ifr[,2])))
# #     cinc_mort_2 <- cumsum(rowSums(parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out_min[,(Hindex+1)]%*%ifr[,2])))
# #     cinc_mort_3 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out_min[,(HCindex+1)]%*%ifr[,2])))
# #     cinc_mort_4 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out_min[,(HCindex+1)]%*%ifr[,2])))
# #     cinc_mort_5 <- cumsum(rowSums(parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out_min[,(ICUindex+1)]%*%ifr[,2])))
# #     cinc_mort_6 <- cumsum(rowSums(parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out_min[,(ICUindex+1)]%*%ifr[,2])))
# #     cinc_mort_7 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out_min[,(ICUCindex+1)]%*%ifr[,2])))
# #     cinc_mort_8 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out_min[,(ICUCindex+1)]%*%ifr[,2])))
# #     cinc_mort_9 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out_min[,(Ventindex+1)]%*%ifr[,2])))
# #     cinc_mort_10 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_min[,(VentCindex+1)]%*%ifr[,2])))
# #     cinc_mort_11 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_min[,(ICUCVindex+1)]%*%ifr[,2])))
# #     cinc_mort_121 <- cumsum(rowSums(parameters["nusc"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_min[,(HCICUindex+1)]%*%ifr[,2])))
# #     cinc_mort_131 <- cumsum(rowSums(parameters["nusc"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_min[,(HCICUindex+1)]%*%ifr[,2])))
# #     cinc_mort_141 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_vent_hc"]*(out_min[,(HCVindex+1)]%*%ifr[,2])))
    
# #     cinc_mort_all_min<-cinc_mort_1+cinc_mort_2+cinc_mort_3+cinc_mort_4+cinc_mort_5+cinc_mort_6+
# #       cinc_mort_7+cinc_mort_8+cinc_mort_9+cinc_mort_10+cinc_mort_11+cinc_mort_121+cinc_mort_131+cinc_mort_141
    
# #     cinc_mort_1 <- cumsum(rowSums(parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out_max[,(Hindex+1)]%*%ifr[,2])))
# #     cinc_mort_2 <- cumsum(rowSums(parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out_max[,(Hindex+1)]%*%ifr[,2])))
# #     cinc_mort_3 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*parameters["propo2"]*parameters["pdeath_hco"]*(out_max[,(HCindex+1)]%*%ifr[,2])))
# #     cinc_mort_4 <- cumsum(rowSums(parameters["nusc"]*parameters["report_death_HC"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out_max[,(HCindex+1)]%*%ifr[,2])))
# #     cinc_mort_5 <- cumsum(rowSums(parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out_max[,(ICUindex+1)]%*%ifr[,2])))
# #     cinc_mort_6 <- cumsum(rowSums(parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out_max[,(ICUindex+1)]%*%ifr[,2])))
# #     cinc_mort_7 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out_max[,(ICUCindex+1)]%*%ifr[,2])))
# #     cinc_mort_8 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out_max[,(ICUCindex+1)]%*%ifr[,2])))
# #     cinc_mort_9 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out_max[,(Ventindex+1)]%*%ifr[,2])))
# #     cinc_mort_10 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_max[,(VentCindex+1)]%*%ifr[,2])))
# #     cinc_mort_11 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_max[,(ICUCVindex+1)]%*%ifr[,2])))
# #     cinc_mort_121 <- cumsum(rowSums(parameters["nusc"]*parameters["propo2"]*parameters["pdeath_icu_hco"]*(out_max[,(HCICUindex+1)]%*%ifr[,2])))
# #     cinc_mort_131 <- cumsum(rowSums(parameters["nusc"]*(1-parameters["propo2"])*parameters["pdeath_icu_hc"]*(out_max[,(HCICUindex+1)]%*%ifr[,2])))
# #     cinc_mort_141 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_vent_hc"]*(out_max[,(HCVindex+1)]%*%ifr[,2])))
    
# #     cinc_mort_all_max<-cinc_mort_1+cinc_mort_2+cinc_mort_3+cinc_mort_4+cinc_mort_5+cinc_mort_6+
# #       cinc_mort_7+cinc_mort_8+cinc_mort_9+cinc_mort_10+cinc_mort_11+cinc_mort_121+cinc_mort_131+cinc_mort_141
    
# #     base_mort_H1 <- cumsum(rowSums(out_min[,(Hindex+1)]%*%mort))
# #     base_mort_ICU1 <- cumsum(rowSums(out_min[,(ICUindex+1)]%*%mort))
# #     base_mort_ICUC1 <- cumsum(rowSums(out_min[,(ICUCindex+1)]%*%mort))
# #     base_mort_ICUCV1 <- cumsum(rowSums(out_min[,(ICUCVindex+1)]%*%mort))
# #     base_mort_Vent1 <- cumsum(rowSums(out_min[,(Ventindex+1)]%*%mort))
# #     base_mort_VentC1 <- cumsum(rowSums(out_min[,(VentCindex+1)]%*%mort))
# #     base_mort_Z1 <- cumsum(rowSums(out_min[,(Zindex+1)]%*%mort))
# #     base_mort_HC11 <- cumsum(rowSums(out_min[,(HCindex+1)]%*%mort))
# #     base_mort_HCICU11 <- cumsum(rowSums(out_min[,(HCICUindex+1)]%*%mort))
# #     base_mort_HCV11 <- cumsum(rowSums(out_min[,(HCVindex+1)]%*%mort))
# #     base_mort_E11 <- cumsum(rowSums(out_min[,(Eindex+1)]%*%mort))
# #     base_mort_I11 <- cumsum(rowSums(out_min[,(Iindex+1)]%*%mort))
# #     base_mort_CL11 <- cumsum(rowSums(out_min[,(CLindex+1)]%*%mort))
# #     base_mort_X11 <- cumsum(rowSums(out_min[,(Xindex+1)]%*%mort))
# #     base_mort_QE11 <- cumsum(rowSums(out_min[,(QEindex+1)]%*%mort))
# #     base_mort_QI11 <- cumsum(rowSums(out_min[,(QIindex+1)]%*%mort))
# #     base_mort_QC11 <- cumsum(rowSums(out_min[,(QCindex+1)]%*%mort))
# #     base_mort_ER11 <- cumsum(rowSums(out_min[,(ERindex+1)]%*%mort))
# #     base_mort_EV11 <- cumsum(rowSums(out_min[,(EVindex+1)]%*%mort))
# #     base_mort_EVR11 <- cumsum(rowSums(out_min[,(EVRindex+1)]%*%mort))
# #     base_mort_QEV11 <- cumsum(rowSums(out_min[,(QEVindex+1)]%*%mort))
# #     base_mort_QER11 <- cumsum(rowSums(out_min[,(QERindex+1)]%*%mort))
# #     base_mort_QEVR11 <- cumsum(rowSums(out_min[,(QEVRindex+1)]%*%mort))
    
# #     nat_deaths_inf_min <- last(round(base_mort_E11 + base_mort_I11 + base_mort_CL11 + base_mort_X11 + 
# #                               base_mort_ER11 + base_mort_EV11+  base_mort_EVR11+   
# #                               base_mort_QE11 + base_mort_QI11 + base_mort_QC11 +  
# #                               base_mort_QEV11 + base_mort_QER11 + base_mort_QEVR11 + base_mort_Z1+
# #                               base_mort_H1+base_mort_HC11+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
# #                               base_mort_Vent1+base_mort_VentC1+base_mort_HCICU11+base_mort_HCV11))
    
# #     base_mort_H1 <- cumsum(rowSums(out_max[,(Hindex+1)]%*%mort))
# #     base_mort_ICU1 <- cumsum(rowSums(out_max[,(ICUindex+1)]%*%mort))
# #     base_mort_ICUC1 <- cumsum(rowSums(out_max[,(ICUCindex+1)]%*%mort))
# #     base_mort_ICUCV1 <- cumsum(rowSums(out_max[,(ICUCVindex+1)]%*%mort))
# #     base_mort_Vent1 <- cumsum(rowSums(out_max[,(Ventindex+1)]%*%mort))
# #     base_mort_VentC1 <- cumsum(rowSums(out_max[,(VentCindex+1)]%*%mort))
# #     base_mort_Z1 <- cumsum(rowSums(out_max[,(Zindex+1)]%*%mort))
# #     base_mort_HC11 <- cumsum(rowSums(out_max[,(HCindex+1)]%*%mort))
# #     base_mort_HCICU11 <- cumsum(rowSums(out_max[,(HCICUindex+1)]%*%mort))
# #     base_mort_HCV11 <- cumsum(rowSums(out_max[,(HCVindex+1)]%*%mort))
# #     base_mort_E11 <- cumsum(rowSums(out_max[,(Eindex+1)]%*%mort))
# #     base_mort_I11 <- cumsum(rowSums(out_max[,(Iindex+1)]%*%mort))
# #     base_mort_CL11 <- cumsum(rowSums(out_max[,(CLindex+1)]%*%mort))
# #     base_mort_X11 <- cumsum(rowSums(out_max[,(Xindex+1)]%*%mort))
# #     base_mort_QE11 <- cumsum(rowSums(out_max[,(QEindex+1)]%*%mort))
# #     base_mort_QI11 <- cumsum(rowSums(out_max[,(QIindex+1)]%*%mort))
# #     base_mort_QC11 <- cumsum(rowSums(out_max[,(QCindex+1)]%*%mort))
# #     base_mort_ER11 <- cumsum(rowSums(out_max[,(ERindex+1)]%*%mort))
# #     base_mort_EV11 <- cumsum(rowSums(out_max[,(EVindex+1)]%*%mort))
# #     base_mort_EVR11 <- cumsum(rowSums(out_max[,(EVRindex+1)]%*%mort))
# #     base_mort_QEV11 <- cumsum(rowSums(out_max[,(QEVindex+1)]%*%mort))
# #     base_mort_QER11 <- cumsum(rowSums(out_max[,(QERindex+1)]%*%mort))
# #     base_mort_QEVR11 <- cumsum(rowSums(out_max[,(QEVRindex+1)]%*%mort))
    
# #     nat_deaths_inf_max<- last(round(base_mort_E11 + base_mort_I11 + base_mort_CL11 + base_mort_X11 + 
# #                                        base_mort_ER11 + base_mort_EV11+  base_mort_EVR11+   
# #                                        base_mort_QE11 + base_mort_QI11 + base_mort_QC11 +  
# #                                        base_mort_QEV11 + base_mort_QER11 + base_mort_QEVR11 + base_mort_Z1+
# #                                        base_mort_H1+base_mort_HC11+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
# #                                        base_mort_Vent1+base_mort_VentC1+base_mort_HCICU11+base_mort_HCV11))
    
    
# #     previcureq1_max<-rowSums(out_max[,(Hindex+1)])+ rowSums(out_max[,(ICUCindex+1)])+rowSums(out_max[,(ICUCVindex+1)]) # surge beds occupancy
# #     previcureq21_max<-rowSums(out_max[,(ICUindex+1)])+rowSums(out_max[,(VentCindex+1)])   # icu beds occupancy
# #     previcureq31_max<-rowSums(out_max[,(Ventindex+1)])   # ventilator occupancy
# #     cmortality1_max<-rowSums(out_max[,(CMindex+1)])      # cumulative mortality
# #     overloadH1_max<-rowSums(out_max[,(HCindex+1)])       # requirement for beds
# #     overloadICU1_max<-rowSums(out_max[,(ICUCindex+1)])+ rowSums(out_max[,(HCICUindex+1)])  # requirement for icu beds
# #     overloadICUV1_max<-rowSums(out_max[,(ICUCVindex+1)])+ rowSums(out_max[,(HCVindex+1)])  # requirement for ventilators
# #     overloadVent1_max<-rowSums(out_max[,(VentCindex+1)]) # requirement for ventilators
# #     ccases1_max<-rowSums(out_max[,(Cindex+1)])           # cumulative cases
# #     reqsurge1_max<-rowSums(out_max[,(Hindex+1)])+overloadH1  # surge beds total requirements
# #     reqicu1_max<-rowSums(out_max[,(ICUindex+1)])+overloadICU1 # ICU beds total requirements
# #     reqvent1_max<-rowSums(out_max[,(Ventindex+1)])+overloadICUV1+overloadVent1 # ventilator beds total requirements
    
# #     previcureq1_min<-rowSums(out_min[,(Hindex+1)])+rowSums(out_min[,(ICUCindex+1)])+rowSums(out_min[,(ICUCVindex+1)]) # surge beds occupancy
# #     previcureq21_min<-rowSums(out_min[,(ICUindex+1)])+rowSums(out_min[,(VentCindex+1)])   # icu beds occupancy
# #     previcureq31_min<-rowSums(out_min[,(Ventindex+1)])   # ventilator occupancy
# #     cmortality1_min<-rowSums(out_min[,(CMindex+1)])      # cumulative mortality
# #     overloadH1_min<-rowSums(out_min[,(HCindex+1)])       # requirement for beds
# #     overloadICU1_min<-rowSums(out_min[,(ICUCindex+1)])   # requirement for icu beds
# #     overloadICUV1_min<-rowSums(out_min[,(ICUCVindex+1)]) # requirement for ventilators
# #     overloadVent1_min<-rowSums(out_min[,(VentCindex+1)]) # requirement for ventilators
# #     ccases1_min<-rowSums(out_min[,(Cindex+1)])           # cumulative cases
# #     reqsurge1_min<-rowSums(out_min[,(Hindex+1)])+overloadH1  # surge beds total requirements
# #     reqicu1_min<-rowSums(out_min[,(ICUindex+1)])+overloadICU1 # ICU beds total requirements
# #     reqvent1_min<-rowSums(out_min[,(Ventindex+1)])+overloadICUV1+overloadVent1 # ventilator beds total requirements
    
# #     results$Rt_max <- out$max_Rt
# #     results$Rt_min <- out$min_Rt
    
# #     results$daily_incidence_max <- out$max_cases
# #     results$daily_incidence_min <- out$min_cases  
    
# #     results$deaths_from_covid_min<-last(cinc_mort_all_min)
# #     results$deaths_from_covid_max<-last(cinc_mort_all_max)
      
# #     results$deaths_with_covid_min<-last(nat_deaths_inf_min)
# #     results$deaths_with_covid_max<-last(nat_deaths_inf_max)
    
# #     results$daily_total_cases_max <- out$max_daily_infection
# #     results$daily_total_cases_min <- out$min_daily_infection
    
# #     results$total_reported_deaths_end_min <- last(cmortality1_min)
# #     results$total_reported_deaths_end_max <- last(cmortality1_max)
    
# #     results$pct_total_pop_infected_min <- out$min_infections  # proportion of the  population that has been infected at the end of the simulation
# #     results$pct_total_pop_infected_max <- out$max_infections  # proportion of the  population that has been infected at the end of the simulation
# #   }
# #   return(results)
# # }

# # Results derived from out$min, out$max or out$mean ----
# process_ode_outcome_compute <- function(out_mat, param_used, startdate, times, ihr, ifr, mort, popstruc, intv_vector) {

#   # snippet v16.5 lines 1615-1627 already in fun_multi_runs.R

#   # Start snippet v16.5 lines 1629-1642 ----
#   # changes: replaced out$mean with out_mat
#   # total population
#   pop1<-out_mat[,(Sindex+1)]+out_mat[,(Eindex+1)]+out_mat[,(Iindex+1)]+out_mat[,(CLindex+1)]+out_mat[,(Rindex+1)]+
#     out_mat[,(Xindex+1)]+out_mat[,(Vindex+1)]+out_mat[,(Zindex+1)]+out_mat[,(EVindex+1)]+out_mat[,(ERindex+1)]+out_mat[,(EVRindex+1)]+
#     out_mat[,(QSindex+1)]+out_mat[,(QEindex+1)]+out_mat[,(QIindex+1)]+out_mat[,(QCindex+1)]+out_mat[,(QRindex+1)]+
#     out_mat[,(QVindex+1)]+out_mat[,(QEVindex+1)]+out_mat[,(QERindex+1)]+out_mat[,(QVRindex+1)]+out_mat[,(QEVRindex+1)]+
#     out_mat[,(Hindex+1)]+out_mat[,(HCindex+1)]+out_mat[,(ICUindex+1)]+out_mat[,(ICUCindex+1)]+out_mat[,(ICUCVindex+1)]+
#     out_mat[,(Ventindex+1)]+out_mat[,(VentCindex+1)]+out_mat[,(HCICUindex+1)]+out_mat[,(HCVindex+1)]
#   tpop1<-rowSums(pop1)


#   ##########################    AB prevalence
#   ab_age<-out_mat[,(Abindex+1)]
#   ab_all_ages<-rowSums(out_mat[,(Abindex+1)])
#   # End snippet v16.5 lines 1629-1642 ----


#   # Start snippet https://github.com/bogaotory/comoOdeCpp/blob/master/comoOdeCpp/tests/testthat/v16.4.core.mod.16.6.R----
#   # lines 1680-1770
#   prob_v_hist <- rep(param_used["prob_vent"],length(times))
#   dexo2_hist <- rep(0,length(times))
#   dexo2c_hist <- rep(0,length(times))
#   dexv_hist <- rep(0,length(times))
#   dexvc_hist <- rep(0,length(times))
#   for (tt in times) {
#     if(tt < max(times)){
#       if(intv_vector$dex[tt*20+1]) {
#         prob_v_hist[tt+1] <- param_used["prob_vent"]*param_used["vent_dex"]
#         dexo2_hist[tt+1] <- param_used["dexo2"]
#         dexo2c_hist[tt+1] <- param_used["dexo2c"]
#         dexv_hist[tt+1] <- param_used["dexv"]
#         dexvc_hist[tt+1] <- param_used["dexvc"]
#       } else {
#         prob_v_hist[tt+1] <- param_used["prob_vent"]
#         dexo2_hist[tt+1] <- 1
#         dexo2c_hist[tt+1] <- 1
#         dexv_hist[tt+1] <- 1
#         dexvc_hist[tt+1] <- 1
#       }
#     } else {
#       prob_v_hist[tt+1] <- prob_v_hist[tt]
#       dexo2_hist[tt+1] <- dexo2_hist[tt]
#       dexo2c_hist[tt+1] <- dexo2c_hist[tt]
#       dexv_hist[tt+1] <- dexv_hist[tt]
#       dexvc_hist[tt+1] <- dexvc_hist[tt]
#     }
#   }
#   # End snippet https://github.com/bogaotory/comoOdeCpp/blob/master/comoOdeCpp/tests/testthat/v16.4.core.mod.16.6.R----

#   # Start snippet v16.5 lines 1669-1763 ----
#   # changes: replaced out$mean with out_mat; replaced parameters with param_used
#   cinc_mort_1 <- cumsum(rowSums(param_used["nus"]*param_used["propo2"]*param_used["pdeath_ho"]*dexo2_hist*(out_mat[,(Hindex+1)]%*%ifr[,2])))
#   cinc_mort_2 <- cumsum(rowSums(param_used["nus"]*(1-param_used["propo2"])*param_used["pdeath_h"]*(out_mat[,(Hindex+1)]%*%ifr[,2])))

#   cinc_mort_3 <- cumsum(rowSums(param_used["nusc"]*param_used["report_death_HC"]*param_used["propo2"]*param_used["pdeath_hco"]*(out_mat[,(HCindex+1)]%*%ifr[,2])))
#   cinc_mort_4 <- cumsum(rowSums(param_used["nusc"]*param_used["report_death_HC"]*(1-param_used["propo2"])*param_used["pdeath_hc"]*(out_mat[,(HCindex+1)]%*%ifr[,2])))

#   cinc_mort_5 <- cumsum(rowSums(param_used["nu_icu"]*param_used["propo2"]*param_used["pdeath_icuo"]*dexo2_hist*(out_mat[,(ICUindex+1)]%*%ifr[,2])))
#   cinc_mort_6 <- cumsum(rowSums(param_used["nu_icu"]*(1-param_used["propo2"])*param_used["pdeath_icu"]*(out_mat[,(ICUindex+1)]%*%ifr[,2])))
#   cinc_mort_7 <- cumsum(rowSums(param_used["nu_icuc"]*param_used["propo2"]*param_used["pdeath_icuco"]*dexo2c_hist*(out_mat[,(ICUCindex+1)]%*%ifr[,2])))
#   cinc_mort_8 <- cumsum(rowSums(param_used["nu_icuc"]*(1-param_used["propo2"])*param_used["pdeath_icuc"]*(out_mat[,(ICUCindex+1)]%*%ifr[,2])))

#   cinc_mort_9 <- cumsum(rowSums(param_used["nu_vent"]*param_used["pdeath_vent"]*dexv_hist*(out_mat[,(Ventindex+1)]%*%ifr[,2])))
#   cinc_mort_10 <- cumsum(rowSums(param_used["nu_ventc"]*param_used["pdeath_ventc"]*dexvc_hist*(out_mat[,(VentCindex+1)]%*%ifr[,2])))
#   cinc_mort_11 <- cumsum(rowSums(param_used["nu_ventc"]*param_used["pdeath_ventc"]*dexvc_hist*(out_mat[,(ICUCVindex+1)]%*%ifr[,2])))

#   cinc_mort_12 <- cumsum(rowSums(param_used["nusc"]*param_used["report_death_HC"]*param_used["propo2"]*param_used["pdeath_icu_hco"]*(out_mat[,(HCICUindex+1)]%*%ifr[,2])))
#   cinc_mort_13 <- cumsum(rowSums(param_used["nusc"]*param_used["report_death_HC"]*(1-param_used["propo2"])*param_used["pdeath_icu_hc"]*(out_mat[,(HCICUindex+1)]%*%ifr[,2])))
#   cinc_mort_14 <- cumsum(rowSums(param_used["nu_ventc"]*param_used["report_death_HC"]*param_used["pdeath_vent_hc"]*(out_mat[,(HCVindex+1)]%*%ifr[,2])))

#   cinc_mort_121 <- cumsum(rowSums(param_used["nusc"]*param_used["propo2"]*param_used["pdeath_icu_hco"]*(out_mat[,(HCICUindex+1)]%*%ifr[,2])))
#   cinc_mort_131 <- cumsum(rowSums(param_used["nusc"]*(1-param_used["propo2"])*param_used["pdeath_icu_hc"]*(out_mat[,(HCICUindex+1)]%*%ifr[,2])))
#   cinc_mort_141 <- cumsum(rowSums(param_used["nu_ventc"]*param_used["pdeath_vent_hc"]*(out_mat[,(HCVindex+1)]%*%ifr[,2])))


#   cinc_mort_H1 <- cinc_mort_1 + cinc_mort_2
#   cinc_mort_HC1 <- cinc_mort_3 + cinc_mort_4 + cinc_mort_12 + cinc_mort_13 + cinc_mort_14
#   cinc_mort_ICU1 <- cinc_mort_5 + cinc_mort_6
#   cinc_mort_ICUC1 <- cinc_mort_7 + cinc_mort_8
#   cinc_mort_Vent1 <- cinc_mort_9
#   cinc_mort_VentC1 <- cinc_mort_10
#   cinc_mort_ICUCV1 <- cinc_mort_11

#   # all deaths due to covid19 disease - reported + unreported
#   cinc_mort_all<-cinc_mort_1+cinc_mort_2+cinc_mort_3+cinc_mort_4+cinc_mort_5+cinc_mort_6+
#     cinc_mort_7+cinc_mort_8+cinc_mort_9+cinc_mort_10+cinc_mort_11+cinc_mort_121+cinc_mort_131+cinc_mort_141

#   base_mort_H1 <- cumsum(rowSums(out_mat[,(Hindex+1)]%*%mort))
#   base_mort_HC1 <- cumsum(rowSums(param_used["report_death_HC"]*out_mat[,(HCindex+1)]%*%mort))
#   base_mort_ICU1 <- cumsum(rowSums(out_mat[,(ICUindex+1)]%*%mort))
#   base_mort_ICUC1 <- cumsum(rowSums(out_mat[,(ICUCindex+1)]%*%mort))
#   base_mort_ICUCV1 <- cumsum(rowSums(out_mat[,(ICUCVindex+1)]%*%mort))
#   base_mort_Vent1 <- cumsum(rowSums(out_mat[,(Ventindex+1)]%*%mort))
#   base_mort_VentC1 <- cumsum(rowSums(out_mat[,(VentCindex+1)]%*%mort))
#   base_mort_Z1 <- cumsum(rowSums(out_mat[,(Zindex+1)]%*%mort))
#   base_mort_HCICU1 <- cumsum(rowSums(param_used["report_death_HC"]*out_mat[,(HCICUindex+1)]%*%mort))
#   base_mort_HCV1 <- cumsum(rowSums(param_used["report_death_HC"]*out_mat[,(HCVindex+1)]%*%mort))

#   base_mort_V1 <- cumsum(rowSums(out_mat[,(Vindex+1)]%*%mort))
#   base_mort_S1 <- cumsum(rowSums(out_mat[,(Sindex+1)]%*%mort))
#   base_mort_QS1 <- cumsum(rowSums(out_mat[,(QSindex+1)]%*%mort))
#   base_mort_QR1 <- cumsum(rowSums(out_mat[,(QRindex+1)]%*%mort))
#   base_mort_R1 <- cumsum(rowSums(out_mat[,(Rindex+1)]%*%mort))
#   base_mort_QVR1 <- cumsum(rowSums(out_mat[,(QVRindex+1)]%*%mort))
#   base_mort_VR1 <- cumsum(rowSums(out_mat[,(VRindex+1)]%*%mort))
#   base_mort_QV1 <- cumsum(rowSums(out_mat[,(QVindex+1)]%*%mort))

#   base_mort_E1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(Eindex+1)]%*%mort))
#   base_mort_I1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(Iindex+1)]%*%mort))
#   base_mort_CL1 <- cumsum(rowSums(param_used["report_natdeathCL"]*out_mat[,(CLindex+1)]%*%mort))
#   base_mort_X1 <- cumsum(rowSums(param_used["report_natdeathCL"]*out_mat[,(Xindex+1)]%*%mort))
#   base_mort_QE1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(QEindex+1)]%*%mort))
#   base_mort_QI1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(QIindex+1)]%*%mort))
#   base_mort_QC1 <- cumsum(rowSums(param_used["report_natdeathCL"]*out_mat[,(QCindex+1)]%*%mort))
#   base_mort_ER1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(ERindex+1)]%*%mort))
#   base_mort_EV1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(EVindex+1)]%*%mort))
#   base_mort_EVR1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(EVRindex+1)]%*%mort))
#   base_mort_QEV1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(QEVindex+1)]%*%mort))
#   base_mort_QER1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(QERindex+1)]%*%mort))
#   base_mort_QEVR1 <- cumsum(rowSums(param_used["report_natdeathI"]*out_mat[,(QEVRindex+1)]%*%mort))


#   base_mort_HC11 <- cumsum(rowSums(out_mat[,(HCindex+1)]%*%mort))
#   base_mort_HCICU11 <- cumsum(rowSums(out_mat[,(HCICUindex+1)]%*%mort))
#   base_mort_HCV11 <- cumsum(rowSums(out_mat[,(HCVindex+1)]%*%mort))
#   base_mort_E11 <- cumsum(rowSums(out_mat[,(Eindex+1)]%*%mort))
#   base_mort_I11 <- cumsum(rowSums(out_mat[,(Iindex+1)]%*%mort))
#   base_mort_CL11 <- cumsum(rowSums(out_mat[,(CLindex+1)]%*%mort))
#   base_mort_X11 <- cumsum(rowSums(out_mat[,(Xindex+1)]%*%mort))
#   base_mort_QE11 <- cumsum(rowSums(out_mat[,(QEindex+1)]%*%mort))
#   base_mort_QI11 <- cumsum(rowSums(out_mat[,(QIindex+1)]%*%mort))
#   base_mort_QC11 <- cumsum(rowSums(out_mat[,(QCindex+1)]%*%mort))
#   base_mort_ER11 <- cumsum(rowSums(out_mat[,(ERindex+1)]%*%mort))
#   base_mort_EV11 <- cumsum(rowSums(out_mat[,(EVindex+1)]%*%mort))
#   base_mort_EVR11 <- cumsum(rowSums(out_mat[,(EVRindex+1)]%*%mort))
#   base_mort_QEV11 <- cumsum(rowSums(out_mat[,(QEVindex+1)]%*%mort))
#   base_mort_QER11 <- cumsum(rowSums(out_mat[,(QERindex+1)]%*%mort))
#   base_mort_QEVR11 <- cumsum(rowSums(out_mat[,(QEVRindex+1)]%*%mort))

#   # all deaths of infected with sars-cov-2 virus - reported + unreported
#   nat_deaths_inf <- round(base_mort_E11 + base_mort_I11 + base_mort_CL11 + base_mort_X11 + 
#                             base_mort_ER11 + base_mort_EV11+  base_mort_EVR11+   
#                             base_mort_QE11 + base_mort_QI11 + base_mort_QC11 +  
#                             base_mort_QEV11 + base_mort_QER11 + base_mort_QEVR11 + base_mort_Z1+
#                             base_mort_H1+base_mort_HC11+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
#                             base_mort_Vent1+base_mort_VentC1+base_mort_HCICU11+base_mort_HCV11)
#   # End snippet v16.5 lines 1669-1763 ----

#   # Start snippet v16.5 lines 1764-1772 ----
#   results <- list()
#   results$time <- startdate + times  # dates
#   results$N <- tpop1

#   ## Ab
#   results$ab_all_ages<-ab_all_ages
#   results$ab<-ab_age
#   # End snippet v16.5 lines 1764-1772 ----

#   # Start snippet v16.5 lines 1782-1825 (Part 1) ----
#   # changes: replaced out$mean with out_mat; replaced out with our_mat; replaced parameters with param_used
#   # Hospital requirements
#   previcureq1<-rowSums(out_mat[,(Hindex+1)])+ rowSums(out_mat[,(ICUCindex+1)])+rowSums(out_mat[,(ICUCVindex+1)]) # surge beds occupancy
#   previcureq21<-rowSums(out_mat[,(ICUindex+1)])+rowSums(out_mat[,(VentCindex+1)])   # icu beds occupancy
#   previcureq31<-rowSums(out_mat[,(Ventindex+1)])   # ventilator occupancy
#   overloadH1<-rowSums(out_mat[,(HCindex+1)])       # requirement for beds
#   overloadICU1<-rowSums(out_mat[,(ICUCindex+1)])+rowSums(out_mat[,(HCICUindex+1)])   # requirement for icu beds
#   overloadICUV1<-rowSums(out_mat[,(ICUCVindex+1)]) # requirement for ventilators
#   overloadVent1<-rowSums(out_mat[,(VentCindex+1)])+rowSums(out_mat[,(HCVindex+1)]) # requirement for ventilators

#   results$required_beds <- round(previcureq1)  # required beds
#   results$saturation <- param_used["beds_available"]  # saturation
#   results$hospital_surge_beds <- round(previcureq1)
#   results$icu_beds <- round(previcureq21)
#   results$ventilators <- round(previcureq31)
#   results$normal_bed_requirement <- round(rowSums(out_mat[,(Hindex+1)])+overloadH1)   #real required beds. previcureq1 above is the occupancy
#   results$icu_bed_requirement <- round(rowSums(out_mat[,(ICUindex+1)])+overloadICU1)
#   results$icu_ventilator_requirement <- round(rowSums(out_mat[,(Ventindex+1)])+overloadICUV1+overloadVent1)

#   ### MORTALITY
#   results$cum_mortality <- round(rowSums(out_mat[,(CMindex+1)]))       # cumulative mortality

#   # End Snippet (Part 1) ----

#   results$deaths_from_covid <- round(cinc_mort_all)
#   results$deaths_with_covid <- round(nat_deaths_inf)

#   # Start Snippet (Part 2) ----
#   results$death_natural_non_exposed <- round(base_mort_S1+base_mort_V1+base_mort_QS1)
#   results$death_natural_exposed <- round(base_mort_E1 + base_mort_I1 + base_mort_CL1 + base_mort_X1 + 
#                                            base_mort_R1+ base_mort_ER1 + base_mort_EV1+  base_mort_EVR1+   
#                                            base_mort_QE1 + base_mort_QI1 + base_mort_QC1 + base_mort_QR1 + 
#                                            base_mort_QEV1 + base_mort_QER1 + base_mort_QEVR1 + base_mort_QVR1 +
#                                            base_mort_H1+base_mort_HC1+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
#                                            base_mort_Vent1+base_mort_VentC1+base_mort_HCICU1+base_mort_HCV1)
#   results$death_treated_hospital <- round(cinc_mort_H1)
#   results$death_treated_icu <- round(cinc_mort_ICU1)
#   results$death_treated_ventilator <- round(cinc_mort_Vent1)
#   results$death_untreated_hospital <- round(cinc_mort_HC1)
#   results$death_untreated_icu <- round(cinc_mort_ICUC1)
#   results$death_untreated_ventilator <- round(cinc_mort_VentC1)+round(cinc_mort_ICUCV1)
#   results$attributable_deaths <- results$death_treated_hospital + results$death_treated_icu + results$death_treated_ventilator +
#     results$death_untreated_hospital + results$death_untreated_icu + results$death_untreated_ventilator
#   results$attributable_deaths_end <- last(results$attributable_deaths)
#   results$total_deaths <- results$attributable_deaths + results$death_natural_non_exposed + results$death_natural_exposed
#   results$total_deaths_end <- last(results$total_deaths)
#   results$total_reported_deaths_end <- last(results$cum_mortality)
#   # End snippet v16.5 lines 1764-1825 (Part 2) ----

#   # Start snippet v16.5 lines 1826-1857 ----
#   # changes: replaced out$mean with out_mat; replaced parameters with param_used
#   cinc_mort_H1 <- param_used["nus"]*param_used["propo2"]*param_used["pdeath_ho"]*dexo2_hist*(out_mat[,(Hindex+1)])+
#     param_used["nus"]*(1-param_used["propo2"])*param_used["pdeath_h"]*(out_mat[,(Hindex+1)])
#   cinc_mort_HC1 <- param_used["nusc"]*param_used["report_death_HC"]*param_used["propo2"]*param_used["pdeath_hco"]*(out_mat[,(HCindex+1)])+
#     param_used["nusc"]*param_used["report_death_HC"]*(1-param_used["propo2"])*param_used["pdeath_hc"]*(out_mat[,(HCindex+1)])
#   cinc_mort_ICU1 <- param_used["nu_icu"]*param_used["propo2"]*param_used["pdeath_icuo"]*dexo2_hist*(out_mat[,(ICUindex+1)])+
#     param_used["nu_icu"]*(1-param_used["propo2"])*param_used["pdeath_icu"]*(out_mat[,(ICUindex+1)])
#   cinc_mort_ICUC1 <- param_used["nu_icuc"]*param_used["propo2"]*param_used["pdeath_icuco"]*dexo2c_hist*(out_mat[,(ICUCindex+1)] )+
#     param_used["nu_icuc"]*(1-param_used["propo2"])*param_used["pdeath_icuc"]*(out_mat[,(ICUCindex+1)] )
#   cinc_mort_Vent1  <- param_used["nu_vent"]*param_used["pdeath_vent"]*dexv_hist*(out_mat[,(Ventindex+1)] )
#   cinc_mort_VentC1 <- param_used["nu_ventc"]*param_used["pdeath_ventc"]*dexvc_hist*(out_mat[,(VentCindex+1)] )
#   cinc_mort_ICUCV1 <- param_used["nu_ventc"]*param_used["pdeath_ventc"]*dexvc_hist*(out_mat[,(ICUCVindex+1)] )
#   cinc_mort_HCICU1 <- param_used["nusc"]*param_used["report_death_HC"]*param_used["propo2"]*param_used["pdeath_icu_hco"]*(out_mat[,(HCICUindex+1)] )+
#     param_used["nusc"]*param_used["report_death_HC"]*(1-param_used["propo2"])*param_used["pdeath_icu_hc"]*(out_mat[,(HCICUindex+1)] )
#   cinc_mort_HCV1 <- param_used["nu_ventc"]*param_used["report_death_HC"]*param_used["pdeath_vent_hc"]*(out_mat[,(HCVindex+1)] )

#   totage1<-as.data.frame(cinc_mort_H1+cinc_mort_HC1+cinc_mort_ICU1+cinc_mort_ICUC1+
#                            cinc_mort_Vent1+cinc_mort_VentC1+cinc_mort_ICUCV1+cinc_mort_HCICU1+cinc_mort_HCV1)


#   basemort_H1<-(out_mat[,(Hindex+1)])
#   basemort_HC1<-param_used["report_death_HC"]*(out_mat[,(HCindex+1)])
#   basemort_ICU1<-(out_mat[,(ICUindex+1)])
#   basemort_ICUC1<-(out_mat[,(ICUCindex+1)])
#   basemort_ICUCV1<-(out_mat[,(ICUCVindex+1)])
#   basemort_Vent1<-(out_mat[,(Ventindex+1)])
#   basemort_VentC1<-(out_mat[,(VentCindex+1)])
#   basemort_HCICU1<-param_used["report_death_HC"]*(out_mat[,(HCICUindex+1)])
#   basemort_HCV1<-param_used["report_death_HC"]*(out_mat[,(HCVindex+1)])
#   basemort_I<-param_used["report_natdeathI"]*(out_mat[,(Iindex+1)])
#   basemort_QI<-param_used["report_natdeathI"]*(out_mat[,(QIindex+1)])
#   basemort_E<-param_used["report_natdeathI"]*(out_mat[,(Eindex+1)])
#   basemort_QE<-param_used["report_natdeathI"]*(out_mat[,(QEindex+1)])
#   basemort_EV<-param_used["report_natdeathI"]*(out_mat[,(EVindex+1)])
#   basemort_EVR<-param_used["report_natdeathI"]*(out_mat[,(EVRindex+1)])
#   basemort_ER<-param_used["report_natdeathI"]*(out_mat[,(ERindex+1)])
#   basemort_QEV<-param_used["report_natdeathI"]*(out_mat[,(QEVindex+1)])
#   basemort_QEVR<-param_used["report_natdeathI"]*(out_mat[,(QEVRindex+1)])
#   basemort_QER<-param_used["report_natdeathI"]*(out_mat[,(QERindex+1)])
#   basemort_CL<-param_used["report_natdeathCL"]*(out_mat[,(CLindex+1)])
#   basemort_QC<-param_used["report_natdeathCL"]*(out_mat[,(QCindex+1)])
#   basemort_X<-param_used["report_natdeathCL"]*(out_mat[,(Xindex+1)])

#   totbase1<-as.data.frame(basemort_H1+basemort_HC1+basemort_ICU1+basemort_ICUC1+basemort_ICUCV1+
#                             basemort_Vent1+basemort_VentC1+basemort_HCICU1+basemort_HCV1+ 
#                             basemort_I+basemort_QI+basemort_E+basemort_QE+basemort_EV+basemort_EVR+
#                             basemort_ER+basemort_QEV+basemort_QEVR+basemort_QER+basemort_CL+basemort_QC+basemort_X)


#   tc<-c()
#   for (i in 1:dim(cinc_mort_H1)[1]) {
#     for (j in 1:dim(cinc_mort_H1)[2]) {
#       # print(totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j])
#       tc<-rbind(tc,c(i, j, totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j]))
#     }
#   }
#   tc<-as.data.frame(tc)
#   colnames(tc)<-c("Day","Age","value")
#   results$tc <- tc %>%
#     mutate(Date = startdate + Day,
#            age_cat = case_when(
#              Age >=  1 & Age <= 6   ~ "<= 30 y.o.",
#              Age >  6 & Age <= 8    ~ "30-40 y.o.",
#              Age >  8 & Age <= 10    ~ "40-50 y.o.",
#              Age >  10 & Age <= 12    ~ "50-60 y.o.",
#              Age >  12 & Age <= 14    ~ "60-70 y.o.",
#              Age >=  15  ~ ">= 70 y.o.")) %>%
#     mutate(age_cat = factor(age_cat, levels = rev(c("<= 30 y.o.", "30-40 y.o.",
#                                                     "40-50 y.o.", "50-60 y.o.", "60-70 y.o.", ">= 70 y.o."))))

#   mortality_lag <- data.frame(Age = popstruc$agefloor)
#   if(nrow(out_mat) >= 30)  mortality_lag <- bind_cols(mortality_lag,
#                                                        data.frame(day30 = out_mat[30,CMindex+1]/out_mat[30,Cindex+1]) %>%
#                                                          mutate(day30 = ifelse(is.infinite(day30), 0, day30)))
#   if(nrow(out_mat) >= 60)  mortality_lag <- bind_cols(mortality_lag,
#                                                        data.frame(day60 = out_mat[60,CMindex+1]/out_mat[60,Cindex+1]) %>%
#                                                          mutate(day60 = ifelse(is.infinite(day60), 0, day60)))
#   if(nrow(out_mat) >= 90)  mortality_lag <- bind_cols(mortality_lag,
#                                                        data.frame(day90 = out_mat[90,CMindex+1]/out_mat[90,Cindex+1]) %>%
#                                                          mutate(day90 = ifelse(is.infinite(day90), 0, day90))) 
#   if(nrow(out_mat) >= 120)  mortality_lag <- bind_cols(mortality_lag,
#                                                         data.frame(day120 = out_mat[120,CMindex+1]/out_mat[120,Cindex+1]) %>%
#                                                           mutate(day120 = ifelse(is.infinite(day120), 0, day120)))

#   results$mortality_lag <- mortality_lag
#   # End snippet v16.5 lines 1863-1893 ----

#   # Start bridge v16.5 ----
#   results$total_covid_deaths <- results$deaths_from_covid + results$deaths_with_covid
#   results$reportable_deaths <- results$attributable_deaths + results$death_natural_exposed
#   results$total_reportable_deaths_end <- last(results$total_reportable_deaths)
#   results$total_cm_deaths_end <- round(last(results$cum_mortality))
#   # End bridge v16.5 ----

#   # to compare scripts and app
#   # browser()
#   # results_app <<- results

#   return(results)
# }


# process_ode_outcome <- function(out, param_used, startdate, times, ihr, ifr, mort, popstruc, intv_vector){

#   # Define object to return ----
#   results <- list()
#   results$time = startdate + times
  
#   results$med <- process_ode_outcome_compute(out_mat = out$mean, param_used, startdate, times, ihr, ifr, mort, popstruc, intv_vector)
#   results$min <- process_ode_outcome_compute(out_mat = out$min, param_used, startdate, times, ihr, ifr, mort, popstruc, intv_vector)
#   results$max <- process_ode_outcome_compute(out_mat = out$max, param_used, startdate, times, ihr, ifr, mort, popstruc, intv_vector)
  
#   # Results already computed in multi_runs() ----
#   # Rt
#   results$med$Rt <- out$mean_Rt
#   results$min$Rt <- out$min_Rt
#   results$max$Rt <- out$max_Rt
  
#   # proportion of the  population that has been infected at the end of the simulation
#   results$med$pct_total_pop_infected <- out$mean_infections
#   results$min$pct_total_pop_infected <- out$min_infections
#   results$max$pct_total_pop_infected <- out$max_infections
  
#   # Daily incidence
#   ifelse(is.null(dim(out$mean_cases)), results$med$daily_incidence <- round(out$mean_cases), results$med$daily_incidence <- round(out$mean_cases[, 1]))
#   ifelse(is.null(dim(out$min_cases)), results$min$daily_incidence <- round(out$min_cases), results$min$daily_incidence <- round(out$min_cases[, 1]))
#   ifelse(is.null(dim(out$max_cases)), results$max$daily_incidence <- round(out$max_cases), results$max$daily_incidence <- round(out$max_cases[, 1]))
  
#   # overtime proportion of the population that has been reported infected (cumulative)
#   results$med$pct_reported_pop_infected <- round(100 * cumsum(results$med$daily_incidence) / results$med$N, 1)
#   results$min$pct_reported_pop_infected <- round(100 * cumsum(results$min$daily_incidence) / results$min$N, 1)
#   results$max$pct_reported_pop_infected <- round(100 * cumsum(results$max$daily_incidence) / results$max$N, 1)
  
#   # Daily total cases
#   ifelse(is.null(dim(out$mean_daily_infection)), results$med$daily_total_cases <- round(out$mean_daily_infection), results$med$daily_total_cases <- round(out$mean_daily_infection[, 1]))
#   ifelse(is.null(dim(out$min_daily_infection)), results$min$daily_total_cases <- round(out$min_daily_infection), results$min$daily_total_cases <- round(out$min_daily_infection[, 1]))
#   ifelse(is.null(dim(out$max_daily_infection)), results$max$daily_total_cases <- round(out$max_daily_infection), results$max$daily_total_cases <- round(out$max_daily_infection[, 1]))
  
#   # Doubling time (only for baseline)
#   results$med$doubling_time <- round(log(2)*7 / (log(out$mean_cases[2+7] / out$mean_cases[2])), 2)
#   results$min$doubling_time <- round(log(2)*7 / (log(out$min_cases[2+7] / out$min_cases[2])), 2)
#   results$max$doubling_time <- round(log(2)*7 / (log(out$max_cases[2+7] / out$max_cases[2])), 2)
  
#   # Variables that are only downloaded as median value
#   results$hospital_surge_beds <- results$med$hospital_surge_beds
#   results$icu_beds <- results$med$icu_beds
#   results$ventilators <- results$med$ventilators
#   results$normal_bed_requirement <- results$med$normal_bed_requirement
#   results$icu_bed_requirement <- results$med$icu_bed_requirement
#   results$icu_ventilator_requirement <- results$med$icu_ventilator_requirement
  
#   results$death_natural_non_exposed <- results$med$death_natural_non_exposed
#   results$death_natural_exposed <- results$med$death_natural_exposed
#   results$death_treated_hospital <- results$med$death_treated_hospital
#   results$death_treated_icu <- results$med$death_treated_icu
#   results$death_treated_ventilator <- results$med$death_treated_ventilator
#   results$death_untreated_hospital <- results$med$death_untreated_hospital
#   results$death_untreated_icu <- results$med$death_untreated_icu
#   results$death_untreated_ventilator <- results$med$death_untreated_ventilator
#   results$reportable_death <- results$med$reportable_death
  
#   # Compute seroprevalence ----
#   samp.sizes <- round(rnorm(length(results$time),
#                             param_used["sample_size"],
#                             param_used["sample_size"] / 5))
  
#   ab <- data.frame(Time = rep(results$time, 100), Ab = 0)
  
#   aux <- NULL
#   for (i in 1:100) {
#     num.inf.samp <- rbinom(length(results$time), size = samp.sizes, 
#                            prob = (results$med$ab_all_ages / results$med$N))
#     aux<-c(aux, num.inf.samp / samp.sizes)
#   }
  
#   ab$Ab <- (param_used["se"]/100) * aux + (1 - (param_used["sp"]/100))*(1 - aux)
  
# #   quantile_ab <- ab %>%
# #     group_by(Time) %>%
# #     summarise(tibble(
# #       q05 = quantile(Ab, probs = 0.05), 
# #       q25 = quantile(Ab, probs = 0.25), 
# #       q50 = median(Ab),
# #       q75 = quantile(Ab, probs = 0.75), 
# #       q95 = quantile(Ab, probs = 0.95))
# #     )
  
#   results$seroprevalence <- ab
#   #results$seroprevalence_quantile <- quantile_ab
  
#   return(results)
# }



# # Align the output of the calls to the ODE solver with the output from V17 of the Shiny app
# align_with_app_output <- function(simul_baseline, cpp_output = TRUE){
    
#     # Outputs of the model ----
#     dta <- tibble(
#       date = simul_baseline$results$time, 
      
#       # Baseline
#       baseline_predicted_reported_min = simul_baseline$results$min$daily_incidence,
#       baseline_predicted_reported_and_unreported_min = simul_baseline$results$min$daily_total_cases,
#       baseline_normal_bed_occupancy_min = simul_baseline$results$min$hospital_surge_beds,
#       baseline_icu_bed_occupancy_min = simul_baseline$results$min$icu_beds,
#       baseline_icu_ventilator_occupancy_min = simul_baseline$results$min$ventilators,
#       baseline_normal_bed_requirement_min = simul_baseline$results$min$normal_bed_requirement,
#       baseline_icu_bed_requirement_min = simul_baseline$results$min$icu_bed_requirement,
#       baseline_icu_ventilator_requirement_min = simul_baseline$results$min$icu_ventilator_requirement,
#       baseline_death_natural_non_exposed_min = simul_baseline$results$min$death_natural_non_exposed,
#       baseline_death_natural_exposed_min = simul_baseline$results$min$death_natural_exposed,
#       baseline_death_treated_hospital_min = simul_baseline$results$min$death_treated_hospital,
#       baseline_death_treated_icu_min = simul_baseline$results$min$death_treated_icu,
#       baseline_death_treated_ventilator_min = simul_baseline$results$min$death_treated_ventilator,
#       baseline_death_untreated_hospital_min = simul_baseline$results$min$death_untreated_hospital,
#       baseline_death_untreated_icu_min = simul_baseline$results$min$death_untreated_icu,
#       baseline_death_untreated_ventilator_min = simul_baseline$results$min$death_untreated_ventilator,
#       baseline_death_cum_mortality_min = simul_baseline$results$min$cum_mortality,
#       baseline_death_deaths_from_covid_min = simul_baseline$results$min$deaths_from_covid,
#       baseline_death_deaths_with_covid_min = simul_baseline$results$min$deaths_with_covid,
      
      
#       baseline_predicted_reported_med = simul_baseline$results$med$daily_incidence,
#       baseline_predicted_reported_and_unreported_med = simul_baseline$results$med$daily_total_cases,
#       baseline_normal_bed_occupancy_med = simul_baseline$results$med$hospital_surge_beds,
#       baseline_icu_bed_occupancy_med = simul_baseline$results$med$icu_beds,
#       baseline_icu_ventilator_occupancy_med = simul_baseline$results$med$ventilators,
#       baseline_normal_bed_requirement_med = simul_baseline$results$med$normal_bed_requirement,
#       baseline_icu_bed_requirement_med = simul_baseline$results$med$icu_bed_requirement,
#       baseline_icu_ventilator_requirement_med = simul_baseline$results$med$icu_ventilator_requirement,
#       baseline_death_natural_non_exposed_med = simul_baseline$results$med$death_natural_non_exposed,
#       baseline_death_natural_exposed_med = simul_baseline$results$med$death_natural_exposed,
#       baseline_death_treated_hospital_med = simul_baseline$results$med$death_treated_hospital,
#       baseline_death_treated_icu_med = simul_baseline$results$med$death_treated_icu,
#       baseline_death_treated_ventilator_med = simul_baseline$results$med$death_treated_ventilator,
#       baseline_death_untreated_hospital_med = simul_baseline$results$med$death_untreated_hospital,
#       baseline_death_untreated_icu_med = simul_baseline$results$med$death_untreated_icu,
#       baseline_death_untreated_ventilator_med = simul_baseline$results$med$death_untreated_ventilator,
#       baseline_death_cum_mortality_med = simul_baseline$results$med$cum_mortality,
#       baseline_death_deaths_from_covid_med = simul_baseline$results$med$deaths_from_covid,
#       baseline_death_deaths_with_covid_med = simul_baseline$results$med$deaths_with_covid,
      
      
#       baseline_predicted_reported_max = simul_baseline$results$max$daily_incidence,
#       baseline_predicted_reported_and_unreported_max = simul_baseline$results$max$daily_total_cases,
#       baseline_normal_bed_occupancy_max = simul_baseline$results$max$hospital_surge_beds,
#       baseline_icu_bed_occupancy_max = simul_baseline$results$max$icu_beds,
#       baseline_icu_ventilator_occupancy_max = simul_baseline$results$max$ventilators,
#       baseline_normal_bed_requirement_max = simul_baseline$results$max$normal_bed_requirement,
#       baseline_icu_bed_requirement_max = simul_baseline$results$max$icu_bed_requirement,
#       baseline_icu_ventilator_requirement_max = simul_baseline$results$max$icu_ventilator_requirement,
#       baseline_death_natural_non_exposed_max = simul_baseline$results$max$death_natural_non_exposed,
#       baseline_death_natural_exposed_max = simul_baseline$results$max$death_natural_exposed,
#       baseline_death_treated_hospital_max = simul_baseline$results$max$death_treated_hospital,
#       baseline_death_treated_icu_max = simul_baseline$results$max$death_treated_icu,
#       baseline_death_treated_ventilator_max = simul_baseline$results$max$death_treated_ventilator,
#       baseline_death_untreated_hospital_max = simul_baseline$results$max$death_untreated_hospital,
#       baseline_death_untreated_icu_max = simul_baseline$results$max$death_untreated_icu,
#       baseline_death_untreated_ventilator_max = simul_baseline$results$max$death_untreated_ventilator,
#       baseline_death_cum_mortality_max = simul_baseline$results$max$cum_mortality,
#       baseline_death_deaths_from_covid_max = simul_baseline$results$max$deaths_from_covid,
#       baseline_death_deaths_with_covid_max = simul_baseline$results$max$deaths_with_covid
#       )
      
# #       # Hypothetical scenario
# #       hypothetical_predicted_reported_min = simul_interventions$results$min$daily_incidence,
# #       hypothetical_predicted_reported_and_unreported_min = simul_interventions$results$min$daily_total_cases,
# #       hypothetical_normal_bed_occupancy_min = simul_interventions$results$min$hospital_surge_beds,
# #       hypothetical_icu_bed_occupancy_min = simul_interventions$results$min$icu_beds,
# #       hypothetical_icu_ventilator_occupancy_min = simul_interventions$results$min$ventilators,
# #       hypothetical_normal_bed_requirement_min = simul_interventions$results$min$normal_bed_requirement,
# #       hypothetical_icu_bed_requirement_min = simul_interventions$results$min$icu_bed_requirement,
# #       hypothetical_icu_ventilator_requirement_min = simul_interventions$results$min$icu_ventilator_requirement,
# #       hypothetical_death_natural_non_exposed_min = simul_interventions$results$min$death_natural_non_exposed,
# #       hypothetical_death_natural_exposed_min = simul_interventions$results$min$death_natural_exposed,
# #       hypothetical_death_treated_hospital_min = simul_interventions$results$min$death_treated_hospital,
# #       hypothetical_death_treated_icu_min = simul_interventions$results$min$death_treated_icu,
# #       hypothetical_death_treated_ventilator_min = simul_interventions$results$min$death_treated_ventilator,
# #       hypothetical_death_untreated_hospital_min = simul_interventions$results$min$death_untreated_hospital,
# #       hypothetical_death_untreated_icu_min = simul_interventions$results$min$death_untreated_icu,
# #       hypothetical_death_untreated_ventilator_min = simul_interventions$results$min$death_untreated_ventilator,
# #       hypothetical_death_cum_mortality_min = simul_interventions$results$min$cum_mortality,
# #       hypothetical_death_deaths_from_covid_min = simul_interventions$results$min$deaths_from_covid,
# #       hypothetical_death_deaths_with_covid_min = simul_interventions$results$min$deaths_with_covid,
      
# #       hypothetical_predicted_reported_med = simul_interventions$results$med$daily_incidence,
# #       hypothetical_predicted_reported_and_unreported_med = simul_interventions$results$med$daily_total_cases,
# #       hypothetical_normal_bed_occupancy_med = simul_interventions$results$med$hospital_surge_beds,
# #       hypothetical_icu_bed_occupancy_med = simul_interventions$results$med$icu_beds,
# #       hypothetical_icu_ventilator_occupancy_med = simul_interventions$results$med$ventilators,
# #       hypothetical_normal_bed_requirement_med = simul_interventions$results$med$normal_bed_requirement,
# #       hypothetical_icu_bed_requirement_med = simul_interventions$results$med$icu_bed_requirement,
# #       hypothetical_icu_ventilator_requirement_med = simul_interventions$results$med$icu_ventilator_requirement,
# #       hypothetical_death_natural_non_exposed_med = simul_interventions$results$med$death_natural_non_exposed,
# #       hypothetical_death_natural_exposed_med = simul_interventions$results$med$death_natural_exposed,
# #       hypothetical_death_treated_hospital_med = simul_interventions$results$med$death_treated_hospital,
# #       hypothetical_death_treated_icu_med = simul_interventions$results$med$death_treated_icu,
# #       hypothetical_death_treated_ventilator_med = simul_interventions$results$med$death_treated_ventilator,
# #       hypothetical_death_untreated_hospital_med = simul_interventions$results$med$death_untreated_hospital,
# #       hypothetical_death_untreated_icu_med = simul_interventions$results$med$death_untreated_icu,
# #       hypothetical_death_untreated_ventilator_med = simul_interventions$results$med$death_untreated_ventilator,
# #       hypothetical_death_cum_mortality_med = simul_interventions$results$med$cum_mortality,
# #       hypothetical_death_deaths_from_covid_med = simul_interventions$results$med$deaths_from_covid,
# #       hypothetical_death_deaths_with_covid_med = simul_interventions$results$med$deaths_with_covid,
      
# #       hypothetical_predicted_reported_max = simul_interventions$results$max$daily_incidence,
# #       hypothetical_predicted_reported_and_unreported_max = simul_interventions$results$max$daily_total_cases,
# #       hypothetical_normal_bed_occupancy_max = simul_interventions$results$max$hospital_surge_beds,
# #       hypothetical_icu_bed_occupancy_max = simul_interventions$results$max$icu_beds,
# #       hypothetical_icu_ventilator_occupancy_max = simul_interventions$results$max$ventilators,
# #       hypothetical_normal_bed_requirement_max = simul_interventions$results$max$normal_bed_requirement,
# #       hypothetical_icu_bed_requirement_max = simul_interventions$results$max$icu_bed_requirement,
# #       hypothetical_icu_ventilator_requirement_max = simul_interventions$results$max$icu_ventilator_requirement,
# #       hypothetical_death_natural_non_exposed_max = simul_interventions$results$max$death_natural_non_exposed,
# #       hypothetical_death_natural_exposed_max = simul_interventions$results$max$death_natural_exposed,
# #       hypothetical_death_treated_hospital_max = simul_interventions$results$max$death_treated_hospital,
# #       hypothetical_death_treated_icu_max = simul_interventions$results$max$death_treated_icu,
# #       hypothetical_death_treated_ventilator_max = simul_interventions$results$max$death_treated_ventilator,
# #       hypothetical_death_untreated_hospital_max = simul_interventions$results$max$death_untreated_hospital,
# #       hypothetical_death_untreated_icu_max = simul_interventions$results$max$death_untreated_icu,
# #       hypothetical_death_untreated_ventilator_max = simul_interventions$results$max$death_untreated_ventilator,
# #       hypothetical_death_cum_mortality_max = simul_interventions$results$max$cum_mortality,
# #       hypothetical_death_deaths_from_covid_max = simul_interventions$results$max$deaths_from_covid,
# #       hypothetical_death_deaths_with_covid_max = simul_interventions$results$max$deaths_with_covid,
# #     )
    
#     # Cases Data ---- (used to be called on cases_rv$data)
#     dta <- left_join(
#       dta, 
#       cases_rv %>% rename(input_cases = cases, input_deaths = deaths, input_cumulative_death = cumulative_death), 
#       by = "date")
    
#     # Interventions ----
#     startdate <- input$date_range[1]
#     stopdate <- input$date_range[2]
#     times <- seq(0, as.numeric(stopdate - startdate))
#     inp <- bind_rows(interventions$baseline_mat %>% mutate(apply_to = "Baseline (Calibration)"),
#                      interventions$future_mat %>% mutate(apply_to = "Hypothetical Scenario"))
    
#     vectors0 <- inputs(inp, 'Baseline (Calibration)', times, startdate, stopdate)
#     vectors0_cbind <- do.call(cbind, vectors0)
#     vectors0_reduced <- vectors0_cbind[seq(from=0,to=nrow(vectors0_cbind),by=20),]
#     vectors0_reduced <- as.data.frame(rbind(rep(0,ncol(vectors0_reduced)),vectors0_reduced))
#     vectors0_reduced <- vectors0_reduced[,1:12] #subsetting only the coverages - total of 12 different interventions
#     names(vectors0_reduced) <- paste0("interventions_baseline_",names(vectors0_reduced))
    
#     vectors <- inputs(inp, 'Hypothetical Scenario', times, startdate, stopdate)
#     vectors_cbind <- do.call(cbind, vectors)
#     vectors_reduced <- vectors_cbind[seq(from=0,to=nrow(vectors_cbind),by=20),]
#     vectors_reduced <- as.data.frame(rbind(rep(0,ncol(vectors_reduced)),vectors_reduced))
#     vectors_reduced <- vectors_reduced[,1:12] #subsetting only the coverages - total of 12 different interventions
#     names(vectors_reduced) <- paste0("interventions_hypothetical_",names(vectors_reduced))
    
#     intv_vectors <- as_tibble(cbind(date=simul_baseline$results$time, vectors0_reduced, vectors_reduced))
#     intv_vectors$date <- as.Date(intv_vectors$date)
#     dta <- left_join(dta,intv_vectors, by="date")
    
#     return(dta)
# }





# multi_runs <- function(Y, times, parameters, input, A, ihr, ifr, mort, popstruc, popbirth, ageing,
#                        contact_home, contact_school, contact_work, contact_other,
#                        age_group_vectors){
  
#   # Define objects to store results ----
#   results <- list()
#   nb_times <- length(times)
#   nb_col <- length(Y) + 1
#   aux <- array(0, dim = c(nb_times, nb_col, parameters["iterations"]))
  
#   empty_mat <- matrix(0, nrow = nb_times, ncol = parameters["iterations"])
#   cases <- empty_mat
#   cum_cases <- empty_mat
#   day_infections <- empty_mat
#   Rt_aux <- empty_mat
#   infections <- empty_mat

#   # Define spline function ----
#   # the parameters give and beds_available have no noise added to them
#   f <- c(1,(1+parameters["give"])/2,(1-parameters["give"])/2,0)
#   KH<-parameters["beds_available"]
#   KICU<- parameters["icu_beds_available"]+parameters["ventilators_available"]
#   Kvent<- parameters["ventilators_available"]
#   x.H <- c(0,(1+parameters["give"])*KH/2,(3-parameters["give"])*KH/2,2*KH)
#   x.ICU <- c(0,(1+parameters["give"])*KICU/2,(3-parameters["give"])*KICU/2,2*KICU)
#   x.Vent <- c(0,(1+parameters["give"])*Kvent/2,(3-parameters["give"])*Kvent/2,2*Kvent)
#   fH <- splinefun(x.H, f, method = "hyman")
#   fICU <- splinefun(x.ICU, f, method = "hyman")
#   fVent<- splinefun(x.Vent, f, method = "hyman")
  
#   parameters_dup <- parameters  # duplicate parameters to add noise 
  
#   for (i in 1:parameters["iterations"]) {
#     #showNotification(paste("Run", i, "of", parameters["iterations"]))
    
#     # Add noise to parameters only if there are several iterations
#     if (parameters["iterations"] > 1) {
#       parameters_dup[parameters_noise] <- parameters[parameters_noise] + 
#         rnorm(length(parameters_noise), mean = 0, sd = parameters["noise"] * abs(parameters[parameters_noise]))
#     }
    
#     covidOdeCpp_reset()
#     mat_ode <- ode(
#       y = Y, times = times, method = "euler", hini = 0.05,
#       func = covidOdeCpp, parms = parameters_dup,
#       input = input, A = A,
#       contact_home = contact_home,
#       contact_school = contact_school,
#       contact_work = contact_work,
#       contact_other = contact_other,
#       popbirth_col2 = popbirth[, 2],
#       popstruc_col2 = popstruc[, 2],
#       ageing = ageing,
#       ifr_col2 = ifr[, 2],
#       ihr_col2 = ihr[, 2],
#       mort_col = mort,
#       age_group_vectors = age_group_vectors
#     )
#     aux[, , i] <- mat_ode
    
#     # Use spline function
#     critH<-c()
#     crit<-c()
#     critV<-c()
    
#     for (ii in 1:length(times)){
#         critH[ii]<-min(1-fH((sum(mat_ode[ii,(Hindex+1)]))+sum(mat_ode[ii,(ICUCindex+1)])+sum(mat_ode[ii,(ICUCVindex+1)])),1)
#         crit[ii]<-min(1-fICU((sum(mat_ode[ii,(ICUindex+1)]))+(sum(mat_ode[ii,(Ventindex+1)]))+(sum(mat_ode[ii,(VentCindex+1)]))),1)
#         critV[ii]<-min(1-fVent((sum(mat_ode[ii,(Ventindex+1)]))),1)
#       }

#     # daily incidence
#     incidence<-parameters_dup["report"]*parameters_dup["gamma"]*(1-parameters_dup["pclin"])*mat_ode[,(Eindex+1)]%*%(1-ihr[,2])+
#       parameters_dup["reportc"]*parameters_dup["gamma"]*parameters_dup["pclin"]*mat_ode[,(Eindex+1)]%*%(1-ihr[,2])+
#       parameters_dup["report"]*parameters_dup["gamma"]*(1-parameters_dup["pclin"])*mat_ode[,(QEindex+1)]%*%(1-ihr[,2])+
#       parameters_dup["reportc"]*parameters_dup["gamma"]*parameters_dup["pclin"]*mat_ode[,(QEindex+1)]%*%(1-ihr[,2])+
#       parameters_dup["report_v"]*parameters_dup["gamma"]*(1-parameters_dup["pclin_v"])*mat_ode[,(EVindex+1)]%*%(1-parameters_dup["sigmaEV"]*ihr[,2])+
#       parameters_dup["report_cv"]*parameters_dup["gamma"]*parameters_dup["pclin_v"]*mat_ode[,(EVindex+1)]%*%(1-parameters_dup["sigmaEV"]*ihr[,2])+
#       parameters_dup["report_vr"]*parameters_dup["gamma"]*(1-parameters_dup["pclin_vr"])*mat_ode[,(EVRindex+1)]%*%(1-parameters_dup["sigmaEVR"]*ihr[,2])+
#       parameters_dup["report_cvr"]*parameters_dup["gamma"]*parameters_dup["pclin_vr"]*mat_ode[,(EVRindex+1)]%*%(1-parameters_dup["sigmaEVR"]*ihr[,2])+
#       parameters_dup["report_r"]*parameters_dup["gamma"]*(1-parameters_dup["pclin_r"])*mat_ode[,(ERindex+1)]%*%(1-parameters_dup["sigmaER"]*ihr[,2])+
#       parameters_dup["report_cr"]*parameters_dup["gamma"]*parameters_dup["pclin_r"]*mat_ode[,(ERindex+1)]%*%(1-parameters_dup["sigmaER"]*ihr[,2])
      
#     incidenceh<- parameters_dup["gamma"]*mat_ode[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters_dup["prob_icu"])*parameters_dup["reporth"]+
#       parameters_dup["gamma"]*mat_ode[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters_dup["prob_icu"])*(1-parameters_dup["reporth"])*parameters_dup["reporth_g"]+
#       parameters_dup["gamma"]*mat_ode[,(QEindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters_dup["prob_icu"])*parameters_dup["reporth"]+
#       parameters_dup["gamma"]*mat_ode[,(QEindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters_dup["prob_icu"])*(1-parameters_dup["reporth"])*parameters_dup["reporth_g"]+
#       parameters_dup["gamma"]*parameters_dup["sigmaEV"]*mat_ode[,(EVindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters_dup["prob_icu_v"])*parameters_dup["reporth"]+
#       parameters_dup["gamma"]*parameters_dup["sigmaEVR"]*mat_ode[,(EVRindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters_dup["prob_icu_vr"])*parameters_dup["reporth"]+
#       parameters_dup["gamma"]*parameters_dup["sigmaER"]*mat_ode[,(ERindex+1)]%*%ihr[,2]*(1-critH)*(1-parameters_dup["prob_icu_r"])*parameters_dup["reporth"]+
#       parameters_dup["gamma"]*mat_ode[,(Eindex+1)]%*%ihr[,2]*critH*parameters_dup["reporth_g"]*(1-parameters_dup["prob_icu"])+
#       parameters_dup["gamma"]*mat_ode[,(QEindex+1)]%*%ihr[,2]*critH*parameters_dup["reporth_g"]*(1-parameters_dup["prob_icu"])+
#       parameters_dup["gamma"]*parameters_dup["sigmaEV"]*mat_ode[,(EVindex+1)]%*%ihr[,2]*critH*parameters_dup["reporth_g"]*(1-parameters_dup["prob_icu_v"])+
#       parameters_dup["gamma"]*parameters_dup["sigmaEVR"]*mat_ode[,(EVRindex+1)]%*%ihr[,2]*critH*parameters_dup["reporth_g"]*(1-parameters_dup["prob_icu_vr"])+
#       parameters_dup["gamma"]*parameters_dup["sigmaER"]*mat_ode[,(ERindex+1)]%*%ihr[,2]*critH*parameters_dup["reporth_g"]*(1-parameters_dup["prob_icu_r"])+
#       #ICU
#       parameters_dup["gamma"]*mat_ode[,(Eindex+1)]%*%ihr[,2]*parameters_dup["prob_icu"]*(1-crit)*parameters_dup["reporth_ICU"]+
#       parameters_dup["gamma"]*mat_ode[,(QEindex+1)]%*%ihr[,2]*parameters_dup["prob_icu"]*(1-crit)*parameters_dup["reporth_ICU"]+
#       parameters_dup["gamma"]*mat_ode[,(Eindex+1)]%*%ihr[,2]*parameters_dup["prob_icu"]*crit*parameters_dup["reporth_ICU"]*parameters_dup["reporth_g"]+
#       parameters_dup["gamma"]*mat_ode[,(QEindex+1)]%*%ihr[,2]*parameters_dup["prob_icu"]*crit*parameters_dup["reporth_ICU"]*parameters_dup["reporth_g"]+
#       parameters_dup["gamma"]*parameters_dup["sigmaEV"]*mat_ode[,(EVindex+1)]%*%ihr[,2]*(1-crit)*parameters_dup["prob_icu_v"]*parameters_dup["reporth_ICU"]+
#       parameters_dup["gamma"]*parameters_dup["sigmaEVR"]*mat_ode[,(EVRindex+1)]%*%ihr[,2]*(1-crit)*parameters_dup["prob_icu_vr"]*parameters_dup["reporth_ICU"]+
#       parameters_dup["gamma"]*parameters_dup["sigmaER"]*mat_ode[,(ERindex+1)]%*%ihr[,2]*(1-crit)*parameters_dup["prob_icu_r"]*parameters_dup["reporth_ICU"]+
#       parameters_dup["gamma"]*parameters_dup["sigmaEV"]*mat_ode[,(EVindex+1)]%*%ihr[,2]*crit*parameters_dup["prob_icu_v"]*parameters_dup["reporth_ICU"]*parameters_dup["reporth_g"]+
#       parameters_dup["gamma"]*parameters_dup["sigmaEVR"]*mat_ode[,(EVRindex+1)]%*%ihr[,2]*crit*parameters_dup["prob_icu_vr"]*parameters_dup["reporth_ICU"]*parameters_dup["reporth_g"]+
#       parameters_dup["gamma"]*parameters_dup["sigmaER"]*mat_ode[,(ERindex+1)]%*%ihr[,2]*crit*parameters_dup["prob_icu_r"]*parameters_dup["reporth_ICU"]*parameters_dup["reporth_g"]+
#       parameters_dup["gamma"]*mat_ode[,(Eindex+1)]%*%ihr[,2]*parameters_dup["prob_icu"]*(1-parameters_dup["reporth_ICU"])*parameters_dup["reporth_g"]+
#       parameters_dup["gamma"]*mat_ode[,(QEindex+1)]%*%ihr[,2]*parameters_dup["prob_icu"]*(1-parameters_dup["reporth_ICU"])*parameters_dup["reporth_g"]+
#       parameters_dup["gamma"]*parameters_dup["sigmaEV"]*mat_ode[,(EVindex+1)]%*%ihr[,2]*parameters_dup["prob_icu_v"]*(1-parameters_dup["reporth_ICU"])*parameters_dup["reporth_g"]+
#       parameters_dup["gamma"]*parameters_dup["sigmaEVR"]*mat_ode[,(EVRindex+1)]%*%ihr[,2]*parameters_dup["prob_icu_vr"]*(1-parameters_dup["reporth_ICU"])*parameters_dup["reporth_g"]+
#       parameters_dup["gamma"]*parameters_dup["sigmaER"]*mat_ode[,(ERindex+1)]%*%ihr[,2]*parameters_dup["prob_icu_r"]*(1-parameters_dup["reporth_ICU"])*parameters_dup["reporth_g"]
    
    
#     cases[,i] <- rowSums(incidence) + rowSums(incidenceh)           # daily incidence cases
#     cum_cases[,i] <- colSums(incidence) + colSums(incidenceh)         # cumulative incidence cases
#     day_infections[,i]<- round(rowSums(parameters_dup["gamma"]*mat_ode[,(Eindex+1)]+
#                                          parameters_dup["gamma"]*mat_ode[,(QEindex+1)]+
#                                          parameters_dup["gamma"]*mat_ode[,(EVindex+1)]+
#                                          parameters_dup["gamma"]*mat_ode[,(EVRindex+1)]+
#                                          parameters_dup["gamma"]*mat_ode[,(ERindex+1)])
#                                )
    
#     # overtime proportion of the  population that is infected
#     # different from covidage_v16.5.R
#     infections[, i] <- round(100 * cumsum(
#       rowSums(parameters_dup["gamma"]*mat_ode[,(Eindex+1)]+
#                 parameters_dup["gamma"]*mat_ode[,(QEindex+1)]+
#                 parameters_dup["gamma"]*mat_ode[,(EVindex+1)]+
#                 parameters_dup["gamma"]*mat_ode[,(EVRindex+1)]+
#                 parameters_dup["gamma"]*mat_ode[,(ERindex+1)])
#     ) / sum(popstruc[,2]), 1)
    
    
#     for (w in (ceiling(1/parameters_dup["nui"])+1):nb_times){
#       Rt_aux[w,i]<-cumsum(sum(parameters_dup["gamma"]*mat_ode[w,(Eindex+1)]))/cumsum(sum(parameters_dup["gamma"]*mat_ode[(w-1/parameters_dup["nui"]),(Eindex+1)]))
#       if(Rt_aux[w,i] >= 7) {Rt_aux[w,i]  <- NA}
#     }
#   }
  
#   if (parameters["iterations"] == 1) {
#     results$mean_infections <- infections
#     results$min_infections <- infections
#     results$max_infections <- infections
    
#     results$mean_cases <- cases
#     results$min_cases <- cases
#     results$max_cases <- cases

#     results$mean_cum_cases <- cum_cases
#     results$min_cum_cases <- cum_cases
#     results$max_cum_cases <- cum_cases

#     results$mean_daily_infection <- day_infections
#     results$min_daily_infection <- day_infections
#     results$max_daily_infection <- day_infections

#     results$mean_Rt <- Rt_aux
#     results$min_Rt <- Rt_aux
#     results$max_Rt <- Rt_aux

#     results$mean <- aux[, , 1]
#     results$min <- aux[, , 1]
#     results$max <- aux[, , 1]
#   }
  
#   if (parameters["iterations"] > 1) {
#     #showNotification(HTML("Aggregation of results. <br>This step takes 5 to 15 minutes."), duration = NULL, id = "aggregation_results")
#     results$mean_infections <- apply(infections, 1, quantile, probs = 0.5)
#     results$min_infections <- apply(infections, 1, quantile, probs = parameters["confidence"]/100)
#     results$max_infections <- apply(infections, 1, quantile, probs = (1 - parameters["confidence"]/100))
    
#     results$mean_cases <- apply(cases, 1, quantile, probs = 0.5)
#     results$min_cases <- apply(cases, 1, quantile, probs = parameters["confidence"]/100)
#     results$max_cases <- apply(cases, 1, quantile, probs = (1 - parameters["confidence"]/100))

#     results$mean_daily_infection <- apply(day_infections, 1, quantile, probs = 0.5)
#     results$min_daily_infection <- apply(day_infections, 1, quantile, probs = parameters["confidence"]/100)
#     results$max_daily_infection <- apply(day_infections, 1, quantile, probs = (1 - parameters["confidence"]/100))

#     results$mean_Rt <- apply(Rt_aux, 1, quantile, probs = 0.5, na.rm = TRUE)
#     results$min_Rt <- apply(Rt_aux, 1, quantile, probs = parameters["confidence"]/100, na.rm = TRUE)
#     results$max_Rt <- apply(Rt_aux, 1, quantile, probs = (1 - parameters["confidence"]/100), na.rm = TRUE)
    
#     # these instructions take a long time:
#     results$mean <- apply(aux, 1:2, quantile, probs = 0.5)
#     results$min <- apply(aux, 1:2, quantile, probs = parameters["confidence"]/100)
#     results$max <- apply(aux, 1:2, quantile, probs = (1 - parameters["confidence"]/100))

#   }
#   return(results)
# }
