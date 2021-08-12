# From here: 
# https://github.com/ocelhay/como/blob/1b61938191d9f63d512a3aaec9f5271a3ca0ed5a/misc/scripts/covidage_v16.7.R

# set up a function to solve the equations
covid <- function(t, Y, parameters, input, age_group_vectors, contact_home , contact_school,
    contact_work, contact_other, ihr, ifr, mort, popstruc, popbirth){

  f <- c(1,(1+parameters["give"])/2,(1-parameters["give"])/2,0)
  KH<-parameters["beds_available"]
  KICU<- parameters["icu_beds_available"]+parameters["ventilators_available"]
  Kvent<- parameters["ventilators_available"]
  x.H <- c(0,(1+parameters["give"])*KH/2,(3-parameters["give"])*KH/2,2*KH)
  x.ICU <- c(0,(1+parameters["give"])*KICU/2,(3-parameters["give"])*KICU/2,2*KICU)
  x.Vent <- c(0,(1+parameters["give"])*Kvent/2,(3-parameters["give"])*Kvent/2,2*Kvent)
  fH <- splinefun(x.H, f, method = "hyman")
  fICU <- splinefun(x.ICU, f, method = "hyman")
  fVent<- splinefun(x.Vent, f, method = "hyman")
  
  with(as.list(c(Y, parameters)),
       {
         S <- Y[Sindex]
         E <- Y[Eindex]
         I <- Y[Iindex]
         R <- Y[Rindex]
         X <- Y[Xindex]
         Z <- Y[Zindex]
         H <- Y[Hindex]
         HC <- Y[HCindex]
         C <- Y[Cindex]
         CM <- Y[CMindex]
         V <- Y[Vindex]
         QS <- Y[QSindex]
         QE <- Y[QEindex]
         QI <- Y[QIindex]
         QR <- Y[QRindex]
         CL <- Y[CLindex]
         QC <- Y[QCindex]
         ICU <- Y[ICUindex]
         ICUC <- Y[ICUCindex]
         ICUCV <-Y[ICUCVindex]
         Vent <- Y[Ventindex]
         VentC <- Y[VentCindex]
         CMC <- Y[CMCindex]
         EV <- Y[EVindex]
         ER <- Y[ERindex]
         EVR <- Y[EVRindex]
         VR <- Y[VRindex]
         QV <- Y[QVindex]
         QEV <- Y[QEVindex]
         QER <- Y[QERindex]
         QEVR <- Y[QEVRindex]
         QVR <- Y[QVRindex]
         HCICU <- Y[HCICUindex]
         HCV <- Y[HCVindex]
         Ab <- Y[Abindex]
         
         # Added in como_command_line
         # Adjust p based upon variant
         if( exists("date_range_variant_start") & exists("new_variant_p_multiplier") ){
           if( t > date_range_variant_start ){
             p <- p*new_variant_p_multiplier
           }
         }

         if( exists("date_range_variant2_start") & exists("new_variant2_p_multiplier") ){
           if( t > date_range_variant2_start ){
             p <- p*new_variant2_p_multiplier
           }
	 }

         P <- (S+E+I+R+X+Z+V+H+HC+ICU+ICUC+ICUCV+Vent+VentC+EV+ER+EVR+VR+HCICU+HCV+
                 QS+QE+QI+QR+CL+QC+QEV+QV+QER+QEVR+QVR)
         Q <- (sum(QS)+sum(QE)+sum(QI)+sum(QC)+sum(QR)+sum(QV)+sum(QER)+sum(QEVR)+sum(QEV)+sum(QVR))/sum(P)
         # health system performance
         critH<-min(1-fH(sum(H)+sum(ICUC)+sum(ICUCV)),1)
         crit<-min(1-fICU(sum(ICU)+sum(Vent)+sum(VentC)),1)
         critV<-min(1-fVent(sum(Vent)),1)
         
         # interventions
         isolation<-input$isolation[t*20+1]
         distancing<-input$distancing[t*20+1]
         handwash<-input$handwash[t*20+1]
         masking<-input$masking[t*20+1]
         workhome<-input$workhome[t*20+1]
         schoolclose<-input$schoolclose[t*20+1]
         cocoon<-input$cocoon[t*20+1]
         vaccine<-input$vaccine[t*20+1]
         travelban<-input$travelban[t*20+1]
         screen<-input$screen[t*20+1]
         quarantine<-input$quarantine[t*20+1]
         masstesting<-input$masstesting[t*20+1]
         dexamethasone<-input$dex[t*20+1]
         
         screen_eff<-0
         selfis<-0
         school<-1
         dist<-1
         hand<-0
         mask<-0
         vaccinate<-0
         trvban_eff<-0
         quarantine_rate<-0
         tests_per_day<-0
         
         selfis_cov<-(input$si_vector[t*20+1])/100
         screen_contacts<-(input$scr_vector[t*20+1])/10
         school_eff<-(input$sc_vector[t*20+1])/100
         dist_cov<-(input$sd_vector[t*20+1])/100
         hand_cov<-(input$hw_vector[t*20+1])/100
         mask_cov<-(input$msk_vector[t*20+1])/100
         cocoon<-(input$cte_vector[t*20+1])/100
         work_cov<-(input$wah_vector[t*20+1])/100
         travelban_eff<-(input$tb_vector[t*20+1])/100
         vaccine_cov<-(input$vc_vector[t*20+1])/100
         quarantine_cov<-(input$q_vector[t*20+1])/100
         tests_per_day<-(input$mt_vector[t*20+1])
         vaccineage<-input$vaccineage[t*20+1]
         testage<-input$testage[t*20+1]
         if (vaccine){
           age_vaccine_vector<-as.numeric(age_group_vectors[[vaccineage]])
           vac_rate<-(-log(1-vaccine_cov)/vac_campaign)
           vaccinate<-vac_rate
         }else{age_vaccine_vector<-rep(0,A)}
         if (masstesting){
           age_testing_vector<-as.numeric(age_group_vectors[[testage]])
         }else{age_testing_vector<-rep(0,A)}
         if (workhome){
           work<-work_cov*work_eff
         }else{work<-1}
         if (isolation){
           selfis<-selfis_cov
           if(screen){
             screen_eff<-min(sum(report*I+reportc*(CL)+H+ICU+Vent+reporth*(HC+ICUC+ICUCV+VentC+HCICU+HCV))*screen_contacts*(screen_overdispersion*I/P)*screen_test_sens/P,1) 
           }
         }
         if (schoolclose>=1){
           school<-school_eff
           schoolclose2<-as.numeric(age_group_vectors[[schoolclose]])
         }else{schoolclose2<-0}
         if(distancing){
           dist<-dist_cov*dist_eff
         }
         if(handwash){
           hand<-hand_eff*hand_cov
         }
         if(masking){
           mask<-mask_eff*mask_cov
         }
         if(travelban){
           trvban_eff<-travelban_eff
         }
         if(quarantine){
           rate_q<-min((min(sum((CL+H+ICU+Vent+HC+ICUC+ICUCV+VentC+HCV+HCICU))*(household_size-1)/sum(P),1)*quarantine_effort),quarantine_cov/2)
           quarantine_rate<-rate_q/(1+exp(-10*(quarantine_cov/2-Q)))
         }
         if(dexamethasone){
           prob_v<-prob_vent*vent_dex
           dexo2<-parameters["dexo2"];dexo2c<-parameters["dexo2c"];dexv<-parameters["dexv"];dexvc<-parameters["dexvc"];
         }else{
           dexo2<-1;dexo2c<-1;dexv<-1;dexvc<-1;prob_v<-prob_vent;
         }
         
         # testing rates
         propI<-sum(I)/sum(P)
         propC<-sum(CL)/sum(P)
         propE<-sum(E)/sum(P)
         propEV<-sum(EV)/sum(P)
         propER<-sum(ER)/sum(P)
         propEVR<-sum(EVR)/sum(P)
         propHC<-sum(HC)/sum(P)
         propHCICU<-sum(HCICU)/sum(P)
         propHCV<-sum(HCV)/sum(P)
         testE<-tests_per_day*propE
         testEV<-tests_per_day*propEV
         testER<-tests_per_day*propER
         testEVR<-tests_per_day*propEVR
         testI<-tests_per_day*propI
         testC<-tests_per_day*propC
         testHC<-tests_per_day*propHC
         testHCICU<-tests_per_day*propHCICU
         testHCV<-tests_per_day*propHCV
         
         if(sum(I)>1){
           ratetestI<-mass_test_sens*testI/sum(I)
         }else{ratetestI<-0}
         if(sum(CL)>1){
           ratetestC<-mass_test_sens*testC/sum(CL)
         }else{ratetestC<-0}
         if(sum(E)>1){
           ratetestE<-mass_test_sens*testE/sum(E)
         }else{ratetestE<-0}
         if(sum(EV)>1){
           ratetestEV<-mass_test_sens*testEV/sum(EV)
         }else{ratetestEV<-0}
         if(sum(ER)>1){
           ratetestER<-mass_test_sens*testER/sum(ER)
         }else{ratetestER<-0}
         if(sum(EVR)>1){
           ratetestEVR<-mass_test_sens*testEVR/sum(EVR)
         }else{ratetestEVR<-0}
         if(sum(HC)>1){
           ratetestHC<-mass_test_sens*testHC/sum(HC)
         }else{ratetestHC<-0}
         if(sum(HCICU)>1){
           ratetestHCICU<-mass_test_sens*testHCICU/sum(HCICU)
         }else{ratetestHCICU<-0}
         if(sum(HCV)>1){
           ratetestHCV<-mass_test_sens*testHCV/sum(HCV)
         }else{ratetestHCV<-0}
         
         # cocooning the elderly
         cocoon_mat<-matrix((1-cocoon_eff),nrow = length(popstruc$pop),ncol = length(popstruc$pop))
         cocoon_mat[1:(age_cocoon-1),1:(age_cocoon-1)]<-1
         
         # contact matrices
         cts<-(contact_home+distancing*(1-dist)*contact_other+(1-distancing)*contact_other
               +(1-schoolclose2)*contact_school # school on
               +schoolclose2*(1-school)*contact_school # school close
               +schoolclose2*contact_home*school*s2h # inflating contacts at home when school closes
               +(1-workhome)*contact_work  # normal work
               +workhome*(1-work)*contact_work # people not working from home when homework is active
               +contact_home*workhome*work*w2h # inflating contacts at home when working from home
         )

         
         # Final transmission related parameters
         contacts <- (1-cocoon)*cts+cocoon*cts*cocoon_mat+cocoon*(1+schoolclose2*(1-school_eff)+workhome*(1-work_eff))*contact_home*(1-cocoon_mat)
         seas <- 1+amp*cos(2*3.14*(t-(phi*365.25/12))/365.25)
         importation <- mean_imports*(1-trvban_eff)
         HH<-H+ICU+Vent+ICUC+ICUCV+VentC
         HHC<-HC+HCICU+HCV
         lam <- (1-max(hand,mask))*p*seas*(contacts%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC)+rhos*(HH))/P))+
           (1-max(hand,mask))*p*seas*(1-quarantine*quarantine_eff_other)*(contact_other%*%((rho*QE+QI+QC+QEV+QEVR+QER)/P))
         # contacts under home quarantine
         lamq<-(1-max(hand,mask))*p*seas*((1-quarantine_eff_home)*contact_home%*%(((1-selfis_eff)*(X+HHC+rho*QE+QI+QC++QEV+QEVR+QER))/P))+
           (1-max(hand,mask))*p*seas*(1-quarantine_eff_other)*(contact_other%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC+rho*QE+QI+QC++QEV+QEVR+QER)+rhos*(HH))/P))
         
         # birth/death
         b1<-sum(popbirth[,2]*popstruc[,2])
         birth<-0*popbirth[,2]
         birth[1]<-b1
         
         # ODE system
         dSdt <- -S*lam-vaccinate*age_vaccine_vector*S+omega*R+vac_dur*V-
           quarantine_rate*S +(1/quarantine_days)*QS+ageing%*%S-mort*S+birth
         dEdt <- S*lam-gamma*E+ageing%*%E- vaccinate*age_vaccine_vector*E - mort*E -
           quarantine_rate*E+(1/quarantine_days)*QE
         dIdt <- gamma*(1-pclin)*(1-age_testing_vector*ratetestE)*(1-screen_eff)*(1-ihr[,2])*E+
           gamma*(1-pclin_v)*(1-age_testing_vector*ratetestEV)*(1-screen_eff)*(1-sigmaEV*ihr[,2])*EV+
           gamma*(1-pclin_vr)*(1-age_testing_vector*ratetestEVR)*(1-screen_eff)*(1-sigmaEVR*ihr[,2])*EVR+
           gamma*(1-pclin_r)*(1-age_testing_vector*ratetestER)*(1-screen_eff)*(1-sigmaER*ihr[,2])*ER-
           vaccinate*age_vaccine_vector*I - nui*I+ageing%*%I-mort*I + 
           (1/quarantine_days)*QI - quarantine_rate*I - ratetestI*age_testing_vector*I
         dCLdt<- gamma*pclin*(1-age_testing_vector*ratetestE)*(1-selfis)*(1-ihr[,2])*(1-quarantine_rate)*E+
           gamma*pclin_v*(1-age_testing_vector*ratetestEV)*(1-selfis)*(1-sigmaEV*ihr[,2])*(1-quarantine_rate)*EV+
           gamma*pclin_vr*(1-age_testing_vector*ratetestEVR)*(1-selfis)*(1-sigmaEVR*ihr[,2])*(1-quarantine_rate)*EVR+
           gamma*pclin_r*(1-age_testing_vector*ratetestER)*(1-selfis)*(1-sigmaER*ihr[,2])*(1-quarantine_rate)*ER-
           nui*CL+ageing%*%CL-mort*CL  + (1/quarantine_days)*QC - ratetestC*age_testing_vector*CL
         dRdt <- vac_dur_r*VR-omega*R-vaccinate*age_vaccine_vector*R-lam*sigmaR*R - quarantine_rate*R+
           nui*I+nui*X+nui*CL+ageing%*%R-mort*R + (1/isolation_days)*Z+(1/quarantine_days)*QR+ 
           nus*propo2*(1-dexo2*pdeath_ho)*ifr[,2]*H+nus*(1-propo2)*(1-pdeath_h)*ifr[,2]*H+
           nusc*propo2*(1-pdeath_hco)*ifr[,2]*HC+nusc*(1-propo2)*(1-pdeath_hc)*ifr[,2]*HC+  
           nusc*propo2*(1-pdeath_icu_hco)*ifr[,2]*HCICU+nusc*(1-propo2)*(1-pdeath_icu_hc)*ifr[,2]*HCICU+
           nu_icu*propo2*(1-dexo2*pdeath_icuo)*ifr[,2]*ICU+nu_icu*(1-propo2)*(1-pdeath_icu)*ifr[,2]*ICU+
           nu_icuc*propo2*(1-dexo2c*pdeath_icuco)*ifr[,2]*ICUC+nu_icuc*(1-propo2)*(1-pdeath_icuc)*ifr[,2]*ICUC+
           nu_vent*(1-dexv*pdeath_vent)*ifr[,2]*Vent+
           nu_ventc*(1-pdeath_vent_hc)*ifr[,2]*HCV+
           nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*VentC+nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*ICUCV 
         dXdt <- gamma*selfis*(1-age_testing_vector*ratetestE)*pclin*(1-ihr[,2])*E+
           gamma*(1-pclin)*(1-age_testing_vector*ratetestE)*screen_eff*(1-ihr[,2])*E+
           gamma*selfis*(1-age_testing_vector*ratetestEV)*pclin_v*(1-sigmaEV*ihr[,2])*EV+
           gamma*(1-pclin_v)*(1-age_testing_vector*ratetestEV)*screen_eff*(1-sigmaEV*ihr[,2])*EV+
           gamma*selfis*(1-age_testing_vector*ratetestEVR)*pclin_v*(1-sigmaEVR*ihr[,2])*EVR+
           gamma*(1-pclin_vr)*(1-age_testing_vector*ratetestEVR)*screen_eff*(1-sigmaEVR*ihr[,2])*EVR+
           gamma*selfis*(1-age_testing_vector*ratetestER)*pclin_r*(1-sigmaER*ihr[,2])*ER+
           gamma*(1-pclin_r)*(1-age_testing_vector*ratetestER)*screen_eff*(1-sigmaER*ihr[,2])*ER+
           -nui*X+ageing%*%X-mort*X 
         dVdt <- vaccinate*age_vaccine_vector*S + omega*VR - (1-vaccine_eff)*lam*V - vac_dur*V + ageing%*%V-mort*V - quarantine_rate*V
         dEVdt<- (1-vaccine_eff)*lam*V - gamma*EV + ageing%*%EV - mort*EV - quarantine_rate*EV +(1/quarantine_days)*QEV
         dERdt<- lam*sigmaR*R - gamma*ER + ageing%*%ER - mort*ER - quarantine_rate*ER +(1/quarantine_days)*QER
         dVRdt <- vaccinate*age_vaccine_vector*E + vaccinate*age_vaccine_vector*I + vaccinate*age_vaccine_vector*R -
           (1-vaccine_eff_r)*lam*VR - vac_dur_r*VR + ageing%*%VR - mort*VR - omega*VR - quarantine_rate*VR + (1/quarantine_days)*QVR
         dEVRdt<- (1-vaccine_eff_r)*lam*VR - gamma*EVR + ageing%*%EVR-mort*EVR - quarantine_rate*EVR +
           (1/quarantine_days)*QEVR
         
         
         dQSdt <- quarantine_rate*S + ageing%*%QS-mort*QS - (1/quarantine_days)*QS - lamq*QS
         dQEdt <- quarantine_rate*E - gamma*QE + ageing%*%QE-mort*QE - (1/quarantine_days)*QE + lamq*QS 
         dQIdt <- quarantine_rate*I + gamma*(1-ihr[,2])*(1-pclin)*QE+
           gamma*(1-sigmaEV*ihr[,2])*(1-pclin_v)*QEV+
           gamma*(1-sigmaER*ihr[,2])*(1-pclin_r)*QER+           
           gamma*(1-sigmaEVR*ihr[,2])*(1-pclin_vr)*QEVR-
           nui*QI+ageing%*%QI-mort*QI - (1/quarantine_days)*QI
         dQCdt <- gamma*pclin*(1-selfis)*(1-age_testing_vector*ratetestE)*(1-ihr[,2])*quarantine_rate*E+
           gamma*pclin_v*(1-age_testing_vector*ratetestEV)*(1-selfis)*(1-sigmaEV*ihr[,2])*quarantine_rate*EV+
           gamma*pclin_vr*(1-age_testing_vector*ratetestEVR)*(1-selfis)*(1-sigmaEVR*ihr[,2])*quarantine_rate*EVR+
           gamma*pclin_r*(1-age_testing_vector*ratetestER)*(1-selfis)*(1-sigmaER*ihr[,2])*quarantine_rate*ER+
           gamma*(1-ihr[,2])*pclin*QE + 
           gamma*(1-sigmaEV*ihr[,2])*pclin_v*QEV + 
           gamma*(1-sigmaER*ihr[,2])*pclin_r*QER + 
           gamma*(1-sigmaEVR*ihr[,2])*pclin_vr*QEVR -
           nui*QC+ageing%*%QC-mort*QC - (1/quarantine_days)*QC
         dQRdt <- quarantine_rate*R + nui*QI + nui*QC + ageing%*%QR-mort*QR - (1/quarantine_days)*QR + vac_dur_r*QVR
         dQVdt <- quarantine_rate*V + ageing%*%QV-mort*QV - (1/quarantine_days)*QV - (1-vaccine_eff)*lamq*QV + omega*QVR 
         dQEVdt <- quarantine_rate*EV - gamma*QEV + ageing%*%QEV-mort*QEV - (1/quarantine_days)*QEV + (1-vaccine_eff)*lamq*QV 
         dQERdt <- quarantine_rate*ER - gamma*QER + ageing%*%QER-mort*QER - (1/quarantine_days)*QER + sigmaR*lamq*QR 
         dQVRdt <- quarantine_rate*VR - (1-vaccine_eff_r)*lam*QVR - vac_dur_r*QVR - omega*QVR + ageing%*%QVR - mort*QVR 
         dQEVRdt <- quarantine_rate*EVR - gamma*QEVR +ageing%*%QEVR-mort*QEVR -
           (1/quarantine_days)*QEVR +(1-vaccine_eff_r)*lamq*QVR 
            
         
         dHdt <- gamma*ihr[,2]*(1-prob_icu)*(1-critH)*reporth*E+ 
           gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*(1-critH)*reporth*EV + 
           gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*(1-critH)*reporth*EVR + 
           gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*(1-critH)*reporth*ER + 
           gamma*ihr[,2]*(1-prob_icu)*(1-critH)*reporth*QE +
           gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*(1-critH)*reporth*QEV + 
           gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*(1-critH)*reporth*QEVR + 
           gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*(1-critH)*reporth*QER - 
           nus*H + ageing%*%H-mort*H 
         dHCdt <- gamma*ihr[,2]*(1-prob_icu)*(1-reporth)*E+gamma*ihr[,2]*(1-prob_icu)*critH*reporth*E + 
           gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*(1-reporth)*EV+gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*critH*reporth*EV+
           gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*(1-reporth)*EVR+gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*critH*reporth*EVR+
           gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*(1-reporth)*ER+gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*critH*reporth*ER +
           gamma*ihr[,2]*(1-prob_icu)*(1-reporth)*QE+gamma*ihr[,2]*(1-prob_icu)*critH*reporth*QE+
           gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*(1-reporth)*QEV+gamma*sigmaEV*ihr[,2]*(1-prob_icu_v)*critH*reporth*QEV+
           gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*(1-reporth)*QEVR+gamma*sigmaEVR*ihr[,2]*(1-prob_icu_vr)*critH*reporth*QEVR+
           gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*(1-reporth)*QER+gamma*sigmaER*ihr[,2]*(1-prob_icu_r)*critH*reporth*QER - 
           nusc*HC + ageing%*%HC-mort*HC - ratetestHC*age_testing_vector*HC
         dHCICUdt <- gamma*(1-reporth_ICU)*ihr[,2]*prob_icu*(1-prob_v)*E+
           gamma*(1-reporth_ICU)*sigmaEV*ihr[,2]*prob_icu_v*(1-prob_v_v)*EV+
           gamma*(1-reporth_ICU)*sigmaEVR*ihr[,2]*prob_icu_vr*(1-prob_v_vr)*EVR+
           gamma*(1-reporth_ICU)*sigmaER*ihr[,2]*prob_icu_r*(1-prob_v_r)*ER+
           gamma*(1-reporth_ICU)*ihr[,2]*prob_icu*(1-prob_v)*QE+
           gamma*(1-reporth_ICU)*sigmaEV*ihr[,2]*prob_icu_v*(1-prob_v_v)*QEV+
           gamma*(1-reporth_ICU)*sigmaEVR*ihr[,2]*prob_icu_vr*(1-prob_v_vr)*QEVR+
           gamma*(1-reporth_ICU)*sigmaER*ihr[,2]*prob_icu_r*(1-prob_v_r)*QER-
           nusc*HCICU + ageing%*%HCICU-mort*HCICU - ratetestHCICU*age_testing_vector*HCICU
         dHCVdt <- gamma*(1-reporth_ICU)*ihr[,2]*prob_icu*prob_v*E+
           gamma*(1-reporth_ICU)*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*EV+
           gamma*(1-reporth_ICU)*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*EVR+
           gamma*(1-reporth_ICU)*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*ER+
           gamma*(1-reporth_ICU)*ihr[,2]*prob_icu*prob_v*QE+
           gamma*(1-reporth_ICU)*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*QEV+
           gamma*(1-reporth_ICU)*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*QEVR+
           gamma*(1-reporth_ICU)*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*QER-
           nu_ventc*HCV + ageing%*%HCV-mort*HCV - ratetestHCV*age_testing_vector*HCV 
         dICUdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*(1-crit)*(1-prob_v)*E+ 
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*(1-crit)*(1-prob_v_v)*EV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*(1-crit)*(1-prob_v_vr)*EVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*(1-crit)*(1-prob_v_r)*ER+
           gamma*reporth_ICU*ihr[,2]*prob_icu*(1-crit)*(1-prob_v)*QE+ 
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*(1-crit)*(1-prob_v_v)*QEV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*(1-crit)*(1-prob_v_vr)*QEVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*(1-crit)*(1-prob_v_r)*QER - 
           nu_icu*ICU +ageing%*%ICU - mort*ICU + (1-crit)*ICUC*1/2
         dICUCdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*crit*(1-prob_v)*E+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*crit*(1-prob_v_v)*EV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*crit*(1-prob_v_vr)*EVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*crit*(1-prob_v_r)*ER+
           gamma*reporth_ICU*ihr[,2]*prob_icu*crit*(1-prob_v)*QE+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*crit*(1-prob_v_v)*QEV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*crit*(1-prob_v_vr)*QEVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*crit*(1-prob_v_r)*QER - 
           nu_icuc*ICUC -(1-crit)*ICUC*1/2 +ageing%*%ICUC - mort*ICUC 
         dICUCVdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*prob_v*crit*E+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*crit*EV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*crit*EVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*crit*ER+
           gamma*reporth_ICU*ihr[,2]*prob_icu*prob_v*crit*QE+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*crit*QEV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*crit*QEVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*crit*QER -
           nu_ventc*ICUCV +ageing%*%ICUCV - mort*ICUCV - (1-critV)*ICUCV*1/2
         dVentdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_v*E+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*(1-crit)*(1-critV)*prob_v_v*EV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*(1-crit)*(1-critV)*prob_v_vr*EVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*(1-crit)*(1-critV)*prob_v_r*ER+
           gamma*reporth_ICU*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_v*QE+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*(1-crit)*(1-critV)*prob_v_v*QEV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*(1-crit)*(1-critV)*prob_v_vr*QEVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*(1-crit)*(1-critV)*prob_v_r*QER +
           (1-critV)*VentC*1/2 +(1-critV)*ICUCV*1/2 - nu_vent*Vent +ageing%*%Vent - mort*Vent 
         dVentCdt <- gamma*reporth_ICU*ihr[,2]*prob_icu*prob_v*(1-crit)*critV*E+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*(1-crit)*critV*EV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*(1-crit)*critV*EVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*(1-crit)*critV*ER+
           gamma*reporth_ICU*ihr[,2]*prob_icu*prob_v*(1-crit)*critV*QE+
           gamma*reporth_ICU*sigmaEV*ihr[,2]*prob_icu_v*prob_v_v*(1-crit)*critV*QEV+
           gamma*reporth_ICU*sigmaEVR*ihr[,2]*prob_icu_vr*prob_v_vr*(1-crit)*critV*QEVR+
           gamma*reporth_ICU*sigmaER*ihr[,2]*prob_icu_r*prob_v_r*(1-crit)*critV*QER - 
           (1-critV)*VentC*1/2 -nu_ventc*VentC +ageing%*%VentC - mort*VentC 
         
         dCdt <- report*gamma*(1-age_testing_vector*ratetestE)*(1-pclin)*(1-ihr[,2])*(E+QE)+reportc*gamma*pclin*(1-age_testing_vector*ratetestE)*(1-ihr[,2])*(E+QE)+
           gamma*ihr[,2]*(1-critH)*(1-prob_icu)*(E+QE)+gamma*ihr[,2]*critH*reporth*(1-prob_icu)*(E+QE)+
           gamma*ihr[,2]*prob_icu*(E+QE)+ratetestI*age_testing_vector*I+ratetestC*age_testing_vector*CL+gamma*age_testing_vector*ratetestE*(1-ihr[,2])*E
         dCMdt<- nus*propo2*dexo2*pdeath_ho*ifr[,2]*H+nus*(1-propo2)*pdeath_h*ifr[,2]*H+
           nusc*report_death_HC*propo2*pdeath_hco*ifr[,2]*HC+nusc*report_death_HC*(1-propo2)*pdeath_hc*ifr[,2]*HC+
           nu_icu*propo2*dexo2*pdeath_icuo*ifr[,2]*ICU+nu_icu*(1-propo2)*pdeath_icu*ifr[,2]*ICU+
           nu_icuc*propo2*dexo2c*pdeath_icuco*ifr[,2]*ICUC+nu_icuc*(1-propo2)*pdeath_icuc*ifr[,2]*ICUC+
           nu_vent*dexv*pdeath_vent*ifr[,2]*Vent+nu_ventc*dexvc*pdeath_ventc*ifr[,2]*VentC +
           nu_ventc*dexvc*pdeath_ventc*ifr[,2]*ICUCV+ nu_ventc*report_death_HC*pdeath_vent_hc*ifr[,2]*HCV+
           nusc*report_death_HC*propo2*pdeath_icu_hco*ifr[,2]*HCICU+
           nusc*report_death_HC*(1-propo2)*pdeath_icu_hc*ifr[,2]*HCICU +
           mort*H + mort*ICU + mort*ICUC + mort*ICUCV + mort*Vent + mort*VentC + mort*Z + 
           mort*report_death_HC*HC +mort*report_death_HC*HCICU + mort*report_death_HC*HCV +
           report_natdeathI*mort*I + report_natdeathI*mort*QI+ report_natdeathI*mort*E+
           report_natdeathI*mort*QE + report_natdeathI*mort*EV+ report_natdeathI*mort*EVR+
           report_natdeathI*mort*ER + report_natdeathI*mort*QEV+
           report_natdeathI*mort*QEVR + report_natdeathI*mort*QER+
           report_natdeathCL*mort*CL + report_natdeathCL*mort*QC + report_natdeathCL*mort*X
         dCMCdt <- nusc*propo2*pdeath_hco*ifr[,2]*HC+nusc*(1-propo2)*pdeath_hc*ifr[,2]*HC+
           nu_icuc*propo2*dexo2c*pdeath_icuco*ifr[,2]*ICUC+nu_icuc*(1-propo2)*pdeath_icuc*ifr[,2]*ICUC+
           nu_ventc*dexvc*pdeath_ventc*ifr[,2]*VentC+nu_ventc*dexvc*pdeath_ventc*ifr[,2]*ICUCV+
           mort*HC + mort*ICUC + mort*VentC + mort*ICUCV 
         
         dZdt <- gamma*ratetestE*age_testing_vector*(1-ihr[,2])*E+
           ratetestI*age_testing_vector*I+
           ratetestC*age_testing_vector*CL+
           gamma*(1-ihr[,2])*ratetestEV*age_testing_vector*EV+
           gamma*(1-ihr[,2])*ratetestEVR*age_testing_vector*EVR+
           gamma*(1-ihr[,2])*ratetestER*age_testing_vector*ER+
           ratetestHC*age_testing_vector*HC+
           ratetestHCICU*age_testing_vector*HCICU+
           ratetestHCV*age_testing_vector*HCV-
           (1/isolation_days)*Z-mort*Z
         
         dAbdt <- nui*I+nui*X+nui*CL+ 
           nus*propo2*(1-dexo2*pdeath_ho)*ifr[,2]*H+nus*(1-propo2)*(1-pdeath_h)*ifr[,2]*H+
           nusc*propo2*(1-pdeath_hco)*ifr[,2]*HC+nusc*(1-propo2)*(1-pdeath_hc)*ifr[,2]*HC+  
           nusc*propo2*(1-pdeath_icu_hco)*ifr[,2]*HCICU+nusc*(1-propo2)*(1-pdeath_icu_hc)*ifr[,2]*HCICU+
           nu_ventc*(1-pdeath_vent_hc)*ifr[,2]*HCV+
           nu_icu*propo2*(1-dexo2*pdeath_icuo)*ifr[,2]*ICU+nu_icu*(1-propo2)*(1-pdeath_icu)*ifr[,2]*ICU+
           nu_icuc*propo2*(1-dexo2c*pdeath_icuco)*ifr[,2]*ICUC+nu_icuc*(1-propo2)*(1-pdeath_icuc)*ifr[,2]*ICUC+
           nu_vent*(1-dexv*pdeath_vent)*ifr[,2]*Vent+
           nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*VentC+
           nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*ICUCV - 
           seroneg*Ab - mort*Ab + ageing%*%Ab
         
         # return the rate of change
         list(c(S=dSdt,dEdt,dIdt,dRdt,dXdt,dHdt,dHCdt,dCdt,dCMdt,dVdt,dQSdt,dQEdt,dQIdt,dQRdt,dCLdt,dQCdt,dICUdt,dICUCdt,dICUCVdt,
                dVentdt,dVentCdt,dCMCdt,dZdt,dEVdt,dERdt,dEVRdt,dVRdt,dQVdt,dQEVdt,dQEVRdt,dQERdt,dQVRdt,dHCICUdt,dHCVdt,dAbdt))
       }
  ) 
}



# ####################################################################
# # FOR CoMo V18 and onwards (cause includes partial school closures)
# ####################################################################

# f <- c(1,(1+parameters["give"])/2,(1-parameters["give"])/2,0)
# KH<-parameters["beds_available"]
# KICU<- parameters["icu_beds_available"]+parameters["ventilators_available"]
# Kvent<- parameters["ventilators_available"]
# x.H <- c(0,(1+parameters["give"])*KH/2,(3-parameters["give"])*KH/2,2*KH)
# x.ICU <- c(0,(1+parameters["give"])*KICU/2,(3-parameters["give"])*KICU/2,2*KICU)
# x.Vent <- c(0,(1+parameters["give"])*Kvent/2,(3-parameters["give"])*Kvent/2,2*Kvent)
# fH <- splinefun(x.H, f, method = "hyman")
# fICU <- splinefun(x.ICU, f, method = "hyman")
# fVent<- splinefun(x.Vent, f, method = "hyman")


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
