


shinyServer(function(input, output, session) {
  
  pop_by_age <- reactive({
    # Demography
    pop_by_age <- data.frame(age_group = dhaka_pop_by_age$age_group)
    pop_by_age$prop <- c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
                         input$demog_5,input$demog_6,input$demog_7,input$demog_8,
                         input$demog_9)/100
    pop_by_age$pop <- pop_by_age$prop*input$pop
    
    
    
    return(pop_by_age)
  })
  observeEvent(input$pop_reset,{
    
    # Return ui inputs to defaults
    updateNumericInput(session, inputId = "pop", value = as.numeric(parms_baseline["population"]))
    updateNumericInput(session, inputId = "demog_1", value = round(dhaka_pop_by_age$prop[1]*100,1))
    updateNumericInput(session, inputId = "demog_2", value = round(dhaka_pop_by_age$prop[2]*100,1))
    updateNumericInput(session, inputId = "demog_3", value = round(dhaka_pop_by_age$prop[3]*100,1))
    updateNumericInput(session, inputId = "demog_4", value = round(dhaka_pop_by_age$prop[4]*100,1))
    updateNumericInput(session, inputId = "demog_5", value = round(dhaka_pop_by_age$prop[5]*100,1))
    updateNumericInput(session, inputId = "demog_6", value = round(dhaka_pop_by_age$prop[6]*100,1))
    updateNumericInput(session, inputId = "demog_7", value = round(dhaka_pop_by_age$prop[7]*100,1))
    updateNumericInput(session, inputId = "demog_8", value = round(dhaka_pop_by_age$prop[8]*100,1))
    updateNumericInput(session, inputId = "demog_9", value = round(dhaka_pop_by_age$prop[9]*100,1))
    # updateSliderInput(session, inputId = "initial_immune", value = 25)
    # updateNumericInput(session, inputId = "initial_infectious", value = 11451)
    # updateNumericInput(session, inputId = "days", value = 120)
    
  })

  # Adjust baseline parameters based on selected R0
  parms_baseline_adjust <- reactive({

    # Update R0
    parms_edit <- parms_baseline
    parms_edit["R0"] <- input$bl_R0
    
    # Calculate multiplier to be applied to betas based on input R0
    beta_multiplier <- input$bl_R0/parms_baseline["R0"]
    

    # Adjust baseline transmission rates based on the multiplier
    parms_edit[c("beta_p","beta_a","beta_s")] <-
      parms_edit[c("beta_p","beta_a","beta_s")]*beta_multiplier

    # Make additional edits from the baseline tab of sidepanel
    parms_edit["ld"]=as.logical(input$bl_ld) # Is there a lockdown?
    parms_edit["syndromic"]=as.logical(input$bl_syndromic) # Is there syndromic surveillance?
    parms_edit["mask"]=as.logical(input$bl_mask) # Is there mask wearing?
    parms_edit["ld_start"]=as.numeric(input$bl_ld_dates[1] - start_date)
    parms_edit["ld_end"]=as.numeric(input$bl_ld_dates[2] - start_date) # When does the lockdown start and end
    parms_edit["fEW"]=input$bl_fEW*parms_baseline["propWorkers"]/100 # What proportion of people are essential workers?
    parms_edit["fNC"]=1-input$bl_ld_compliance/100 # What proportion of people are non-compliant to lockdown?
    parms_edit["ld_improve"]=input$bl_ld_improve # How many days does it take for the full effect of the lockdown to be reached?
    parms_edit["community"]=input$bl_community/100 # capacity of community HWs supporting isolation
    parms_edit["syn_start"]=as.numeric(input$bl_syn_dates[1] - start_date)
    parms_edit["syn_end"]=as.numeric(input$bl_syn_dates[2] - start_date) # When does the syndromic surveillance start and end
    parms_edit["syn_improve"]=input$bl_syn_improve # How many days does it take for the full effect of the syndromic surveillance to be reached?
    parms_edit["mask_start"]=as.numeric(input$bl_mask_dates[1] - start_date)
    parms_edit["mask_end"]=as.numeric(input$bl_mask_dates[2] - start_date) # When does mask wearing start and end
    parms_edit["mask_effect_outward"]=input$bl_mask_effect_out # By what proportion does mask wearing reduce transmission to others
    parms_edit["f_mask_effect_inward"]=input$bl_mask_effect_in/input$bl_mask_effect_out # What proportion of mask_effect_outward is the impact of masks in protecting the wearer from infection
    parms_edit["mask_compliance"]=input$bl_mask_compliance/100 # What proportion of people are compliant to mask wearing?
    parms_edit["mask_improve"]=input$bl_mask_improve # How many days does it take for the full effect of mask wearing to be reached?
    parms_edit["vax_order"]=as.numeric(input$bl_vax_order)
    parms_edit["vax_compliance"]=input$bl_vax_compliance/100
    parms_edit["vax_transmission_effect_dose1"]=input$bl_vax_transmission_effect_dose1/100
    parms_edit["vax_transmission_effect_dose2"]=input$bl_vax_transmission_effect_dose2/100
    parms_edit["vax_severity_effect_dose1"]=input$bl_vax_severity_effect_dose1/100
    parms_edit["vax_severity_effect_dose2"]=input$bl_vax_severity_effect_dose2/100
    parms_edit["vax_delay"]=input$bl_vax_delay
    parms_edit["population"]=input$pop
    
    if(sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
             input$demog_5,input$demog_6,input$demog_7,input$demog_8,
             input$demog_9))>=99.5 &
       sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
             input$demog_5,input$demog_6,input$demog_7,input$demog_8,
             input$demog_9))<=100.5){
      
      # Demography
      pop_by_age <- data.frame(age_group = dhaka_pop_by_age$age_group)
      pop_by_age$prop <- c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
                           input$demog_5,input$demog_6,input$demog_7,input$demog_8,
                           input$demog_9)/100
      pop_by_age$pop <- pop_by_age$prop*input$pop
      
      # Make edits
      parms_edit[c("fa","fd","fHosp")] <- calc_fractions(age_dep_pars,pop_by_age,
                                                         Vax1 = dhaka_vax[which(dhaka_vax$date==(start_date-1)),"Vax1"],
                                                         Vax2 = dhaka_vax[which(dhaka_vax$date==(start_date-1)),"Vax2"],
                                                         model_parms = parms_edit)
    }
    
    return(parms_edit)

  })
  
  
  

  
  # Change minimum and value of second lockdown date if dates of first lockdown change
  observeEvent(input$int_ld_dates, {
    updateSliderInput(session, inputId = "int_ld2_dates",value=c(max(input$int_ld_dates[2],input$int_ld2_dates[1]),max(input$int_ld_dates[2],input$int_ld2_dates[2])), min=input$int_ld_dates[2], timeFormat = "%d %b %y")
  })
  observeEvent(input$bl_ld_dates, {
    updateSliderInput(session, inputId = "bl_ld2_dates",value=c(max(input$bl_ld_dates[2],input$bl_ld2_dates[1]),max(input$bl_ld_dates[2],input$bl_ld2_dates[2])), min=input$bl_ld_dates[2], timeFormat = "%d %b %y")
  })
  observeEvent(input$int2_ld_dates, {
    updateSliderInput(session, inputId = "int2_ld2_dates",value=c(max(input$int2_ld_dates[2],input$int2_ld2_dates[1]),max(input$int2_ld_dates[2],input$int2_ld2_dates[2])), min=input$int2_ld_dates[2], timeFormat = "%d %b %y")
  })

  # Update max and value of inward max effect if changing outward effect
  observeEvent(input$int_mask_effect_out, {
    updateSliderInput(session, inputId = "int_mask_effect_in",value=min(input$int_mask_effect_in,input$int_mask_effect_out), max=input$int_mask_effect_out)
  })
  observeEvent(input$bl_mask_effect_out, {
    updateSliderInput(session, inputId = "bl_mask_effect_in",value=min(input$bl_mask_effect_in,input$bl_mask_effect_out), max=input$bl_mask_effect_out)
  })
  observeEvent(input$int2_mask_effect_out, {
    updateSliderInput(session, inputId = "int2_mask_effect_in",value=min(input$int2_mask_effect_in,input$int2_mask_effect_out), max=input$int2_mask_effect_out)
  })
  

  # Parameters for scenario 1
  parms_intervention <- reactive({

    # Update R0
    parms_edit <- parms_baseline
    parms_edit["R0"] <- input$int_R0
    
    # Calculate multiplier to be applied to betas based on input R0
    beta_multiplier <- input$int_R0/parms_baseline["R0"]
    
    # Adjust transmission rates based on the multiplier
    parms_edit[c("beta_p","beta_a","beta_s")] <-
      parms_edit[c("beta_p","beta_a","beta_s")]*beta_multiplier
    
    
    # Make intervention parameter edits
    parms_edit["ld"]=as.logical(input$int_ld) # Is there a lockdown?
    parms_edit["testing"]=as.logical(input$int_testing) # Is there testing?
    parms_edit["syndromic"]=as.logical(input$int_syndromic) # Is there syndromic surveillance?
    parms_edit["mask"]=as.logical(input$int_mask) # Is there mask wearing?
    parms_edit["ld_start"]=as.numeric(input$int_ld_dates[1] - start_date)
    parms_edit["ld_end"]=as.numeric(input$int_ld_dates[2] - start_date) # When does the lockdown start and end
    parms_edit["fEW"]=input$int_fEW*parms_baseline["propWorkers"]/100 # What proportion of people are essential workers?
    parms_edit["fNC"]=1-input$int_ld_compliance/100 # What proportion of people are non-compliant to lockdown?
    parms_edit["ld_improve"]=input$int_ld_improve # How many days does it take for the full effect of the lockdown to be reached?
    parms_edit["community"]=input$int_community/100 # capacity of community HWs supporting isolation
    parms_edit["syn_start"]=as.numeric(input$int_syn_dates[1] - start_date)
    parms_edit["syn_end"]=as.numeric(input$int_syn_dates[2] - start_date) # When does the syndromic surveillance start and end
    parms_edit["test_start"]=as.numeric(input$int_test_dates[1] - start_date)
    parms_edit["test_end"]=as.numeric(input$int_test_dates[2] - start_date) # When does the testing start and end
    parms_edit["test_capacity"]=input$int_test_capacity # testing capacity
    parms_edit["test_compliance"]=input$int_test_compliance/100 # testing compliance
    parms_edit["test_fneg"]=input$int_test_fneg # false negative probability for test
    parms_edit["mask_start"]=as.numeric(input$int_mask_dates[1] - start_date)
    parms_edit["mask_end"]=as.numeric(input$int_mask_dates[2] - start_date) # When does mask wearing start and end
    parms_edit["mask_effect_outward"]=input$int_mask_effect_out # By what proportion does mask wearing reduce transmission to others
    parms_edit["f_mask_effect_inward"]=input$int_mask_effect_in/input$int_mask_effect_out # What proportion of mask_effect_outward is the impact of masks in protecting the wearer from infection
    parms_edit["mask_compliance"]=input$int_mask_compliance/100 # What proportion of people are compliant to mask wearing?
    parms_edit["mask_improve"]=input$int_mask_improve # How many days does it take for the full effect of mask wearing to be reached?
    parms_edit["vax"]=as.logical(input$int_vax)
    parms_edit["vax_2_doses"]=as.logical(input$int_vax_2_doses)
    parms_edit["vax_order"]=as.numeric(input$int_vax_order)
    parms_edit["vax_compliance"]=input$int_vax_compliance/100
    parms_edit["vax_transmission_effect_dose1"]=input$int_vax_transmission_effect_dose1/100
    parms_edit["vax_transmission_effect_dose2"]=input$int_vax_transmission_effect_dose2/100
    parms_edit["vax_severity_effect_dose1"]=input$int_vax_severity_effect_dose1/100
    parms_edit["vax_severity_effect_dose2"]=input$int_vax_severity_effect_dose2/100
    parms_edit["t_between_doses"]=input$int_t_between_doses
    parms_edit["vax_rate"]=input$int_vax_rate
    parms_edit["maxVax"]=input$int_maxVax/100
    parms_edit["vax_start"]=as.numeric(input$int_vax_dates[1] - start_date)
    parms_edit["vax_end"]=as.numeric(input$int_vax_dates[2] - start_date)
    parms_edit["vax_delay"]=input$int_vax_delay
    parms_edit["population"]=input$pop
    
    if(sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
             input$demog_5,input$demog_6,input$demog_7,input$demog_8,
             input$demog_9))>=99.5 &
       sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
             input$demog_5,input$demog_6,input$demog_7,input$demog_8,
             input$demog_9))<=100.5){
      
      # Demography
      pop_by_age <- data.frame(age_group = dhaka_pop_by_age$age_group)
      pop_by_age$prop <- c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
                           input$demog_5,input$demog_6,input$demog_7,input$demog_8,
                           input$demog_9)/100
      pop_by_age$pop <- pop_by_age$prop*input$pop
      
      # Make edits
      parms_edit[c("fa","fd","fHosp")] <- calc_fractions(age_dep_pars,pop_by_age)
    }
    return(parms_edit)
  })
  
  # Parameters for scenario 2
  parms_intervention2 <- reactive({
    
    # Update R0
    parms_edit <- parms_baseline
    parms_edit["R0"] <- input$int2_R0
    
    # Calculate multiplier to be applied to betas based on input R0
    beta_multiplier <- input$int2_R0/parms_baseline["R0"]
    
    # Adjust transmission rates based on the multiplier
    parms_edit[c("beta_p","beta_a","beta_s")] <-
      parms_edit[c("beta_p","beta_a","beta_s")]*beta_multiplier
    
    
    # Make intervention parameter edits
    parms_edit["ld"]=as.logical(input$int2_ld) # Is there a lockdown?
    parms_edit["testing"]=as.logical(input$int2_testing) # Is there testing?
    parms_edit["syndromic"]=as.logical(input$int2_syndromic) # Is there syndromic surveillance?
    parms_edit["mask"]=as.logical(input$int2_mask) # Is there mask wearing?
    parms_edit["ld_start"]=as.numeric(input$int2_ld_dates[1] - start_date)
    parms_edit["ld_end"]=as.numeric(input$int2_ld_dates[2] - start_date) # When does the lockdown start and end
    parms_edit["fEW"]=input$int2_fEW*parms_baseline["propWorkers"]/100 # What proportion of people are essential workers?
    parms_edit["fNC"]=1-input$int2_ld_compliance/100 # What proportion of people are non-compliant to lockdown?
    parms_edit["ld_improve"]=input$int2_ld_improve # How many days does it take for the full effect of the lockdown to be reached?
    parms_edit["community"]=input$int2_community/100 # capacity of community HWs supporting isolation
    parms_edit["syn_start"]=as.numeric(input$int2_syn_dates[1] - start_date)
    parms_edit["syn_end"]=as.numeric(input$int2_syn_dates[2] - start_date) # When does the syndromic surveillance start and end
    parms_edit["test_start"]=as.numeric(input$int2_test_dates[1] - start_date)
    parms_edit["test_end"]=as.numeric(input$int2_test_dates[2] - start_date) # When does the testing start and end
    parms_edit["test_capacity"]=input$int2_test_capacity # testing capacity
    parms_edit["test_compliance"]=input$int2_test_compliance/100 # testing compliance
    parms_edit["test_fneg"]=input$int2_test_fneg # false negative probability for test
    parms_edit["mask_start"]=as.numeric(input$int2_mask_dates[1] - start_date)
    parms_edit["mask_end"]=as.numeric(input$int2_mask_dates[2] - start_date) # When does mask wearing start and end
    parms_edit["mask_effect_outward"]=input$int2_mask_effect_out # By what proportion does mask wearing reduce transmission to others
    parms_edit["f_mask_effect_inward"]=input$int2_mask_effect_in/input$int2_mask_effect_out # What proportion of mask_effect_outward is the impact of masks in protecting the wearer from infection
    parms_edit["mask_compliance"]=input$int2_mask_compliance/100 # What proportion of people are compliant to mask wearing?
    parms_edit["mask_improve"]=input$int2_mask_improve # How many days does it take for the full effect of mask wearing to be reached?
    parms_edit["vax"]=as.logical(input$int2_vax)
    parms_edit["vax_2_doses"]=as.logical(input$int2_vax_2_doses)
    parms_edit["vax_order"]=as.numeric(input$int2_vax_order)
    parms_edit["vax_compliance"]=input$int2_vax_compliance/100
    parms_edit["vax_transmission_effect_dose1"]=input$int2_vax_transmission_effect_dose1/100
    parms_edit["vax_transmission_effect_dose2"]=input$int2_vax_transmission_effect_dose2/100
    parms_edit["vax_severity_effect_dose1"]=input$int2_vax_severity_effect_dose1/100
    parms_edit["vax_severity_effect_dose2"]=input$int2_vax_severity_effect_dose2/100
    parms_edit["t_between_doses"]=input$int2_t_between_doses
    parms_edit["vax_rate"]=input$int2_vax_rate
    parms_edit["maxVax"]=input$int2_maxVax/100
    parms_edit["vax_start"]=as.numeric(input$int2_vax_dates[1] - start_date)
    parms_edit["vax_end"]=as.numeric(input$int2_vax_dates[2] - start_date)
    parms_edit["vax_delay"]=input$int2_vax_delay
    parms_edit["population"]=input$pop
    
    if(sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
             input$demog_5,input$demog_6,input$demog_7,input$demog_8,
             input$demog_9))>=99.5 &
       sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
             input$demog_5,input$demog_6,input$demog_7,input$demog_8,
             input$demog_9))<=100.5){
      
      # Demography
      pop_by_age <- data.frame(age_group = dhaka_pop_by_age$age_group)
      pop_by_age$prop <- c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
                           input$demog_5,input$demog_6,input$demog_7,input$demog_8,
                           input$demog_9)/100
      pop_by_age$pop <- pop_by_age$prop*input$pop
      
      # Make edits
      parms_edit[c("fa","fd","fHosp")] <- calc_fractions(age_dep_pars,pop_by_age)
    }
    return(parms_edit)
  })
  
  
  # Give warning if pop dist proportions don't add up
  output$demog_warning <- renderText({
    test <- sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
                  input$demog_5,input$demog_6,input$demog_7,input$demog_8,
                  input$demog_9))<99.5|
      sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
            input$demog_5,input$demog_6,input$demog_7,input$demog_8,
            input$demog_9))>100.5
    if(test){
      "Warning: percentages describing age distribution do not sum to 100. Please correct before interpreting results."
    }else{
      ""
    }
  })
 
  
  observeEvent(input$days, {

    updateSliderInput(session, inputId = "int_ld_dates",value=c(input$int_ld_dates[1],min(input$days+start_date_forecast,input$int_ld_dates[2])), max=input$days+start_date_forecast, timeFormat = "%d %b %y")
    updateSliderInput(session, inputId = "int_mask_dates",value=c(input$int_mask_dates[1],min(input$days+start_date_forecast,input$int_mask_dates[2])), max=input$days+start_date_forecast, timeFormat = "%d %b %y")
    updateSliderInput(session, inputId = "int_syn_dates",value=c(input$int_syn_dates[1],min(input$days+start_date_forecast,input$int_syn_dates[2])), max=input$days+start_date_forecast, timeFormat = "%d %b %y")
    updateSliderInput(session, inputId = "int_vax_dates",value=c(input$int_vax_dates[1],min(input$days+start_date_forecast,input$int_vax_dates[2])), max=input$days+start_date_forecast, timeFormat = "%d %b %y")
    updateSliderInput(session, inputId = "int_test_dates",value=c(input$int_test_dates[1],min(input$days+start_date_forecast,input$int_test_dates[2])), max=input$days+start_date_forecast, timeFormat = "%d %b %y")

    updateSliderInput(session, inputId = "int2_ld_dates",value=c(input$int2_ld_dates[1],min(input$days+start_date_forecast,input$int2_ld_dates[2])), max=input$days+start_date_forecast, timeFormat = "%d %b %y")
    updateSliderInput(session, inputId = "int2_mask_dates",value=c(input$int2_mask_dates[1],min(input$days+start_date_forecast,input$int2_mask_dates[2])), max=input$days+start_date_forecast, timeFormat = "%d %b %y")
    updateSliderInput(session, inputId = "int2_syn_dates",value=c(input$int2_syn_dates[1],min(input$days+start_date_forecast,input$int2_syn_dates[2])), max=input$days+start_date_forecast, timeFormat = "%d %b %y")
    updateSliderInput(session, inputId = "int2_vax_dates",value=c(input$int2_vax_dates[1],min(input$days+start_date_forecast,input$int2_vax_dates[2])), max=input$days+start_date_forecast, timeFormat = "%d %b %y")
    updateSliderInput(session, inputId = "int2_test_dates",value=c(input$int2_test_dates[1],min(input$days+start_date_forecast,input$int2_test_dates[2])), max=input$days+start_date_forecast, timeFormat = "%d %b %y")

  })

  # Update maximum initial infectious in response to changing upazila population/% immune
  observeEvent(input$pop, {
        updateNumericInput(session, inputId = "initial_infectious",value=min(input$pop*(1-input$initial_immune/100)*0.2,input$initial_infectious), max=input$pop*(1-input$initial_immune/100)*0.2)
  })
  observeEvent(input$initial_immune, {
    updateNumericInput(session, inputId = "initial_infectious",value=min(input$pop*(1-input$initial_immune/100)*0.2,input$initial_infectious), max=input$pop*(1-input$initial_immune/100)*0.2)
  })

  
  times_forecast <- reactive({
    c(as.numeric(start_date_forecast-start_date):as.numeric(start_date_forecast-start_date+input$days))
  })
  

  # Initialise population
  inits <- reactive(initial_conds(parms_baseline_adjust(), initial_immune = input$initial_immune, initial_infectious = input$initial_infectious))

  upa_days <- eventReactive(input$upa_go,{
    upa_days<-input$upa_days
  })
    
  
  # Obtain vaccination vectors
  vax_intervention <- reactive(create_vax(parms_intervention(),times_forecast(),vax_init=dhaka_vax[which(dhaka_vax$date<=start_date_forecast),]))
  vax_intervention2 <- reactive(create_vax(parms_intervention2(),times_forecast(),vax_init=dhaka_vax[which(dhaka_vax$date<=start_date_forecast),]))
  

  # Run model with input parameters
  prevVax <- reactive(c(Vax1=as.numeric(vax_intervention()$Vax1[max(times_initial)+1]),vax_order=as.numeric(input$bl_vax_order),vax_compliance=as.numeric(input$bl_vax_compliance/100)))
  out_baseline <- reactive(amalgamate_cats(as.data.frame(lsoda(y=inits(), times_initial, covid_model, parms=parms_baseline_adjust(),age_dep_pars=age_dep_pars, demog=pop_by_age(), vax1vec=dhaka_vax[which(dhaka_vax$date<=start_date_forecast),"Vax1"],vax2vec=dhaka_vax[which(dhaka_vax$date<=start_date_forecast),"Vax2"])))) # output with no extra intervention
  inits_forecast <- reactive(setNames(as.numeric(out_baseline()[length(times_initial),which(names(out_baseline())%in%names(inits()))]), names(inits())))
  out_intervention <- reactive(amalgamate_cats(as.data.frame(lsoda(y=inits_forecast(), times_forecast(), covid_model, parms=parms_intervention(),age_dep_pars=age_dep_pars, demog=pop_by_age(), vax1vec=vax_intervention()$Vax1,vax2vec=vax_intervention()$Vax2,prevVax=prevVax())))) # output with selected interventions)
  out_intervention2 <- reactive(amalgamate_cats(as.data.frame(lsoda(y=inits_forecast(), times_forecast(), covid_model, parms=parms_intervention2(),age_dep_pars=age_dep_pars, demog=pop_by_age(), vax1vec=vax_intervention2()$Vax1,vax2vec=vax_intervention2()$Vax2,prevVax=prevVax())))) # output with selected interventions)
  out_full <- reactive(rbind(out_baseline(),out_intervention()[2:length(times_forecast()),])) 
  out_full2 <- reactive(rbind(out_baseline(),out_intervention2()[2:length(times_forecast()),])) 
  
  output$out <- renderPrint({
    parms_intervention()
  })
  
  #----- Create plot - vaccinations ----
  output$plot_vax <- renderPlot({
    
    par(mgp=c(2,1,0), mar=c(2,3.5,1.5,4.5),mfrow=c(1,2))
    
    vax_int<-vax_intervention()[times_forecast()+1,c("Vax1","Vax2")]
    vax_int[,"Vax1"]<-vax_int[,"Vax1"]-vax_int[,"Vax2"]
    vax_int2<-vax_intervention2()[times_forecast()+1,c("Vax1","Vax2")]
    vax_int2[,"Vax1"]<-vax_int2[,"Vax1"]-vax_int2[,"Vax2"]
    yRange = c(0, input$pop)#max(vax_int,vax_int2))
    
    barplot(t(as.matrix(vax_int)),beside = F,names.arg = rep("",length(times_forecast())),main="Scenario 1",
            border=c("black","grey45"),col=c("black","grey45"),axes=F,ylim=yRange,ylab="")
    mtext("Count",side=2,line=2.5,cex=1.3) 
    axis(1, at=date_ticks_forecast-min(date_ticks_forecast)+1,labels = date_labels_forecast)
    axis(2,at=pretty(yRange),labels=format(pretty(yRange),scientific=FALSE,big.mark = ','))
    graphics::box(bty="u")
    legend("topleft",border=c("black","grey45"),cex=1.2,
           c("Single vaccinated","Double vaccinated"),
           fill=c("black","grey45"), bty="n")
    par(new=T)
    axis(4,at=pretty(yRange/input$pop)*input$pop,labels=format(pretty(yRange/input$pop)*100,scientific=FALSE,big.mark = ','))
    mtext("% of the population",side=4,line=2.1,cex=1.3) 
    
    barplot(t(as.matrix(vax_int2)),beside = F,names.arg = rep("",length(times_forecast())),main="Scenario 2",
            border=c("red","#FF9999"),col=c("red","#FF9999"),axes=F,ylim=yRange,ylab="")
    mtext("Count",side=2,line=2.5,cex=1.3) 
    axis(1, at=date_ticks_forecast-min(date_ticks_forecast)+1,labels = date_labels_forecast)
    axis(2,at=pretty(yRange),labels=format(pretty(yRange),scientific=FALSE,big.mark = ','))
    graphics::box(bty="u")
    legend("topleft",cex=1.2,
           c("Single vaccinated","Double vaccinated"),
           fill=c("red","#FF9999"),border=c("red","#FF9999"), bty="n")
    par(new=T)
    axis(4,at=pretty(yRange/input$pop)*input$pop,labels=format(pretty(yRange/input$pop)*100,scientific=FALSE,big.mark = ','))
    mtext("% of the population",side=4,line=2.1,cex=1.3) 
    
  })
  



  #----- Create plot - mortality in baseline vs. intervention (RS) -----------
  output$plot_mortality <- renderPlot({

    # # Create subsets of each dataset, then combine
    # intervention_sub <- out_full() %>% select(time, int=D) %>% mutate(int_deaths = diff(c(0,int))) %>% filter(time>=min(times_forecast()))
    # intervention2_sub <- out_full2() %>% select(time, int2=D) %>% mutate(int2_deaths = diff(c(0,int2))) %>% filter(time>=min(times_forecast()))
    # mortality <- merge(intervention_sub, intervention2_sub, by="time")

    par(mgp=c(2,1,0), mar=c(2,3.5,0,3.5))
    
    deaths_intervention<-diff(c(0,out_full()$D))[which(out_full()$time>=min(times_forecast()))]
    deaths_intervention2<-diff(c(0,out_full2()$D))[which(out_full2()$time>=min(times_forecast()))]
    yRange = c(0, max(deaths_intervention,deaths_intervention2))
    plot(deaths_intervention~times_forecast(),
         type="l",lwd=3,axes=F,ylab="",xlab="")
    lines(deaths_intervention2~times_forecast(),
         lwd=3,col="red",lty=3)
    mtext("Daily deaths",side=2,line=2.5,cex=1.3) 
    axis(1, at=date_ticks_forecast,
         labels = date_labels_forecast)
    if(input$show_y_axis==TRUE){axis(2,at=pretty(yRange),labels=format(pretty(yRange),scientific=FALSE,big.mark = ','))}
    graphics::box(bty="l")
    
    legend("topright",
           c("Scenario 1","Scenario 2"),
           col=c(1,2), lty=c(1,3), lwd=3, bty="n")
    
    
    # # Build plot
    # pl1 <- ggplot() +
    #   # geom_col(data=mortality, aes(x = time, y = int_deaths, fill = int_deaths), width=1,show.legend = F) +
    #   geom_line(data=mortality, aes(x = time, y = int_deaths,linetype="Scenario 1", color="#323232"),lwd=1.2) +
    #   geom_line(data=mortality, aes(x = time, y = int2_deaths,linetype="Scenario 2", color="red"),lwd=1.2) +
    #   labs(x = "", y = "Daily deaths") +
    #   scale_y_continuous(limits = c(0, max(mortality$int_deaths,mortality$int2_deaths))) +
    #   scale_x_continuous(limits = c(min(date_ticks_forecast), max(date_ticks_forecast)), breaks=date_ticks_forecast, labels=date_labels_forecast, guide = guide_axis(check.overlap = TRUE)) +
    #   # scale_fill_gradient(low = "#e4e2e2", high = "red", limits = c(0, max(mortality$int_deaths))) +
    #   theme_classic() +
    #   theme(axis.text.y = element_text(size=12),
    #         axis.title.y = element_text(size=14),
    #         axis.text.x = element_text(size=12),
    #         axis.title.x = element_blank(),
    #         legend.position = c(0.7, 0.9),
    #         legend.title = element_blank(),
    #         legend.text = element_text(size=14),
    #         plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
    #   scale_colour_identity()
    # 
    # if(input$show_y_axis==FALSE){
    #   pl1 <- pl1 +
    #     theme(axis.text.y = element_blank(),
    #           axis.ticks.y = element_blank())
    # }
    # 
    # print(pl1)

  })

  #----- Create mortality barplot ----------------------------------------------
  output$barplot_mortality <- renderPlot({

    # Build dataframe
    mortalities_df <- data.frame(x = c("Scenario 1", "Scenario 2"), y = c(max(out_intervention()$D)-min(out_intervention()$D), max(out_intervention2()$D)-min(out_intervention2()$D)))

    # Set as factor
    mortalities_df$x <- factor(mortalities_df$x, levels=c("Scenario 1", "Scenario 2"))

    # Build plots
    mortalities_plot <- triple_barplot_base(mortalities_df, 
                                            y_axis_title="", show_y_axis=input$show_y_axis, col_pal=barplot_pal)
    print(mortalities_plot)

  })


  #----- Create plot - hospitalisations in baseline vs. intervention (RS) ----
  output$plot_hosp <- renderPlot({
    
    par(mgp=c(2,1,0), mar=c(2,3.5,0,3.5))
    
    hosp_intervention<-out_intervention()$Hosp+out_intervention()$ICU
    hosp_intervention2<-out_intervention2()$Hosp+out_intervention()$ICU
    ICU_intervention<-out_intervention()$ICU
    ICU_intervention2<-out_intervention2()$ICU
    yRange = c(0, max(hosp_intervention,hosp_intervention2))
    plot(hosp_intervention~times_forecast(),
         type="l",lwd=3,axes=F,ylab="",xlab="")
    lines(hosp_intervention2~times_forecast(),
          lwd=3,col="red",lty=3)
    lines(ICU_intervention~times_forecast(),
          lwd=3,col="grey55",lty=1)
    lines(ICU_intervention2~times_forecast(),
          lwd=3,col="#FF9999",lty=3)
    mtext("Bed demand",side=2,line=2.5,cex=1.3) 
    axis(1, at=date_ticks_forecast,
         labels = date_labels_forecast)
    if(input$show_y_axis==TRUE){axis(2,at=pretty(yRange),labels=format(pretty(yRange),scientific=FALSE,big.mark = ','))}
    graphics::box(bty="l")
    
    lines(c(0,max(times_forecast())),rep(parms_baseline["beds"], 2),  col="grey45", type = "l", lwd=1,lty=2)
    text(start_date_forecast-start_date+15, parms_baseline["beds"], col="grey45", "bed capacity")
    
    
    legend("topright",
           c("Scenario 1: hospitalised","Scenario 2: hospitalised","Scenario 1: ICU","Scenario 2: ICU"),
           col=c("black","red","grey45","#FF9999"), lty=c(1,3,1,3), lwd=3, bty="n")
    

    # # Create hospital bed demand
    # beds <- out_intervention() %>% select(time)
    # beds$bl <- out_baseline()$Hosp+out_baseline()$ICU # Under baseline
    # beds$int <- out_intervention()$Hosp+out_intervention()$ICU # Under intervention
    # beds$capacity <- parms_baseline["beds"]
    # 
    # pl2 <- ggplot() +
    #     geom_col(data=beds, aes(x = time, y = int, fill = int), width=1, size=1.1, show.legend = F) +
    #     geom_line(data=beds, aes(x = time, y = bl, linetype="Baseline"), color=1) +
    #     geom_line(data=beds, aes(x=time,y=capacity, linetype="Bed capacity")) + # line to show bed capacity
    #     labs(x = "Day", y = "Bed demand") +
    #     scale_x_continuous(limits = c(min(date_ticks), max(date_ticks)),breaks=date_ticks,labels=date_labels,guide = guide_axis(check.overlap = TRUE)) +
    #   scale_y_continuous(limits=c(0,max(beds$bl,beds$int)),breaks=pretty(c(0,max(beds$bl,beds$int))),labels=format(pretty(c(0,max(beds$bl,beds$int))),big.mark=",",scientific = F))+
    #   scale_fill_gradient(low = "#e4e2e2", high = "red", limits = c(min(beds$int,beds$bl),max(beds$int,beds$bl))) +
    #     theme_classic() +
    #     theme(axis.text.y = element_text(size=12),
    #           axis.title.y = element_text(size=14),
    #           axis.text.x = element_text(size=12),
    #           axis.title.x = element_blank(),
    #           legend.position = c(0.76, 0.9),
    #           legend.title = element_blank(),
    #           legend.text = element_text(size=14),
    #           plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
    # 
    # if(input$show_y_axis==FALSE){
    #   pl2 <- pl2 +
    #     theme(axis.text.y = element_blank(),
    #           axis.ticks.y = element_blank())
    # }
    # 
    # print(pl2)

  })

  #----- Create hospitalisation barplot ----------------------------------------
  output$barplot_hosp <- renderPlot({

    # Build dataframes
    hospitalisations_df <- data.frame(x = c("Hospitalised\nScenario 1", "Hospitalised\nScenario 2","ICU\nScenario 1","ICU\nScenario 2"),
                                      cat =c("Hospitalised","ICU","Hospitalised","ICU"),
                                      y = c(sum(c(0,diff(out_intervention()$CumSevere))),
                                            sum(c(0,diff(out_intervention2()$CumSevere))),
                                            sum(c(0,diff(out_intervention()$CumICU))),
                                            sum(c(0,diff(out_intervention2()$CumICU)))))
    # Set as factor
    hospitalisations_df$x <- factor(hospitalisations_df$x, levels=c("Hospitalised\nScenario 1", "Hospitalised\nScenario 2","ICU\nScenario 1","ICU\nScenario 2"))

    # Build plots
    hospitalisations_plot <- triple_barplot_base(hospitalisations_df, 
                                                 y_axis_title="", show_y_axis=input$show_y_axis, col_pal=c("black","red","grey55","#FF9999"))
    print(hospitalisations_plot)
    # Combine plots
    # ggarrange(mortalities_plot, hospitalisations_plot, econlosses_plot,
    #          ncol=3, common.legend = TRUE, legend="bottom")

  })





  #----- Create case detection plot --------------------------------------
  output$case_detection <- renderPlot({
    
    par(mgp=c(2,1,0), mar=c(2,3.5,1.5,0),mfrow=c(1,2))
    

    cases_int<-data.frame("detected"=diff(out_full()$Detected[which(out_full()$time>=(min(times_forecast())-1))]),
                         "symp_undetected"=diff(out_full()$CumSymp[which(out_full()$time>=(min(times_forecast())-1))]) - diff(out_full()$Detected[which(out_full()$time>=(min(times_forecast())-1))]),
                         "asymp"=diff(out_full()$CumAsymp[which(out_full()$time>=(min(times_forecast())-1))]))
    cases_int2<-data.frame("detected"=diff(out_full2()$Detected[which(out_full2()$time>=(min(times_forecast())-1))]),
                         "symp_undetected"=diff(out_full2()$CumSymp[which(out_full2()$time>=(min(times_forecast())-1))]) - diff(out_full2()$Detected[which(out_full2()$time>=(min(times_forecast())-1))]),
                         "asymp"=diff(out_full2()$CumAsymp[which(out_full2()$time>=(min(times_forecast())-1))]))
    
    yRange = c(0, max(rowSums(cases_int),rowSums(cases_int2)))
    
    barplot(t(as.matrix(cases_int)),beside = F,names.arg = rep("",length(times_forecast())),main="Scenario 1",
            border=c("black","grey35","grey65"),col=c("black","grey35","grey65"),axes=F,ylim=yRange,ylab="")
    mtext("Count",side=2,line=2.5,cex=1.3) 
    axis(1, at=date_ticks_forecast-min(date_ticks_forecast)+1,labels = date_labels_forecast)
    if(input$show_y_axis==TRUE){axis(2,at=pretty(yRange),labels=format(pretty(yRange),scientific=FALSE,big.mark = ','))}
    graphics::box(bty="l")
    legend("topright",border=c("black","grey35","grey65"),cex=1.2,
           c("Detected","Symptomatic undetected","Asymptomatic"),
           fill=c("black","grey35","grey65"), bty="n")

    barplot(t(as.matrix(cases_int2)),beside = F,names.arg = rep("",length(times_forecast())),main="Scenario 2",
            border=c("red","#FF6B6B","#FF9C9C"),col=c("red","#FF6B6B","#FF9C9C"),axes=F,ylim=yRange,ylab="")
    mtext("Count",side=2,line=2.5,cex=1.3) 
    axis(1, at=date_ticks_forecast-min(date_ticks_forecast)+1,labels = date_labels_forecast)
    if(input$show_y_axis==TRUE){axis(2,at=pretty(yRange),labels=format(pretty(yRange),scientific=FALSE,big.mark = ','))}
    graphics::box(bty="l")
    legend("topright",cex=1.2,
           c("Detected","Symptomatic undetected","Asymptomatic"),
           fill=c("red","#FF6B6B","#FF9C9C"),border=c("red","#FF6B6B","#FF9C9C"), bty="n")

    

  })

  #----- Create case detection barplot ----------------------------------------------
  output$barplot_testing <- renderPlot({

    # Build dataframes
    test_bar_df <- data.frame(x = c("Scenario 1", "Scenario 2"),
                               y = c((max(out_intervention()$Detected)-min(out_intervention()$Detected))/(max(out_intervention()$CumCases)-min(out_intervention()$CumCases)),
                                     (max(out_intervention2()$Detected)-min(out_intervention2()$Detected))/(max(out_intervention2()$CumCases)-min(out_intervention2()$CumCases))))

    # Set as factor
    test_bar_df$x <- factor(test_bar_df$x, levels=c("Scenario 1", "Scenario 2"))

    # Build plots
    test_barplot <- triple_barplot_base(test_bar_df, 
                                          y_axis_title="", show_y_axis=input$show_y_axis,
                                          col_pal=barplot_pal)
    print(test_barplot)

  })
  
 
  

  
  
  #----- Create epidemiological ts plots ----------------------------------------
  output$epi_ts <- renderPlot({

    par(mgp=c(2,1,0), mar=c(2,3.5,2,0))
    yRange = c(0, max(diff(out_full()$CumCases),diff(out_full2()$CumCases),out_full()$D,out_full2()$D)*1)
    plot(out_full()$time, c(0,diff(out_full()$CumCases)), col="red", type = "l", xlab = "", ylab = "", lwd=2,
         ylim=yRange, axes=F,xlim=c(15,max(times_forecast())))
    mtext("Count",side=2,line=2.5,cex=1.3) 
    axis(1, at=date_ticks,
         labels = date_labels)
    axis(2,at=pretty(yRange),labels=format(pretty(yRange),scientific=FALSE,big.mark = ','))
    graphics::box(bty="l")
    lines(out_full()$time, c(0,diff(out_full()$CumSymp)), col="orange", lwd=2)
    # lines(out_full()$time, c(0,diff(out_full()$CumSevere)), col="darkred", lwd=2)
    lines(out_full()$time, out_full()$D, col=1, type = "l", lwd=2)

    lines(out_full2()$time, c(0,diff(out_full2()$CumCases)), col="red", lwd=2,lty=3)
    lines(out_full2()$time, c(0,diff(out_full2()$CumSymp)), col="orange", lwd=2,lty=3)
    # lines(out_full2()$time, c(0,diff(out_full2()$CumSevere)), col="darkred", lwd=2,lty=3)
    lines(out_full2()$time, out_full2()$D, col=1, lwd=2,lty=3)
    

    # Bangladesh data
    # lines(BGD$date-start_date, BGD$dhaka_cases*10, col="red", type = "l", lwd=1, lty=3)
    lines(BGD$date-start_date, BGD$dhaka_cum_deaths_2021-BGD$dhaka_cum_deaths_2021[which(BGD$date==start_date)], col="black", type = "l", lwd=1, lty=2)
    lines(BGD$date-start_date, BGD$dhaka_cases_2021, col="red", type = "l", lwd=1, lty=2)
    
    # Demarcate initial and forecast periods
    lines(rep(start_date_forecast-start_date, 2), c(0,yRange[2]*1.2), col="grey45", type = "l", lwd=1,lty=2)
    text(start_date_forecast-start_date+5, yRange[2]*0.9, col="grey45", "forecast start",srt=90)

    legend("topleft",
           c("Daily new cases (scenario 1)","Daily new symptomatic (scenario 1)","Total deaths (scenario 1)","Daily new cases (scenario 2)","Daily new symptomatic (scenario 2)","Total deaths (scenario 2)", "Reported daily cases", "Reported total deaths"),
           col=c("red","orange","black","red","orange","black","red","black"),
           lty=c(1,1,1,3,3,3,2,2,2), lwd=c(2,2,2,2,2,2,1,1), bty="n")

  })

  
  output$vax_ts <- renderPlot({
    
    par(mgp=c(2,1,0), mar=c(2,3.5,2,3.5))
    yRange = c(0, max(vax_intervention2()$VaxDosesUsed,vax_intervention()$VaxDosesUsed))
    plot(out_full()$time, vax_intervention()$Vax1-vax_intervention()$Vax2, col="black", type = "l", xlab = "", ylab = "", lwd=2,
         ylim=yRange, axes=F,cex.lab=1.2,xlim=c(15,max(times_forecast())))
    mtext("Count",side=2,line=2.5,cex=1.3) 
    axis(1, at=date_ticks,
         labels = date_labels)
    axis(2,at=pretty(yRange),labels=format(pretty(yRange),scientific=FALSE,big.mark = ','))
    graphics::box(bty="u")

    lines(out_full()$time, vax_intervention()$Vax2, col="red", type = "l", lwd=2, lty=1)
    lines(out_full()$time, vax_intervention()$VaxDosesUsed, col="orange", type = "l", lwd=2, lty=1)
    lines(out_full2()$time, vax_intervention2()$Vax1-vax_intervention2()$Vax2, col=1, type = "l", lwd=2, lty=3)
    lines(out_full2()$time, vax_intervention2()$Vax2, col="red", type = "l", lwd=2, lty=3)
    lines(out_full2()$time, vax_intervention2()$VaxDosesUsed, col="orange", type = "l", lwd=2, lty=3)
        
    # Bangladesh data
    lines(dhaka_vax$date-start_date, dhaka_vax$Vax1-dhaka_vax$Vax2, col="black", type = "l", lwd=2, lty=2)
    lines(dhaka_vax$date-start_date, dhaka_vax$Vax2, col="red", type = "l", lwd=2, lty=2)
    lines(dhaka_vax$date-start_date, dhaka_vax$VaxDosesUsed, col="orange", type = "l", lwd=2, lty=2)
    
    # Demarcate initial and forecast periods
    lines(rep(start_date_forecast-start_date, 2), c(0,yRange[2]*1.2), col="grey45", type = "l", lwd=1,lty=2)
    text(start_date_forecast-start_date+5, yRange[2]*0.9, col="grey45", "forecast start",srt=90)

    legend("topleft",
           c("Single vaccinated (scenario 1)","Double Vaccinated (scenario 1)","Vaccine doses used (scenario 1)","Single vaccinated (scenario 2)","Double Vaccinated (scenario 2)","Vaccine doses used (scenario 2)","Reported single vaccinated", "Reported double vaccinated","Reported vaccine doses used"),
           col=rep(c("black","red","orange"),3),
           lty=c(1,1,1,3,3,3,2,2,2), lwd=c(2,2,2,2,2,2,2,2,2), bty="n")
    
    par(new=T)
    axis(4,at=pretty(yRange/input$pop)*input$pop,labels=format(pretty(yRange/input$pop)*100,scientific=FALSE,big.mark = ','))
    mtext("% of the population",side=4,line=2.5,cex=1.3) 
  })
  

})
