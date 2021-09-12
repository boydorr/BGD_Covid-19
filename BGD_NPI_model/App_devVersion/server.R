


shinyServer(function(input, output, session) {

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
    parms_edit["ld2"]=as.logical(input$bl_ld2) # Is there a second stage of lockdown?
    parms_edit["testing"]=as.logical(input$bl_testing) # Is there lab testing?
    parms_edit["syndromic"]=as.logical(input$bl_syndromic) # Is there syndromic surveillance?
    parms_edit["mask"]=as.logical(input$bl_mask) # Is there mask wearing?
    parms_edit["ld_start"]=as.numeric(input$bl_ld_dates[1] - start_date)
    parms_edit["ld_end"]=as.numeric(input$bl_ld_dates[2] - start_date) # When does the lockdown start and end
    parms_edit["fEW"]=input$bl_fEW*parms_baseline["propWorkers"]/100 # What proportion of people are essential workers?
    parms_edit["fNC"]=1-input$bl_ld_compliance/100 # What proportion of people are non-compliant to lockdown?
    parms_edit["ld_improve"]=input$bl_ld_improve # How many days does it take for the full effect of the lockdown to be reached?
    parms_edit["ld2_start"]=as.numeric(input$bl_ld2_dates[1] - start_date)
    parms_edit["ld2_end"]=as.numeric(input$bl_ld2_dates[2] - start_date) # When does the second lockdown start and end
    parms_edit["fNC2"]=1-input$bl_ld2_compliance/100 # What proportion of people are non-compliant to lockdown?
    parms_edit["fEW2"]=input$bl_fEW2*parms_baseline["propWorkers"]/100 # What proportion of people are essential workers?
    parms_edit["ld2_improve"]=input$bl_ld2_improve # How many days does it take for the full effect of the lockdown to be reached?
    parms_edit["community"]=input$bl_community/100 # capacity of community HWs supporting isolation
    parms_edit["syn_start"]=as.numeric(input$bl_syn_dates[1] - start_date)
    parms_edit["syn_end"]=as.numeric(input$bl_syn_dates[2] - start_date) # When does the syndromic surveillance start and end
    parms_edit["syn_improve"]=input$bl_syn_improve # How many days does it take for the full effect of the syndromic surveillance to be reached?
    parms_edit["test_start"]=as.numeric(input$bl_test_dates[1] - start_date)
    parms_edit["test_end"]=as.numeric(input$bl_test_dates[2] - start_date) # When does the lab testing start and end
    parms_edit["test_capacity"]=input$bl_test_capacity # lab testing capacity
    parms_edit["test_compliance"]=input$bl_test_compliance/100 # lab testing capacity
    parms_edit["test_fneg"]=input$bl_test_fneg # false negative probability for lab test
    parms_edit["mask_start"]=as.numeric(input$bl_mask_dates[1] - start_date)
    parms_edit["mask_end"]=as.numeric(input$bl_mask_dates[2] - start_date) # When does mask wearing start and end
    parms_edit["mask_effect_outward"]=input$bl_mask_effect_out # By what proportion does mask wearing reduce transmission to others
    parms_edit["f_mask_effect_inward"]=input$bl_mask_effect_in/input$bl_mask_effect_out # What proportion of mask_effect_outward is the impact of masks in protecting the wearer from infection
    parms_edit["mask_compliance"]=input$bl_mask_compliance/100 # What proportion of people are compliant to mask wearing?
    parms_edit["mask_improve"]=input$bl_mask_improve # How many days does it take for the full effect of mask wearing to be reached?
    parms_edit["vax"]=as.logical(input$bl_vax)
    parms_edit["vax_2_doses"]=as.logical(input$bl_vax_2_doses)
    parms_edit["vax_order"]=as.numeric(input$bl_vax_order)
    parms_edit["vax_compliance"]=input$bl_vax_compliance/100
    parms_edit["vax_transmission_effect_dose1"]=input$bl_vax_transmission_effect_dose1/100
    parms_edit["vax_transmission_effect_dose2"]=input$bl_vax_transmission_effect_dose2/100
    parms_edit["vax_severity_effect_dose1"]=input$bl_vax_severity_effect_dose1/100
    parms_edit["vax_severity_effect_dose2"]=input$bl_vax_severity_effect_dose2 /100
    parms_edit["t_between_doses"]=input$bl_t_between_doses
    parms_edit["vax_rate"]=input$bl_vax_rate
    parms_edit["maxVax"]=input$bl_maxVax/100
    parms_edit["vax_start"]=as.numeric(input$bl_vax_dates[1] - start_date)
    parms_edit["vax_end"]=as.numeric(input$bl_vax_dates[2] - start_date)
    parms_edit["vax_delay"]=input$bl_vax_delay
    
    return(parms_edit)

  })

  
  # Change minimum and value of second lockdown date if dates of first lockdown change
  observeEvent(input$int_ld_dates, {
    updateSliderInput(session, inputId = "int_ld2_dates",value=c(max(input$int_ld_dates[2],input$int_ld2_dates[1]),max(input$int_ld_dates[2],input$int_ld2_dates[2])), min=input$int_ld_dates[2], timeFormat = "%d %b %y")
  })
  observeEvent(input$bl_ld_dates, {
    updateSliderInput(session, inputId = "bl_ld2_dates",value=c(max(input$bl_ld_dates[2],input$bl_ld2_dates[1]),max(input$bl_ld_dates[2],input$bl_ld2_dates[2])), min=input$bl_ld_dates[2], timeFormat = "%d %b %y")
  })

  # Update max and value of inward max effect if changing outward effect
  observeEvent(input$int_mask_effect_out, {
    updateSliderInput(session, inputId = "int_mask_effect_in",value=min(input$int_mask_effect_in,input$int_mask_effect_out), max=input$int_mask_effect_out)
  })
  observeEvent(input$bl_mask_effect_out, {
    updateSliderInput(session, inputId = "bl_mask_effect_in",value=min(input$bl_mask_effect_in,input$bl_mask_effect_out), max=input$bl_mask_effect_out)
  })
  
  
  # Reset intervention inputs if user clicks to edit baseline params
  observeEvent(input$up_int_adj_bl, {


    # Show a notification
    showNotification(HTML("<b>Comparison values will be adjusted to the new baseline values</b>"), type="message")

    # Lockdown inputs
    updateRadioButtons(session, inputId = "int_ld", selected=as.logical(input$bl_ld))
    updateRadioButtons(session, inputId = "int_ld2", selected=as.logical(input$bl_ld2))
    updateSliderInput(session, inputId = "int_ld_dates", value = c(input$bl_ld_dates[1], input$bl_ld_dates[2]), timeFormat = "%d %b %y")
    updateNumericInput(session, inputId = "int_ld_improve", value = input$bl_ld_improve)
    updateSliderInput(session, inputId = "int_fEW", value = input$bl_fEW)
    updateSliderInput(session, inputId = "int_ld_compliance", value = input$bl_ld_compliance)
    updateSliderInput(session, inputId = "int_ld2_dates", value = c(input$bl_ld2_dates[1], input$bl_ld2_dates[2]), timeFormat = "%d %b %y")
    updateNumericInput(session, inputId = "int_ld2_improve", value = input$bl_ld2_improve)
    updateSliderInput(session, inputId = "int_fEW2", value = input$bl_fEW2)
    updateSliderInput(session, inputId = "int_ld2_compliance", value = input$bl_ld2_compliance)
    # Testing inputs
    updateRadioButtons(session, inputId = "int_testing", selected=as.logical(input$bl_testing))
    updateSliderInput(session, inputId = "int_test_dates", value = c(input$bl_test_dates[1], input$bl_test_dates[2]), timeFormat = "%d %b %y")
    updateSliderInput(session, inputId = "int_test_compliance", value = input$bl_test_compliance)
    updateSliderInput(session, inputId = "int_test_capacity", value = input$bl_test_capacity)
    updateSliderInput(session, inputId = "int_test_fneg", value = input$bl_test_fneg)
    # Community input
    updateRadioButtons(session, inputId = "int_syndromic", selected=as.logical(input$bl_syndromic))
    updateSliderInput(session, inputId = "int_syn_dates", value = c(input$bl_syn_dates[1], input$bl_syn_dates[2]))
    updateNumericInput(session, inputId = "int_syn_improve", value = input$bl_syn_improve)
    updateSliderInput(session, inputId = "int_community", value = input$bl_community)
    # Masks input
    updateRadioButtons(session, inputId = "int_mask", selected=as.logical(input$bl_mask))
    updateSliderInput(session, inputId = "int_mask_dates", value = c(input$bl_mask_dates[1], input$bl_mask_dates[2]), timeFormat = "%d %b %y")
    updateNumericInput(session, inputId = "int_mask_improve", value = input$bl_mask_improve)
    updateSliderInput(session, inputId = "int_mask_effect_out", value = input$bl_mask_effect_out)
    updateSliderInput(session, inputId = "int_mask_effect_in", value = input$bl_mask_effect_in/input$bl_mask_effect_out)
    # # Vaccination inputs
    updateRadioButtons(session, inputId = "int_vax", selected=as.logical(input$bl_vax))
    updateRadioButtons(session, inputId = "int_vax_2_doses", selected=as.logical(input$bl_vax_2_doses))
    updateRadioButtons(session, inputId = "int_vax_order", selected=input$bl_vax_order)
    updateSliderInput(session, inputId = "int_vax_dates", value = c(input$bl_vax_dates[1], input$bl_vax_dates[2]), timeFormat = "%d %b %y")
    updateSliderInput(session, inputId = "int_vax_compliance", value = input$bl_vax_compliance)
    updateSliderInput(session, inputId = "int_vax_transmission_effect_dose1", value = input$bl_vax_transmission_effect_dose1)
    updateSliderInput(session, inputId = "int_vax_transmission_effect_dose2", value = input$bl_vax_transmission_effect_dose2)
    updateSliderInput(session, inputId = "int_vax_severity_effect_dose1", value = input$bl_vax_severity_effect_dose1)
    updateSliderInput(session, inputId = "int_vax_severity_effect_dose2", value = input$bl_vax_severity_effect_dose2)
    updateSliderInput(session, inputId = "int_maxVax", value = input$bl_maxVax)
    updateNumericInput(session, inputId = "int_t_between_doses", value = input$bl_t_between_doses)
    updateNumericInput(session, inputId = "int_vax_rate", value = input$bl_vax_rate)
    updateNumericInput(session, inputId = "int_vax_delay", value = input$bl_vax_delay)
    
    
  })


  # Parameters for scenario to be compared with baseline
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
    parms_edit["ld2"]=as.logical(input$int_ld2) # Is there a second lockdown stage?
    parms_edit["testing"]=as.logical(input$int_testing) # Is there testing?
    parms_edit["syndromic"]=as.logical(input$int_syndromic) # Is there syndromic surveillance?
    parms_edit["mask"]=as.logical(input$int_mask) # Is there mask wearing?
    parms_edit["ld_start"]=as.numeric(input$int_ld_dates[1] - start_date)
    parms_edit["ld_end"]=as.numeric(input$int_ld_dates[2] - start_date) # When does the lockdown start and end
    parms_edit["fEW"]=input$int_fEW*parms_baseline["propWorkers"]/100 # What proportion of people are essential workers?
    parms_edit["fNC"]=1-input$int_ld_compliance/100 # What proportion of people are non-compliant to lockdown?
    parms_edit["ld_improve"]=input$int_ld_improve # How many days does it take for the full effect of the lockdown to be reached?
    parms_edit["ld2_start"]=as.numeric(input$int_ld2_dates[1] - start_date)
    parms_edit["ld2_end"]=as.numeric(input$int_ld2_dates[2] - start_date) # When does the second lockdown start and end
    parms_edit["fNC2"]=1-input$int_ld2_compliance/100 # What proportion of people are non-compliant to lockdown?
    parms_edit["fEW2"]=input$int_fEW2*parms_baseline["propWorkers"]/100 # What proportion of people are essential workers?
    parms_edit["ld2_improve"]=input$int_ld2_improve # How many days does it take for the full effect of the lockdown to be reached?
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
    
    return(parms_edit)
  })
  
  
  # # Give warning if pop dist proportions don't add up
  # observeEvent(input$go, {
  #   if(sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
  #            input$demog_5,input$demog_6,input$demog_7,input$demog_8,
  #            input$demog_9))<99.5|
  #      sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
  #            input$demog_5,input$demog_6,input$demog_7,input$demog_8,
  #            input$demog_9))>100.5){
  #     output$demog_warning <-renderText("Error: percentages describing age distribution must sum to 100")
  #   }else{
  #     output$demog_warning <-renderText("")
  #   }
  #  
  # })
  
  # # Update maximum days on date sliders in response to changing upazila forecast period
  # observeEvent(input$upa_days, {
  #  
  #   updateSliderInput(session, inputId = "upa_ld_dates",value=c(min(input$upa_days+start_date_upa,input$upa_ld_dates[1]),min(input$upa_days+start_date_upa,input$upa_ld_dates[2])), max=input$upa_days+start_date_upa, timeFormat = "%d %b %y")
  #   updateSliderInput(session, inputId = "upa_ld2_dates",value=c(min(input$upa_days+start_date_upa,input$upa_ld2_dates[1]),min(input$upa_days+start_date_upa,input$upa_ld2_dates[2])), max=input$upa_days+start_date_upa, timeFormat = "%d %b %y")
  #   updateSliderInput(session, inputId = "upa_mask_dates",value=c(min(input$upa_days+start_date_upa,input$upa_mask_dates[1]),min(input$upa_days+start_date_upa,input$upa_mask_dates[2])), max=input$upa_days+start_date_upa, timeFormat = "%d %b %y")
  #   updateSliderInput(session, inputId = "upa_syn_dates",value=c(min(input$upa_days+start_date_upa,input$upa_syn_dates[1]),min(input$upa_days+start_date_upa,input$upa_syn_dates[2])), max=input$upa_days+start_date_upa, timeFormat = "%d %b %y")
  #   
  # })
  # 
  # # Update maximum initial infectious in response to changing upazila population/% immune
  # observeEvent(input$pop, {
  #       updateNumericInput(session, inputId = "initial_infectious",value=min(input$pop*(1-input$initial_immune/100)*0.2,input$initial_infectious), max=input$pop*(1-input$initial_immune/100)*0.2)
  # })
  # observeEvent(input$initial_immune, {
  #   updateNumericInput(session, inputId = "initial_infectious",value=min(input$pop*(1-input$initial_immune/100)*0.2,input$initial_infectious), max=input$pop*(1-input$initial_immune/100)*0.2)
  # })
  # 
  
  # parms_upazila <- eventReactive(input$go,{
  #   
  #   # Update population
  #   parms_edit["population"]=input$pop
  # 
  # 
  #   if(sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
  #            input$demog_5,input$demog_6,input$demog_7,input$demog_8,
  #            input$demog_9))>99.5 &
  #      sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
  #            input$demog_5,input$demog_6,input$demog_7,input$demog_8,
  #            input$demog_9))<100.5){
  # 
  #     # Upazila demography
  #     upazila_pop_by_age <- data.frame(age_group = dhaka_pop_by_age$age_group)
  #     upazila_pop_by_age$prop <- c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
  #                                  input$demog_5,input$demog_6,input$demog_7,input$demog_8,
  #                                  input$demog_9)/100
  #     upazila_pop_by_age$pop <- upazila_pop_by_age$prop*input$pop
  # 
  #     # Make edits
  #     parms_edit[c("fa","fd","fHosp")] <- calc_fractions(age_dep_pars,upazila_pop_by_age)
  # 
  # 
  #   }
  #   
  #   return(parms_edit)
  # })
  # pop_by_age <- eventReactive(input$go,{
  #   
  #   if(sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
  #            input$demog_5,input$demog_6,input$demog_7,input$demog_8,
  #            input$demog_9))>99.5 &
  #      sum(c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
  #            input$demog_5,input$demog_6,input$demog_7,input$demog_8,
  #            input$demog_9))<100.5){
  #     
  #     # Upazila demography
  #     pop_by_age <- data.frame(age_group = dhaka_pop_by_age$age_group)
  #     pop_by_age$prop <- c(input$demog_1,input$demog_2,input$demog_3,input$demog_4,
  #                                  input$demog_5,input$demog_6,input$demog_7,input$demog_8,
  #                                  input$demog_9)/100
  #     pop_by_age$pop <- pop_by_age$prop*input$pop
  #     
  #   }
  #   
  #   return(pop_by_age)
  # })
  # observeEvent(input$pop_reset,{
  #   
  #   # Return ui inputs to defaults
  #   updateNumericInput(session, inputId = "pop", value = as.numeric(parms_baseline["population"]))
  #   updateNumericInput(session, inputId = "demog_1", value = round(dhaka_pop_by_age$prop[1]*100,1))
  #   updateNumericInput(session, inputId = "demog_2", value = round(dhaka_pop_by_age$prop[2]*100,1))
  #   updateNumericInput(session, inputId = "demog_3", value = round(dhaka_pop_by_age$prop[3]*100,1))
  #   updateNumericInput(session, inputId = "demog_4", value = round(dhaka_pop_by_age$prop[4]*100,1))
  #   updateNumericInput(session, inputId = "demog_5", value = round(dhaka_pop_by_age$prop[5]*100,1))
  #   updateNumericInput(session, inputId = "demog_6", value = round(dhaka_pop_by_age$prop[6]*100,1))
  #   updateNumericInput(session, inputId = "demog_7", value = round(dhaka_pop_by_age$prop[7]*100,1))
  #   updateNumericInput(session, inputId = "demog_8", value = round(dhaka_pop_by_age$prop[8]*100,1))
  #   updateNumericInput(session, inputId = "demog_9", value = round(dhaka_pop_by_age$prop[9]*100,1))
  #   updateSliderInput(session, inputId = "initial_immune", value = 25)
  #   updateNumericInput(session, inputId = "initial_infectious", value = 11451)
  #   updateNumericInput(session, inputId = "days", value = 120)
  # 
  # })
  # 
  # # Initialise population
  # inits <- eventReactive(input$go,{
  #   
  #   inits <- y
  #   
  #   inits["Ia_f"] <- input$initial_infectious*parms_baseline["fa"] # assume infectious individuals are the only infectious individual in their households and are at the beginning of their infectious period
  #   inits["Ip_f"] <- input$initial_infectious*(1-parms_baseline["fa"])
  #   inits["E_f"]<- input$initial_infectious*0.5# Assume same number incubating as infectious and evenly distribute among households with and without infecteds (assume no more than one per houshold)
  #   inits["E_sa"]<- input$initial_infectious*0.5*parms_baseline["fa"]
  #   inits["E_ss"]<- input$initial_infectious*0.5*(1-parms_baseline["fa"])
  #   inits["R_n"] <- parms_baseline["population"]*(input$initial_immune/100)*(1-((input$initial_infectious+input$initial_infectious*0.5)/(parms_baseline["population"]/parms_baseline["HHsize"])))
  #   inits["R_I"] <- parms_baseline["population"]*(input$initial_immune/100)*(input$initial_infectious/(parms_baseline["population"]/parms_baseline["HHsize"]))
  #   inits["R_E"] <- parms_baseline["population"]*(input$initial_immune/100)*((input$initial_infectious*0.5)/(parms_baseline["population"]/parms_baseline["HHsize"]))
  #   inits["S_I"] <- (parms_baseline["HHsize"]-1)*(input$initial_infectious*0.5) 
  #   inits["S_E"] <- (parms_baseline["HHsize"]-1)*(input$initial_infectious*0.5) 
  #   inits["S_n"] <- as.numeric(parms_baseline["population"] - 
  #                            (inits["R_n"]+inits["R_I"]+inits["R_E"]+inits["Ia_f"]+inits["Ip_f"]+inits["E_f"]+
  #                               inits["E_sa"]+inits["E_ss"]+inits["S_I"]+inits["S_E"]))
  #   inits["CumCases"] <- input$initial_infectious
  # 
  #   return(inits)
  # })
  # 
  # upa_days <- eventReactive(input$upa_go,{
  #   upa_days<-input$upa_days
  # })
    
  
  # Obtain vaccination vectors
  vax_baseline <- reactive(create_vax(parms_baseline_adjust(),times))
  vax_intervention <- reactive(create_vax(parms_intervention(),times))

  # Run model with input parameters
  out_baseline <- reactive(amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms_baseline_adjust(),age_dep_pars=age_dep_pars, demog=dhaka_pop_by_age, vax1vec=vax_baseline()$Vax1,vax2vec=vax_baseline()$Vax2))))) # output with no extra intervention
  out_intervention <- reactive(amalgamate_cats(rbind(preIntro,as.data.frame(lsoda(y, times_model, covid_model, parms=parms_intervention(),age_dep_pars=age_dep_pars, demog=dhaka_pop_by_age, vax1vec=vax_intervention()$Vax1,vax2vec=vax_intervention()$Vax2))))) # output with selected interventions

  output$out <- renderPrint(out_baseline()$TestsUsed)
  


  #----- Create plot 1 - mortality in baseline vs. intervention (RS) -----------
  output$plot1 <- renderPlot({

    # Create subsets of each dataset, then combine
    baseline_sub <- out_baseline() %>% select(time, bl=D)
    intervention_sub <- out_intervention() %>% select(time, int=D)
    rc_mortality <- merge(baseline_sub, intervention_sub, by="time")

    # Calculate relative change
    rc_mortality$bl_rel_change <- rel_mortality(72.05, parms_baseline["population"], rc_mortality$bl) # Under baseline
    rc_mortality$int_rel_change <- rel_mortality(72.05, parms_baseline["population"], rc_mortality$int) # Under intervention

    # Build plot
    pl1 <- ggplot() +
      geom_col(data=rc_mortality, aes(x = time, y = int_rel_change, fill = int_rel_change), width=1,show.legend = F) +
      geom_line(data=rc_mortality, aes(x = time, y = bl_rel_change,linetype="Baseline"), color="#323232") +
      labs(x = "", y = "% increase in daily deaths") +
      scale_y_continuous(limits = c(0, max(rc_mortality$bl_rel_change,rc_mortality$int_rel_change))) +
      scale_x_continuous(limits = c(min(date_ticks), max(date_ticks)), breaks=date_ticks, labels=date_labels, guide = guide_axis(check.overlap = TRUE)) +
      scale_fill_gradient(low = "#e4e2e2", high = "red", limits = c(0, max(rc_mortality$int_rel_change))) +
      theme_classic() +
      theme(axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=14),
            axis.text.x = element_text(size=12),
            axis.title.x = element_blank(),
            legend.position = c(0.7, 0.9),
            legend.title = element_blank(),
            legend.text = element_text(size=14),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

    if(input$show_y_axis==FALSE){
      pl1 <- pl1 +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
    }

    print(pl1)

  })

  #----- Create mortality barplot ----------------------------------------------
  output$barplot_mortality <- renderPlot({

    # Build dataframe
    mortalities_df <- data.frame(x = c("Baseline", "Comparison"), y = c(max(out_baseline()$D), max(out_intervention()$D)))

    # Set as factor
    mortalities_df$x <- factor(mortalities_df$x, levels=c("Baseline", "Comparison"))

    # Build plots
    mortalities_plot <- triple_barplot_base(mortalities_df, 
                                            y_axis_title="", show_y_axis=input$show_y_axis, col_pal=barplot_pal)
    print(mortalities_plot)

  })


  #----- Create plot 2 - hospitalisations in baseline vs. intervention (RS) ----
  output$plot2 <- renderPlot({

    # Create hospital bed demand
    beds <- out_intervention() %>% select(time)
    beds$bl <- out_baseline()$Hosp+out_baseline()$ICU # Under baseline
    beds$int <- out_intervention()$Hosp+out_intervention()$ICU # Under intervention
    beds$capacity <- parms_baseline["beds"]

    pl2 <- ggplot() +
        geom_col(data=beds, aes(x = time, y = int, fill = int), width=1, size=1.1, show.legend = F) +
        geom_line(data=beds, aes(x = time, y = bl, linetype="Baseline"), color=1) +
        geom_line(data=beds, aes(x=time,y=capacity, linetype="Bed capacity")) + # line to show bed capacity
        labs(x = "Day", y = "Bed demand") +
        scale_x_continuous(limits = c(min(date_ticks), max(date_ticks)),breaks=date_ticks,labels=date_labels,guide = guide_axis(check.overlap = TRUE)) +
      scale_y_continuous(limits=c(0,max(beds$bl,beds$int)),breaks=pretty(c(0,max(beds$bl,beds$int))),labels=format(pretty(c(0,max(beds$bl,beds$int))),big.mark=",",scientific = F))+
      scale_fill_gradient(low = "#e4e2e2", high = "red", limits = c(min(beds$int,beds$bl),max(beds$int,beds$bl))) +
        theme_classic() +
        theme(axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=14),
              axis.text.x = element_text(size=12),
              axis.title.x = element_blank(),
              legend.position = c(0.76, 0.9),
              legend.title = element_blank(),
              legend.text = element_text(size=14),
              plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

    if(input$show_y_axis==FALSE){
      pl2 <- pl2 +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
    }

    print(pl2)

  })

  #----- Create hospitalisation barplot ----------------------------------------
  output$barplot_hosp <- renderPlot({

    # Build dataframes
    hospitalisations_df <- data.frame(x = c("Baseline", "Comparison"),
                                      y = c(sum(c(0,diff(out_baseline()$CumSevere))),
                                            sum(c(0,diff(out_intervention()$CumSevere)))))
    # Set as factor
    hospitalisations_df$x <- factor(hospitalisations_df$x, levels=c("Baseline", "Comparison"))

    # Build plots
    hospitalisations_plot <- triple_barplot_base(hospitalisations_df, 
                                                 y_axis_title="", show_y_axis=input$show_y_axis, col_pal=barplot_pal)
    print(hospitalisations_plot)
    # Combine plots
    # ggarrange(mortalities_plot, hospitalisations_plot, econlosses_plot,
    #          ncol=3, common.legend = TRUE, legend="bottom")

  })





  #----- Create case detection plot --------------------------------------
  output$case_detection <- renderPlot({
    cd_data <- data.frame("time"=times,
                          "value"=c(out_intervention()[,c("CumCases")]-out_intervention()$Detected,
                                    out_intervention()$Detected),
                          "group"=rep(c("Undetected","Detected"),each=length(times)))
    cd_data <- mutate(cd_data,group = fct_relevel(group,"Undetected", "Detected"))

    cd_df <- out_intervention() %>% select(time)
    cd_df$new <- out_intervention()[,c("CumCases")]-out_intervention()$Detected
    cd_df$test <- out_intervention()$Detected


    pl4 <- ggplot() +
        geom_col(data=cd_data, aes(x = time, y = value, fill=group), width=1) +
        scale_fill_manual(values=c("red", "#302f2f"), breaks=c("Undetected", "Detected")) + # "#302f2f"
        #geom_col(data=cd_df, aes(x = time, y = rdt, fill = "+ve RDTs"), width=1, size=1.1, show.legend = T, color="#323232") +
        #geom_col(data=cd_df, aes(x = time, y = pcr, fill = "+ve PCR"), width=1, size=1.1, show.legend = T, color="black") +

        labs(x = "Day", y = "Cumulative cases") +
        scale_x_continuous(limits = c(min(date_ticks), max(date_ticks)),breaks=date_ticks,labels=date_labels,guide = guide_axis(check.overlap = TRUE)) +
        #   scale_fill_gradient(low = "#e4e2e2", high = "red", limits = c(min(beds$int,beds$bl),max(beds$int,beds$bl))) +
        theme_classic() +
        theme(axis.title.y = element_text(size=14),
              axis.text.y = element_text(size=12),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size=12),
              legend.position = c(0.15, 0.9),
              legend.title = element_blank(),
              legend.text = element_text(size=14),
              plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

    if(input$show_y_axis==FALSE){
      pl4 <- pl4 +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
    }

    print(pl4)


  })

  #----- Create case detection barplot ----------------------------------------------
  output$barplot_testing <- renderPlot({

    # Build dataframes
    test_bar_df <- data.frame(x = c("Baseline", "Comparison"),
                               y = c(max(out_baseline()$Detected)/max(out_baseline()$CumCases),
                                     max(out_intervention()$Detected)/max(out_intervention()$CumCases)))

    # Set as factor
    test_bar_df$x <- factor(test_bar_df$x, levels=c("Baseline", "Comparison"))

    # Build plots
    test_barplot <- triple_barplot_base(test_bar_df, 
                                          y_axis_title="", show_y_axis=input$show_y_axis,
                                          col_pal=barplot_pal)
    print(test_barplot)

  })
  
 
  

  
  
  #----- Create epidemiological ts plots ----------------------------------------
  output$epi_ts <- renderPlot({

    par(mgp=c(2,1,0), mar=c(2,3,2,0))
    yRange = c(0, max(c(0,diff(out_baseline()$CumCases)),out_baseline()$D*1.2))
    xRange = c(0, max(times))
    plot(out_baseline()$time, c(0,diff(out_baseline()$CumCases)), col="red", type = "l", xlab = "Days", ylab = "Count", lwd=2,
         ylim=yRange, axes=F)
    axis(1, at=date_ticks,
         labels = date_labels)
    axis(2,at=pretty(yRange),labels=format(pretty(yRange),scientific=FALSE,big.mark = ','))
    graphics::box(bty="l")
    lines(out_baseline()$time, c(0,diff(out_baseline()$CumSymp)), col="orange", lwd=2)
    lines(out_baseline()$time, c(0,diff(out_baseline()$CumSevere)), col="darkred", lwd=2)
    lines(out_baseline()$time, out_baseline()$D, col=1, type = "l", lwd=2)
    

    # Baseline cumulative mortality
    lines(rep(max(out_baseline()$D), nrow(out_baseline())), col="#a39999", type = "l", lwd=1, lty=2)
    text(60, max(out_baseline()$D)*1.25, col="#a39999", "Mortality")

    # Bangladesh data
    # lines(BGD$date-start_date, BGD$dhaka_cases*10, col="red", type = "l", lwd=1, lty=3)
    lines(BGD$date-start_date, BGD$dhaka_cum_deaths, col="black", type = "l", lwd=1, lty=2)
    
    # First detection
    lines(rep(BGD$date[1]-start_date, 100), (0:99)*yRange[2]/100, col="#a39999", type = "l", lwd=1) # First detected case
    text(BGD$date[1]-start_date-10, yRange[2]/3,srt=90, col="#a39999", "First detection") # First detected case

    # # Today
    # lines(rep(Sys.Date()-start_date, 100), (0:99)*yRange[2]/100, col="black", type = "l", lwd=1)
    # text(Sys.Date()-start_date-25, yRange[2]/4, col="black", "Today")

  })

  
  output$death_ts <- renderPlot({
    
    par(mgp=c(2,1,0), mar=c(2,3,2,0))
    yRange = c(0, max(c(0,out_baseline()$D)))
    xRange = c(0, max(times))
    plot(out_baseline()$time, out_baseline()$D, col="black", type = "l", xlab = "Days", ylab = "Count", lwd=2,
         ylim=yRange, axes=F)
    axis(1, at=date_ticks,
         labels = date_labels)
    axis(2,at=pretty(yRange),labels=format(pretty(yRange),scientific=FALSE,big.mark = ','))
    graphics::box(bty="l")
    
    # Bangladesh data
    lines(BGD$date-start_date, BGD$dhaka_cum_deaths, col="black", type = "l", lwd=1, lty=2)
    
  })
  
  

  #----- Create early epidemiological ts plots -----------------------------
  output$epi_ts_early <- renderPlot({

    par(mgp=c(2,1,0), mar=c(2,3,2,0))
    yRange = c(0, max(c(0,diff(out_baseline()$CumSymp))[1:120]))
    xRange = c(60, 152)
    plot(BGD$date-start_date, BGD$dhaka_cases, col="red", type = "l", lwd=1, lty=2,
         ylim=yRange, xlim=xRange, axes=F, ylab="Count")
    axis(1, at=as.numeric(start_date %m+% months(0:15) - start_date),
         labels = paste(month.abb[month(start_date %m+% months(0:15))], year(start_date %m+% months(0:15))))

    lines(out_baseline()$time, c(0,diff(out_baseline()$CumCases)), col="red", lwd=2)
    lines(out_baseline()$time, c(0,diff(out_baseline()$CumSymp)), col="orange", lwd=2)
    lines(out_baseline()$time, c(0,diff(out_baseline()$CumSevere)), col="darkred", lwd=2)
    lines(out_baseline()$time, out_baseline()$D, col=1, type = "l", lwd=2)

    axis(2, at=axTicks(2),
         labels=formatC(axTicks(2), format="d", big.mark=','))
    graphics::box(bty="l")

    # Dhaka data
    # lines(BGD$date-start_date, BGD$dhaka_cases*10, col="red", type = "l", lwd=1, lty=3)
    lines(BGD$date-start_date, BGD$dhaka_cum_deaths, col="black", type = "l", lwd=1, lty=2)

    # First detection
    lines(rep(BGD$date[1]-start_date, 100), (0:99)*yRange[2]/100, col="#a39999", type = "l", lwd=1) # First detected case
    text(BGD$date[1]-start_date+6, yRange[2]/4, col="#a39999", "First \n detection") # First detected case

    # # Today
    # lines(rep(Sys.Date()-start_date, 100), (0:99)*yRange[2]/100, col="black", type = "l", lwd=1)
    # text(Sys.Date()-start_date-5, yRange[2]/4, col="black", "Today")

    legend("topleft",
           c("Daily cases","Symptomatic","Severe","Total fatal", "Reported daily cases", "Reported total deaths"),
           col=c("red","orange","darkred","black","red","black"),
           lty=c(1,1,1,1,2,2), lwd=c(2,2,2,2,1,1), bty="n")

  })
  
  
  output$death_ts_early <- renderPlot({
    
    par(mgp=c(2,1,0), mar=c(2,3,2,0))
    yRange = c(0, max(out_baseline()$D[1:152]))
    xRange = c(60, 152)
    plot(BGD$date-start_date, BGD$dhaka_cum_deaths, col="black", type = "l", lwd=1, lty=2,
         ylim=yRange, xlim=xRange, axes=F, ylab="Count")
    axis(1, at=as.numeric(start_date %m+% months(0:15) - start_date),
         labels = paste(month.abb[month(start_date %m+% months(0:15))], year(start_date %m+% months(0:15))))
    
    lines(out_baseline()$time, out_baseline()$D, col=1, type = "l", lwd=2)
    
    axis(2, at=axTicks(2),
         labels=formatC(axTicks(2), format="d", big.mark=','))
    graphics::box(bty="l")
    
    
    legend("topleft",
           c("Total fatal", "Reported total deaths"),
           col="black", lty=c(1,2), lwd=c(2,1), bty="n")
    
  })
  

  
  # #----- Create upazila epidemiological ts plot ----------------------------------------
  # output$epi_ts_upazila <- renderPlot({
  #   
  #   par(mgp=c(2,1,0), mar=c(3,3,2,0))
  #   yRange = c(0, max(diff(c(0,out_upazila()$CumSymp)),out_upazila()$D*1.2))
  #   xRange_date <- c(start_date_upa+15, start_date_upa+upa_days())
  #   xRange_num <- c(15,upa_days())
  #   plot(out_upazila()$time, diff(c(0,out_upazila()$CumSymp)), col="orange", type = "l", xlab = "", ylab = "Count", lwd=2,
  #        ylim=yRange, xlim=xRange_num, axes=F,cex.lab=1.3)
  #   axis(1, at=start_date_upa %m+% months(0:(month(xRange_date[2])-month(start_date_upa))) - start_date_upa,
  #        labels = paste(month.abb[month(start_date_upa %m+% months(0:(month(xRange_date[2])-month(start_date_upa))))], year(start_date_upa %m+% months(0:(month(xRange_date[2])-month(start_date_upa))))))
  #   axis(2,at=pretty(yRange),labels=format(pretty(yRange),scientific=FALSE,big.mark = ','))
  #   graphics::box(bty="l")
  #   # lines(out_upazila()$time, out_upazila()$CumCases, col="cyan", lwd=3)
  #   lines(out_upazila()$time, diff(c(0,out_upazila()$CumSevere)), col="darkred", lwd=2)
  #   lines(out_upazila()$time, out_upazila()$D, col=1, type = "l", lwd=2)
  #   
  #   lines(BGD$date[which(BGD$date==start_date_upa):nrow(BGD)]-start_date_upa, BGD$dhaka_cum_deaths_2021[which(BGD$date==start_date_upa):nrow(BGD)]-BGD$dhaka_cum_deaths_2021[which(BGD$date==start_date_upa)], col="black", type = "l", lwd=1, lty=2)
  #   lines(BGD$date[which(BGD$date==start_date_upa):nrow(BGD)]-start_date_upa, BGD$dhaka_cases_2021[which(BGD$date==start_date_upa):nrow(BGD)], col="red", type = "l", lwd=1, lty=2)
  #   # plot(BGD$date[which(BGD$date==start_date_upa):nrow(BGD)]-start_date_upa, BGD$dhaka_deaths_2021[which(BGD$date==start_date_upa):nrow(BGD)], col="black", type = "l", lwd=1, lty=3)
  #   
  #   # Legend
  #   legend("topright",
  #          c("Daily new symptomatic","Daily new severe","Cumulative deaths", "Reported daily cases Dhaka", "Reported cumulative deaths Dhaka"),
  #          col=c("orange","darkred","black","red","black"),
  #          lty=c(1,1,1,2,2), lwd=c(2,2,2,1,1),bg=rgb(1,1,1,0.7),box.col=1)
  #   
  #   
  # })
  # 
  # output$death_ts_upazila <- renderPlot({
  #   
  #   par(mgp=c(2,1,0), mar=c(3,3,2,0))
  #   yRange = c(0, max(diff(c(0,out_upazila()$D))))
  #   xRange_date <- c(start_date_upa+8, start_date_upa+upa_days())
  #   xRange_num <- c(15,upa_days())
  #   plot(out_upazila()$time, diff(c(0,out_upazila()$D)), col=1, type = "l", xlab = "", ylab = "Count", lwd=2,
  #        ylim=yRange, xlim=xRange_num, axes=F,cex.lab=1.3)
  #   axis(1, at=start_date_upa %m+% months(0:(month(xRange_date[2])-month(start_date_upa))) - start_date_upa,
  #        labels = paste(month.abb[month(start_date_upa %m+% months(0:(month(xRange_date[2])-month(start_date_upa))))], year(start_date_upa %m+% months(0:(month(xRange_date[2])-month(start_date_upa))))))
  #   axis(2,at=pretty(yRange),labels=format(pretty(yRange),scientific=FALSE,big.mark = ','))
  #   graphics::box(bty="l")
  # 
  #   lines(BGD$date[which(BGD$date==start_date_upa):nrow(BGD)]-start_date_upa, BGD$dhaka_deaths_2021[which(BGD$date==start_date_upa):nrow(BGD)], 
  #         col="black", type = "l", lwd=1, lty=2)
  # 
  #   # Legend
  #   legend("topright",
  #          c("Daily deaths", "Reported daily deaths Dhaka"),
  #          col=1,
  #          lty=c(1,2), lwd=c(2,1),bg=rgb(1,1,1,0.7),box.col=1)
  #   
  #   
  # })
  
  
  #----- Create tables of model parameters --------------------------------------
  
  ## Epidemiological parameters
  output$param_table <- renderDataTable({
    datatable(
      data.frame(Parameter = Parameter,Value = Value, Description = Description, References = References, stringsAsFactors = FALSE), # from params_table.R
      colnames=c("Parameter", "Value (default)", "Description", "Source"), escape=F, rownames=F, # Allow html to be recognised
      options = list(dom = 't', ordering=F, "pageLength" = 40)) %>% # Remove all extra formatting in datatable
      formatStyle(columns = 1, width='140px') %>% # Set width for column 1
      formatStyle(columns = 2, width='140px') # Set width for column 2
  })
  
  
  ## Intervention/Testing parameters
  output$int_param_table <- renderDataTable({
    datatable(
      data.frame(Parameter = int_parameter,Value = int_value, Description = int_description, stringsAsFactors = FALSE), # from params_table.R
      colnames=c("Parameter", "Value (baseline default)", "Description"), escape=F, rownames=F, # Allow html to be recognised
      options = list(dom = 't', ordering=F, "pageLength" = 40)) %>% # Remove all extra formatting in datatable
      formatStyle(columns = 1, width='140px') %>% # Set width for column 1
      formatStyle(columns = 2, width='140px') # Set width for column 2
  })

  
  ## Population parameters
  output$pop_param_table <- renderDataTable({
    datatable(
      data.frame(Parameter = pop_parameter,Value = pop_value, Description = pop_description, References = pop_references, stringsAsFactors = FALSE), # from params_table.R
      colnames=c("Parameter", "Value (baseline default)", "Description", "Source"), escape=F, rownames=F, # Allow html to be recognised
      options = list(dom = 't', ordering=F, "pageLength" = 40)) %>% # Remove all extra formatting in datatable
      formatStyle(columns = 1, width='140px') %>% # Set width for column 1
      formatStyle(columns = 2, width='140px') # Set width for column 2
  })
  
  
  ## Age-dependent parameters  
  age_dep_pars_display <- age_dep_pars
  age_dep_pars_display$perc_symptomatic <- age_dep_pars$prop_symptomatic*100
  age_dep_pars_display <- age_dep_pars_display[,c("age_group","perc_symptomatic","hospitalised","CFR")]
  output$age_dep_param_table <- renderDataTable({
    datatable(
      age_dep_pars_display, # from params_table.R
      colnames=c("Age Group", 
                 HTML("% Symptomatic</br><a class='table_a' href=https://doi.org/10.1038/s41591-020-0962-9>(Davies et al. 2020)</a><br>"), 
                 HTML("% Hospitalised</br><a class='table_a' href=https://doi.org/10.1101/2020.05.06.20092734>(Davies et al. 2020)</a><br>"), 
                 HTML("% Fatal</br><a class='table_a' href=https://doi.org/10.1101/2020.05.06.20092734>(Davies et al. 2020)</a><br>")), 
      escape=F, rownames=F, # Allow html to be recognised
      options = list(dom = 't', ordering=F, "pageLength" = 40,# Remove all extra formatting in datatable
                     autoWidth = TRUE,columnDefs = list(list(width = '180px', targets = "_all"))
                     ))
  })
})
