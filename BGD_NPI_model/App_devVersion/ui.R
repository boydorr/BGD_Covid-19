#------------------------------------------------------------------------------#
# Use find and replace all to change colours used in this shiny app:
#   Main colour: #6b7b8a (slider: #96214A)
#   Highlight color: #302f2f
#   Background colour: #f2f2f2
#   Panel color: white
#------------------------------------------------------------------------------#

# Define UI 
shinyUI(
  navbarPage(

    #----- Add initial formatting ----------------------------------------------
    id = "main_page", # needed for landing page
    title=div(div(id = "img-id", img(src = "header_logos.png")), "COVID-19 Epi/Econ Decision Support (CEEDS)"),
    windowTitle = "COVID-19 analyses", #title for browser tab
    theme = shinytheme("sandstone"),
    collapsible = TRUE, # tab panels collapse into menu in small screens

    #----- Add CSS section to customise appearence --------------------
    header =
      tags$head(
        # Javascript for heading logo positioning
        # tags$script(type="text/javascript", src = "heading_code.js"),
        # CSS styles
        tags$style(
          # Heading logo image
          "#img-id{
            position: fixed;
            right: 3px;
            top: 3px;
          }",
          # Colour of navbar
          ".navbar-default {
              background-color: #6b7b8a;
                    }",
          # Colour of active tab
          ".navbar-default .navbar-nav .active a,
            .navbar-default .navbar-nav .active a:hover,
            .navbar-default .navbar-nav .active a:focus {
              background-color: #302f2f ;
                    }",
          # Colour of tab on hover
          ".navbar-default .navbar-nav li a:hover,
           .navbar-default .navbar-nav li a:focus {
              background-color: #302f2f ;
                    }",
          # Navbar title
          ".navbar-brand {
                     float: left;
                     padding: 20px 50px;
                     font-size: 20px;
                     line-height: 20px;
                     height: 60px;
                   }",
          # Navbar text
          ".navbar .nav li a {
                     font-size: 16px;
                     line-height: 22px;
                     font-weight: 500;
                     text-transform: uppercase;
                     color: white;
                   }",
          # Page background color
          "body {
            background-color: #f2f2f2;
          }",
          # Format boxes
          ".box {
           background-color: white;
          border: 1px dotted #6b7b8a ;
          padding: 10px;
          }",
          ".box-header,  {
          background-color: #96214A;
          padding: 1px 0;
          text-align: center;
          color: white;
          }",
          ".box-header h3 {
            text-align: center;
            padding-top: 2px;
            font-size: 20px;
          }",
          # Format inputs
          ".well {
            background-color: white;
            border-left: 2px solid #6b7b8a;
            border-right: 2px solid #6b7b8a;
            border-bottom: 2px solid #6b7b8a;
          }",
          # Format horizontal rule
          "hr.solid {
            border: 1px solid #6b7b8a;
          }",
          "hr.dot {
            border: 1px dotted #6b7b8a;
          }",
          # Format heading margins
          "h3 {
            margin-top: 5px;
          margin-bottom: 5px;
          font-weight: bold;
          }",
          # Tabs color: black; border: 1px solid #6b7b8a;
          ".tab-pane {
            background-color: white;
            border-top: 2px solid #6b7b8a;
            border-right: 2px solid #6b7b8a;
            width: 100%;
          }",
          ".tab-pane .active {
            border: none;
          }",
          ".nav .nav-tabs {
            width: 100%;
          }",
          ".nav-tabs {
            border-bottom: none;
          }",
          ".nav-tabs li.active a, .nav-tabs li.active a:hover, .nav-tabs li a:hover,
          .nav-tabs li.active a:focus {
            color: white;
            background-color: #302f2f ;
            text-transform: none;
            font-size: 16px;
          }",
          ".nav-tabs li a {
            background-color: #6b7b8a;
            color: white;
            text-transform: none;
            font-size: 16px;
          }",
          # Format text size
          "h5 {
            font-size: 16px;
          }",
          "h6 {
            font-size: 14px;
          }",
          # Remove bottom border on table
          "table.dataTable.no-footer {
            border-bottom: 0 !important;
          }",
          # Add some slide styling
          ".irs-grid-text {
            color: #302f2f;
            font-size: 11px;
          }",
          ".irs-min, .irs-max {
            color: #302f2f;
            font-size: 11px;
          }",
          ".irs-single {
            font-size: 11px;
          }",
          ".irs-line, .-irs-line-left, .-irs-line-right, .irs-bar-edge {
            color: red;
          }",
          # Move notification box
          "#shiny-notification-panel {
            width: auto;
          }",
          ".shiny-notification {
            opacity: 1;
            background-color: #6b7b8a;
            color: white;
            border: 2px solid #302f2f;
          }",
          ".shiny-notification-content{
            font-size: 16px;
          }",
          "shiny-notification-close {
            color: white;
          }",
          # Alter action button on baseline tab
          ".bttn, .bttn-default, .bttn-md {
            background: #6b7b8a;
            width: 100%;
          }",
          ".bttn-minimal.bttn-md {
            font-size: 14px;
          }",
          # Change link colour for parameter table
          ".table_a {
            color: red;
            font-weight: bold;
          }",
          ".table_a:hover {
            color: #302f2f;
            font-weight: bold;
            text-decoration: none;
          }"

        )),

    #------------------------------------------------------------------
    # Create 1st tab panel
    tabPanel(
      title = " Explore", icon = icon("home"),

      # Set slider colour for all sliders
      setSliderColor(color = rep("SlateGray",100),
                     sliderId = 1:100),

      # Set slider skin (style of marker)
      chooseSliderSkin(skin = "Flat"),
      # Change width of the sidebar: width = 350

      sidebarLayout(
        #----- Create sidebar -------------------------------------------
        sidebarPanel(width=3,
                     tabsetPanel(
                       #----- Add tab for Interventions ------------------------
                       #Interventions 1
                       tabPanel("Scenario 1",
                                br(),
                                h6(HTML("Adjust R<sub>0</sub>, intervention, and testing settings for the first of two scenarios for the forecast period (coloured black on the output plots on the 'Compare interventions' tab).")),
                                #----- R0 -------------------------
                                sliderInput(inputId = "int_R0", label = h4(HTML(paste("<strong>R", tags$sub(0),"</strong>", sep = ""))),
                                            min = minR0,  max = 6, value = parms_baseline["R0"], step = 0.01),
                                hr(class = "dot"),
                                #----- Vaccination inputs -------------------------
                                h4(HTML("<strong>Vaccination</strong>")),
                                radioButtons(inputId = "int_vax", "Vaccinate?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=TRUE),
                                conditionalPanel(
                                  condition="input.int_vax == 'TRUE'",
                                  sliderInput(inputId = "int_vax_dates", label = "Vaccination period:",
                                              min = start_date_forecast,  max=start_date_forecast+90,
                                              value = c(start_date_forecast,  start_date_forecast+90),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  sliderInput(inputId = "int_maxVax", label = "Target vaccination coverage",
                                              min = 0,  max = 100, value = 50, post="%"),
                                  numericInput(inputId = "int_vax_rate", label = "Vaccination capacity (doses/day):",
                                               value = 140000, width = "100%", min=0),
                                  sliderInput(inputId = "int_vax_compliance", label = "% of people in each age class that comply with vaccination:",
                                              min = 0,  max = 100, value = 80, post="%"),
                                  sliderInput(inputId = "int_vax_transmission_effect_dose1", label = "% reduction in transmission to people with one vaccination dose:",
                                              min = 0,  max = 100, value = parms_baseline["vax_transmission_effect_dose1"]*100, post="%"),
                                  sliderInput(inputId = "int_vax_severity_effect_dose1", label = "% reduction in an individual's risks of symptoms, hospitalisation, and death following one vaccination dose:",
                                              min = 0,  max = 100, value = parms_baseline["vax_severity_effect_dose1"]*100, post="%"),
                                  numericInput(inputId = "int_vax_delay", label = "days taken for vaccination to have a protective effect:",
                                              min = 0, value = parms_baseline["vax_delay"]),
                                  radioButtons(inputId = "int_vax_order", "Vaccinate oldest first, or vaccinate at random?",
                                               choices = list("Oldest" = 1,"Random" = 2), inline=TRUE, selected=parms_baseline["vax_order"]),
                                  radioButtons(inputId = "int_vax_2_doses", "Give a second vaccination dose?",
                                               choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["vax_2_doses"])),
                                  conditionalPanel(
                                    condition="input.int_vax_2_doses == 'TRUE'",
                                    sliderInput(inputId = "int_vax_transmission_effect_dose2", label = "% reduction in transmission to people with two vaccination doses:",
                                                min = 0,  max = 100, value = parms_baseline["vax_transmission_effect_dose2"]*100, post="%"),
                                    sliderInput(inputId = "int_vax_severity_effect_dose2", label = "% reduction in an individual's risks of symptoms, hospitalisation, and death following second vaccination dose:",
                                                min = 0,  max = 100, value = parms_baseline["vax_severity_effect_dose2"]*100, post="%"),
                                    numericInput(inputId = "int_t_between_doses", label = "Days between giving first and second doses:",
                                                 min = 0, value = parms_baseline["t_between_doses"]),
                                    
                                  )
                                ),
                                hr(class = "dot"),
                                #----- Lockdown inputs -------------------------
                                h4(HTML("<strong>Lockdown</strong>")),
                                radioButtons(inputId = "int_ld", "Add a lockdown?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=FALSE),
                                conditionalPanel(
                                  condition="input.int_ld == 'TRUE'",
                                  sliderInput(inputId = "int_ld_dates", label = "Lockdown period:",
                                              min = start_date_forecast,  max=start_date_forecast+90,
                                              value = c(start_date_forecast,  start_date_forecast+90-30),
                                            timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "int_ld_improve", label = "Days to full effectiveness:",
                                               value = parms_baseline["ld_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "int_fEW", label = "% workforce not under lockdown:",
                                              min = 0,  max = 100, value = (parms_baseline["fEW"]/parms_baseline["propWorkers"])*100, post="%"),
                                  sliderInput(inputId = "int_ld_compliance", label = "Maximum compliance:",
                                              min = 30,  max = 100, value = (1-parms_baseline["fNC"])*100, post="%"),
                                ),
                                hr(class = "dot"),
                                #----- Mask wearing inputs ----------------------
                                h4(HTML("<strong>Public mask wearing</strong>")),
                                radioButtons(inputId = "int_mask", "Introduce public mask wearing?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=FALSE),
                                conditionalPanel(
                                  condition="input.int_mask == 'TRUE'",
                                  sliderInput(inputId = "int_mask_dates", label = "Implementation period:",
                                              min = start_date_forecast,  max=start_date_forecast+90,
                                              value = c(start_date_forecast,  start_date_forecast+90),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "int_mask_improve", label = "Days to scale up:",
                                               value = 0, width = "100%", min=0),
                                  sliderInput(inputId = "int_mask_compliance", label = "Compliance:",
                                              min = 0,  max = 100, value = parms_baseline["mask_compliance"]*100, post="%"),
                                  sliderInput(inputId = "int_mask_effect_out", label = "Proportion reduction in transmission from mask-wearers:",
                                              min = 0,  max = 1, value = parms_baseline["mask_effect_outward"]),
                                  sliderInput(inputId = "int_mask_effect_in", label = "Proportion reduction in transmission to mask-wearers:",
                                              min = 0,  max = parms_baseline["mask_effect_outward"], value = parms_baseline["f_mask_effect_inward"]*parms_baseline["mask_effect_outward"])
                                ),
                                hr(class = "dot"),
                                #----- Community inputs ------------------------
                                h4(HTML("<strong>Household quarantine (with support by community support teams (CST))</strong>")),
                                radioButtons(inputId = "int_syndromic", "Introduce household quarantine?",
                                             choices = list("Yes" = TRUE,"No" = FALSE),inline=TRUE, selected=FALSE),
                                conditionalPanel(
                                  condition="input.int_syndromic == 'TRUE'",
                                  sliderInput(inputId = "int_syn_dates", label = "Implementation period:",
                                              min = start_date_forecast,  max=start_date_forecast+90,
                                              value = c(start_date_forecast,start_date_forecast+90),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "int_syn_improve", label = "Days to scale up:",
                                               value = parms_baseline["syn_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "int_community", label = "Quarantine adherence:",
                                              min = 0,  max = 100, value = parms_baseline["community"]*100, post="%")
                                ),
                                hr(class = "dot"),
                                #----- Testing inputs ----------------------
                                h4(HTML("<strong>Testing</strong>")),
                                radioButtons(inputId = "int_testing", "Add testing?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["testing"])),
                                conditionalPanel(
                                  condition="input.int_testing == 'TRUE'",
                                  sliderInput(inputId = "int_test_dates", label = "Implementation period:",
                                              min = start_date_forecast,  max=start_date_forecast+90,
                                              value = c(start_date_forecast,start_date_forecast+90),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  sliderInput(inputId = "int_test_compliance", label = "% symptomatic people that comply with testing:",
                                              value = parms_baseline["test_compliance"]*100, width = "100%", min=0,max=100,step=1),
                                  sliderInput(inputId = "int_test_capacity", label = "Test capacity (max tests/day):",
                                              min = 0, max = 100000, value = parms_baseline["test_capacity"],
                                              step = 100),
                                  sliderInput(inputId = "int_test_fneg", label = "False negative probability:",
                                              min = 0, max = 1, value = parms_baseline["test_fneg"])
                                )
                                
                       ),
                       #Interventions2
                       tabPanel("Scenario 2",
                                br(),
                                h6(HTML("Adjust R<sub>0</sub>, intervention, and testing settings for the second of two scenarios for the forecast period (coloured red on the output plots on the 'Compare interventions' tab).")),
                                #----- R0 -------------------------
                                sliderInput(inputId = "int2_R0", label = h4(HTML(paste("<strong>R", tags$sub(0),"</strong>", sep = ""))),
                                            min = minR0,  max = 6, value = parms_baseline["R0"], step = 0.01),
                                hr(class = "dot"),
                                #----- Vaccination inputs -------------------------
                                h4(HTML("<strong>Vaccination</strong>")),
                                radioButtons(inputId = "int2_vax", "Vaccinate?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=FALSE),
                                conditionalPanel(
                                  condition="input.int2_vax == 'TRUE'",
                                  sliderInput(inputId = "int2_vax_dates", label = "Vaccination period:",
                                              min = start_date_forecast,  max=start_date_forecast+90,
                                              value = c(start_date_forecast,  start_date_forecast+90),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  sliderInput(inputId = "int2_maxVax", label = "Target vaccination coverage",
                                              min = 0,  max = 100, value = 70, post="%"),
                                  numericInput(inputId = "int2_vax_rate", label = "Vaccination capacity (doses/day):",
                                               value = 140000, width = "100%", min=0),
                                  sliderInput(inputId = "int2_vax_compliance", label = "% of people in each age class that comply with vaccination:",
                                              min = 0,  max = 100, value = 80, post="%"),
                                  sliderInput(inputId = "int2_vax_transmission_effect_dose1", label = "% reduction in transmission to people with one vaccination dose:",
                                              min = 0,  max = 100, value = parms_baseline["vax_transmission_effect_dose1"]*100, post="%"),
                                  sliderInput(inputId = "int2_vax_severity_effect_dose1", label = "% reduction in an individual's risks of symptoms, hospitalisation, and death following one vaccination dose:",
                                              min = 0,  max = 100, value = parms_baseline["vax_severity_effect_dose1"]*100, post="%"),
                                  numericInput(inputId = "int2_vax_delay", label = "days taken for vaccination to have a protective effect:",
                                               min = 0, value = parms_baseline["vax_delay"]),
                                  radioButtons(inputId = "int2_vax_order", "Vaccinate oldest first, or vaccinate at random?",
                                               choices = list("Oldest" = 1,"Random" = 2), inline=TRUE, selected=parms_baseline["vax_order"]),
                                  radioButtons(inputId = "int2_vax_2_doses", "Give a second vaccination dose?",
                                               choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["vax_2_doses"])),
                                  conditionalPanel(
                                    condition="input.int2_vax_2_doses == 'TRUE'",
                                    sliderInput(inputId = "int2_vax_transmission_effect_dose2", label = "% reduction in transmission to people with two vaccination doses:",
                                                min = 0,  max = 100, value = parms_baseline["vax_transmission_effect_dose2"]*100, post="%"),
                                    sliderInput(inputId = "int2_vax_severity_effect_dose2", label = "% reduction in an individual's risks of symptoms, hospitalisation, and death following second vaccination dose:",
                                                min = 0,  max = 100, value = parms_baseline["vax_severity_effect_dose2"]*100, post="%"),
                                    numericInput(inputId = "int2_t_between_doses", label = "Days between giving first and second doses:",
                                                 min = 0, value = parms_baseline["t_between_doses"]),
                                    
                                  )
                                ),
                                hr(class = "dot"),
                                #----- Lockdown inputs -------------------------
                                h4(HTML("<strong>Lockdown</strong>")),
                                radioButtons(inputId = "int2_ld", "Add a lockdown?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=FALSE),
                                conditionalPanel(
                                  condition="input.int2_ld == 'TRUE'",
                                  sliderInput(inputId = "int2_ld_dates", label = "Lockdown period:",
                                              min = start_date_forecast,  max=start_date_forecast+90,
                                              value = c(start_date_forecast,  start_date_forecast+90-30),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "int2_ld_improve", label = "Days to full effectiveness:",
                                               value = parms_baseline["ld_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "int2_fEW", label = "% workforce not under lockdown:",
                                              min = 0,  max = 100, value = (parms_baseline["fEW"]/parms_baseline["propWorkers"])*100, post="%"),
                                  sliderInput(inputId = "int2_ld_compliance", label = "Maximum compliance:",
                                              min = 30,  max = 100, value = (1-parms_baseline["fNC"])*100, post="%"),
                                ),
                                hr(class = "dot"),
                                #----- Mask wearing inputs ----------------------
                                h4(HTML("<strong>Public mask wearing</strong>")),
                                radioButtons(inputId = "int2_mask", "Introduce public mask wearing?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=FALSE),
                                conditionalPanel(
                                  condition="input.int2_mask == 'TRUE'",
                                  sliderInput(inputId = "int2_mask_dates", label = "Implementation period:",
                                              min = start_date_forecast,  max=start_date_forecast+90,
                                              value = c(start_date_forecast,  start_date_forecast+90),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "int2_mask_improve", label = "Days to scale up:",
                                               value = 0, width = "100%", min=0),
                                  sliderInput(inputId = "int2_mask_compliance", label = "Compliance:",
                                              min = 0,  max = 100, value = parms_baseline["mask_compliance"]*100, post="%"),
                                  sliderInput(inputId = "int2_mask_effect_out", label = "Proportion reduction in transmission from mask-wearers:",
                                              min = 0,  max = 1, value = parms_baseline["mask_effect_outward"]),
                                  sliderInput(inputId = "int2_mask_effect_in", label = "Proportion reduction in transmission to mask-wearers:",
                                              min = 0,  max = parms_baseline["mask_effect_outward"], value = parms_baseline["f_mask_effect_inward"]*parms_baseline["mask_effect_outward"])
                                ),
                                hr(class = "dot"),
                                #----- Community inputs ------------------------
                                h4(HTML("<strong>Household quarantine (with support by community support teams (CST))</strong>")),
                                radioButtons(inputId = "int2_syndromic", "Introduce household quarantine?",
                                             choices = list("Yes" = TRUE,"No" = FALSE),inline=TRUE, selected=FALSE),
                                conditionalPanel(
                                  condition="input.int2_syndromic == 'TRUE'",
                                  sliderInput(inputId = "int2_syn_dates", label = "Implementation period:",
                                              min = start_date,  max=start_date_forecast+90,
                                              value = c(start_date_forecast,start_date_forecast+90),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "int2_syn_improve", label = "Days to scale up:",
                                               value = parms_baseline["syn_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "int2_community", label = "Quarantine adherence:",
                                              min = 0,  max = 100, value = parms_baseline["community"]*100, post="%")
                                ),
                                hr(class = "dot"),
                                #----- Testing inputs ----------------------
                                h4(HTML("<strong>Testing</strong>")),
                                radioButtons(inputId = "int2_testing", "Add testing?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["testing"])),
                                conditionalPanel(
                                  condition="input.int2_testing == 'TRUE'",
                                  sliderInput(inputId = "int2_test_dates", label = "Implementation period:",
                                              min = start_date_forecast,  max=start_date_forecast+90,
                                              value = c(start_date_forecast,start_date_forecast+90),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  sliderInput(inputId = "int2_test_compliance", label = "% symptomatic people that comply with testing:",
                                              value = parms_baseline["test_compliance"]*100, width = "100%", min=0,max=100,step=1),
                                  sliderInput(inputId = "int2_test_capacity", label = "Test capacity (max tests/day):",
                                              min = 0, max = 100000, value = parms_baseline["test_capacity"],
                                              step = 100),
                                  sliderInput(inputId = "int2_test_fneg", label = "False negative probability:",
                                              min = 0, max = 1, value = parms_baseline["test_fneg"])
                                )
                                
                       ),
                       #----- Add tab for baseline -----------------------------
                       tabPanel("Initialisation",
                                br(),
                                h6(HTML("Adjust R<sub>0</sub>, intervention, and testing settings for the initial period from 1st March to 15th September, prior to the forecast period. The model outputs during this period are not shown in the 'Compare interventions' tab, but can be viewed in the 'Model and data comparison' tab")),
                                br(),
                                
                                #----- R0 -------------------------
                                sliderInput(inputId = "bl_R0", label = h4(HTML(paste("<strong>R", tags$sub(0),"</strong>", sep = ""))),
                                            min = minR0,  max = 6, value = parms_baseline["R0"], step = 0.01),
                                hr(class = "dot"),
                                
                                #----- Pop initialisation -------------------------
                                numericInput(inputId = "initial_infectious", label = h4(HTML("<strong>Number infectious at initialisation</strong>")), min=0, max = 2000000,
                                             value = 19086, width = "300px"),
                                sliderInput(inputId = "initial_immune", label = h4(HTML("<strong>% population immune at initialisation</strong>")),
                                            min = 0, max = 100, value = 40, post="%"),
                                hr(class = "dot"),
                                
                                #----- Vaccination inputs -------------------------
                                h4(HTML("<strong>Vaccination</strong>")),
                                h6(HTML("Data on numbers of single and double-vaccinated individuals in the pre-forecast period are taken from <a class='table_a' href=https://github.com/RamiKrispin/coronavirus/>https://github.com/RamiKrispin/coronavirus/</a>")),
                                br(),
                                numericInput(inputId = "bl_vax_delay", label = "Days taken for vaccination to have a protective effect:",
                                             min = 0, value = parms_baseline["vax_delay"]),
                                radioButtons(inputId = "bl_vax_order", "Vaccinate oldest first, or vaccinate at random?",
                                             choices = list("Oldest" = 1,"Random" = 2), inline=TRUE, selected=parms_baseline["vax_order"]),
                                sliderInput(inputId = "bl_vax_compliance", label = "% of people in each age class that comply with vaccination:",
                                            min = 0,  max = 100, value = (parms_baseline["vax_compliance"])*100, post="%"),
                                sliderInput(inputId = "bl_vax_transmission_effect_dose1", label = "% reduction in transmission to people with one vaccination dose:",
                                            min = 0,  max = 100, value = parms_baseline["vax_transmission_effect_dose1"]*100, post="%"),
                                sliderInput(inputId = "bl_vax_severity_effect_dose1", label = "% reduction in an individual's risks of symptoms, hospitalisation, and death following one vaccination dose:",
                                            min = 0,  max = 100, value = parms_baseline["vax_severity_effect_dose1"]*100, post="%"),
                                sliderInput(inputId = "bl_vax_transmission_effect_dose2", label = "% reduction in transmission to people with two vaccination doses:",
                                            min = 0,  max = 100, value = parms_baseline["vax_transmission_effect_dose2"]*100, post="%"),
                                sliderInput(inputId = "bl_vax_severity_effect_dose2", label = "% reduction in an individual's risks of symptoms, hospitalisation, and death following second vaccination dose:",
                                            min = 0,  max = 100, value = parms_baseline["vax_severity_effect_dose2"]*100, post="%"),
                                
                                hr(class = "dot"),
                                #----- Lockdown inputs -------------------------
                                h4(HTML("<strong>Lockdown</strong>")),
                                radioButtons(inputId = "bl_ld", "Add a lockdown?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["ld"])),
                                conditionalPanel(
                                  condition="input.bl_ld == 'TRUE'",
                                  sliderInput(inputId = "bl_ld_dates", label = "Lockdown period:",
                                              min = start_date,  max=start_date_forecast,
                                              value = c(start_date+parms_baseline[c("ld_start","ld_end")]),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "bl_ld_improve", label = "Days to full effectiveness:",
                                               value = parms_baseline["ld_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "bl_fEW", label = "% workforce not under lockdown:",
                                              min = 0,  max = 100, value = (parms_baseline["fEW"]/parms_baseline["propWorkers"])*100, post="%"),
                                  sliderInput(inputId = "bl_ld_compliance", label = "Maximum compliance:",
                                              min = 30,  max = 100, value = (1-parms_baseline["fNC"])*100, post="%"),
                                ),
                                
                                hr(class = "dot"),
                                #----- Mask wearing inputs ----------------------
                                h4(HTML("<strong>Public mask wearing</strong>")),
                                radioButtons(inputId = "bl_mask", "Introduce public mask wearing?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["mask"])),
                                conditionalPanel(
                                  condition="input.bl_mask == 'TRUE'",
                                  sliderInput(inputId = "bl_mask_dates", label = "Implementation period:",
                                              min = start_date,  max=start_date_forecast,
                                              value = c(start_date+parms_baseline[c("mask_start","mask_end")]),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "bl_mask_improve", label = "Days to scale up:",
                                               value = parms_baseline["mask_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "bl_mask_compliance", label = "Compliance:",
                                              min = 0,  max = 100, value = parms_baseline["mask_compliance"]*100, post="%"),
                                  sliderInput(inputId = "bl_mask_effect_out", label = "Proportion reduction in transmission from mask-wearers:",
                                              min = 0,  max = 1, value = parms_baseline["mask_effect_outward"]),
                                  sliderInput(inputId = "bl_mask_effect_in", label = "Proportion reduction in transmission to mask-wearers:",
                                              min = 0,  max = parms_baseline["mask_effect_outward"], value = parms_baseline["f_mask_effect_inward"]*parms_baseline["mask_effect_outward"])
                                  
                                ),
                                hr(class = "dot"),
                                
                                #----- Community inputs ------------------------
                                h4(HTML("<strong>Household quarantine (with support by community support teams (CST))</strong>")),
                                radioButtons(inputId = "bl_syndromic", "Introduce household quarantine?",
                                             choices = list("Yes" = TRUE,"No" = FALSE),inline=TRUE, selected=as.logical(parms_baseline["syndromic"])),
                                conditionalPanel(
                                  condition="input.bl_syndromic == 'TRUE'",
                                  sliderInput(inputId = "bl_syn_dates", label = "Implementation period:",
                                              min = start_date,  max=start_date_forecast,
                                              value = c(start_date+parms_baseline[c("syn_start","syn_end")]),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "bl_syn_improve", label = "Days to scale up:",
                                               value = parms_baseline["syn_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "bl_community", label = "Quarantine adherence:",
                                              min = 0,  max = 100, value = parms_baseline["community"]*100, post="%"),
                                  
                                )

                       )
                     )
        ),

        #----- Create main Panel -----------------------------------------------
        mainPanel(
          tabsetPanel(
            #----- Add tab for Interventions Dhaka -----------------------------------------
            tabPanel("Compare interventions",
                     br(),
                     #----- Add row for description of content -----------------
                     fluidRow(
                       box(width=10,
                           h5(HTML("Forecasts are shown for mortality, severe cases requiring hospital care,
                              and case detection by testing in Dhaka District. These metrics are shown both as time series and
                              summarized over the entire forecast period (barplots to the right).")),
                           h5("All plots show outputs for two different scenarios to allow the impact of different interventions to be compared. 
                              The interventions applied in each scenario can be adjusted via the 'Scenario 1' and 'Scenario 2' tabs in the sidebar."),
                           h5("The forecast period starts on 15th September 2021, and ends after a number of days that can be selected at the bottom of this page (defaults to 90 days)."),
                           h5("The proportions of cases that are symptomatic, hospitalised and fatal increase as the 
                              average age of the population increases. The age distribution can be adjusted at the 
                              bottom of this tab, and defaults to the age distribution in Dhaka District at the time
                              of the last census.  The size of the population can also be adjusted."),
                           h5("If selected, vaccination occurs over a chosen time period within the forecast period. 
                              The target vaccination coverage is the maximum value that coverage can reach during the forecast period. 
                              However, this target may not be achieved if the daily vaccination capacity within the vaccination period is too low, or if too small a
                              percentage of the population complies with vaccination. Vaccines can either be distributed to the oldest first, or randomly among age classes. If 
                              a second dose is to be given then the time between first and second doses can also be selected.")
                           # verbatimTextOutput("out")
                           ),
                       box(width=2,
                           HTML("<center><img src='bd_map.png' style='max-width: 100%; height: auto;'></center>")
                           )
                     ),
                     br(),

                     #----- Add row for vaccination plots ------------------------
                     fluidRow(
                       box(width=12, title="Vaccination coverage", solidHeader = TRUE,
                           plotOutput("plot_vax", height="250px")
                       ),
                       
                       # box(width=4, title="Total Vaccinated", solidHeader = TRUE,
                       #     plotOutput("barplot_vax", height="200px")
                       # )
                     ),
                     br(),
                     #----- Add row for mortality plots ------------------------
                     fluidRow(
                       box(width=8, title="Daily deaths", solidHeader = TRUE,
                           plotOutput("plot_mortality", height="200px")
                       ),

                       box(width=4, title="Total deaths", solidHeader = TRUE,
                           plotOutput("barplot_mortality", height="200px")
                       )
                     ),
                     br(),
                     #----- Add row for hospitalisation plots ----------------
                     fluidRow(
                       box(width=8, title="Hospital Demand", solidHeader = TRUE,
                           plotOutput("plot_hosp", height="200px")
                       ),
                       box(width=4, title="Total hospitalised", solidHeader = TRUE,
                           plotOutput("barplot_hosp", height="200px")
                       )
                     ),
                     br(),
                     
                     #----- Add row for case detection plot --------------------
                     fluidRow(
                       box(width=9, title = "Cases & detection", solidHeader = TRUE,
                           plotOutput("case_detection", height="200px")
                       ),
                       box(width=3, title="% cases detected", solidHeader = TRUE,
                           plotOutput("barplot_testing", height="200px")
                       )
                     ),
                     br(),
                     fluidRow(
                       box(width=12.5, solidHeader=TRUE,title="Select forecast length & population parameters",
                           br(),
                           span(textOutput("demog_warning"), style="color:red;font-size: 17px;
                                 font-weight: bold;"),
                           br(),
                           actionBttn(inputId="pop_reset", label = "Reset defaults",
                                      style="minimal"),
                           br(),
                           br(),
                           splitLayout(
                             numericInput(inputId = "days", label = h4(HTML("<strong>Number of days to forecast from 15th September</strong>")), min=30, max = 6*30,
                                          value = 90, width = "300px"),
                             
                             # h5("Defaults to the estimated population of Dhaka District in 0."),
                             numericInput(inputId = "pop", label = h4(HTML("<strong>Population size</strong>")),
                                          value = parms_baseline["population"], width = "300px", min=0)
                             
                           ),
                           splitLayout(
                             h5(""),
                             h5("Defaults to the estimated population of Dhaka District in 2020.")
                             
                           ),
                           
                           br(),br(),
                           
                           
                           h4(HTML("<strong>Age distribution</strong>")),
                           h5("Defaults to the overall age distribution for Dhaka District estimated by the 2011 census."),
                           br(),
                           splitLayout(
                             numericInput(inputId = "demog_1", label = "% Age 0-9:",
                                          value = round(dhaka_pop_by_age$prop[1]*100,1), width = "150px", min=0,max=100),
                             numericInput(inputId = "demog_2", label = "% Age 10-19:",
                                          value = round(dhaka_pop_by_age$prop[2]*100,1), width = "150px", min=0,max=100),
                             numericInput(inputId = "demog_3", label = "% Age 20-29:",
                                          value = round(dhaka_pop_by_age$prop[3]*100,1), width = "150px", min=0,max=100)
                           ),
                           splitLayout(
                             numericInput(inputId = "demog_4", label = "% Age 30-39:",
                                          value = round(dhaka_pop_by_age$prop[4]*100,1), width = "150px", min=0,max=100),
                             numericInput(inputId = "demog_5", label = "% Age 40-49:",
                                          value = round(dhaka_pop_by_age$prop[5]*100,1), width = "150px", min=0,max=100),
                             numericInput(inputId = "demog_6", label = "% Age 50-59:",
                                          value = round(dhaka_pop_by_age$prop[6]*100,1), width = "150px", min=0,max=100)
                           ),
                           splitLayout(
                             numericInput(inputId = "demog_7", label = "% Age 60-69:",
                                          value = round(dhaka_pop_by_age$prop[7]*100,1), width = "150px", min=0,max=100),
                             numericInput(inputId = "demog_8", label = "% Age 70-79:",
                                          value = round(dhaka_pop_by_age$prop[8]*100,1), width = "150px", min=0,max=100),
                             numericInput(inputId = "demog_9", label = "% Age 80+",
                                          value = round(dhaka_pop_by_age$prop[9]*100,1), width = "150px", min=0,max=100)
                           )
                           
                           
                           
                           
                       )
                       
                     )
            ),
            #----- Add tab for model and data comparison -------------------------------
            tabPanel("Model and data comparison",
                     br(),
                     #----- Add row for explanation of panels ------------------
                     fluidRow(
                       box(width=12, #title="", solidHeader = TRUE,
                           h5(HTML("We use an SEIR model to explore impacts of control measures on COVID-19 transmission in Dhaka District.",
                                   "R<sub>0</sub> was tuned to match the trend in deaths prior in March 2021.",
                                   "Other epidemiological and population parameters were obtained from the literature. 
                                   Most parameters describing the interventions, including timing, compliance, and impacts on transmission, can be adjusted in the sidebar tabs. 
                                   The model is first run through an initialisation period from 1st March to 15th September 2021 (the interventions and some of the parameters assumed during this period can be
                                   viewed and adjusted in the 'Initialisation' tab of the sidebar. The forecast period then runs from 15th September  for a selected number of days."
                                   
                           )),
                           h5(HTML("A detailed description of the model and of how this app has been used in Bangladesh, along with further analyses, can be found in our <a class='table_a' href=https://www.medrxiv.org/content/10.1101/2021.04.19.21255673v1.full.pdf>preprint</a>. 
                                   R code for the model and app can be found in our <a class='table_a' href=https://github.com/boydorr/BGD_Covid-19/tree/main/BGD_NPI_model/App_devVersion>Github repository</a>."))
                       )
                     ),
                     br(),
                     
                     #----- Add rows for case timeseries plots--------------------
                     fluidRow(
                       box(width=12, title="Forecast vs reported cases & deaths", solidHeader = TRUE,
                           plotOutput("epi_ts", height = "350px")
                       ),
                       
                     ),
                     br(),
                     fluidRow(
                       box(width=12, title="Forecast vaccination vs reported vaccination", solidHeader = TRUE,
                           plotOutput("vax_ts", height = "350px")
                       )
                     ),
                     br(),
                     #----- Add row for 'secret' y-axis checkbox ---------------
                     fluidRow(
                       div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                       div(style="display: inline-block;vertical-align:top; width: 250px;",
                               checkboxInput(inputId = "show_y_axis", label="Show y-axis on all plots?",
                                     value=FALSE))
                     ),
                     br()
            ),
            #----- Add tab for assumptions -------------------------------
            tabPanel("Intervention assumptions",
                     br(),
                     #----- Add row for explanation of panels ------------------
                     fluidRow(
                       box(width=12, #title="", solidHeader = TRUE,
                           h5(HTML("<strong>Vaccination</strong>")),
                           h5(HTML("<ul><li>If selected, vaccination occurs over a chosen time period within the forecast period. </li><li>
                              The target vaccination coverage is the maximum value that coverage can reach during the forecast period. 
                              However, this target may not be achieved if the daily vaccination capacity within the vaccination period is too low, or if too small a
                              percentage of the population complies with vaccination (i.e. accepts a vaccine when offered). </li><li>Vaccines can either be distributed to the oldest first, or randomly among age classes. </li><li>If 
                              a second dose is to be given then the time delay between first and second doses can also be selected.</li><li>
                              There is a (selectable) time delay between the date a vaccination dose is given and the date that dose has any protective effect</li><li>
                                   Vaccinations have two effects: 1) they reduce the rate of transmission to a partially or fully vaccinated person by a selected percentage; and 2) if a partially or fully vaccinated person does become infected, their risks of developing symptoms, being hospitalised, or dying are all reduced by a selected percentage.</ul></li>")),
                           br(),
                           
                           h5(HTML("<strong>Lockdown</strong>")),
                           h5(HTML("<ul><li>If selected, lockdown occurs over a chosen time period within the forecast (or initialisation) period. </li><li>
                              The between-household transmission rates of individuals that comply with the lockdown are reduced by 76% (estimated using COVID-19 death data during the first lockdown in Bangladesh). </li><li>                              
                              Compliance with the lockdown increases linearly from 0% to the maximum compliance during the selected scale-up period. Following this compliance declines according to a sigmoidal function (with rate of decline and inflection point fitted to Google community mobility data at the time of the first lockdown in Bangladesh) to a minimum compliance of 30%.</li><li>
                              A selected proportion of the workforce is permitted to continue working during lockdown. It is assumed that these individuals have no reduction in their transmission rates (unless they become symptomatic, then their transmission is reduced with a probability equal to the current compliance level).
                              </ul></li>")),
                           br(),
                           
                           h5(HTML("<strong>Public mask wearing</strong>")),
                           h5(HTML("<ul><li>If selected, mask wearing occurs over a chosen time period within the forecast (or initialisation) period. </li><li>
                              Compliance with mask wearing increases linearly from 0% to the selected compliance level during the selected scale-up period. </li><li>
                              The between-household transmission rates of individuals that wear masks are reduced by a selected proportion.  Those that wear masks also have a proportion protection from transmission from others (this proportion must be lower than the proportion reduction in transmission by mask-wearers). 
                              </ul></li>")),
                           br(),
                           
                           h5(HTML("<strong>Household quarantine</strong>")),
                           h5(HTML("<ul><li>If selected, household quarantine occurs over a chosen time period within the forecast (or initialisation) period. </li><li>
                              Compliance with household quarantine increases linearly from 0% to the selected compliance level during the selected scale-up period. </li><li>
                              A household is assumed to be of size 4. </li><li>
                              Individuals in quarantined households are assumed to cause no between-household transmission (and cannot be infected by between-household transmission) from the time the household is quarantined. </li><li>
                              Quarantine of a household can be triggered by a presymptomatic individual becoming symptomatic when that individual was either: 1) the first infection in their household; or 2) infected by an asymptomatic first infection in their household. However, only a selected percentage of households comply with quarantine when one of these trigger events occurs.
                              </ul></li>")),
                           br(),
                           
                           h5(HTML("<strong>Testing</strong>")),
                           h5(HTML("<ul><li>If selected, testing occurs over a chosen time period within the forecast period. </li><li>
                              Only those with symptoms seek testing. </li><li>
                              It is assumed that 35% of people in the population contract a non-COVID-19 influenza-like illness (ILI) each year, and that these people are just as likely to seek COVID-19 testing as those with actual COVID-19 infections. </li><li>
                              A selected percentage of those with COVID-19 or other ILI symptoms comply with testing. </li><li>
                              If more individuals become symptomatic and seek testing on a given day than there is testing capacity for, the excess individuals are not tested. </li><li>
                              A selected proportion of tests on those with real COVID-19 infections will give false negative results. </li><li>
                              Testing has no impact on transmission or health outcomes - it only determines what proportion of cases are detected.
                              </ul></li>"))
                           
                       )
                     )
                     
            )
          )
        )
      )
    ),
    #------------------------------------------------------------------
    # Create 2nd tab panel
    tabPanel(
      title = "Info", icon = icon("info"),
      mainPanel(width = 12,
                fluidRow(
                  column(width=1
                  ),
                  column(width=10,
                         box(width = 12,
                             br(),
                             h3(HTML("<center> Background </center>")),
                             h5(HTML("This tool was developed in collaboration with multiple partners in early 2020.
                                  A detailed description of the model and of how this app has been used in Bangladesh, along with further analyses, 
                                  can be found in our <a class='table_a' href=https://www.medrxiv.org/content/10.1101/2021.04.19.21255673v1.full.pdf>preprint</a>. 
                                   Code for the model and app is in our 
                                     <a class='table_a' href=https://github.com/boydorr/BGD_Covid-19/tree/main/BGD_NPI_model>Github repository</a>.")),
                             h5("To understand how the epidemic could progress in Bangladesh and the potential impacts of different responses we
                                developed a relatively simple deterministic SEIR framework. We assume persons infected with the virus 
                                are either asymptomatic for the duration of their infectious period or enter a pre-symptomatic infectious state before progressing
                                to symptomatic infection. A proportion of symptomatic people are hospitalised and/or die. The rates and probabilities 
                                of movement between states are informed by published studies."),
                             h5("We assume that transmission can be reduced through response measures, but the size of the reduction depends 
                                on how well interventions are implemented, adhered to and enforced. i.e. as a result of investment, technological and sociological capacity, communications, trust etc."),
                             h5("The timing and duration of the epidemic and associated responses will have major impacts on morbidity,
                                mortality and the economy, with potential to overwhelm health systems and cause catastrophic
                                consequences for families, communities and society as a whole. These knock on effects are beyond the scope of
                                our model. However, we attempt to lay out short-term impacts on morbidity and hospital demand relative to capacity."),
                             h5("There is considerable uncertainty in quantitative predictions, so we focus on order of magnitude
                                impacts. We caveat that while this framework helps us to understand the consequences of different
                                decisions, outcomes depend on how interventions are delivered and complied with. We use publicly available data 
                                on the course of the pandemic to calibrate the model, and caution that recorded cases and deaths are underestimated 
                                in most countries. Caution is therefore needed in comparing model trajectories and data."),
                             br()
                         )
                  ),
                  column(width=1
                  )
                )
      )
    )

  )
)


