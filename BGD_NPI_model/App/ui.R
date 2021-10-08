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
      setSliderColor(color = rep("SlateGray",50),
                     sliderId = 1:50),

      # Set slider skin (style of marker)
      chooseSliderSkin(skin = "Flat"),
      # Change width of the sidebar: width = 350

      sidebarLayout(
        #----- Create sidebar -------------------------------------------
        sidebarPanel(width=3,
                     tabsetPanel(
                       #----- Add tab for Interventions ------------------------
                       tabPanel("Comparison",
                                br(),
                                h6(HTML("Adjust R<sub>0</sub>, intervention, and testing settings for the comparison scenario on the 'Interventions Dhaka' tab (coloured red on output plots).")),
                                #----- R0 -------------------------
                                sliderInput(inputId = "int_R0", label = HTML(paste("R", tags$sub(0), sep = "")),
                                            min = minR0,  max = 6, value = parms_baseline["R0"], step = 0.01),
                                hr(class = "dot"),
                                #----- Lockdown inputs -------------------------
                                sliderInput(inputId = "int_ld_dates", label = "Lockdown period:",
                                            min = start_date,  max=end_date,
                                            value = c(start_date+parms_baseline[c("ld_start","ld_end")]),
                                            timeFormat = "%d %b %y",step=1,ticks=F),
                                numericInput(inputId = "int_ld_improve", label = "Days to full effectiveness:",
                                             value = parms_baseline["ld_improve"], width = "100%", min=0),
                                sliderInput(inputId = "int_fEW", label = "% workforce not under lockdown:",
                                            min = 0,  max = 100, value = (parms_baseline["fEW"]/parms_baseline["propWorkers"])*100, post="%"),
                                sliderInput(inputId = "int_ld_compliance", label = "Peak compliance:",
                                            min = 30,  max = 100, value = (1-parms_baseline["fNC"])*100, post="%"),
                                hr(class = "dot"),
                                radioButtons(inputId = "int_ld2", "Add a second lockdown phase?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["ld2"])),
                                conditionalPanel(
                                  condition="input.int_ld2 == 'TRUE'",
                                  sliderInput(inputId = "int_ld2_dates", label = "Implementation period:",
                                              min = start_date + parms_baseline["ld_end"],  max=end_date,
                                              value = c(start_date+parms_baseline[c("ld2_start","ld2_end")]),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "int_ld2_improve", label = "Days to full effectiveness:",
                                               value = parms_baseline["ld2_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "int_fEW2", label = "% workforce not under lockdown:",
                                              min = 0,  max = 100, value = (parms_baseline["fEW2"]/parms_baseline["propWorkers"])*100, post="%"),
                                  sliderInput(inputId = "int_ld2_compliance", label = "Peak compliance:",
                                              min = 30,  max = 100, value = (1-parms_baseline["fNC2"])*100, post="%"),

                                ),
                                hr(class = "dot"),
                                #----- Mask wearing inputs ----------------------
                                radioButtons(inputId = "int_mask", "Compulsory public mask wearing?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["mask"])),
                                conditionalPanel(
                                  condition="input.int_mask == 'TRUE'",
                                  sliderInput(inputId = "int_mask_dates", label = "Implementation period:",
                                              min = start_date,  max=end_date,
                                              value = c(start_date+parms_baseline[c("mask_start","mask_end")]),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "int_mask_improve", label = "Days to scale up:",
                                               value = parms_baseline["mask_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "int_mask_compliance", label = "Compliance:",
                                              min = 0,  max = 100, value = parms_baseline["mask_compliance"]*100, post="%"),
                                  sliderInput(inputId = "int_mask_effect_out", label = "Proportion reduction in transmission from mask-wearers:",
                                              min = 0,  max = 1, value = parms_baseline["mask_effect_outward"]),
                                  sliderInput(inputId = "int_mask_effect_in", label = "Proportion reduction in transmission to mask-wearers:",
                                              min = 0,  max = parms_baseline["mask_effect_outward"], value = parms_baseline["f_mask_effect_inward"]*parms_baseline["mask_effect_outward"])
                                ),
                                hr(class = "dot"),
                                #----- Lab testing inputs ----------------------
                                radioButtons(inputId = "int_lab", "Laboratory testing?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["lab"])),
                                conditionalPanel(
                                  condition="input.int_lab == 'TRUE'",
                                  sliderInput(inputId = "int_lab_dates", label = "Implementation period:",
                                              min = start_date,  max=end_date,
                                              value = c(start_date+parms_baseline[c("lab_start","lab_end")]),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "int_lab_improve", label = "Days to scale up:",
                                               value = parms_baseline["lab_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "int_lab_capacity", label = "Full capacity (tests/day):",
                                              min = 0, max = 100000, value = parms_baseline["capacity_lab"],
                                              step = 100),
                                  sliderInput(inputId = "int_lab_fneg", label = "False negatives:",
                                              min = 0, max = 1, value = parms_baseline["lab_fneg"]),
                                  numericInput(inputId = "int_lab_cost", label = "Cost per Lab test:",
                                               value = parms_baseline["lab_cost"])
                                ),
                                hr(class = "dot"),
                                #----- Community inputs ------------------------
                                radioButtons(inputId = "int_syndromic", "Community Support Team response?",
                                             choices = list("Yes" = TRUE,"No" = FALSE),inline=TRUE, selected=as.logical(parms_baseline["syndromic"])),
                                conditionalPanel(
                                  condition="input.int_syndromic == 'TRUE'",
                                  sliderInput(inputId = "int_syn_dates", label = "Implementation period:",
                                              min = start_date,  max=end_date,
                                              value = c(start_date+parms_baseline[c("syn_start","syn_end")]),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "int_syn_improve", label = "Days to scale up:",
                                               value = parms_baseline["syn_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "int_community", label = "Quarantine adherence:",
                                              min = 0,  max = 100, value = parms_baseline["community"]*100, post="%"),
                                  sliderInput(inputId = "int_rapid_capacity", label = "Rapid tests (/day):",
                                              min = 0, max = 200000, value = parms_baseline["capacity_rapid"],
                                              step = 500),
                                  sliderInput(inputId = "int_rapid_fneg", label = "False negatives:",
                                              min = 0, max = 1, value = parms_baseline["rapid_fneg"]),
                                  numericInput(inputId = "int_RDT_cost", label = "Cost per RDT:",
                                               value = parms_baseline["rapid_cost"])

                                )
                       ),
                       #----- Add tab for baseline -----------------------------
                       tabPanel("Baseline",
                                br(),
                                h6(HTML("Adjust R<sub>0</sub>, intervention, and testing settings for the baseline scenario on the 'Interventions Dhaka' tab (coloured black on output plots). The baseline parameter settings are also used to produce the plots on the 'Technical Details' tab")),
                                br(),
                                radioButtons(inputId = "edit_baseline", "Adjust Baseline Parameters?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=FALSE),
                                conditionalPanel(
                                  condition="input.edit_baseline == 'TRUE'",
                                  actionBttn(inputId="up_int_adj_bl", label = "Update Comparison values with new baseline",
                                             style="minimal"),
                                  hr(class = "dot"),
                                  #----- R0 -------------------------
                                  sliderInput(inputId = "bl_R0", label = HTML(paste("R", tags$sub(0), sep = "")),
                                              min = minR0,  max = 6, value = parms_baseline["R0"], step = 0.01),
                                  hr(class = "dot"),
                                  #----- Lockdown inputs -------------------------
                                  sliderInput(inputId = "bl_ld_dates", label = "Lockdown period:",
                                              min = start_date,  max=end_date,
                                              value = c(start_date+parms_baseline[c("ld_start","ld_end")]),
                                              timeFormat = "%d %b %y",step=1,ticks=F),
                                  numericInput(inputId = "bl_ld_improve", label = "Days to full effectiveness:",
                                               value = parms_baseline["ld_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "bl_fEW", label = "% workforce not under lockdown:",
                                              min = 0,  max = 100, value = (parms_baseline["fEW"]/parms_baseline["propWorkers"])*100, post="%"),
                                  sliderInput(inputId = "bl_ld_compliance", label = "Peak compliance:",
                                              min = 30,  max = 100, value = (1-parms_baseline["fNC"])*100, post="%"),
                                  hr(class = "dot"),
                                  radioButtons(inputId = "bl_ld2", "Add a second lockdown phase?",
                                               choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["ld2"])),
                                  conditionalPanel(
                                    condition="input.bl_ld2 == 'TRUE'",
                                    sliderInput(inputId = "bl_ld2_dates", label = "Implementation period:",
                                                min = start_date + parms_baseline["ld_end"],  max=end_date,
                                                value = c(start_date+parms_baseline[c("ld2_start","ld2_end")]),
                                                timeFormat = "%d %b %y",step=1,ticks=F),
                                    numericInput(inputId = "bl_ld2_improve", label = "Days to full effectiveness:",
                                                 value = parms_baseline["ld2_improve"], width = "100%", min=0),
                                    sliderInput(inputId = "bl_fEW2", label = "% workforce not under lockdown:",
                                                min = 0,  max = 100, value = (parms_baseline["fEW2"]/parms_baseline["propWorkers"])*100, post="%"),
                                    sliderInput(inputId = "bl_ld2_compliance", label = "Peak compliance:",
                                                min = 30,  max = 100, value = (1-parms_baseline["fNC2"])*100, post="%"),

                                  ),
                                  hr(class = "dot"),
                                  #----- Mask wearing inputs ----------------------
                                  radioButtons(inputId = "bl_mask", "Compulsory public mask wearing?",
                                               choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["mask"])),
                                  conditionalPanel(
                                    condition="input.bl_mask == 'TRUE'",
                                    sliderInput(inputId = "bl_mask_dates", label = "Implementation period:",
                                                min = start_date,  max=end_date,
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
                                  #----- Lab testing inputs ----------------------
                                  radioButtons(inputId = "bl_lab", "Laboratory testing?",
                                               choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["lab"])),
                                  conditionalPanel(
                                    condition="input.bl_lab == 'TRUE'",
                                    sliderInput(inputId = "bl_lab_dates", label = "Implementation period:",
                                                min = start_date,  max=end_date,
                                                value = c(start_date+parms_baseline[c("lab_start","lab_end")]),
                                                timeFormat = "%d %b %y",step=1,ticks=F),
                                    numericInput(inputId = "bl_lab_improve", label = "Days to scale up:",
                                                 value = parms_baseline["lab_improve"], width = "100%", min=0),
                                    sliderInput(inputId = "bl_lab_capacity", label = "Full capacity (tests/day):",
                                                min = 0, max = 100000, value = parms_baseline["capacity_lab"],
                                                step = 100),
                                    sliderInput(inputId = "bl_lab_fneg", label = "False negatives:",
                                                min = 0, max = 1, value = parms_baseline["lab_fneg"]),
                                    numericInput(inputId = "bl_lab_cost", label = "Cost per Lab test:",
                                                     value = parms_baseline["lab_cost"])
                                  ),
                                  hr(class = "dot"),
                                  #----- Community inputs ------------------------
                                  radioButtons(inputId = "bl_syndromic", "Community Support Team response (household quarantine & rapid testing)?",
                                               choices = list("Yes" = TRUE,"No" = FALSE),inline=TRUE, selected=as.logical(parms_baseline["syndromic"])),
                                  conditionalPanel(
                                    condition="input.bl_syndromic == 'TRUE'",
                                    sliderInput(inputId = "bl_syn_dates", label = "Implementation period:",
                                                min = start_date,  max=end_date,
                                                value = c(start_date+parms_baseline[c("syn_start","syn_end")]),
                                                timeFormat = "%d %b %y",step=1,ticks=F),
                                    numericInput(inputId = "bl_syn_improve", label = "Days to scale up:",
                                                 value = parms_baseline["syn_improve"], width = "100%", min=0),
                                    sliderInput(inputId = "bl_community", label = "Quarantine adherence:",
                                                min = 0,  max = 100, value = parms_baseline["community"]*100, post="%"),
                                    sliderInput(inputId = "bl_rapid_capacity", label = "Rapid tests (/day):",
                                                min = 0, max = 200000, value = parms_baseline["capacity_rapid"],
                                                step = 500),
                                    sliderInput(inputId = "bl_rapid_fneg", label = "False negatives:",
                                                min = 0, max = 1, value = parms_baseline["rapid_fneg"]),
                                    numericInput(inputId = "bl_RDT_cost", label = "Cost per RDT:",
                                                     value = parms_baseline["rapid_cost"])

                                  )
                                )
                       ),
                       #----- Add tab for upazilas -----------------------------
                       tabPanel("2021",
                                br(),
                                h6(HTML("Adjust R<sub>0</sub> and intervention settings for the 'Forecast 2021' tab.")),
                                br(),
                                
                                #----- R0 -------------------------
                                sliderInput(inputId = "upa_R0", label = HTML(paste("R", tags$sub(0), sep = "")),
                                            min = minR0,  max = 6.5, value = 2.8, step = 0.01),
                                h6("Defaults to 2.80. This value was tuned to data for Dhaka District in March 2021, assuming 25% of the population was already immune and 19086 initial infectious."),
                                hr(class = "dot"),
                                #----- Lockdown inputs -------------------------
                                radioButtons(inputId = "upa_ld", "Include a lockdown phase?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=TRUE),
                                conditionalPanel(
                                  condition="input.upa_ld == 'TRUE'",
                                  sliderInput(inputId = "upa_ld_dates", label = "Lockdown period:",
                                              min = start_date_upa,  max = start_date_upa + 120,timeFormat = "%d %b %y",step=1,ticks=F,
                                              value = c(as.Date("2021-04-05"),as.Date("2021-06-01"))),
                                  numericInput(inputId = "upa_ld_improve", label = "Days to full effectiveness:",
                                               value = 9, width = "100%", min=0),
                                  sliderInput(inputId = "upa_fEW", label = "% workforce not under lockdown:",
                                              min = 0,  max = 100, value = (parms_baseline["fEW"]/parms_baseline["propWorkers"])*100, post="%"),
                                  sliderInput(inputId = "upa_ld_compliance", label = "Peak compliance:",
                                              min = 30,  max = 100, value = (1-0.2)*100, post="%")
                                ),
                                hr(class = "dot"),
                                radioButtons(inputId = "upa_ld2", "Add a second lockdown phase?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=as.logical(parms_baseline["ld2"])),
                                conditionalPanel(
                                  condition="input.upa_ld2 == 'TRUE'",
                                  sliderInput(inputId = "upa_ld2_dates", label = "Implementation period:",
                                              min = start_date_upa+30,  max = start_date_upa + 120,timeFormat = "%d %b %y",step=1,ticks=F,
                                              value = c(start_date_upa+30,start_date_upa+60)),
                                  numericInput(inputId = "upa_ld2_improve", label = "Days to full effectiveness:",
                                               value = parms_baseline["ld2_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "upa_fEW2", label = "% workforce not under lockdown:",
                                              min = 0,  max = 100, value = (parms_baseline["fEW2"]/parms_baseline["propWorkers"])*100, post="%"),
                                  sliderInput(inputId = "upa_ld2_compliance", label = "Peak compliance:",
                                              min = 30,  max = 100, value = (1-parms_baseline["fNC2"])*100, post="%"),
                                  
                                ),
                                hr(class = "dot"),
                                #----- Mask wearing inputs ----------------------
                                radioButtons(inputId = "upa_mask", "Compulsory public mask wearing?",
                                             choices = list("Yes" = TRUE,"No" = FALSE), inline=TRUE, selected=TRUE),
                                conditionalPanel(
                                  condition="input.upa_mask == 'TRUE'",
                                  sliderInput(inputId = "upa_mask_dates", label = "Implementation period:",
                                              min = start_date_upa,  max = start_date_upa + 120,timeFormat = "%d %b %y",step=1,ticks=F,
                                              value = c(as.Date("2021-04-05"),as.Date("2021-06-01"))),
                                  numericInput(inputId = "upa_mask_improve", label = "Days to scale up:",
                                               value = parms_baseline["mask_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "upa_mask_compliance", label = "Compliance:",
                                              min = 0,  max = 100, value = 0.3*100, post="%"),
                                  sliderInput(inputId = "upa_mask_effect_out", label = "Proportion reduction in transmission from mask-wearers:",
                                              min = 0,  max = 1, value = parms_baseline["mask_effect_outward"]),
                                  sliderInput(inputId = "upa_mask_effect_in", label = "Proportion reduction in transmission to mask-wearers:",
                                              min = 0,  max = parms_baseline["mask_effect_outward"], value = parms_baseline["f_mask_effect_inward"]*parms_baseline["mask_effect_outward"])
                                  
                                ),
                                hr(class = "dot"),
                                #----- Community inputs ------------------------
                                radioButtons(inputId = "upa_syndromic", "Community Support Team response (household quarantine)?",
                                             choices = list("Yes" = TRUE,"No" = FALSE),inline=TRUE, selected=TRUE),
                                conditionalPanel(
                                  condition="input.upa_syndromic == 'TRUE'",
                                  sliderInput(inputId = "upa_syn_dates", label = "Implementation period:",
                                              min = start_date_upa,  max = start_date_upa + 120,timeFormat = "%d %b %y",step=1,ticks=F,
                                              value = c(as.Date("2021-04-05"),as.Date("2021-06-01"))),
                                  numericInput(inputId = "upa_syn_improve", label = "Days to scale up:",
                                               value = parms_baseline["syn_improve"], width = "100%", min=0),
                                  sliderInput(inputId = "upa_community", label = "Quarantine adherence:",
                                              min = 0,  max = 100, value = 0.3*100, post="%")
                                    
                                  )
                                )
                     )
        ),

        #----- Create main Panel -----------------------------------------------
        mainPanel(
          tabsetPanel(
            #----- Add tab for Interventions Dhaka -----------------------------------------
            tabPanel("Interventions 2020",
                     br(),
                     #----- Add row for description of content -----------------
                     fluidRow(
                       box(width=10,
                           h5(HTML("Forecasts are shown for <strong>mortality</strong>, severe cases requiring <strong>hospital care</strong>,
                              proportion of <strong>working days lost</strong>, and <strong>case detection</strong> by tests (rapid tests and/or RT-PCR) in Dhaka District. These metrics are shown both as time series and
                              summarized over 12 months (barplots to the right).")),
                           h5(HTML("The costs effectiveness of the intervention scenarios is described in the bottom three barplots. 
                              <strong>Total costs</strong> include costs of implementing the selected interventions (e.g. from advertising, community support teams, mask distribution, and tests) and providing hospital care for COVID-19 patients.
                              The <strong>cost per death averted</strong> is calculated by comparison with a scenario with no interventions implemented. 
                              <strong>% return on investment (ROI)</strong> (in terms of saved healthcare costs) is also estimated relative to no interventions.")),
                           h5("All plots show outputs for both a baseline scenario and a comparison scenario so the impact of interventions can be compared. 
                              The interventions applied can be adjusted via the 'Comparison' and 'Baseline' tabs in the sidebar.")
                              
                             
                       ),
                       box(width=2,
                           HTML("<center><img src='bd_map.png' style='max-width: 100%; height: auto;'></center>")
                           )
                     ),
                     br(),

                     #----- Add row for mortality plots ------------------------
                     fluidRow(
                       box(width=9, title="Excess daily mortality", solidHeader = TRUE,
                           plotOutput("plot1", height="200px")
                       ),

                       box(width=3, title="2020", solidHeader = TRUE,
                           plotOutput("barplot_mortality", height="200px")
                       )
                     ),
                     br(),
                     #----- Add row for hospitalisation plots ----------------
                     fluidRow(
                       box(width=9, title="Hospital Demand", solidHeader = TRUE,
                           plotOutput("plot2", height="200px")
                       ),
                       box(width=3, title="2020", solidHeader = TRUE,
                           plotOutput("barplot_hosp", height="200px")
                       )
                     ),
                     br(),
                     #----- Add row for economic loss plots ------------------
                     fluidRow(
                       box(width=9, title="Working days lost", solidHeader = TRUE,
                           plotOutput("wdl_plot", height="200px")
                       ),
                       box(width=3, title="2020", solidHeader = TRUE,
                           plotOutput("barplot_wdl", height="200px")
                       )
                     ),
                     br(),
                     #----- Add row for case detection plot --------------------
                     fluidRow(
                       box(width=9, title = "Cases & detection", solidHeader = TRUE,
                           plotOutput("case_detection", height="200px")
                       ),
                       box(width=3, title="2020", solidHeader = TRUE,
                           plotOutput("barplot_testing", height="200px")
                       )
                     ),
                     br(),
                     #----- Add row for cost plot --------------------
                     fluidRow(
                       box(width=3, title = "Total Costs", solidHeader = TRUE,
                           plotOutput("costs", height="200px")
                       ),
                       box(width=3, title="Cost/Death Averted", solidHeader = TRUE,
                           plotOutput("costs_deaths_averted", height="200px")
                       ),
                       box(width=3, title="%ROI", solidHeader = TRUE,
                           plotOutput("ROI", height="200px")
                       )
                       
                     ),
                     br()
            ),
            #----- Add tab for Forecast Upazilas -----------------------------------------
            tabPanel("Forecast 2021",
                     br(),
                     fluidRow(
                       
                       box(width=12.5,
                           h5("This tab can be used to generate forecasts for 2021 in populations of different sizes and age distributions (defaults represent Dhaka District)."),
                           h5("The number of days to forecast following 1st March 2021 (beginning of the third COVID-19 wave) can be selected (minimum 30), 
                              along with starting infections and the percentage of the population immune (from prior infection or vaccination) at the forecast start.
                              Interventions applied during the forecast period can be selected in the 2021 tab of the sidebar. 
                              Once selected, inputs need to be confirmed and the model run using the grey button below. The parameters that can be adjusted in this tab
                               can be returned to their default settings by pressing the 'Reset defaults' button"),
                           h5("The proportions of cases that are symptomatic, hospitalised and fatal increase as the 
                              average age of the population increases. The age distribution can be adjusted at the bottom of this tab.")
                           
                       )
                     ),
                       #----- Add row for additional inputs ----------------------
                     
                      br(),
                     fluidRow(
                       box(width=12.5, solidHeader=TRUE,
                           splitLayout(
                             numericInput(inputId = "upa_days", label = h4(HTML("<strong>Number of days to forecast from 1st March</strong>")), min=30, max = 365,
                                          value = 120, width = "300px"),
                        
                             # h5("Defaults to the estimated population of Dhaka District in 2020."),
                             numericInput(inputId = "upa_pop", label = h4(HTML("<strong>Population size</strong>")),
                                          value = parms_baseline["population"], width = "300px", min=0)
                             
                           ),
                           splitLayout(
                             h5(""),
                             h5("Defaults to the estimated population of Dhaka District in 2020.")
                             
                           ),
                           
                           br(),br(),
                           
                           splitLayout(
                             
                             numericInput(inputId = "upa_infectious", label = h4(HTML("<strong>Number infectious at start of forecast</strong>")), min=0, max = 2000000,
                                          value = 19086, width = "300px"),
                             sliderInput(inputId = "upa_immune", label = h4(HTML("<strong>% population immune at start of forecast</strong>")),
                                         min = 0, max = 100, value = 25, post="%")
                           ),
                           br(),br(),
                           splitLayout(
                             actionBttn(inputId="upa_go", label = "Confirm inputs and run model",
                                        style="minimal"),
                             actionBttn(inputId="upa_reset", label = "Reset defaults",
                                        style="minimal")
                           ),
                           span(textOutput("demog_warning"), style="color:red")
                           
                           
                       )
                       
                     ), 
                      
                     br(),
                     #----- Add row for SEIR timeseries plot--------------------
                     fluidRow(
                       box(width=12.5, title="Forecast", solidHeader = TRUE,
                           plotOutput("epi_ts_upazila", height = "350px")
                       ),
                       br(),
                       box(width=12.5, title="Forecast deaths", solidHeader = TRUE,
                           plotOutput("death_ts_upazila", height = "350px")
                       )
                     ),
                     br(),
                     #----- Add row for age distribution --------------------
                     fluidRow(
                       box(width=12.5, solidHeader = TRUE,
                           h4(HTML("<strong>Age distribution</strong>")),
                           h5("Defaults to the overall age distribution for Dhaka District estimated by the 2011 census."),
                           br(),
                           splitLayout(
                             numericInput(inputId = "upa_demog_1", label = "% Age 0-9:",
                                          value = round(dhaka_pop_by_age$prop[1]*100,1), width = "150px", min=0,max=100),
                             numericInput(inputId = "upa_demog_2", label = "% Age 10-19:",
                                          value = round(dhaka_pop_by_age$prop[2]*100,1), width = "150px", min=0,max=100),
                             numericInput(inputId = "upa_demog_3", label = "% Age 20-29:",
                                          value = round(dhaka_pop_by_age$prop[3]*100,1), width = "150px", min=0,max=100)
                           ),
                           splitLayout(
                             numericInput(inputId = "upa_demog_4", label = "% Age 30-39:",
                                          value = round(dhaka_pop_by_age$prop[4]*100,1), width = "150px", min=0,max=100),
                             numericInput(inputId = "upa_demog_5", label = "% Age 40-49:",
                                          value = round(dhaka_pop_by_age$prop[5]*100,1), width = "150px", min=0,max=100),
                             numericInput(inputId = "upa_demog_6", label = "% Age 50-59:",
                                          value = round(dhaka_pop_by_age$prop[6]*100,1), width = "150px", min=0,max=100)
                           ),
                           splitLayout(
                             numericInput(inputId = "upa_demog_7", label = "% Age 60-69:",
                                          value = round(dhaka_pop_by_age$prop[7]*100,1), width = "150px", min=0,max=100),
                             numericInput(inputId = "upa_demog_8", label = "% Age 70-79:",
                                          value = round(dhaka_pop_by_age$prop[8]*100,1), width = "150px", min=0,max=100),
                             numericInput(inputId = "upa_demog_9", label = "% Age 80+",
                                          value = round(dhaka_pop_by_age$prop[9]*100,1), width = "150px", min=0,max=100)
                           )
                       )
                     ),
                     br(),
                     
            ),
            #----- Add tab for technical details -------------------------------
            tabPanel("Technical Details",
                     br(),
                     #----- Add row for explanation of panels ------------------
                     fluidRow(
                       box(width=12, #title="", solidHeader = TRUE,
                           h5(HTML("We use an SEIR model to explore impacts of control measures on COVID-19 transmission in Dhaka District.",
                                   "R<sub>0</sub> and the impact of lockdown were tuned to match the trend in deaths prior to June 2020 (see early epidemic forecasts below).",
                                   "Other epidemiological and population parameters were obtained from the literature. Parameters and their sources are detailed in the tables below. 
                                   Most parameters describing the interventions, including timing, compliance, and impacts on transmission, can be adjusted in the sidebar tabs."
                                   
                           )),
                           h5(HTML("A detailed description of the model and of how this app has been used in Bangladesh, along with further analyses, can be found in our <a class='table_a' href=https://www.medrxiv.org/content/10.1101/2021.04.19.21255673v1.full.pdf>preprint</a>. 
                                   R code for the model and app can be found in our <a class='table_a' href=https://github.com/boydorr/BGD_Covid-19/tree/main/BGD_NPI_model/App>Github repository</a>."))
                       )
                     ),
                     br(),
                     
                     #----- Add rows for case timeseries plots--------------------
                     fluidRow(
                       box(width=6, title="Forecast vs reports (early epidemic)", solidHeader = TRUE,
                           plotOutput("epi_ts_early", height = "350px")
                       ),
                       box(width=6, title="Forecast vs reports (2020)", solidHeader = TRUE,
                           plotOutput("epi_ts", height = "350px")
                       )
                     ),
                     br(),
                     fluidRow(
                       box(width=6, title="Forecast vs reported deaths (early epidemic)", solidHeader = TRUE,
                           plotOutput("death_ts_early", height = "350px")
                       ),
                       box(width=6, title="Forecast vs reported deaths (2020)", solidHeader = TRUE,
                           plotOutput("death_ts", height = "350px")
                       )
                     ),
                     br(),
                     #----- Add row for model schematic table --------------------
                     fluidRow(
                       box(width=12, title="Model Schematic", solidHeader = TRUE,
                           HTML('<center><img src="model_schematic.png"></center>')
                       )
                     ),
                     br(),
                     #----- Add row for model details table --------------------
                     fluidRow(
                       box(width=12, title="Epidemiological Parameters", solidHeader = TRUE,
                           dataTableOutput("param_table")  , style = "height:500px; overflow-y: scroll;" # add scroll bar
                       )
                     ),
                     br(),
                     fluidRow(
                       box(width=12, title="Intervention/Testing Parameters", solidHeader = TRUE,
                           dataTableOutput("int_param_table")  , style = "height:500px; overflow-y: scroll;" # add scroll bar
                       )
                     ),
                     br(),
                     fluidRow(
                       box(width=12, title="Age-dependent severity of infection", solidHeader = TRUE,
                           dataTableOutput("age_dep_param_table")  #, style = "height:500px; overflow-y: scroll;" # add scroll bar
                       )
                     ),
                     br(),
                     fluidRow(
                       box(width=12, title="Population parameters", solidHeader = TRUE,
                           dataTableOutput("pop_param_table")  #, style = "height:500px; overflow-y: scroll;" # add scroll bar
                       )
                     ),
                     br(),br(),
                     #----- Add row for 'secret' y-axis checkbox ---------------
                     fluidRow(
                       div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                       div(style="display: inline-block;vertical-align:top; width: 250px;",
                               checkboxInput(inputId = "show_y_axis", label="Show y-axis on all plots?",
                                     value=FALSE))
                     ),
                     br()
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
                                of movement between states are detailed in the technical details tab, informed by published studies."),
                             h5("We assume that transmission can be reduced through response measures, but the size of the reduction depends 
                                on how well interventions are implemented, adhered to and enforced. i.e. as a result of investment, technological and sociological capacity, communications, trust etc."),
                             h5("The timing and duration of the epidemic and associated responses will have major impacts on morbidity,
                                mortality and the economy, with potential to overwhelm health systems and cause catastrophic
                                consequences for families, communities and society as a whole. These knock on effects are beyond the scope of
                                our model. However, we attempt to lay out short-term impacts on morbidity and hospital demand relative to capacity.
                                Immediate economic costs, from intervention implementation, testing, and healthcare provision, and from
                                loss of work due to illness or restrictions, are also explored.
                                We summarize these impacts over 12 months to better understand longer-term consequences of decisions that
                                need to be taken quickly. We further calibrate the model to the resurgence in 2021 to better understand 
                                factors (R0 of new variants, prior immunity, NPIs) that led to the resurgence and the potential for control measures 
                                to mitigate impacts."),
                             h5("There is considerable uncertainty in quantitative predictions therefore we focus on order of magnitude
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


