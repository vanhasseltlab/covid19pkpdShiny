library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
library(shinyjs)
library(V8)
library(shinyhelper)
library(dplyr)

# javascript code to collapse box
jscode <- "shinyjs.collapse = function(boxid) {
           $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
          }"

body <- dashboardBody(
  useShinyjs(),
  extendShinyjs(text = jscode),
  
  useShinyalert(),
  
  fluidRow(setSliderColor(rep("#001158",1000), 1:1000),
           # set shadow for boxes
           setShadow("box"),
           setShadow("Button"),
           
           tags$head(includeHTML("gtag.html")),
           
           tags$head(tags$style(HTML(".skin-blue .main-header > .logo { background-color: #001158;
                                     font-weight: bold;
                                     font-size: 28px}
                                     .skin-blue .main-header .logo:hover {background-color: #001158;}
                                     
                                     .skin-blue .main-header .navbar { background-color: #001158;} 
                                     
                                     .box-primary .box-header>.logo{
                                     font-weight: bold;}
                                     
                                     .nav-tabs-custom .nav-tabs li.active {
                                     border-top-color: #001158;
                                     }
                                     .box.box-solid.box-primary>.box-header {
                                     color: #fff;
                                     background: #001158;}
                                     
                                     .box.box-solid.box-primary{
                                     border-bottom-color: #ccc;
                                     border-left-color: #ccc;
                                     border-right-color: #ccc;
                                     border-top-color: #ccc;
                                     
                                     }
                                     .box.box-solid.box-success>.box-header {background: #f46e32}
                                     .box.box-solid.box-success{
                                     border-bottom-color: #ccc;
                                     border-left-color: #ccc;
                                     border-right-color: #ccc;
                                     border-top-color: #ccc;
                                     
                                     }
                                     #titleID0{background-color:#001158}
                                     #titleID1{background-color:#001158}
                                     #titleID2{background-color:#001158}
                                     #titleID3{background-color:#001158}
                                     #titleID4{background-color:#001158}
                                     #titleID5{background-color:#001158}
                                     #titleID6{background-color:#001158}
                                     "))),
    column(width = 4,
           
           box(width = NULL,
               id = "box5",
               collapsible = TRUE,
               #collapsed = TRUE,
               status = "primary",
               solidHeader = TRUE,
               title = actionLink("titleID5",span(icon("user"),span("Patient characteristics",style = "font-weight:bold;font-size:18px"))),
               numericInput("bw", "Body weight (kg):", 75, min = 1, max = 200) %>% 
                 helper(type = "inline",
                        title = "Body weight",
                        content = c("When using weight-based dosing weight is used to calculate the absolute dose.",
                                    "Patient body weight is used to adjust PK simulations for body weight for chloroquine and hydrochloroquine. For lopinavir/ritonavir simulations are not adjusted for body weight, and assume a typical adult body weight."),
                        size = "m", colour = "#001158")
               ),
           box(width = NULL,
               id = "box0",
               collapsible = TRUE,
               #collapsed = TRUE,
               status = "primary",
               solidHeader = TRUE,
               title = actionLink("titleID0",span(icon("pills"),span("Dosage",style = "font-weight:bold;font-size:18px"))),
               pickerInput("drugname", "Drug:",
                           choices = c("Chloroquine","Hydroxychloroquine","Lopinavir + Ritonavir"),
                           options = list(title = "Select a reference drug")) %>% 
                 helper(type = "inline",
                        title = "Drug",
                        content = c("The user can select the drug of interest for which to perform PK/PD simulations."),
                        size = "m", colour = "#001158"),
               conditionalPanel(condition = "input.drugname == 'Chloroquine'",
                                span("Oral Administration",style="font-style: italic"),
                                br(),
                                tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3909452/",target="_blank", "Zhao et al. Malar J.(2014)",style = "font-size:12px;font-style: italic;")),
               conditionalPanel(condition = "input.drugname == 'Hydroxychloroquine'",
                                span("Oral Administration",style="font-style: italic"),
                                br(),
                                span("Hydroxychloroquine sulfate",style="font-style: italic"),
                                br(),
                                tags$a(href="https://aac.asm.org/content/53/4/1468.long",target="_blank", "Lim H S et al. AAC(2009)",style = "font-size:12px;font-style: italic;")),
               conditionalPanel(condition = "input.drugname == 'Lopinavir + Ritonavir'",
                                span("Oral Administration",style="font-style: italic"),
                                br(),
                                tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1884825/",target="_blank", "Crommentuyn KM et al. BJCP(2005)",style = "font-size:12px;font-style: italic;")),
               #awesomeRadio("route","Select administration route:",choices = c("i.v. bolus","oral"), selected = "i.v. bolus",inline = TRUE),
               awesomeRadio("free","Plasma concentration:", choices = c("total","free"),selected = "total",inline = TRUE) %>% 
                 helper(type = "inline",
                        title = "Plasma concentrations",
                        content = c("Either total or unbound plasma concentrations can be displayed."),
                        size = "m", colour = "#001158"),
               awesomeRadio("bwd","Dosing based on body weight:",choices = c("yes","no"),selected = "no",inline = TRUE),
               #textInput("bw","Body weight (kg):", value = 75),
               ######################################################################
               
               conditionalPanel(condition = "input.drugname == 'Chloroquine'",
                                #------------Loading Dose of Chloroquine---------------
                                fluidRow(align ="center",span("Loading Dose of Chloroquine",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")) %>% 
                                  helper(type = "inline",
                                         title = "Dosing regimens",
                                         content = c("Dosing amount, interval and duration can be specified for an initial loading dose and a subsequent maintenance dose."),
                                         size = "m", colour = "#001158"),
                                conditionalPanel(condition = "input.bwd =='yes'",
                                                 sliderInput("amt1_1_1","Amount (mg/kg):", min = 0, max = 10, value = 5, step = 1)),
                                conditionalPanel(condition = "input.bwd =='no'",
                                                 sliderInput("amt1_2_1","Amount (mg):", min = 0, max = 800, value = 300, step = 50)),
                                sliderInput("D1_1","Duration (h):",min = 12, max = 48, value = 24,step = 12),
                                sliderTextInput("ii1_1","Dosing Interval (h):",choices = c(3,6,12,24),selected=24,grid = TRUE),
                                #------------Maintenance Dose of Chloroquine-----------------
                                fluidRow(align ="center",span("Maintenance Dose of Chloroquine",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")),
                                conditionalPanel(condition = "input.bwd =='yes'",
                                                 sliderInput("amt2_1_1","Amount (mg/kg):", min = 0, max = 10, value = 0, step = 1)),
                                conditionalPanel(condition = "input.bwd =='no'",
                                                 sliderInput("amt2_2_1","Amount (mg):", min = 0, max = 800, value = 0, step = 50)),
                                sliderInput("D2_1","Duration (day):",min = 0, max = 6, value = 6,step = 1),
                                sliderTextInput("ii2_1","Dosing Interval (h):",choices = c(3,6,8,12,24),selected=24,grid = TRUE),
                                
                                conditionalPanel(condition = "input.D1_1 < input.ii1_1",
                                                 span("Warning:",style="font-style: italic; color:#D62728;font-weight:bold"),
                                                 br(),
                                                 span("Duration of Loading dose should be greater than the dosing interval!",style="font-style: italic; color:#D62728")
                                                 )
                                ),
               
               conditionalPanel(condition = "input.drugname == 'Hydroxychloroquine'",
                                #------------Loading Dose of Hydroxychloroquine---------------
                                fluidRow(align ="center",span("Loading Dose of Hydroxychloroquine",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")) %>% 
                                  helper(type = "inline",
                                         title = "Dosing regimens",
                                         content = c("Dosing amount, interval and duration can be specified for an initial loading dose and a subsequent maintenance dose."),
                                         size = "m", colour = "#001158"),
                                conditionalPanel(condition = "input.bwd =='yes'",
                                                 sliderInput("amt1_1_2","Amount (mg/kg):", min = 0, max = 10, value = 5, step = 1)),
                                conditionalPanel(condition = "input.bwd =='no'",
                                                 sliderInput("amt1_2_2","Amount (mg):", min = 0, max = 800, value = 400, step = 50)),
                                sliderInput("D1_2","Duration (h):",min = 12, max = 48, value = 24,step = 12),
                                sliderTextInput("ii1_2","Dosing Interval (h):",choices = c(3,6,12,24), selected = 12,grid = TRUE),
                                #------------Maintenance Dose of Hydroxychloroquine-----------------
                                fluidRow(align ="center",span("Maintenance Dose of Hydroxychloroquine",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")),
                                conditionalPanel(condition = "input.bwd =='yes'",
                                                 sliderInput("amt2_1_2","Amount (mg/kg):", min = 0, max = 10, value = 1, step = 1)),
                                conditionalPanel(condition = "input.bwd =='no'",
                                                 sliderInput("amt2_2_2","Amount (mg):", min = 0, max = 800, value = 200, step = 50)),
                                sliderInput("D2_2","Duration (day):",min = 0, max = 6, value = 4, step = 1),
                                sliderTextInput("ii2_2","Dosing Interval (h):",choices = c(3,6,8,12,24), selected = 12,grid = TRUE),
                                
                                conditionalPanel(condition = "input.D1_2 < input.ii1_2",
                                                 span("Warning:",style="font-style: italic; color:#D62728;font-weight:bold"),
                                                 br(),
                                                 span("Duration of Loading dose should be greater than the dosing interval!",style="font-style: italic; color:#D62728")
                                )
               ),
               
               conditionalPanel(condition = "input.drugname == 'Lopinavir + Ritonavir'",
                                #------------Loading Dose of Ritonavir---------------
                                fluidRow(align ="center",span("Loading Dose of Ritonavir",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")) %>% 
                                  helper(type = "inline",
                                         title = "Dosing regimens",
                                         content = c("Dosing amount, interval and duration can be specified for an initial loading dose and a subsequent maintenance dose."),
                                         size = "m", colour = "#001158"),
                                conditionalPanel(condition = "input.bwd =='yes'",
                                                 sliderInput("amt1_1_4","Amount (mg/kg):", min = 0, max = 10, value = 1, step = 1)),
                                conditionalPanel(condition = "input.bwd =='no'",
                                                 sliderInput("amt1_2_4","Amount (mg):", min = 0, max = 800, value = 100, step = 50)),
                                #------------Maintenance Dose of Ritonavir-----------------
                                fluidRow(align ="center",span("Maintenance Dose of Ritonavir",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")),
                                conditionalPanel(condition = "input.bwd =='yes'",
                                                 sliderInput("amt2_1_4","Amount (mg/kg):", min = 0, max = 10, value = 1, step = 1)),
                                conditionalPanel(condition = "input.bwd =='no'",
                                                 sliderInput("amt2_2_4","Amount (mg):", min = 0, max = 800, value = 100, step = 50)),
                                #------------Loading Dose of Lopinavir---------------
                                fluidRow(align ="center",span("Loading Dose of Lopinavir",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")),
                                conditionalPanel(condition = "input.bwd =='yes'",
                                                 sliderInput("amt1_1_3","Amount (mg/kg):", min = 0, max = 10, value = 1, step = 1)),
                                conditionalPanel(condition = "input.bwd =='no'",
                                                 sliderInput("amt1_2_3","Amount (mg):", min = 0, max = 800, value = 400, step = 50)),
                                #------------Maintenance Dose of Lopinavir-----------------
                                fluidRow(align ="center",span("Maintenance Dose of Lopinavir",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")),
                                conditionalPanel(condition = "input.bwd =='yes'",
                                                 sliderInput("amt2_1_3","Amount (mg/kg):", min = 0, max = 10, value = 1, step = 1)),
                                conditionalPanel(condition = "input.bwd =='no'",
                                                 sliderInput("amt2_2_3","Amount (mg):", min = 0, max = 800, value = 400, step = 50)),
                                #-----------loading dose frequancy---------------------------
                                fluidRow(align ="center",span("Loading Dose Frequancy",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")),
                                sliderInput("D1_3","Duration (h):",min = 12, max = 48, value = 24,step = 12),
                                sliderTextInput("ii1_3","Dosing Interval (h):",choices = c(3,6,12,24), selected =12,grid = TRUE),
                                #-----------Maintenance dose frequency---------------------------
                                fluidRow(align ="center",span("Maintenance Dose Frequancy",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")),
                                sliderInput("D2_3","Duration (day):",min = 0, max = 6, value = 4,step = 1),
                                sliderTextInput("ii2_3","Dosing Interval (h):",choices = c(3,6,8,12,24), selected =12,grid = TRUE),
                                
                                conditionalPanel(condition = "input.D1_3 < input.ii1_3",
                                                 span("Warning:",style="font-style: italic; color:#D62728;font-weight:bold"),
                                                 br(),
                                                 span("Duration of Loading dose should be greater than the dosing interval!",style="font-style: italic; color:#D62728")
                                )
               ),
               
               div(style="text-align: center;",actionButton("plot", span("Run Simulation",style = "font-weight:bold;font-size:16px"),style = "background-color: #f46e32; color: white;"))
               
    ),
    box(width = NULL,
        id = "box2",
        collapsible = TRUE,
        #collapsed = TRUE,
        status = "primary",
        solidHeader = TRUE,
        title = actionLink("titleID2",span(icon("flask"),span("EC50",style = "font-weight:bold;font-size:18px"))),
        conditionalPanel(condition = "input.drugname == 'Chloroquine'",
                         pickerInput("ec50_1", "Select EC50 for Chloroquine:",
                                     choices = c("SARS-cov-2 1.75ug/ml (Yao et al. CID(2020))",
                                                 "SARS-cov-2 0.36ug/ml (Wang et al. Cell Res(2020))",
                                                 "SARS-cov-2 3.67ug/ml (Jeon et al. bioRxiv(2020))",
                                                 "SARS-cov-1 2.84ug/ml (Keyaerts E et al. BBRC(2004))",
                                                 "SARS-cov-1 1.43ug/ml (Vincent MJ et al. Virol J(2005))",
                                                 "SARS-cov-1 1.33ug/ml (de Wilde AH et al. AAC(2014))",
                                                 "Mers-cov 0.95ug/ml (de Wilde AH et al. AAC(2014))"),
                                     selected = "SARS-cov-2 1.75ug/ml (Yao et al. CID(2020))"),
                         conditionalPanel(condition = "input.ec50_1 == 'SARS-cov-2 1.75ug/ml (Yao et al. CID(2020))'",
                                          tags$a(href="https://academic.oup.com/cid/advance-article/doi/10.1093/cid/ciaa237/5801998",target="_blank", "Yao et al. CID(2020)",style = "font-size:12px;font-style: italic;")),
                         conditionalPanel(condition = "input.ec50_1 == 'SARS-cov-2 0.36ug/ml (Wang et al. Cell Res(2020))'",
                                          tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7054408/",target="_blank", "Wang et al. Cell Res(2020)",style = "font-size:12px;font-style: italic;")),
                         conditionalPanel(condition = "input.ec50_1 == 'SARS-cov-2 3.67ug/ml (Jeon et al. bioRxiv(2020))'",
                                          tags$a(href="https://doi.org/10.1101/2020.03.20.999730",target="_blank", "Jeon et al. bioRxiv(2020)",style = "font-size:12px;font-style: italic;")),
                         conditionalPanel(condition = "input.ec50_1 == 'SARS-cov-1 2.84ug/ml (Keyaerts E et al. BBRC(2004))'",
                                          tags$a(href="https://www.sciencedirect.com/science/article/pii/S0006291X0401839X?via%3Dihub",target="_blank", "Keyaerts E et al. BBRC(2004)",style = "font-size:12px;font-style: italic;")),
                         conditionalPanel(condition = "input.ec50_1 == 'SARS-cov-1 1.43ug/ml (Vincent MJ et al. Virol J(2005))'",
                                          tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1232869/",target="_blank", "Vincent MJ et al. Virol J(2005)",style = "font-size:12px;font-style: italic;")),
                         conditionalPanel(condition = "input.ec50_1 == 'SARS-cov-1 1.33ug/ml (de Wilde AH et al. AAC(2014))'",
                                          tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4136071/",target="_blank", "de Wilde AH et al. AAC(2014)",style = "font-size:12px;font-style: italic;")),
                         conditionalPanel(condition = "input.ec50_1 == 'Mers-cov 0.95ug/ml (de Wilde AH et al. AAC(2014))'",
                                          tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4136071/",target="_blank", "de Wilde AH et al. AAC(2014)",style = "font-size:12px;font-style: italic;"))
                         ),
        conditionalPanel(condition = "input.drugname == 'Hydroxychloroquine'",
                         pickerInput("ec50_2", "Select EC50 for Hydroxychloroquine:",
                                     choices = c("SARS-cov-2 0.24ug/ml (Yao et al. CID(2020))"),
                                     selected = "SARS-cov-2 0.24ug/ml (Yao et al. CID(2020))"),
                         conditionalPanel(condition = "input.ec50_2 == 'SARS-cov-2 0.24ug/ml (Yao et al. CID(2020))'",
                                          tags$a(href="https://academic.oup.com/cid/advance-article/doi/10.1093/cid/ciaa237/5801998",target="_blank", "Yao et al. CID(2020)",style = "font-size:12px;font-style: italic;"))
                         ),
        conditionalPanel(condition = "input.drugname == 'Lopinavir + Ritonavir'",
                         pickerInput("ec50_3", "Select EC50 for Lopinavir:",
                                     choices = c("SARS-cov-2 5.73ug/ml (Jeon et al. bioRxiv(2020))",
                                                 "SARS-cov-1 4ug/ml (Chu CM et al. Thorax(2004))",
                                                 "SARS-cov-1 2.52ug/ml (de Wilde AH et al. AAC(2014))",
                                                 "Mers-cov 4.4ug/ml (de Wilde AH et al. AAC(2014))"),
                                     selected = "SARS-cov-2 5.73ug/ml (Jeon et al. bioRxiv(2020))"),
                         conditionalPanel(condition = "input.ec50_3 == 'SARS-cov-2 5.73ug/ml (Jeon et al. bioRxiv(2020))'",
                                          tags$a(href="https://doi.org/10.1101/2020.03.20.999730",target="_blank", "Jeon et al. bioRxiv(2020)",style = "font-size:12px;font-style: italic;")),
                         conditionalPanel(condition = "input.ec50_3 == 'SARS-cov-1 4ug/ml (Chu CM et al. Thorax(2004))'",
                                          tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1746980/",target="_blank", "Chu CM et al. Thorax(2004)",style = "font-size:12px;font-style: italic;")),
                         conditionalPanel(condition = "input.ec50_3 == 'Mers-cov 4.4ug/ml (de Wilde AH et al. AAC(2014))'| input.ec50_3 == 'SARS-cov-1 2.52ug/ml (de Wilde AH et al. AAC(2014))'",
                                          tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4136071/",target="_blank", "de Wilde AH et al. AAC(2014)",style = "font-size:12px;font-style: italic;"))
        )
        )

    ),
    box(width = NULL,
        id = "box6",
        collapsible = TRUE,
        #collapsed = TRUE,
        status = "primary",
        solidHeader = TRUE,
        title = actionLink("titleID6",span(icon("user-circle"),span("User Information",style = "font-weight:bold;font-size:18px"))),
        textInput("name", "Name:", placeholder = "Name"),
        textInput("aff", "Affiliation:", placeholder = "Affiliation"),
        textInput("email", "Email address:", placeholder = "Email address"),
        div(style="text-align: center;",actionButton("submit", span("Submit",style = "font-weight:bold;font-size:16px"),style = "background-color: #001158; color: white;")),
        div(style="text-align: center;",span(textOutput("thanks"),style = "font-style: italic; color:#D62728"))
    )
  ),
  
    column(width = 4,
           box(width = NULL, height = 660,
               id = "box3", collapsible = TRUE, 
               plotOutput("PK",height="400px") %>% 
                 helper(type = "inline",
                        title = "Pharmacokinetics",
                        content = c("This figure shows the median (solid line) and 95% prediction intervals for either total or unbound drug concentrations in plasma, and predicted concentrations in lung tissue, simulated for 100 patients.",
                                    "The predicted lung concentrations are based on lung-to-plasma partitioning data from rats.",
                                    "The blue horizontal line indicate the in vitro EC50 value for viral inhibition.",
                                    "The red horizontal line indicates the maximum tolerated dose concentration, which is based on the Upper 95% maximum concentrations for the highest dosing schedules available in the drug label.",
                                    "",
                                    "Dosing schedules for MTD:",
                                    "1. CQ: 600 mg/day (base) for 2 days, then 300 mg/day (base) for 14 days",
                                    "2. HCQ: 465 mg/day (base) for 14 days (which is 600 mg/day (sulfate))",
                                    "3. LPV/r: 800 mg LPV - 200 mg RTV for 14 days"),
                        size = "l", colour = "#001158"), 
               title = actionLink("titleID3",span(icon("prescription-bottle-alt"),span("Pharmacokinetics",style = "font-weight:bold"))),
               status = "primary", solidHeader = TRUE)
           ),
    column(width = 4,
           box(width = NULL, height = 660,
               id = "box4", collapsible = TRUE, 
               plotOutput("PD",height="400px") %>% 
                 helper(type = "inline",
                        title = "Pharmacodynamics",
                        content = c("The drug trough concentrations in plasma and lung tissue per dose interval are calculated and scaled by the in vitro EC50 value from viral inhibition assays. The higher the trough/EC50 ratio, the more likely is expected efficacy.",
                                    "The MTD heatmap indicated the amount (%) of patients that are above the plasma-based maximum tolerated dose concentrations."),
                        size = "m", colour = "#001158"), 
               title = actionLink("titleID4",span(icon("prescription-bottle-alt"),span("Pharmacodynamics",style = "font-weight:bold"))), 
               status = "primary", solidHeader = TRUE)
    )
           
  ),
  #--------------------------------------------- The Bottom ------------------------------------------
  fluidRow(align = "center",
           span(
                "Developed by ",
                tags$a(href="https://www.universiteitleiden.nl/en/staffmembers/yu-fu#tab-1",target="_blank", "Yu Fu"),",",
                tags$a(href="https://www.universiteitleiden.nl/en/staffmembers/linda-aulin#tab-1",target="_blank", "Linda Aulin"),",",
                tags$a(href="https://www.universiteitleiden.nl/en/staffmembers/jinqiu-yin#tab-1",target="_blank", "Jinqiu Yin"),",",
                tags$a(href="https://www.universiteitleiden.nl/en/staffmembers/feiyan-liu#tab-1",target="_blank", "Feiyan Liu"),",",
                tags$a(href="https://www.researchgate.net/profile/Pyry_Vaelitalo",target="_blank", "Pyry Välitalo"),",",
                tags$a(href="https://www.universiteitleiden.nl/en/staffmembers/parth-upadhyay#tab-1",target="_blank", "Parth Upadhyay"),",",
                tags$a(href="https://www.universiteitleiden.nl/en/staffmembers/coen-van-hasselt#tab-1",target="_blank", "J.G.C. van Hasselt"),
                br(),
                tags$a(href="http://vanhasseltlab.nl/",target="_blank", "van Hasselt Group"),
                style = "font-size:18px;font-style: italic"),
           HTML("<p style='color:#D62728; font-size: 12pt'><br><b>DISCLAIMER:</b><br>
                                      <b>This app is intended to guide and evaluate potential dosing schedules to be evaluated in clinical trials investigating potential drugs against COVID19. 
                                      <b>This app should not be used to select dosing schedules for routine patient care.
                                      <b>The use of the COVID-19 PKPD Simulator is in all cases at the sole risk and responsibility of the user. 
                                      <b>Although we have carefully validated this application, there is no guarantee for the accuracy of the provided results.</b>
                               ")),
  fluidRow(align = "center",
           img(src = "LU_logo.png", height = 120),
           img(src = "LACDR_logo.png",height = 80))
  )

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "COVID-19 PKPD Simulator",
                                    #tags$li(class = "dropdown",
                                           # actionBttn("modelinfo",
                                                       #icon = icon("info-circle"),
                                                       #style = "bordered",size = "sm"),
                                           # style = "padding-top:8px; padding-bottom:8px;padding-right:8px"),
                                    #tags$li(class = "dropdown",
                                            #dropdown(icon = icon("cog"),
                                                  #   style = "bordered",size = "sm", 
                                                   #  tooltip = tooltipOptions(placement = "bottom",title = "Click here to change units!"),
                                                   #  awesomeRadio("timeunit","Time Unit:",choices = c("hour","day","week"), selected = "day", inline = TRUE),
                                                   #  awesomeRadio("concunit","Conc. Unit:",choices = c("mg/ml","ug/ml","ng/ml"), selected = "ug/ml", inline = TRUE)
                                           # ),
                                           # style = "padding-top:8px; padding-bottom:8px;padding-right:80px"),
                                    #tags$li(class = "dropdown", downloadBttn("report", 
                                                                        #     span("Generate Report",style = "font-weight:bold;color:#fff"),
                                                                           #  size = "sm",style = "bordered"),
                                           # style = "padding-top:8px; padding-bottom:8px;padding-right:10px"),
                                    
                                    titleWidth = "400px"),
                    dashboardSidebar(disable = TRUE),
                    body
)