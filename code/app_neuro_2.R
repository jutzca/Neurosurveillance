# -----------------------------------------------------------------------------------
# Building a Shiny app for visualisation of neurological outcomes in SCI
#
# July 8, 2020
# L. Bourguignon
# -----------------------------------------------------------------------------------

# Set working directory ----

# Load packages ----
library(rsconnect)
library(shiny)
library(shinyWidgets) 
library(shinydashboard)
library(ggplot2)
library(data.table)
library(assertthat)
library(plyr)
library(dplyr)
library('ggthemes') 
library(ggpubr)
library(ggridges)
library(gridExtra)

library(sjPlot)
library(jtools)
library(reshape2)
library(PMCMRplus)

#library(epicalc)
library(EpiReport)
library(epiDisplay)
library(naniar)

library(boot)
library(table1)
library(broom)
#library(pander)
library(gtable)
library(grid)
library(tidyr)
library(Hmisc)
library(RColorBrewer)
library(lme4)

library(DT)
library(shinyjs)
library(sodium)
library(shinymanager)
library(shinyalert)

library(plotly)


# Source helper functions -----
source("helper_functions_3.R")

# Load data ----
data_emsci <- read.csv('data/df_emsci_formatted2.csv')
data_emsci$ExamStage <- as.factor(data_emsci$ExamStage)
data_emsci$ExamStage <- relevel(data_emsci$ExamStage, ref = "very acute")
data_sygen <- read.csv('data/df_sygen_formatted_3.csv')
data_SCI_rehab <- read.csv('data/df_rehab_formatted.csv')
data_All <- read.csv('data/df_all_formatted.csv')
data_emsci_sygen <- read.csv('data/df_emsci_sygen_formatted.csv')
data_age_emsci <- read.csv('data/emsci_age.csv')
data_emsci_epi <- read.csv('data/emsci.csv')
data_emsci_epi$ExamStage <- as.factor(data_emsci_epi$ExamStage)
data_emsci_epi$ExamStage <- relevel(data_emsci_epi$ExamStage, ref = "very acute")
data_sygen_epi <- read.csv('data/sygen_epi.csv')
data_SCI_rehab_epi <- read.csv('data/df_rehab_epi.csv')

# Functions ----

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}



latest.DateTime <- file.info("app_neuro_2.R")$mtime

# User interface ----
ui <- dashboardPage(skin = "blue", # make the frame blue
      dashboardHeader(title = img(src="neurosurveillance_logo.png", height="80%", width="80%")), # rename the left column 'Menu'
      
      ## Sidebar content
      dashboardSidebar(
        sidebarMenu(id = "sidebarmenu", # create the main and sub parts within the sidebar menu
                    menuItem("About", tabName = "AboutTab", icon = icon("info-circle")),
                    menuItem("User guide", tabName = "user_guide", icon = icon("book-reader")),
                    menuItem('EMSCI', tabName = 'emsci', icon=icon("database"),
                             menuSubItem("Study details", tabName = "about_emsci", icon = icon("info-circle")),
                             menuSubItem("Epidemiological features", tabName = "epi_emsci", icon = icon("users")), 
                             menuSubItem("Neurological features", tabName = "neuro_emsci", icon = icon("user-check")),
                             menuSubItem("Functional features", tabName = "funct_emsci", icon = icon("accessible-icon")),
                             menuSubItem("Monitoring", tabName = "monitore_emsci", icon = icon("clipboard-list"))),
                    menuItem('Sygen Trial', tabName = 'sygen', icon = icon("hospital-user"), 
                             menuSubItem("Study details", tabName = "about_sygen", icon = icon("info-circle")),
                             menuSubItem("Epidemiological features", tabName = "epi_sygen", icon = icon("users")), 
                             menuSubItem("Neurological features", tabName = "neuro_sygen", icon = icon("user-check")),
                             menuSubItem("Functional features", tabName = "funct_sygen", icon = icon("accessible-icon"))),
                    menuItem('Data sources compared', tabName = 'All', icon = icon("balance-scale"), 
                             menuSubItem("Neurological features", tabName = "neuro_all", icon = icon("user-check"))),
                    menuItem("Abbreviations", tabName = "abbreviations", icon = icon("language")),
                    menuItem(HTML(paste0("Contact for collaborations ", icon("external-link"))), icon=icon("envelope"), href = "mailto:catherine.jutzeler@bsse.ethz.ch"),
                    uiOutput("dynamic_content")
                    ) # end sidebarMenu
        ), # end dashboardSidebar


      dashboardBody(
        tags$script(HTML("
                            var openTab = function(tabName){
                              $('a', $('.sidebar')).each(function() {
                                if(this.getAttribute('data-value') == tabName) {
                                  this.click()
                                };
                              });
                            };
                            $('.sidebar-toggle').attr('id','menu');
                            var dimension = [0, 0];
                                $(document).on('shiny:connected', function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange('dimension', dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange('dimension', dimension);
                                });
                          ")),
        
        # Customize color for the box status 'primary' and 'success' to match the skin color
        tags$style(HTML("
                    .btn.btn-success {
                     color: #fff;
                    background-color: #3c8dbc;
                    border-color: #3c8dbc;
                    }
                    .btn.btn-success.focus,
                    .btn.btn-success:focus {
                    color: #fff;
                    background-color: #3c8dbc;
                    border-color: #3c8dbc;
                    outline: none;
                    box-shadow: none;
                    }
                    .btn.btn-success:hover {
                    color: #fff;
                    background-color: #3c8dbc;
                    border-color: #3c8dbc;
                    outline: none;
                    box-shadow: none;
                    }
                    .btn.btn-success.active,
                    .btn.btn-success:active {
                    color: #fff;
                    background-color: #3c8dbc;
                    border-color: #3c8dbc;
                    outline: none;
                    }
                    .btn.btn-success.active.focus,
                    .btn.btn-success.active:focus,
                    .btn.btn-success.active:hover,
                    .btn.btn-success:active.focus,
                    .btn.btn-success:active:focus,
                    .btn.btn-success:active:hover {
                    color: #fff;
                    background-color: #7ac8f5 ;
                    border-color: #7ac8f5 ;
                    outline: none;
                    box-shadow: none;
                    }

                  ")),
        
        tags$div(class = "tab-content",
        #tabItems(
          
          tabItem(tabName = "AboutTab",
                  titlePanel(title = div(strong("Welcome to ", img(src="neurosurveillance_logo.png", height="35%", width="35%")))),
                  
                  fluidRow( # create a separation in the panel
                    column(width = 8, # create first column for boxplot
                           box(width = NULL, status = "primary",
                               p(h3("Benchmarking the spontaneous functional and neurological recovery following spinal cord injury"), align = "justify"),
                               br(),
                               p('Traumatic spinal cord injury is a rare but devastating neurological disorder. 
                                It constitutes a major public health issue, burdening both patients, caregivers, 
                                as well as society as a whole. The goal of this project is to establish an 
                                international benchmark for neurological and functional recovery after spinal 
                                cord injury. Currently, Neurosurveillance leverages three decades of data from two 
                                of the largest data sources in the field facilitating the analysis of temporal 
                                trends in epidemiological landscape, providing reference values for future clinical 
                                trials and studies, and enabling monitoring of patients on a personalized level.', align = "justify"),
                               p('More information can be found here:', a(icon('github'), href ="https://github.com/jutzca/Neurosurveillance", target="_blank")),
                               br(),
                               p(h3('What You Can Do Here:')),
                               p("This applet has enables visitors to directly interact with the data collected 
                                 in the EMSCI study and the Sygen clinical trial. Both", 
                                 a("EMSCI", onclick = "openTab('emsci')", href="#"), 
                                "and", a("Sygen", onclick = "openTab('sygen')", href="#"),
                                "Tabs are organized as follows:", align = "justify"),
                               
                               tags$ul(align="justify",
                                 tags$li("The Study details Tab provides information the data sources, 
                                         (",
                                         a("EMSCI study", onclick = "openTab('about_emsci')", href="#"),
                                         "and the ",
                                         a("Sygen clinical trial", onclick = "openTab('about_sygen')", href="#"),
                                         ")."),
                                 tags$li("The Epidemiological feature Tabs (",
                                         a("EMSCI study", onclick = "openTab('epi_emsci')", href="#"), " and ",
                                         a("Sygen clinical trial", onclick = "openTab('epi_sygen')", href="#"),
                                         "), Neurological features Tabs (",
                                         a("EMSCI study", onclick = "openTab('neuro_emsci')", href="#"), "and",
                                         a("Sygen clinical trial", onclick = "openTab('neuro_sygen')", href="#"),
                                         ") and Functional features Tabs (",
                                         a("EMSCI study", onclick = "openTab('funct_emsci')", href="#"), "and",
                                         a("Sygen clinical trial", onclick = "openTab('funct_sygen')", href="#"),
                                         ") offer an interactive interface to explore the epidemiological chracteristics, as well as 
                                         neurological and functional recovery after spinal cord injury, respectively. 
                                         You can choose the data source, 
                                         outcome variable of interest, select the cohort of interest based in demographics and 
                                         injury characteristics (entire cohort or a subset thereof), and chose the variables for 
                                         the visualization (see more details in",
                                         a("User Guide", onclick = "openTab('user_guide')", href="#"), 
                                         ")."),
                                 tags$li("The ",
                                         a("Monitoring", onclick = "openTab('monitore_emsci')", href="#"),
                                         " Tab, part of the EMSCI Tab, gives you the possibility to visualize and monitor the neurological and 
                                         functional recovery trajectory of single patients or patient groups that share very similar 
                                         demographics and baseline injury characteristics. As an example, if you have a patient in the 
                                         clinical with a certain motor score and you are interested in their recovery, you can have a 
                                         look at previous patients with comparable characteristics. This follows the concept of a 
                                         digital twin/sibling.")),
                               
                               p("Additionally, you can find:", align = "justify"),
                               
                               tags$ul(align="justify",
                                       tags$li("A ",
                                               a("User Guide", onclick = "openTab('user_guide')", href="#"),
                                               " describing the different ways to interact with the plots."),
                                 tags$li("The ",
                                         a("Abbreviation", onclick = "openTab('abbreviations')", href="#"),
                                         " Tab describes the abbreviations used within the framework of this applet.")
                                 ),
                               
                               uiOutput("video"),
                               
                               br(),
                               p(h3('Study team')),
                               p(h4('Principal Investigators')),
                               HTML(
                                 paste('<p style="text-align:justify">',
                                   a(icon('envelope'), href = 'mailto:catherine.jutzeler@bsse.ethz.ch'), strong('Dr. Catherine Jutzeler'),
                                   ', Research Group Leader, Department of Biosystems Science and Engineering, ETH Zurich, Switzerland', 
                                   '<br/>',
                                   a(icon('envelope'), href = 'mailto:lucie.bourguignon@bsse.ethz.ch'), strong('Lucie Bourguignon'),
                                   ', MSc. PhD Student, Department of Biosystems Science and Engineering, ETH Zurich, Switzerland', 
                                   '<br/>',
                                   a(icon('envelope'), href = 'mailto:john.kramer@ubc.ca'), strong('Prof. John Kramer'),
                                   ', ICORD principle investigator, Anesthesiology, Pharmacology, and Therapeutics, 
                                   University of British Columbia, Vancouver, Canada', 
                                   '<br/>',
                                   a(icon('envelope'), href = 'mailto:armin.curt@balgrist.ch'), strong('Prof. Armin Curt'),
                                   ', Director of Balgrist Spinal Cord Injury Center and EMSCI, University Hospital Balgrist, 
                                   Zurich, Switzerland'
                                   )
                                 ),
                               p(h4('Collaborators')),
                               p('Bobo Tong, Fred Geisler, Martin Schubert, Frank Röhrich, Marion Saur, Norbert Weidner, 
                                 Ruediger Rupp, Yorck-Bernhard B. Kalke, Rainer Abel, Doris Maier, Lukas Grassner, 
                                 Harvinder S. Chhabra, Thomas Liebscher, Jacquelyn J. Cragg, ',
                                 a('EMSCI study group', href = 'https://www.emsci.org/index.php/members', target="_blank"), align = "justify"),
                               br(),
                               p(h3('Ethics statement')),
                               p('All patients gave their written informed consent before being included in the EMSCI database. 
                                 The study was performed in accordance with the Declaration of Helsinki and was approved 
                                 by all responsible institutional review boards. The study was performed in accordance with 
                                 the Declaration of Helsinki. If you have any questions or concerns regarding the study 
                                 please do not hesitate to contact the Principal Investigator, ',
                                 a('Dr. Catherine Jutzeler', href = 'mailto:catherine.jutzeler@bsse.ethz.ch'), '.', align = "justify"),
                               br(),
                               p(h3('Funding')),
                               p('This project is supported by the ',
                                 a('Swiss National Science Foundation', href = 'http://p3.snf.ch/project-186101', target="_blank"),
                                 ' (Ambizione Grant, #PZ00P3_186101), ',
                                 a('Wings for Life Research Foundation', href = 'https://www.wingsforlife.com/en/', target="_blank"),
                                 ' (#2017_044), the ',
                                 a('International Foundation for Research in Paraplegia', href = 'https://www.irp.ch/en/foundation/', target="_blank"),
                                 ' (IRP).', align = "justify"),
                            
                           
                           ), # end box
                           tags$style(".small-box{border-radius: 15px}"),
                           valueBox("5000+", "Patients", icon = icon("hospital-user"), width = 3, color = "blue"),
                           valueBox("15", "Countries", icon = icon("globe-europe"), width = 3, color = "blue"),
                           valueBox("20", "Years", icon = icon("calendar-alt"), width = 3, color = "blue"),
                           valueBox("50+", "Researchers", icon = icon("user-cog"), width = 3, color = "blue")#,
                    ), # end column
                    
                    column(width = 4, 
                           box(width = NULL, status = "primary",
                               tags$div(
                                 HTML('<a href="https://twitter.com/Neurosurv_Sci?ref_src=twsrc%5Etfw" class="twitter-follow-button" data-show-count="false">Follow @Neurosurv_Sci</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
                               ),
                               tags$div(
                                 HTML('<a class="twitter-timeline" data-height="1800" data-theme="light" href="https://twitter.com/Neurosurv_Sci?ref_src=twsrc%5Etfw">Tweets by Neurosurv_Sci</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
                               )) # end box
                    ) # end column
                  ), # end fluidRow
                  
                  
                  shinyjs::useShinyjs(),
                  tags$footer(HTML("<strong>Copyright &copy; 2020 <a href=\"https://github.com/jutzca/Neurosurveillance\" target=\"_blank\">Neurosurveillance</a>.</strong> 
                   <br>This work is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-nd/4.0/\" target=\"_blank\">Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License</a>.
                   <br><a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-nd/4.0/\" target=\"_blank\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png\" /></a>
                   <br>Last updated:<br>"), 
                              latest.DateTime,
                              id = "sideFooter",
                              align = "left",
                              style = "
                  position:absolute;
                  bottom:0;
                  width:100%;
                  padding: 10px;
                  "
                  )
          ), # end tabItem
        
        tabItem(tabName = "user_guide",
                titlePanel(title = div(strong("User Guide"))),
        ), #end tabItem
          
          tabItem(tabName = "about_emsci",
                  h3("European Multicenter Study about Spinal Cord Injury"),
                  box(#title = "Explore The Data", 
                    width = 12, 
                    heigth = "500px",
                    solidHeader = TRUE,
                    
                    strong("Study design"),
                    "The EMSCI is an ongoing longitudinal, observational study that prospectively collects clinical, 
                    functional, and neurophysiological data at fixed time points over the first year of injury: very acute 
                    (within 2 weeks), acute I (2-4 weeks), acute II (3 months), and acute III (6 months), and chronic 
                    (12 months)",
                    br(),
                    br(),
                    strong("Governance structure"),
                    "Founded in 2001 and led by the University Clinic Balgrist (Zurich, Switzerland), EMSCI 
                    consists of 23 active members from 9 countries and 6 passive members from four countries. 
                    Moreover, it has three associated members from Canada (2) and Germany (1).",
                    br(),
                    br(),
                    strong("Ethical approval and registration"),
                    "The study was performed in accordance with the Declaration of Helsinki and 
                    approved by the local ethics committee of each participating center. All patients 
                    gave their written informed consent before being included in the EMSCI database. 
                    European Multicenter Study about Spinal Cord Injury is registered with ClinicalTrials.gov 
                    Identifier (NCT01571531).",
                    br(),
                    br(),
                    strong("Inclusion/exclusion criteria."), 
                    "Three inclusion criteria have to be met before a patient can be enrolled in the EMSCI: 
                    (1) the patient has to be capable and willing to give written informed consent; 
                    (2) the injury was caused by a single event; and 
                    (3) the first EMSCI assessment has to be possible within the first 6 weeks following injury. 
                    Patients are excluded from EMSCI for the following reasons: 
                    (1) nontraumatic para- or tetraplegia (i.e. discusprolaps, tumor, AV-malformation, myelitis) excl. single event ischemic incidences; 
                    (2) previously known dementia or severe reduction of intelligence, leading to reduced capabilities of cooperation or giving consent; 
                    (3) peripheral nerve lesions above the level of lesion (i.e. plexus brachialis impairment); 
                    (4) pre-existing polyneuropathy; and 
                    (5) severe craniocerebral injury. All individuals in the EMSCI receive standards of rehabilitation care.",
                    br(),
                    br(),
                    strong("Neurological Scoring"), 
                    "The EMSCI centers collect the following neurological scores: total motor score, 
                    lower extremity motor score, upper extremity motor score, total pinprick score, 
                    and total light touch score. For motor scores, key muscles in the upper and lower 
                    extremities were examined according to the International Standards for the Neurological 
                    Classification of SCI (ISNCSCI), with a maximum score of 50 points for each of the upper 
                    and lower extremities (for a maximum total score of 100). Light touch and pin prick 
                    (sharp-dull discrimination) scores were also assessed according to ISNCSCI, with a maximum 
                    score of 112 each.",
                    br(),
                    br(),
                    strong("Functional Assessments"), 
                    "Functional outcomes comprise Spinal Cord Independence Measure (SCIM), 
                    Walking Index for Spinal Cord Injury, 10-meter walking test (10MWT), 
                    6-minute walk test (6MWT), and Timed Up & Go (TUG) test. The SCIM is a 
                    scale for the assessment of achievements of daily function of patients with spinal cord lesions. 
                    Throughout the duration of this study (2001-2019), two different versions of the SCIM were used: 
                    between 2001-2007 SCIM II23 and since 2008 SCIM III. The major difference between the versions 
                    is that SCIM II does not consider intercultural differences. Both versions contain 19 tasks, all 
                    activities of daily living organized, in four areas of function (subscales): Self-Care (scored 0–20); 
                    respiration and sphincter management (0–40); mobility in room and toilet (0–10); and mobility 
                    indoors and outdoors (0–30). WISCI II has an original scale that quantifies a patient's walking 
                    ability; a score of 0 indicates that a patient cannot stand and walk and the highest score of 20 
                    is assigned in case a patient can walk more than 10m without walking aids of assistance. Lastly, 
                    10MWT measures the time (in seconds) it takes a patient to walk 10m, the 6MWT quantifies the 
                    distance (in meters) covered by the patient within 6 minutes, and TUG measures the time (in seconds) 
                    it takes a patient to stand up from an armchair, walk 3m, return to the chair, and sit down."
                  )
          ), #end tabItem
          
          tabItem(tabName = "epi_emsci",
                  fluidRow(
                    column (width = 8,
                            
                            box(status = "primary", width = NULL,
                                
                                div(style="display:inline-block;width:100%;text-align:center;",
                                    radioGroupButtons(
                                      inputId = "var_epi_emsci", 
                                      label = "Epidemiological features:", 
                                      selected = "sex_emsci",
                                      status = "success",
                                      individual = T, #if false, then the boxes are connected
                                      choiceNames = c("Sex", "Age", "AIS grade", "Level of injury"),
                                      choiceValues = c("sex_emsci", "age_emsci", "ais_emsci", "nli_emsci")
                                    ) # Close radioGroupButtons bracket
                                ), # Close div bracket
                            ),
                            
                            box(status = "primary", width = NULL,
                                div(plotOutput("plot.epi.emsci", width = "90%",
                                               height = "660px",
                                               inline = FALSE), 
                                    align='center')
                            ), # end box 
                    ), # end column
                    
                    column(width = 4, # create second column for second type of user inputs (filters)
                           box(status = "primary", width = NULL, # create a new box
                               sliderTextInput(inputId = "year_epi_emsci", # create new slider text
                                               label = "Years of injury to display:", # label of the box
                                               choices = list("2000" = 2000,"2001" = 2001,"2002" = 2002,"2003" = 2003,"2004" = 2004,
                                                              "2005" = 2005,"2006" = 2006,"2007" = 2007,"2008" = 2008,"2009" = 2009,
                                                              "2010" = 2010,"2011" = 2011,"2012" = 2012,"2013" = 2013,"2014" = 2014,
                                                              "2015" = 2015,"2016" = 2016,"2017" = 2017,"2018" = 2018,"2019" = 2019,
                                                              "2019" = 2019),
                                               selected = c(2000, 2019),
                                               animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                               to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                               to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                               post = NULL, dragRange = TRUE)
                               ), # end box
                           
                           box(status = "primary", width = NULL, # create a new box
                               sliderInput(inputId = "binyear_epi_emsci", # create new slider text
                                           label = "Number of years per bin:", # label of the box
                                           min = 1, max = 5,
                                           value = 1)
                               ), # end box
                           
                           box(status = "primary", width = NULL, # create box
                               radioButtons("checkbox_epi_emsci",
                                            label = "Do you want to inspect subpopulations ?",
                                            choices = c("No" = 1, 'Yes' = 0),
                                            selected = 1) # checkbox to go to basic plot: no filters, EMSCI patients, x-axis=stages, y-axis=value of score
                               ), # end box
                           
                           conditionalPanel(condition = "input.checkbox_epi_emsci == 0", # if user decides to not display EMSCI data and apply filters, make a new panel appear
                                            box(status = "primary", width = NULL, # create new box
                                                radioButtons(inputId = "checkGroup_epi_emsci", # create new check box group
                                                             label = "Criteria:", # label of the box
                                                             choices = list("AIS grade" = '1', "Paralysis type" = '2'), # choices are the different filters that can be applied
                                                             selected = c('1')) # by default, sex and AIS grades are selected
                                                ) # end box
                                            ), # end conditionalPanel
                           
                           conditionalPanel(condition = "input.checkbox_epi_emsci == 0 && input.checkGroup_epi_emsci.includes('1')", # if user chooses to filter based on AIS grade
                                            box(status = "primary", width = NULL, # create a new box
                                                checkboxGroupInput(inputId = "grade_epi_emsci", # create new check box group
                                                                   label = "AIS grade:", # label of the box
                                                                   choices = list("AIS A" = "A", "AIS B" = "B", "AIS C" = "C", "AIS D" = "D"), # choices (match the levels in EMSCI dataset), missing AIS grades will automaticEMSCIy be removed
                                                                   selected = c("A", "B", "C", "D")) # by default, EMSCI grades are selected but missing grades
                                                ) # end box
                                            ), # end conditionalPanel
                           
                           conditionalPanel(condition = "input.checkbox_epi_emsci == 0 && input.checkGroup_epi_emsci.includes('2')", # if user chooses to filter based on functlogical level of injury
                                            box(status = "primary", width = NULL, # create a new box
                                                checkboxGroupInput(inputId = "paralysis_epi_emsci", # create new check box group
                                                                   label = "Type of paralysis:", # label of the box
                                                                   choices = list("paraplegia", 'tetraplegia'),
                                                                   selected = c("paraplegia", 'tetraplegia'))
                                                ) # end box
                                            ) # end conditionalPanel
                           ), #end column
                  )  #close fluid row
          ), #end tabItem
          
          tabItem(tabName = "neuro_emsci",
                  fluidRow(
                    column (width = 8,
                            
                            box(status = "primary", width = NULL,
                                
                                div(style="display:inline-block;width:100%;text-align:center;",
                                    radioGroupButtons(
                                      inputId = "var_neuro_emsci", 
                                      label = "Neurological features:", 
                                      selected = "UEMS",
                                      status = "success",
                                      individual = T, #if false, then the boxes are connected
                                      choiceNames = c("UEMS","RUEMS","LUEMS",
                                                      "LEMS","RLEMS","LLEMS",
                                                      "RMS","LMS","TMS",
                                                      "RPP","LPP","TPP",
                                                      "RLT","LLT","TLT"),
                                      choiceValues = c("UEMS","RUEMS","LUEMS",
                                                       "LEMS","RLEMS","LLEMS",
                                                       "RMS","LMS","TMS",
                                                       "RPP","LPP","TPP",
                                                       "RLT","LLT","TLT")
                                    ) # Close radioGroupButtons bracket
                                ), # Close div bracket
                            ), # close box
                            
                            box(status = "primary", width = NULL,
                                div(plotlyOutput("plot.neuro.emsci", width = "90%",
                                               height = "660px",
                                               inline = FALSE), 
                                    align='center'),
                                
                            ), # end box 
                    ), # end column
                  
                  column(width = 4, # create second column for second type of user inputs (filters)
                         
                         box(status = "primary", width = NULL, # create box 
                             radioButtons("cont_neuro_emsci",
                                          label = "Type of plot ?",
                                          choices = c("Trend plot" = 0, 'Boxplot' = 1), 
                                          selected = 0)
                         ), # end box
                         
                         box(status = "primary", width = NULL, # create box 
                             radioButtons("subset_neuro_EMSCI",
                                          label = "Select subset of the cohort ?",
                                          choices = c("No" = 1, 'Yes' = 0), 
                                          selected = 1), # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                             
                             conditionalPanel(condition = "input.subset_neuro_EMSCI == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                              checkboxGroupInput(inputId = "checksub_neuro_EMSCI", # create new check box group
                                                                 label = "Subsetting criteria:", # label of the box
                                                                 choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "Level of injury" = '5', "Country" = '6'),# "Year of injury" = '7'), # choices are the different filters that can be applied
                                                                 selected = c('1', '4')) # by default, sex and AIS grades are selected
                                              ), # end conditionalPanel
                             
                             conditionalPanel(condition = "input.subset_neuro_EMSCI == 0 && input.checksub_neuro_EMSCI.includes('1')", # if user chooses to filter based on sex
                                              checkboxGroupInput(inputId = "sex_neuro_EMSCI", # create new check box group
                                                                 label = "Sex:", # label of the box
                                                                 choices = list("Male", "Female"), # choices are male and female (match the levels in EMSCI dataset)
                                                                 selected = c("Male", "Female")) # by default, both male and female are selected
                                              ), # end conditionalPanel
                             
                             conditionalPanel(condition = "input.subset_neuro_EMSCI == 0 && input.checksub_neuro_EMSCI.includes('2')", # if user chooses to filter based on age
                                              checkboxGroupInput(inputId = "age_neuro_EMSCI", # create new check box group
                                                                 label = "Age at injury:", # label of the box
                                                                 choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4), # choices are age group divided in 20 years (match the levels in EMSCI dataset from categorised column)
                                                                 selected = c(0,1,2,3,4)) # by default, all categories are selected
                                              ), # end conditionalPanel
                             
                             conditionalPanel(condition = "input.subset_neuro_EMSCI == 0 && input.checksub_neuro_EMSCI.includes('3')", # if user chooses to filter based on cause of injury
                                              checkboxGroupInput(inputId = "cause_neuro_EMSCI", # create new check box group
                                                                 label = "Cause of injury:", # label of the box
                                                                 choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"), # choices (match the levels in EMSCI dataset)
                                                                 selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other")) # by default, all categories are selected
                                              ), # end conditionalPanel
                             
                             conditionalPanel(condition = "input.subset_neuro_EMSCI == 0 && input.checksub_neuro_EMSCI.includes('4')", # if user chooses to filter based on AIS grade
                                              checkboxGroupInput(inputId = "grade_neuro_EMSCI", # create new check box group
                                                                 label = "AIS grade:", # label of the box
                                                                 choices = list("AIS A", "AIS B", "AIS C", "AIS D", "AIS E"), # choices (match the levels in EMSCI dataset), missing AIS grades will automatically be removed
                                                                 selected = c("AIS A", "AIS B", "AIS C", "AIS D", "AIS E")) # by default, all grades are selected but missing grades
                                              ), # end conditionalPanel

                             conditionalPanel(condition = "input.subset_neuro_EMSCI == 0 && input.checksub_neuro_EMSCI.includes('5')", # if user chooses to filter based on neurological level of injury
                                              checkboxGroupInput(inputId = "level_neuro_EMSCI", # create new check box group
                                                                 label = "Neurological level of injury:", # label of the box
                                                                 choices = list("cervical", "thoracic", "lumbar", "sacral"), # choices (match the levels in EMSCI dataset)
                                                                 selected = c("cervical", "thoracic", "lumbar", "sacral")) # by default, all categories are selected
                             ), # end conditionalPanel
                             
                             conditionalPanel(condition = "input.subset_neuro_EMSCI == 0 && input.checksub_neuro_EMSCI.includes('6')", # if user chooses to filter based on country
                                              checkboxGroupInput(inputId = "country_neuro_EMSCI", # create new check box group
                                                                 label = "Country:", # label of the box
                                                                 choices = list("Austria", "Czech Republic", "France", "Germany",
                                                                                "Great Britain", "India", "Italy", "Netherlands",
                                                                                "Spain", "Switzerland"), # choices (match the levels in EMSCI dataset)
                                                                 selected = c("Austria", "Czech Republic", "France", "Germany",
                                                                              "Great Britain", "India", "Italy", "Netherlands",
                                                                              "Spain", "Switzerland")) # by default, all countries are selected
                             ), # end conditionalPanel
                             
                             # conditionalPanel(condition = "input.subset_neuro_EMSCI == 0 && input.checksub_neuro_EMSCI.includes('7')", # if user chooses to filter based on year of injury (categorised)
                             #                  sliderTextInput(inputId = "filteryear_neuro_EMSCI", # create new slider text
                             #                                  label = "Years of injury to display:", # label of the box
                             #                                  choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2019" = 2019), # choices (match the levels in EMSCI dataset in categorised column)
                             #                                  selected = c(2000, 2019), # by default, all groups are selected
                             #                                  animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                             #                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                             #                                  to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                             #                                  post = NULL, dragRange = TRUE)
                             # ) # end conditionalPanel
                         ), # end box
                         
                         box(status = "primary", width = NULL, # create box
                             radioButtons("checkbox_neuro_EMSCI",
                                          label = "How do you want to visualise the data?",
                                          choices = c("Default" = 1, 'Customize (display by subgroups)' = 0),
                                          selected = 1), # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                             conditionalPanel(condition = "input.checkbox_neuro_EMSCI == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                              checkboxGroupInput(inputId = "checkGroup_neuro_EMSCI", # create new check box group
                                                                 label = "Visualisation criteria:", # label of the box
                                                                 choices = list("Sex" = '1',
                                                                                "Age at injury" = '2',
                                                                                "Cause of SCI" = '3',
                                                                                "AIS grade" = '4',
                                                                                "Level of injury" = '5',
                                                                                "Country" = '6'),
                                                                                #"Year of injury" = '7'), # choices are the different filters that can be applied
                                                                 selected = c('1','4')) # by default, sex and AIS grades are selected
                             ), # end conditionalPanel
                         ), # end box
                         
                         box(status = "primary", width = NULL,
                             radioButtons(inputId = "neuro_choose_time_EMSCI",
                                          label = "Choose time displayed:",
                                          choices = c("Single time point" = "single",
                                                      "Time range" = "multiple"),
                                          selected = "multiple"),
                             
                             conditionalPanel(condition = "input.neuro_choose_time_EMSCI == 'multiple'",
                                              sliderTextInput(inputId = "neuro_time_multiple_EMSCI",
                                                              label = "Time range:",
                                                              choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                              selected = c("very acute", 'chronic'),
                                                              animate = F, grid = TRUE, hide_min_max = FALSE, from_fixed = FALSE,
                                                              to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                              to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                              post = NULL, dragRange = TRUE)
                             ), # end conditionalPanel
                             
                             conditionalPanel(condition = "input.neuro_choose_time_EMSCI == 'single'",
                                              sliderTextInput(inputId = "neuro_time_single_EMSCI",
                                                              label = "Time point:",
                                                              choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                              selected = c("very acute"),
                                                              animate = F, grid = TRUE, hide_min_max = FALSE, from_fixed = FALSE,
                                                              to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                              to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                              post = NULL, dragRange = TRUE)
                             ) # end conditionalPanel
                         ), # end box
                         
                         box(status = "primary", width = NULL, # create a new box
                             sliderTextInput(inputId = "year_neuro_emsci", # create new slider text
                                             label = "Years of injury to display:", # label of the box
                                             choices = list("2000" = 2000,"2001" = 2001,"2002" = 2002,"2003" = 2003,"2004" = 2004,
                                                            "2005" = 2005,"2006" = 2006,"2007" = 2007,"2008" = 2008,"2009" = 2009,
                                                            "2010" = 2010,"2011" = 2011,"2012" = 2012,"2013" = 2013,"2014" = 2014,
                                                            "2015" = 2015,"2016" = 2016,"2017" = 2017,"2018" = 2018,"2019" = 2019,
                                                            "2019" = 2019),
                                             selected = c(2000, 2019),
                                             animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                             to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                             to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                             post = NULL, dragRange = TRUE)
                         ), # end box
                         
                         box(status = "primary", width = NULL, # create a new box
                             sliderInput(inputId = "binyear_neuro_emsci", # create new slider text
                                         label = "Number of years per bin:", # label of the box
                                         min = 1, max = 5,
                                         value = 5)
                         ), # end box
                         
                  ) #end column
                  ) #end FluidRow
          ), #end tabItem
          
          tabItem(tabName = "funct_emsci",
                  
                  fluidRow(
                    column (width = 8,
                            
                            box(status = "primary", width = NULL,
                                
                                div(style="display:inline-block;width:100%;text-align:center;",
                                    radioGroupButtons(
                                      inputId = "var_funct_emsci", 
                                      label = "Functional features:", 
                                      selected = "WISCI",
                                      status = "success",
                                      individual = T, #if false, then the boxes are connected
                                      choiceNames = c("WISCI","test_6min","test_10m","TUG","SCIM2","SCIM3"),
                                      choiceValues = c("WISCI","test_6min","test_10m","TUG","SCIM2","SCIM3")
                                    ) # Close radioGroupButtons bracket
                                ), # Close div bracket
                            ),
                            
                            box(status = "primary", width = NULL,
                                
                                div(plotlyOutput("plot.funct.emsci", width = "90%",
                                               height = "660px",
                                               inline = FALSE), 
                                    align='center'),
                                
                            ), # end box 
                    ), # end column
                    
                    column(width = 4, # create second column for second type of user inputs (filters)
                           
                           box(status = "primary", width = NULL, # create box 
                               radioButtons("cont_funct_emsci",
                                            label = "Type of plot ?",
                                            choices = c("Trend plot" = 0, 'Boxplot' = 1), 
                                            selected = 0)
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create box
                               radioButtons("subset_funct_EMSCI",
                                            label = "Select subset of the cohort ?",
                                            choices = c("No" = 1, 'Yes' = 0),
                                            selected = 1), # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                               
                               conditionalPanel(condition = "input.subset_funct_EMSCI == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                checkboxGroupInput(inputId = "checksub_funct_EMSCI", # create new check box group
                                                                   label = "Subsetting criteria:", # label of the box
                                                                   choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "Level of injury" = '5', "Country" = '6'),# "Year of injury" = '7'), # choices are the different filters that can be applied
                                                                   selected = c('1', '4')) # by default, sex and AIS grades are selected
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_funct_EMSCI == 0 && input.checksub_funct_EMSCI.includes('1')", # if user chooses to filter based on sex
                                                checkboxGroupInput(inputId = "sex_funct_EMSCI", # create new check box group
                                                                   label = "Sex:", # label of the box
                                                                   choices = list("Male", "Female"), # choices are male and female (match the levels in EMSCI dataset)
                                                                   selected = c("Male", "Female")) # by default, both male and female are selected
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_funct_EMSCI == 0 && input.checksub_funct_EMSCI.includes('2')", # if user chooses to filter based on age
                                                checkboxGroupInput(inputId = "age_funct_EMSCI", # create new check box group
                                                                   label = "Age at injury:", # label of the box
                                                                   choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4), # choices are age group divided in 20 years (match the levels in EMSCI dataset from categorised column)
                                                                   selected = c(0,1,2,3,4)) # by default, all categories are selected
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_funct_EMSCI == 0 && input.checksub_funct_EMSCI.includes('3')", # if user chooses to filter based on cause of injury
                                                checkboxGroupInput(inputId = "cause_funct_EMSCI", # create new check box group
                                                                   label = "Cause of injury:", # label of the box
                                                                   choices = list("ischemic", "traumatic"), # choices (match the levels in EMSCI dataset)
                                                                   selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other")) # by default, all categories are selected
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_funct_EMSCI == 0 && input.checksub_funct_EMSCI.includes('4')", # if user chooses to filter based on AIS grade
                                                checkboxGroupInput(inputId = "grade_funct_EMSCI", # create new check box group
                                                                   label = "AIS grade:", # label of the box
                                                                   choices = list("AIS A", "AIS B", "AIS C", "AIS D", "AIS E"), # choices (match the levels in EMSCI dataset), missing AIS grades will automatically be removed
                                                                   selected = c("AIS A", "AIS B", "AIS C", "AIS D", "AIS E")) # by default, all grades are selected but missing grades
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_funct_EMSCI == 0 && input.checksub_funct_EMSCI.includes('5')", # if user chooses to filter based on neurological level of injury
                                                checkboxGroupInput(inputId = "level_funct_EMSCI", # create new check box group
                                                                   label = "Neurological level of injury:", # label of the box
                                                                   choices = list("cervical", "thoracic", "lumbar", "sacral"), # choices (match the levels in EMSCI dataset)
                                                                   selected = c("cervical", "thoracic", "lumbar", "sacral")) # by default, all categories are selected
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_funct_EMSCI == 0 && input.checksub_funct_EMSCI.includes('6')", # if user chooses to filter based on country
                                                checkboxGroupInput(inputId = "country_funct_EMSCI", # create new check box group
                                                                   label = "Country:", # label of the box
                                                                   choices = list("Austria", "Czech Republic", "France", "Germany",
                                                                                  "Great Britain", "India", "Italy", "Netherlands",
                                                                                  "Spain", "Switzerland"), # choices (match the levels in EMSCI dataset)
                                                                   selected = c("Austria", "Czech Republic", "France", "Germany",
                                                                                "Great Britain", "India", "Italy", "Netherlands",
                                                                                "Spain", "Switzerland")) # by default, all countries are selected
                               ), # end conditionalPanel
                               
                               # conditionalPanel(condition = "input.subset_funct_EMSCI == 0 && input.checksub_funct_EMSCI.includes('7')", # if user chooses to filter based on year of injury (categorised)
                               #                  sliderTextInput(inputId = "filteryear_funct_EMSCI", # create new slider text
                               #                                  label = "Years of injury to display:", # label of the box
                               #                                  choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2019" = 2019), # choices (match the levels in EMSCI dataset in categorised column)
                               #                                  selected = c(2000, 2019), # by default, all groups are selected
                               #                                  animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                               #                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                               #                                  to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                               #                                  post = NULL, dragRange = TRUE)
                               # ) # end conditionalPanel
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create box
                               radioButtons("checkbox_funct_EMSCI",
                                            label = "How do you want to visualise the data?",
                                            choices = c("Default" = 1, 'Customize (display by subgroups)' = 0),
                                            selected = 1), # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                               conditionalPanel(condition = "input.checkbox_funct_EMSCI == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                checkboxGroupInput(inputId = "checkGroup_funct_EMSCI", # create new check box group
                                                                   label = "Visualisation criteria:", # label of the box
                                                                   choices = list("Sex" = '1',
                                                                                  "Age at injury" = '2',
                                                                                  "Cause of SCI" = '3',
                                                                                  "AIS grade" = '4',
                                                                                  "Level of injury" = '5',
                                                                                  "Country" = '6'),
                                                                                  #"Year of injury" = '7'), # choices are the different filters that can be applied
                                                                   selected = c('1','4')) # by default, sex and AIS grades are selected
                               ), # end conditionalPanel
                           ), # end box
                           
                           box(status = "primary", width = NULL,
                               radioButtons(inputId = "funct_choose_time_EMSCI",
                                            label = "Choose time displayed:",
                                            choices = c("Single time point" = "single",
                                                        "Time range" = "multiple"),
                                            selected = "multiple"),
                               
                               conditionalPanel(condition = "input.funct_choose_time_EMSCI == 'multiple'",
                                                sliderTextInput(inputId = "funct_time_multiple_EMSCI",
                                                                label = "Time range:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.funct_choose_time_EMSCI == 'single'",
                                                sliderTextInput(inputId = "funct_time_single_EMSCI",
                                                                label = "Time point:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute"),
                                                                animate = F, grid = TRUE, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                               ) # end conditionalPanel
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create a new box
                               sliderTextInput(inputId = "year_funct_emsci", # create new slider text
                                               label = "Years of injury to display:", # label of the box
                                               choices = list("2000" = 2000,"2001" = 2001,"2002" = 2002,"2003" = 2003,"2004" = 2004,
                                                              "2005" = 2005,"2006" = 2006,"2007" = 2007,"2008" = 2008,"2009" = 2009,
                                                              "2010" = 2010,"2011" = 2011,"2012" = 2012,"2013" = 2013,"2014" = 2014,
                                                              "2015" = 2015,"2016" = 2016,"2017" = 2017,"2018" = 2018,"2019" = 2019,
                                                              "2019" = 2019),
                                               selected = c(2000, 2019),
                                               animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                               to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                               to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                               post = NULL, dragRange = TRUE)
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create a new box
                               sliderInput(inputId = "binyear_funct_emsci", # create new slider text
                                           label = "Number of years per bin:", # label of the box
                                           min = 1, max = 5,
                                           value = 5)
                           ), # end box
                    ) #end column
                  ) # end FluidRow
          ), #end tabItem
          
          tabItem(tabName = "monitore_emsci",
                  useShinyalert(),
                  actionButton("preview_predict", "Disclaimer"),
                  htmlOutput("title_predict"),
                  fluidRow( # create a separation in the panel
                    column(width = 8, # create first column for boxplot
                           box(width = NULL, status = "primary",
                           p("Using this monitoring tool you can visualise, in blue, patients that had a 
                           similar score at a very acute stage for your outcome of interest (± 5 points). 
                           These patients share the characteristics you specified in terms of sex, 
                           age injury severity, (AIS grade), and neurological of injury. 
                           Black points represent patients with the similar characteristics, 
                           irrespective of their score at a very acute stage.")),
                           box(width = NULL, status = "primary", # create box to display plot
                               align="center", # center the plot
                               plotOutput('plot_predict', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                              ), # end column
                    
                    column(width = 4, # create second column for second type of user inputs (filters)
                           
                           box(status = "primary", width = NULL, # create a new box
                               selectInput("select_score",
                                           label = "Select outcome of interest",
                                           choices = list("UEMS", 'LEMS', 'TMS'),
                                           selected = c("UEMS"))
                               ), # end box
                           
                           box(status = "primary", width = NULL, # create a new box
                               textInput("input_compscore",
                                         label = "Patient score at very acute stage", 
                                         value = "Enter value...")
                               ), # end box
                           
                           box(status = "primary", width = NULL, # create a new box
                               selectInput("select_sex", 
                                           label = "Select sex",
                                           choices = list("Male" = "m", "Female" = "f", "Unknown" = "Unknown"), 
                                           selected = c("Unknown"))
                               ), # end box
                           
                           box(status = "primary", width = NULL, # create box 
                               selectInput("select_age", 
                                           label = "Select age at injury",
                                           choices = list("0-19", "20-39", "40-59", "60-79", "80+", 'Unknown'), 
                                           selected = c("Unknown"))
                               ), # end box
                           
                           box(status = "primary", width = NULL, # create a new box
                               selectInput("select_ais", 
                                           label = "Select baseline AIS grade", 
                                           choices = list("AIS A" = "A", "AIS B" = "B", "AIS C" = "C", "AIS D" = "D", "AIS E" = "E", "Unknown" = "Unknown"), 
                                           selected = c("Unknown"))
                               ), # end box
                           
                           box(status = "primary", width = NULL, # create a new box
                               selectInput("select_nli", 
                                           label = "Select injury level", 
                                           choices = list("Unknown", "Cervical", "Thoracic", "Lumbar", "Sacral",
                                                          "C1","C2","C3","C4","C5","C6","C7","C8",
                                                          "T1","T2","T3","T4","T5","T6","T7","T8","T9","T1","T11", "T12",
                                                          "L1", "L2", "L3", "L4", "L5",
                                                          "S1", "S2", "S3", "S4", "S5"), 
                                           selected = c("Unknown"))
                               )#, # end box
                           ) #end column
                    ) #end fluidRow
                  ), #end tabItem
          
          tabItem(tabName = "about_sygen",
                  h3(strong("The Sygen Clinical Trial")),
                  br(),
                  fluidRow(
                    box(#title = "Explore The Data", 
                      width = 8, 
                      heigth = "500px",
                      solidHeader = TRUE,
                      
                      h4("Objectives of original study"),
                      "To determine efficacy and safety of monosialotetrahexosylganglioside GM1 (i.e., Sygen) in acute spinal cord injury.",
                      br(),
                      h4("Methods"),
                      strong("Monosialotetrahexosylganglioside GM1"),
                      "Sygen (monosialotetrahexosylganglioside GM1 sodium salt) is a naturally occurring compound in cell membranes of mammals and is especially abundant in the membranes of the central nervous system. 
                      Acute neuroprotective and longer-term regenerative effects in multiple experimental models of ischemia and injury have been reported. The proposed mechanisms of action of GM1 include 
                      anti-excitotoxic activity, apoptosis prevention, and potentiation of neuritic sprouting and the effects of nerve growth factors.",
                      br(),
                      br(),
                      strong("Study design."), "Randomized, double-blind, sequential,
                      multicenter clinical trial of two doses Sygen (i.e., low-dose GM-1: 300 mg intravenous loading dose followed by 100 mg/d x 56 days or high-dose GM-1:00 mg intravenous loading dose followed by 200 mg/d x 56 days) versus
                      placebo. All patients received the National Acute Spinal Cord Injury Study (NASCIS-2) protocol dosage of methylprednisolone. Based on a potential adverse interaction between concomitant MPSS and GM-1 administration, 
                      the initial dose of GM-1 was delayed until after the steroids were given (mean onset of study medication, 54.9 hours).",
                      br(),
                      br(),
                      strong("Inclusion/exclusion criteria."), "For inclusion in Sygen, patients were required to have at least one lower extremity with a substantial motor deficit. Patients with spinal cord transection 
                      or penetration were excluded, as were patients with a cauda equina, brachial or lumbosacral plexus, or peripheral nerve injury. Multiple trauma cases were included as long as they were not so severe
                      as to preclude neurologic evaluation. It is notable that this requirement of participating in a detailed neurologic exam excluded major head trauma cases and also intubated 
                      chest trauma cases.",
                      br(),
                      br(),
                      strong("Assessments."), "Baseline neurologic assessment included both the AIS and detailed American Spinal Injury Association (ASIA) motor and
                      sensory examinations. Additionally, the Modified Benzel Classification and the ASIA motor and
                      sensory examinations were performed at 4, 8, 16, 26, and 52 weeks after injury. The Modified Benzel Classification was used for post-baseline measurement because it rates walking
                      ability and, in effect, subdivides the broad D category of the AIS. Because most patients have an unstable spinal fracture at
                      baseline, it is not possible to assess walking ability at that time; hence the use of different baseline and follow-up scales.
                      Marked recovery was defined as at least a two-grade equivalent improvement in the Modified Benzel Classification from the
                      baseline AIS. The primary efficacy assessment was the proportion of patients with marked recovery at week 26. The secondary efficacy assessments included the time course of marked recovery and
                      other established measures of spinal cord function (the ASIA motor and sensory scores, relative and absolute sensory levels of impairment, and assessments of bladder and bowel
                      function).",
                      br(),
                      br(),
                      strong("Concomitant medications."), "The use of medications delivered alongside the study medication (i.e., GM-1) was rigorously tracked. 
                      For each concomitant medication administered during the trial, the dosage, reason for administration, and the timing of administration were recorded.",
                      br(),
                      br(),
                      strong("Results."), "Of 797 patients recruited, 760 were included in the analysis. The prospectively planned analysis at the prespecified endpoint time for all patients was negative.
                      The negative finding of the Sygen study is considered Class I Medical Evidence by the spinal cord injury Committee of the 
                      American Association of Neurological Surgeons (AANS) and the Congress of Neurological Surgeons (CNS). Subsequent analyses of the Sygen 
                      data have been performed to characterize the trajectory and extent of spontaneous recovery from acute spinal cord injury.",
                      br()
                    ), # close box
                    
                    fluidRow(
                      valueBox(prettyNum(797, big.mark=" ", scientific=FALSE), "Patients", icon = icon("user-edit"), width = 3, color = "blue"),
                      #valueBox(prettyNum(489, big.mark=" ", scientific=FALSE), "Unique concomittant medications to treat secondary complications", icon = icon("pills"), width = 3,  color = "blue"),
                      #valueBox(tagList("10", tags$sup(style="font-size: 20px", "%")),
                      #         "Prophylactic medication use", icon = icon("prescription"),  width = 3,  color = "blue"
                      #),
                      valueBox("28", "North American clinical sites", icon = icon("clinic-medical"), width = 3,  color = "blue"),
                      valueBox("1991-1997", "Running time", icon = icon("calendar-alt"), width = 3,  color = "blue")#,
                    )
                  ), # close fluid row
                  fluidRow(
                    box(#title = "Explore The Data", 
                      width = 8, 
                      heigth = "500px",
                      solidHeader = TRUE,
                      
                      h4("References"),
                      
                      tags$ul(
                        tags$li(a('Geisler et al, 2001', href = 'https://europepmc.org/article/med/11805612', target="_blank"), "Recruitment and early treatment in a multicenter study of acute spinal cord injury. Spine (Phila Pa 1976)."),
                        tags$li(a('Geisler et al, 2001', href = 'https://journals.lww.com/spinejournal/Fulltext/2001/12151/The_Sygen_R__Multicenter_Acute_Spinal_Cord_Injury.15.aspx', target="_blank"), "The Sygen multicenter acute spinal cord injury study. Spine (Phila Pa 1976)")
                      ) # close tags
                    ) # close box
                  ) # close fluid row
                  
          ), # close tab item
          
          tabItem(tabName = "epi_sygen",
                  fluidRow(
                    column (width = 8,
                            
                            box(status = "primary", width = NULL,
                                
                                div(style="display:inline-block;width:100%;text-align:center;",
                                    radioGroupButtons(
                                      inputId = "var_epi_sygen", 
                                      label = "Epidemiological features:", 
                                      selected = "sex_sygen",
                                      status = "success",
                                      individual = T, #if false, then the boxes are connected
                                      choiceNames = c("Sex", "Age", "AIS grade", "Level of injury"),
                                      choiceValues = c("sex_sygen", "age_sygen", "ais_sygen", "nli_sygen")
                                      ) # Close radioGroupButtons bracket
                                    ), # Close div bracket
                            ),
                            
                            box(status = "primary", width = NULL,
                                
                                div(plotOutput("plot.epi.sygen", width = "90%",
                                                 height = "660px",
                                                 inline = FALSE), 
                                    align='center')
                                ), # end box 
                            ), # end column
                    
                    column(width = 4, # create second column for second type of user inputs (filters)
                           
                           box(status = "primary", width = NULL, # create a new box
                               sliderInput(inputId = "year_epi_sygen", # create new slider text
                                           label = "Years of injury to display:", # label of the box
                                           min = 1991, max = 1997,
                                           value = c(1991,1997),
                                           sep = "")
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create a new box
                               sliderInput(inputId = "binyear_epi_sygen", # create new slider text
                                           label = "Number of years per bin:", # label of the box
                                           min = 1, max = 3,
                                           value = 1)
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create box 
                               radioButtons("checkbox_epi_sygen",
                                            label = "Do you want to inspect subpopulations ?",
                                            choices = c("No" = 1, 'Yes' = 0), 
                                            selected = 1) # checkbox to go to basic plot: no filters, Sygen patients, x-axis=stages, y-axis=value of score
                           ), # end box
                           
                           conditionalPanel(condition = "input.checkbox_epi_sygen == 0", # if user decides to not display Sygen data and apply filters, make a new panel appear
                                            box(status = "primary", width = NULL, # create new box
                                                radioButtons(inputId = "checkGroup_epi_sygen", # create new check box group
                                                             label = "Criteria:", # label of the box
                                                             choices = list("AIS grade" = '1', "Paralysis type" = '2'), # choices are the different filters that can be applied
                                                             selected = c('1')) # by default, NLI and AIS grades are selected
                                            ) # end box
                           ), # end conditionalPanel
                           
                           conditionalPanel(condition = "input.checkbox_epi_sygen == 0 && input.checkGroup_epi_sygen.includes('1')", # if user chooses to filter based on AIS grade
                                            box(status = "primary", width = NULL, # create a new box
                                                checkboxGroupInput(inputId = "grade_epi_sygen", # create new check box group 
                                                                   label = "AIS grade:", # label of the box
                                                                   choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automaticSygeny be removed
                                                                   selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, Sygen grades are selected but missing grades
                                            ) # end box
                           ), # end conditionalPanel
                           
                           conditionalPanel(condition = "input.checkbox_epi_sygen == 0 && input.checkGroup_epi_sygen.includes('2')", # if user chooses to filter based on functlogical level of injury
                                            box(status = "primary", width = NULL, # create a new box
                                                checkboxGroupInput(inputId = "paralysis_epi_sygen", # create new check box group 
                                                                   label = "Type of paralysis:", # label of the box
                                                                   choices = list("paraplegia", 'tetraplegia'), 
                                                                   selected = c("paraplegia", 'tetraplegia'))
                                            ) # end box
                           ) # end conditionalPanel
                    ) #end column
                    )  #close fluid row
                  ),   #close tabitem epi_sygen
          
          tabItem(tabName = "neuro_sygen",
                  fluidRow(
                    column (width = 8,
                            
                            box(status = "primary", width = NULL,
                                
                                div(style="display:inline-block;width:100%;text-align:center;",
                                    radioGroupButtons(
                                      inputId = "var_neuro_sygen", 
                                      label = "Neurological features:", 
                                      selected = "UEMS",
                                      status = "success",
                                      individual = T, #if false, then the boxes are connected
                                      choiceNames = c("UEMS", "LEMS", "TEMS",
                                                      "RMS", "LMS", "TMS",
                                                      "RPP","LPP","TPP",
                                                      "RLT","LLT","TLT"),
                                      choiceValues = c("UEMS", "LEMS", "TEMS",
                                                       "RMS", "LMS", "TMS",
                                                       "RPP","LPP","TPP",
                                                       "RLT","LLT","TLT")
                                    ) # Close radioGroupButtons bracket
                                ), # Close div bracket
                            ),
                            
                            box(status = "primary", width = NULL,
                                
                                div(plotlyOutput("plot.neuro.sygen", width = "90%",
                                               height = "660px",
                                               inline = FALSE), 
                                    align='center')
                            ), # end box 
                    ), # end column
                    
                    column(width = 4, # create second column for second type of user inputs (filters)
                           
                           box(status = "primary", width = NULL, # create box 
                               radioButtons("cont_neuro_Sygen",
                                            label = "Type of plot ?",
                                            choices = c("Trend plot" = 0, 'Boxplot' = 1), 
                                            selected = 0)
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create box 
                               radioButtons("subset_neuro_Sygen",
                                            label = "Select subset of the cohort ?",
                                            choices = c("No" = 1, 'Yes' = 0), 
                                            selected = 1), # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                               
                               conditionalPanel(condition = "input.subset_neuro_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                checkboxGroupInput(inputId = "checksub_neuro_Sygen", # create new check box group
                                                                   label = "Subsetting criteria:", # label of the box
                                                                   choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "Level of injury" = '5'),
                                                                   selected = c('1', '4')) # by default, sex and AIS grades are selected
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_neuro_Sygen == 0 && input.checksub_neuro_Sygen.includes('1')", # if user chooses to filter based on sex
                                                checkboxGroupInput(inputId = "sex_neuro_Sygen", # create new check box group
                                                                   label = "Sex:", # label of the box
                                                                   choices = list("Male", "Female"), # choices are male and female (match the levels in Sygen dataset)
                                                                   selected = c("Male", "Female")) # by default, both male and female are selected
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_neuro_Sygen == 0 && input.checksub_neuro_Sygen.includes('2')", # if user chooses to filter based on age
                                                checkboxGroupInput(inputId = "age_neuro_Sygen", # create new check box group
                                                                   label = "Age at injury:", # label of the box
                                                                   choices = list("0-19", "20-39", "40-59", "60-79"),
                                                                   selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_neuro_Sygen == 0 && input.checksub_neuro_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                checkboxGroupInput(inputId = "cause_neuro_Sygen", # create new check box group
                                                                   label = "Cause of injury:", # label of the box
                                                                   choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"),
                                                                   selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"))
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_neuro_Sygen == 0 && input.checksub_neuro_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                checkboxGroupInput(inputId = "grade_neuro_Sygen", # create new check box group
                                                                   label = "AIS grade:", # label of the box
                                                                   choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                   selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_neuro_Sygen == 0 && input.checksub_neuro_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                checkboxGroupInput(inputId = "level_neuro_Sygen", # create new check box group
                                                                   label = "Neurological level of injury:", # label of the box
                                                                   choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                   selected = c("cervical", "thoracic")) # by default, all categories are selected
                               ) # end conditionalPanel
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create box 
                               radioButtons("checkbox_neuro_Sygen",
                                            label = "How do you want to visualise the data?",
                                            choices = c("Default" = 1, 'Customize (display by subgroups)' = 0), 
                                            selected = 1), # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                               conditionalPanel(condition = "input.checkbox_neuro_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                checkboxGroupInput(inputId = "checkGroup_neuro_Sygen", # create new check box group
                                                                   label = "Visualisation criteria:", # label of the box
                                                                   choices = list("Sex" = '1', 
                                                                                  "Age at injury" = '2', 
                                                                                  "Cause of SCI" = '3', 
                                                                                  "AIS grade" = '4', 
                                                                                  "Level of injury" = '5'), # choices are the different filters that can be applied
                                                                   selected = c('1','4')) # by default, sex and AIS grades are selected
                               ), # end conditionalPanel
                           ), # end box
                           
                           box(status = "primary", width = NULL,

                               radioButtons(inputId = "neuro_choose_time_Sygen",
                                            label = "Choose time displayed:",
                                            choices = c("Single time point" = "single",
                                                        "Time range" = "multiple"),
                                            selected = "multiple"),

                               conditionalPanel(condition = "input.neuro_choose_time_Sygen == 'multiple'",
                                                sliderTextInput(inputId = "neuro_time_multiple_Sygen",
                                                                #label = "Time range:",
                                                                label = NULL,
                                                                choices = c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"),
                                                                selected = c("Week00", "Week52"),
                                                                animate = F, grid = TRUE, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                               ), # end conditionalPanel

                               conditionalPanel(condition = "input.neuro_choose_time_Sygen == 'single'",
                                                sliderTextInput(inputId = "neuro_time_single_Sygen",
                                                                label = "Time point:",
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"),
                                                                selected = c("Week00"),
                                                                animate = F, grid = TRUE, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, dragRange = TRUE)
                               ) # end conditionalPanel
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create a new box
                               sliderInput(inputId = "year_neuro_sygen", # create new slider text
                                           label = "Years of injury to display:", # label of the box
                                           min = 1991, max = 1997,
                                           value = c(1991,1997),
                                           sep = "")
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create a new box
                               sliderInput(inputId = "binyear_neuro_sygen", # create new slider text
                                           label = "Number of years per bin:", # label of the box
                                           min = 1, max = 3,
                                           value = 1)
                           ), # end box
                           
                    ) #end column
                  )  #close fluid row
          ), #end tabItem
          
          tabItem(tabName = "funct_sygen",
                  fluidRow(
                    column (width = 8,
                            
                            box(status = "primary", width = NULL,
                                
                                div(style="display:inline-block;width:100%;text-align:center;",
                                    radioGroupButtons(
                                      inputId = "var_funct_sygen", 
                                      label = "Functional features:", 
                                      selected = "Benzel",
                                      status = "success",
                                      individual = T, #if false, then the boxes are connected
                                      choiceNames = c('Modified Benzel score'),
                                      choiceValues = c("Benzel")
                                    ) # Close radioGroupButtons bracket
                                ), # Close div bracket
                            ),
                            box(status = "primary", width = NULL,
                                div(plotlyOutput("plot.funct.sygen", width = "90%",
                                               height = "660px",
                                               inline = FALSE), 
                                    align='center')
                            ), # end box 
                    ), # end column
                    
                    column(width = 4, # create second column for second type of user inputs (filters)
                           
                           box(status = "primary", width = NULL, # create box 
                               radioButtons("cont_funct_Sygen",
                                            label = "Type of plot ?",
                                            choices = c("Trend plot" = 0, 'Boxplot' = 1), 
                                            selected = 0)
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create box 
                               radioButtons("subset_funct_Sygen",
                                            label = "Select subset of the cohort ?",
                                            choices = c("No" = 1, 'Yes' = 0), 
                                            selected = 1), # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                               
                               conditionalPanel(condition = "input.subset_funct_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                checkboxGroupInput(inputId = "checksub_funct_Sygen", # create new check box group
                                                                   label = "Subsetting criteria:", # label of the box
                                                                   choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "Level of injury" = '5'),
                                                                   selected = c('1', '4')) # by default, sex and AIS grades are selected
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_funct_Sygen == 0 && input.checksub_funct_Sygen.includes('1')", # if user chooses to filter based on sex
                                                checkboxGroupInput(inputId = "sex_funct_Sygen", # create new check box group
                                                                   label = "Sex:", # label of the box
                                                                   choices = list("Male", "Female"), # choices are male and female (match the levels in Sygen dataset)
                                                                   selected = c("Male", "Female")) # by default, both male and female are selected
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_funct_Sygen == 0 && input.checksub_funct_Sygen.includes('2')", # if user chooses to filter based on age
                                                checkboxGroupInput(inputId = "age_funct_Sygen", # create new check box group
                                                                   label = "Age at injury:", # label of the box
                                                                   choices = list("0-19", "20-39", "40-59", "60-79"),
                                                                   selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_funct_Sygen == 0 && input.checksub_funct_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                checkboxGroupInput(inputId = "cause_funct_Sygen", # create new check box group
                                                                   label = "Cause of injury:", # label of the box
                                                                   choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"),
                                                                   selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"))
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_funct_Sygen == 0 && input.checksub_funct_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                checkboxGroupInput(inputId = "grade_funct_Sygen", # create new check box group
                                                                   label = "AIS grade:", # label of the box
                                                                   choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                   selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.subset_funct_Sygen == 0 && input.checksub_funct_Sygen.includes('5')", # if user chooses to filter based on functlogical level of injury
                                                checkboxGroupInput(inputId = "level_funct_Sygen", # create new check box group
                                                                   label = "Neurological level of injury:", # label of the box
                                                                   choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                   selected = c("cervical", "thoracic")) # by default, all categories are selected
                               ) # end conditionalPanel
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create box 
                               radioButtons("checkbox_funct_Sygen",
                                            label = "How do you want to visualise the data?",
                                            choices = c("Default" = 1, 'Customize (display by subgroups)' = 0), 
                                            selected = 1), # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                               conditionalPanel(condition = "input.checkbox_funct_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                checkboxGroupInput(inputId = "checkGroup_funct_Sygen", # create new check box group
                                                                   label = "Visualisation criteria:", # label of the box
                                                                   choices = list("Sex" = '1', 
                                                                                  "Age at injury" = '2', 
                                                                                  "Cause of SCI" = '3', 
                                                                                  "AIS grade" = '4', 
                                                                                  "Level of injury" = '5'), # choices are the different filters that can be applied
                                                                   selected = c('1','4')) # by default, sex and AIS grades are selected
                               ), # end conditionalPanel
                           ), # end box
                           
                           box(status = "primary", width = NULL,
                               
                               radioButtons(inputId = "funct_choose_time_Sygen",
                                            label = "Choose time displayed:",
                                            choices = c("Single time point" = "single",
                                                        "Time range" = "multiple"),
                                            selected = "multiple"),
                               
                               conditionalPanel(condition = "input.funct_choose_time_Sygen == 'multiple'",
                                                sliderTextInput(inputId = "funct_time_multiple_Sygen",
                                                                #label = "Time range:",
                                                                label = NULL,
                                                                choices = c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"),
                                                                selected = c("Week00", "Week52"),
                                                                animate = F, grid = TRUE, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                               ), # end conditionalPanel
                               
                               conditionalPanel(condition = "input.funct_choose_time_Sygen == 'single'",
                                                sliderTextInput(inputId = "funct_time_single_Sygen",
                                                                label = "Time point:",
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"),
                                                                selected = c("Week00"),
                                                                animate = F, grid = TRUE, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, dragRange = TRUE)
                               ) # end conditionalPanel
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create a new box
                               sliderInput(inputId = "year_funct_sygen", # create new slider text
                                           label = "Years of injury to display:", # label of the box
                                           min = 1991, max = 1997,
                                           value = c(1991,1997),
                                           sep = "")
                           ), # end box
                           
                           box(status = "primary", width = NULL, # create a new box
                               sliderInput(inputId = "binyear_funct_sygen", # create new slider text
                                           label = "Number of years per bin:", # label of the box
                                           min = 1, max = 3,
                                           value = 1)
                           ), # end box
                    ) #end column
                  )  #close fluid row
          ), #end tabItem
        
        tabItem(tabName = "neuro_all",
                fluidRow(
                  column (width = 8,
                          
                          box(status = "primary", width = NULL,
                              
                              div(style="display:inline-block;width:100%;text-align:center;",
                                  radioGroupButtons(
                                    inputId = "var_neuro_all", 
                                    label = "Neurological features:", 
                                    selected = "UEMS",
                                    status = "success",
                                    individual = T, #if false, then the boxes are connected
                                    choiceNames = c("UEMS", "LEMS",
                                                    "RMS", "LMS", "TMS",
                                                    "RPP","LPP","TPP",
                                                    "RLT","LLT","TLT"),
                                    choiceValues = c("UEMS", "LEMS",
                                                     "RMS", "LMS", "TMS",
                                                     "RPP","LPP","TPP",
                                                     "RLT","LLT","TLT")
                                  ) # Close radioGroupButtons bracket
                              ), # Close div bracket
                          ),
                          
                          box(status = "primary", width = NULL,
                              div(plotOutput("plot.neuro.all", width = "90%",
                                             height = "660px",
                                             inline = FALSE), 
                                  align='center')
                          ), # end box 
                  ), # end column
                  
                  column(width = 4, # create second column for second type of user inputs (filters)
                         
                         box(status = "primary", width = NULL, # create box
                             radioButtons("subset_neuro_All",
                                          label = "Select subset of the cohort ?",
                                          choices = c("No" = 1, 'Yes' = 0),
                                          selected = 1), # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                             
                             conditionalPanel(condition = "input.subset_neuro_All == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                              checkboxGroupInput(inputId = "checksub_neuro_All", # create new check box group
                                                                 label = "Subsetting criteria:", # label of the box
                                                                 choices = list("Sex" = '1', "Age at injury" = '2', "AIS grade" = '3', "Level of injury" = '4'),
                                                                 selected = NULL) # by default, sex and AIS grades are selected
                             ), # end conditionalPanel
                             
                             conditionalPanel(condition = "input.subset_neuro_All == 0 && input.checksub_neuro_All.includes('1')", # if user chooses to filter based on sex
                                              checkboxGroupInput(inputId = "sex_neuro_All", # create new check box group
                                                                 label = "Sex:", # label of the box
                                                                 choices = list("Male", "Female"), # choices are male and female (match the levels in All dataset)
                                                                 selected = c("Male", "Female")) # by default, both male and female are selected
                             ), # end conditionalPanel
                             
                             conditionalPanel(condition = "input.subset_neuro_All == 0 && input.checksub_neuro_All.includes('2')", # if user chooses to filter based on age
                                              checkboxGroupInput(inputId = "age_neuro_All", # create new check box group
                                                                 label = "Age at injury:", # label of the box
                                                                 choices = list("0-19", "20-39", "40-59", "60-79", "80+"),
                                                                 selected = c("0-19", "20-39", "40-59", "60-79", "80+")) # by default, all categories are selected
                             ), # end conditionalPanel
                             
                             conditionalPanel(condition = "input.subset_neuro_All == 0 && input.checksub_neuro_All.includes('3')", # if user chooses to filter based on AIS grade
                                              checkboxGroupInput(inputId = "grade_neuro_All", # create new check box group
                                                                 label = "AIS grade:", # label of the box
                                                                 choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in All dataset), missing AIS grades will automatically be removed
                                                                 selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                             ), # end conditionalPanel
                             
                             conditionalPanel(condition = "input.subset_neuro_All == 0 && input.checksub_neuro_All.includes('4')", # if user chooses to filter based on neurological level of injury
                                              checkboxGroupInput(inputId = "level_neuro_All", # create new check box group
                                                                 label = "Neurological level of injury:", # label of the box
                                                                 choices = list("cervical", "lumbar", 'sacral', "thoracic"), # choices (match the levels in All dataset)
                                                                 selected = c("cervical", "lumbar", 'sacral', "thoracic")) # by default, all categories are selected
                             ) # end conditionalPanel
                         ), # end box
                         
                         box(status = "primary", width = NULL, # create box
                             radioButtons("checkbox_neuro_All",
                                          label = "How do you want to visualise the data?",
                                          choices = c("Default" = 1, 'Customize (display by subgroups)' = 0),
                                          selected = 1), # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                             conditionalPanel(condition = "input.checkbox_neuro_All == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                              checkboxGroupInput(inputId = "checkGroup_neuro_All", # create new check box group
                                                                 label = "Visualisation criteria:", # label of the box
                                                                 choices = list("Sex" = '1',
                                                                                "Age at injury" = '2',
                                                                                "AIS grade" = '3',
                                                                                "Level of injury" = '4'), # choices are the different filters that can be applied
                                                                 selected = c('1','4')) # by default, sex and AIS grades are selected
                             ), # end conditionalPanel
                         ), # end box
                  ) # end column
                  
                )  #close fluid row
        ), #end tabItem
          
          tabItem(tabName = "abbreviations",
                  titlePanel(strong("Dictionary of abbreviations")),
                  fluidRow(
                    column(width = 6,
                           box(width = NULL, status = "primary",
                             h4(strong('General')),
                             p(strong('SCI'), 'spinal cord injury'),
                             p(strong(a('ASIA', href ="https://asia-spinalinjury.org/", target="_blank")), 'american spinal injury association'),
                             p(strong(a('EMSCI', href ="https://www.emsci.org/", target="_blank")), 'european multicenter study about spinal cord injury'),
                             p(strong('PBE'), 'practice-based evidence')
                            ),
                           
                           box(width = NULL, status = "primary",
                               h4(strong('Functional outcomes')),
                               p(strong(a('WISCI', href = "http://www.spinalcordcenter.org/research/wisci_guide.pdf", target="_blank")), 'walking index for spinal cord injury'),
                               p(strong(a('test_6min', href = "https://www.emsci.org/index.php/project/the-assessments/functional-test", target="_blank")), '6 minutes walking test'),
                               p(strong(a('test_10m', href = "https://www.emsci.org/index.php/project/the-assessments/functional-test", target="_blank")), '10 meters walking test'),
                               p(strong(a('TUG', href = "https://www.emsci.org/index.php/project/the-assessments/functional-test", target="_blank")), 'timed up and go test'),
                               p(strong(a('SCIM2', href = "https://www.emsci.org/index.php/project/the-assessments/independence", target="_blank")), 'spinal cord independence measure type 2'),
                               p(strong(a('SCIM3', href = "https://www.emsci.org/index.php/project/the-assessments/independence", target="_blank")), 'spinal cord independence measure type 3'),
                               p(strong('benzel'), 'modified benzel classification')
                           )
                           
                    ), # end column
                    
                    column(width = 6,
                           box(width = NULL, status = "primary",
                             h4(strong(a('Neurological outcomes', href ="https://asia-spinalinjury.org/wp-content/uploads/2016/02/International_Stds_Diagram_Worksheet.pdf", target="_blank"))),
                             p(strong(a('AIS', href ='https://www.icf-casestudies.org/introduction/spinal-cord-injury-sci/american-spinal-injury-association-asia-impairment-scale#:~:text=The%20American%20Spinal%20Injury%20Association,both%20sides%20of%20the%20body', target="_blank")), 'ASIA impairment scale'),
                             p(strong('UEMS'), 'upper extremity motor score'),
                             p(strong('RUEMS'), 'right upper extremity motor score'),
                             p(strong('LUEMS'), 'left upper extremity motor score'),
                             p(strong('LEMS'), 'lower extremity motor score'),
                             p(strong('RLEMS'), 'right lower extremity motor score'),
                             p(strong('LLEMS'), 'left lower extremity motor score'),
                             p(strong('RMS'), 'right motor score'),
                             p(strong('LMS'), 'left motor score'),
                             p(strong('TMS'), 'total motor score'),
                             p(strong('RPP'), 'right pin prick'),
                             p(strong('LPP'), 'left pin prick'),
                             p(strong('TPP'), 'total pin prick'),
                             p(strong('RLT'), 'right light touch'),
                             p(strong('LLT'), 'left light touch'),
                             p(strong('TLT'), 'total light touch')
                           )
                           
                      ) # end column
                    ) # end fluidRow
                  ), # end tabItem
        ) # end tabitems
      ) #end dashboardBody
) # end dashboardPage



ui <- secure_app(ui)


# Server logic ----
server <- function(input, output) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  output$plot.funct.sygen <- renderPlotly({
    
    years <- c(unique(input$year_funct_sygen)[1]:unique(input$year_funct_sygen)[2])
    nb_bins <- unique(input$binyear_funct_sygen)[1]
    
    data_modified <- data_sygen[data_sygen$Year_of_injury %in% years, ]
    ########################################################################################## 
    vector_temp <- seq(input$year_funct_sygen[1],(input$year_funct_sygen[2]+input$year_funct_sygen[2]%%nb_bins),nb_bins)
    vector_temp <- ifelse(vector_temp > input$year_funct_sygen[2],input$year_funct_sygen[2],vector_temp)
    if (!(input$year_funct_sygen[2] %in% vector_temp)){
      vector_temp <- append(vector_temp, input$year_funct_sygen[2])
    }
    
    data_modified$Year_of_injury_cat<-cut(data_modified$Year_of_injury,
                                          c(unique(vector_temp)),
                                          include.lowest = T, right = F)
    ##########################################################################################
    
    type_plot <- input$cont_funct_Sygen[1]
    
    # FILTERING BASED ON STAGES TO DISPLAY #
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    if (input$funct_choose_time_Sygen == 'single'){
      times <- unique(input$funct_time_single_Sygen)
      indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
      list_time = list_all_stages[c(indices_time)]
    } else if (input$funct_choose_time_Sygen == 'multiple'){
      times <- unique(input$funct_time_multiple_Sygen)
      indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
      list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    }
    
    if (input$subset_funct_Sygen == 0){
      input_sex <- unique(input$sex_funct_Sygen)
      input_age <- unique(input$age_funct_Sygen)
      input_cause <- unique(input$cause_funct_Sygen)
      input_ais <- unique(input$grade_funct_Sygen)
      input_nli <- unique(input$level_funct_Sygen)
      
      #data_modified <- data_sygen
      
      if (!('Unknown' %in% input_sex)){data_modified <- data_modified[data_modified$Sex %in% input_sex, ]}
      if (!('Unknown' %in% input_age)){data_modified <- data_modified[data_modified$Age %in% input_age, ]}
      if (!('Unknown' %in% input_ais)){data_modified <- data_modified[data_modified$AIS %in% input_ais, ]}
      if (!('Unknown' %in% input_cause)){data_modified <- data_modified[data_modified$Cause %in% input_cause, ]}
      if (!('Unknown' %in% input_nli)){data_modified <- data_modified[data_modified$NLI %in% input_nli, ]}
      
      data_sygen_copy <- data_modified
      
    } else {
      data_sygen_copy <- data_modified
    }
    
    if (input$checkbox_funct_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen_copy, score = input$var_funct_sygen, list_time, type_plot) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_funct_Sygen == 0){ # if user chooses filters
      if (!(length(input$checkGroup_funct_Sygen) == 2)){ # if user chooses something else than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_funct_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_funct_Sygen))) # store filters the user has selected
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters        
        plot <- plot_filters_Sygen(data_sygen_copy, score = input$var_funct_sygen, # call function for emsci plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   filter1_all, 
                                   filter2_all, type_plot)
      }
    }
    
    plot})
  
  output$plot.funct.emsci <- renderPlotly({
    
    years <- c(unique(input$year_funct_emsci)[1]:unique(input$year_funct_emsci)[2])
    nb_bins <- unique(input$binyear_funct_emsci)[1]
    
    data_modified <- data_emsci[data_emsci$YEARDOI %in% years, ]
    data_modified$YEARDOI_cat<-cut(data_modified$YEARDOI, 
                                   seq(input$year_funct_emsci[1],
                                       input$year_funct_emsci[2]+input$year_funct_emsci[2]%%nb_bins,
                                       nb_bins),
                                   include.lowest = T, right = F)
    
    type_plot <- input$cont_funct_emsci[1]
    
    # FILTERING BASED ON STAGES TO DISPLAY #
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic") # store all potential stages
    if (input$funct_choose_time_EMSCI == 'single'){
      times <- unique(input$funct_time_single_EMSCI)
      indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
      list_time = list_all_stages[c(indices_time)]
    } else if (input$funct_choose_time_EMSCI == 'multiple'){
      times <- unique(input$funct_time_multiple_EMSCI)
      indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
      list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    }
    
    if (input$subset_funct_EMSCI == 0){
      input_sex <- unique(input$sex_funct_EMSCI)
      input_age <- unique(input$age_funct_EMSCI)
      input_cause <- unique(input$cause_funct_EMSCI)
      input_ais <- unique(input$grade_funct_EMSCI)
      input_nli <- unique(input$level_funct_EMSCI)
      input_country <- unique(input$country_funct_EMSCI)
      # input_year <- unique(input$filteryear_funct_EMSCI)
      
      #data_modified <- data_emsci
      
      if (!('Unknown' %in% input_sex)){data_modified <- data_modified[data_modified$Sex %in% input_sex, ]}
      if (!('Unknown' %in% input_age)){data_modified <- data_modified[data_modified$age_category %in% input_age, ]}
      if (!('Unknown' %in% input_ais)){data_modified <- data_modified[data_modified$AIS %in% input_ais, ]}
      if (!('Unknown' %in% input_cause)){data_modified <- data_modified[data_modified$Cause %in% input_cause, ]}
      if (!('Unknown' %in% input_nli)){data_modified <- data_modified[data_modified$NLI_level %in% input_nli, ]}
      if (!('Unknown' %in% input_country)){data_modified <- data_modified[data_modified$Country %in% input_country, ]}
      
      all_years <- c(2000, 2005, 2010, 2015, 2019)
      temp <- which(all_years %in% unlist(input_year, use.names=FALSE))
      int_filter = c(all_years[c(temp[1]:temp[2])])
      vector_year <- c()
      if (2000 %in% int_filter && 2005 %in% int_filter){vector_year <- c(vector_year, '2000-2004')}
      if (2005 %in% int_filter && 2010 %in% int_filter){vector_year <- c(vector_year, '2005-2009')}
      if (2010 %in% int_filter && 2015 %in% int_filter){vector_year <- c(vector_year, '2010-2014')}
      if (2015 %in% int_filter && 2019 %in% int_filter){vector_year <- c(vector_year, '2015-2019')}
      data_modified <- data_modified[data_modified$YEARDOI_cat %in% vector_year, ]
      
      data_emsci_copy <- data_modified
      
    } else {
      data_emsci_copy <- data_modified
    }
    
    if (input$checkbox_funct_EMSCI == 1){ # if user chooses to display all data
      plot <- plot_base_emsci(data_emsci_copy, score = input$var_funct_emsci, list_time, type_plot) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_funct_EMSCI == 0){ # if user chooses filters
      if (!(length(input$checkGroup_funct_EMSCI) == 2)){ # if user chooses something else than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_funct_EMSCI) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_funct_EMSCI))) # store filters the user has selected
        list_all = list(c("Male", "Female"), # store all the different options for the different filters
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany",
                          "Great Britain", "India", "Italy", "Netherlands",
                          "Spain", "Switzerland"),
                        c('2000-2004', '2005-2009', '2010-2014', '2015-2019'))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat") # names of columns corresponding to the available filters
        
        plot <- plot_filters_emsci(data_emsci_copy, score = input$var_funct_emsci, # call function for emsci plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   filter1_all, 
                                   filter2_all, type_plot)
      }
    }
  plot})
  
  output$plot.neuro.sygen <- renderPlotly({
    # FILTERING BASED ON STAGES TO DISPLAY #
    
    years <- c(unique(input$year_neuro_sygen)[1]:unique(input$year_neuro_sygen)[2])
    nb_bins <- unique(input$binyear_neuro_sygen)[1]
    
    data_modified <- data_sygen[data_sygen$Year_of_injury %in% years, ]
    ########################################################################################## 
    vector_temp <- seq(input$year_neuro_sygen[1],(input$year_neuro_sygen[2]+input$year_neuro_sygen[2]%%nb_bins),nb_bins)
    vector_temp <- ifelse(vector_temp > input$year_neuro_sygen[2],input$year_neuro_sygen[2],vector_temp)
    if (!(input$year_neuro_sygen[2] %in% vector_temp)){
      vector_temp <- append(vector_temp, input$year_neuro_sygen[2])
    }
    
    data_modified$Year_of_injury_cat<-cut(data_modified$Year_of_injury,
                                          c(unique(vector_temp)),
                                          include.lowest = T, right = F)
    ##########################################################################################
    
    type_plot <- input$cont_neuro_Sygen[1]
    
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    if (input$neuro_choose_time_Sygen == 'single'){
      times <- unique(input$neuro_time_single_Sygen)
      indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
      list_time = list_all_stages[c(indices_time)]
    } else if (input$neuro_choose_time_Sygen == 'multiple'){
      times <- unique(input$neuro_time_multiple_Sygen)
      indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
      list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    }
    
    if (input$subset_neuro_Sygen == 0){
      #print('test')
      #print(dim(data_modified)[1])
      input_sex <- unique(input$sex_neuro_Sygen)
      input_age <- unique(input$age_neuro_Sygen)
      input_cause <- unique(input$cause_neuro_Sygen)
      input_ais <- unique(input$grade_neuro_Sygen)
      input_nli <- unique(input$level_neuro_Sygen)
      
      if (!('Unknown' %in% input_sex)){data_modified <- data_modified[data_modified$Sex %in% input_sex, ]}
      if (!('Unknown' %in% input_age)){data_modified <- data_modified[data_modified$Age %in% input_age, ]}
      if (!('Unknown' %in% input_ais)){data_modified <- data_modified[data_modified$AIS %in% input_ais, ]}
      if (!('Unknown' %in% input_cause)){data_modified <- data_modified[data_modified$Cause %in% input_cause, ]}
      if (!('Unknown' %in% input_nli)){data_modified <- data_modified[data_modified$NLI %in% input_nli, ]}
      
      data_sygen_copy <- data_modified
      #print(dim(data_sygen_copy)[1])
      
    } else {
      data_sygen_copy <- data_modified
    }
    
    if (input$checkbox_neuro_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen_copy, score = input$var_neuro_sygen, list_time, type_plot) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_neuro_Sygen == 0){ # if user chooses filters
      if (!(length(input$checkGroup_neuro_Sygen) == 2)){ # if user chooses something else than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_neuro_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_neuro_Sygen))) # store filters the user has selected
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters        
        plot <- plot_filters_Sygen(data_sygen_copy, score = input$var_neuro_sygen, # call function for emsci plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   filter1_all, 
                                   filter2_all, type_plot)
      }
    }
    
  plot})
  
  output$plot.neuro.emsci <- renderPlotly({
    
    years <- c(unique(input$year_neuro_emsci)[1]:unique(input$year_neuro_emsci)[2])
    nb_bins <- unique(input$binyear_neuro_emsci)[1]
    
    data_modified <- data_emsci[data_emsci$YEARDOI %in% years, ]
    data_modified$YEARDOI_cat<-cut(data_modified$YEARDOI, 
                                   seq(input$year_neuro_emsci[1],
                                       input$year_neuro_emsci[2]+input$year_neuro_emsci[2]%%nb_bins,
                                       nb_bins),
                                   include.lowest = T, right = F)
    
    type_plot <- input$cont_neuro_emsci[1]
    
    # FILTERING BASED ON STAGES TO DISPLAY #
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic") # store all potential stages
    if (input$neuro_choose_time_EMSCI == 'single'){
      times <- unique(input$neuro_time_single_EMSCI)
      indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
      list_time = list_all_stages[c(indices_time)]
    } else if (input$neuro_choose_time_EMSCI == 'multiple'){
      times <- unique(input$neuro_time_multiple_EMSCI)
      indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
      list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    }
    
    if (input$subset_neuro_EMSCI == 0){
      #print('test')
      #print(dim(data_modified)[1])
      input_sex <- unique(input$sex_neuro_EMSCI)
      input_age <- unique(input$age_neuro_EMSCI)
      input_cause <- unique(input$cause_neuro_EMSCI)
      input_ais <- unique(input$grade_neuro_EMSCI)
      input_nli <- unique(input$level_neuro_EMSCI)
      input_country <- unique(input$country_neuro_EMSCI)
      # input_year <- unique(input$filteryear_neuro_EMSCI)
      
      #data_modified <- data_emsci
      
      if (!('Unknown' %in% input_sex)){data_modified <- data_modified[data_modified$Sex %in% input_sex, ]}
      if (!('Unknown' %in% input_age)){data_modified <- data_modified[data_modified$age_category %in% input_age, ]}
      if (!('Unknown' %in% input_ais)){data_modified <- data_modified[data_modified$AIS %in% input_ais, ]}
      if (!('Unknown' %in% input_cause)){data_modified <- data_modified[data_modified$Cause %in% input_cause, ]}
      if (!('Unknown' %in% input_nli)){data_modified <- data_modified[data_modified$NLI_level %in% input_nli, ]}
      if (!('Unknown' %in% input_country)){data_modified <- data_modified[data_modified$Country %in% input_country, ]}
      
      # all_years <- c(2000, 2005, 2010, 2015, 2019)
      # temp <- which(all_years %in% unlist(input_year, use.names=FALSE))
      # int_filter = c(all_years[c(temp[1]:temp[2])])
      # vector_year <- c()
      # if (2000 %in% int_filter && 2005 %in% int_filter){vector_year <- c(vector_year, '2000-2004')}
      # if (2005 %in% int_filter && 2010 %in% int_filter){vector_year <- c(vector_year, '2005-2009')}
      # if (2010 %in% int_filter && 2015 %in% int_filter){vector_year <- c(vector_year, '2010-2014')}
      # if (2015 %in% int_filter && 2019 %in% int_filter){vector_year <- c(vector_year, '2015-2019')}
      # data_modified <- data_modified[data_modified$YEARDOI_cat %in% vector_year, ]
      
      data_emsci_copy <- data_modified
      
    } else {
      data_emsci_copy <- data_modified
    }
    
    if (input$checkbox_neuro_EMSCI == 1){ # if user chooses to display all data
      plot <- plot_base_emsci(data_emsci_copy, score = input$var_neuro_emsci, list_time, type_plot) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_neuro_EMSCI == 0){ # if user chooses filters
      if (!(length(input$checkGroup_neuro_EMSCI) == 2)){ # if user chooses something else than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_neuro_EMSCI) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_neuro_EMSCI))) # store filters the user has selected
        list_all = list(c("Male", "Female"), # store all the different options for the different filters
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany",
                          "Great Britain", "India", "Italy", "Netherlands",
                          "Spain", "Switzerland"),
                        c('2000-2004', '2005-2009', '2010-2014', '2015-2019'))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat") # names of columns corresponding to the available filters
        
        plot <- plot_filters_emsci(data_emsci_copy, score = input$var_neuro_emsci, # call function for emsci plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   filter1_all, 
                                   filter2_all, type_plot)
      }
    }
    
    plot})
  
  output$plot.neuro.all <- renderPlot ({
    if (input$subset_neuro_All == 0){
      input_sex <- unique(input$sex_neuro_All)
      input_age <- unique(input$age_neuro_All)
      input_ais <- unique(input$grade_neuro_All)
      input_nli <- unique(input$level_neuro_All)
      
      data_modified <- data_emsci_sygen
      
      if (!('Unknown' %in% input_sex)){data_modified <- data_modified[data_modified$Sex %in% input_sex, ]}
      if (!('Unknown' %in% input_age)){data_modified <- data_modified[data_modified$Age %in% input_age, ]}
      if (!('Unknown' %in% input_ais)){data_modified <- data_modified[data_modified$AIS %in% input_ais, ]}
      if (!('Unknown' %in% input_nli)){data_modified <- data_modified[data_modified$NLI %in% input_nli, ]}
      
      data_All_copy <- data_modified
      
    } else {
      data_All_copy <- data_emsci_sygen
    }
    
    if (input$checkbox_neuro_All == 1){ # if user chooses to display all data
      plot <- plot_base_All(data_All_copy, score = input$var_neuro_all) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_neuro_All == 0){ # if user chooses filters
      if (!(length(input$checkGroup_neuro_All) == 2)){ # if user chooses something else than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_neuro_All) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_neuro_All))) # store filters the user has selected
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("12-19", "20-39", "40-59", "60-79", "80+"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "lumbar", "sacral", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "AIS", "NLI") # names of columns corresponding to the available filters        
        plot <- plot_filters_All(data_All_copy, score = input$var_neuro_all, # call function for All plots in helper_functions.R 
                                 list_names[filters[1]], 
                                 list_names[filters[2]],
                                 filter1_all, 
                                 filter2_all)
      }
    }
  plot})
  
  output$plot.epi.sygen <- renderPlot({
    
    years <- c(unique(input$year_epi_sygen)[1]:unique(input$year_epi_sygen)[2])
    nb_bins <- unique(input$binyear_epi_sygen)[1]
    
    data_modified <- data_sygen_epi[data_sygen_epi$yeardoi %in% years, ]
    ########################################################################################## 
    vector_temp <- seq(input$year_epi_sygen[1],(input$year_epi_sygen[2]+input$year_epi_sygen[2]%%nb_bins),nb_bins)
    vector_temp <- ifelse(vector_temp > input$year_epi_sygen[2],input$year_epi_sygen[2],vector_temp)
    if (!(input$year_epi_sygen[2] %in% vector_temp)){
      vector_temp <- append(vector_temp, input$year_epi_sygen[2])
    }
    
    data_modified$YEARDOI_cat<-cut(data_modified$yeardoi, 
                                   c(unique(vector_temp)),
                                   include.lowest = T, right = F)
    ########################################################################################## 
    
    if (input$var_epi_sygen == "sex_sygen"){
      if (input$checkbox_epi_sygen == 1){ # if user chooses to display all data
        plot <- plot_base_Sex_Sygen(data_modified, '') # display basic plot with all patients, and user selected stSexs
      }
      else if (input$checkbox_epi_sygen == 0){ # if user chooses filters
        if (as.numeric(as.vector(unique(input$checkGroup_epi_sygen))) == 1){
           if (length(unique(input$grade_epi_sygen)) == 1){
            plot <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[1]), '')
          } else if (length(unique(input$grade_epi_sygen)) == 2){
            plot.1 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[1]), paste(input$grade_epi_sygen[1]))
            plot.2 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[2]), paste(input$grade_epi_sygen[2]))
            plot <- grid.arrange(plot.1, plot.2, ncol=2)
          } else if (length(unique(input$grade_epi_sygen)) == 3){
            plot.1 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[1]), paste(input$grade_epi_sygen[1]))
            plot.2 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[2]), paste(input$grade_epi_sygen[2]))
            plot.3 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[3]), paste(input$grade_epi_sygen[3]))
            plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
          } else if (length(unique(input$grade_epi_sygen)) == 4){
            plot.1 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[1]), paste(input$grade_epi_sygen[1]))
            plot.2 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[2]), paste(input$grade_epi_sygen[2]))
            plot.3 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[3]), paste(input$grade_epi_sygen[3]))
            plot.4 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[4]), paste(input$grade_epi_sygen[4]))
            plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
          }
          
        } else if (as.numeric(as.vector(unique(input$checkGroup_epi_sygen))) == 2){
          if (length(unique(input$paralysis_epi_sygen)) == 2){
            data_modified.tetra <-subset(data_modified, plegia =='tetra')
            data_modified.para <-subset(data_modified, plegia =='para')
            plot_tetra <- plot_base_Sex_Sygen(data_modified.tetra, 'Tetraplegic')
            plot_para <- plot_base_Sex_Sygen(data_modified.para, 'Paraplegic')
            plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
          } else {
            if ('paraplegia' %in% as.vector(unique(input$paralysis_epi_sygen))){
              plot <- plot_base_Sex_Sygen(subset(data_modified, plegia =='para'), 'Paraplegic')
            } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_epi_sygen))){
              plot <- plot_base_Sex_Sygen(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
            }
            
          }
        }
      }
    }
    
    
    else if (input$var_epi_sygen == "age_sygen"){
      if (input$checkbox_epi_sygen == 1){ # if user chooses to display all data
        plot <- plot_base_Age_Sygen(data_modified, '') # display basic plot with all patients, and user selected stages
      }
      else if (input$checkbox_epi_sygen == 0){ # if user chooses filters
        if (as.numeric(as.vector(unique(input$checkGroup_epi_sygen))) == 1){
          if (length(unique(input$grade_epi_sygen)) == 1){
            plot <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[1]), '')
          } else if (length(unique(input$grade_epi_sygen)) == 2){
            plot.1 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[1]), paste(input$grade_epi_sygen[1]))
            plot.2 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[2]), paste(input$grade_epi_sygen[2]))
            plot <- grid.arrange(plot.1, plot.2, ncol=2)
          } else if (length(unique(input$grade_epi_sygen)) == 3){
            plot.1 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[1]), paste(input$grade_epi_sygen[1]))
            plot.2 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[2]), paste(input$grade_epi_sygen[2]))
            plot.3 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[3]), paste(input$grade_epi_sygen[3]))
            plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
          } else if (length(unique(input$grade_epi_sygen)) == 4){
            plot.1 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[1]), paste(input$grade_epi_sygen[1]))
            plot.2 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[2]), paste(input$grade_epi_sygen[2]))
            plot.3 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[3]), paste(input$grade_epi_sygen[3]))
            plot.4 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[4]), paste(input$grade_epi_sygen[4]))
            plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
          }
          
        } else if (as.numeric(as.vector(unique(input$checkGroup_epi_sygen))) == 2){
          if (length(unique(input$paralysis_epi_sygen)) == 2){
            data_modified.tetra <-subset(data_modified, plegia =='tetra')
            data_modified.para <-subset(data_modified, plegia =='para')
            plot_tetra <- plot_base_Age_Sygen(data_modified.tetra, 'Tetraplegic')
            plot_para <- plot_base_Age_Sygen(data_modified.para, 'Paraplegic')
            plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
          } else {
            if ('paraplegia' %in% as.vector(unique(input$paralysis_epi_sygen))){
              plot <- plot_base_Age_Sygen(subset(data_modified, plegia =='para'), 'Paraplegic')
            } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_epi_sygen))){
              plot <- plot_base_Age_Sygen(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
            }
            
          }
        }
      }
    }  
    
    
    else if (input$var_epi_sygen == 'ais_sygen')  {
     if (input$checkbox_epi_sygen == 1){ # if user chooses to display all data
      plot <- plot_base_AIS_Sygen(data_modified, '') # display basic plot with all patients, and user selected stAISs
      }
      else if (input$checkbox_epi_sygen == 0){ # if user chooses filters
        if (as.numeric(as.vector(unique(input$checkGroup_epi_sygen))) == 2){
          if (length(unique(input$paralysis_epi_sygen)) == 2){
            data_modified.tetra <-subset(data_modified, plegia =='tetra')
            data_modified.para <-subset(data_modified, plegia =='para')
            plot_tetra <- plot_base_AIS_Sygen(data_modified.tetra, 'Tetraplegic')
            plot_para <- plot_base_AIS_Sygen(data_modified.para, 'Paraplegic')
            plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
            } else {
              if ('paraplegia' %in% as.vector(unique(input$paralysis_epi_sygen))){
                plot <- plot_base_AIS_Sygen(subset(data_modified, plegia =='para'), 'Paraplegic')
            } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_epi_sygen))){
                plot <- plot_base_AIS_Sygen(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
            }
            }
        }
      }
    }

    else if (input$var_epi_sygen == "nli_sygen")  {
      if (input$checkbox_epi_sygen == 1){ # if user chooses to display all data
        plot <- plot_base_NLI_Sygen(data_modified, '') # display basic plot with all patients, and user selected stNLIs
      }
      else if (input$checkbox_epi_sygen == 0){ # if user chooses filters
        if (as.numeric(as.vector(unique(input$checkGroup_epi_sygen))) == 1){
          if (length(unique(input$grade_epi_sygen)) == 1){
            plot <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[1]), '')
          } else if (length(unique(input$grade_epi_sygen)) == 2){
            plot.1 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[1]), paste(input$grade_epi_sygen[1]))
            plot.2 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[2]), paste(input$grade_epi_sygen[2]))
            plot <- grid.arrange(plot.1, plot.2, ncol=2)
          } else if (length(unique(input$grade_epi_sygen)) == 3){
            plot.1 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[1]), paste(input$grade_epi_sygen[1]))
            plot.2 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[2]), paste(input$grade_epi_sygen[2]))
            plot.3 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[3]), paste(input$grade_epi_sygen[3]))
            plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
          } else if (length(unique(input$grade_epi_sygen)) == 4){
            plot.1 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[1]), paste(input$grade_epi_sygen[1]))
            plot.2 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[2]), paste(input$grade_epi_sygen[2]))
            plot.3 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[3]), paste(input$grade_epi_sygen[3]))
            plot.4 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_epi_sygen[4]), paste(input$grade_epi_sygen[4]))
            plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
          }
          
        } else if (as.numeric(as.vector(unique(input$checkGroup_epi_sygen))) == 2){
          if (length(unique(input$paralysis_epi_sygen)) == 2){
            data_modified.tetra <-subset(data_modified, plegia =='tetra')
            data_modified.para <-subset(data_modified, plegia =='para')
            plot_tetra <- plot_base_NLI_Sygen(data_modified.tetra, 'Tetraplegic')
            plot_para <- plot_base_NLI_Sygen(data_modified.para, 'Paraplegic')
            plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
          } else {
            if ('paraplegia' %in% as.vector(unique(input$paralysis_epi_sygen))){
              plot <- plot_base_NLI_Sygen(subset(data_modified, plegia =='para'), 'Paraplegic')
            } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_epi_sygen))){
              plot <- plot_base_NLI_Sygen(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
            }
            
          }
        }
      }
    }  
    
    
  plot})
  
  
  output$plot.epi.emsci <- renderPlot({
    
    years <- c(unique(input$year_epi_emsci)[1]:unique(input$year_epi_emsci)[2])
    nb_bins <- unique(input$binyear_epi_emsci)[1]
    
    data_modified <- data_age_emsci[data_age_emsci$YEARDOI %in% years, ]
    data_modified$YEARDOI_cat<-cut(data_modified$YEARDOI, 
                                   seq(input$year_epi_emsci[1],
                                       input$year_epi_emsci[2]+input$year_epi_emsci[2]%%nb_bins,
                                       nb_bins),
                                   include.lowest = T, right = F)
    
    if (input$var_epi_emsci == "sex_emsci"){
      if (input$checkbox_epi_emsci == 1){ # if user chooses to display all data
        plot <- plot_base_Sex_EMSCI(data_modified, '') # display basic plot with all patients, and user selected stSexs
      }
      else if (input$checkbox_epi_emsci == 0){ # if user chooses filters
        if (as.numeric(as.vector(unique(input$checkGroup_epi_emsci))) == 1){
          if (length(unique(input$grade_epi_emsci)) == 1){
            plot <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_epi_emsci[1]), '')
          } else if (length(unique(input$grade_epi_emsci)) == 2){
            plot.1 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_epi_emsci[1]), paste(input$grade_epi_emsci[1]))
            plot.2 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_epi_emsci[2]), paste(input$grade_epi_emsci[2]))
            plot <- grid.arrange(plot.1, plot.2, ncol=2)
          } else if (length(unique(input$grade_epi_emsci)) == 3){
            plot.1 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_epi_emsci[1]), paste(input$grade_epi_emsci[1]))
            plot.2 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_epi_emsci[2]), paste(input$grade_epi_emsci[2]))
            plot.3 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_epi_emsci[3]), paste(input$grade_epi_emsci[3]))
            plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
          } else if (length(unique(input$grade_epi_emsci)) == 4){
            plot.1 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_epi_emsci[1]), paste(input$grade_epi_emsci[1]))
            plot.2 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_epi_emsci[2]), paste(input$grade_epi_emsci[2]))
            plot.3 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_epi_emsci[3]), paste(input$grade_epi_emsci[3]))
            plot.4 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_epi_emsci[4]), paste(input$grade_epi_emsci[4]))
            plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
          }
          
        } else if (as.numeric(as.vector(unique(input$checkGroup_epi_emsci))) == 2){
          if (length(unique(input$paralysis_epi_emsci)) == 2){
            data_modified.tetra <-subset(data_modified, plegia =='tetra')
            data_modified.para <-subset(data_modified, plegia =='para')
            plot_tetra <- plot_base_Sex_EMSCI_paralysis(data_modified.tetra, 'Tetraplegic')
            plot_para <- plot_base_Sex_EMSCI_paralysis(data_modified.para, 'Paraplegic')
            plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
          } else {
            if ('paraplegia' %in% as.vector(unique(input$paralysis_epi_emsci))){
              plot <- plot_base_Sex_EMSCI_paralysis(subset(data_modified, plegia =='para'), 'Paraplegic')
            } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_epi_emsci))){
              plot <- plot_base_Sex_EMSCI_paralysis(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
            }
            
          }
        }
      }
    }
    
    
    else if (input$var_epi_emsci == "age_emsci"){
      if (input$checkbox_epi_emsci == 1){ # if user chooses to display all data
        plot <- plot_base_Age_EMSCI(data_modified, '') # display basic plot with all patients, and user selected stages
      }
      else if (input$checkbox_epi_emsci == 0){ # if user chooses filters
        if (as.numeric(as.vector(unique(input$checkGroup_epi_emsci))) == 1){
          if (length(unique(input$grade_epi_emsci)) == 1){
            plot <- plot_base_Age_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[1]), '')
          } else if (length(unique(input$grade_epi_emsci)) == 2){
            plot.1 <- plot_base_Age_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[1]), paste(input$grade_epi_emsci[1]))
            plot.2 <- plot_base_Age_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[2]), paste(input$grade_epi_emsci[2]))
            plot <- grid.arrange(plot.1, plot.2, ncol=2)
          } else if (length(unique(input$grade_epi_emsci)) == 3){
            plot.1 <- plot_base_Age_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[1]), paste(input$grade_epi_emsci[1]))
            plot.2 <- plot_base_Age_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[2]), paste(input$grade_epi_emsci[2]))
            plot.3 <- plot_base_Age_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[3]), paste(input$grade_epi_emsci[3]))
            plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
          } else if (length(unique(input$grade_epi_emsci)) == 4){
            plot.1 <- plot_base_Age_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[1]), paste(input$grade_epi_emsci[1]))
            plot.2 <- plot_base_Age_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[2]), paste(input$grade_epi_emsci[2]))
            plot.3 <- plot_base_Age_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[3]), paste(input$grade_epi_emsci[3]))
            plot.4 <- plot_base_Age_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[4]), paste(input$grade_epi_emsci[4]))
            plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
          }
          
        } else if (as.numeric(as.vector(unique(input$checkGroup_epi_emsci))) == 2){
          if (length(unique(input$paralysis_epi_emsci)) == 2){
            data_modified.tetra <-subset(data_modified, plegia =='tetra')
            data_modified.para <-subset(data_modified, plegia =='para')
            plot_tetra <- plot_base_Age_EMSCI(data_modified.tetra, 'Tetraplegic')
            plot_para <- plot_base_Age_EMSCI(data_modified.para, 'Paraplegic')
            plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
          } else {
            if ('paraplegia' %in% as.vector(unique(input$paralysis_epi_emsci))){
              plot <- plot_base_Age_EMSCI(subset(data_modified, plegia =='para'), 'Paraplegic')
            } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_epi_emsci))){
              plot <- plot_base_Age_EMSCI(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
            }
            
          }
        }
      }
    }  
    
    
    else if (input$var_epi_emsci == 'ais_emsci')  {
      if (input$checkbox_epi_emsci == 1){ # if user chooses to display all data
        plot <- plot_base_AIS_EMSCI(data_modified, '') # display basic plot with all patients, and user selected stAISs
      }
      else if (input$checkbox_epi_emsci == 0){ # if user chooses filters
        if (as.numeric(as.vector(unique(input$checkGroup_epi_emsci))) == 2){
          if (length(unique(input$paralysis_epi_emsci)) == 2){
            data_modified.tetra <-subset(data_modified, plegia =='tetra')
            data_modified.para <-subset(data_modified, plegia =='para')
            plot_tetra <- plot_base_AIS_EMSCI(data_modified.tetra, 'Tetraplegic')
            plot_para <- plot_base_AIS_EMSCI(data_modified.para, 'Paraplegic')
            plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
          } else {
            if ('paraplegia' %in% as.vector(unique(input$paralysis_epi_emsci))){
              plot <- plot_base_AIS_EMSCI(subset(data_modified, plegia =='para'), 'Paraplegic')
            } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_epi_emsci))){
              plot <- plot_base_AIS_EMSCI(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
            }
          }
        }
      }
    }
    
    else if (input$var_epi_emsci == "nli_emsci")  {
      if (input$checkbox_epi_emsci == 1){ # if user chooses to display all data
        plot <- plot_base_NLI_EMSCI(data_modified, '') # display basic plot with all patients, and user selected stNLIs
      }
      else if (input$checkbox_epi_emsci == 0){ # if user chooses filters
        if (as.numeric(as.vector(unique(input$checkGroup_epi_emsci))) == 1){
          if (length(unique(input$grade_epi_emsci)) == 1){
            plot <- plot_base_NLI_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[1]), '')
          } else if (length(unique(input$grade_epi_emsci)) == 2){
            plot.1 <- plot_base_NLI_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[1]), paste(input$grade_epi_emsci[1]))
            plot.2 <- plot_base_NLI_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[2]), paste(input$grade_epi_emsci[2]))
            plot <- grid.arrange(plot.1, plot.2, ncol=2)
          } else if (length(unique(input$grade_epi_emsci)) == 3){
            plot.1 <- plot_base_NLI_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[1]), paste(input$grade_epi_emsci[1]))
            plot.2 <- plot_base_NLI_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[2]), paste(input$grade_epi_emsci[2]))
            plot.3 <- plot_base_NLI_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[3]), paste(input$grade_epi_emsci[3]))
            plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
          } else if (length(unique(input$grade_epi_emsci)) == 4){
            plot.1 <- plot_base_NLI_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[1]), paste(input$grade_epi_emsci[1]))
            plot.2 <- plot_base_NLI_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[2]), paste(input$grade_epi_emsci[2]))
            plot.3 <- plot_base_NLI_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[3]), paste(input$grade_epi_emsci[3]))
            plot.4 <- plot_base_NLI_EMSCI(subset(data_modified, ais1 == input$grade_epi_emsci[4]), paste(input$grade_epi_emsci[4]))
            plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
          }
          
        } else if (as.numeric(as.vector(unique(input$checkGroup_epi_emsci))) == 2){
          if (length(unique(input$paralysis_epi_emsci)) == 2){
            data_modified.tetra <-subset(data_modified, plegia =='tetra')
            data_modified.para <-subset(data_modified, plegia =='para')
            plot_tetra <- plot_base_NLI_EMSCI(data_modified.tetra, 'Tetraplegic')
            plot_para <- plot_base_NLI_EMSCI(data_modified.para, 'Paraplegic')
            plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
          } else {
            if ('paraplegia' %in% as.vector(unique(input$paralysis_epi_emsci))){
              plot <- plot_base_NLI_EMSCI(subset(data_modified, plegia =='para'), 'Paraplegic')
            } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_epi_emsci))){
              plot <- plot_base_NLI_EMSCI(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
            }
            
          }
        }
      }
    }  
    
    
    plot})
  
  output$title_predict <- renderText({paste("<h2><b>", "Monitoring of individual patient", "</b>")})
  
  output$plot_predict <- renderPlot({ # create output function for plot of interest
    input_sex <- unique(input$select_sex)[1]
    input_age <- unique(input$select_age)[1]
    input_ais <- unique(input$select_ais)[1]
    input_nli <- unique(input$select_nli)[1]
    input_score <- unique(input$select_score)[1]
    input_indscore <- unique(input$input_compscore)[1]
    
    data_temp <- data_emsci_epi
    data_temp$UEMS <- as.numeric(as.character(data_temp$UEMS))
    data_temp$LEMS <- as.numeric(as.character(data_temp$LEMS))
    data_temp$TMS <- as.numeric(as.character(data_temp$TMS))
    
    data_modified <- data_temp
    
    if (!(input_sex == 'Unknown')){
      data_modified <- data_modified[data_modified$Sex %in% input_sex, ]
    }
    
    if (!(input_ais == 'Unknown')){
      data_modified <- data_modified[data_modified$AIS %in% input_ais, ]
    }
    
    if (!(input_nli == 'Unknown')){
      if (input_nli == 'Cervical'){
        data_modified <- data_modified[data_modified$NLI_level %in% 'cervical', ]
      } else if (input_nli == 'Thoracic'){
        data_modified <- data_modified[data_modified$NLI_level %in% 'thoracic', ]
      } else if (input_nli == 'Lumbar'){
        data_modified <- data_modified[data_modified$NLI_level %in% 'lumbar', ]
      } else {
        data_modified <- data_modified[data_modified$NLI %in% input_nli, ]
      }
    }
    
    if (!(input_age == 'Unknown')){
      if (input_age == '0-19'){
        data_modified <- data_modified[data_modified$AgeAtDOI>0 & data_modified$AgeAtDOI<20, ]
      } else if (input_age == '20-39'){
        data_modified <- data_modified[data_modified$AgeAtDOI>=20 & data_modified$AgeAtDOI<40, ]
      } else if (input_age == '40-59'){
        data_modified <- data_modified[data_modified$AgeAtDOI>=40 & data_modified$AgeAtDOI<60, ]
      } else if (input_age == '60-79'){
        data_modified <- data_modified[data_modified$AgeAtDOI>=60 & data_modified$AgeAtDOI<80, ]
      } else if (input_age == '80+'){
        data_modified <- data_modified[data_modified$AgeAtDOI>=80, ]
      }
    }
    
    data_modified <- data_modified[data_modified$UEMS != 'NA', ]
    data_modified <- data_modified[data_modified$UEMS != 'NT', ]
  
    if (dim(data_modified)[1] == 0){
      plot <- plot_error_data()
    } else if (input_indscore == "Enter value..." || input_indscore == "") {
      plot <- plot_predict_emsci(data_modified, input_score)
    } else {
      value <- as.numeric(as.character(input_indscore))
      if (value < 0 || value > 50){
        plot <- plot_error_value()
      } else {
        #plot <- plot_error_value()
        plot <- plot_predict_emsci_NN(data_modified, input_score, value)
      }
    }
    
    
    
    plot})
  
  output$video <- renderUI({
    tags$video(src = "video_version3.mp4", type = "video/mp4", autoplay = NA, controls = NA, height = 350, width = 750)
  })
  
  # observe({
  #   showNotification('Disclaimer: 
  #   We cannot be held responsible for conclusions you might draw from the website. 
  #   It is intended for data visualisation only.', type='error')
  # })
  
  observeEvent(input$preview, {
    # Show a modal when the button is pressed
    shinyalert("Disclaimer: 
    We cannot be held responsible for conclusions you might draw from the website. 
    It is intended for data visualisation only.", type = "error")
  })
  
  observeEvent(input$preview_predict, {
    # Show a modal when the button is pressed
    shinyalert("Disclaimer: 
    We cannot be held responsible for conclusions you might draw from the website. 
    It is intended for data visualisation only.", type = "error")
  })
  
}




# Run app ----
shinyApp(ui, server)