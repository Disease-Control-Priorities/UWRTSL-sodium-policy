library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(tidyr)
library(data.table)
library(shinyjs)
library(plotly)
library(networkD3)
library(ggplot2)
library(rhandsontable)
#library(vroom)
library(readxl)
#library(xlsx)
library(shinycssloaders)
library(htmlwidgets)

#############################################
### GLOBAL ENVIRONMENT ###
#############################################
b_rates<-as.data.table(read.csv("base_rates_app2.csv", stringsAsFactors=FALSE))
data.in<-as.data.table(read.csv("bp_data.csv", stringsAsFactors=FALSE))
ref<-as.data.table(read.csv("global_pop_0122.csv", stringsAsFactors=FALSE))
baseline<-as.data.table(read.csv("baseline_out_app2.csv", stringsAsFactors=FALSE))
baseline[, intervention:="Baseline"]

# Adding Ethiopia, Nigeria and Peru
b_rates_p <- as.data.table(read.csv("base_rates0122.csv", stringsAsFactors=FALSE))

common_cols <- intersect(colnames(b_rates_p), colnames(b_rates))
b_rates_p <- b_rates_p[year <= 2040 & location %in% c("Ethiopia","Nigeria","Peru"), ..common_cols]

baseline_p <- as.data.table(read.csv("baseline_out_app0122.csv", stringsAsFactors=FALSE))

baseline_p[,X:=0]
baseline_p[,intervention:="Baseline"]
baseline_p[,newcases:=IR*well]

common_cols <- intersect(colnames(baseline), colnames(baseline_p))
baseline_p <- baseline_p[year <= 2040 & location %in% c("Ethiopia","Nigeria","Peru"), ..common_cols]

# b_rates_p <- b_rates[location=="Colombia",]
# b_rates_p[,location:="Peru"]
# 
# baseline_p <- baseline[location=="Colombia",]
# baseline_p[,location:="Peru"]

b_rates <- rbind(b_rates,b_rates_p)
baseline <- rbind(baseline,baseline_p)

currency<-read.csv("currencies.csv", stringsAsFactors = F)
data2<-read.csv("cascade_data.csv", stringsAsFactors = F)

countries<-c("Bangladesh", "Barbados", "Brazil", "Chile", "China", "Colombia", 
             "Cuba", "Ethiopia","India", "Mexico", "Mongolia", "Nepal", "Nigeria", "Peru", "Philippines",
             "Senegal", "Tajikistan", "Thailand", "Uganda")

#print<-read.csv("baseline_out_full.csv", stringsAsFactors = F)%>%filter(location %in% countries)
#write.csv(print, "baseline_out_app2.csv", row.names = F)
#p2<-read.csv("base_rates_full.csv", stringsAsFactors = F)%>%filter(location %in% countries)
#write.csv(p2, "base_rates_app2.csv", row.names = F)

drugoptions<-c("ACE-I: Lisinopril (20 mg)","ACE-I: Ramipril (5 mg)" ,                             
               "ACE-I: Perindopril (4 mg)", "ACE-I: Enalapril (20 mg)" ,                           
               "ARB: Losartan (50 mg)", "ARB: Telmisartan (40 mg)" ,                           
               "Calcium Channel Blockers: Amlodipine (5 mg)",
               "Calcium Channel Blockers: Amlodipine (10 mg)",       
               "Thiazide-like diuretics: Chlorthalidone (12.5 mg)",
               "Thiazide-like diuretics: Indapamide SR (1.5 mg)",     
               "Thiazide-like diuretics: Hydrochlorothiazide (25 mg)", 
               "Thiazide-like diuretics: Hydrochlorothiazide (50 mg)")

#loading spinners
options(spinner.color="#3c8dbc", spinner.color.background="#ffffff", spinner.size=1)

###Functions
#custom function to manipulate blood pressure distribution
get.bp.prob<-function(DT, salteff, saltyear1, saltyear2, rx, drugcov1, drugcov2, drugyear1, drugyear2){
    
    covgap<-(drugcov2-drugcov1)/100
    
    if(salteff!=0){
        DT[Year>=saltyear1 & Year<=saltyear2, Mean:=Mean-(((1.23*raisedBP)+((1-raisedBP)*0.55))*salteff*(Year-saltyear1+1)/(saltyear2-saltyear1+1))]
        DT[Year>saltyear2, Mean:=Mean-(((1.23*raisedBP)+((1-raisedBP)*0.55))*salteff)]
    }
    
    else{}
    
    DT[, Scale:=stdev^2/Mean]
    DT[, Shape:=(Mean/stdev)^2]
    
    DT[bp_cat=="<120", prob:=pgamma(q=120, shape=Shape, scale=Scale, lower.tail=TRUE)]
    DT[bp_cat=="120-129", prob:=pgamma(q=130, shape=Shape, scale=Scale, lower.tail=TRUE)-
           pgamma(q=120, shape=Shape, scale=Scale, lower.tail=TRUE)]
    DT[bp_cat=="130-139", prob:=pgamma(q=140, shape=Shape, scale=Scale, lower.tail=TRUE)-
           pgamma(q=130, shape=Shape, scale=Scale, lower.tail=TRUE)]
    DT[bp_cat=="140-149", prob:=pgamma(q=150, shape=Shape, scale=Scale, lower.tail=TRUE)-
           pgamma(q=140, shape=Shape, scale=Scale, lower.tail=TRUE)]
    DT[bp_cat=="150-159", prob:=pgamma(q=160, shape=Shape, scale=Scale, lower.tail=TRUE)-
           pgamma(q=150, shape=Shape, scale=Scale, lower.tail=TRUE)]
    DT[bp_cat=="160-169", prob:=pgamma(q=170, shape=Shape, scale=Scale, lower.tail=TRUE)-
           pgamma(q=160, shape=Shape, scale=Scale, lower.tail=TRUE)]
    DT[bp_cat=="170-179", prob:=pgamma(q=180, shape=Shape, scale=Scale, lower.tail=TRUE)-
           pgamma(q=170, shape=Shape, scale=Scale, lower.tail=TRUE)]
    DT[bp_cat=="180+", prob:=1-pgamma(q=180, shape=Shape, scale=Scale, lower.tail=TRUE)]
    
    if(rx==1 & covgap>0){
        DT[Year>=drugyear1 & Year<=drugyear2, shift:=prob*covgap*(Year-drugyear1+1)/(drugyear2-drugyear1+1)]
        DT[Year>drugyear2, shift:=prob*covgap]
        DT[bp_cat=="<120" | bp_cat=="120-129" | bp_cat=="130-139", shift:=0]
        DT[, add130:=sum(shift*diabetes), by=.(age, sex, Year)]
        DT[, add140:=sum(shift*(1-diabetes)), by=.(age, sex, Year)]
        DT[Year>=drugyear1, prob:=prob-shift]
        DT[Year>=drugyear1 & bp_cat=="120-129", prob:=prob+add130]
        DT[Year>=drugyear1 & bp_cat=="130-139", prob:=prob+add140]
    }
    
    else{}
    
    DT[,c("age", "sex", "Year", "bp_cat" ,"prob", "location")]
    
}#end of function

#significant digit formatter function
so_formatter <- function(x) {
    dplyr::case_when(
        x < 1e3 ~ as.character(x),
        x < 1e6 ~ paste0(as.character(x/1e3), "K"),
        x < 1e9 ~ paste0(as.character(x/1e6), "M"),
        x < 1e12 ~ paste0(as.character(x/1e9), "B"),
        x < 1e15 ~ paste0(as.character(x/1e12), "T")
    )
} #end of function

#custom function for applying RR
addRR<-function(RR, bp){
    if(bp=="<120"){1}
    else if (bp=="120-129"){1/RR}
    else if (bp=="130-139"){1/RR^2}
    else if (bp=="140-149"){1/RR^3}
    else if (bp=="150-159"){1/RR^4}
    else if (bp=="160-169"){1/RR^5}
    else if (bp=="170-179"){1/RR^6}
    else {1/RR^7}
} #end of function

#############################################
### UI ###
#############################################

ui <- dashboardPage(skin="purple",
                    title="Hypertension Investment Case Tool",
                    dashboardHeader(title = ""),
                    dashboardSidebar(
                            sidebarMenu(id="tabs",
                                menuItem("Baseline data", tabName="intro", icon=icon("star")),
                                menuItem("Sodium policies", tabName="salt", icon=icon("utensils")),
                                menuItem("Hypertension treatment", tabName="HTN", 
                                         icon=icon("prescription-bottle-alt")),
                                menuItem("Cost inputs", tabName="HTNcosts", icon=icon("dollar-sign")),
                                menuItem("Results", tabName="results", icon=icon("dashboard"))
                            )#end of sidebar menu
                    ), #end of dashboard sidebar
                    dashboardBody(
                    tabItems(

#############################################
### Introduction tab UI ####
#############################################

                    tabItem(tabName="intro",
                            fluidRow(
                                box(width=12,
                                    h3("Hypertension Investment Case Tool"),
                                    "In this tab, users will select some baseline settings for the app. 
                                    Start by choosing the country you would like to analyze.",
                                    br(),br(),
                                    selectInput("country", "Country:", countries, selected="Thailand"),
                                    #set warning text format
                                    tags$head(tags$style("#warningtext{color: red;
                                                             font-size: 14px;
                                                             font-style: italic;
                                                             }"
                                                )
                                            ),
                                    "Baseline assumptions are based on defaults from the HEARTS costing tool. 
                                    Here, you can download the country-specific default values to review. 
                                    You may also enter user-specific values in the downloaded spreadsheet
                                    and upload those inputs below. All values can also be edited in this app 
                                    as you move through each section.",
                                    br(),br(),
                                    downloadButton("inputDNLD", label="Costing template"),
                                    br(),br(),
                                    "If you would like to use your own custom inputs (and not the defaults) 
                                    from the spreadsheet above, upload your workbook below as a CSV file and 
                                    then select the 'User uploaded data' option. If you are choosing the 
                                    'HEARTS defaults' option, there is no need to upload a file.",
                                    br(),br(),
                                    fileInput("upload", "Upload a file", accept=".csv", multiple = T),
                                    radioButtons("hearts", "Inputs based on:", c("HEARTS defaults", "User uploaded data")),
                                    br(),
                                    textOutput("warningtext")#warn users if they don't have a file uploaded yet
                                ),
                                box(width=12,
                                    h4("Baseline info"),
                                    uiOutput("base_prev"),
                                    uiOutput("hcu"),
                                    uiOutput("lowcvdrisk"),
                                    uiOutput("medcvdrisk"),
                                    uiOutput("highcvdrisk")
                                )
                            )), #end of Intro tab UI
############################################# 
### Salt tab UI ###      
#############################################
                    tabItem(tabName="salt",
                            shinyjs::useShinyjs(),
                            fluidRow(
                                box(width=12,
                                    "To design your sodium policy package, select from the list of default 
                                    sodium policies below or select 'Other' and create your own policy.
                                    Details about the cost and effectiveness of each policy can be adjusted 
                                    in the 'Policy settings' box at the bottom of the page.", 
                                    br(),br(),
                                    checkboxGroupInput("NaPolicies",
                                                       "WHO 'Best Buys' to reduce unhealthy diet:", 
                                                       choices=c("Reformulation", 
                                                                 "Package labelling", 
                                                                 "Media campaigns", 
                                                                 "Supportive environment", 
                                                                 "Other"), 
                                                       inline=T),  
                                )
                            ),
                            fluidRow(
                                box(width=6,
                                    h5("Select the timeline for policy implementation. Impact will be 
                                       scaled up linearly during the selected timeframe."),
                                    sliderInput("saltyear", "Sodium policy scale-up timeline:",
                                                min=2020, max=2040, value=c(2021,2025), sep=""),
                                    br(),
                                    "Adjust baseline assumptions about population sodium intake below. 
                                    In accordance with", 
                                        a("research", 
                                        href="https://www.nejm.org/doi/full/10.1056/nejmoa1311889"), 
                                    "on ideal sodium intake, it is reccomended that the target sodium 
                                    consumption not fall below 3g/day.",
                                    br(),br(),
                                    rHandsontableOutput("salttable"),
                                    br(),
                                    uiOutput("packagedsalt"),
                                    uiOutput("outsidesalt")
                                ),
                                box(width=6,
                                    h5("Sodium policy impact on the cascade of care for hypertension"),
                                    #hide error messages
                                    #https://groups.google.com/g/shiny-discuss/c/FyMGa2R_Mgs?pli=1
                                    tags$style(type="text/css",
                                               ".shiny-output-error { visibility: hidden; }",
                                               ".shiny-output-error:before { visibility: hidden; }"
                                               
                                    ),
                                   plotlyOutput("plot2")
                                )
                            ),
                            fluidRow(
                                box(width=12,
                                title="Policy settings",
                                tabBox(width=NULL,
                                       id="policytabs",
                                       tabPanel("Reformulation",
                                                "Reduce sodium intake through the reformulation of food 
                                                products to contain less sodium and the setting of target 
                                                levels for the amount of sodium in foods and meals.",
                                                br(),br(),
                                                "Choose either mandatory or voluntary reformulation of 
                                                high sodium foods:",
                                                selectInput("reform", "", c("Mandatory", "Voluntary")),
                                                uiOutput("reformeff"),
                                                "Default efficacy estimates based on", 
                                                    a("An economic evaluation of salt reduction policies to reduce coronary heart disease in England: a policy modeling study.",
                                                    href="https://pubmed.ncbi.nlm.nih.gov/25128044/"), 
                                                br(), br(),
                                                "Average per capita cost of implementing this policy ($ USD):", 
                                                numericInputIcon("reform_cost", "", value=0.05, icon = list(icon("dollar")))
                                       ),
                                       tabPanel("Package labelling",
                                                "Reduce sodium intake through the implementation of 
                                                front-of-pack labeling.",
                                                br(),br(),
                                                uiOutput("labeleff"),
                                                "Default efficacy estimates based on", 
                                                    a("Policy options to reduce population salt intake", 
                                                    href="https://pubmed.ncbi.nlm.nih.gov/21835876/"), 
                                                br(), br(),
                                                "Average per capita cost of implementing this policy ($ USD):",
                                                numericInputIcon("label_cost", "", value=0.05, icon = list(icon("dollar")))
                                       ),
                                       tabPanel("Media campaigns",
                                                "Reduce sodium intake through a behavior change communication 
                                                and mass media campaign.",
                                                br(),br(),
                                                uiOutput("mediaeff"),
                                                "Default efficacy estimates based on", 
                                                    a("WHO Technical Briefing for Dietary Interventions", 
                                                    href="https://www.who.int/ncds/governance/unhealthy_diet.pdf"), 
                                                br(), br(),
                                                "Average per capita cost of implementing this policy ($ USD):",
                                                numericInputIcon("media_cost", "", value=0.05, icon = list(icon("dollar")))
                                       ),
                                       tabPanel("Supportive environment",
                                                "Reduce sodium intake through establishment of a supportive 
                                                environment in public institutions such as hospitals, schools 
                                                and nursing homes to enable low sodium meals to be provided.",
                                                br(),br(),
                                                uiOutput("supporteff"),
                                                "Default efficacy estimates based on", 
                                                    a("WHO Technical Briefing for Dietary Interventions", 
                                                    href="https://www.who.int/ncds/governance/unhealthy_diet.pdf"), 
                                                br(), br(),
                                                "Average per capita cost of implementing this policy ($ USD):",
                                                numericInputIcon("support_cost", "", value=0.05, icon = list(icon("dollar")))
                                       ),
                                       tabPanel("Other",
                                                "Users may design a custom sodium policy.",
                                                br(),br(),
                                                uiOutput("othereff"),
                                                "Average per capita cost of implementing this policy ($ USD):",
                                                numericInputIcon("other_cost", "", value=0.00, icon = list(icon("dollar")))
                                       )
                                       
                                )#end of tabBox
                                )#end of box
                            )#end of fluid row
                    ),#end of salt tab

#############################################
### HTN tab UI ###      
#############################################
                    tabItem(tabName="HTN",
                        fluidRow(
                            box(width=4,
                                h3("Baseline coverage:"),
                                uiOutput("aware1"),
                                uiOutput("treated1"),
                                uiOutput("controlled1"),
                                numericInputIcon("retained", "Among patients who have recieved treatment for
                                                 hypertension, but who have not achieved BP control <140 mmHg, 
                                                 what proportion are retained in care:", 
                                                 50,
                                                 icon = list(NULL, icon("percent"))
                                                 )
                            ),
                            box(width=8,
                                h4("Intervention impact on the cascade of care for hypertension"),
                                plotlyOutput("cascade"),
                                checkboxGroupInput("showsalt", "Sodium policy impacts:", 
                                                   choices = "Show impact", selected = "Show impact"
                                                   )
                            )
                        ),
                        fluidRow(
                            box(width=4,
                            h3("Set targets:"),
                            h5("In this section you can set targets for your hypertension program. 
                            Either set targets for the predicted awareness, treatment, and control rates 
                            below or select 'Alt targets' to proceed with programatic control knobs to 
                            estimate future impacts. *Note: the results shown will reflect which tab you 
                            select in this section as well as throughout the next stages of the app."),
                            h5("Select the start and end years for program scale-up. Coverage is assumed to 
                            increase linearly over the selected time period."),
                            sliderInput("drugcovyear", "Antihypertensive therapy scale-up timeline:",
                                        min=2020, max=2040, value=c(2021,2030), sep=""),
                            tabBox(width=NULL,
                                   id="targets",
                                   tabPanel("Program targets",
                                            uiOutput("aware_target"),
                                            uiOutput("treated_target"),
                                            uiOutput("controlled_target")
                                            ),
                                   tabPanel("Alt targets",
                                   "Increase screening coverage to widen the pool of patients available to 
                                   receive treatment. The diagram on the right shows how increasing screening 
                                   measures results in an increase in the number of patients
                                   with hypertension on treatment and/or with controlled blood pressure.",
                                   uiOutput("screen"),
                                   "Possible to add other control knobs here (e.g. improved linkage to care, 
                                   retention, etc)."
                                   )
                            )
                            ),
                            box(width=8,
                                h4("Patient pathways through the cascade of care"),
                                sankeyNetworkOutput("sankey")
                            )
                        )#end of fluidRow
                    ),#end of HTN tab

#############################################
### HTN cost tab UI ###
#############################################
                tabItem(tabName="HTNcosts",
                    fluidRow(
                      column(width=1), #buffer
                      column(width=10,
                        fluidRow(
                            box(width=NULL,
                                h5("In this tab you can adjust cost assumptions for the hypertension treatment 
                                package. Global settings are presented at the top and costs by service type 
                                are displayed in sections below. A summary of the cost per beneficiary is 
                                reported here and will update to reflect your changes to inputs below."),
                                rHandsontableOutput("unitcosts"),
                                br(),
                                useShinyjs(),
                                tags$div(selectInput("currency", "Prices show in:", c("Local currency", "USD"), selected = "USD"),
                                         style="display:inline-block; vertical-align:top; width:30%"),
                                tags$div(style="display: inline-block; vertical-align:top; width: 3%;",HTML("<br>")),
                                tags$div(uiOutput("exchange"), style="display:inline-block; vertical-align:top; width:30%"),
                                tags$div(style="display: inline-block; vertical-align:top; width: 3%;",HTML("<br>")),
                                tags$div(uiOutput("safetystock"),style="display:inline-block; vertical-align:top; width:30%"),
                                br(),
                                rHandsontableOutput("costopts")
                            ),
                            box(width=NULL,
                                collapsible = T,
                                collapsed = F,
                                title = "Screening",
                                "Annual cost of population blood pressure screening for adults age 18+ who 
                                attend a primary care facility. Values are imported from the HEARTS costing 
                                tool unless otherwise updated below.",
                                br(),br(),
                                rHandsontableOutput("screencosts")
                            ),
                            box(width=NULL,
                                collapsible = T,
                                collapsed = F,
                                title="Diagnosis confirmation",
                                "One time cost for newly diagnosed hypertension patients. The population in 
                                need consists of all adults (18+) presenting with high blood pressure at a 
                                primary care facility for which a health provider trained to conduct a full 
                                CVD risk assessment is available.", 
                                br(),br(),
                                rHandsontableOutput("ddxcosts"),
                                br(),br(),
                                rHandsontableOutput("ddxitems")
                            ),
                            box(width=NULL,
                                collapsible=T,
                                collapsed = F,
                                title="Pharmacological treatment from hypertension",
                                "Select a HEARTS standard treatment protocol and update inputs in the table below.",
                                br(),
                                selectInput("protocol", "Select treatment protocol:", 
                                            c("CCB + diuretic", "ACE-I or ARB + CCB", "ACE-I or ARB + diuretic",
                                              "ACE-I or ARB", "CCB", "Diuretic")),
                                div(style="display: inline-block;vertical-align:top;", 
                                    selectInput("med1", "Select CCB:","Amlodipine") 
                                    ),
                                div(style="display: inline-block;vertical-align:top;", 
                                    selectInput("med2", "Select thiazide-like diuretic:", 
                                                c("Chlorthalidone","Indapamide","Hydrochlorothiazide"))   
                                    ),
                                div(style="display: inline-block;vertical-align:top;", 
                                    selectInput("med3", "Select ACE-I:", 
                                                c("Lisinopril","Ramipril","Perindopril","Enalapril"))
                                    ),
                                div(style="display: inline-block;vertical-align:top;", 
                                    selectInput("med4", "Select ARB:", c("Losartan","Telmisartan"))
                                    ),
                                br(),
                                numericInput("acei", "What percent of patients on ACE-I (vs ARB):", 
                                             min=0, max=100, value=75),
                                br(),
                                "Make adjustment to protocol assumptions here:",
                                br(),
                                rHandsontableOutput("rxcosts")
                                ),
                            box(width=NULL,
                                collapsible=T,
                                collapsed = F,
                                title="BP monitoring and clinical review",
                                "Return visit frequency and delivery of care by patient CVD risk category.",
                                br(),br(),
                                rHandsontableOutput("reviewcosts")
                                ),
                            box(width=NULL,
                                collapsible=T,
                                collapsed = F,
                                title="Training and health system costs",
                                "Annual costs for above health facility and health system strengthening costs. 
                                Markups based on USAID Global Health Supply Chain and 2009 WHO High Level Taskforce 
                                estimates.",
                                br(),br(),
                                rHandsontableOutput("markups")
                            )
                        )#end of fluidRow
                      ),#end of column
                    column(width=1) #buffer
                    )#end of fluiRdow
                ),#end of HTNcosts tab

#############################################
### RESULTS UI ###
#############################################
                tabItem(tabName="results", 
                    fluidRow(
                    column(width=6,
                        box(width=NULL,
                            textOutput("country_results"), br(),
                            "If you go back and make edits in previous tabs, click the 'Recalculate results' button 
                            to rerun the analysis with your updated inputs.",
                            br(),br(),
                            actionButton("plot", "Recalculate results"),
                        ),
                        box(width=NULL,
                            withSpinner(rHandsontableOutput("impacts"), type=6)
                        ),
                        box(width=NULL,
                            withSpinner(plotlyOutput("ceaplot"), type=6)
                        )
                    ),
                    column(width=6,
                       box(width=NULL,
                           withSpinner(plotlyOutput("acosts"), type=6),
                           br(),
                           withSpinner(plotlyOutput("stackedplot"), type=6)
                       ),
                       box(width=NULL,
                           "Total annual medication requirements and costs.",
                           sliderInput("year", "Select costing year:", 2020, 2040, 2030, sep=""),
                           withSpinner(rHandsontableOutput("resultsmed"), type=6)
                       )
                    )
                    )#end of fluidRow
                )#end of results tab
            )#end of tab items
        )#end of dashboard body
    ) #End of dashboard page

#############################################
### SERVER ###
#############################################
server <- function(input, output){
    
    ## Reactive Values ##
    rv<-reactiveValues(data=NA)
    newsalt<-reactiveVal(0)
    saltcost<-reactiveVal(0)
    controlincrease<-reactiveVal(1)
    prevdecrease<-reactiveVal(1)
    
    hearts<-reactive({
        prev.hearts<-read.csv(paste0("inputs_",input$country,".csv"), stringsAsFactors = F)
        prev.hearts$Default<-as.numeric(prev.hearts$Default)
        
        if(!is.null(input$upload)){
            userdf<-read.csv(input$upload$datapath, header=T, stringsAsFactors = F)
            df<-data.frame(Measure = prev.hearts$Measure,
                           Default = prev.hearts$Default,
                           User = as.numeric(userdf$User))
            
            prev.hearts<-df
            return(prev.hearts)
        }
        
        else{return(prev.hearts)}
        
    })

#############################################
### Intro tab server ###
#############################################
    output$warningtext<-renderText({
        if(is.null(input$upload) & input$hearts!="HEARTS defaults"){
            "No file or incorrect file type has been uploaded. Upload file above."
        }
        else{}
    })
    
    #https://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
    
    options(shiny.maxRequestSize=10*1024^2)
    
    output$inputDNLD <- downloadHandler(
        filename = function() {
            paste0("inputs_",input$country,".csv")
        },
        content = function(file) {
            write.csv(read.csv(paste0("inputs_",input$country,".csv"), stringsAsFactors = F), file)
        }
    )
    
    output$base_prev<-renderUI({
        
        df<-hearts()
        
        if(input$hearts=="HEARTS defaults"){
            numericInputIcon("prev1", "Prevalence of hypertension (among adults 20+):", 
                             round(df$Default[df$Measure=="Hypertension (>=140mmHg)"]*100),icon = list(NULL, icon("percent")))
        }
        else if(input$hearts!="HEARTS defaults" & is.null(input$upload)){
            numericInputIcon("prev1", "Prevalence of hypertension (among adults 20+):", 
                             NA, icon = list(NULL, icon("percent")))
        }
        
        else{
            numericInputIcon("prev1", "Prevalence of hypertension (among adults 20+):", 
                             round(df$User[df$Measure=="Hypertension (>=140mmHg)"]*100),icon = list(NULL, icon("percent")))
        }
    })
    
    output$hcu<-renderUI({
        
        df<-hearts()
        
        if(input$hearts=="HEARTS defaults"){
            numericInputIcon("utilization", "Primary healthcare attendance rate (annual):", 
                             round(df$Default[df$Measure=="Primary healthcare attendance rate (annual)"]*100),icon = list(NULL, icon("percent")))
        }
        else if(input$hearts!="HEARTS defaults" & is.null(input$upload)){
            numericInputIcon("utilization", "Primary healthcare attendance rate (annual):", 
                             NA,icon = list(NULL, icon("percent")))
        }
        else{
            numericInputIcon("utilization", "Primary healthcare attendance rate (annual):", 
                             round(df$User[df$Measure=="Primary healthcare attendance rate (annual)"]*100),icon = list(NULL, icon("percent")))
        }
    })
    
    output$lowcvdrisk<-renderUI({
        
        df<-hearts()
        
        if(input$hearts=="HEARTS defaults"){
            numericInputIcon("lowrisk", "Proportion with low CVD risk (0 to <10%):", 
                             round(df$Default[df$Measure=="Low CVD risk (0 to <10%)"]*100,1),icon = list(NULL, icon("percent")))
        }
        else if(input$hearts!="HEARTS defaults" & is.null(input$upload)){
            numericInputIcon("lowrisk", "Proportion with low CVD risk (0 to <10%):", 
                             NA,icon = list(NULL, icon("percent")))
        }
        
        else{
            numericInputIcon("lowrisk", "Proportion with low CVD risk (0 to <10%):", 
                             round(df$User[df$Measure=="Low CVD risk (0 to <10%)"]*100,1),icon = list(NULL, icon("percent")))
        }
    })
    
    output$medcvdrisk<-renderUI({
        
        df<-hearts()
        
        if(input$hearts=="HEARTS defaults"){
            numericInputIcon("medrisk", "Proportion with medium CVD risk (10 to <20%):", 
                             round(df$Default[df$Measure=="Medium CVD risk (10 to <20%)"]*100,1),icon = list(NULL, icon("percent")))
        }
        else if(input$hearts!="HEARTS defaults" & is.null(input$upload)){
            numericInputIcon("medrisk", "Proportion with medium CVD risk (10 to <20%):", 
                             NA,icon = list(NULL, icon("percent")))
        }
        else{
            numericInputIcon("medrisk", "Proportion with medium CVD risk (10 to <20%):", 
                             round(df$User[df$Measure=="Medium CVD risk (10 to <20%)"]*100,1),icon = list(NULL, icon("percent")))
        }
    })
    
    output$highcvdrisk<-renderUI({
        df<-hearts()
        
        if(input$hearts=="HEARTS defaults"){
            numericInputIcon("highrisk", "Proportion with high CVD risk (>=20%):", 
                             round(df$Default[df$Measure=="High CVD risk (>=20%)"]*100, 1),icon = list(NULL, icon("percent")))
        }
        else if(input$hearts!="HEARTS defaults" & is.null(input$upload)){
            numericInputIcon("highrisk", "Proportion with high CVD risk (>=20%):", 
                             NA,icon = list(NULL, icon("percent")))
        }
        
        else{
            numericInputIcon("highrisk", "Proportion with high CVD risk (>=20%):", 
                             round(df$User[df$Measure=="High CVD risk (>=20%)"]*100, 1),icon = list(NULL, icon("percent")))
            
        }
    })

#############################################
### Salt tab server ###
#############################################
    
    output$salttable<-renderRHandsontable({
        
        df<-hearts()
        if(input$hearts=="HEARTS defaults"){
            salt<-df$Default[df$Measure=="Average daily sodium intake (g)"]
        }
        else{
            salt<-df$User[df$Measure=="Average daily sodium intake (g)"]
        }
        
        DF<-data.frame(
            Baseline=c(signif(salt,digits=2)),
            Reduced=c(0),
            Target=c(signif(salt,digits=2))
        )
        
        rhandsontable(DF, colHeaders = c("Baseline sodium intake (g)",
                                         "Reduction (g)",
                                         "Target sodium intake (g)"),
                      digits=2)%>%
            hot_cols(colWidths = c(4,4,4))%>%
            hot_col("Reduction (g)", readOnly = T)%>%
            hot_col("Target sodium intake (g)", readOnly = T,
                    renderer = "function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value < 3) {
              td.style.background = 'red';
             } 
           }")%>% hot_table(stretchH = "all")
    })
    
    observe({
        #req(input$reform_cost)
        #req(input$reformeff)
        
        if(is.null(input$packagedsalt)){
            effects<-data.frame(policy=c("Reformulation", 
                                         "Package labelling",
                                         "Media campaigns",
                                         "Supportive environment", 
                                         "Other"),
                                efficacy=c(0.06,0.03,0.05,0.05,0.05),
                                cost=c(input$reform_cost,input$label_cost, input$media_cost, input$support_cost,
                                       input$other_cost)
            )
        }
        
        else{
            effects<-data.frame(policy=c("Reformulation", 
                                         "Package labelling",
                                         "Media campaigns",
                                         "Supportive environment", 
                                         "Other"),
                                efficacy=c((input$reformeff[1]/100)*(input$packagedsalt[1]/100),
                                           (input$labeleff[1]/100)*(input$packagedsalt[1]/100),
                                           input$mediaeff[1]/100,
                                           (input$supporteff[1]/100)*(input$outsidesalt[1]/100),
                                           input$othereff[1]/100),
                                cost=c(input$reform_cost,input$label_cost, input$media_cost, input$support_cost,
                                       input$other_cost)
            )
        }
        
        if(is.null(input$NaPolicies)){
            newsalt(0)
            saltcost(0)
        }
        
        else{
            effects<-effects%>%filter(policy%in%input$NaPolicies)
            newsalt(sum(effects$efficacy))
            saltcost(sum(effects$cost))
        }
        
    })
    
    observe({
        
        if(!is.null(input$salttable)){
            df<-as.data.frame(hot_to_r(input$salttable))
            b<-df$Baseline
            r<-df$Baseline*newsalt()
        }
        else{
            df<-hearts()
            if(input$hearts=="HEARTS defaults"){
                salt<-df$Default[df$Measure=="Average daily sodium intake (g)"]
            }
            else{
                salt<-df$User[df$Measure=="Average daily sodium intake (g)"]
            }
            
            b<-signif(salt,digits=2)
            r<-0
        }
        
        DF<-data.frame(
            Baseline=b,
            Reduced=r,
            Target=b-r
        )
        
        output$salttable<-renderRHandsontable({
            rhandsontable(DF, language = "en-EN", rowHeaders = NULL, height = '100%', overflow='visible',
                          colHeaders = c("Baseline sodium intake (g)","Reduction (g)","Target sodium intake (g)"))%>%
                hot_cols(colWidths = c(4,4,4))%>%
                hot_col("Reduction (g)", readOnly = T)%>%
                hot_col("Target sodium intake (g)", readOnly = T,
                        renderer = "function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value < 3) {
              td.style.background = '#FF6262';
             } 
           }")%>% hot_table(stretchH = "all")
        })
    })
    
    ### render ui
    
    output$packagedsalt<-renderUI({
        numericInputIcon("packagedsalt", "Proportion of sodium intake from packaged foods", 30, 
                         icon = list(NULL, icon("percent")))
    })
    
    output$outsidesalt<-renderUI({
        numericInputIcon("outsidesalt", "Proportion of sodium intake from meals outside the home", 30, 
                         icon = list(NULL, icon("percent")))
    })
    
    output$reformeff<-renderUI({
        if(input$reform=="Mandatory"){
            numericInputIcon("reformeff", "Average reduction in sodium intake per day from packaged foods:", 
                             20, icon = list(NULL, icon("percent")))
        }
        else{
            numericInputIcon("reformeff", "Average reduction in sodium intake per day from packaged foods:", 
                             15, icon = list(NULL, icon("percent")))
        }
    })
    outputOptions(output, "reformeff", suspendWhenHidden = FALSE)
    
    output$labeleff<-renderUI({
        numericInputIcon("labeleff", "Average reduction in sodium intake per day from packaged foods:", 
                         10, icon = list(NULL, icon("percent")))
    })
    
    outputOptions(output, "labeleff", suspendWhenHidden = FALSE)
    
    output$mediaeff<-renderUI({
        numericInputIcon("mediaeff", "Average reduction in total sodium intake per day:", 10, 
                         icon = list(NULL, icon("percent")))
    })
    
    outputOptions(output, "mediaeff", suspendWhenHidden = FALSE)
    
    output$supporteff<-renderUI({
        numericInputIcon("supporteff", "Average reduction in sodium intake per day from foods consumed outside the home:", 20,
                         icon = list(NULL, icon("percent")))
    })
    
    outputOptions(output, "supporteff", suspendWhenHidden = FALSE)
    
    output$othereff<-renderUI({
        numericInputIcon("othereff", "Average reduction in total sodium intake:", 0, 
                         icon = list(NULL, icon("percent")))
    })
    
    outputOptions(output, "othereff", suspendWhenHidden = FALSE)
    
    output$percapitacost<-renderText({
        
        costs<-data.frame( policy=c("Reformulation", "Package labelling",
                                    "Media campaigns", "Supportive environment", "Other"),
                           unitcost=c(input$reform_cost, input$label_cost,
                                      input$media_cost,input$support_cost, input$other_cost)
        )
        costs<-costs%>%filter(policy%in%input$NaPolicies)
        sum(costs$unitcost)
    })
    
    
    ##care cascade
    output$plot2<-renderPlotly({
        
        df<-hearts()
        if(input$hearts=="HEARTS defaults"){
            aware<-df$Default[df$Measure=="Proportion of hypertensive patients aware of their diagnosis"]
            treated<-df$Default[df$Measure=="Proportion of hypertensive patients on treatment for high blood pressure"]
            controlled<-df$Default[df$Measure=="Proportion of hypertensive patients with controlled systolic blood pressure"]
        }
        else{
            aware<-df$User[df$Measure=="Proportion of hypertensive patients aware of their diagnosis"]
            treated<-df$User[df$Measure=="Proportion of hypertensive patients on treatment for high blood pressure"]
            controlled<-df$User[df$Measure=="Proportion of hypertensive patients with controlled systolic blood pressure"]
        }
        
        data<-as.data.table(data2%>%filter(gbd2019==input$country))
        
        deltasalt<-as.data.frame(hot_to_r(input$salttable))
        
        data[Year==2016, Mean:=Mean-(((1.23*raisedBP)+((1-raisedBP)*0.55))*deltasalt$Reduced[1]*2.54)]
        
        data[, Scale:=stdev^2/Mean]
        data[, Shape:=(Mean/stdev)^2]
        
        data[, noHTN:=pgamma(q=140, shape=Shape, scale=Scale, lower.tail=TRUE)]
        data[, HTN:=1-pgamma(q=140, shape=Shape, scale=Scale, lower.tail=TRUE)]
        
        
        #Need to know something about cascade
        data<-data%>%select(gbd2019, ISO, sex, raisedBP, HTN, Year, World_bank_2015)%>%spread(Year, HTN)
        data$controldiff<-(data$`2015`-data$`2016`)*100
        
        data$Prevalence<-input$prev1
        data$Awareness<-data$Prevalence*aware
        data$Treatment<-data$Prevalence*treated
        data$Control<-data$Prevalence*controlled
        
        plot2<-gather(data[,c("gbd2019", "sex", "controldiff", "Prevalence", "Awareness", "Treatment", "Control")], cascade, prop, -gbd2019, -sex, -controldiff)
        
        names(plot2)[5]<-"Baseline"
        plot2$Target<-plot2$Baseline
        
        plot2$Target[plot2$cascade=="Control"]<-plot2$Target[plot2$cascade=="Control"]+plot2$controldiff[plot2$cascade=="Control"]
        plot2$Target[plot2$cascade=="Prevalence"]<-plot2$Target[plot2$cascade=="Prevalence"]-plot2$controldiff[plot2$cascade=="Prevalence"]
        
        plot2$cascade<-factor(plot2$cascade, levels=c("Prevalence", "Awareness", "Treatment", "Control"))
        plot2<-plot2%>%group_by(cascade)%>%summarise(Baseline=mean(Baseline), Target=mean(Target))
        
        controlincrease(plot2$Target[plot2$cascade=="Control"]/plot2$Baseline[plot2$cascade=="Control"])
        prevdecrease(plot2$Target[plot2$cascade=="Prevalence"]/plot2$Baseline[plot2$cascade=="Prevalence"])
        
        plot_ly(plot2, x=~cascade, y=~Baseline, type='bar', name='Baseline', marker=list(color="#605CA8"))%>%
            add_trace(y=~Target, name="Target",  marker=list(color="#AAA8D0"))%>%
            layout(yaxis=list(title="Proportion of all adults (%)"),
                   xaxis=list(title=""))%>%
            config(modeBarButtonsToRemove = list("resetScale2d", "zoom2d", "autoScale2d", "toggleSpikelines","pan2d","zoomIn2d", "zoomOut2d",
                                                 "lasso2d", "selectbox", "newplotlylogo", "plotlylogo"))
        
    })

#############################################
### HTN tab server###
#############################################

    output$aware1<-renderUI({
        df<-hearts()
        
        if(input$hearts=="HEARTS defaults"){
            aware<-round(df$Default[df$Measure=="Proportion of hypertensive patients aware of their diagnosis"]*100, 1)
        }
        else if(input$hearts!="HEARTS defaults" & is.null(input$upload)){
            aware<-NA
        }
        
        else{            
            aware<-round(df$User[df$Measure=="Proportion of hypertensive patients aware of their diagnosis"]*100, 1)
        }
        
        numericInputIcon("aware1", "Baseline proportion of hypertensive patients aware of their diagnosis:", 
                        aware,icon = list(NULL, icon("percent")))
    })
    
    output$treated1<-renderUI({
        df<-hearts()
        
        if(input$hearts=="HEARTS defaults"){
            treated<-round(df$Default[df$Measure=="Proportion of hypertensive patients on treatment for high blood pressure"]*100, 1)
        }
        else if(input$hearts!="HEARTS defaults" & is.null(input$upload)){
            treated<-NA
        }
        
        else{            
            treated<-round(df$User[df$Measure=="Proportion of hypertensive patients on treatment for high blood pressure"]*100, 1)
        }
        
        numericInputIcon("treated1", "Baseline proportion of hypertensive patients on treatment:", 
                         treated,icon = list(NULL, icon("percent")),max = input$aware1)
    })
    
    output$controlled1<-renderUI({
        df<-hearts()
        
        if(input$hearts=="HEARTS defaults"){
            controlled<-round(df$Default[df$Measure=="Proportion of hypertensive patients with controlled systolic blood pressure"]*100, 1)
        }
        else if(input$hearts!="HEARTS defaults" & is.null(input$upload)){
            controlled<-NA
        }
        
        else{            
            controlled<-round(df$User[df$Measure=="Proportion of hypertensive patients with controlled systolic blood pressure"]*100, 1)
        }
        
        numericInputIcon("controlled1", "Baseline proportion of hypertensive patients with controlled BP (<140 mmHg):", 
                         controlled,icon = list(NULL, icon("percent")),max = input$treated1)
    })
    
    output$screen<-renderUI({
        tagList(
            numericInputIcon("aware2", "Target screening coverage:", round(input$aware1, 0), max=100, icon = list(NULL, icon("percent"))),
            tags$style("#aware2","{background-color:#70a1e6;}")
        )
    })
    
    output$aware_target<-renderUI({
        sliderInput("aware_target", "Target awareness rate (% of those with HTN)", min=input$aware1 , max=100, value=input$aware1, step=1)
            })
    
    output$treated_target<-renderUI({
        sliderInput("treated_target", "Target treatment rate (% of those with HTN)", min=input$treated1, max=input$aware_target, value=input$treated1, step=1) 
    })
    
    output$controlled_target<-renderUI({
        sliderInput("controlled_target", "Target control rate (% of those with HTN)", min=input$controlled1, max=input$treated_target, value=input$controlled1, step=1)
        
    })
    
    ### PLOTS (HTN tab) ###
    output$sankey<-renderSankeyNetwork({
        
        if(input$targets=="Alt targets"){
        treated<-input$aware2*(input$treated1/input$aware1)
        controlled<-input$aware2*(input$controlled1/input$aware1)

        links <- data.frame(
            source=c("Hypertensive","Hypertensive", "Aware", "Treated", "Treated", "Treated", "Aware"), 
            target=c("Aware","Unaware", "Treated", "Controlled", "Uncontrolled", "Lost to follow-up", "Lost to follow-up"), 
            value=c(input$aware2, 100-input$aware2, treated , controlled, 
                    input$retained/100*(treated-controlled), (1-input$retained/100)*(treated-controlled), 
                    input$aware2-treated)
        )
        
        }
        
        else{
            treated<-input$treated_target
            controlled<-input$controlled_target
            
            links <- data.frame(
                source=c("Hypertensive","Hypertensive", "Aware", "Treated", "Treated", "Treated", "Aware"), 
                target=c("Aware","Unaware", "Treated", "Controlled", "Uncontrolled", "Lost to follow-up", "Lost to follow-up"), 
                value=c(input$aware_target, 100-input$aware_target, treated , controlled, 
                        input$retained/100*(treated-controlled), (1-input$retained/100)*(treated-controlled), 
                        input$aware_target-treated)
            )
            
        }
        
        # From these flows we need to create a node data frame: it lists every entities involved in the flow
        nodes <- data.frame(
            name=c(as.character(links$source), 
                   as.character(links$target)) %>% unique()
        )
        
        # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
        links$IDsource <- match(links$source, nodes$name)-1 
        links$IDtarget <- match(links$target, nodes$name)-1
        
        
        
        # prepare color scale: I give one specific color for each node.
        my_color <- 'd3.scaleOrdinal() 
              .range(["#0f35bf", "#70a1e6" , "#ff9021",  "#747e8c", "#05854b", "#6aba92",   "#a60c2a"])'
        
        # Make the Network
        sankeyNetwork(Links = links, Nodes = nodes,
                           Source = "IDsource", Target = "IDtarget",
                           Value = "value", NodeID = "name", 
                           sinksRight=FALSE, iterations = 0,
                           colourScale=my_color)
       
        
    })
    
    
    output$cascade<-renderPlotly({

        if(!is.null(input$showsalt)){
            prev<-input$prev1*prevdecrease()
            pluscontrol<-controlincrease()
        }
        
        else{
            prev<-input$prev1
            pluscontrol<-1
        }
        
        if(input$targets=="Alt targets"){
            plot<-data.frame(
                Scenario=c("Baseline", "Baseline", 
                           "Baseline", "Baseline",
                           "Target", "Target", 
                           "Target", "Target"),
                Measure=c("Hypertensive", "Aware", 
                          "Treated", "Controlled",
                          "Hypertensive", "Aware", 
                          "Treated", "Controlled"),
                Number=c(input$prev1, input$aware1*(input$prev1/100),
                         input$treated1*(input$prev1/100), input$controlled1*(input$prev1/100),
                         prev,input$aware2*(prev/100),
                         input$aware2*(input$treated1/input$aware1)*(prev/100),
                         input$aware2*(input$controlled1/input$aware1)*pluscontrol*(prev/100)
                         #input$adsupport/100*(input$visitnum+input$qoc)/200*(4.5/6)*pluscontrol
                )
            )
        }
        
        else{
            c<-input$controlled_target*pluscontrol
            
            plot<-data.frame(
                Scenario=c("Baseline", "Baseline", 
                           "Baseline", "Baseline",
                           "Target", "Target", 
                           "Target", "Target"),
                Measure=c("Hypertensive", "Aware", 
                          "Treated", "Controlled",
                          "Hypertensive", "Aware", 
                          "Treated", "Controlled"),
                Number=c(input$prev1, input$aware1*(input$prev1/100),
                         input$treated1*(input$prev1/100), input$controlled1*(input$prev1/100),
                         prev,input$aware_target*(prev/100),
                         input$treated_target*(prev/100),
                         ifelse(c>100, prev, c*(prev/100))
                         #input$adsupport/100*(input$visitnum+input$qoc)/200*(4.5/6)*pluscontrol
                )
            )
            
        }
        
        plot$Measure<-factor(plot$Measure, levels = c("Hypertensive", "Aware", "Treated", "Controlled"))
        plot<-plot%>%spread(Scenario, Number)
        
        plot_ly(plot, x=~Measure, y=~Baseline, type='bar', name='Baseline', marker=list(color="#605CA8"))%>%
            add_trace(y=~Target, name="Target",  marker=list(color="#AAA8D0"))%>%
            layout(yaxis=list(title="Proportion of all adults (%)"),
                   xaxis=list(title=""))%>%
            config(modeBarButtonsToRemove = list("resetScale2d", "zoom2d", "autoScale2d", "toggleSpikelines","pan2d","zoomIn2d", "zoomOut2d",
                                                 "lasso2d", "selectbox", "newplotlylogo", "plotlylogo"))
        
    })
    
#############################################
### HTN costs ###
#############################################
    
    #ui elements
    
    output$exchange<-renderUI({
        df<-hearts()
        if(input$hearts=="HEARTS defaults"){
            erate<-df$Default[df$Measure=="What is the LCU to USD exchange rate?"]
        }
        else{
            erate<-df$User[df$Measure=="What is the LCU to USD exchange rate?"]
            
        }
        numericInput("exchange", "LCU to USD exchange rate:", erate)
    })
    
    output$safetystock<-renderUI({
        df<-hearts()
        if(input$hearts=="HEARTS defaults"){
            stock<-df$Default[df$Measure=='How much "safety stock" is required to be on hand for medicines?']
        }
        else{
            stock<-df$User[df$Measure=='How much "safety stock" is required to be on hand for medicines?']
        }
        numericInputIcon("safetystock", "Safety stock", stock*100,icon = list(NULL, icon("percent")))
    })
    
    ##put this inside and observe({})
    observe({
        
        df<-hearts()
        if(input$hearts=="HEARTS defaults"){
            pop<-df$Default[df$Measure=='Adult population']
        }
        else{pop<-df$User[df$Measure=='Adult population']}
        
        if(!is.null(input$screencosts) & !is.null(input$costopts)){
            sal<-as.data.frame(hot_to_r(input$costopts))
            screen<-as.data.frame(hot_to_r(input$screencosts))
            screencost<-(screen$Minutes*screen$Doctor*0.000008*sal$Wage[sal$Provider=="Doctor"])+
                (screen$Minutes*screen$Nurse*0.000008*sal$Wage[sal$Provider=="Nurse"])+
                (screen$Minutes*screen$CHW*0.000008*sal$Wage[sal$Provider=="CHW"])
        }
        else{
            screencost<-NA
        }
        
        if(!is.null(input$costopts) & !is.null(input$ddxcosts) & !is.null(input$ddxitems)){
            ddx<-as.data.frame(hot_to_r(input$ddxcosts))
            items<-as.data.frame(hot_to_r(input$ddxitems))
            sal<-as.data.frame(hot_to_r(input$costopts))
            
            ddxcost<- (sum(ddx$Minutes*ddx$Doctor)*0.000008*sal$Wage[sal$Provider=="Doctor"])+
                (sum(ddx$Minutes*ddx$Nurse)*0.000008*sal$Wage[sal$Provider=="Nurse"])+
                (sum(ddx$Minutes*ddx$CHW)*0.000008*sal$Wage[sal$Provider=="CHW"])+
                (sum(ddx$Minutes*ddx$Labtech)*0.000008*sal$Wage[sal$Provider=="Lab Technician"])+
                sum(items$Cost)
        }
        
        else{
            ddxcost<-NA
        }
        
        if(!is.null(input$rxcosts)){
            rx<-as.data.frame(hot_to_r(input$rxcosts))
            rxcost<-sum(rx$Tabs*rx$Days*rx$Proportion*rx$UC)
        }
        
        else{
            rxcost<-NA
        }
        
        if(!is.null(input$reviewcosts) & !is.null(input$costopts)){
            review<-as.data.frame(hot_to_r(input$reviewcosts))
            sal<-as.data.frame(hot_to_r(input$costopts))
            
            reviewcost<-(sum(review$Proportion/100*review$Visits*review$Time*review$Doctor)*0.000008*sal$Wage[sal$Provider=="Doctor"])+
                (sum(review$Proportion/100*review$Visits*review$Time*review$Nurse)*0.000008*sal$Wage[sal$Provider=="Nurse"])+
                (sum(review$Proportion/100*review$Visits*review$Time*review$CHW)*0.000008*sal$Wage[sal$Provider=="CHW"])
        }
        
        else{
            reviewcost<-NA
        }
        
        DF<-data.frame(
            Measure = "Unit cost per beneficiary",
            Screening = screencost,
            Diagnosis = ddxcost,
            Treatment = rxcost,
            Clinical_review = reviewcost
        )
        
        #language codes @ http://numbrojs.com/languages.html
        if(input$currency=="USD"){
            lang<-"en-EN"
        }
        else{
            lang<-currency$currency[currency$Country==input$country]
        }
        output$unitcosts<-renderRHandsontable({
            rhandsontable(DF, language = "en-EN", rowHeaders = NULL, height = '100%', overflow='visible',
                          colHeaders = c("Measure", "Screening", "Diagnosis", "Treatment", "Clinical review"))%>%
                hot_cols("Screening", format = "$0,000.00", language = lang, readOnly = T)%>%
                hot_cols("Diagnosis", format = "$0,000.00", language = lang, readOnly = T)%>%
                hot_cols("Treatment", format = "$0,000.00", language = lang, readOnly = T)%>%
                hot_cols("Clinical review", format = "$0,000.00", language = lang, readOnly = T)%>%
                hot_table(stretchH = "all")
        })
        
    })
    
    output$costopts<-renderRHandsontable({
        
        df<-hearts()

        if(input$hearts=="HEARTS defaults"){
            DF<-data.frame(
                Provider = c("Doctor", "Nurse", "CHW", "Lab Technician"),
                #Number = c(df$default[16], df$default[17], df$default[18], NA),
                Wage = c(df$Default[df$Measure=="Doctor wage"], df$Default[df$Measure=="Nurse wage"], 
                         df$Default[df$Measure=="CHW wage"], df$Default[df$Measure=="Lab technician wage"])
                #Trained = c(train$default[6],train$default[6],train$default[6], NA)
            )
        }
        else{
            DF<-data.frame(
                Provider = c("Doctor", "Nurse", "CHW", "Lab Technician"),
                #Number = c(df$user[16], df$user[17], df$user[18], NA),
                Wage = c(df$User[df$Measure=="Doctor wage"], df$User[df$Measure=="Nurse wage"], 
                         df$User[df$Measure=="CHW wage"], df$User[df$Measure=="Lab technician wage"])
                #Trained = c(train$user[6],train$user[6],train$user[6], NA)
            )
        }
        
        if(input$currency=="USD"){
            lang<-"en-EN"
            DF$Wage<-DF$Wage/input$exchange
        }
        else{
            lang<-currency$currency[currency$Country==input$country]
        }
        
        rhandsontable(DF, language = "en-EN", rowHeaders = NULL, height = '100%', overflow='visible',
                      colHeaders = c("Provider type",  "Annual wage"))%>%
            hot_col("Annual wage", format = "$0,000.00", language = lang)%>%
            hot_table(stretchH = "all")
    })
    
    
    output$screencosts<-renderRHandsontable({
        df<-hearts()
        
        if(input$hearts=="HEARTS defaults"){
            
            DF<-data.frame(
                Component=c("Ask about patients' health history and take preliminary blood pressure reading"),
                Minutes = c(5),
                Doctor = c(0.9),
                Nurse = c(0.1),
                CHW = c(0)
            )
        }
        
        else{
            DF<-data.frame(
                Component=c("Ask about patients' health history and take preliminary blood pressure reading"),
                Minutes = c(df$User[df$Measure=="Screen patients for total CVD risk (ask about patients' health history)"]),
                Doctor = c(df$User[df$Measure=="Doctor proportion of care"]),
                Nurse = c(df$User[df$Measure=="Nurse proportion of care"]),
                CHW = c(df$User[df$Measure=="CHW proportion of care"])
            )
        }
        
        
        rhandsontable(DF, language = "en-EN", rowHeaders = NULL, height = '100%', overflow='visible',
                      colHeaders = c("Activity", "Time per patient (minutes)","Care provided by doctor", 
                                     "Care provided by nurse","Care provided by CHW")
        )%>%
            hot_cols(colWidths = c(4,2,2,2,2), columnSorting = NULL) %>%
            hot_col("Care provided by doctor", format = "0%")%>%
            hot_col("Care provided by nurse", format = "0%")%>%
            hot_col("Care provided by CHW", format = "0%")%>%
            hot_col("Activity", readOnly = T)%>%
            hot_table(stretchH = "all")
    })
    
    
    output$ddxcosts<-renderRHandsontable({
        df<-hearts()

        if(input$hearts=="HEARTS defaults"){
            
            DF<-data.frame(
                Component=c("Physical exam", "Estimate CVD risk", "Analyze blood test", "Analyze urine test"),
                Minutes = c(5,5,4,4),
                Doctor = c(0.9,0.9,0,0),
                Nurse = c(0.1,0.1,0,0),
                CHW = c(0,0,0,0),
                Labtech = c(0,0,1,1)
            )
        }
        
        else{
            DF<-data.frame(
                Component=c("Physical exam", "Estimate CVD risk", "Analyze blood test", "Analyze urine test"),
                Minutes = c(df$User[df$Measure=="Provide a physical exam to assess patients' total CVD risk"],
                            df$User[df$Measure=="Assess patient risk using a CVD risk chart"],
                            df$User[df$Measure=="Administer and analyze a blood test"],
                            df$User[df$Measure=="Administer and analyze a urine test"]),
                Doctor = c(df$User[df$Measure=="Doctor proportion of care"],df$User[df$Measure=="Doctor proportion of care"],0,0),
                Nurse = c(df$User[df$Measure=="Nurse proportion of care"],df$User[df$Measure=="Nurse proportion of care"],0,0),
                CHW = c(df$User[df$Measure=="CHW proportion of care"],df$User[df$Measure=="CHW proportion of care"],0,0),
                Labtech = c(0,0,1,1)
            )
        }
        
        
        rhandsontable(DF, language = "en-EN", rowHeaders = NULL, height = '100%', overflow='visible',
                      colHeaders = c("Activity", "Time per patient (minutes)",
                                     "Care provided by doctor", "Care provided by nurse",
                                     "Care provided by CHW", "Care provided by lab technician")
        )%>%
            hot_cols(colWidths = c(4,2,2,2,2,2), columnSorting = NULL) %>%
            hot_col("Activity", readOnly = T)%>%
            hot_col("Care provided by doctor", format = "0%")%>%
            hot_col("Care provided by nurse", format = "0%")%>%
            hot_col("Care provided by CHW", format = "0%")%>%
            hot_col("Care provided by lab technician", format = "0%", readOnly = T)%>%
            hot_table(stretchH = "all")
    })    
    
    
    output$ddxitems<-renderRHandsontable({
        df<-hearts()

        if(input$hearts=="HEARTS defaults"){
            
            DF<-data.frame(
                Component=c("Fasting glucose", "Blood lipid panel", "Urine analysis"),
                Cost=c(df$Default[df$Measure=="Fasting blood glucose (FPG)"], 
                       df$Default[df$Measure=="Blood lipid panel"],
                       df$Default[df$Measure=="Urine analysis"])
            )
            
        }
        
        else{
            DF<-data.frame(
                Component=c("Fasting glucose", "Blood lipid panel", "Urine analysis"),
                Cost=c(df$User[df$Measure=="Fasting blood glucose (FPG)"], 
                       df$User[df$Measure=="Blood lipid panel"],
                       df$User[df$Measure=="Urine analysis"])
            )
        }
        
        if(input$currency=="USD"){
            lang<-"en-EN"
            DF$Cost<-DF$Cost/input$exchange
        }
        else{
            lang<-currency$currency[currency$Country==input$country]
        }
        
        rhandsontable(DF, language = "en-EN", rowHeaders = NULL, height = '100%', overflow='visible',
                      colHeaders = c("Component","Unit cost")
        )%>%
            hot_cols(colWidths = c(4,2), columnSorting = NULL) %>%
            hot_col("Unit cost", format = "$0.00", language = lang)%>%
            hot_col("Component", readOnly = T)%>%
            hot_table(stretchH = "all")
    })
    
    #https://handsontable.com/docs/8.3.2/demo-dropdown.html
    output$rxcosts<-renderRHandsontable({
        treat<-hearts()
        names(treat)[1]<-"Medication"
        reg<-read.csv("regimens.csv")
        
        if(input$hearts=="HEARTS defaults"){
            DF<-left_join(reg%>%filter(Regimen==input$protocol & Class%in%c(input$med1, input$med2, input$med3, input$med4)), treat, by="Medication")
            DF$Proportion[DF$Class==input$med3]<-DF$Proportion[DF$Class==input$med3]*input$acei/100
            DF$Proportion[DF$Class==input$med4]<-DF$Proportion[DF$Class==input$med4]*(100-input$acei)/100
            DF<-DF%>%select(c("Step", "Dose", "Medication", "Tabs", "Days", "Proportion", "Default"))  
        }
        
        else{
            DF<-left_join(reg%>%filter(Regimen==input$protocol & Class%in%c(input$med1, input$med2, input$med3, input$med4)), treat, by="Medication")
            DF$Proportion[DF$Class==input$med3]<-DF$Proportion[DF$Class==input$med3]*input$acei/100
            DF$Proportion[DF$Class==input$med4]<-DF$Proportion[DF$Class==input$med4]*(100-input$acei)/100
            DF<-DF%>%select(c("Step", "Dose", "Medication", "Tabs", "Days", "Proportion", "User"))  
        }
        
        names(DF)[7]<-"UC"
        
        if(input$currency=="USD"){
            lang<-"en-EN"
            DF$UC<-DF$UC/input$exchange
        }
        else{
            lang<-currency$currency[currency$Country==input$country]
        }
        
        rhandsontable(DF, language="en-EN", stretchH = "all", rowHeaders = NULL, height ='100%', overflow='visible',
                      colHeaders = c("Step", "Dose", "Medication", "Tabs", "Days", "Proportion", "Cost per tab"))%>%
                    hot_cols(colWidths = c(2,2,4,2,2,2,2))%>%
                    hot_table(stretchH = "all")
    })
    
    #fwrite(DF,"output/rxcosts_DF.csv")
    
    output$reviewcosts<-renderRHandsontable({
        df<-hearts()
        
        
        if(input$hearts=="HEARTS defaults"){
            DF<-data.frame(
                Risk=c("Low CVD risk", "Medium CVD risk", "High CVD risk"),
                Proportion=c(input$lowrisk, input$medrisk, input$highrisk),
                Visits=c(df$Default[df$Measure=="Low CVD risk"],
                         df$Default[df$Measure=="Medium CVD risk"],
                         df$Default[df$Measure=="High CVD risk"]),
                Time=c(5,10,10),
                Doctor=c(0.1,1,1),
                Nurse=c(0.9,0,0),
                CHW=c(0,0,0)
            )
        }
        else{
            DF<-data.frame(
                Risk=c("Low CVD risk", "Medium CVD risk", "High CVD risk"),
                Proportion=c(input$lowrisk, input$medrisk, input$highrisk),
                Visits=c(df$User[df$Measure=="Low CVD risk"],
                         df$User[df$Measure=="Medium CVD risk"],
                         df$User[df$Measure=="High CVD risk"]),
                Time=c(5,10,10),
                Doctor=c(0.1,1,1),
                Nurse=c(0.9, 0, 0),
                CHW=c(0,0,0)
            )
        }
        
        rhandsontable(DF, language="en-EN", stretchH = "all", rowHeaders = NULL, height ='100%', overflow='visible',
                      colHeaders = c("Risk group", "Proportion of patients (%)", 
                                     "Visits per year", "Time per visit (minutes)",
                                     "Care provided by doctor", "Care provided by nurse",
                                     "Care provided by CHW"))%>%
            hot_cols(colWidths = c(2,2,2,2,2,2,2))%>%
            hot_col("Risk group", readOnly = T)%>%
            hot_col("Proportion of patients (%)", readOnly = T)%>%
            hot_table(stretchH = "all")
    })
    
    output$markups<-renderRHandsontable({
        df<-data.frame(
            Category=c("Training", "Health info. systems ", "Governance/admin",
                       "Health financing ", "Supply chain strengthening"),
            Markup=c(0.005,0.015,0.043,0.018, 0.12),
            Share =c("Total cost", "Total cost", "Total cost", "Total cost", "Medication cost")
        )
        
        rhandsontable(df, stretchH = "all", rowHeaders = NULL, height ='100%', overflow='visible' 
        )%>%
            hot_col("Markup", format = "0.0%")
    })
    
    
    
#############################################
### Results tab server ###
#############################################
    
    save(input, file = "output/input.RData")
    
    output$country_results<-renderText({
        input$plot
        isolate({
            deltasalt<-as.data.frame(hot_to_r(input$salttable))
            
            policylist<-gsub("\\, NA*","", paste0(
                input$NaPolicies[1], ", ", input$NaPolicies[2],", ",
                input$NaPolicies[3], ", ", input$NaPolicies[4],", ",
                input$NaPolicies[5])
            )
            
            if(input$targets=="Alt targets"){
                htntext<-paste0("Hypertension screening in this analysis is scaled up from the year ",
                                input$drugcovyear[1], " to the year ",input$drugcovyear[2], 
                                " to increase the proportion of patients with hypertension who are 
                            aware of their diagnosis from ", input$aware1, "% to ", input$aware2, "%. ")
            }
            
            else{
                htntext<-paste0("Hypertension programs in this analysis are scaled up from the year ",
                                input$drugcovyear[1], " to the year ",input$drugcovyear[2], 
                                " to achieve ", input$aware_target, "% awareness, ", input$treated_target, 
                                "% treatment, and ", input$controlled_target, "% control rates (among individuals with
                            hypertension. ")
            }
            
            if(!is.null(input$NaPolicies)){
                salttext<-paste0("Sodium policies (", policylist, ") in this
            analysis are scaled up from the year ", input$saltyear[1], 
                                 " to the year ", input$saltyear[2]," to achieve a ", deltasalt$Reduced[1] ,
                                 "g reduction in mean population sodium intake. ")
            }
            
            else{
                salttext<-""
            }
            
            paste0("Showing results for ", input$country," in ", input$currency, ". ", salttext, htntext)
        })
    })
    
    ### tables & figures ###
    
    output$resultsmed<-renderRHandsontable({
        if(input$tabs=='Alt targets'){
            PIN_treat<-(input$utilization/100)*(input$prev1/100)*(input$aware2/100)*(input$treated1/input$aware1)
        }
        else{
            PIN_treat<-(input$utilization/100)*(input$prev1/100)*(input$treated_target/100)
        }
        
        if(is.null(input$plot)){
            
        }
        else{
        epi<-rv$data%>%filter(year==input$year , age.group=="All ages (20-95)", cause=="All causes", intervention=="Both", sex=="Both", location==input$country)
        pop<-epi$Pop/4
        meds<-as.data.frame(hot_to_r(input$rxcosts))
        DF<-meds%>%group_by(Medication)%>%summarize(Doses=sum(Days*Tabs*Proportion), UC=mean(UC))
        DF$Doses<-DF$Doses*pop*PIN_treat
        DF$Safety<-DF$Doses*input$safetystock/100
        DF$Total<-DF$UC*(DF$Doses+DF$Safety)
        
        #patch
        rxcost.temp <-DF
        rxcost.temp$pop <-pop
        rxcost.temp$PIN_treat <-PIN_treat
        
        fwrite(meds,"output/rxcosts_InputDF.csv")
        fwrite(rxcost.temp,"output/rxcosts_CalculaDF.csv")
        
        for(i in 1:length(DF$Doses)){
        DF$Doses[i]<-so_formatter(signif(as.numeric(DF$Doses[i]), digits=3))
        DF$Safety[i]<-so_formatter(signif(as.numeric(DF$Safety[i]), digits=3))
        DF$Total[i]<-paste0("$", so_formatter(signif(as.numeric(DF$Total[i]), digits=3)))
        }
        
        #COUNTRY <- unique(input$country)
        #fwrite(DF,paste0("output/resultsmed",".csv"))
        fwrite(epi,"output/resultsmed_epi.csv")
        fwrite(DF,"output/resultsmed_DF.csv")
        
        
        rhandsontable(DF, language = "en-EN", rowHeaders = NULL, height = '100%', overflow='visible', 
                      colHeaders = c("Medication", "Doses", "Cost per dose", "Safety stock doses", "Total costs"))%>%
            hot_cols(colWidths = c(4,2,2,2,2), columnSorting = NULL) %>%
            hot_table(stretchH = "all")%>%
            hot_col("Doses", readOnly = T)%>%
            hot_col("Cost per dose", readOnly = T, format = "$0.00")%>%
            hot_col("Total costs", readOnly = T)%>%
            hot_col("Medication", readOnly = T)%>%
            hot_col("Safety stock doses", readOnly = T)
        }
    })
    
    output$acosts<-renderPlotly({
        input$plot
        isolate({
            
        costs<-as.data.frame(hot_to_r(input$unitcosts))
        epi<-rv$data%>%filter(age.group=="All ages (20-95)", cause=="All causes", intervention=="Both", sex=="Both", location==input$country)
        markup<-as.data.frame(hot_to_r(input$markups))
        
        if(input$currency=="USD"){
            exchange<-1
        }
        else{
            exchange<-input$exchange
        }
        
        if(input$tabs=='Alt targets'){
            base<-input$aware1
            #PIN_screen<-(input$utilization/100)*(input$prev1/100)*(input$aware2/100)
            gap<-(input$aware2-input$aware1)
            adjustment<-(input$treated1/input$aware1)*(input$utilization/100)*(input$prev1/100)
            aware2<-input$aware2
        }
        else{
            base<-input$treated1
            #PIN_screen<-(input$utilization/100)*(input$prev1/100)*(input$aware_target/100)
            gap<-input$treated_target-input$treated1
            adjustment<-(input$utilization/100)*(input$prev1/100)
            aware2<-input$aware_target
        }
        
        
        for(y in 2020:2040){
            if(y<input$drugcovyear[1]){
                epi$HRcosts[epi$year==y]<-((epi$Pop[epi$year==y]/4)*(input$utilization/100)*costs$Screening[1])+
                             ((epi$Pop[epi$year==y]/4)*(input$utilization/100)*(input$prev1/100)*(input$treated1/100)*costs$Clinical_review[1])
                epi$Medcosts[epi$year==y]<-(epi$Pop[epi$year==y]/4)*(input$utilization/100)*(input$prev1/100)*(input$treated1/100)*costs$Treatment[1]*(1+input$safetystock)
        
                epi$ddxcosts[epi$year==y]<-(epi$Pop[epi$year==y]/4)*(input$utilization/100)*(input$prev1/100)*(input$aware1/100)*costs$Diagnosis[1]
                #epi$HScosts[epi$year==y]<-(epi$Pop[epi$year==y]/4)*(input$utilization/100)*costs$Health_system[1]
            }
            
            else if(y>=input$drugcovyear[1] & y<input$drugcovyear[2]){
                step<-(y-input$drugcovyear[1]+1)/(input$drugcovyear[2]-input$drugcovyear[1]+1)
                PIN_treat<-((base+gap*step)/100)*adjustment
                PIN_ddx<-((input$aware1+(aware2-input$aware1)*step)/100)*(input$utilization/100)*(input$prev1/100)
                
                epi$HRcosts[epi$year==y]<-((epi$Pop[epi$year==y]/4)*(input$utilization/100)*costs$Screening[1])+
                                          ((epi$Pop[epi$year==y]/4)*PIN_treat*costs$Clinical_review[1])
                epi$Medcosts[epi$year==y]<-(epi$Pop[epi$year==y]/4)*PIN_treat*costs$Treatment[1]*(1+input$safetystock)
                
                epi$ddxcosts[epi$year==y]<-(epi$Pop[epi$year==y]/4)*PIN_ddx*costs$Diagnosis[1]
                #epi$HScosts[epi$year==y]<-(epi$Pop[epi$year==y]/4)*(input$utilization/100)*costs$Health_system[1]
            }
            
            else {
                epi$HRcosts[epi$year==y]<-((epi$Pop[epi$year==y]/4)*(input$utilization/100)*costs$Screening[1])+
                                          ((epi$Pop[epi$year==y]/4)*((base+gap)/100)*adjustment*costs$Clinical_review[1])
                epi$Medcosts[epi$year==y]<-(epi$Pop[epi$year==y]/4)*((base+gap)/100)*adjustment*costs$Treatment[1]*(1+input$safetystock)
                
                epi$ddxcosts[epi$year==y]<-(epi$Pop[epi$year==y]/4)*(input$utilization/100)*(input$prev1/100)*(aware2/100)*costs$Diagnosis[1]
                #epi$HScosts[epi$year==y]<-(epi$Pop[epi$year==y]/4)*(input$utilization/100)*costs$Health_system[1]
            }
        }
        
        for (y in 2020:2040){
            if(y<input$saltyear[1]){
                epi$saltcosts[epi$year==y]<-0
            }
            else if (y>=input$saltyear[1] & y<input$saltyear[2]){
                epi$saltcosts[epi$year==y]<-(epi$Pop[epi$year==y]/4)*saltcost()*exchange*(y-input$saltyear[1]+1)/(input$saltyear[2]-input$saltyear[1]+1)
            }
            else{
                epi$saltcosts[epi$year==y]<-(epi$Pop[epi$year==y]/4)*saltcost()*exchange
            }
        }
        
        
        epi$HScosts<-(epi$HRcosts+epi$Medcosts+epi$ddxcosts)*(sum(markup$Markup[markup$Share!="Medication cost"]))+
            epi$Medcosts*(markup$Markup[markup$Share=="Medication cost"])
        
        fwrite(epi,"output/acost_epiComponentCosts.csv")
        
        fig<-plot_ly(epi, x=~year, y=~Medcosts, name="Medication costs", 
                     type='scatter', mode='none', stackgroup='one',
                     hoverinfo='text', fillcolor='#cc79a7',
                     text=~paste0("Year: ", epi$year, "\nTotal annual cost: ", 
                                  so_formatter(signif(epi$Medcosts, digits=3))))
        fig<-fig%>%add_trace(y=~HRcosts, name="Human resource costs", 
                             fillcolor='#d55e00',
                             hoverinfo='text',
                             text=~paste0("Year: ", epi$year, "\nTotal annual cost: ", 
                                          so_formatter(signif(epi$HRcosts, digits=3))))
        fig<-fig%>%add_trace(y=~ddxcosts, name="Diagnostics costs", fillcolor='#0072b2',
                             hoverinfo='text',
                             text=~paste0("Year: ", epi$year, "\nTotal annual cost: ", 
                                          so_formatter(signif(epi$ddxcosts, digits=3))))
        fig<-fig%>%add_trace(y=~HScosts, name="Health system costs", fillcolor='#f0e442',
                             hoverinfo='text',
                             text=~paste0("Year: ", epi$year, "\nTotal annual cost: ", 
                                          so_formatter(signif(epi$HScosts, digits=3))))
        fig<-fig%>%add_trace(y=~saltcosts, name="Sodium policy costs", fillcolor='#009e73',
                             hoverinfo='text',
                             text=~paste0("Year: ", epi$year, "\nTotal annual cost: ", 
                                          so_formatter(signif(epi$saltcosts, digits=3))))%>%
            layout(title="Total annual costs by component over time",
                   titlefont=list(size=12),
                   font=list(size=11),
                   xaxis=list(title="", showgrid=FALSE),
                   yaxis=list(title=paste0("Cost (", input$currency,")"), showgrid=FALSE),
                   legend = list(orientation = 'h'),
                   hovermode='compare')%>%
            config(modeBarButtonsToRemove = list("resetScale2d", "zoom2d", "autoScale2d", "toggleSpikelines","pan2d","zoomIn2d", "zoomOut2d"))
        
        fig
    })
})

    
    output$impacts<-renderRHandsontable({
        input$plot
        isolate({
        #costs<-as.data.frame(input$resultstab1)
        epi<-rv$data%>%filter(age.group=="All ages (20-95)", sex=="Both", cause=="All causes", location==input$country)
        costs<-as.data.frame(hot_to_r(input$unitcosts))
        markup<-as.data.frame(hot_to_r(input$markups))
        
        if(input$currency=="USD"){
            exchange<-1
            dollar.sign<-"$"
        }
        else if (input$currency!="USD" & input$country=="Thailand"){
            exchange<-input$exchange
            dollar.sign<-"\u0E3F"
        }
        
        else if (input$currency!="USD" & input$country=="Bangladesh"){
            exchange<-input$exchange
            dollar.sign<-"\u09F3"
        }
        
        else{
            exchange<-input$exchange
            dollar.sign<-"$"
        }
        
        if(input$targets=='Alt targets'){
            PIN_treat<-(input$utilization/100)*(input$prev1/100)*(input$aware2/100)*(input$treated1/input$aware1)
            PIN_ddx<-(input$utilization/100)*(input$prev1/100)*(input$aware2/100)
        }
        else{
            PIN_treat<-(input$utilization/100)*(input$prev1/100)*(input$treated_target/100)
            PIN_ddx<-(input$utilization/100)*(input$prev1/100)*(input$aware_target/100)
        }
        
        UC<-(input$utilization/100)*costs$Screening[1]*(1+sum(markup$Markup[markup$Share!="Medication cost"]))+
            PIN_treat*costs$Clinical_review[1]*(1+sum(markup$Markup[markup$Share!="Medication cost"]))+
            PIN_treat*costs$Treatment[1]*(1+input$safetystock)*(1+sum(markup$Markup))+ 
            PIN_ddx*costs$Diagnosis[1]*(1+sum(markup$Markup[markup$Share!="Medication cost"]))+
            saltcost()*exchange
        
        UC0<-(input$utilization/100)*costs$Screening[1]*(1+sum(markup$Markup[markup$Share!="Medication cost"]))+
            (input$utilization/100)*(input$prev1/100)*(input$treated1/100)*costs$Clinical_review[1]*(1+sum(markup$Markup[markup$Share!="Medication cost"]))+
            (input$utilization/100)*(input$prev1/100)*(input$treated1/100)*costs$Treatment[1]*(1+input$safetystock)*(1+sum(markup$Markup))+ 
            (input$utilization/100)*(input$prev1/100)*(input$aware1/100)*costs$Diagnosis[1]*(1+sum(markup$Markup[markup$Share!="Medication cost"]))
        
        DF<-data.frame(
            Scenario=c("Baseline", "Target", NA, 
                       "Baseline", "Target", NA, 
                       NA),
            Measure=c("Total cost", "Total cost", "Incremental annual cost", 
                      " CVD Deaths", "CVD Deaths", "Lives saved", 
                      "Cost effectiveness ratio (Cost/life saved)"),
            x2030=c((epi$Pop[epi$intervention=="Baseline"& epi$year==2030]*UC0/4),
                    (epi$Pop[epi$intervention=="Both" & epi$year==2030]*UC/4),
                    (epi$Pop[epi$intervention=="Both" & epi$year==2030]*UC/4)-(epi$Pop[epi$intervention=="Baseline"& epi$year==2030]*UC0/4),
                    (epi$Death[epi$intervention=="Baseline" & epi$year==2030]),
                    (epi$Death[epi$intervention=="Both" & epi$year==2030]),
                    (epi$Death[epi$intervention=="Baseline" & epi$year==2030]-epi$Death[epi$intervention=="Both" & epi$year==2030]),
                    0),
            x2040=c((epi$Pop[epi$intervention=="Baseline"& epi$year==2040]*UC0/4),
                    (epi$Pop[epi$intervention=="Both" & epi$year==2040]*UC/4),
                    (epi$Pop[epi$intervention=="Both" & epi$year==2040]*UC/4)-(epi$Pop[epi$intervention=="Baseline"& epi$year==2040]*UC0/4),
                    (epi$Death[epi$intervention=="Baseline" & epi$year==2040]),
                    (epi$Death[epi$intervention=="Both" & epi$year==2040]),
                    (epi$Death[epi$intervention=="Baseline" & epi$year==2040]-epi$Death[epi$intervention=="Both" & epi$year==2040]),
                    0)
        )
        DF$x2030[DF$Measure=="Cost effectiveness ratio (Cost/life saved)"]<- DF$x2030[DF$Measure=="Incremental annual cost"]/DF$x2030[DF$Measure=="Lives saved"]
        DF$x2040[DF$Measure=="Cost effectiveness ratio (Cost/life saved)"]<- DF$x2040[DF$Measure=="Incremental annual cost"]/DF$x2040[DF$Measure=="Lives saved"]
        DF[DF<=0]<-0
        
        for(i in 1:length(DF$x2030)){
            DF$x2030[i]<-so_formatter(signif(as.numeric(DF$x2030[i]), digits=3))
            DF$x2040[i]<-so_formatter(signif(as.numeric(DF$x2040[i]), digits=3))
        }
        
        DF$x2030[DF$Measure=="Total cost"]<-paste(dollar.sign, DF$x2030[DF$Measure=="Total cost"])
        DF$x2040[DF$Measure=="Total cost"]<-paste(dollar.sign, DF$x2040[DF$Measure=="Total cost"])
        DF$x2030[DF$Measure=="Incremental annual cost"]<-paste(dollar.sign, DF$x2030[DF$Measure=="Incremental annual cost"])
        DF$x2040[DF$Measure=="Incremental annual cost"]<-paste(dollar.sign, DF$x2040[DF$Measure=="Incremental annual cost"])
        
        fwrite(DF,"output/Impacts.csv")
        
        rhandsontable(DF, rowHeaders = F, colHeaders = c("Scenario", "Measure", "2030", "2040"))%>%
            hot_cols(colWidths = c(2,2,2,2))%>%
            hot_col("Scenario", readOnly = T)%>%
            hot_col("Measure", readOnly = T)%>%
            hot_col("2030", readOnly=T)%>%
            hot_col("2040", readOnly = T)%>%
            hot_table(stretchH = "all")
    })
})
    
    
    observeEvent(input$tabs, {
            req(input$tabs == 'results')
            base_rates<-b_rates[location==input$country]#[, -c("year")]
            deltasalt<-as.data.frame(hot_to_r(input$salttable))
            #################################################################################################
            #################################################################################################
            #intervention scenarios
            
            DT<-data.in[location==input$country][,Year:=2017][,-c("Low95CI", "High95CI")]
            
            repYear<-function(row){
                2017+floor((row-1)/224)
            }
            
            DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
            
            if(input$targets=="Alt targets"){
            control2<-input$aware2*(input$controlled1/input$aware1)
            }
            else{
            control2<-input$controlled_target
            }
            
            bp_prob_salt<-get.bp.prob(DT.in, deltasalt$Reduced[1]*2.54, input$saltyear[1], input$saltyear[2], 0, 0, 0, 0, 0)
            DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
            
            bp_prob_drug<-get.bp.prob(DT.in, 0,  input$saltyear[1], input$saltyear[2], 
                                      1, input$controlled1, control2, input$drugcovyear[1], input$drugcovyear[2])
            DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
            bp_prob_both<-get.bp.prob(DT.in, deltasalt$Reduced[1]*2.54, input$saltyear[1], input$saltyear[2],
                                      1, input$controlled1, control2, input$drugcovyear[1], input$drugcovyear[2])
            DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
            bp_prob_base<-get.bp.prob(DT.in, 0, input$saltyear[1], input$saltyear[2], 0, 0, 0, 0, 0)
            
            bp_prob_salt[,intervention:="Sodium reduction"]
            bp_prob_drug[,intervention:="Antihypertensive therapy"]
            bp_prob_both[,intervention:="Both"]
            
            setnames(bp_prob_base, "prob", "prob_0")
            bp_probs<-rbindlist(list(bp_prob_both, bp_prob_salt, bp_prob_drug))
            bp_probs<-merge(bp_probs, bp_prob_base, by=c("age","sex", "bp_cat", "Year", "location"))
            
            #duplicating data to be age-specific
            bp_probs[, age:=as.numeric(substr(age, 1,2))]
            bp_probs<-bp_probs[rep(seq_len(nrow(bp_probs)), each=5)]
            bp_probs[,age2:=rep(1:5, nrow(bp_probs)/5)][,age:=age+age2-1]
            
            over90<-bp_probs[age==89]
            
            over90<-over90[rep(seq_len(nrow(over90)), each=6)]
            over90[,age2:=rep(1:6, nrow(over90)/6)][,age:=age+age2]
            
            #bind  
            bp_probs<-rbindlist(list(bp_probs, over90))
            
            ##add RRis##
            bp_probs[, RRi_IHD:=sapply(bp_cat, addRR, RR=0.83)]
            bp_probs[, RRi_HHD:=sapply(bp_cat, addRR, RR=0.72)]
            bp_probs[, RRi_stroke:=sapply(bp_cat, addRR, RR=0.73)]
            
            
            ##add alphas##
            alphas<-bp_probs[,.(ihd=sum(prob_0*RRi_IHD), istroke=sum(prob_0*RRi_stroke), 
                                hstroke=sum(prob_0*RRi_stroke), hhd=sum(prob_0*RRi_HHD)), 
                             by=.(age, sex, location, intervention, Year)] #change to "Year"
            
            alphas<-melt(alphas, id.vars=c("age", "sex", "location", "intervention", "Year"), measure.vars=c(), variable.name = "cause",
                         value.name="alpha")#change to "Year"
            
            
            rris<-bp_probs[,list(age, sex, Year, location, intervention, bp_cat, prob, RRi_IHD, RRi_HHD, RRi_stroke)]#change to "Year"
            rris[,hstroke:=RRi_stroke]
            
            setnames(rris, c("RRi_IHD", "RRi_HHD", "RRi_stroke"), c("ihd", "hhd","istroke"))
            rris<-melt(rris, id.vars=c("age", "sex", "location", "intervention", "bp_cat", "prob", "Year"), measure.vars=c(), variable.name = "cause",
                       value.name="RRi")#change to "Year"
            
            bp_probs<-merge(rris, alphas, by=c("age", "sex", "location", "intervention","cause", "Year"))#change to "Year"
            setnames(bp_probs, "Year", "year")
            
            ####adding baseline_rates
            intervention_rates<-merge(bp_probs, base_rates, by=c("age", "sex", "location", "cause", "year"))
            
            #calculating yi*pi
            intervention_rates[, yixpi:=(RRi*IR/alpha)*prob]
            intervention_rates[, IR:=sum(yixpi), by=.(age, sex, location, intervention, cause, CF, 
                                                      BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, year, ALL.mx)]#change to "Year"
            
            #intervention_rates[, year:=NULL]
            #setnames(intervention_rates, "Year", "year")
            intervention_rates<-unique(intervention_rates[,-c("prob", "bp_cat", "yixpi", "RRi", "alpha")])
            
            #################################################################################################
            
            intervention_rates[CF>0.99, CF:=0.99]
            intervention_rates[IR>0.99, IR:=0.99]
            
            #STATE TRANSITIONS#
            for(i in 1:24){
                
                b2<-intervention_rates[year<=2017+i & year>=2017+i-1]
                b2[,age2:=age+1]
                
                #sick
                b2[, sick2:=shift(sick)*(1-(CF+BG.mx)) + shift(well)*IR, by=.(sex, location, cause, age, intervention)]
                #b2[age2>=95, sick2:=sick2+shift(sick2, type="lead"), by=.(sex, location, cause, year)]
                
                #dead
                b2[, dead2:=shift(sick)*CF, by=.(sex, location, cause, age, intervention)]
                #b2[age2>=95, dead2:=dead2+shift(dead2, type="lead"), by=.(sex, location, cause, year)]
                
                #pop
                b2[,pop2:=shift(pop)-shift(all.mx), by=.(sex, location, cause, age, intervention)]
                #b2[age2>=95, pop2:=pop2+shift(pop2, type="lead"), by=.(sex, location, cause, year)]
                b2[pop2<0, pop2:=0] #prevent negatives
                
                #all dead
                b2[, all.mx2:=sum(dead2), by=.(sex, location, year, age, intervention)]
                b2[,all.mx2:=all.mx2+(pop2*BG.mx.all)]
                
                #well
                b2[, well2:=pop2-all.mx2-sick2]
                b2[well2<0, well2:=0] #prevent negatives
                
                #re-combined into original data.table
                b2<-b2[year==2017+i & age2<96, c("age2", "sick2", "dead2", "well2", "pop2", 
                                                 "all.mx2", "sex", "location", "cause", "intervention")]
                setnames(b2, "age2", "age")
                intervention_rates[year==2017+i & age>20, sick:=b2[,sick2]]
                intervention_rates[year==2017+i & age>20, dead:=b2[,dead2]]
                intervention_rates[year==2017+i & age>20, well:=b2[,well2]]
                intervention_rates[year==2017+i & age>20, pop:=b2[,pop2]]
                intervention_rates[year==2017+i & age>20, all.mx:=b2[,all.mx2]]
                
            }
            
            out.df<-intervention_rates[, c("age", "cause", "sex", "year", "well", "sick",
                                           "dead", "pop", "all.mx", "intervention", "location")]
            
            #################################################################################################
            
            #baseline_out <- baseline%>%filter(location==input$country)#%>%select(-c(location))
            #baseline_out[,intervention:="Baseline"]
            base<-baseline[location==input$country, c("age", "sex", "year", "cause", "well", "sick", "dead",
                                                       "pop", "all.mx", "location", "intervention")]
            graphs<-rbindlist(list(base, out.df), use.names=TRUE)
            
            graphs$pop[graphs$pop==0]<-1
            
            fwrite(graphs,"output/ObservedEvent_graphs.csv")
            #baseline_out$intervention<-"Baseline"
            #out.df<-bind_rows(baseline_out, out.df)
            
            ##making age groups#
            all<-graphs[, .(Prevalence=sum(sick), Death=sum(dead), Pop=sum(pop)), 
                        by=.(sex, year, cause, intervention, location)]
            all[, age.group:="All ages (20-95)"]
            
            allu40<-graphs[age<40, .(Prevalence=sum(sick), Death=sum(dead), Pop=sum(pop)), 
                           by=.(sex, year, cause, intervention, location)]
            allu40[, age.group:="20-39"]
            
            all3069<-graphs[age<70 & age>=30, .(Prevalence=sum(sick), Death=sum(dead), Pop=sum(pop)), 
                            by=.(sex, year, cause, intervention, location)]
            all3069[, age.group:="30-69"]
            
            allo70<-graphs[age>=70, .(Prevalence=sum(sick), Death=sum(dead), Pop=sum(pop)), 
                           by=.(sex, year, cause, intervention, location)]
            allo70[, age.group:="70-95"]
            
            allu70<-graphs[age<70, .(Prevalence=sum(sick), Death=sum(dead), Pop=sum(pop)), 
                           by=.(sex, year, cause, intervention, location)]
            allu70[, age.group:="20-69"]
            
            allage<-rbindlist(list(all, allu40, all3069, allo70, allu70))
            
            # creating "both sexes" category#
            allsex<-allage[, .(Prevalence=sum(Prevalence), Death=sum(Death), Pop=sum(Pop)), 
                           by=.(age.group, year, cause, intervention, location)]
            allsex[, sex:="Both"]
            graphitSA<-rbindlist(list(allage, allsex), use.names=TRUE)
            
            # creating all cvd causes #
            allcause<-graphitSA[, .(Prevalence=sum(Prevalence), Death=sum(Death), Pop=sum(Pop)), 
                                by=.(age.group, year, sex, intervention, location)]
            allcause[, cause:="All causes"]
            graphitSA<-rbindlist(list(graphitSA, allcause), use.names=TRUE)
            
            #country aggregate results
            allcountry<-graphitSA[, .(Prevalence=sum(Prevalence), Death=sum(Death), Pop=sum(Pop)), 
                                  by=.(age.group, year, cause, intervention, sex)]
            allcountry[, location:="Country aggregate"]
            graphitSA<-rbindlist(list(graphitSA, allcountry), use.names=TRUE)
            
            #prevalence and death rates
            graphitSA[, prevrate:=(Prevalence/Pop)*100000]
            graphitSA[, deathrate:=(Death/Pop)*100000]
            
            ##age-standardized rates##
            allgraphs<-graphs[, .(sick=sum(sick), dead=sum(dead), pop=sum(pop)), 
                              by=.(age, year, cause, intervention, sex)]
            allgraphs[,location:="Country aggregate"]
            graphs<-rbindlist(list(allgraphs, graphs), use.names=TRUE, fill=TRUE)
            
            ## use 2040 baseline for reference population in age-standardization
            #ref<-fread("global_pop_0122.csv")
            new.out<-merge(graphs, ref, by=c("sex", "age", "cause", "location"))
            
            new.out[,crudeprevrate:=sick/pop]
            new.out[,crudedeathrate:=dead/pop]
            
            maletotal<-sum(ref$Nx2040[ref$sex=="Male" & ref$location!="Country aggregate" &  ref$cause=="ihd"])
            femtotal<-sum(ref$Nx2040[ref$sex=="Female" & ref$location!="Country aggregate" &  ref$cause=="ihd"])
            
            new.out[, newprev:=crudeprevrate*Nx2040]
            new.out[, newdead:=crudedeathrate*Nx2040]
            
            age.standardized.rates<-new.out[, .(prevrate=sum(newprev), deathrate=sum(newdead)), 
                                            by=.(sex, location, cause, year, intervention)]
            
            age.standardized.rates[sex=="Male", prevrate:=(prevrate/maletotal)*100000]
            age.standardized.rates[sex=="Male", deathrate:=(deathrate/maletotal)*100000]
            age.standardized.rates[sex=="Female", prevrate:=(prevrate/femtotal)*100000]
            age.standardized.rates[sex=="Female", deathrate:=(deathrate/femtotal)*100000]
            age.standardized.rates[,age.group:="Age-standardized"]
            
            allcause2<-age.standardized.rates[, .(prevrate=sum(prevrate), deathrate=sum(deathrate)), 
                                              by=.(age.group, sex, year, intervention, location)]
            allcause2[, cause:="All causes"]
            age.standardized.rates<-rbindlist(list(age.standardized.rates, allcause2), use.names = TRUE)
            
            allsex2<-age.standardized.rates[, .(prevrate=sum(prevrate), deathrate=sum(deathrate)), 
                                            by=.(age.group, year, cause, intervention, location)]
            allsex2[,sex:="Both"]
            age.standardized.rates<-rbindlist(list(age.standardized.rates, allsex2), use.names=TRUE)
            
            graphitSA<-rbindlist(list(graphitSA, age.standardized.rates), use.names=TRUE, fill=TRUE)
            
            graphitSA[cause=="ihd", cause:="Ischemic heart disease"]
            graphitSA[cause=="hhd", cause:= "Hypertensive heart disease"]
            graphitSA[cause=="istroke", cause:="Ischemic stroke"]
            graphitSA[cause=="hstroke", cause:="Hemorrhagic stroke"]
            
            fwrite(graphitSA,"output/ObservedEvent_graphitSA.csv")
            
            rv$data<-as.data.frame(graphitSA)
    })#end of obersveEvent
    
    
    ##CEA plot
    
    output$ceaplot<-renderPlotly({
        
        input$plot
        isolate({
            
        epi<-rv$data%>%filter(age.group=="All ages (20-95)", sex=="Both", cause=="All causes", location==input$country)
        costs<-as.data.frame(hot_to_r(input$unitcosts))
        markup<-as.data.frame(hot_to_r(input$markups))
        
        if(input$currency=="USD"){
            exchange<-1
        }
        else{
            exchange<-input$exchange
        }
        
        basedf<-data.frame(
            year=c(2020:2040),
            uc=(input$utilization/100)*costs$Screening[1]+
               (input$utilization/100)*(input$prev1/100)*(input$treated1/100)*costs$Clinical_review[1]+
               (input$utilization/100)*(input$prev1/100)*(input$aware1/100)*costs$Diagnosis[1],
            meds=(input$utilization/100)*(input$prev1/100)*(input$treated1/100)*costs$Treatment[1]*(1+input$safetystock),
            salt=0,
            pop=epi$Pop[epi$intervention=="Baseline" & epi$year>=2020 & epi$year<=2040]/4,
            deaths=epi$Death[epi$intervention=="Baseline"& epi$year>=2020 & epi$year<=2040],
            cases=epi$Prevalence[epi$intervention=="Baseline"& epi$year>=2020 & epi$year<=2040],
            intervention="Baseline"
        )
        
        
        
        intdf<-data.frame(
            year=c(2020:2040),
            uc=(input$utilization/100)*costs$Screening[1]+
                (input$utilization/100)*(input$prev1/100)*(input$treated1/100)*costs$Clinical_review[1]+
                (input$utilization/100)*(input$prev1/100)*(input$aware1/100)*costs$Diagnosis[1],
            meds=(input$utilization/100)*(input$prev1/100)*(input$treated1/100)*costs$Treatment[1]*(1+input$safetystock),
            salt=0,
            pop=epi$Pop[epi$intervention=="Both" & epi$year>=2020 & epi$year<=2040]/4,
            deaths=epi$Death[epi$intervention=="Both"& epi$year>=2020 & epi$year<=2040],
            cases=epi$Prevalence[epi$intervention=="Both"& epi$year>=2020 & epi$year<=2040],
            intervention="Both"
        )
        
        
        if(input$targets=="Alt targets"){
            base<-input$aware1
            gap<-input$aware2-input$aware1
            adjustment<-(input$utilization/100)*(input$prev1/100)*(input$treated1/input$aware1)
            aware2<-input$aware2
        }
        
        else{
            base<-input$treated1
            gap<-input$treated1-input$treated_target
            adjustment<-(input$utilization/100)*(input$prev1/100)
            aware2<-input$aware_target
        }
        
        for (y in 2020:2040){
            
            if(y<input$drugcovyear[1]){
                intdf$uc[intdf$year==y]<-(input$utilization/100)*costs$Screening[1]+
                    (input$utilization/100)*(input$prev1/100)*(input$treated1/100)*costs$Clinical_review[1]+
                    (input$utilization/100)*(input$prev1/100)*(input$aware1/100)*costs$Diagnosis[1]
                
                intdf$meds[intdf$year==y]<-(input$utilization/100)*(input$prev1/100)*(input$treated1/100)*costs$Treatment[1]*(1+input$safetystock)

            }
            
            else if (y>=input$drugcovyear[1] & y<input$drugcovyear[2]){
                
                step<-(y-input$drugcovyear[1]+1)/(input$drugcovyear[2]-input$drugcovyear[1]+1)
                PIN_treat<-((base+gap*step)/100)*adjustment
                PIN_ddx<-((input$aware1+(aware2-input$aware1)*step)/100)*(input$utilization/100)*(input$prev1/100)
                
                intdf$uc[intdf$year==y]<-(input$utilization/100)*costs$Screening[1]+
                    PIN_treat*costs$Clinical_review[1]+
                    PIN_ddx*costs$Diagnosis[1]
                
                intdf$meds[intdf$year==y]<-PIN_treat*costs$Treatment[1]*(1+input$safetystock)
                    
            }
            
            else{
                
                PIN_treat2<-((base+gap)/100)*adjustment
                PIN_ddx2<-((input$aware1+(aware2-input$aware1))/100)*(input$utilization/100)*(input$prev1/100)
                
                intdf$uc[intdf$year==y]<-(input$utilization/100)*costs$Screening[1]+
                    PIN_treat2*costs$Clinical_review[1]+
                    PIN_ddx2*costs$Diagnosis[1]
                
                intdf$meds[intdf$year==y]<-PIN_treat*costs$Treatment[1]*(1+input$safetystock)
                    
            }
        }
        
        for (y in 2020:2040){
            if(y<input$saltyear[1]){
                intdf$salt[intdf$year==y]<-0
            }
            else if (y>=input$saltyear[1] & y<input$saltyear[2]){
                intdf$salt[intdf$year==y]<-saltcost()*exchange*(y-input$saltyear[1]+1)/(input$saltyear[2]-input$saltyear[1]+1)
            }
            else{
                intdf$salt[intdf$year==y]<-saltcost()*exchange
            }
        }
        
        
        df<-bind_rows(basedf, intdf)
        df$cost<-(df$uc+df$meds)*df$pop*(1+sum(markup$Markup[markup$Share!="Medication cost"]))+
                 (df$meds*df$pop*markup$Markup[markup$Share=="Medication cost"])+
                 df$salt*df$pop
        
        fwrite(df,"output/ceaplot_df.csv")
        
        deaths<-spread(df[,c("deaths", "intervention", "year")], intervention, deaths)
        deaths$mxdiff<-deaths$Baseline-deaths$Both
        costs<-spread(df[,c("cost", "intervention", "year")], intervention, cost)
        costs$costdiff<-costs$Both-costs$Baseline
        
        plot<-left_join(deaths[,c("year", "mxdiff")], costs[,c("year","costdiff")], by=c("year"))
        plot[plot<=0]<-0
        plot$icer<-plot$costdiff/plot$mxdiff
        plot[plot<=0]<-0
        
        plot<-plot%>%filter(year>2021)
        
        fwrite(basedf,"output/ceaplot_plot.csv")
        
        percap<-df%>%filter(intervention=="Both")%>%filter(year>2021)
        percap$cost2<-percap$cost/percap$pop
        
        ay <- list(
            tickfont = list(color = "red"),
            overlaying = "y",
            side = "right",
            title = "Cost per capita",
            showgrid=FALSE
        )
        
        fig<- plot_ly()
        fig<-fig%>%add_trace(x=plot$year, y=plot$icer, name="Cost effectiveness")
        fig<-fig%>%add_trace(x=percap$year, y=percap$cost2, name="Cost per capita", yaxis="y2")
        fig<-fig%>%
            layout(title=paste("Cost effectivesness ratio over time"),
                   titlefont=list(size=12),
                   font=list(size=11),
                   yaxis2 = ay,
                   xaxis=list(title="Year", showgrid=FALSE),
                   yaxis=list(title="Cost effectiveness (Cost/Life saved)", showgrid=FALSE))%>%
            config(modeBarButtonsToRemove = list("resetScale2d", "zoom2d", "autoScale2d", "toggleSpikelines","pan2d","zoomIn2d", "zoomOut2d",
                                                 "lasso2d", "selectbox", "newplotlylogo", "plotlylogo"))
        
        fig
    })
})
    
    output$stackedplot <- renderPlotly({
        
        input$plot
        
        isolate({
        newgraph2<-rv$data[c("age.group", "sex", "cause", "location", "intervention", "Death", "year")]%>%spread(intervention, Death)
        newgraph2$drugdiff<-newgraph2$Baseline-newgraph2$`Antihypertensive therapy`
        newgraph2$saltdiff<-newgraph2$Baseline-newgraph2$`Sodium reduction`
        newgraph2$bothdiff<-newgraph2$Baseline-newgraph2$Both
        
        newgraphit<-newgraph2%>%select(-c("Baseline", "Antihypertensive therapy", "Sodium reduction", "Both"))%>%
            gather(intervention, Death, -age.group, -sex, -cause, -location, -year)
        
        newgraphit$intervention[newgraphit$intervention=="bothdiff"]<-"Both"
        newgraphit$intervention[newgraphit$intervention=="drugdiff"]<-"Antihypertensive therapy"
        newgraphit$intervention[newgraphit$intervention=="saltdiff"]<-"Sodium reduction"
        
        #newgraphit$Prevalence[newgraphit$Prevalence<1]<-0
        newgraphit$Death[newgraphit$Death<1]<-0
        
            
            p2<-newgraphit%>%filter(location==input$country, sex=="Both", age.group=="All ages (20-95)", 
                                    cause=="All causes", year>2019)#%>%select(-c("Prevalence"))
            p2<-p2%>%spread(intervention, Death)
           
                    fig<-plot_ly(p2, x=~year, y=~`Antihypertensive therapy`, name="Antihypertensive<br>therapy", 
                                 type='scatter', mode='none', stackgroup='one', fillcolor='#3e7dcc',
                                 hoverinfo='text',
                                 text=~paste0("Year: ", p2$year, "\nLives saved: ", 
                                              so_formatter(signif(p2$`Antihypertensive therapy`, digits=3))))
                    fig<-fig%>%add_trace(y=~`Sodium reduction`, name="Sodium reduction", fillcolor='#464d77',
                                         hoverinfo='text',
                                         text=~paste0("Year: ", p2$year, "\nLives saved: ", 
                                                      so_formatter(signif(p2$`Sodium reduction`, digits=3))))%>%
                        layout(title=paste("Lives saved by intervention over time"),
                               titlefont=list(size=12),
                               font=list(size=11),
                               xaxis=list(title="", showgrid=FALSE),
                               yaxis=list(title="Lives saved", showgrid=FALSE),
                               legend=list(orientation="h"),
                               hovermode='compare')%>%
                        config(modeBarButtonsToRemove = list("resetScale2d", "zoom2d", "autoScale2d", "toggleSpikelines","pan2d","zoomIn2d", "zoomOut2d"))
                fig
    })
})

    # Export reactive data
    # Download handler to save the reactive data as an .RData file
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("exported_data_", Sys.Date(), ".RData")
      },
      content = function(file) {
        data_to_save <- input  # Obtain the reactive data
        save(data_to_save, file = file)  # Save it as an RData file
      }
    )
    
    
}#end of server

#run app
shinyApp(ui, server)

