#' Roxygen style comments
#' This structure supports later package documentation

library(shiny)
library(tidyverse)
library(tidycensus)
library(sf) #https://r-spatial.github.io/sf/
library(tigris) #https://cran.r-project.org/web/packages/tigris/tigris.pdf
options(tigris_use_cache = TRUE)
library(viridis)
library(shinycssloaders)
library(corrplot)
library(recipes)
library(nortest)
library(leaps)
library(shinydashboard)

leoka2<- readRDS("../data-raw/leoka2.ori.RDS")
leoka1<- readRDS("../data-raw/leoka1.ori.RDS")
leo2_assign<- readRDS("../data/leo2.assign.RDS")
leo2_weapon<- readRDS("../data/leo2.weapon.RDS")
leo2_other<- readRDS("../data/leo2.other.RDS")
leo2_activity<- readRDS("../data/leo2.activity.RDS")
leo1_injury<- readRDS("../data/leo1.injury.RDS")

# Data for regression tab
datacounty <- leo2_other %>%
  arrange(COUNTY_NAME) %>%
  na.omit() %>%
  select(-STATE, -REGION_NAME, -POPULATION_GROUP_DESC, -GEOID, -DATA_YEAR, -Hawaiian_pop,
         -COUNTY_NAME, -total_county_cleared, -others_race) 

datacounty2 <- leo2_other %>%
  arrange(COUNTY_NAME) %>%
  na.omit() %>%
  select(-REGION_NAME, -POPULATION_GROUP_DESC, -GEOID, -Hawaiian_pop,
         -total_county_cleared, -others_race)

response <- datacounty %>%
  select(total_county, median_income, unemployee_pop, get_Bachelor)

# Data for Anova tab
regionaov <- leo2_other %>%
  select(REGION_NAME, total_county) %>%
  rename(Region = REGION_NAME, Assaults = total_county)

weaponaov <- leo2_weapon %>%
  select(WEAPON, total_per_type) %>%
  rename(Weapon = WEAPON,  Total = total_per_type)

activityaov <- leo2_activity %>%
  select(ACTIVITY_NAME, total_per_type) %>%
  rename(Activity = ACTIVITY_NAME, Total = total_per_type)


# For tab state, county use
sta_selection <- c("State", "ACTIVITY_NAME", "ASSIGNMENT", "WEAPON","INJURY","county_pop","median_income","unemployee_pop","get_Bachelor")
cou_selection <- c("County","ACTIVITY_NAME", "ASSIGNMENT", "WEAPON","INJURY","county_pop","median_income","unemployee_pop","get_Bachelor")

ui <- fluidPage(theme = bslib::bs_theme(version = 5, bootswatch = "united"),
                titlePanel(h1(strong("Analysis of Assaults on Law Enforcement Officers"))),
                tabsetPanel(type="pills",selected="Introduction",
                            
                            # Tab Intro
                            tabPanel("Introduction", icon = icon("check-square"),
                                     h3(strong("Description")),
                                     p(style="text-align: justify; font-size = 25px",
                                       "The objective of this app is to provide a tool to analyze the number of 
                                       assaults against law enforcement officers in the United States. We aim to 
                                       provide useful insights about the total number of assaults, identify 
                                       significant changes, and provide comparisons for the past decade (2011 to 2020) by implementing
                                       a variety of approaches for data analysis that include graphical representation
                                       of the data, mapping, and statistical analysis."),
                                     h3(strong("Use Case")),
                                     p(style="text-align: justify; font-size = 25px",
                                       "This app supports exploratory data analysis of assaults against law enforcement officers in the United States. 
                                       It is targeted to people interested on this matter, either for curiosity or for professional reasons.
                                       The main users for the app are:"),
                                     p(style="text-align: justify; font-size = 25px",
                                      "-> First type include private security personnel and security agencies
                                       with presence in violent locations. Identifying violent locations will enable them to allocate resources for crime prevention."),
                                     p(style="text-align: justify; font-size = 25px", 
                                       "-> The second type includes police agencies. This app provides sufficient information to identify
                                       historical trends of things like activity performed, weapon usage, and injury type, in addition to statistical evidence
                                       to identify which demographic characteristics can potentially lead to an increase or decrease of total assaults. We consider that this app can be used
                                       as an evaluation tool for comparison purposes, as well as a method that can aid with decision making in terms of personnel and resources allocation given a 
                                       variety of factors that include activity frequency, type of weapon, and location."),
                                     p(style="text-align: justify; font-size = 25px",
                                       "Lastly, this app could also be useful as a baseline for future analysis as new data becomes available."),
                                     h6(strong("Questions of Interest")),
                                     p(style="text-align: justify; font-size = 25px",
                                       "1. Nationally, what states have the highest amount of assaults in 2011? Have they reduced their numbers as of 2020?"),
                                     p(style="text-align: justify; font-size = 25px",
                                       "2. From 2011 to 2020, what is the relationship between each type of weapon and the conditions based on DC, Baltimore, and California?"),
                                     p(style="text-align: justify; font-size = 25px",
                                       "3. At a county level, what is the impact of median income, unemployment, and education level on the total amount of cases?"),
                                     h3(strong("Data")),
                                     p(style="text-align: justify; font-size = 25px",
                                       "The data used in this app comes from a variety of sources. Our main source comes from the",
                                       a(href = "https://crime-data-explorer.app.cloud.gov/pages/downloads", "FBI Crime Data Explorer."),
                                       "The FBI publishes the data each year and it is obtained through the 
                                       Uniform Crime Reporting Programs Law Enforcement Officers Killed and Assaulted (LEOKA) data collection.
                                       The main variables include: ", em("year, state, county, agency_name, type of weapon, assignments of officers, and activity of officers."), 
                                       "This data set contains information for the years 1995 to 2020, however, we extracted only the information from 
                                       the last decade, 2011 to 2020, to use it in the app."),
                                     p(style="text-align: justify; font-size = 25px",
                                       "The second data source was the ", 
                                       a(href = "https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-state-detail.html",
                                         "United States Census Bureau."), "Demographic data by county and state was integrated with the first data set
                                       in order to facilitate data exploration and analysis. Selected variables include: ", 
                                       em("median income, population, unemployment, ethnicity(Asian, White, Black, Indian), and education level(population with at least bachelor's degree)."))
                                     
                            ),
                            
                            # tab State
                            tabPanel("State", icon = icon("flag-usa"), 
                                     sidebarLayout(
                                       # left
                                       sidebarPanel(width=3,
                                                    selectInput(
                                                      inputId = "sta_state",
                                                      label = "Select a state:",
                                                      choices = c("ALL", levels(leo2_other$STATE)),
                                                      size = 10,selectize = FALSE,
                                                      selected = "ALL"
                                                    ),
                                                    checkboxGroupInput("sta_year"," Select a year: ",
                                                                       levels(leo2_other$DATA_YEAR),
                                                                       "2020"),
                                                    selectInput("sta_variable", "Select a variable:", choices=sta_selection)
                                       ),
                                       # right
                                       mainPanel(width=8,
                                                 fluidRow(
                                                   column(10,
                                                          conditionalPanel("input.sta_select=='Plot'",
                                                                           withSpinner(plotOutput("sta_plot"))),
                                                          conditionalPanel("input.sta_select=='Statistics'",
                                                                           DT::dataTableOutput("sta_table")),
                                                          conditionalPanel("input.sta_select=='Donut'",
                                                                           withSpinner(plotOutput("sta_donut")))
                                                   ),
                                                   column(2,
                                                          radioButtons(inputId = "sta_select",
                                                                       label=NULL,
                                                                       choices=c("Plot","Donut","Statistics"),
                                                                       selected = "Plot")
                                                   )
                                                 )
                                       )
                                     )
                            ),
                            
                            # tab county
                            tabPanel("County", icon = icon("flag-usa"),
                                     sidebarLayout(
                                       # tab county left
                                       sidebarPanel(width=3,
                                                    selectInput(
                                                      inputId = "cou_state",
                                                      label = "Select a state:",
                                                      choices = c("ALL", levels(leo2_other$STATE)),
                                                      size = 10,selectize = FALSE,
                                                      selected = "California"
                                                    ),
                                                    selectInput(
                                                      inputId = "cou_county",
                                                      label = "Select a county:",
                                                      c("")
                                                    ),
                                                    checkboxGroupInput("cou_year"," Select a year: ",
                                                                       levels(leo2_other$DATA_YEAR),
                                                                       "2020"),
                                                    selectInput("cou_variable", "Select a variable:", choices=cou_selection,selected = "ACTIVITY_NAME")
                                       ),
                                       # tab county right
                                       mainPanel(width=8,
                                                 fluidRow(
                                                   column(10,
                                                          conditionalPanel("input.cou_select=='Plot'",
                                                                           withSpinner(plotOutput("cou_plot"))),
                                                          conditionalPanel("input.cou_select=='Statistics'",
                                                                           DT::dataTableOutput("cou_table")),
                                                          conditionalPanel("input.cou_select=='Donut'",
                                                                           withSpinner(plotOutput("cou_donut")))
                                                   ),
                                                   column(2,
                                                          radioButtons(inputId = "cou_select",
                                                                       label=NULL,
                                                                       choices=c("Plot","Donut","Statistics"),
                                                                       selected = "Plot"))
                                                 )
                                       )
                                     )
                            ),
                            
                            tabPanel("State Map", icon = icon("map-marked-alt"), 
                                     sidebarLayout(
                                       #left
                                       sidebarPanel(width=3,
                                         selectInput(
                                           inputId = "sta_state1",
                                           label = "Select a state:",
                                           choices = leo2_weapon %>%
                                             select(STATE) %>%
                                             unique(),
                                           size = 10,selectize = FALSE,
                                           selected = "California"
                                         ),
                                         checkboxGroupInput("sta_year1"," Select a year: ",
                                                            levels(leo2_weapon$DATA_YEAR),
                                                            "2020"),
                                         radioButtons(inputId = "Condition1",
                                                      label=HTML('<FONT color="Orange"><FONT size="5pt">Choose</FONT></FONT><br> <b>Condition</b>'),
                                                      choices= list("medain_income" =1, "total_population"=2, "unemployee_population"=3, "Bachelor_population"=4, "Race"=5),
                                                      selected = 1)
                                       ),
                                       # right
                                       mainPanel(
                                         plotOutput("sta_plot1"),
                                         plotOutput("Condition1"),
                                         DT::dataTableOutput("EY")
                                       )
                                     )),
                            tabPanel("County Map", icon = icon("map-marked-alt"),
                                     sidebarLayout(
                                       #left
                                       sidebarPanel(width=3,
                                         selectInput(
                                           inputId = "sta_state2",
                                           label = "Select a state:",
                                           choices = leo2_weapon %>%
                                             select(STATE) %>%
                                             unique(),
                                           size = 10,selectize = FALSE,
                                           selected = "California"
                                         ),
                                         selectInput(
                                           inputId = "sta_county",
                                           label = "Select a county:",
                                           choices = NULL,
                                           size = 10,selectize = FALSE
                                         ),
                                         checkboxGroupInput("sta_year2"," Select a year: ",
                                                            levels(leo2_weapon$DATA_YEAR),
                                                            "2020"),
                                         radioButtons(inputId = "Condition2",
                                                      label=HTML('<FONT color="Orange"><FONT size="5pt">Choose</FONT></FONT><br> <b>Condition</b>'),
                                                      choices= list("medain_income" =1, "total_population"=2, "unemployee_population"=3, "Bachelor_population"=4, "Race"=5),
                                                      selected = 1)
                                       ),
                                       # right
                                       mainPanel(
                                         plotOutput("sta_plot3"),
                                         plotOutput("Condition2"),
                                         DT::dataTableOutput("EY1")
                                       )
                                     )),
                            tabPanel("Regression Analysis - County Level", icon = icon("registered"),
                                     sidebarLayout(
                                       sidebarPanel(width=3,
                                                    selectInput("y", label = h4("Select Y Variable:"),
                                                                choices = names(response), selected = "total_county"),
                                                    solidHeader = TRUE,
                                                    status = "primary",            
                                                    checkboxGroupInput("x", label = h4("Select X Variables:"),
                                                                       choices = names(datacounty), selected = "median_income"),
                                                    varSelectInput("bi_variable_x","X Variable - Bivariate Tab:",datacounty,"median_income"),
                                                    varSelectInput("bi_variable_y","Y Variable - Bivariate Tab:",datacounty,"total_county"),
                                                    solidHeader = TRUE,
                                                    status = "primary"),
                                       mainPanel(tabsetPanel(type = "tabs",
                                                             tabPanel("Model Summary",icon = icon("calculator"),
                                                                      box(withSpinner(verbatimTextOutput("Model")), width = 12, title = h4("Regression Summary")
                                                                      ),
                                                                      box(withSpinner(verbatimTextOutput("Summ")), width = 12, title = h4("Descriptive Statistics")
                                                                      )
                                                             ),
                                                             tabPanel("Correlation", icon = icon("copyright"),
                                                                      box(withSpinner(plotOutput("Corr")),width = 12, title = h4("Correlation - by Selected Variables")
                                                                      )
                                                             ),
                                                             tabPanel("Bivariate", icon = icon("chart-area"),
                                                                      box(withSpinner(plotOutput("bi_plot")),width=12,title=h4("Bivariate Plot - by Selected variables")
                                                                      )
                                                             ),
                                                             tabPanel("Model Fit",
                                                                      icon = icon("clipboard-check"),
                                                                      box(withSpinner(plotOutput("residualPlots")), width = 12, title = h4("Diagnostic Plots")
                                                                      ),
                                                                      box(withSpinner(verbatimTextOutput("normality")), width = 12, title = h5("Normality Test")
                                                                      )
                                                             ), 
                                                             #column(10, box(h5("Equal Variance Test"), withSpinner(verbatimTextOutput("variances")))))),
                                                             tabPanel("Data",icon = icon("table"), 
                                                                      box(withSpinner(dataTableOutput('tbl')), width = 12, title = h4("Data for all Counties")
                                                                      )
                                                             )
                                       )       
                                       
                                       )
                                     )),
                            tabPanel("Anova - County Level", icon = icon("adn"),
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Region",icon = icon("map-marked-alt"),
                                                          p(style="text-align: justify; font-size = 25px", strong("Regions include:"), em("West, South, Northeast, Midwest.")),
                                                          p(style="text-align: justify; font-size = 25px"),
                                                          p(style="text-align: justify; font-size = 25px", strong("H0:"), em("There are no mean differences in the amount of incidents across regions for the 2011 - 2020 period.")),
                                                          p(style="text-align: justify; font-size = 25px", strong("Ha:"), em("There are mean differences in the amount of incidents across regions for the 2011 - 2020 period.")),
                                                          box(withSpinner(verbatimTextOutput("regionaov")), width = 12, title = h4("Anova Summary")),
                                                          box(withSpinner(plotOutput("regionbox")), width = 12, title = h4("Summary Plots"))
                                                 ),
                                                 tabPanel("Weapon",icon = icon("exclamation-triangle"),
                                                          p(style="text-align: justify; font-size = 25px", strong("Weapons include:"), em("Firearm, Knife, Hands or Feet, Other.")),
                                                          p(style="text-align: justify; font-size = 25px"),
                                                          p(style="text-align: justify; font-size = 25px", strong("H0:"), em("There are no mean differences in the amount of incidents reported for each weapon type during the 2011 - 2020 period.")),
                                                          p(style="text-align: justify; font-size = 25px", strong("Ha:"), em("There are mean differences in the amount of incidents reported for each weapon type during the 2011 - 2020 period.")),
                                                          box(withSpinner(verbatimTextOutput("weaponaov")), width = 12, title = h4("Anova Summary")),
                                                          box(withSpinner(plotOutput("weaponbox")), width = 12, title = h4("Summary Plots"))
                                                 ),
                                                 tabPanel("Activity",icon = icon("dot-circle"),
                                                          p(style="text-align: justify; font-size = 25px", strong("Activities include:"), em("Traffic Pursuits and Stops                        
                                                           Responding to Disturbance Calls,                      
                                                           Burglaries in Progress or Pursuing Burglary Suspects, 
                                                           Robberies in Progress or Pursuing Robbery Suspects,  
                                                           Attempting Other Arrests,                          
                                                           Civil Disorder,                                       
                                                           Handling, Transporting, Custody of Prisoners,         
                                                           Investigating Suspicious Persons or Circumstances,    
                                                           Ambush - No Warning,                                  
                                                           Handling Persons with Mental Illness,                 
                                                           All Other.")),
                                                          p(style="text-align: justify; font-size = 25px"),
                                                          p(style="text-align: justify; font-size = 25px", strong("H0:"), em("There are no mean differences in the amount of incidents reported for each activity type during the 2011 - 2020 period.")),
                                                          p(style="text-align: justify; font-size = 25px", strong("Ha:"), em("There are mean differences in the amount of incidents reported for each activity type during the 2011 - 2020 period.")),
                                                          box(withSpinner(verbatimTextOutput("activity")), width = 12, title = h4("Anova Summary")),
                                                          box(withSpinner(plotOutput("activitybox")), width = 12, title = h4("Summary Plots"))
                                                 )
                                     )),
                            tabPanel("Spreadsheet", icon = icon("table"),
                                     sidebarLayout(
                                       radioButtons(inputId = "data",
                                                    label=HTML('<FONT color="Orange"><FONT size="5pt">Choose</FONT></FONT><br> <b>Data</b>'),
                                                    choices= list("Original1" = 1, "Original2" = 2, "Injury" = 3, "Assign" = 4, "Weapon" = 5, "Other" = 6, "Acativity" = 7),
                                                    selected = 1),
                                       mainPanel(dataTableOutput("data"))
                                      ))      
                           
                            )
)

server <- function(input, output, session) {

  
  # For tab state use 
  yw1<- reactive({
    if(!!input$sta_variable=="ASSIGNMENT"){ta<- leo2_assign}
    else if(!!input$sta_variable=="WEAPON"){ta<- leo2_weapon}
    else if(!!input$sta_variable=="ACTIVITY_NAME"){ta<- leo2_activity}
    else if(!!input$sta_variable=="INJURY"){ta<- leo1_injury}
    else {ta<- leo2_other}
    
    ta %>%
      filter(DATA_YEAR %in% input$sta_year) %>%
      filter(STATE%in%input$sta_state)
  })
  
  p1<- theme_bw()+theme(axis.text.x=element_text(angle=45,hjust=1))
  p_th<- theme_bw()
  
  yw3<- reactive({
    leo2_other %>%
      filter(DATA_YEAR %in% input$sta_year) %>%
      filter(STATE%in%input$sta_state) %>% 
      summarize(
        others_race = sum(others_race,na.rm=TRUE),
        Asian_pop = sum(Asian_pop,na.rm=TRUE),
        Black_pop = sum(Black_pop,na.rm=TRUE),
        White_pop = sum(White_pop,na.rm=TRUE),
        Indian_pop = sum(Indian_pop,na.rm=TRUE),
        Hawaiian_pop = sum(Hawaiian_pop,na.rm=TRUE)
      ) %>% 
      rowwise() %>% 
      summarize(count=sum(others_race+Asian_pop+Black_pop+White_pop+Indian_pop+Hawaiian_pop))
    
  })
  
  
  # tab state plot&statistic
  observe({
    if(!!input$sta_select=="Plot"){
      # tab state plot1
      output$sta_plot<- renderPlot({
        
        validate(
          need(input$sta_year,"Please select a year~")
        )
        
        if(!!input$sta_state=="ALL"){
          validate(
            need(!!input$sta_variable=="State","Please select a state~")
          )
          leo2_other %>%
            filter(DATA_YEAR %in% input$sta_year) %>%
            group_by(DATA_YEAR,STATE) %>%
            summarize(STATE,count=sum(total_county)) %>%
            unique() %>%
            ggplot(aes(x=STATE,y=count))+
            geom_col()+
            facet_wrap(.~DATA_YEAR,ncol=1)+p1
        }
        else{
          if(!!input$sta_variable=="State"){
            yw1() %>% 
              group_by(DATA_YEAR) %>% 
              mutate(total=sum(total_county)) %>% 
              select(DATA_YEAR,total) %>% 
              unique() %>% 
              ggplot(aes(x=DATA_YEAR,y=total))+
              geom_col()+
              theme_bw()
          }else if(!!input$sta_variable %in% c("ACTIVITY_NAME","ASSIGNMENT","WEAPON","INJURY")){
            yw1() %>%
              group_by(DATA_YEAR,!!sym(input$sta_variable)) %>%
              summarize(count=sum(total_per_type)) %>%
              unique() %>%
              ggplot(aes(x=!!sym(input$sta_variable),y=count,fill=!!sym(input$sta_variable)))+
              geom_col(show.legend = FALSE)+
              facet_wrap(.~DATA_YEAR)+p1
          }else{
            yw1() %>%
              ggplot(aes(x=!!sym(input$sta_variable)))+
              geom_histogram()+
              facet_wrap(.~DATA_YEAR)+
              theme_bw()
          }
        }
      })
    }
    else if(!!input$sta_select=="Statistics"){
      
      output$sta_table <- DT::renderDataTable({
        if(!!input$sta_state=="ALL"){
          validate(
            need(!!input$sta_variable=="State","Need to select a state~")
          )
          leo2_other %>%
            filter(DATA_YEAR %in% input$sta_year) %>%
            group_by(DATA_YEAR,STATE) %>%
            summarize(STATE,count=sum(total_county)) %>%
            unique()
        }else{
          if(!!input$sta_variable %in% c("ACTIVITY_NAME","ASSIGNMENT","WEAPON","INJURY")){
            yw1() %>% 
              group_by(DATA_YEAR, !!sym(input$sta_variable)) %>% 
              summarize(total=sum(total_per_type))
          }else if(!!input$sta_variable=="State"){
            yw1() %>% 
              group_by(DATA_YEAR) %>% 
              mutate(total=sum(total_county)) %>% 
              select(DATA_YEAR,total) %>% 
              unique()
          }else{
            yw1() %>% 
              group_by(DATA_YEAR) %>% 
              summarize(Num_county=n(),
                        Meidan=median(median_income,na.rm=TRUE),
                        Mean=mean(median_income,na.rm=TRUE),
                        Minimun=min(median_income,na.rm=TRUE),
                        Maximun=max(median_income,na.rm=TRUE)
              )
          }
        }
        
      }, rownames = F, options = list(
        #dom = "ti",
        pageLength = 10,
        #Styling to make the table look nice
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#ec4404', 'color': '#fff'});",
          "}"
        ),
        scrollCollapse = TRUE
      ))
    }
    else if(!!input$sta_select=="Donut"){
      # donut of state tab
      output$sta_donut<- renderPlot({
        
        validate(
          need(input$sta_year,"Please select a year~"),
          need(!!input$sta_state!="ALL","Please select a state~"),
          need(yw3() $count!=0,"No related data of this state~")
        )
        
        leo2_other %>%
          filter(DATA_YEAR %in% input$sta_year) %>%
          filter(STATE%in%input$sta_state) %>% 
          select(STATE,others_race:Hawaiian_pop) %>% 
          unique() %>% 
          summarize(
            others_race = sum(others_race,na.rm=TRUE),
            Asian_pop = sum(Asian_pop,na.rm=TRUE),
            Black_pop = sum(Black_pop,na.rm=TRUE),
            White_pop = sum(White_pop,na.rm=TRUE),
            Indian_pop = sum(Indian_pop,na.rm=TRUE),
            Hawaiian_pop = sum(Hawaiian_pop,na.rm=TRUE)
          ) %>%
          pivot_longer(
            cols = (others_race:Hawaiian_pop),
            names_to = "race",
            values_to = "pop"
          ) %>%
          mutate(
            perc = pop / sum(pop),
            ymax = cumsum(perc),
            ymin = ifelse(is.na(lag(ymax)), 0, lag(ymax))
          ) %>%
          ggplot() +
          geom_rect(aes(ymin = ymin, ymax = ymax, fill = race, xmin = 2, xmax = 4)) +
          coord_polar(theta = "y", start = 1) +
          theme_void() +
          xlim(c(-1, 4)) +
          geom_text(aes(x = 3.5, y = perc, label = paste0(race, "\n", round(perc * 100, 1), "%")),
                    position = position_stack(vjust = 0.5), size = 2.5
          )
      })
    }
  })
  
  # tab county selection changes by states
  observeEvent(input$cou_state,
               {
                 x<- input$cou_state
                 updateSelectInput(session,"cou_county",
                                   choices=unique(leo2_other[leo2_other$STATE==input$cou_state,]$COUNTY_NAME))
               })
  
  # For tab county use
  yw2<- reactive({
    if(!!input$cou_variable=="ASSIGNMENT"){ta2<- leo2_assign}
    else if(!!input$cou_variable=="WEAPON"){ta2<- leo2_weapon}
    else if(!!input$cou_variable=="ACTIVITY_NAME"){ta2<- leo2_activity}
    else if(!!input$cou_variable=="INJURY"){ta2<- leo1_injury}
    else{ta2<- leo2_other}
    
    ta2 %>%
      filter(DATA_YEAR %in% input$cou_year) %>%
      filter(STATE %in% input$cou_state) %>% 
      filter(COUNTY_NAME %in% input$cou_county)
  })
  
  yw4<- reactive({
    leo2_other %>%
      filter(DATA_YEAR %in% input$cou_year) %>%
      filter(STATE %in% input$cou_state) %>% 
      filter(COUNTY_NAME %in% input$cou_county) %>%  
      summarize(
        others_race = sum(others_race,na.rm=TRUE),
        Asian_pop = sum(Asian_pop,na.rm=TRUE),
        Black_pop = sum(Black_pop,na.rm=TRUE),
        White_pop = sum(White_pop,na.rm=TRUE),
        Indian_pop = sum(Indian_pop,na.rm=TRUE),
        Hawaiian_pop = sum(Hawaiian_pop,na.rm=TRUE)
      ) %>% 
      rowwise() %>% 
      summarize(count=sum(others_race+Asian_pop+Black_pop+White_pop+Indian_pop+Hawaiian_pop))
    
  })
  
  #tab county plot
  observe({
    if(!!input$cou_select=="Plot"){
      output$cou_plot<- renderPlot({
        validate(
          need(!!input$cou_state!="ALL","Please select a state~"),
          need(input$cou_year,"Please select a year~"),
          need(input$cou_variable,"Please select a variable~")
        )
        if(!!input$cou_variable=="County"){
          yw2() %>% 
            select(DATA_YEAR, STATE, COUNTY_NAME,total_county) %>% 
            unique() %>% 
            ggplot(aes(x=DATA_YEAR,y=total_county))+
            geom_col()+
            theme_bw() 
        }
        else if(!!input$cou_variable %in% c("ACTIVITY_NAME","ASSIGNMENT","WEAPON","INJURY")){
          yw2() %>%
            group_by(DATA_YEAR,!!sym(input$cou_variable)) %>% 
            summarize(count=sum(total_per_type)) %>% 
            ggplot(aes(x=!!sym(input$cou_variable),y=count,fill=!!sym(input$cou_variable)))+
            geom_col(show.legend = FALSE)+
            facet_wrap(.~DATA_YEAR)+p1
        }else{
          yw2() %>% 
            ggplot(aes(x=DATA_YEAR,y=!!sym(input$cou_variable)))+
            geom_col()+p_th
        }
      })
    }
    else if(!!input$cou_select=="Statistics"){
      output$cou_table<- DT::renderDataTable({
        validate(
          need(!!input$cou_state!="ALL","Please select a state~"),
          need(input$cou_year,"Please select a year~"),
          need(input$cou_variable,"Please select a variable~")
        )
        
        if(!!input$cou_variable=="County"){
          yw2() %>% 
            select(DATA_YEAR, STATE, COUNTY_NAME,total_county) %>% 
            unique()
        }
        else if(!!input$cou_variable %in% c("ACTIVITY_NAME","ASSIGNMENT","WEAPON","INJURY")){
          yw2() %>%
            group_by(DATA_YEAR,!!sym(input$cou_variable)) %>% 
            summarize(count=sum(total_per_type))
        }else{
          yw2() %>% 
            summarize(DATA_YEAR,!!sym(input$cou_variable)) %>% 
            unique()
        }
      }, rownames = F, options = list(
        #dom = "ti",
        pageLength = 10,
        #Styling to make the table look nice
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#ec4404', 'color': '#fff'});",
          "}"
        ),
        scrollCollapse = TRUE
      ))
    }
    else if(!!input$cou_select=="Donut"){
      # donut of county tab
      output$cou_donut<- renderPlot({
        validate(
          need(input$cou_year,"Please select a year~"),
          need(!!input$cou_state!="ALL","Please select a state~"),
          need(yw4() $count!=0,"No related data of this county~")
        )
        
        leo2_other %>%
          filter(DATA_YEAR %in% input$cou_year) %>%
          filter(STATE %in% input$cou_state) %>% 
          filter(COUNTY_NAME %in% input$cou_county) %>% 
          select(STATE,others_race:Hawaiian_pop) %>% 
          unique() %>% 
          summarize(
            others_race = sum(others_race,na.rm=TRUE),
            Asian_pop = sum(Asian_pop,na.rm=TRUE),
            Black_pop = sum(Black_pop,na.rm=TRUE),
            White_pop = sum(White_pop,na.rm=TRUE),
            Indian_pop = sum(Indian_pop,na.rm=TRUE),
            Hawaiian_pop = sum(Hawaiian_pop,na.rm=TRUE)
          ) %>%
          pivot_longer(
            cols = (others_race:Hawaiian_pop),
            names_to = "race",
            values_to = "pop"
          ) %>%
          mutate(
            perc = pop / sum(pop),
            ymax = cumsum(perc),
            ymin = ifelse(is.na(lag(ymax)), 0, lag(ymax))
          ) %>%
          ggplot() +
          geom_rect(aes(ymin = ymin, ymax = ymax, fill = race, xmin = 2, xmax = 4)) +
          coord_polar(theta = "y", start = 1) +
          theme_void() +
          xlim(c(-1, 4)) +
          geom_text(aes(x = 3.5, y = perc, label = paste0(race, "\n", round(perc * 100, 1), "%")),
                    position = position_stack(vjust = 0.5), size = 2.5
          )
      })
    }
  })
  
  # tab map plot
  
  x4 <- reactive({
    leo2_weapon %>%
      filter(DATA_YEAR %in% input$sta_year1) %>%
      filter(STATE%in%input$sta_state1)
  })
  
  x6 <- reactive({
    leo2_weapon %>%
      filter(DATA_YEAR %in% input$sta_year1) %>%
      filter(STATE%in%input$sta_state1) %>% 
      group_by(WEAPON) %>% 
      slice_max(total_per_type, n = 1) %>% 
      select(DATA_YEAR, STATE, COUNTY_NAME, WEAPON, total_per_type, median_income, county_pop, unemployee_pop, get_Bachelor)
  })
  
  output$sta_plot1<- renderPlot({
    
    validate(
      need(input$sta_year1,"Please select a year~")
    )
    
    x4() %>%
      ggplot(aes(fill = total_per_type)) +
      geom_sf(aes(geometry = geometry), color = NA) +
      scale_fill_viridis_c() +
      facet_wrap(~ WEAPON)
  })
  
  output$Condition1<- renderPlot({
    
    validate(
      need(input$sta_year1,"Please select a year~")
    )
    
    if(input$Condition1 == 1) {
      x4() %>%
        ggplot(aes(fill = median_income)) +
        geom_sf(aes(geometry = geometry), color = NA) +
        scale_fill_viridis_c() +
        ggtitle("Median Income")
    } else if (input$Condition1 == 2) {
      x4() %>%
        ggplot(aes(fill = county_pop)) +
        geom_sf(aes(geometry = geometry), color = NA) +
        scale_fill_viridis_c() +
        ggtitle("Total Population")
    } else if (input$Condition1 == 3) {
      x4() %>%
        ggplot(aes(fill = unemployee_pop)) +
        geom_sf(aes(geometry = geometry), color = NA) +
        scale_fill_viridis_c() +
        ggtitle("Unemployee Population")
    } else if (input$Condition1 == 4) {
      x4() %>%
        ggplot(aes(fill = get_Bachelor)) +
        geom_sf(aes(geometry = geometry), color = NA) +
        scale_fill_viridis_c() +
        ggtitle("Number of Bachelor degree")
    } else if (input$Condition1 == 5) {
      x4() %>% 
        select(STATE,others_race:Hawaiian_pop) %>% 
        unique() %>% 
        summarize(
          others_race = sum(others_race,na.rm=TRUE),
          Asian_pop = sum(Asian_pop,na.rm=TRUE),
          Black_pop = sum(Black_pop,na.rm=TRUE),
          White_pop = sum(White_pop,na.rm=TRUE),
          Indian_pop = sum(Indian_pop,na.rm=TRUE),
          Hawaiian_pop = sum(Hawaiian_pop,na.rm=TRUE)
        ) %>%
        pivot_longer(
          cols = (others_race:Hawaiian_pop),
          names_to = "race",
          values_to = "pop"
        ) %>% 
        ggplot(aes(x = race, y = pop, fill = race)) +
        geom_col()
    } else {
      validate(
        need(!!input$total_population | !!input$medain_income |
               !!input$unemployee_population | !!input$Bachelor_population,
             "Please select a condition")
      )
    }
    
  })
  
  output$EY <- DT::renderDataTable({
    x6()
  }, rownames = F, options = list(
    #dom = "ti",
    pageLength = 10,
    #Styling to make the table look nice
    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#ec4404', 'color': '#fff'});",
      "}"
    ),
    scrollCollapse = TRUE
  ))
  
  # tab county plot
  
  observe({
    print(input$sta_state2)
    x <- leo2_weapon %>%
      filter(STATE == input$sta_state2) %>% 
      select(COUNTY_NAME)
    updateSelectInput(session, "sta_county", "Select a county:",choices = unique(x), selected = "ALAMEDA")
  })
  
  x5 <- reactive({
    leo2_weapon %>%
      filter(DATA_YEAR %in% input$sta_year2) %>%
      filter(STATE %in% input$sta_state2) %>% 
      filter(COUNTY_NAME %in% input$sta_county)
  })
  
  x7 <- reactive({
    leo2_weapon %>%
      filter(DATA_YEAR %in% input$sta_year2) %>%
      filter(STATE%in%input$sta_state2) %>% 
      filter(COUNTY_NAME %in% input$sta_county) %>% 
      select(DATA_YEAR, STATE, COUNTY_NAME, WEAPON, total_per_type, median_income, county_pop, unemployee_pop, get_Bachelor)
  })
  
  output$sta_plot3<- renderPlot({
    validate(
      need(input$sta_year2,"Please select a year~"),
      need(nrow(x5()) != 0, "No data in this county")
    )
    
    x5() %>% 
      ggplot(aes(fill = total_per_type)) +
      geom_sf(aes(geometry = geometry), color = NA) +
      scale_fill_viridis_c() +
      facet_wrap(~ WEAPON)
    
  })
  
  output$Condition2<- renderPlot({
    
    validate(
      need(input$sta_year2,"Please select a year~"),
      need(nrow(x5()) != 0, "No data in this county")
    )
    
    if(input$Condition2 == 1) {
      x5() %>%
        ggplot(aes(fill = median_income)) +
        geom_sf(aes(geometry = geometry), color = NA) +
        scale_fill_viridis_c() +
        ggtitle("Median Income")
    } else if (input$Condition2 == 2) {
      x5() %>%
        ggplot(aes(fill = county_pop)) +
        geom_sf(aes(geometry = geometry), color = NA) +
        scale_fill_viridis_c() +
        ggtitle("Total Population")
    } else if (input$Condition2 == 3) {
      x5() %>%
        ggplot(aes(fill = unemployee_pop)) +
        geom_sf(aes(geometry = geometry), color = NA) +
        scale_fill_viridis_c() +
        ggtitle("Unemployee Population")
    } else if (input$Condition2 == 4) {
      x5() %>%
        ggplot(aes(fill = get_Bachelor)) +
        geom_sf(aes(geometry = geometry), color = NA) +
        scale_fill_viridis_c() +
        ggtitle("Number of Bachelor degree")
    } else if (input$Condition2 == 5) {
      x5() %>% 
        select(STATE,others_race:Hawaiian_pop) %>% 
        unique() %>% 
        summarize(
          others_race = sum(others_race,na.rm=TRUE),
          Asian_pop = sum(Asian_pop,na.rm=TRUE),
          Black_pop = sum(Black_pop,na.rm=TRUE),
          White_pop = sum(White_pop,na.rm=TRUE),
          Indian_pop = sum(Indian_pop,na.rm=TRUE),
          Hawaiian_pop = sum(Hawaiian_pop,na.rm=TRUE)
        ) %>%
        pivot_longer(
          cols = (others_race:Hawaiian_pop),
          names_to = "race",
          values_to = "pop"
        ) %>% 
        ggplot(aes(x = race, y = pop, fill = race)) +
        geom_col()
    } else {
      validate(
        need(!!input$total_population | !!input$medain_income |
               !!input$unemployee_population | !!input$Bachelor_population,
             "Please select a condition")
      )
    }
    
  })
  
  output$EY1 <- DT::renderDataTable({
    x7()
  }, rownames = F, options = list(
    #dom = "ti",
    pageLength = 10,
    #Styling to make the table look nice
    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#ec4404', 'color': '#fff'});",
      "}"
    ),
    scrollCollapse = TRUE
  ))
  
  output$data <- renderDataTable({
    if (input$data == 1){
      leoka1
    }
    else if (input$data == 2) {
      leoka2
    }
    else if (input$data == 3) {
      leo1_injury
    }
    else if (input$data == 4) {
      leo2_assign
    }
    else if (input$data == 5) {
      leo2_weapon
    }
    else if (input$data == 6) {
      leo2_other
    }
    else if (input$data == 7) {
      leo2_activity
    }
  },
  options = list(pageLength = 20))
  
  # For tab Regression by County use
  inputcounty <- reactive({
    datacounty})
  
  # Model Summary
  # Regression Output
  countyreg <-reactive(datacounty %>%
                         recipe() %>%
                         update_role(!!!input$y,new_role = "outcome") %>%
                         update_role(!!!input$x,new_role = "predictor") %>% 
                         prep() %>%
                         formula()
  )
  
  lm_reg <- reactive(
    lm(countyreg(),data = inputcounty()))
  
  output$Model = renderPrint({summary(lm_reg())})
  
  # Descriptive Statistics
  
  sumstats<- reactive({
    datacounty %>% 
      select(!!input$x,!!input$y) -> sumdf
  })
  
  output$Summ <- renderPrint(summary(sumstats()))
  
  # Correlation Plot
  cornum<- reactive({
    datacounty %>% 
      select(!!input$x,!!input$y)->cordf
    round(cor(cordf),1)
  })
  
  output$Corr <-
    renderPlot(corrplot(
      cornum(),
      type = "lower",
      order = "hclust",
      method = "number"
    ))
  
  # Bivariate tab
  output$bi_plot<- renderPlot({
    validate(
      need(input$bi_variable_x, "Please Choose a Variable for X"),
      need(input$bi_variable_y,"Please Choose a Variable for Y")
    )
    
    datacounty %>% 
      ggplot(aes(x=!!input$bi_variable_x,!!input$bi_variable_y))+
      geom_point()+
      geom_smooth(method="lm",se=FALSE)+
      theme_bw()
  })
  
  # Model fit tab
  # Residual and QQ-Plot
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2))
    plot(lm_reg())
    par(mfrow = c(1, 1))
  })
  
  # Normality Test
  rescounty <- reactive(rstudent(lm_reg()))
  output$normality <- renderPrint({ad.test(rescounty())})
  
  # Data tab
  inputcounty2 <- reactive({
    datacounty2})
  output$tbl <- renderDataTable(inputcounty2(), options = list(pageLength = 5))
  
  # For ANOVA tab use
  # Anova by Region
  lmregion <- reactive(
    lm(Assaults ~ factor(Region),data = regionaov))
  
  output$regionaov <- renderPrint({anova(lmregion())})
  output$regionbox <- renderPlot({
    par(mfrow = c(2, 2))
    plot(lmregion())
    par(mfrow = c(1, 1))
  })
  
  #Anova by Weapon
  lmweapon <- reactive(
    lm(Total ~ factor(Weapon),data = weaponaov))
  
  output$weaponaov <- renderPrint({anova(lmweapon())})
  output$weaponbox <- renderPlot({
    par(mfrow = c(2, 2))
    plot(lmweapon())
    par(mfrow = c(1, 1))
  })
  
  #Anova by Activity
  lmactivity <- reactive(
    lm(Total ~ factor(Activity),data = activityaov))
  
  output$activity <- renderPrint({anova(lmactivity())})
  output$activitybox <- renderPlot({
    par(mfrow = c(2, 2))
    plot(lmactivity())
    par(mfrow = c(1, 1))
  })

  
}

shinyApp(ui, server)