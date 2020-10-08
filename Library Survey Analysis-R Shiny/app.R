# PACKAGES


library(rsconnect)
library(plogr)
library(sys)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(aggregation)
library(plotly)
#library(plyr)
library(reshape2)
library(HH)
#library(dplyr)
#library(cowplot)
#library(gridExtra)
library(ggthemes)
#library(stringr)
#library(ggplot2)
library(RColorBrewer)
library(shinyHeatmaply)
library(heatmaply)
library(dendextend)
library(ggalt)
#library(crosstalk)
library(FactoMineR)
library(Factoshiny)
# library(devtools)
library(ggbiplot)
library(factoextra)
library(corrplot)
library(gridExtra)
library(d3r)
library(sunburstR)
library(UpSetR)
#library(ggstatsplot)
library(ggpubr)
library(rstatix)
library(viridis)
library(hrbrthemes)


# Data Load
lib_data<- read_csv("data/Raw Data LSR.csv")
ItemCategories <- read_csv("data/ItemCategories.csv")


# UI
p00 = fluidRow(# Overview of Ratings for Factors by Respondents
    fluidRow(
        column(
            width = 12,
            box(
                title = 'OVERVIEW OF THE SMU LIBRARY SURVEY ANALYSIS',
                status = 'primary',
                width = 15,
                solidHeader = T,
                #plotOutput('plot_intro', height = 700),
                img(src='pic3.png', class="landscape",align = "fill",height='900px',width='1100px')
            )
        )
    )
)
p000 = fluidRow(# Overview of Navigation
    fluidRow(
        column(
            width = 12,
            box(
                title = 'OVERVIEW OF THE NAVIGATION PANEL',
                status = 'primary',
                width = 15,
                solidHeader = T,
                #plotOutput('plot_intro', height = 700),
                img(src='pic4.png', class="landscape",align = "fill",height='900px',width='1100px')
            )
        )
    )
)

p0 = fluidRow(# Overview of Ratings for Factors by Respondents
    fluidRow(
        column(
            width = 12,
            box(
                title = 'SETTINGS',
                status = 'primary',
                width = 2,
                solidHeader = T,
                radioButtons(
                    inputId = 'selectS0',
                    label = 'Study Area',
                    choices = c(
                        'All',
                        'Accountancy',
                        'Business',
                        'Economics',
                        'Information Systems',
                        'Law',
                        'Social Sciences'
                        #  'Others'
                    ),
                    selected = 'All'
                )
            ),
            
            box(
                title = 'SUNBURST CHART: DEMOGRAPHICS OF SMU LIBRARY SURVEY RESPONDENTS',
                status = 'primary',
                width = 8,
                solidHeader = T,
                sunburstOutput('plot_sunburst', height = 700)
            ),
            box(
                title = 'GUIDE',
                status = 'primary',
                width = 2,
                solidHeader = T,
                h3("Help text"),
                helpText("Note: Hover over the chart to see the split of respondents.", 
                         "Select the study area to slice and dice the demography.",
                         "For more details refer to the user guide."),
            )
        )
    )
)

p01 = fluidRow(# Overview of LKS/KGCL Respondents and International/Non-International Respondents
    fluidRow(
        column(
            width = 12,
            box(
                title = 'DONUT CHART: LKS VS KGCL',
                status = 'primary',
                width = 5,
                solidHeader = T,
                plotlyOutput('plot_grouppie1', height = 700)
            ),
            box(
                title = 'DONUT CHART: NON-INTERNATIONAL VS INTERNATIONAL',
                status = 'primary',
                width = 5,
                solidHeader = T,
                plotlyOutput('plot_grouppie2', height = 700)
            ),
            box(
                title = 'GUIDE',
                status = 'primary',
                width = 2,
                solidHeader = T,
                h3("Help text"),
                helpText("Note: Hover over the chart to see the values.", 
                         "To download the plot, hover over the top right portion of each chart",
                         "For more details refer to the user guide."),
            )
        )
    )
)

p02 = fluidRow(# Overview of frequency of visits by Respondents
    fluidRow(
        column(
            width = 12,
            box(
                title = 'SETTINGS',
                status = 'primary',
                width = 2,
                solidHeader = T,
                radioButtons(
                    inputId = 'selectG0',
                    label = 'Study Area',
                    choices = c(
                        'All',
                        'Accountancy',
                        'Business',
                        'Economics',
                        'Information Systems',
                        'Law',
                        'Social Sciences'
                        #  'Others'
                    ),
                    selected = 'All'
                )
            ),
            box(
                title = 'GROUPED BAR CHART: FREQUENCY OF VISITORS',
                status = 'primary',
                width = 8,
                solidHeader = T,
                plotlyOutput('plot_groupbar', height = 700)
            ),
            box(
                title = 'GUIDE',
                status = 'primary',
                width = 2,
                solidHeader = T,
                h3("Help text"),
                helpText("Note: Hover over the chart to see the frequency of the respondents.", 
                         "Select the study area to slice and dice the demography.",
                         "Use the top right portion to zoom, pan, print etc,.",
                         "For more details refer to the user guide."),
            )
        )
    )
)

p1 = fluidRow(# Overview of Ratings for Factors by Respondents
    fluidRow(
        column(
            width = 12,
            box(
                title = 'SETTINGS',
                status = 'primary',
                width = 2,
                solidHeader = T,
                selectInput(
                    inputId = 'select0',
                    label = 'Rating Type',
                    choices = c('Importance Ratings', 'Performance Ratings'),
                    selected = 'Importance Ratings'
                ),
                hr(),
                radioButtons(
                    inputId = 'select1',
                    label = 'Library',
                    choices = c('All Libraries','Li Ka Shing Library', 'Kwa Geok Choo Law Library'),
                    selected = 'All Libraries'
                ),
                hr(),
                radioButtons(
                    inputId = 'select2',
                    label = 'Study Area',
                    choices = c(
                        'All',
                        'Accountancy',
                        'Business',
                        'Economics',
                        'Information Systems',
                        'Law',
                        'Social Sciences'
                    ),
                    selected = 'All'
                ),
                hr(),
                radioButtons(
                    inputId = 'select3',
                    label = 'Respondent Role',
                    choices = c(
                        'All Roles',
                        'Undergraduate Students',
                        'Postgraduate Students',
                        'Faculty',
                        'Non-Faculty Staff, Administrators & Others'
                    ),
                    selected = 'All Roles'
                )
            ),
            
            box(
                title = 'DIVERGING STACKED BAR CHART: RATING SPLIT',
                status = 'primary',
                width = 8,
                solidHeader = T,
                plotlyOutput('plot_map', height = 700)
            ),
            box(
                title = 'GUIDE',
                status = 'primary',
                width = 2,
                solidHeader = T,
                h3("Help text"),
                helpText("Note: Hover over the chart to see the Sentiments of the respondents.", 
                         "Select the study area to slice and dice the demography.",
                         "Use the top right portion to zoom, pan, print etc,.",
                         "For more details refer to the user guide."),
            )
        )
    )
)

p2 = fluidRow(# Overview of Mean Variation in Ratings of Factors
    fluidRow(
        column(
            width = 12,
            box(
                title = 'SETTINGS',
                status = 'primary',
                width = 2,
                solidHeader = T,
                selectInput(
                    inputId = 'selectM4',
                    label = 'Service Category',
                    choices = c(
                        'All',
                        'Communication',
                        'Facilities and Equipment',
                        'Information Resources',
                        'Service Delivery'
                    ),
                    selected = 'All'
                ),
                hr(),
                radioButtons(
                    inputId = 'selectM1',
                    label = 'Library',
                    choices = c('All Libraries','Li Ka Shing Library', 'Kwa Geok Choo Law Library'),
                    selected = 'All Libraries'
                ),
                hr(),
                radioButtons(
                    inputId = 'selectM2',
                    label = 'Study Area',
                    choices = c(
                        'All',
                        'Accountancy',
                        'Business',
                        'Economics',
                        'Information Systems',
                        'Law',
                        'Social Sciences',
                        'Others'
                    ),
                    selected = 'All'
                ),
                hr(),
                radioButtons(
                    inputId = 'selectM3',
                    label = 'Respondent Role',
                    choices = c(
                        'All Roles',
                        'Undergraduate Students',
                        'Postgraduate Students',
                        'Faculty',
                        'Non-Faculty Staff, Administrators & Others'
                    ),
                    selected = 'All Roles'
                )
            ),
            
            box(
                title = 'DUMBBELL PLOT: MEAN VARIATION IN RATINGS',
                status = 'primary',
                width = 8,
                solidHeader = T,
                plotlyOutput('plot_bc_top_cnt', height = 700)
            ),
            box(
                title = 'GUIDE',
                status = 'primary',
                width = 2,
                solidHeader = T,
                h3("Help text"),
                helpText("Note: Hover over the chart to see the Mean scores of the respondents.", 
                         "Select the study area to slice and dice the demography.",
                         "Use the top right portion to zoom, pan, print etc,.",
                         "For more details refer to the user guide."),
            )
        )
    )
)


p3 = fluidRow(# Overview of Heat Map Variation in Ratings of Factors
    fluidRow(
        column(
            width = 12,
            box(
                title = 'SETTINGS',
                status = 'primary',
                width = 2,
                solidHeader = T,
                selectInput(
                    inputId = 'selectH00',
                    label = 'Rating Type',
                    choices = c('Importance Ratings', 'Performance Ratings'),
                    selected = 'Importance Ratings'
                ),
                hr(),
                radioButtons(
                    inputId = 'selectH0',
                    label = 'Select Rating',
                    choices = c('Less than 4','4', '5','6','7'),
                    selected = '7'
                ),
                hr(),
                radioButtons(
                    inputId = 'selectH1',
                    label = 'Library',
                    choices = c('All Libraries', 'Li Ka Shing Library', 'Kwa Geok Choo Law Library'),
                    selected = 'All Libraries'
                ),
                hr(),
                radioButtons(
                    inputId = 'selectH3',
                    label = 'Respondent Role',
                    choices = c(
                        'All Roles',
                        'Undergraduate Students',
                        'Postgraduate Students',
                        'Faculty',
                        'Non-Faculty Staff, Administrators & Others'
                    ),
                    selected = 'All Roles'
                )
            ),
            
            box(
                title = 'HEAT MAP: VARIATION IN PROPORTION OF RATINGS',
                status = 'primary',
                width = 8,
                solidHeader = T,
                plotlyOutput('plot_heat_imp', height = 700)
            ),
            box(
                title = 'GUIDE',
                status = 'primary',
                width = 2,
                solidHeader = T,
                h3("Help text"),
                helpText("Note: Hover over the chart to see the Proportion of Ratings of the respondents.", 
                         "Select the study area to slice and dice the demography.",
                         "Use the top right portion to zoom, pan, print etc,.",
                         "For more details refer to the user guide."),
            )
        )
    )
)

p4 = fluidRow(# Overview of Upset Plot Variation in Ratings of Factors
    fluidRow(
        column(
            width = 12,
            box(
                title = 'SETTINGS',
                status = 'primary',
                width = 2,
                solidHeader = T,
                #  tags$style("#myNumericInput {font-size:50px;height:50px;}"),
                sliderInput('selectU0','Select Rating',
                            min = 1, max = 7, value = 1
                ),
                hr(),
                radioButtons(
                    inputId = 'selectU1',
                    label = 'Service Category',
                    choices = c(
                        'Communication',
                        'Facilities and Equipment',
                        'Information Resources',
                        'Service Delivery'
                    ),
                    selected = 'Service Delivery'
                ),
                hr(),
                selectInput(
                    inputId = 'selectU2',
                    label = 'Rating Type',
                    choices = c('Importance Ratings', 'Performance Ratings'),
                    selected = 'Importance Ratings'
                )
            ),
            
            box(
                title = 'UPSET PLOT: RATING SPLIT',
                status = 'primary',
                width = 8,
                solidHeader = T,
                plotOutput('plot_upset', height = 700)
            ),
            box(
                title = 'GUIDE',
                status = 'primary',
                width = 2,
                solidHeader = T,
                h3("Help text"),
                helpText("Note: Use the slider bar to choose the ratings.", 
                         "Select the Service Category to slice and dice.",
                         "For more details refer to the user guide."),
            )
        )
    )
)


p5 = fluidRow(# Overview of Mean Variation in Ratings of Factors
    fluidRow(
        column(
            width = 12,
            box(
                title = 'SETTINGS',
                status = 'primary',
                width = 2,
                solidHeader = T,
                selectInput(
                    inputId = 'selectP1',
                    label = 'Rating Type',
                    choices = c(
                        'Importance Ratings',
                        'Performance Ratings'
                    ),
                    selected = 'Importance Ratings'
                ),
                hr(),
                radioButtons(
                    inputId = 'selectP2',
                    label = 'Study Area',
                    choices = c(
                        'All',
                        'Accountancy',
                        'Business',
                        'Economics',
                        'Information Systems',
                        'Law',
                        'Social Sciences',
                        'Others'
                    ),
                    selected = 'All'
                ),
                hr(),
                radioButtons(
                    inputId = 'selectP3',
                    label = 'Display Options',
                    choices = c( 'Corr Plot', 'Scree Plot','Loading Plot'),
                    selected = 'Loading Plot'
                ),
                hr(),
                selectInput(
                    inputId = 'selectP4',
                    label = 'Dimensions for Loading Plot',
                    choices = c('Dim 1 vs Dim 2','Dim 2 vs Dim 3', 'Dim 3 vs Dim 4', 'Dim 4 vs Dim 5'),
                    selected = 'Dim 1 vs Dim 2'
                )
            ),
            box(
                title = 'PRINCIPLE COMPONENT ANALYSIS',
                status = 'primary',
                width = 8,
                solidHeader = T,
                plotOutput('plot_pca', height = 700)
            ),
            box(
                title = 'GUIDE',
                status = 'primary',
                width = 2,
                solidHeader = T,
                h3("Help text"),
                helpText("Note: Select the Rating Type from the drop down menu.", 
                         "Select the study area to slice and dice the demography.",
                         "Select from the dropdown the dimenions for the Loading Plot.",
                         "For more details refer to the user guide."),
            )
        )
    )
)

p6 = fluidRow(# Overview of Mean Variation in Ratings of Respondents - Anova
    fluidRow(
        column(
            width = 12,
            box(
                title = 'SETTINGS',
                status = 'primary',
                width = 2,
                solidHeader = T,
                selectInput(
                    inputId = 'selectA1',
                    label = 'Rating Type',
                    choices = c(
                        'Importance Ratings',
                        'Performance Ratings'
                    ),
                    selected = 'Importance Ratings'
                ),
                hr(),
                radioButtons(
                    inputId = 'selectA2',
                    label = 'Service Category',
                    choices = c(
                        'Communication',
                        'Facilities and Equipment',
                        'Information Resources',
                        'Service Delivery'
                    ),
                    selected = 'Service Delivery'
                ),
                hr(),
                radioButtons(
                    inputId = 'selectA3',
                    label = 'Display Option',
                    choices = c(
                        'QQ-Plot',
                        'Anova Graph'
                    ),
                    selected = 'Anova Graph'
                )
            ),
            box(
                title = 'ANOVA: COMPARISON OF MEAN RATINGS',
                status = 'primary',
                width = 8,
                solidHeader = T,
                plotlyOutput('plot_anova', height = 350)
            ),
            box(
                title = 'ANOVA: TUKEY HSD TEST RESULTS',
                status = 'primary',
                width = 10,
                solidHeader = T,
                DT::dataTableOutput('plot_tukey', height = 300)
            )
        )
    )
)

header = dashboardHeader(
    title = textOutput('title')
    # dropdownMenu(
    #   type = 'messages',
    #   messageItem(
    #     from = 'G3 T2',
    #     message = 'Hello!',
    #     time = '11:30'
    #   )
    # )
)

sidebar = dashboardSidebar(#change to blue
    sidebarMenu(
        menuItem('INTRODUCTION', startExpanded = T,
                 menuSubItem('Overview', tabName = 'introduction', icon = icon('award')),
                 menuSubItem('Navigation Map', tabName = 'navigation', icon = icon('route'))
                 
        ),
        menuItem('DEMOGRAPHICS', startExpanded = T,
                 menuSubItem('Sunburst Chart', tabName = 'sunburst', icon = icon('circle-notch')),
                 menuSubItem('Donut Chart', tabName = 'grouppie', icon = icon('chart-pie')),
                 menuSubItem('Grouped Bar Chart', tabName = 'groupbar', icon = icon('chart-bar'))
        ),
        menuItem('VISUALISATIONS', startExpanded = T,
                 menuSubItem('Diverging Bar Chart', tabName = 'survey', icon = icon('align-left')),
                 menuSubItem('Dumbbell Plot', tabName = 'mean-score', icon = icon('line-chart')),
                 menuSubItem('Heat Map Plot', tabName = 'heat-map', icon = icon('braille')),
                 menuSubItem('Upset Plot', tabName = 'upset', icon = icon('chart-bar'))
        ),
        menuItem('MODELLING', startExpanded = T,
                 menuSubItem('Principle Component Analysis', tabName = 'pca', icon = icon('dashboard')),
                 menuSubItem('Anova', tabName = 'anova', icon = icon('ellipsis-v'))
        ),
        menuItem('ABOUT', startExpanded = T,
                 menuSubItem('About', tabName = 'about', icon = icon('info'))
        )
    )
)

body = dashboardBody(
    tabItems(
        tabItem(tabName = 'introduction',p00),
        tabItem(tabName = 'navigation',p000),
        tabItem(tabName = 'sunburst',p0),
        tabItem(tabName = 'grouppie',p01),
        tabItem(tabName = 'groupbar',p02),
        tabItem(tabName = 'survey', p1),
        tabItem(tabName = 'mean-score',p2),
        tabItem(tabName = 'heat-map',p3),
        tabItem(tabName = 'upset',p4),
        tabItem(tabName = 'pca',p5),
        tabItem(tabName = 'anova',p6),
        tabItem(tabName = 'about', includeMarkdown('aboutme.txt'))
    ),
    tags$head(tags$style(HTML(
        '.main-sidebar {
            background-color:   #1b4f72 !important;
          }
          main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #1b4f72;
          }
          .myClass { 
        font-size: 22px;
        line-height: 50px;
        text-align: center;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 50px;
        overflow: hidden;
        color: white;
      }                      
        '))
    ),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> <b> SINGAPORE MANAGEMENT UNIVERSITY - LIBRARY SURVEY ANALYSIS </b> </span>\');
      })
     '))
)

ui = dashboardPage(
    skin = 'yellow',
    header,
    sidebar,
    body
)

# shinyApp (ui=ui, server=server)
server = function(input, output, session) {
    output$title = renderText({
        paste('NAVIGATION PANEL')
    })
    
    opt_explain = c(
        'Li Ka Shing Library',
        'Kwa Geok Choo Law Library',
        'All Libraries'
    )
    
    updateRadioButtons(
        session,
        inputId = 'in_rb_result_sc',
        label = 'Select',
        choices = opt_explain,
        selected = opt_explain[0]
    )
    
    
    # Page 0
    
    output$plot_sunburst = renderSunburst({
        
        lib_surv_data<- lib_data
        
        if(input$selectS0 == 'Accountancy')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '1')  
        }
        else if(input$selectS0 == 'Business')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '2') 
        }
        else if(input$selectS0 == 'Economics')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '3')
        }
        else if(input$selectS0 == 'Information Systems')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '4')
        }
        else if(input$selectS0 == 'Law')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '5')
        }
        else if(input$selectS0 == 'Social Sciences')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '6')
        }
        #    else if(input$selectS0 == 'Others')
        #   {
        #      lib_surv_data <- filter(lib_surv_data, StudyArea == '7')
        #    }
        else if(input$selectS0 == 'All')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '1' | StudyArea == '2'| StudyArea == '3'| StudyArea == '4'| StudyArea == '5'| StudyArea == '6'| StudyArea == '7')
        }
        
        df1 <-lib_surv_data %>% 
            filter(Position == '1') %>%
            summarize(Undergraduate_year1=n_distinct(ResponseID))
        
        df2<-lib_surv_data %>% 
            filter(Position == '2') %>%
            summarize(Undergraduate_year2=n_distinct(ResponseID))
        
        df3<-lib_surv_data %>% 
            filter(Position == '3') %>%
            summarize(Undergraduate_year3=n_distinct(ResponseID))
        
        df4<-lib_surv_data %>% 
            filter(Position == '4') %>%
            summarize(Undergraduate_year4=n_distinct(ResponseID))
        
        df5<-lib_surv_data %>% 
            filter(Position == '5') %>%
            summarize(exch=n_distinct(ResponseID))
        
        df6<-lib_surv_data %>% 
            filter(Position == '6') %>%
            summarize(master=n_distinct(ResponseID))
        
        df7<-lib_surv_data %>% 
            filter(Position == '7') %>%
            summarize(doc=n_distinct(ResponseID))
        
        
        df8<-lib_surv_data %>% 
            filter(Position == '8') %>%
            summarize(prof=n_distinct(ResponseID))
        
        df9<-lib_surv_data %>% 
            filter(Position == '9') %>%
            summarize(assocprof=n_distinct(ResponseID))
        
        df10<-lib_surv_data %>% 
            filter(Position == '10') %>%
            summarize(assisprof=n_distinct(ResponseID))
        
        df11<-lib_surv_data %>% 
            filter(Position == '11') %>%
            summarize(lecturer=n_distinct(ResponseID))
        
        df12<-lib_surv_data %>% 
            filter(Position == '12') %>%
            summarize(resch=n_distinct(ResponseID))
        
        df13<-lib_surv_data %>% 
            filter(Position == '13') %>%
            summarize(admin=n_distinct(ResponseID))
        
        df14<-lib_surv_data %>% 
            filter(Position == '14') %>%
            summarize(other=n_distinct(ResponseID))
        
        
        
        data1 <- data.frame(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14)
        data2<-data1 %>% gather(Category,Respondents, 1:14)
        data3 <- data2 %>% dplyr::mutate(id = row_number())
        data3$parent[data3$id== 1 | data3$id== 2 |data3$id== 3 | data3$id== 4]= "Undergrads"
        data3$parent[data3$id== 5]= "Exchange"
        data3$parent[data3$id== 6 | data3$id== 7]= "PostGrads"
        data3$parent[data3$id== 8 | data3$id== 9| data3$id== 10| data3$id== 11| data3$id== 12]= "Faculty"
        data3$parent[data3$id== 13 | data3$id== 14]= "Others"
        
        packageVersion("sunburstR")
        if(input$selectS0 == 'All' | input$selectS0 == 'Business' | input$selectS0 == 'Economics' | input$selectS0 == 'Information Systems' )
        {
            colors <- c("#003cb3","#99bbff","#4d88ff","#0055ff","#002266","#ff0066","#ffd24d","#997300","#99ff99","#00cc00","#009900","#006600","#004d00","#cc00ff","#7a0099","#660080","#8f00b3","#660080","#660080")
            labels <- c("Undergrads","Undergrads-Undergraduate_year1","Undergrads-Undergraduate_year2","Undergrads-Undergraduate_year3","Undergrads-Undergraduate_year4","Exchange","Exchange-exch","PostGrads","PostGrads-master",
                        "PostGrads-doc","Faculty","Faculty-prof","Faculty-assocprof","Faculty-assisprof","Faculty-lecturer","Faculty-resch","Others","Others-admin","	Others-other")
        }
        
        if(input$selectS0 == 'Accountancy')
        {
            colors <- c("#003cb3","#99bbff","#4d88ff","#0055ff","#002266","#997300","#99ff99","#00cc00","#009900","#006600","#004d00","#cc00ff","#7a0099","#660080","#8f00b3","#660080","#660080")
            labels <- c("Undergrads","Undergrads-Undergraduate_year1","Undergrads-Undergraduate_year2","Undergrads-Undergraduate_year3","Undergrads-Undergraduate_year4","PostGrads","PostGrads-master",
                        "PostGrads-doc","Faculty","Faculty-prof","Faculty-assocprof","Faculty-assisprof","Faculty-lecturer","Faculty-resch","Others","Others-admin","	Others-other")
        }
        
        if(input$selectS0 == 'Law')
        {
            colors <- c("#003cb3","#99bbff","#4d88ff","#0055ff","#002266","#997300","#ffd24d","#00cc00","#009900","#004d00","#cc00ff","#660080","#8f00b3","#660080","#660080")
            labels <- c("Undergrads","Undergrads-Undergraduate_year1","Undergrads-Undergraduate_year2","Undergrads-Undergraduate_year3","Undergrads-Undergraduate_year4","PostGrads","PostGrads-master",
                        "PostGrads-doc","Faculty","Faculty-assocprof","Faculty-assisprof","Faculty-lecturer","Others","Others-admin","	Others-other")
        }
        if(input$selectS0 == 'Social Sciences')
        {
            colors <- c("#003cb3","#99bbff","#4d88ff","#0055ff","#002266","#997300","#00cc00","#009900","#006600","#004d00","#cc00ff","#660080","#8f00b3","#660080","#660080")
            labels <- c("Undergrads","Undergrads-Undergraduate_year1","Undergrads-Undergraduate_year2","Undergrads-Undergraduate_year3","Undergrads-Undergraduate_year4","PostGrads",
                        "PostGrads-doc","Faculty","Faculty-assocprof","Faculty-assisprof","Faculty-lecturer","Faculty-resch","Others","Others-admin","	Others-other")
        }
        
        data6 <- data3 %>%
            mutate(path = paste(parent, Category,sep="-")) %>%
            dplyr::select(path,Respondents)
        data6
        
        # Plot for sunburst
        
        base_settings =
            theme_bw() +
            theme(legend.position = "none") +
            theme(plot.title = element_text(hjust = 0.5))
        
        p <- sunburst(data6,count = TRUE, legend=FALSE, colors = list(range = colors, domain = labels))
        p
        
    })
    
    # Page 01
    
    output$plot_grouppie1 = renderPlotly({
        
        lib_surv_data <- lib_data
        stu_data_hm<- lib_surv_data%>% dplyr::select(ResponseID,ID,Campus)
        d<-na.omit(stu_data_hm)
        
        
        
        d$ID <- ifelse(d$ID==1 , "International",
                       ifelse(d$ID==2 , "Non-International",
                              NA))
        
        d$Campus <- ifelse(d$Campus==1 , "Li Ka Shing Library",
                           ifelse(d$Campus==2 , "Kwa Geok Choo Law Library",
                                  NA))
        
        
        
        df1 <-d %>% dplyr::group_by(Campus)%>% dplyr::summarize(Count=n_distinct(ResponseID))
        names(df1)[1]<-"Campus"
        
        df2 <-d %>% dplyr::group_by(ID)%>% dplyr::summarize(Count=n_distinct(ResponseID))
        names(df2)[1]<-"Demography"
        
        
        fig1 <- df1 %>% plot_ly(labels = ~Campus, values = ~Count)
        fig1 <- fig1 %>% add_pie(hole = 0.7)
        fig1 <- fig1 %>% layout(title = "Comparision of Library used by Respondents",  showlegend = T,
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        fig1
        
    })
    
    output$plot_grouppie2 = renderPlotly({
        
        lib_surv_data <- lib_data
        stu_data_hm<- lib_surv_data%>% dplyr::select(ResponseID,ID,Campus)
        d<-na.omit(stu_data_hm)
        
        
        
        d$ID <- ifelse(d$ID==1 , "International",
                       ifelse(d$ID==2 , "Non-International",
                              NA))
        
        d$Campus <- ifelse(d$Campus==1 , "Li Ka Shing Library",
                           ifelse(d$Campus==2 , "Kwa Geok Choo Law Library",
                                  NA))
        
        
        
        df1 <-d %>% dplyr::group_by(Campus)%>% dplyr::summarize(Count=n_distinct(ResponseID))
        names(df1)[1]<-"Campus"
        
        df2 <-d %>% dplyr::group_by(ID)%>% dplyr::summarize(Count=n_distinct(ResponseID))
        names(df2)[1]<-"Demography"
        
        fig2 <- df2 %>% plot_ly(labels = ~Demography, values = ~Count)
        fig2 <- fig2 %>% add_pie(hole = 0.7)
        fig2 <- fig2 %>% layout(title = "Comparision of International vs Non-International Respondents",  showlegend = T,
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        fig2
        
    })
    
    
    # Page 02
    
    output$plot_groupbar = renderPlotly({
        
        lib_surv_data <- lib_data
        
        if(input$selectG0 == 'Accountancy')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '1')  
        }
        else if(input$selectG0 == 'Business')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '2') 
        }
        else if(input$selectG0 == 'Economics')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '3')
        }
        else if(input$selectG0 == 'Information Systems')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '4')
        }
        else if(input$selectG0 == 'Law')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '5')
        }
        else if(input$selectG0 == 'Social Sciences')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '6')
        }
        #    else if(input$selectS0 == 'Others')
        #   {
        #      lib_surv_data <- filter(lib_surv_data, StudyArea == '7')
        #    }
        else if(input$selectG0 == 'All')
        {
            lib_surv_data <- filter(lib_surv_data, StudyArea == '1' | StudyArea == '2'| StudyArea == '3'| StudyArea == '4'| StudyArea == '5'| StudyArea == '6'| StudyArea == '7')
        }
        
        stu_data_hm<- lib_surv_data%>%
            dplyr::select(ResponseID,ID,StudyArea,starts_with("H"))
        d<-na.omit(stu_data_hm)
        
        d$HowOftenL <- ifelse(d$HowOftenL==1 , "Daily",
                              ifelse(d$HowOftenL==2 , "Weekly",
                                     ifelse(d$HowOftenL==3, "Monthly",
                                            ifelse(d$HowOftenL==4, "Quarterly",
                                                   ifelse(d$HowOftenL==5, "Never",
                                                          NA)))))
        
        d$HowOftenC <- ifelse(d$HowOftenC==1 , "Daily",
                              ifelse(d$HowOftenC==2 , "Weekly",
                                     ifelse(d$HowOftenC==3, "Monthly",
                                            ifelse(d$HowOftenC==4, "Quarterly",
                                                   ifelse(d$HowOftenC==5, "Never",
                                                          NA)))))                              
        
        d$HowOftenW <- ifelse(d$HowOftenW==1 , "Daily",
                              ifelse(d$HowOftenW==2 , "Weekly",
                                     ifelse(d$HowOftenW==3, "Monthly",
                                            ifelse(d$HowOftenW==4, "Quarterly",
                                                   ifelse(d$HowOftenW==5, "Never",
                                                          NA)))))           
        
        
        names(d)[4]<-"Library"
        names(d)[5]<-"Campus"
        names(d)[6]<-"Resources"
        d1<-d[c(1,4,5,6)]
        
        df1 <-d1 %>% 
            dplyr::group_by(d1$Library)%>%
            dplyr::summarize(Count=n_distinct(ResponseID))
        names(df1)[1]<-"ID"
        
        df2 <-d1 %>% 
            dplyr::group_by(d1$Campus)%>%
            dplyr::summarize(Count=n_distinct(ResponseID))
        
        names(df2)[1]<-"ID"
        
        df3 <-d1 %>% 
            dplyr::group_by(d1$Resources)%>%
            dplyr::summarize(Count=n_distinct(ResponseID))
        names(df3)[1]<-"ID"
        
        df4 <- full_join(df1,df2,by="ID")
        df5 <- full_join(df3,df4,by="ID")
        
        names(df5)[2]<- "Resources"
        names(df5)[3]<- "Library"
        names(df5)[4]<- "Campus"
        
        df6<-df5[c(1,3,4,2)]
        
        
        df7<-df6 %>%
            pivot_longer(-ID, names_to = "Category", values_to = "count")
        df7
        
        #Viz 1
        
        df7$ID <- factor(df7$ID,
                         levels = c("Daily","Weekly", "Monthly","Quarterly","Never"))
        
        g1<-ggplot(df7, aes(fill=Category, y=count, x=ID)) + 
            geom_bar(position="dodge", stat="identity",alpha=0.8) +
            scale_fill_viridis(discrete = T, option = "E")+ theme_ipsum()+ labs(x = "Period" ,y="No. of Respondents")
        
        #plotly for Viz 1
        fig1 <- plotly::ggplotly(g1, tooltip=c("text","Category","y"))
        fig1 <- fig1 %>% layout( titlefont=list(family='Arial', size=18),title = "Frequency of Visits by Respondents")
        
        fig1
        
    })
    
    # Page 1
    
    output$plot_map = renderPlotly({
        
        #Based on Radiobutton selections made by the user
        if(input$select1 == 'All Libraries'){
            lib_data_student <- filter(lib_data, Campus == '1' | Campus == '2')
            if(input$select2 == 'All'){
                lib_data_student <- filter(lib_data_student, StudyArea == '1' | StudyArea == '2'| StudyArea == '3'| StudyArea == '4'| StudyArea == '5'| StudyArea == '6'| StudyArea == '7')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
                
            }
            else if(input$select2 == 'Accountancy')
            {
                lib_data_student <- filter(lib_data, StudyArea == '1')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Business')
            {
                lib_data_student <- filter(lib_data, StudyArea == '2')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Economics')
            {
                lib_data_student <- filter(lib_data, StudyArea == '3')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Information Systems')
            {
                lib_data_student <- filter(lib_data, StudyArea == '4')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Law')
            {
                lib_data_student <- filter(lib_data, StudyArea == '5')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Social Sciences')
            {
                lib_data_student <- filter(lib_data, StudyArea == '6')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Others')
            {
                lib_data_student <- filter(lib_data, StudyArea == '7')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
        }
        else if(input$select1 == 'Li Ka Shing Library'){
            lib_data_student <- filter(lib_data, Campus == '1')
            if(input$select2 == 'All'){
                lib_data_student <- filter(lib_data_student, StudyArea == '1' | StudyArea == '2'| StudyArea == '3'| StudyArea == '4'| StudyArea == '5'| StudyArea == '6'| StudyArea == '7')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
                
            }
            else if(input$select2 == 'Accountancy')
            {
                lib_data_student <- filter(lib_data, StudyArea == '1')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Business')
            {
                lib_data_student <- filter(lib_data, StudyArea == '2')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Economics')
            {
                lib_data_student <- filter(lib_data, StudyArea == '3')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Information Systems')
            {
                lib_data_student <- filter(lib_data, StudyArea == '4')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Law')
            {
                lib_data_student <- filter(lib_data, StudyArea == '5')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Social Sciences')
            {
                lib_data_student <- filter(lib_data, StudyArea == '6')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Others')
            {
                lib_data_student <- filter(lib_data, StudyArea == '7')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
        }
        else if(input$select1 == 'Kwa Geok Choo Law Library')
        {
            lib_data_student <- filter(lib_data, Campus == '2')
            if(input$select2 == 'All'){
                lib_data_student <- filter(lib_data_student, StudyArea == '1' | StudyArea == '2'| StudyArea == '3'| StudyArea == '4'| StudyArea == '5'| StudyArea == '6'| StudyArea == '7')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
                
            }
            else if(input$select2 == 'Accountancy')
            {
                lib_data_student <- filter(lib_data, StudyArea == '1')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Business')
            {
                lib_data_student <- filter(lib_data, StudyArea == '2')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Economics')
            {
                lib_data_student <- filter(lib_data, StudyArea == '3')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Information Systems')
            {
                lib_data_student <- filter(lib_data, StudyArea == '4')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Law')
            {
                lib_data_student <- filter(lib_data, StudyArea == '5')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Social Sciences')
            {
                lib_data_student <- filter(lib_data, StudyArea == '6')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$select2 == 'Others')
            {
                lib_data_student <- filter(lib_data, StudyArea == '7')
                if(input$select3 == 'Undergraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$select3 == 'Postgraduate Students')
                {
                    lib_data_student <- filter(lib_data_student, Position == '6'| Position == '7')
                }
                else if(input$select3 == 'Faculty')
                {
                    lib_data_student <- filter(lib_data_student, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$select3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_student <- filter(lib_data_student, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$select3 == 'All Roles')
                {
                    lib_data_student <- filter(lib_data_student, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
        }
        
        #Data Cleaning after selection 
        
        if(input$select0 == 'Importance Ratings')
        {
            
            lib_data_student$Comment1 <- NULL
            lib_data_student$HowOftenL <- NULL
            lib_data_student$HowOftenC <- NULL
            lib_data_student$HowOftenW <- NULL
            lib_data_student$Campus <- NULL
            lib_data_student$Position <- NULL
            lib_data_student$StudyArea <- NULL
            lib_data_student$ID <- NULL
            lib_data_student$NPS1 <- NULL
            lib_data_student1 <- dplyr::select(lib_data_student,-starts_with("NA"))
            lib_data_student2 <- dplyr::select(lib_data_student1,-starts_with("P"))
            lib_data_student2<-na.omit(lib_data_student2)
            survey <- lib_data_student2 %>%
                pivot_longer(-ResponseID, names_to = "measure", values_to = "response")
            survey$measure <- as.factor(survey$measure)
            survey$response <- as.factor(survey$response)
            survey_df <- table(survey$measure,survey$response) %>% as.data.frame.matrix()
            survey_df <- tibble::rownames_to_column(survey_df, var="Measure")
            tab <- survey_df
            mytitle<-"Sentiment of Surveys by Questions (Importance)\n"
            mylevels<-c("Strongly Disagree","Disagree","Somewhat Disagree","Not Sure","Somewhat Agree","Agree","Strongly Agree")
            tab <- cbind(tab[1], prop.table(as.matrix(tab[-1]), margin = 1))
            tab[,-1] <-round(tab[,-1],2) #the "-1" excludes column 1
            numlevels<-length(tab[1,])-1
            numcenter<-ceiling(numlevels/2)+1
            tab$midvalues<-tab[,numcenter]/2
            tab2<-cbind(tab[,1],tab[,2:ceiling(numlevels/2)],
                        tab$midvalues,tab$midvalues,tab[,numcenter:numlevels+1])
            colnames(tab2)<-c("Factor",mylevels[1:floor(numlevels/2)],"notsure-",
                              "notsure+",mylevels[numcenter:numlevels])
            numlevels<-length(mylevels)+1
            point1<-2
            point2<-((numlevels)/2)+1
            point3<-point2+1
            point4<-numlevels+1
            mymin<-(ceiling(max(rowSums(tab2[,point1:point2]))*4)/4)*-100
            mymax<-(ceiling(max(rowSums(tab2[,point3:point4]))*4)/4)*100
            tab2 <- tab2[,c(1,2,3,4,5,9,8,7,6)]
            numlevels<-length(tab[1,])-1
            temp.rows<-length(tab2[,1])
            tab3<-melt(tab2,id="Factor")
            #tab3$col<-rep(pal,each=temp.rows)
            tab3$value<-tab3$value*100
            tab3$Factor<-str_wrap(tab3$Factor, width = 40)
            tab3$Factor<-factor(tab3$Factor, levels = tab2$Factor[order(-(tab2[,5]+tab2[,6]+tab2[,7]))])
            highs<-na.omit(tab3[(length(tab3[,1])/2)+1:length(tab3[,1]),])
            lows<-na.omit(tab3[1:(length(tab3[,1])/2),])
            lows <- lows[rev(rownames(lows)),]
            #lows$col <- factor(lows$col, levels = c("#B2182B","#EF8A62","#FDDBC7", "#DFDFDF"))
            lows<-lows[dim(lows)[1]:1,]
            highs <- transform(highs, Question= ifelse(Factor == "I01","Informed about Services"
                                                       ,ifelse(Factor == "I02","Provide Useful Info"
                                                               ,ifelse(Factor == "I03","Library signage is clear"
                                                                       ,ifelse(Factor == "I04","Workshops Assist in Learning/Research"
                                                                               ,ifelse(Factor == "I05","Anticipates Learning/Research needs"
                                                                                       ,ifelse(Factor == "I06","Opening hours meets needs"
                                                                                               ,ifelse(Factor == "I07","Requested Books/Articles delivered"
                                                                                                       ,ifelse(Factor == "I08","Self Service meets needs"
                                                                                                               ,ifelse(Factor == "I09","Online enquiry meets needs"
                                                                                                                       ,ifelse(Factor == "I10","F2F enquiry meets needs"
                                                                                                                               ,ifelse(Factor == "I11","Library Shelves have items I need"
                                                                                                                                       ,ifelse(Factor == "I12","Library Staff give accurate answers"
                                                                                                                                               ,ifelse(Factor == "I13","Library staff are helpful"
                                                                                                                                                       ,ifelse(Factor == "I14","Can find quiet place to study"
                                                                                                                                                               ,ifelse(Factor == "I15","Can find place to work as groups"
                                                                                                                                                                       ,ifelse(Factor == "I16","Computer is available"
                                                                                                                                                                               ,ifelse(Factor == "I17","Laptop Facilities provided"
                                                                                                                                                                                       ,ifelse(Factor == "I18","Wireless Access Provided"
                                                                                                                                                                                               ,ifelse(Factor == "I19","Print/Scan Facilities Provided"
                                                                                                                                                                                                       ,ifelse(Factor == "I20","Info/Resources meet my needs"
                                                                                                                                                                                                               ,ifelse(Factor == "I21","Online Resources are Useful"
                                                                                                                                                                                                                       ,ifelse(Factor == "I22","Course specific Resources present"
                                                                                                                                                                                                                               ,ifelse(Factor == "I23","Access Library away from Campus"
                                                                                                                                                                                                                                       ,ifelse(Factor == "I24","Library Search Engine is useful"
                                                                                                                                                                                                                                               ,ifelse(Factor == "I25","Access to Library made me succesful"
                                                                                                                                                                                                                                                       ,"Mobile devices useful to access content"))))))))))))))))))))))))))
            lows <- transform(lows, Question= ifelse(Factor == "I01","Informed about Services"
                                                     ,ifelse(Factor == "I02","Provide Useful Info"
                                                             ,ifelse(Factor == "I03","Library signage is clear"
                                                                     ,ifelse(Factor == "I04","Workshops Assist in Learning/Research"
                                                                             ,ifelse(Factor == "I05","Anticipates Learning/Research needs"
                                                                                     ,ifelse(Factor == "I06","Opening hours meets needs"
                                                                                             ,ifelse(Factor == "I07","Requested Books/Articles delivered"
                                                                                                     ,ifelse(Factor == "I08","Self Service meets needs"
                                                                                                             ,ifelse(Factor == "I09","Online enquiry meets needs"
                                                                                                                     ,ifelse(Factor == "I10","F2F enquiry meets needs"
                                                                                                                             ,ifelse(Factor == "I11","Library Shelves have items I need"
                                                                                                                                     ,ifelse(Factor == "I12","Library Staff give accurate answers"
                                                                                                                                             ,ifelse(Factor == "I13","Library staff are helpful"
                                                                                                                                                     ,ifelse(Factor == "I14","Can find quiet place to study"
                                                                                                                                                             ,ifelse(Factor == "I15","Can find place to work as groups"
                                                                                                                                                                     ,ifelse(Factor == "I16","Computer is available"
                                                                                                                                                                             ,ifelse(Factor == "I17","Laptop Facilities provided"
                                                                                                                                                                                     ,ifelse(Factor == "I18","Wireless Access Provided"
                                                                                                                                                                                             ,ifelse(Factor == "I19","Print/Scan Facilities Provided"
                                                                                                                                                                                                     ,ifelse(Factor == "I20","Info/Resources meet my needs"
                                                                                                                                                                                                             ,ifelse(Factor == "I21","Online Resources are Useful"
                                                                                                                                                                                                                     ,ifelse(Factor == "I22","Course specific Resources present"
                                                                                                                                                                                                                             ,ifelse(Factor == "I23","Access Library away from Campus"
                                                                                                                                                                                                                                     ,ifelse(Factor == "I24","Library Search Engine is useful"
                                                                                                                                                                                                                                             ,ifelse(Factor == "I25","Access to Library made me succesful"
                                                                                                                                                                                                                                                     ,"Mobile devices useful to access content"))))))))))))))))))))))))))
            
            
            colnames(highs)[3] <- "Percentage"
            colnames(lows)[3] <- "Percentage"
            
            colnames(highs)[2] <- "Response"
            colnames(lows)[2] <- "Response"
            
            mycolors <- c("#67A9CF","#EF8A62","#DFDFDF","#DFDFDF","#D1E5F0","#FDDBC7","#2166AC","#B2182B")
            
            
            base_settings =
                theme_bw() +
                theme(legend.position = "none") +
                theme(plot.title = element_text(hjust = 0.5))
            
            # Plotting the diverging stacked bar chart
            
            ggplotly(ggplot() + geom_bar(data=highs, aes(x = Question, y=Percentage, fill=Response), position="stack", stat="identity") +
                         geom_bar(data=lows, aes(x = Question, y=-Percentage, fill=Response), position="stack", stat="identity") +
                         geom_hline(yintercept = 0, color =c("white")) +
                         scale_fill_manual(values = mycolors) +
                         theme_minimal() +
                         coord_flip() +
                         labs(title=mytitle, caption = "Data Source: © Singapore Management University - Library Survey Data 2018", y="Percentage of Respondents", x="Factors") +
                         theme(plot.title = element_text(size=14, hjust=0.5)) +
                         theme(axis.text.y = element_text(hjust=1)) +
                         theme(legend.position = "right") +
                         scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax)) + theme(
                             plot.background = element_rect(
                                 fill = "white",
                                 colour = "black",
                                 size = 1
                             )
                         )      , tooltip = c("x","y","Response"))
            
        }
        else if(input$select0 == 'Performance Ratings')
        {
            
            lib_data_student$Comment1 <- NULL
            lib_data_student$HowOftenL <- NULL
            lib_data_student$HowOftenC <- NULL
            lib_data_student$HowOftenW <- NULL
            lib_data_student$Campus <- NULL
            lib_data_student$Position <- NULL
            lib_data_student$StudyArea <- NULL
            lib_data_student$ID <- NULL
            lib_data_student$NPS1 <- NULL
            lib_data_student1 <- dplyr::select(lib_data_student,-starts_with("NA"))
            lib_data_student2 <- dplyr::select(lib_data_student1,-starts_with("I"))
            lib_data_student2$P27 <- NULL
            lib_data_student2<-na.omit(lib_data_student2)
            survey <- lib_data_student2 %>%
                pivot_longer(-ResponseID, names_to = "measure", values_to = "response")
            survey$measure <- as.factor(survey$measure)
            survey$response <- as.factor(survey$response)
            survey_df <- table(survey$measure,survey$response) %>% as.data.frame.matrix()
            survey_df <- tibble::rownames_to_column(survey_df, var="Measure")
            tab <- survey_df
            mytitle<-"Sentiment of Surveys by Questions (Performance)\n"
            mylevels<-c("Strongly Disagree","Disagree","Somewhat Disagree","Not Sure","Somewhat Agree","Agree","Strongly Agree")
            tab <- cbind(tab[1], prop.table(as.matrix(tab[-1]), margin = 1))
            tab[,-1] <-round(tab[,-1],2) #the "-1" excludes column 1
            numlevels<-length(tab[1,])-1
            numcenter<-ceiling(numlevels/2)+1
            tab$midvalues<-tab[,numcenter]/2
            tab2<-cbind(tab[,1],tab[,2:ceiling(numlevels/2)],
                        tab$midvalues,tab$midvalues,tab[,numcenter:numlevels+1])
            colnames(tab2)<-c("Factor",mylevels[1:floor(numlevels/2)],"notsure-",
                              "notsure+",mylevels[numcenter:numlevels])
            numlevels<-length(mylevels)+1
            point1<-2
            point2<-((numlevels)/2)+1
            point3<-point2+1
            point4<-numlevels+1
            mymin<-(ceiling(max(rowSums(tab2[,point1:point2]))*4)/4)*-100
            mymax<-(ceiling(max(rowSums(tab2[,point3:point4]))*4)/4)*100
            tab2 <- tab2[,c(1,2,3,4,5,9,8,7,6)]
            numlevels<-length(tab[1,])-1
            temp.rows<-length(tab2[,1])
            tab3<-melt(tab2,id="Factor")
            #tab3$col<-rep(pal,each=temp.rows)
            tab3$value<-tab3$value*100
            tab3$Factor<-str_wrap(tab3$Factor, width = 40)
            tab3$Factor<-factor(tab3$Factor, levels = tab2$Factor[order(-(tab2[,5]+tab2[,6]+tab2[,7]))])
            highs<-na.omit(tab3[(length(tab3[,1])/2)+1:length(tab3[,1]),])
            lows<-na.omit(tab3[1:(length(tab3[,1])/2),])
            lows <- lows[rev(rownames(lows)),]
            #lows$col <- factor(lows$col, levels = c("#B2182B","#EF8A62","#FDDBC7", "#DFDFDF"))
            lows<-lows[dim(lows)[1]:1,]
            highs <- transform(highs, Question= ifelse(Factor == "P01","Informed about Services"
                                                       ,ifelse(Factor == "P02","Provide Useful Info"
                                                               ,ifelse(Factor == "P03","Library signage is clear"
                                                                       ,ifelse(Factor == "P04","Workshops Assist in Learning/Research"
                                                                               ,ifelse(Factor == "P05","Anticipates Learning/Research needs"
                                                                                       ,ifelse(Factor == "P06","Opening hours meets needs"
                                                                                               ,ifelse(Factor == "P07","Requested Books/Articles delivered"
                                                                                                       ,ifelse(Factor == "P08","Self Service meets needs"
                                                                                                               ,ifelse(Factor == "P09","Online enquiry meets needs"
                                                                                                                       ,ifelse(Factor == "P10","F2F enquiry meets needs"
                                                                                                                               ,ifelse(Factor == "P11","Library Shelves have items I need"
                                                                                                                                       ,ifelse(Factor == "P12","Library Staff give accurate answers"
                                                                                                                                               ,ifelse(Factor == "P13","Library staff are helpful"
                                                                                                                                                       ,ifelse(Factor == "P14","Can find quiet place to study"
                                                                                                                                                               ,ifelse(Factor == "P15","Can find place to work as groups"
                                                                                                                                                                       ,ifelse(Factor == "P16","Computer is available"
                                                                                                                                                                               ,ifelse(Factor == "P17","Laptop Facilities provided"
                                                                                                                                                                                       ,ifelse(Factor == "P18","Wireless Access Provided"
                                                                                                                                                                                               ,ifelse(Factor == "P19","Print/Scan Facilities Provided"
                                                                                                                                                                                                       ,ifelse(Factor == "P20","Info/Resources meet my needs"
                                                                                                                                                                                                               ,ifelse(Factor == "P21","Online Resources are Useful"
                                                                                                                                                                                                                       ,ifelse(Factor == "P22","Course specific Resources present"
                                                                                                                                                                                                                               ,ifelse(Factor == "P23","Access Library away from Campus"
                                                                                                                                                                                                                                       ,ifelse(Factor == "P24","Library Search Engine is useful"
                                                                                                                                                                                                                                               ,ifelse(Factor == "P25","Access to Library made me succesful"
                                                                                                                                                                                                                                                       ,"Mobile devices useful to access content"))))))))))))))))))))))))))
            lows <- transform(lows, Question= ifelse(Factor == "P01","Informed about Services"
                                                     ,ifelse(Factor == "P02","Provide Useful Info"
                                                             ,ifelse(Factor == "P03","Library signage is clear"
                                                                     ,ifelse(Factor == "P04","Workshops Assist in Learning/Research"
                                                                             ,ifelse(Factor == "P05","Anticipates Learning/Research needs"
                                                                                     ,ifelse(Factor == "P06","Opening hours meets needs"
                                                                                             ,ifelse(Factor == "P07","Requested Books/Articles delivered"
                                                                                                     ,ifelse(Factor == "P08","Self Service meets needs"
                                                                                                             ,ifelse(Factor == "P09","Online enquiry meets needs"
                                                                                                                     ,ifelse(Factor == "P10","F2F enquiry meets needs"
                                                                                                                             ,ifelse(Factor == "P11","Library Shelves have items I need"
                                                                                                                                     ,ifelse(Factor == "P12","Library Staff give accurate answers"
                                                                                                                                             ,ifelse(Factor == "P13","Library staff are helpful"
                                                                                                                                                     ,ifelse(Factor == "P14","Can find quiet place to study"
                                                                                                                                                             ,ifelse(Factor == "P15","Can find place to work as groups"
                                                                                                                                                                     ,ifelse(Factor == "P16","Computer is available"
                                                                                                                                                                             ,ifelse(Factor == "P17","Laptop Facilities provided"
                                                                                                                                                                                     ,ifelse(Factor == "P18","Wireless Access Provided"
                                                                                                                                                                                             ,ifelse(Factor == "P19","Print/Scan Facilities Provided"
                                                                                                                                                                                                     ,ifelse(Factor == "P20","Info/Resources meet my needs"
                                                                                                                                                                                                             ,ifelse(Factor == "P21","Online Resources are Useful"
                                                                                                                                                                                                                     ,ifelse(Factor == "P22","Course specific Resources present"
                                                                                                                                                                                                                             ,ifelse(Factor == "P23","Access Library away from Campus"
                                                                                                                                                                                                                                     ,ifelse(Factor == "P24","Library Search Engine is useful"
                                                                                                                                                                                                                                             ,ifelse(Factor == "P25","Access to Library made me succesful"
                                                                                                                                                                                                                                                     ,"Mobile devices useful to access content"))))))))))))))))))))))))))
            
            
            colnames(highs)[3] <- "Percentage"
            colnames(lows)[3] <- "Percentage"
            
            colnames(highs)[2] <- "Response"
            colnames(lows)[2] <- "Response"
            
            mycolors <- c("#67A9CF","#EF8A62","#DFDFDF","#DFDFDF","#D1E5F0","#FDDBC7","#2166AC","#B2182B")
            
            
            base_settings =
                theme_bw() +
                theme(legend.position = "none") +
                theme(plot.title = element_text(hjust = 0.5))
            
            # Plotting the diverging stacked bar chart
            
            ggplotly(ggplot() + geom_bar(data=highs, aes(x = Question, y=Percentage, fill=Response), position="stack", stat="identity") +
                         geom_bar(data=lows, aes(x = Question, y=-Percentage, fill=Response), position="stack", stat="identity") +
                         geom_hline(yintercept = 0, color =c("white")) +
                         scale_fill_manual(values = mycolors) +
                         theme_minimal() +
                         coord_flip() +
                         labs(title=mytitle, caption = "Data Source: © Singapore Management University - Library Survey Data 2018", y="Percentage of Respondents", x="Factors") +
                         theme(plot.title = element_text(size=14, hjust=0.5)) +
                         theme(axis.text.y = element_text(hjust=1)) +
                         theme(legend.position = "right") +
                         scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax)) + theme(
                             plot.background = element_rect(
                                 fill = "white",
                                 colour = "black",
                                 size = 1
                             )
                         )      , tooltip = c("x","y","Response"))
            
            
        }
    })
    
    
    # Page 2
    
    output$plot_bc_top_cnt = renderPlotly({
        
        #Data Cleaning
        #Based on Radiobutton selections made by the user
        if(input$selectM1 == 'All Libraries'){
            lib_data_stud_mean <- filter(lib_data, Campus == '1' | Campus == '2')
            if(input$selectM2 == 'All'){
                lib_data_stud_mean <- filter(lib_data_stud_mean, StudyArea == '1' | StudyArea == '2'| StudyArea == '3'| StudyArea == '4'| StudyArea == '5'| StudyArea == '6'| StudyArea == '7')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
                
            }
            else if(input$selectM2 == 'Accountancy')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '1')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Business')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '2')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Economics')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '3')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Information Systems')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '4')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Law')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '5')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Social Sciences')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '6')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Others')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '7')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
        }
        else if(input$selectM1 == 'Li Ka Shing Library'){
            lib_data_stud_mean <- filter(lib_data, Campus == '1')
            if(input$selectM2 == 'All'){
                lib_data_stud_mean <- filter(lib_data_stud_mean, StudyArea == '1' | StudyArea == '2'| StudyArea == '3'| StudyArea == '4'| StudyArea == '5'| StudyArea == '6'| StudyArea == '7')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
                
            }
            else if(input$selectM2 == 'Accountancy')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '1')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Business')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '2')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Economics')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '3')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Information Systems')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '4')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Law')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '5')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Social Sciences')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '6')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Others')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '7')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
        }
        else if(input$selectM1 == 'Kwa Geok Choo Law Library')
        {
            lib_data_stud_mean <- filter(lib_data, Campus == '2')
            if(input$selectM2 == 'All'){
                lib_data_stud_mean <- filter(lib_data_stud_mean, StudyArea == '1' | StudyArea == '2'| StudyArea == '3'| StudyArea == '4'| StudyArea == '5'| StudyArea == '6'| StudyArea == '7')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
                
            }
            else if(input$selectM2 == 'Accountancy')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '1')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Business')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '2')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Economics')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '3')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Information Systems')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '4')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Law')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '5')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Social Sciences')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '6')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
            else if(input$selectM2 == 'Others')
            {
                lib_data_stud_mean <- filter(lib_data, StudyArea == '7')
                if(input$selectM3 == 'Undergraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
                }
                else if(input$selectM3 == 'Postgraduate Students')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '6'| Position == '7')
                }
                else if(input$selectM3 == 'Faculty')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '8'| Position == '9'| Position == '10'| Position == '11')
                }
                else if(input$selectM3 == 'Non-Faculty Staff, Administrators & Others')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '12'| Position == '13' | Position == '14')
                }
                else if(input$selectM3 == 'All Roles')
                {
                    lib_data_stud_mean <- filter(lib_data_stud_mean, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
                }
            }
        }
        
        
        
        lib_data_IMP <- dplyr::select(lib_data_stud_mean,starts_with("I"),-matches("ID"))
        lib_data_PERF <- dplyr::select(lib_data_stud_mean,starts_with("P"), -matches("Position"), -ends_with("27"))
        
        Score_IMP <- data.frame(colMeans(lib_data_IMP, na.rm=TRUE))
        Score_PERF <- data.frame(colMeans(lib_data_PERF, na.rm=TRUE))
        
        names(Score_IMP) <- c("Mean Importance Score")
        names(Score_PERF) <- c("Mean Performance Score")
        
        Results<- bind_cols(ItemCategories,Score_IMP,Score_PERF)
        Results$RatioScore <- Results$`Mean Performance Score`/Results$`Mean Importance Score`
        Results$Classify <- ifelse(Results$RatioScore >=1, "Higher Performance", "Lower Performance")
        Results$Item <- as.factor(Results$Item)
        Results$Gap <- Results$`Mean Performance Score` - Results$`Mean Importance Score`
        
        
        if(input$selectM4 == 'Communication')
        {
            Results <- Results[!(Results$`Service Category`!='Communication'),]
        }
        else if(input$selectM4 == 'Facilities and Equipment')
        {
            Results <- Results[!(Results$`Service Category`!='Facilities and Equipment'),]
        }
        else if(input$selectM4 == 'Information Resources')
        {
            Results <- Results[!(Results$`Service Category`!='Information Resources'),]
        }
        else if(input$selectM4 == 'Service Delivery')
        {
            Results <- Results[!(Results$`Service Category`!='Service Delivery'),]
        }
        
        Results$Item <- factor(Results$Item, levels = Results$Item[order(Results$`Mean Importance Score`)])
        share_results <- Results
        #share_results
        # Plotting our mean performance vs Importance graph
        
        base_settings =
            theme_bw() +
            theme(legend.position = "none") +
            theme(plot.title = element_text(hjust = 0.5))
        
        Results$Item <- factor(Results$Item, levels = Results$Item[order(Results$`Mean Importance Score`)])
        
        fig3 <- share_results %>% 
            plot_ly() %>%
            add_segments( x = ~Results$`Mean Importance Score`, y = ~Item,
                          xend = ~Results$`Mean Performance Score`, yend = ~Item, 
                          color = I("gray"), showlegend = FALSE, hoverinfo = "none") %>%
            
            add_markers(
                x = ~Results$`Mean Importance Score`, y = ~Item, size=3, color=I("blue4"), marker = list(symbol = 'diamond', alpha=0.7), text=~paste(Results$Description,"<br>", Results$`Service Category`,"<br>Mean Performance Score:", round(Results$`Mean Performance Score`,2), "<br>Mean Importance Score:", round(Results$`Mean Importance Score`,2), "<br>Performance - Importance Gap:", round(Results$Gap,2)), hoverinfo="text",
                name = "Importance") %>%
            
            add_markers(
                x = ~Results$`Mean Performance Score`, y = ~Item, size=3, symbol=~Results$Classify, text=~paste(Results$Description, "<br>", Results$`Service Category`, "<br>Mean Performance Score:", round(Results$`Mean Performance Score`,2), "<br>Mean Importance Score:", round(Results$`Mean Importance Score`,2), "<br>Performance - Importance Gap:", round(Results$Gap,2)), hoverinfo="text") %>% 
            
            layout(title = list( titlefont=list(family="Arial, monospace"), text="<b>Comparison of Mean Importance and Performance Scores by Questions</b>"), legend=list(orientation="h",title=list(text="<b> Scoring Type </b>")), yaxis = list(title="Item Number", titlefont=list(family = "Arial, monospace",
                                                                                                                                                                                                                                                                                      size = 16)), xaxis = list(title="Mean Scores", titlefont=list(family = "Arial, monospace",
                                                                                                                                                                                                                                                                                                                                                    size = 16)))
        
        fig3
        #  qq<- subplot(fig3, titleX=TRUE,titleY=TRUE, margin=0.1) %>%
        #   hide_legend() %>%
        #    highlight(on="plotly_selected", off="plotly_deselect")
        #   qq
        # bscols(list(qq, filter_checkbox(id='Service Category', label="Category", sharedData=share_results, group=~Results$`Service Category`)))
        
    })
    
    
    # Page 3
    
    output$plot_heat_imp = renderPlotly({
        
        if(input$selectH1 == 'All Libraries'){
            lib_data_stud <- filter(lib_data, Campus == '1' | Campus == '2')
            
            lib_data_stud <- filter(lib_data_stud, StudyArea == '1' | StudyArea == '2'| StudyArea == '3'| StudyArea == '4'| StudyArea == '5'| StudyArea == '6'| StudyArea == '7')
            if(input$selectH3 == 'Undergraduate Students')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
            }
            else if(input$selectH3 == 'Postgraduate Students')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '6'| Position == '7')
            }
            else if(input$selectH3 == 'Faculty')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '8'| Position == '9'| Position == '10'| Position == '11')
            }
            else if(input$selectH3 == 'Non-Faculty Staff, Administrators & Others')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '12'| Position == '13' | Position == '14')
            }
            else if(input$selectH3 == 'All Roles')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
            }
        }
        else if(input$selectH1 == 'Li Ka Shing Library'){
            lib_data_stud <- filter(lib_data, Campus == '1')
            
            lib_data_stud <- filter(lib_data_stud, StudyArea == '1' | StudyArea == '2'| StudyArea == '3'| StudyArea == '4'| StudyArea == '5'| StudyArea == '6'| StudyArea == '7')
            if(input$selectH3 == 'Undergraduate Students')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
            }
            else if(input$selectH3 == 'Postgraduate Students')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '6'| Position == '7')
            }
            else if(input$selectH3 == 'Faculty')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '8'| Position == '9'| Position == '10'| Position == '11')
            }
            else if(input$selectH3 == 'Non-Faculty Staff, Administrators & Others')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '12'| Position == '13' | Position == '14')
            }
            else if(input$selectH3 == 'All Roles')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
            }
            
        }
        else if(input$selectH1 == 'Kwa Geok Choo Law Library'){
            lib_data_stud <- filter(lib_data, Campus == '2')
            
            lib_data_stud <- filter(lib_data_stud, StudyArea == '1' | StudyArea == '2'| StudyArea == '3'| StudyArea == '4'| StudyArea == '5'| StudyArea == '6'| StudyArea == '7')
            if(input$selectH3 == 'Undergraduate Students')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5')    
            }
            else if(input$selectH3 == 'Postgraduate Students')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '6'| Position == '7')
            }
            else if(input$selectH3 == 'Faculty')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '8'| Position == '9'| Position == '10'| Position == '11')
            }
            else if(input$selectH3 == 'Non-Faculty Staff, Administrators & Others')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '12'| Position == '13' | Position == '14')
            }
            else if(input$selectH3 == 'All Roles')
            {
                lib_data_stud <- filter(lib_data_stud, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
            }
            
        } 
        
        #Data Cleaning after selection
        
        if(input$selectH00 == 'Importance Ratings')
            
        {
            lib_data_stud$Comment1 <- NULL
            lib_data_stud$HowOftenL <- NULL
            lib_data_stud$HowOftenC <- NULL
            lib_data_stud$HowOftenW <- NULL
            lib_data_stud$Campus <- NULL
            lib_data_stud$Position <- NULL
            lib_data_stud$ID <- NULL
            lib_data_stud$NPS1 <- NULL
            stu_data_hm<-lib_data_stud[c(1:28)] 
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 1] <- "Accountancy"
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 2] <- "Business"
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 3] <- "Economics"
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 4] <- "Information Systems"
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 5] <- "Law"
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 6] <- "Social Sciences"
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 7] <- "Others"
            
            base_settings =
                theme_bw() +
                theme(legend.position = "none") +
                theme(plot.title = element_text(hjust = 0.5))
            
            #Code for plotting the interactive heatmap
            
            d<-na.omit(stu_data_hm)
            d2<-d[c(2:28)]
            d3 <- d2
            
            if(input$selectH0 == 'Less than 4')
            {
                d3[ ,c(2:27)][d3[ ,c(2:27)] < 4] <- 1
                d3[ ,c(2:27)][d3[ ,c(2:27)] >= 4] <- 0
            }  
            if(input$selectH0 == '4')
            {
                d3[ ,c(2:27)][d3[ ,c(2:27)] != 4] <- 0
                d3[ ,c(2:27)][d3[ ,c(2:27)] == 4] <- 1
            }
            else if(input$selectH0 == '5')
            {
                d3[ ,c(2:27)][d3[ ,c(2:27)] != 5] <- 0
                d3[ ,c(2:27)][d3[ ,c(2:27)] == 5] <- 1 
            }
            else if(input$selectH0 == '6')
            {
                d3[ ,c(2:27)][d3[ ,c(2:27)] != 6] <- 0
                d3[ ,c(2:27)][d3[ ,c(2:27)] == 6] <- 1 
            }
            else if(input$selectH0 == '7')
            {
                d3[ ,c(2:27)][d3[ ,c(2:27)] != 7] <- 0
                d3[ ,c(2:27)][d3[ ,c(2:27)] == 7] <- 1 
            }
            
            df3<-aggregate(d3[, 2:27], list(d3$StudyArea), sum)
            df4<-aggregate(d2[, 2:27], list(d2$StudyArea), length)
            d1<-df3[,c(2:27)]/df4[, c(2:27)] * 100
            d1$StudyArea <- df3$Group.1
            row.names(d1) <- d1$StudyArea
            colnames(d1) <- c("Informed about Services",
                              "Provide Useful Info",
                              "Library signage is clear",
                              "Workshops Assist in Learning/Research",
                              "Anticipates Learning/Research needs",
                              "Opening hours meets needs",
                              "Requested Books/Articles delivered",
                              "Self Service meets needs",
                              "Online enquiry meets needs",
                              "F2F enquiry meets needs",
                              "Library Shelves have items I need",
                              "Library Staff give accurate answers",
                              "Library staff are helpful",
                              "Can find quiet place to study",
                              "Can find place to work as groups",
                              "Computer is available",
                              "Laptop Facilities provided",
                              "Wireless Access Provided",
                              "Print/Scan Facilities Provided",
                              "Info/Resources meet my needs",
                              "Online Resources are Useful",
                              "Course specific Resources present",
                              "Access Library away from Campus",
                              "Library Search Engine is useful",
                              "Access to Library made me succesful",
                              "Mobile devices useful to access content","StudyArea")
            d1$StudyArea<- NULL
            wh_matrix <- data.matrix((d1))
            heatmaply(t(wh_matrix), 
                      Rowv=NA, Colv=NA,
                      seriate = "none",
                      colors = Blues,
                      fontsize_row = 10,
                      fontsize_col = 10,
                      grid_color = "black",
                      grid_lw=0.3,
                      branches_lwd = 0.6,
                      grid_size = 1,
                      xlab = "Study Area",
                      ylab = "Questions", 
                      main = "Ratings by Questions and Area of Study: Importance")
        }
        else if(input$selectH00 == 'Performance Ratings')
        {
            
            lib_data_stud$Comment1 <- NULL
            lib_data_stud$HowOftenL <- NULL
            lib_data_stud$HowOftenC <- NULL
            lib_data_stud$HowOftenW <- NULL
            lib_data_stud$Campus <- NULL
            lib_data_stud$Position <- NULL
            lib_data_stud$ID <- NULL
            lib_data_stud$NPS1 <- NULL
            lib_data_stud
            stu_data_hm<-lib_data_stud[c(1:2, 29:54)] 
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 1] <- "Accountancy"
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 2] <- "Business"
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 3] <- "Economics"
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 4] <- "Information Systems"
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 5] <- "Law"
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 6] <- "Social Sciences"
            stu_data_hm$StudyArea[stu_data_hm$StudyArea== 7] <- "Others"
            
            base_settings =
                theme_bw() +
                theme(legend.position = "none") +
                theme(plot.title = element_text(hjust = 0.5))
            
            
            #Code for plotting the interactive heatmap
            
            d<-na.omit(stu_data_hm)
            d2<-d[c(2:28)]
            d3 <- d2
            
            if(input$selectH0 == 'Less than 4')
            {
                d3[ ,c(2:27)][d3[ ,c(2:27)] < 4] <- 1
                d3[ ,c(2:27)][d3[ ,c(2:27)] >= 4] <- 0
            }  
            if(input$selectH0 == '4')
            {
                d3[ ,c(2:27)][d3[ ,c(2:27)] != 4] <- 0
                d3[ ,c(2:27)][d3[ ,c(2:27)] == 4] <- 1
            }
            else if(input$selectH0 == '5')
            {
                d3[ ,c(2:27)][d3[ ,c(2:27)] != 5] <- 0
                d3[ ,c(2:27)][d3[ ,c(2:27)] == 5] <- 1 
            }
            else if(input$selectH0 == '6')
            {
                d3[ ,c(2:27)][d3[ ,c(2:27)] != 6] <- 0
                d3[ ,c(2:27)][d3[ ,c(2:27)] == 6] <- 1 
            }
            else if(input$selectH0 == '7')
            {
                d3[ ,c(2:27)][d3[ ,c(2:27)] != 7] <- 0
                d3[ ,c(2:27)][d3[ ,c(2:27)] == 7] <- 1 
            }
            
            df3<-aggregate(d3[, 2:27], list(d3$StudyArea), sum)
            df4<-aggregate(d2[, 2:27], list(d2$StudyArea), length)
            d1<-df3[,c(2:27)]/df4[, c(2:27)] * 100
            d1$StudyArea <- df3$Group.1
            row.names(d1) <- d1$StudyArea
            colnames(d1) <- c("Informed about Services",
                              "Provide Useful Info",
                              "Library signage is clear",
                              "Workshops Assist in Learning/Research",
                              "Anticipates Learning/Research needs",
                              "Opening hours meets needs",
                              "Requested Books/Articles delivered",
                              "Self Service meets needs",
                              "Online enquiry meets needs",
                              "F2F enquiry meets needs",
                              "Library Shelves have items I need",
                              "Library Staff give accurate answers",
                              "Library staff are helpful",
                              "Can find quiet place to study",
                              "Can find place to work as groups",
                              "Computer is available",
                              "Laptop Facilities provided",
                              "Wireless Access Provided",
                              "Print/Scan Facilities Provided",
                              "Info/Resources meet my needs",
                              "Online Resources are Useful",
                              "Course specific Resources present",
                              "Access Library away from Campus",
                              "Library Search Engine is useful",
                              "Access to Library made me succesful",
                              "Mobile devices useful to access content","StudyArea")
            d1$StudyArea<- NULL
            wh_matrix <- data.matrix((d1))
            heatmaply(t(wh_matrix), 
                      Rowv=NA, Colv=NA,
                      seriate = "none",
                      colors = Blues,
                      fontsize_row = 10,
                      fontsize_col = 10,
                      grid_color = "black",
                      grid_lw=0.3,
                      branches_lwd = 0.6,
                      grid_size = 1,
                      xlab = "Study Area",
                      ylab = "Questions",
                      main = "Ratings by Questions and Area of Study: Performance")
            
        }
        
    })
    
    
    
    #Page 4 - for upset plot
    
    
    output$plot_upset = renderPlot({
        
        
        libUSR <- lib_data
        
        if(input$selectU0 == '1')
        {
            n <- 0
        }
        else if(input$selectU0 == '2')
        {
            n <- 1
        }
        else if(input$selectU0 == '3')
        {
            n <- 2
        }
        else if(input$selectU0 == '4')
        {
            n <- 3
        }
        else if(input$selectU0 == '5')
        {
            n <- 4
        }
        else if(input$selectU0 == '6')
        {
            n <- 5
        }
        else if(input$selectU0 == '7')
        {
            n <- 6
        }
        
        
        
        libUSR$I01 <- ifelse(libUSR$I01 <=n, 0, 1)
        libUSR$I02 <- ifelse(libUSR$I02 <=n, 0, 1)
        libUSR$I03 <- ifelse(libUSR$I03 <=n, 0, 1)
        libUSR$I04 <- ifelse(libUSR$I04 <=n, 0, 1)
        libUSR$I05 <- ifelse(libUSR$I05 <=n, 0, 1)
        libUSR$I06 <- ifelse(libUSR$I06 <=n, 0, 1)
        libUSR$I07 <- ifelse(libUSR$I07 <=n, 0, 1)
        libUSR$I08 <- ifelse(libUSR$I08 <=n, 0, 1)
        libUSR$I09 <- ifelse(libUSR$I09 <=n, 0, 1)
        libUSR$I10 <- ifelse(libUSR$I10 <=n, 0, 1)
        libUSR$I11 <- ifelse(libUSR$I11 <=n, 0, 1)
        libUSR$I12 <- ifelse(libUSR$I12 <=n, 0, 1)
        libUSR$I13 <- ifelse(libUSR$I13 <=n, 0, 1)
        libUSR$I14 <- ifelse(libUSR$I14 <=n, 0, 1)
        libUSR$I15 <- ifelse(libUSR$I15 <=n, 0, 1)
        libUSR$I16 <- ifelse(libUSR$I16 <=n, 0, 1)
        libUSR$I17 <- ifelse(libUSR$I17 <=n, 0, 1)
        libUSR$I18 <- ifelse(libUSR$I18 <=n, 0, 1)
        libUSR$I19 <- ifelse(libUSR$I19 <=n, 0, 1)
        libUSR$I20 <- ifelse(libUSR$I20 <=n, 0, 1)
        libUSR$I21 <- ifelse(libUSR$I21 <=n, 0, 1)
        libUSR$I22 <- ifelse(libUSR$I22 <=n, 0, 1)
        libUSR$I23 <- ifelse(libUSR$I23 <=n, 0, 1)
        libUSR$I24 <- ifelse(libUSR$I24 <=n, 0, 1)
        libUSR$I25 <- ifelse(libUSR$I25 <=n, 0, 1)
        libUSR$I26 <- ifelse(libUSR$I26 <=n, 0, 1)
        
        
        
        libUSR$P01 <- ifelse(libUSR$P01<=n, 0, 1)
        libUSR$P02 <- ifelse(libUSR$P02 <=n, 0, 1)
        libUSR$P03 <- ifelse(libUSR$P03 <=n, 0, 1)
        libUSR$P04 <- ifelse(libUSR$P04 <=n, 0, 1)
        libUSR$P05 <- ifelse(libUSR$P05 <=n, 0, 1)
        libUSR$P06 <- ifelse(libUSR$P06 <=n, 0, 1)
        libUSR$P07 <- ifelse(libUSR$P07 <=n, 0, 1)
        libUSR$P08 <- ifelse(libUSR$P08 <=n, 0, 1)
        libUSR$P09 <- ifelse(libUSR$P09 <=n, 0, 1)
        libUSR$P10 <- ifelse(libUSR$P10 <=n, 0, 1)
        libUSR$P11 <- ifelse(libUSR$P11 <=n, 0, 1)
        libUSR$P12 <- ifelse(libUSR$P12 <=n, 0, 1)
        libUSR$P13 <- ifelse(libUSR$P13 <=n, 0, 1)
        libUSR$P14 <- ifelse(libUSR$P14 <=n, 0, 1)
        libUSR$P15 <- ifelse(libUSR$P15 <=n, 0, 1)
        libUSR$P16 <- ifelse(libUSR$P16 <=n, 0, 1)
        libUSR$P17 <- ifelse(libUSR$P17 <=n, 0, 1)
        libUSR$P18 <- ifelse(libUSR$P18 <=n, 0, 1)
        libUSR$P19 <- ifelse(libUSR$P19 <=n, 0, 1)
        libUSR$P20 <- ifelse(libUSR$P20 <=n, 0, 1)
        libUSR$P21 <- ifelse(libUSR$P21 <=n, 0, 1)
        libUSR$P22 <- ifelse(libUSR$P22 <=n, 0, 1)
        libUSR$P23 <- ifelse(libUSR$P23 <=n, 0, 1)
        libUSR$P24 <- ifelse(libUSR$P24 <=n, 0, 1)
        libUSR$P25 <- ifelse(libUSR$P25 <=n, 0, 1)
        libUSR$P26 <- ifelse(libUSR$P26 <=n, 0, 1)
        
        
        
        libUSR <- dplyr::rename(libUSR, RecommendationScore=NPS1)
        libUSR[,6:57][is.na(libUSR[,6:57])] <- 0
        # remove NA so that set sizes can be calculated
        libUSR3 <- data.frame(libUSR)
        libUSR3[,4][is.na(libUSR3[,4])] <- 0
        libUSR3[,89][is.na(libUSR3[,89])] <- 0
        
        
        
        if(input$selectU1 == 'Communication')
        { 
            
            #'Communication' Service Category - Performance and Importance Scores Q1-3
            
            if(input$selectU2 == 'Importance Ratings')
            {
                
                BBB1 <- upset(libUSR3, nsets=1,  nintersects = 20, sets=c('I01','I02','I03'), keep.order=FALSE,
                              main.bar.color = "skyblue4", mainbar.y.label = "No. of Respondents", mainbar.y.max = NA,
                              sets.x.label = "No. of Respondents", set_size.show = TRUE, set_size.numbers_size = 7, 
                              set_size.scale_max = 3000, sets.bar.color = "skyblue4", point.size = 2, line.size = 1, 
                              att.pos="bottom" ,att.color = "skyblue4", show.numbers = "yes", order.by = "freq", 
                              group.by="degree", shade.color = "skyblue2", shade.alpha = 0.7, text.scale = 1.5, 
                              matrix.dot.alpha=0.8, boxplot.summary = "RecommendationScore")
                
                BBB1
            }
            else if(input$selectU2 == 'Performance Ratings')
            {
                BBB2 <- upset(libUSR3, nsets=1,  nintersects = 20, sets=c('P01','P02','P03'), keep.order=FALSE,
                              main.bar.color = "skyblue4", mainbar.y.label = "No. of Respondents", mainbar.y.max = NA,
                              sets.x.label = "No. of Respondents", set_size.show = TRUE, set_size.numbers_size = 7,
                              set_size.scale_max = 3000, sets.bar.color = "skyblue4", point.size = 2, line.size = 1,
                              att.pos="bottom" ,att.color = "skyblue4", show.numbers = "yes", order.by = "freq",
                              group.by="degree", shade.color = "skyblue2", shade.alpha = 0.7,text.scale = 1.5,
                              matrix.dot.alpha=0.8, boxplot.summary = "RecommendationScore")
                
                BBB2
            }
        }
        else if(input$selectU1 == 'Service Delivery')
        {
            
            #'Service Delivery' Service Category - Performance and Importance Scores Q4-13
            if(input$selectU2 == 'Importance Ratings')
            {
                CCC1 <- upset(libUSR3, nsets=1,  nintersects = 20, sets=c('I04','I05','I06','I07','I08','I09','I10','I11','I12', 'I13'), keep.order=FALSE, main.bar.color = "skyblue4", mainbar.y.label = "No. of Respondents",
                              mainbar.y.max = NA, sets.x.label = "No. of Respondents", set_size.show = TRUE, set_size.numbers_size = 7, set_size.scale_max = 3000, sets.bar.color = "skyblue4", point.size = 2, line.size = 1,
                              att.pos="bottom" ,att.color = "skyblue4", show.numbers = "yes", order.by = "freq", group.by="degree", shade.color = "skyblue2", shade.alpha = 0.7, text.scale = 1.5,
                              matrix.dot.alpha=0.8, boxplot.summary = "RecommendationScore")
                
                CCC1
            }
            else if(input$selectU2 == 'Performance Ratings')
            {
                CCC2 <- upset(libUSR3, nsets=1,  nintersects = 20, sets=c('P04','P05','P06','P07','P08','P09','P10','P11','P12', 'P13'), keep.order=FALSE, main.bar.color = "skyblue4", mainbar.y.label = "No. of Respondents",
                              mainbar.y.max = NA, sets.x.label = "No. of Respondents", set_size.show = TRUE, set_size.numbers_size = 7, set_size.scale_max = 3000, sets.bar.color = "skyblue4", point.size = 2, line.size = 1,
                              att.pos="bottom" ,att.color = "skyblue4", show.numbers = "yes", order.by = "freq", group.by="degree", shade.color = "skyblue2", shade.alpha = 0.7, text.scale = 1.5,
                              matrix.dot.alpha=0.8, boxplot.summary = "RecommendationScore")
                
                CCC2
            }
        }
        else if(input$selectU1 == 'Facilities and Equipment')
        {
            #'Facilities and Equipment' Service Category - Performance and Importance Scores Q14-19
            
            if(input$selectU2 == 'Importance Ratings')
            {
                DDD1 <- upset(libUSR3, nsets=1,  nintersects = 20, sets=c('I14','I15','I16','I17','I18','I19'), keep.order=FALSE, main.bar.color = "skyblue4",
                              mainbar.y.label = "No. of Respondents", mainbar.y.max = NA, sets.x.label = "No. of Respondents", set_size.show = TRUE, set_size.numbers_size = 7,
                              set_size.scale_max = 3000, sets.bar.color = "skyblue4", point.size = 2, line.size = 1, att.pos="bottom" ,att.color = "skyblue4", show.numbers = "yes", order.by = "freq",
                              group.by="degree", shade.color = "skyblue2", shade.alpha = 0.7,text.scale = 1.5,  matrix.dot.alpha=0.8, boxplot.summary = "RecommendationScore")
                
                DDD1
            }
            else if(input$selectU2 == 'Performance Ratings')
            {
                DDD2 <- upset(libUSR3, nsets=1,  nintersects = 20, sets=c('P14','P15','P16','P17','P18','P19'), keep.order=FALSE, main.bar.color = "skyblue4",
                              mainbar.y.label = "No. of Respondents", mainbar.y.max = NA, sets.x.label = "No. of Respondents", set_size.show = TRUE, set_size.numbers_size = 7,
                              set_size.scale_max = 3000, sets.bar.color = "skyblue4", point.size = 2, line.size = 1, att.pos="bottom" ,att.color = "skyblue4", show.numbers = "yes",
                              order.by = "freq", group.by="degree", shade.color = "skyblue2", shade.alpha = 0.7,text.scale = 1.5,  matrix.dot.alpha=0.8, boxplot.summary = "RecommendationScore")
                
                DDD2
            }
        }
        else if(input$selectU1 == 'Information Resources')
        {
            #'Information Resources' Service Category - Performance and Importance Scores Q20-26
            if(input$selectU2 == 'Importance Ratings')
            {
                EEE1 <- upset(libUSR3, nsets=1,  nintersects = 20, sets=c('I20','I21','I22','I23','I24','I25','I26'), keep.order=FALSE, main.bar.color = "skyblue4",
                              mainbar.y.label = "No. of Respondents", mainbar.y.max = NA, sets.x.label = "No. of Respondents", set_size.show = TRUE, set_size.numbers_size = 7, 
                              set_size.scale_max = 3000, sets.bar.color = "skyblue4", point.size = 2, line.size = 1, att.pos="bottom" ,att.color = "skyblue4", show.numbers = "yes", 
                              order.by = "freq", group.by="degree", shade.color = "skyblue2", shade.alpha = 0.7, text.scale = 1.5, matrix.dot.alpha=0.8, boxplot.summary = "RecommendationScore")
                
                EEE1
            }
            else if(input$selectU2 == 'Performance Ratings')
            {
                EEE2 <- upset(libUSR3, nsets=1,  nintersects = 20, sets=c('P20','P21','P22','P23','P24','P25','P26'), keep.order=FALSE, main.bar.color = "skyblue4",
                              mainbar.y.label = "No. of Respondents", mainbar.y.max = NA, sets.x.label = "No. of Respondents", set_size.show = TRUE, set_size.numbers_size = 7,
                              set_size.scale_max = 3000, sets.bar.color = "skyblue4", point.size = 2, line.size = 1, att.pos="bottom" ,att.color = "skyblue4", show.numbers = "yes", order.by = "freq",
                              group.by="degree", shade.color = "skyblue2", shade.alpha = 0.7,text.scale = 1.5,  matrix.dot.alpha=0.8, boxplot.summary = "RecommendationScore")
                
                EEE2
            }
        }
    })
    
    
    #Page 5
    
    output$plot_pca = renderPlot({
        
        #Selection based on input
        
        stu_data_h<- filter(lib_data, Position == '1' | Position == '2'| Position == '3'| Position == '4' | Position == '5' | Position == '6'| Position == '7'| Position == '8' | Position == '9'| Position == '10'| Position == '11'| Position == '12' | Position == '13'| Position == '14')
        
        if(input$selectP1 == 'Importance Ratings')
        {
            stu_data_hm <- stu_data_h %>% dplyr::select(ResponseID,StudyArea,starts_with("I"))
            stu_data_hm$ID <- NULL
            d1<- stu_data_hm[c(3:28)]
            d1<-na.omit(d1)
            c1<-cor(d1)
            g1<-corrplot(c1)
        }
        else if(input$selectP1 == 'Performance Ratings')
        {
            stu_data_hm <- stu_data_h %>% dplyr::select(ResponseID,StudyArea,starts_with("P"))
            stu_data_hm$Position <- NULL
            stu_data_hm$P27 <- NULL
            stu_data_hm$ID <- NULL
            d1<- stu_data_hm[c(3:28)]
            d1<-na.omit(d1)
            c1<-cor(d1)
            g1<-corrplot(c1)
        }
        
        if(input$selectP2 == 'Accountancy')
        {
            stu_data_hm <- filter(stu_data_hm, StudyArea == '1')
        }
        else if(input$selectP2 == 'Business')
        {
            stu_data_hm <- filter(stu_data_hm, StudyArea == '2')
        }
        else if(input$selectP2 == 'Economics')
        {
            stu_data_hm <- filter(stu_data_hm, StudyArea == '3')
        }
        else if(input$selectP2 == 'Information Systems')
        {
            stu_data_hm <- filter(stu_data_hm, StudyArea == '4')
        }
        else if(input$selectP2 == 'Law')
        {
            stu_data_hm <- filter(stu_data_hm, StudyArea == '5')
        }
        else if(input$selectP2 == 'Social Sciences')
        {
            stu_data_hm <- filter(stu_data_hm, StudyArea == '6')
        }
        else if(input$selectP2 == 'Others')
        {
            stu_data_hm <- filter(stu_data_hm, StudyArea == '7')
        }
        
        d<-na.omit(stu_data_hm)
        
        #Data Cleaning after selection
        res.pca <- PCA(d[, 3:28], graph = FALSE)
        eig.val <- get_eigenvalue(res.pca)
        eig.val
        if(input$selectP3 == 'Scree Plot')
        {
            base_settings =
                theme_bw() +
                theme(legend.position = "none") +
                theme(plot.title = element_text(hjust = 0.5))  
            scree <-  fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
            scree
        }
        else if(input$selectP3 == 'Corr Plot')
        {
            base_settings =
                theme_bw() +
                theme(legend.position = "none") +
                theme(plot.title = element_text(hjust = 0.5)) 
            var <- get_pca_var(res.pca)
            corr <- g1
            
        }
        
        #Code for plotting the pca cluster map
        
        else if(input$selectP3 == 'Loading Plot')
        {
            if(input$selectP4 == 'Dim 1 vs Dim 2')
            {
                base_settings =
                    theme_bw() +
                    theme(legend.position = "none") +
                    theme(plot.title = element_text(hjust = 0.5))    
                fviz_pca_var(res.pca, col.var = "cos2", axes = 1:2,
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
                ) 
            }
            else if(input$selectP4 == 'Dim 2 vs Dim 3')
            {
                base_settings =
                    theme_bw() +
                    theme(legend.position = "none") +
                    theme(plot.title = element_text(hjust = 0.5))    
                fviz_pca_var(res.pca, col.var = "cos2", axes = 2:3,
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
                ) 
            }
            else if(input$selectP4 == 'Dim 3 vs Dim 4')
            {
                base_settings =
                    theme_bw() +
                    theme(legend.position = "none") +
                    theme(plot.title = element_text(hjust = 0.5))    
                fviz_pca_var(res.pca, col.var = "cos2", axes = 3:4,
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
                ) 
            }
            else if(input$selectP4 == 'Dim 4 vs Dim 5')
            {
                base_settings =
                    theme_bw() +
                    theme(legend.position = "none") +
                    theme(plot.title = element_text(hjust = 0.5))    
                fviz_pca_var(res.pca, col.var = "cos2", axes = 4:5,
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
                ) 
            }
        }
        
    })
    
    # Page 6
    
    output$plot_anova = renderPlotly({
        
        d1 <- lib_data
        
        d1$cat <- ifelse(d1$Position==1 | d1$Position== 2 |d1$Position== 3 | d1$Position== 4, "Undergrads",
                         ifelse(d1$Position==5 , "Exchange",
                                ifelse(d1$Position== 6 | d1$Position== 7, "PostGrads",
                                       ifelse(d1$Position== 8 | d1$Position== 9| d1$Position== 10| d1$Position== 11| d1$Position== 12, "Faculty",
                                              ifelse(d1$Position== 13 |d1$Position== 14,"Others",
                                                     NA  )))) )
        
        if(input$selectA1 == 'Importance Ratings')
        {
            d2<-  d1 %>% dplyr::select(ResponseID,cat,starts_with("I"),-ID)
            d2<-d2[-c(5),] # Dropping the NA value
            
            #Communications
            
            dc<-d2 %>% dplyr::select(ResponseID,cat,I01,I02,I03)
            dc$Mean_Comm<-rowMeans(dc[,3:5],na.rm =TRUE)
            dcbox<-dc[,c(1,2,6)]
            dcbox <- na.omit(dcbox)
            
            res.aov <- dcbox %>% anova_test(Mean_Comm ~ cat)
            res.aov
            
            DT::datatable(
                head(res.aov), extensions = 'FixedColumns',
                options = 
                    list(dom = 't',
                         columnDefs = list(list(width = '30px', targets = c(1, ncol(res.aov)))),
                         scrollX = TRUE,
                         scrollCollapse = TRUE)
            )
            
            pwc <- dcbox %>% tukey_hsd(Mean_Comm ~ cat)
            pwc
            
            DT::datatable(
                head(pwc), extensions = 'FixedColumns',
                options = 
                    list(dom = 't',
                         columnDefs = list(list(width = '300px', targets = c(1, ncol(pwc)))),
                         scrollX = FALSE,
                         scrollCollapse = FALSE)
            )
            
            #Serice Delivery
            
            ds<-d2 %>% dplyr::select(ResponseID,cat,I04,I05,I06,I07,I08,I09,I10,I11,I12,I13)
            ds$Mean_Serv<-rowMeans(ds[,3:12],na.rm =TRUE)
            dsbox<-ds[,c(1,2,13)]
            dsbox <- na.omit(dsbox)
            
            #Facilities and Equipment
            
            dfac<-d2 %>% dplyr::select(ResponseID,cat,I14,I15,I16,I17,I18,I19,I20)
            dfac$Mean_Fac<-rowMeans(dfac[,3:9],na.rm =TRUE)
            dfacbox<-dfac[,c(1,2,10)]
            dfacbox <- na.omit(dfacbox)
            
            #Information Resources
            
            dinf<-d2 %>% dplyr::select(ResponseID,cat,I21,I22,I23,I24,I25,I26)
            dinf$Mean_Inf<-rowMeans(dinf[,3:8],na.rm =TRUE)
            dinfbox<-dinf[,c(1,2,9)]
            dinfbox <- na.omit(dinfbox)
            
        }
        else if(input$selectA1 == 'Performance Ratings')
        {
            d2<-  d1 %>% dplyr::select(ResponseID,cat,starts_with("P"),-ID)
            d2<-d2[-c(5),] # Dropping the NA value
            
            #Communications
            
            dc<-d2 %>% dplyr::select(ResponseID,cat,P01,P02,P03)
            dc$Mean_Comm<-rowMeans(dc[,3:5],na.rm =TRUE)
            dcbox<-dc[,c(1,2,6)]
            dcbox <- na.omit(dcbox)
            
            #Service Delivery
            
            ds<-d2 %>% dplyr::select(ResponseID,cat,P04,P05,P06,P07,P08,P09,P10,P11,P12,P13)
            ds$Mean_Serv<-rowMeans(ds[,3:12],na.rm =TRUE)
            dsbox<-ds[,c(1,2,13)]
            dsbox <- na.omit(dsbox)
            
            #Facilities and Equipment
            
            dfac<-d2 %>% dplyr::select(ResponseID,cat,P14,P15,P16,P17,P18,P19,P20)
            dfac$Mean_Fac<-rowMeans(dfac[,3:9],na.rm =TRUE)
            dfacbox<-dfac[,c(1,2,10)]
            dfacbox <- na.omit(dfacbox)
            
            #Information Resources
            
            dinf<-d2 %>% dplyr::select(ResponseID,cat,P21,P22,P23,P24,P25,P26)
            dinf$Mean_Inf<-rowMeans(dinf[,3:8],na.rm =TRUE)
            dinfbox<-dinf[,c(1,2,9)]
            dinfbox <- na.omit(dinfbox)
        }
        
        if(input$selectA2 == 'Communication')
        {
            
            if(input$selectA3 == 'QQ-Plot')
            {
                n1<- ggqqplot(dcbox, "Mean_Comm", facet.by = "cat",conf.int.level = 0.95, title = "Normality test for Communication")
                n1
            }
            else if(input$selectA3 == 'Anova Graph')
            {
                fig1 <- dcbox %>%
                    plot_ly(
                        x = ~cat,
                        y = ~Mean_Comm,
                        split = ~cat,
                        type = 'violin',
                        box = list(
                            visible = T
                        ),
                        opacity = 0.6,
                        meanline = list(
                            visible = T
                        )
                    ) 
                
                fig1 <- fig1 %>%
                    layout(title = "Difference in Mean Across Respondents: Communication",
                           xaxis = list(
                               title = "Respondent Role"
                           ),
                           yaxis = list(
                               title = "Mean Ratings",
                               zeroline = T
                           )
                    )
                
                fig1
            }
        }
        else if(input$selectA2 == 'Service Delivery')
        {
            if(input$selectA3 == 'QQ-Plot')
            {
                n2<- ggqqplot(dsbox, "Mean_Serv", facet.by = "cat",conf.int.level = 0.95, title = "Normality test for Service Delivery")
                n2
            }
            else if(input$selectA3 == 'Anova Graph')
            {
                fig2 <- dsbox %>%
                    plot_ly(
                        x = ~cat,
                        y = ~Mean_Serv,
                        split = ~cat,
                        type = 'violin',
                        box = list(
                            visible = T
                        ),
                        opacity = 0.6,
                        meanline = list(
                            visible = T
                        )
                    ) 
                
                fig2 <- fig2 %>%
                    layout(title = "Difference in Mean Across Respondents: Service Delivery",
                           xaxis = list(
                               title = "Respondent Role"
                           ),
                           yaxis = list(
                               title = "Mean Ratings",
                               zeroline = T
                           )
                    )
                
                fig2
            }
        }
        else if(input$selectA2 == 'Facilities and Equipment')
        {
            if(input$selectA3 == 'QQ-Plot')
            {
                n3<- ggqqplot(dfacbox, "Mean_Fac", facet.by = "cat",conf.int.level = 0.95, title = "Normality test for Facilities and Equipment")
                n3
            }
            else if(input$selectA3 == 'Anova Graph')
            {
                fig3 <- dfacbox %>%
                    plot_ly(
                        x = ~cat,
                        y = ~Mean_Fac,
                        split = ~cat,
                        type = 'violin',
                        box = list(
                            visible = T
                        ),
                        opacity = 0.6,
                        meanline = list(
                            visible = T
                        )
                    ) 
                
                fig3 <- fig3 %>%
                    layout(title = "Difference in Mean Across Respondents: Facilities & Equipment",
                           xaxis = list(
                               title = "Respondent Role"
                           ),
                           yaxis = list(
                               title = "Mean Ratings",
                               zeroline = T
                           )
                    )
                
                fig3
            }
        }
        else if(input$selectA2 == 'Information Resources')
        {
            if(input$selectA3 == 'QQ-Plot')
            {
                n4<- ggqqplot(dinfbox, "Mean_Inf", facet.by = "cat",conf.int.level = 0.95, title = "Normality test for Information Resources")
                n4
            }
            else if(input$selectA3 == 'Anova Graph')
            {
                fig4 <- dinfbox %>%
                    plot_ly(
                        x = ~cat,
                        y = ~Mean_Inf,
                        split = ~cat,
                        type = 'violin',
                        box = list(
                            visible = T
                        ),
                        opacity = 0.6,
                        meanline = list(
                            visible = T
                        )
                    ) 
                
                fig4 <- fig4 %>%
                    layout(title = "Difference in Mean Across Respondents: Information Resources",
                           xaxis = list(
                               title = "Respondent Role"
                           ),
                           yaxis = list(
                               title = "Mean Ratings",
                               zeroline = T
                           )
                    )
                
                fig4
            }
        }
        
        
    })
    
    output$plot_tukey = DT::renderDataTable({
        
        d1 <- lib_data
        
        d1$cat <- ifelse(d1$Position==1 | d1$Position== 2 |d1$Position== 3 | d1$Position== 4, "Undergrads",
                         ifelse(d1$Position==5 , "Exchange",
                                ifelse(d1$Position== 6 | d1$Position== 7, "PostGrads",
                                       ifelse(d1$Position== 8 | d1$Position== 9| d1$Position== 10| d1$Position== 11| d1$Position== 12, "Faculty",
                                              ifelse(d1$Position== 13 |d1$Position== 14,"Others",
                                                     NA  )))) )
        
        if(input$selectA1 == 'Importance Ratings')
        {
            d2<-  d1 %>% dplyr::select(ResponseID,cat,starts_with("I"),-ID)
            d2<-d2[-c(5),] # Dropping the NA value
            
            if(input$selectA2 == 'Communication')
            {
                #Communications
                
                dc<-d2 %>% dplyr::select(ResponseID,cat,I01,I02,I03)
                dc$Mean_Comm<-rowMeans(dc[,3:5],na.rm =TRUE)
                dcbox<-dc[,c(1,2,6)]
                dcbox <- na.omit(dcbox)
                
                res.aov <- dcbox %>% anova_test(Mean_Comm ~ cat)
                
                pwc <- dcbox %>% tukey_hsd(Mean_Comm ~ cat)
                pwc
                
                DT::datatable(
                    head(pwc), extensions = 'FixedColumns',
                    options = 
                        list(dom = 't',
                             columnDefs = list(list(width = '300px', targets = c(1, ncol(pwc)))),
                             scrollX = TRUE,
                             scrollCollapse = FALSE)
                )
            }
            else if(input$selectA2 == 'Service Delivery')
            {
                #Service Delivery
                
                ds<-d2 %>% dplyr::select(ResponseID,cat,I04,I05,I06,I07,I08,I09,I10,I11,I12,I13)
                ds$Mean_Serv<-rowMeans(ds[,3:12],na.rm =TRUE)
                dsbox<-ds[,c(1,2,13)]
                dsbox <- na.omit(dsbox)
                
                res.aov <- dsbox %>% anova_test(Mean_Serv ~ cat)
                
                pwc <- dsbox %>% tukey_hsd(Mean_Serv ~ cat)
                pwc
                
                DT::datatable(
                    head(pwc), extensions = 'FixedColumns',
                    options = 
                        list(dom = 't',
                             columnDefs = list(list(width = '300px', targets = c(1, ncol(pwc)))),
                             scrollX = TRUE,
                             scrollCollapse = FALSE)
                )
            }
            else if(input$selectA2 == 'Facilities and Equipment')
            {
                #Facilities and Equipment
                
                dfac<-d2 %>% dplyr::select(ResponseID,cat,I14,I15,I16,I17,I18,I19,I20)
                dfac$Mean_Fac<-rowMeans(dfac[,3:9],na.rm =TRUE)
                dfacbox<-dfac[,c(1,2,10)]
                dfacbox <- na.omit(dfacbox)
                
                res.aov <- dfacbox %>% anova_test(Mean_Fac ~ cat)
                
                pwc <- dfacbox %>% tukey_hsd(Mean_Fac ~ cat)
                pwc
                
                DT::datatable(
                    head(pwc), extensions = 'FixedColumns',
                    options = 
                        list(dom = 't',
                             columnDefs = list(list(width = '300px', targets = c(1, ncol(pwc)))),
                             scrollX = TRUE,
                             scrollCollapse = FALSE)
                )
            }
            else if(input$selectA2 == 'Information Resources')
            {
                #Information Resources
                
                dinf<-d2 %>% dplyr::select(ResponseID,cat,I21,I22,I23,I24,I25,I26)
                dinf$Mean_Inf<-rowMeans(dinf[,3:8],na.rm =TRUE)
                dinfbox<-dinf[,c(1,2,9)]
                dinfbox <- na.omit(dinfbox)
                
                res.aov <- dinfbox %>% anova_test(Mean_Inf ~ cat)
                
                pwc <- dinfbox %>% tukey_hsd(Mean_Inf ~ cat)
                pwc
                
                DT::datatable(
                    head(pwc), extensions = 'FixedColumns',
                    options = 
                        list(dom = 't',
                             columnDefs = list(list(width = '300px', targets = c(1, ncol(pwc)))),
                             scrollX = TRUE,
                             scrollCollapse = FALSE)
                )
            }
        }
        else if(input$selectA1 == 'Performance Ratings')
        {
            d2<-  d1 %>% dplyr::select(ResponseID,cat,starts_with("P"),-ID)
            d2<-d2[-c(5),] # Dropping the NA value
            
            if(input$selectA2 == 'Communication')
            {
                #Communications
                
                dc<-d2 %>% dplyr::select(ResponseID,cat,P01,P02,P03)
                dc$Mean_Comm<-rowMeans(dc[,3:5],na.rm =TRUE)
                dcbox<-dc[,c(1,2,6)]
                dcbox <- na.omit(dcbox)
                
                res.aov <- dcbox %>% anova_test(Mean_Comm ~ cat)
                
                pwc <- dcbox %>% tukey_hsd(Mean_Comm ~ cat)
                pwc
                
                DT::datatable(
                    head(pwc), extensions = 'FixedColumns',
                    options = 
                        list(dom = 't',
                             columnDefs = list(list(width = '300px', targets = c(1, ncol(pwc)))),
                             scrollX = TRUE,
                             scrollCollapse = FALSE)
                )
            }
            else if(input$selectA2 == 'Service Delivery')
            {
                #Serice Delivery
                
                ds<-d2 %>% dplyr::select(ResponseID,cat,P04,P05,P06,P07,P08,P09,P10,P11,P12,P13)
                ds$Mean_Serv<-rowMeans(ds[,3:12],na.rm =TRUE)
                dsbox<-ds[,c(1,2,13)]
                dsbox <- na.omit(dsbox)
                
                res.aov <- dsbox %>% anova_test(Mean_Serv ~ cat)
                
                pwc <- dsbox %>% tukey_hsd(Mean_Serv ~ cat)
                pwc
                
                DT::datatable(
                    head(pwc), extensions = 'FixedColumns',
                    options = 
                        list(dom = 't',
                             columnDefs = list(list(width = '300px', targets = c(1, ncol(pwc)))),
                             scrollX = TRUE,
                             scrollCollapse = FALSE)
                )
            }
            else if(input$selectA2 == 'Facilities and Equipment')
            {
                #Facilities and Equipment
                
                dfac<-d2 %>% dplyr::select(ResponseID,cat,P14,P15,P16,P17,P18,P19,P20)
                dfac$Mean_Fac<-rowMeans(dfac[,3:9],na.rm =TRUE)
                dfacbox<-dfac[,c(1,2,10)]
                dfacbox <- na.omit(dfacbox)
                
                res.aov <- dfacbox %>% anova_test(Mean_Fac ~ cat)
                
                pwc <- dfacbox %>% tukey_hsd(Mean_Fac ~ cat)
                pwc
                
                DT::datatable(
                    head(pwc), extensions = 'FixedColumns',
                    options = 
                        list(dom = 't',
                             columnDefs = list(list(width = '300px', targets = c(1, ncol(pwc)))),
                             scrollX = TRUE,
                             scrollCollapse = FALSE)
                )
            }
            else if(input$selectA2 == 'Information Resources')
            {
                #Information Resources
                
                dinf<-d2 %>% dplyr::select(ResponseID,cat,P21,P22,P23,P24,P25,P26)
                dinf$Mean_Inf<-rowMeans(dinf[,3:8],na.rm =TRUE)
                dinfbox<-dinf[,c(1,2,9)]
                dinfbox <- na.omit(dinfbox)
                
                res.aov <- dinfbox %>% anova_test(Mean_Inf ~ cat)
                
                pwc <- dinfbox %>% tukey_hsd(Mean_Inf ~ cat)
                pwc
                
                DT::datatable(
                    head(pwc), extensions = 'FixedColumns',
                    options = 
                        list(dom = 't',
                             columnDefs = list(list(width = '300px', targets = c(1, ncol(pwc)))),
                             scrollX = TRUE,
                             scrollCollapse = FALSE)
                )
            }
        }
        
    })
    
    
}
shinyApp (ui=ui, server=server)

