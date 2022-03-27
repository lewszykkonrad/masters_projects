
#shiny::runApp(appDir='D:/UW/Adv_R/Project', launch.browser = TRUE)
#"C:/Users/Admin/Documents"
library(shinythemes)
library(shinydashboard)
?icon()
dashboardPage(
  
  
  
  dashboardHeader( title="Cycling App by KL and AD"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName ="About", icon = icon("book")),
      #### ANALYSIS MENU ####
      menuItem("Analysis", tabName = "Analysis", icon = icon("dashboard")),
      
      #### COMPARISON MENU ####
      menuItem("Comparison", tabName = "Comparison", icon = icon("not-equal")),
      menuItem("Health" ,tabName = "Health", icon = icon("heart"))
    )
  
      
    ),
  
    dashboardBody(
      tabItems(
        tabItem("About",
                h1("welcome")),
        tabItem("Analysis",
                fluidRow(
                  column(3,fileInput(inputId = "file_choice",label = "Choose file from disk:", multiple = FALSE, accept = c(".fit"),
                                     width = "300px",buttonLabel = icon("folder"),placeholder = "Search...")
                         ),
                  column(3,
                         sliderInput(inputId = "distance", "Select section of the ride for analysis",
                                     min = 0, max = 100, value = c(0,100),
                                     step = 1)
                         ),
                  column(3,
                          selectInput(inputId = "xaxis",label = "X axis", choices = c("distance","speed","power","timestamp","altitude","cadence","power","temperature")),
                          selectInput(inputId = "yaxis",label = "Y axis", choices = c("speed","distance","power","timestamp","altitude","cadence","power","temperature"))),
                  column(3,
                         fileInput(inputId = "myFile",label = "Choose photo from disk:", multiple = FALSE, accept = c(".JPG"),
                            width = "300px",buttonLabel = icon("camera"),placeholder = "Search..."))
                ),
                

                
                tabsetPanel(
                  tabPanel(
                    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"
                    ),
                      title = "statistics",
                           fluidRow(
                             tags$style(".topimg {
                            margin-left:15px;
                            margin-right:10px;
                            margin-bottom:45px;
                          }"),
                             div(class="topimg",id = "image-container", style = "display:flexbox")),
                           #div(id = "image-container", style = "display:flexbox")),
                           h1("Basic Statistics", style = "font-weight: bold;"),
                           fluidRow(
                             shinycssloaders::withSpinner(valueBoxOutput("avg_speed_box",width=2), hide.ui = FALSE),
                             valueBoxOutput("avg_heart_rate",width=2),
                             valueBoxOutput("distance_overall",width=2),
                             valueBoxOutput("avg_cadence",width=2),
                             valueBoxOutput("time_overall",width=2),
                             valueBoxOutput("avg_power",width=2)
                           ),
                           h1("Heart rate stats", style = "font-weight: bold"),
                           fluidRow(
                             shinycssloaders::withSpinner(valueBoxOutput("avg_hr",width=2), hide.ui = FALSE),
                                    valueBoxOutput("max_hr",width=2),
                                    valueBoxOutput("min_hr",width=2)),
                           h1("Time & speed stats", style = "font-weight: bold"),
                           fluidRow(
                             shinycssloaders::withSpinner(valueBoxOutput("total_time",width=2), hide.ui = FALSE),
                                    valueBoxOutput("avg_speed2",width=2),
                                    valueBoxOutput("max_speed2",width=2)),
                           h1("Power & cadence stats", style = "font-weight: bold"),
                           fluidRow(
                             shinycssloaders::withSpinner(valueBoxOutput("avg_power2",width=2), hide.ui = FALSE),
                                    valueBoxOutput("max_power",width=2),
                                    valueBoxOutput("max_Wkg_power",width=2),
                                    valueBoxOutput("avg_cadence2",width=2),
                                    valueBoxOutput("max_cadence",width=2)),
                           h1("Altitude & temperature stats", style = "font-weight: bold"),
                           fluidRow(
                             shinycssloaders::withSpinner(valueBoxOutput("max_altitude",width=2), hide.ui = FALSE),
                                    valueBoxOutput("min_altitude",width=2),
                                    valueBoxOutput("max_temp",width=2),
                                    valueBoxOutput("min_temp",width=2),
                                    valueBoxOutput("avg_temp",width=2))

                           ),
                  tabPanel(title = "visualization",
                           shinycssloaders::withSpinner(plotlyOutput('plot1'), hide.ui = FALSE),
                           shinycssloaders::withSpinner(leafletOutput("map"), hide.ui = FALSE))
                )
                ),
        tabItem("Comparison",
                fluidRow(
                  column(4,fileInput(inputId = "file_choice_2",label = "Choose file for comparison", multiple = FALSE, accept = c(".fit"),
                          width = "300px",buttonLabel = icon("folder"),placeholder = "Search...")),
                column(4, selectInput(inputId = "variable_choice", label = "choose variable for visualization",
                            choices = c("power", "cadence", "speed")))
                ),
                tabsetPanel(
                  tabPanel(title = "statistics",
                           h2("speed", style = "font-weight: bold"),
                           shinycssloaders::withSpinner(valueBoxOutput("avg_speed_box_1.1", width = 6), hide.ui = FALSE),
                           shinycssloaders::withSpinner(valueBoxOutput("avg_speed_box_2", width = 6), hide.ui = FALSE),
                           h2("heart rate", style = "font-weight: bold"),
                           valueBoxOutput("avg_heart_rate_1.2", width = 6),
                           valueBoxOutput("avg_heart_rate_2", width = 6),
                           h2("distance", style = "font-weight: bold"),
                           valueBoxOutput("distance_overall_1.3", width = 6),
                           valueBoxOutput("distance_overall_2", width = 6),
                           h2("cadence", style = "font-weight: bold"),
                           valueBoxOutput("avg_cadence_1.4", width = 6),
                           valueBoxOutput("avg_cadence_2", width = 6),
                           h2("elapsed time", style = "font-weight: bold"),
                           valueBoxOutput("time_overall_1.6", width = 6),
                           valueBoxOutput("time_overall_2", width = 6),
                           h2("power", style = "font-weight: bold"),
                           valueBoxOutput("avg_power_1.5", width = 6),
                           valueBoxOutput("avg_power_2", width = 6)
                           
                           # box(title = "workout 1",column(4,
                           #        valueBoxOutput("avg_speed_box_1.1", width = 12),
                           #        valueBoxOutput("avg_heart_rate_1.2", width = 12),
                           #        valueBoxOutput("distance_overall_1.3", width = 12),
                           #        valueBoxOutput("avg_cadence_1.4", width = 12),
                           #        valueBoxOutput("time_overall_1.6", width = 12),
                           #        valueBoxOutput("avg_power_1.5", width = 12)
                           #        )),
                           # box(column(4,
                           #        valueBoxOutput("avg_speed_box_2", width = 6),
                           #        valueBoxOutput("avg_heart_rate_2", width = 6),
                           #        valueBoxOutput("distance_overall_2", width = 6),
                           #        valueBoxOutput("avg_cadence_2", width = 6),
                           #        valueBoxOutput("time_overall_2", width = 6),
                           #        valueBoxOutput("avg_power_2", width = 6)))
                           
                           # flipBox(id = "flipbox_1", trigger = "hover", width = 6,
                           #         front = div(
                           #           class = "text-center",
                           #           h2("avg speed")
                           #         ),
                           #         back = textOutput("first_comparison")
                           #         )
                           ),
                  tabPanel(title = "visualization",
                           plotlyOutput("comparison_plot")
                           )
                )
                ),
        tabItem("Health",
                sidebarPanel(width = 2, 
                             radioButtons(
                               inputId = "rdb",
                               label = "Excluding choices:",
                               selected = "foot",
                               choiceNames = list(icon("male"), icon("female")),
                               choiceValues = list("male", "female"),
                               inline = FALSE),
                             
                             numericInput(
                               inputId = "num",
                               label = "Enter your age:",
                               value = 0,
                               min = 1,
                               max = 1000,
                               step = 1,
                               width = "300px"
                             ),
                             
                             numericInput(
                               inputId = "weight",
                               label = "Enter your weight in kg:",
                               value = 0,
                               min = 1,
                               max = 200,
                               step = 1,
                               width = "300px"
                             ),
                             numericInput(
                               inputId = "height",
                               label = "Enter your height in cm:",
                               value = 0,
                               min = 1,
                               max = 1000,
                               step = 1,
                               width = "300px"
                             )
                ),
                mainPanel( h2("Calories", align= "center"),
                           h4("An ideal daily intake of calories varies depending on age, metabolism and levels of physical activity, among other things. Below you can see your estimated calories intake needed for healthy lifestyle (green box). Remember that due to your activity you should additionally consume amount of calories given in blue box ", align= "center"),
                          fluidRow(valueBoxOutput("calories",width=6),valueBoxOutput("BMR",width=6)),
                          h2("Heart zones from your trening",align="center"),
                          h4("Heart rate zones, or HR zones, are a way to monitor how hard you’re training. There are five heart rate zones based on the intensity of training with regard to your maximum heart rate.", align="center"),
                          
                          p(strong("HEART RATE ZONE 1: 50–60% OF HRMAX")," - This is the very low intensity zone. Training at this intensity will boost your recovery and get you ready to train in the higher heart rate zones."),
                          p(strong("HEART RATE ZONE 2: 60–70% OF HRMAX")," - This is the zone that improves your general endurance: your body will get better at oxidizing – burning – fat and your muscular fitness will increase along with your capillary density.
"),
                          p(strong("HEART RATE ZONE 1: 70–80% OF HRMAX")," - Working out in heart rate zone 3 is especially effective for improving the efficiency of blood circulation in the heart and skeletal muscles. This is the zone in which that pesky lactic acid starts building up in your bloodstream."),
                          p(strong("HEART RATE ZONE 1: 80–90% OF HRMAX")," - If you train at this intensity, you’ll improve your speed endurance. Your body will get better at using carbohydrates for energy and you’ll be able to withstand higher levels of lactic acid in your blood for longer."),
                          p(strong("HEART RATE ZONE 1: 90–100% OF HRMAX")," - Heart rate zone 5 is your maximal effort. Your heart and your blood and respiratory system will be working at their maximal capacity. Lactic acid will build up in your blood and after a few minutes you won’t be able to continue at this intensity."),
                          plotlyOutput("plot_heart_rate")
                )
                )
      ),
      tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tags$style(HTML(".sidebar-menu li a { font-size: 15px; font-weight: bold}"))
    )
  )



