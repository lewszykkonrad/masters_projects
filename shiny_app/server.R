
library(FITfileR)
library(shiny)
library(ggplot2)
library(stringr)
library(dygraphs)
library(dplyr)
library(leaflet)
library(tidyverse)
library(plotly)


library(shinythemes)
library(shinydashboard)
library(Rcpp)
library(DT)
library(fontawesome)
library(shinycssloaders)
library(shinydashboardPlus)


#### function for reading the fit file and converting it to a dataframe
fit_processor <- function(input_file) {

  file <- readFitFile(input_file)
  file_records <- records(file)
  data <- file_records$record_1
  
  
  data$time <- strftime(data$timestamp, format="%H:%M:%S")

  
  data$date<- as.Date(data$timestamp)
  data$speed <- c(data$speed)*3.6
  data$distance <- c(data$distance)/1000
  
  return(data)
}

Calories_burnt <- function(gender,Age,Weight,avg_heart_rate,time_in_min){
  if(gender=="male") {
    calories_burnt_value <-round(((((Age* 0.2017)+(Weight* 0.6036) + (avg_heart_rate *0.6309)- 90.0969)*time_in_min)/ 4.184),2)
  } else {
    calories_burnt_value <-round(((((Age* 0.074)+(Weight* 0.3041) + (avg_heart_rate *0.4472)- 40.4022)*time_in_min)/ 4.184),2)
  }
}

cppFunction('
 double calories_burnt(std::string gender, int age, double weight, double avg_heart_rate, int time_in_min)
{
   if (gender == "male")
  {
    return (round((age*0.2017 + weight*0.6036 + avg_heart_rate*0.6309 - 90.0969) * time_in_min / 4.184*100)/100);
  } else {
    return (round((age*0.074 + weight*0.3041 + avg_heart_rate*0.4472 - 40.4022) * time_in_min / 4.184*100)/100);
  }
}')

BMR <- function(gender,Age,Weight,height){
  if(gender=="male") {
    BMR_needed <- round((88.362 + (13.397*Weight) + (4.799* height ) - (5.677 *Age)),2)
    BMR_needed
  } else {
    BMR_needed <- round((447.593 + (9.247*Weight) + (3.098* height ) - (4.330 *Age)),2)
    BMR_needed
  }
}


server <- function(input, output, session) {
  
  #### the data in reactive 
  
output$description <- renderText({
  print("hello");
  'there'
  }) 
  
  data <- reactive({
    df <- fit_processor(input$file_choice$datapath)
    start <- input$distance[1]
    end <- input$distance[2]
    start <- round(dim(df)[1]*(start/100))
    end <- round(dim(df)[1]*(end/100))
    df[start:end,]
    
    
    })
  
  data_2 <- reactive({
    df <- fit_processor(input$file_choice_2$datapath)
    start <- input$distance[1]
    end <- input$distance[2]
    start <- round(dim(df)[1]*(start/100))
    end <- round(dim(df)[1]*(end/100))
    df[start:end,]
  })
  

  # rv <- reactiveValues()
  # rv$setupComplete <- FALSE
  # 
  # observe({
  #   if(input$action_button == NULL){
  #     rv$setupComplete <- TRUE
  #   }
  #   output$setupComplete <- reactive({
  #     return(rv$setupComplete)
  #   })
  #   outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
  # })
  
#### FIRST DATA VERSION 1 ####
  output$avg_speed_box <- renderValueBox({
      valueBox(paste(round(mean(data()$speed),1),"km/h",sep= " "),
               "Avg Speed",  
               icon = icon("tachometer-alt"), 
               color = "olive")
  })
  output$avg_heart_rate <- renderValueBox({
    valueBox(paste(round(mean(data()$heart_rate),0),"bpm",sep= " "),
             "Avg Heart Rate",  
             icon = icon("heartbeat"), 
             color = "olive")
  })  
  output$distance_overall <- renderValueBox({
    valueBox(paste(round(max(data()$distance),1),"km",sep= " "),
             "Distance Overall",  
             icon = icon("road"), 
             color = "olive")
  })
  output$avg_cadence <- renderValueBox({
    valueBox(paste(round(mean(data()$cadence),1),"",sep= " "),
             "Avg Cadence",  
             icon = icon("biking"), 
             color = "olive")
  })
  output$avg_power <- renderValueBox({
    valueBox(paste(round(mean(data()$power),1),"W",sep= " "),
             "Avg Power",  
             icon = icon("bolt"), 
             color = "olive")
  })
  output$time_overall <- renderValueBox({
    valueBox(paste(round(difftime(strptime (max(data()$time),format = "%H:%M:%OS", tz = "EST"), strptime(min(data()$time),format = "%H:%M:%OS", tz = "EST")),2),"h",sep= " "),
              "Time Overall",  
              icon = icon("clock"), 
              color = "olive")
  })
  
  output$calories <- renderValueBox({
    valueBox(calories_burnt(input$rdb,input$num,input$weight,round(mean(data()$heart_rate),0),time_in_min =round(difftime(strptime (max(data()$time),format = "%H:%M:%OS", tz = "EST"), strptime(min(data()$time),format = "%H:%M:%OS", tz = "EST"))*60,2) ),"Calories burnt",  icon = icon("fire"), color = "blue")
  })
  
  output$BMR <- renderValueBox({
    valueBox(BMR(gender=input$rdb,Age = input$num,Weight=input$weight,height = input$height) ,"Calories needed for healthy lifestyle",  icon = icon("fire"), color = "green")
  })
  
#### FIRST DATA VERSION 2 ####
  output$avg_speed_box_1.1 <- renderValueBox({
    valueBox(round(mean(data()$speed)),
             "Avg Speed",  
             icon = icon("tachometer-alt"), 
             color = "blue")
  })
  output$avg_heart_rate_1.2 <- renderValueBox({
    valueBox(round(mean(data()$heart_rate)),
             "Avg Heart Rate",  
             icon = icon("heartbeat"), 
             color = "yellow")
  })  
  output$distance_overall_1.3 <- renderValueBox({
    valueBox(round(max(data()$distance)),
             "Distance Overall",  
             icon = icon("road"), 
             color = "green")
  })
  output$avg_cadence_1.4 <- renderValueBox({
    valueBox(round(mean(data()$cadence)),
             "Avg Cadence",  
             icon = icon("biking"), 
             color = "orange")
  })
  output$avg_power_1.5 <- renderValueBox({
    valueBox(round(mean(data()$power)),
             "Avg Power",  
             icon = icon("bolt"), 
             color = "red")
  })
  output$time_overall_1.6 <- renderValueBox({
    valueBox( difftime(strptime (max(data()$time),
                                 format = "%H:%M:%OS", 
                                 tz = "EST"), 
                       strptime(min(data()$time),
                                format = "%H:%M:%OS", tz = "EST")),
              "Time Overall",  
              icon = icon("clock"), 
              color = "yellow")
  })
  
#######FIRST PAGE BOXES BELOW  ######
  output$avg_hr <- renderValueBox({
    valueBox(round(mean(data()$heart_rate)),
             "Avg Heart Rate",  
             icon = icon("heartbeat"), 
             color = "blue")
  })
  output$max_hr <- renderValueBox({
    valueBox(round(max(data()$heart_rate)),
             "Max Heart Rate",  
             icon = icon("heartbeat"), 
             color = "blue")
  })
  output$min_hr <- renderValueBox({
    valueBox(round(min(data()$heart_rate)),
             "Min Heart Rate",  
             icon = icon("heartbeat"), 
             color = "blue")
  })
  
#"Time & speed stats"
  output$total_time <- renderValueBox({
    valueBox( difftime(strptime (max(data()$time),
                                 format = "%H:%M:%OS", 
                                 tz = "EST"), 
                       strptime(min(data()$time),
                                format = "%H:%M:%OS", tz = "EST")),
              "Time Overall",  
              icon = icon("clock"), 
              color = "teal")
  })
  output$avg_speed2 <- renderValueBox({
    valueBox(round(mean(data()$speed),1),
             "Avg Speed",  
             icon = icon("tachometer-alt"), 
             color = "teal")
  })

  output$max_speed2 <- renderValueBox({
    valueBox(round(max(data()$speed),1),
             "Max Speed",  
             icon = icon("tachometer-alt"), 
             color = "teal")
  })

  # h3("Power & cadence stats"),
  output$avg_power2 <- renderValueBox({
    valueBox(round(mean(data()$power),1),
             "Avg Power",  
             icon = icon("bolt"), 
             color = "red")
  })
  output$max_power <- renderValueBox({
    valueBox(round(max(data()$power),1),
             "Max Power",  
             icon = icon("bolt"), 
             color = "red")
  })
  output$max_Wkg_power <- renderValueBox({
    valueBox(round(max(data()$power)/75,1),
             "Max power per kg",  
             icon = icon("bolt"), 
             color = "red")
  })
  output$avg_cadence2 <- renderValueBox({
    valueBox(round(mean(data()$cadence)),
             "Avg Cadence",  
             icon = icon("biking"), 
             color = "red")
  })
  output$max_cadence <- renderValueBox({
    valueBox(round(max(data()$cadence)),
             "Max Cadence",  
             icon = icon("biking"), 
             color = "red")
  })
  # h3("Altitude & temperature stats"),
  output$max_altitude <- renderValueBox({
    valueBox(round(max(data()$altitude)),
             "Max altitude",  
             icon = icon("mountain"), 
             color = "lime")
  })
  output$min_altitude <- renderValueBox({
    valueBox(round(min(data()$altitude)),
             "Min altitude",  
             icon = icon("mountain"), 
             color = "lime")
  })
  output$max_temp <- renderValueBox({
    valueBox(round(max(data()$temperature)),
             "Max temperature",  
             icon = icon("temperature-high"), 
             color = "lime")
  })
  output$min_temp <- renderValueBox({
    valueBox(round(min(data()$temperature)),
             "Min temperature",  
             icon = icon("temperature-low"), 
             color = "lime")
  })
  output$avg_temp <- renderValueBox({
    valueBox(round(mean(max(data()$temperature),min(data()$temperature)),1),
             "Avg temperature",  
             icon = icon("thermometer-half"), 
             color = "lime")
  })

## SECOND DATA ####
  output$avg_speed_box_2 <- renderValueBox({
    valueBox(round(mean(data_2()$speed)),
             "Avg Speed",  
             icon = icon("tachometer-alt"), 
             color = "blue")
  })
  output$avg_heart_rate_2 <- renderValueBox({
    valueBox(round(mean(data_2()$heart_rate)),
             "Avg Heart Rate",  
             icon = icon("heartbeat"), 
             color = "yellow")
  })  
  output$distance_overall_2 <- renderValueBox({
    valueBox(round(max(data_2()$distance)),
             "Distance Overall",  
             icon = icon("road"), 
             color = "green")
  })
  output$avg_cadence_2 <- renderValueBox({
    valueBox(round(mean(data_2()$cadence)),
             "Avg Cadence",  
             icon = icon("biking"), 
             color = "orange")
  })
  output$avg_power_2 <- renderValueBox({
    valueBox(round(mean(data_2()$power)),
             "Avg Power",  
             icon = icon("bolt"), 
             color = "red")
  })
  output$time_overall_2 <- renderValueBox({
    valueBox( difftime(strptime (max(data_2()$time),
                                 format = "%H:%M:%OS", tz = "EST"), 
                       strptime(min(data()$time),
                                format = "%H:%M:%OS", tz = "EST")),
              "Time Overall",  
              icon = icon("clock"), 
              color = "yellow")
  })
  

  
#### PLOTS ####

  output$plot1 <- renderPlotly({
     plot1<-ggplot(data(), aes_string(x=colnames(data())[grep(paste("^",input$xaxis,"$",sep = ""),
                                                              colnames(data()))],
                                      y=colnames(data())[grep( paste("^",input$yaxis,"$",sep = ""),
                                                               colnames(data()))]))+
      geom_line(size=1)
     
    ggplotly(plot1,dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x")
  })
  
  #to do mapy - flaga w szachownice
  greenLeafIcon <- makeIcon(
    iconUrl = "https://i.ibb.co/9wtyNtk/waving-checking-flag-1.png",
    iconWidth = 32, iconHeight = 32,
    iconAnchorX = 6, iconAnchorY = 32
  )
  
  output$map <- renderLeaflet({
    coords <- data() %>% 
      select(position_long, position_lat)
    mapka<- as.matrix(coords) %>%
      leaflet()%>%
      addTiles() %>%
      addPolylines() %>%
      addMarkers(lng= tail(data()$position_long,n=1), lat =tail(data()$position_lat,n=1), icon =greenLeafIcon )
    mapka
  })
  

  output$table <- DT::renderDataTable(data(), options = list(autoWidth = TRUE, scrollX = TRUE))
  
  output$plot_heart_rate <- renderPlotly({
    heart_rate_table <- as.data.frame(data() %>% group_by(Heart_rate_interval = cut(heart_rate, breaks = seq(100, max(heart_rate), 20))) %>%
                    summarise(Seconds = n()) %>%   mutate(Percentage_of_time = round((Seconds/sum(Seconds))*100),2))
    
    plot_heart_rate<-ggplot(heart_rate_table, aes(x=Heart_rate_interval, y=Percentage_of_time ))+
      geom_bar(stat="identity", width = 0.5, fill="tomato2")+ 
      labs(title="Five Heart Rate Zones from your training ") +
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
    
    ggplotly(plot_heart_rate,dynamicTicks = TRUE)
  })
  
  
  
  ###ZDJECIA
  
  observeEvent(input$myFile, {
    inFile <- input$myFile
    if (is.null(inFile))
      return()
    
    b64 <- base64enc::dataURI(file = inFile$datapath, mime = "image/png")
    insertUI(
      selector = "#image-container",
      where = "afterBegin",
      ui = img(src = b64, width = 350, height = 250)
    )
  })
  
#### PLOTS FOR COMPARISON ####
  
  output$comparison_plot <- renderPlotly({
  
    final_plot <- ggplot() +
      geom_line(data = data(), aes_string(x = c(1:dim(data())[1]), 
                                   y = colnames(data())[grep(paste("^",input$variable_choice,"$",sep = ""),
                                                             colnames(data()))])) +
      geom_line(data = data_2(), aes_string(x = c(1:dim(data_2())[1]), 
                                     y = colnames(data_2())[grep(paste("^",input$variable_choice,"$",sep = ""),
                                                               colnames(data_2()))]), color = "red")
    
    final_plot
  })

  
}
