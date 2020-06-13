library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(dplyr)

df = read.csv('HW_4_electricity_load_Turkey.csv')
df$Avg_Temperature = round((df$T_1+df$T_2+df$T_3+df$T_4+df$T_5+df$T_6+df$T_7) / 7 , 2 )
DT <- data.table(df)
summary_table = data.table(DT[,c("T_1","T_2","T_3","T_4","T_5","T_6","T_7"):=NULL])
summary_table$Date <- as.Date(summary_table$Date)
df_plot2 = data.table(df)
df_plot2 = data.table(df_plot2[,c("Consumption","Avg_Temperature"):=NULL])
df_plot2$Date <- as.Date(df_plot2$Date)


ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "datatable")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "datatable",
              fluidRow(
                dateRangeInput(
                  inputId = "daterange",
                  label = "Select Start and End Date",
                  start = min(summary_table$Date),
                  end = max(summary_table$Date),
                  min = min(summary_table$Date),
                  max = max(summary_table$Date),
                  format = "dd/mm/yyyy",
                  separator = "-"
                ),
                textOutput("range"),
                box(DT::dataTableOutput("mytable"), height = 525),
                box(plotOutput("plot1"), height = 250),
                box(plotOutput("plot2"), height = 250)
                
              )
              
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$range = renderText({
    paste("Selected date range: ", input$daterange[1], "-" , input$daterange[2])
    
  }
  )
  
  output$mytable = DT::renderDataTable({
    s=subset(summary_table, summary_table$Date >= input$daterange[1] & summary_table$Date <= input$daterange[2])
    
  })
  
  
  graph1  <- reactive({
    s=subset(summary_table, summary_table$Date >= input$daterange[1] & summary_table$Date <= input$daterange[2])
    
    ggplot(s, aes(Hour, Consumption)) +
      geom_point(color = "grey90") + 
      stat_summary(geom = "point", 
                   fun.y = mean, 
                   color = "red") + 
      geom_smooth()+
      xlab("Hour") + 
      ylab("Average Consumption")
    
  })
  
  output$plot1 <- renderPlot({graph1()},
  height = 230
  )
  
  
  graph2  <- reactive({
    s2=subset(df_plot2, df_plot2$Date >= input$daterange[1] & df_plot2$Date <= input$daterange[2])
    
    gd <- s2 %>% 
      group_by(Hour) %>% 
      summarise(
        T_1 = mean(T_1),
        T_2 = mean(T_2),
        T_3 = mean(T_3),
        T_4 = mean(T_4),
        T_5 = mean(T_5),
        T_6 = mean(T_6),
        T_7 = mean(T_7)
      )
    
    ggplot(gd, aes(Hour)) +
      geom_line(aes(y = T_1), color = "darkred")  +
      geom_line(aes(y = T_2), color = "darkblue")  +
      geom_line(aes(y = T_3), color = "darkgreen")  +
      geom_line(aes(y = T_4), color = "red")  +
      geom_line(aes(y = T_5), color = "blue")  +
      geom_line(aes(y = T_6), color = "green")  +
      geom_line(aes(y = T_7), color = "black")  +
      xlab("Hour") + 
      ylab("Average Temperature")+
      theme(legend.position="bottom")
  })
  
  output$plot2 <- renderPlot({
    graph2()},
    height = 230
  )
  
  
}

shinyApp(ui, server)