library(shiny)
library(tidyverse)
library(shinythemes)
library(readxl)

data <- read.csv("Master_Suicide_Data.csv")
tib <- as_tibble(data)
head(tib)

not_sel <- "Not Selected"

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
        
                    h1 {
                    font-family: 'Aldhabi';
                    font-weight: 500;
                    line-height: 1.1;
                    color: navy;
                    }
                    
                    h4 {
                    font-family: 'Arial';
                    font-weight: 300;
                    line-height: 1.1;
                    }
                    
                    h5 {
                    font-family: 'Arial';
                    font-weight: 300;
                    line-height: 1.1;
                    }
                    
                    "))
  ),
  
  titlePanel(h1("Analysis on Global Suicide Rate Trend")),
  h4("This app is created to analyse Global Suicide Rate Trend. Data will be preview based on details selected below."), 
  h4("For scatter graph and bar chart, please refer to other tab."),
  h4("For more info regarding this app, please refer 'About' tab."),
  h6("Attn: Graph and Chart tab will take time to load data."),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId="country",label="Select a Country",choices = c(not_sel, unique(tib$Country)),
                    levels(tib$Country)),
      selectizeInput(inputId = "sex",label = "Select a Gender",choices = c(not_sel, unique(tib$Sex)),
                    levels(tib$Sex)),
      selectizeInput(inputId="age",label="Select an Age Group",choices = c(not_sel, unique(tib$Age)),
                     levels(tib$Age)),
      sliderInput("year","Select Year Range",1985,2016, value = c(1985,2016))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Table", 
          DT::dataTableOutput("table")
        ),
        tabPanel(
          title = "Scatter Plot by Gender",
          plotOutput("plot1", width = "150%", height = "1200px")
        ),
        tabPanel(
          title = "Scatter Plot by Age",
          plotOutput("plot2", width = "150%", height = "1200px")
        ),
        tabPanel(
          title = "Bar Chart by Gender",
          plotOutput("sexbar", width = "150%", height = "1200px")
        ),
        tabPanel(
          title = "Bar Chart by Age",
          plotOutput("agebar", width = "150%", height = "1200px")
        ),
        tabPanel(
          title = "User Manual",
          img(src="usermanual.jpeg", height = 500, width = 700, align ="center")
        ),
        tabPanel(
          title = "About",
          h5("This shiny app is created by group G for WQD7001 Principle of Data Science course."),
          h5("The dataset used in this app is from https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016."),
          h5("Gender = Refer to gender (Female/ Male)"),
          h5("Age = Refer to age group"),
          h5("Suicide_100k_pop = Suicide rate per 100k population"),
          h5("Scatter plot and bar chart are plot based on x= Year, y= Suicide rate.")
          
        )
      )
    )
  )
)
#server = function(input, output) {}
  
server = function(input, output) {
  
  #filter
  tibnew <- reactive({
    if(input$country != not_sel & input$sex != not_sel & input$age != not_sel){
      tib %>% 
        filter(Country == input$country) %>% 
        filter(Sex == input$sex) %>% 
        filter(Age == input$age) %>%
        filter(Year >= input$year[1] & Year <= input$year[2])
    }
    else if(input$country != not_sel & input$sex != not_sel & input$age == not_sel){
      tib %>% 
        filter(Country == input$country) %>% 
        filter(Sex == input$sex) %>% 
        filter(Year >= input$year[1] & Year <= input$year[2])
    }
    else if(input$country != not_sel & input$sex == not_sel & input$age != not_sel){
      tib %>% 
        filter(Country == input$country) %>% 
        filter(Age == input$age) %>%
        filter(Year >= input$year[1] & Year <= input$year[2])
    }
    else if(input$country == not_sel & input$sex != not_sel & input$age != not_sel){
      tib %>% 
        filter(Sex == input$sex) %>% 
        filter(Age == input$age) %>%
        filter(Year >= input$year[1] & Year <= input$year[2])
    }
    else if(input$country != not_sel & input$sex == not_sel & input$age == not_sel){
      tib %>% 
        filter(Country == input$country) %>% 
        filter(Year >= input$year[1] & Year <= input$year[2])
    }
    else if(input$country == not_sel & input$sex == not_sel & input$age != not_sel){
      tib %>% 
        filter(Age == input$age) %>%
        filter(Year >= input$year[1] & Year <= input$year[2])
    }
    else if(input$country == not_sel & input$sex != not_sel & input$age == not_sel){
      tib %>% 
        filter(Country == input$country) %>% 
        filter(Sex == input$sex) %>% 
        filter(Age == input$age) %>%
        filter(Year >= input$year[1] & Year <= input$year[2])
    }
    else if(input$country == not_sel & input$sex == not_sel & input$age == not_sel){
      tib %>% 
        filter(Year >= input$year[1] & Year <= input$year[2])
    }
  })
  
  #Table
  output$table <- DT::renderDataTable({ 
    DT::datatable(tibnew(), options = list(orderClasses = TRUE))
    
  })
  
  #Plot Group by Sex
  output$plot1 <- renderPlot({
    ggplot(tib,
           aes_string(x = tib$Year, y = tib$Suicide_100k_pop)) +
      geom_point(aes(color=Sex)) + facet_wrap(~Country)
  })
  
  #Plot Group by Age
  output$plot2 <- renderPlot({
    ggplot(tib,
           aes_string(x = tib$Year, y = tib$Suicide_100k_pop)) +
      geom_point(aes(color=Age)) + facet_wrap(~Country)
  })
  
  #Barplot group by sex
  output$sexbar <- renderPlot({
    ggplot(tib, aes(x=tib$Year, y=tib$Suicide_100k_pop, fill=Sex)) +
      geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~Country)
    
  })
  #Barplot group by age
  output$agebar <- renderPlot({
    ggplot(tib, aes(x=tib$Year, y=tib$Suicide_100k_pop, fill=Age)) +
      geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~Country)
  })
  
}
shinyApp(ui, server)

