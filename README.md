# Shiny App for visualizing Statistics on Income Insecurity.
_Jesús Estévez-Sánchez, 05 December 2020_
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

This repository is based on [Ariane Aumaitre](https://github.com/aaumaitre/eurostat) repo  where she 
explains how to build a Shiny App with Eurostat data.

First, we have to obtain the data from the [Eurostat](https://ec.europa.eu/eurostat/data/database) database. In this case,
we are using the statistics about Income Insecurity which Eurostat measures as the Labour transitions by employment status.

```{r, echo=TRUE,message = FALSE }
# Libraries needed:

library(shiny)
library(tidyverse)
library(rsconnect)
library(plotly)
library(wesanderson)

```

```{r,message = FALSE, warning = FALSE}
# Now we want to read the data:

data <- read.csv("C:\\Users\\Usuario\\Desktop\\R_Studio_clases\\SHINY\\LIVING_CONDITIONS\\transitions.csv")

# And clean parts of the data:

data <- data %>% select(-X)


# Change the Value column from chracter to integer.

data$VALUE <- as.numeric(as.character(data$VALUE))

# And the Chracter columns that contains strings have to be converted to factors.

data$COUNTRY <- as.factor(data$COUNTRY)
data$TRANS1Y <- as.factor(data$TRANS1Y)
data$SEX <- as.factor(data$SEX)
data$WSTATUS <- as.factor(data$WSTATUS)


```

Once we have all the data, we can start to create the user interface.

```{r,message = FALSE, warning = FALSE}
#Creating the user interface
ui <- fluidPage(
    #This line allows the app to show properly on mobile devices:
    HTML('<meta name="viewport" content="width=1024">'),
    
    #Setting the relative size of the sidebar and the plot area:
    tags$head(tags$style(HTML(".col-sm-4 { width: 25%;}
                    .col-sm-8 { width: 75%;}")),
              #Title to be shown @ browser (to get rid of html tags)
              tags$title("Plotting Eurostat statistics on Income Insecurity")),
    
    
    # Application title
    headerPanel(HTML("<b><center>Plotting Eurostat statistics on Income Insecurity</b></center></br>")),
    
    
    
    #Giving a tabset appearance to the app
    tabsetPanel(type = "tabs",
                #Each tabPanel call specifies input for contents of tab
                tabPanel("Line plots", #Tab title
                         sidebarLayout( #To have a personalized sidebar per tab
                             sidebarPanel(
                                 #creating the select lists for countries, indicators, sex, age:
                                 selectInput(inputId = "geo",
                                             label = "Select countries:",
                                             choices = levels(data$COUNTRY),
                                             selected = levels(data$COUNTRY)[1],
                                             multiple = TRUE), #allowing multiple country selection
                                 selectInput(inputId = "ind",
                                             label = "Select type of transition:",
                                             choices = levels(data$TRANS1Y),
                                             selected = levels(data$TRANS1Y)[1]),
                                 selectInput(inputId = "wstat",
                                             label = "Select working status:",
                                             choices = levels(data$WSTATUS),
                                             selected = levels(data$WSTATUS)[1]),
                                 selectInput(inputId = "sex",
                                             label = "Sex:",
                                             choices = levels(data$SEX),
                                             selected = "Total"),
                                 #Slider bar to allow custom x axis
                                 sliderInput("years", "Year range",
                                             min(data$YEAR), max(data$YEAR),
                                             value = c(2006, 2019),
                                             step = 5)),
                             #The main panel of the tab will show the lines plot(ly)
                             mainPanel(plotlyOutput("lines")))),
                #Same process for the next tab: bar plots 
                #(some changes made to the options in the side panel)
                tabPanel("Bar plots",
                         sidebarLayout(
                             sidebarPanel( selectInput(inputId ="years_b", 
                                                       label = "Year",
                                                       choices = c(2006:2019),
                                                       selected = 2017),
                                           selectInput(inputId = "ind_b",
                                                       label = "Transition",
                                                       choices = levels(data$TRANS1Y),
                                                       selected = levels(data$TRANS1Y)[1]),
                                           selectInput(inputId = "wstat_b",
                                                       label = "Working status",
                                                       choices = levels(data$WSTATUS),
                                                       selected = levels(data$WSTATUS)[1]),
                                           selectInput(inputId = "sex_b",
                                                       label = "Sex",
                                                       choices = levels(data$SEX),
                                                       selected = "Total")),
                             mainPanel(plotlyOutput("bars")))),
                #Panel with information about the app:
                tabPanel("About", 
                         p(HTML("This is a Shiny Application built to plot statistics on Income Insecurity from Eurostat.")),
                         p(HTML("It allows to either compare countries across time by using line charts, or to take more specific snapshots of a moment in time by comparing the 34 countries available.")),
                         p(HTML("You can browse through different indicators and look at their values while specifying sex groups.")),
                         p(HTML("Passing the mouse over the chart gives the exact values of the indicators by country and year.")),
                         p(HTML("Code for the app is available on <a href= https://github.com/jesusestevez/Living_Conditions/>Github</a>")),
                         p(HTML("Plots are generated using ggplot2 and ggplotly.")),
                )
                
    ))

```

Now we proceed with the Server:

```{r,message = FALSE, warning = FALSE}

#Server function to define the output that will be shown in the app

server <- function(input, output) {
    
    #First, the lines chart
    output$lines <- renderPlotly({
        #The "Selected" variables will serve to subset out data in function of
        # the input: they are a way of storing the input selected
        GEOSelected = input$geo
        INDSelected = input$ind
        SEXSelected = input$sex
        WSTSelected = input$wstat
        #To link the input selected with our dataframe, we subset our data
        #frame ("eurostat") with the values selected by the user and create
        #a data frame
        lines_data <- 
            subset(data, 
                   COUNTRY %in% GEOSelected & 
                       TRANS1Y == INDSelected &
                       WSTATUS == WSTSelected &
                       YEAR >= input$years[1] & 
                       YEAR <= input$years[2] &
                       SEX == SEXSelected)
        
        #And with this the  plot is created
        h1 <- lines_data%>%
            ggplot(aes(x = YEAR, y = VALUE, color = COUNTRY))+
            geom_line(size = 0.8)+
            geom_point(size = 1.8)+
            #The second argument in wes_palette makes values change depending on 
            #the number of countries that are selected
            scale_color_manual(values =rev(wes_palette("Darjeeling1", 
                                                       length(unique(lines_data$COUNTRY)), 
                                                       type = "continuous")))+
            #bringing some air to the plot:
            expand_limits(y = (max(lines_data$VALUE) + 0.05*max(lines_data$VALUE)))+
            scale_x_continuous(breaks = seq(min(lines_data$YEAR), max(lines_data$YEAR), by = 2))+
            labs(title = "Income Insecurity (in % of initial status; population aged 15-74)",
                 x = NULL, y = NULL, color = NULL)+ 
            theme_minimal()+
            theme(panel.grid.major.y = element_line(color = "grey87"), 
                  panel.grid.major.x = element_blank(),
                  axis.line.x = element_line(color = "grey87"),
                  axis.ticks.x = element_line(color = "grey40"),
                  plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
                  axis.text.x = element_text(size = 10, color = "grey40"),
                  axis.text.y = element_text(size = 10, color = "grey40"))
        
        ggplotly(h1, width = 1200, height = 600)%>%
            layout(legend = list(orientation = "h", x = 0.3, y =-0.1))
    }
    )
    
    #Same process for the bar chart:
    output$bars <- renderPlotly(({
        IBSelected = input$ind_b
        YBSelected = input$years_b
        SEXBSelected = input$sex_b 
        WSTBSelected = input$wstat_b
        
        bars_data <- subset(data,                                             
                            TRANS1Y == IBSelected &
                                YEAR == YBSelected & 
                                SEX == SEXBSelected & 
                                WSTATUS == WSTBSelected)
        
        h2 <- bars_data%>%
            ggplot(aes(x = reorder(COUNTRY, VALUE), y = VALUE,
                       text = paste0(COUNTRY, " ", YEAR,": ", VALUE)))+
            geom_bar(stat = "identity", fill = "dodgerblue", width = 0.5)+
            expand_limits(y = (max(bars_data$VALUE) + 0.05*max(bars_data$VALUE)))+
            labs(title = "Income Insecurity (in % of initial status; population aged 15-74)", y = NULL, x = NULL)+
            theme_minimal()+ 
            theme(panel.grid.major.y = element_line(color = "grey87"), 
                  panel.grid.major.x = element_blank(),
                  axis.line.x = element_line(color = "grey87"),
                  axis.ticks.x = element_line(color = "grey40"),
                  plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
                  axis.text.x = element_text(size = 10, color = "grey40", angle = 45),
                  axis.text.y = element_text(size = 10, color = "grey40"))
        
        ggplotly(h2, tooltip = "text", width = 1200, height = 600)
        
    }))
    
}

# Run the application 
shinyApp(ui = ui, server = server)

```

