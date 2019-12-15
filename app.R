library(tidyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(janitor)
library(shiny)
library(tidyverse)
library(forcats)
library(hash)
library(shinythemes)

load("data_2016.Rda")
load("percent_vote_long.Rda")

var_labels <- hash()
var_labels[["Income: Median Income"]] = "median_income_2015"
var_labels[["Bachelor Degree or Higher: Percent of Population"]] = "bach_higher_25_2015"
var_labels[["Age 18-24: Percent of Population"]] = "per_18_24_2015"
var_labels[["Age 35 and Older: Percent of Population"]] = "per_35_older_2015"
var_labels[["Rent: Median Rent"]] = "median_rent_2015"
var_labels[["White, non-Hispanic: Percent of Population"]] = "per_white_2015"
var_labels[["Asthma: Percent of Population"]] = "asthma_per_2012_2015"
var_labels[["Diabetes: Percent of Population"]] = "diabetes_per_2012_2015"
var_labels[["Hypertension: Percent of Population"]] = "hypertension_2013_2015"
var_labels[["Obesity: Percent of Population"]] = "obesity_2013_2015"
var_labels[["Persistent Sadness: Percent of Population"]] ="sadness_2015"


ui <- fluidPage(theme=shinytheme("cerulean"),
                titlePanel("Voting Percentage Data"),
                tabsetPanel(
                    tabPanel("Time Trends",
                             fluidPage(
                                 fluidRow(
                                     column(width=12,
                                            sliderInput(inputId = "year",
                                                        label="Select Year",
                                                        value = 2018, min=2008, max=2018,
                                                        step=1,
                                                        sep = "",
                                                        ticks=FALSE,
                                                        animate=TRUE))),
                                 fluidRow(column(width=4,
                                                 selectInput(inputId = "neighborhood1",
                                                             label="Select Neighborhood 1",
                                                             choices = c("Allston", "Back Bay", "Beacon Hill", "Brighton", "Charlestown",
                                                                         "Chinatown", "Dorchester", "East Boston", "Fenway", "Harbor Islands",
                                                                         "Hyde Park", "Jamaica Plain", "Mattapan", "Mission Hill", "North End",
                                                                         "Roslindale", "Roxbury", "South Boston", "South End", "West End", "West Roxbury")
                                                 )
                                 ),
                                 column(width=4,
                                        selectInput(inputId = "neighborhood2",
                                                    label="Select Neighborhood 2",
                                                    choices = c("None", "Allston", "Back Bay", "Beacon Hill", "Brighton", "Charlestown",
                                                                "Chinatown", "Dorchester", "East Boston", "Fenway", "Harbor Islands",
                                                                "Hyde Park", "Jamaica Plain", "Mattapan", "Mission Hill", "North End",
                                                                "Roslindale", "Roxbury", "South Boston", "South End", "West End", "West Roxbury"),
                                                    selected=c("Back Bay")
                                        )
                                 ),
                                 column(width=4,
                                        selectInput(inputId = "neighborhood3",
                                                    label="Select Neighborhood 3",
                                                    choices = c("None", "Allston", "Back Bay", "Beacon Hill", "Brighton", "Charlestown",
                                                                "Chinatown", "Dorchester", "East Boston", "Fenway", "Harbor Islands",
                                                                "Hyde Park", "Jamaica Plain", "Mattapan", "Mission Hill", "North End",
                                                                "Roslindale", "Roxbury", "South Boston", "South End", "West End", "West Roxbury"),
                                                    selected=c("Beacon Hill")
                                        )
                                 )
                                 ),
                                 fluidRow(width=12,
                                          plotOutput(outputId ="time")
                                 ),
                                 fluidRow(column(4, tableOutput("table_neighbor1")),
                                          column(4, tableOutput("table_neighbor2")),
                                          column(4, tableOutput("table_neighbor3")))
                             )  
                    ),
                    tabPanel("Scatter Plot",
                             fluidRow(
                                 column(width=12,
                                        selectInput(inputId = "custom_var",
                                                    label="Select Variable",
                                                    choices = c("Age 18-24: Percent of Population", "Age 35 and Older: Percent of Population",
                                                                "Asthma: Percent of Population", "Bachelor Degree or Higher: Percent of Population", 
                                                                "Diabetes: Percent of Population", "Hypertension: Percent of Population", "Income: Median Income", 
                                                                "Obesity: Percent of Population", "Persistent Sadness: Percent of Population", "Rent: Median Rent",
                                                                "White, non-Hispanic: Percent of Population")
                                        ),
                                        plotOutput(outputId="customscatter", height=300,
                                                   click="plot2_click",
                                                   brush=brushOpts(id="plot2_brush")
                                        )
                                 )
                             ),
                             fluidRow(
                                 column(width=6,
                                        h5("Click Data Point to View Neighborhood Data"),
                                        verbatimTextOutput("click_info2")
                                 ),
                                 column(width=6,
                                        h5("Click and Drag to Select Multiple Neighborhoods"),
                                        verbatimTextOutput("brush_info2")
                                 )
                             ),
                             fluidRow(
                                 column(12, tableOutput("table"))
                             )
                    )
                )
)

server <- function(input, output, session) {
    output$customscatter <- renderPlot({
        data_2016 %>% select(neighborhood, vote_per_2016, var_labels[[input$custom_var]]) %>% 
            ggplot() +
            geom_point(aes_string(x=var_labels[[input$custom_var]],y="vote_per_2016", color="neighborhood"), 
                       alpha=0.7, size=5, show.legend = FALSE) +
            geom_smooth(aes_string(x=var_labels[[input$custom_var]],y="vote_per_2016"), method='lm', se=FALSE, 
                        alpha=0.5, size=0.5, weight=0.5, color="grey")+
            xlab(input$custom_var) +
            ylab("Voting Percentage in 2016") +
            expand_limits(x = 0, y = 0) +
            theme_gray() +
            theme(axis.text = element_text(size=12),
                  axis.title=element_text(size=18)
            )
    })
    output$click_info2 <- renderPrint({
        nearPoints(data_2016 %>% select(neighborhood, vote_per_2016, var_labels[[input$custom_var]]),  
                   input$plot2_click, 
                   addDist = FALSE
        )
    })
    output$brush_info2 <- renderPrint({
        brushedPoints(data_2016 %>% select(neighborhood, vote_per_2016, var_labels[[input$custom_var]]), 
                      input$plot2_brush
        )
    })
    output$table <- renderTable(
        data_2016 %>% select(neighborhood, vote_per_2016, var_labels[[input$custom_var]]) %>%
            na.omit() %>%
            rename("Neighborhood"="neighborhood", "Voting Percentage"="vote_per_2016", 
                   !!input$custom_var:=var_labels[[input$custom_var]]),
        digits=0, align='c', striped=TRUE
    )
    output$time <- renderPlot(
        percent_vote_long %>% filter(neighborhood %in% c(input$neighborhood1, 
                                                         input$neighborhood2, 
                                                         input$neighborhood3)) %>%
            filter(year %in% (2008:input$year)) %>%
            mutate(percent_vote_100 = percent_vote*100) %>%
            ggplot(aes(y=percent_vote_100, x=year, color = neighborhood)) +
            geom_line(size=2) +
            scale_y_continuous(name="Voting Percentage - Registered Voters") +
            scale_x_continuous(name="Year", limits=c(2008,2018), breaks=c(2008:2019)) +
            theme(axis.text = element_text(size=12),
                  axis.title=element_text(size=18)
            )
    )
    output$table_neighbor1 <- renderTable(
        percent_vote_long %>% filter(neighborhood %in% c(input$neighborhood1)) %>%
            mutate(percent_vote_100 = percent_vote*100) %>% 
            group_by(year) %>%
            select(neighborhood, percent_vote_100) %>% 
            arrange(neighborhood, year) %>%
            rename("Year" = "year", "Neighborhood"="neighborhood", "Voting Percentage"="percent_vote_100"),
        digits=0, align='c', striped=TRUE
    )
    output$table_neighbor2 <- renderTable(
        percent_vote_long %>% filter(neighborhood %in% c(input$neighborhood2)) %>%
            mutate(percent_vote_100 = percent_vote*100) %>% 
            group_by(year) %>%
            select(neighborhood, percent_vote_100) %>% 
            arrange(neighborhood, year) %>%
            rename("Year" = "year", "Neighborhood"="neighborhood", "Voting Percentage"="percent_vote_100"),
        digits=0, align='c', striped = TRUE
    )
    output$table_neighbor3 <- renderTable(
        percent_vote_long %>% filter(neighborhood %in% c(input$neighborhood3)) %>%
            mutate(percent_vote_100 = percent_vote*100) %>% 
            group_by(year) %>%
            select(neighborhood, percent_vote_100) %>% 
            arrange(neighborhood, year) %>%
            rename("Year" = "year", "Neighborhood"="neighborhood", "Voting Percentage"="percent_vote_100"),
        digits=0, align='c', striped=TRUE
    )
}

shinyApp(ui, server)



