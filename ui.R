#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Libraries: 
library(shiny)
library(tidyverse)
library(naniar) # for NA definition
library(psych)
library(viridis) # for scale color
library(viridisLite) # for scale color
library(ggrepel) # for text labels for points
library(scales) # for scales 
library(splines) #for splines


# einlesen der Daten
wh_df <- read.csv2("data/WDB_health_prep.csv")
wg_df <- read.csv2("data/WDB_gdp_prep.csv")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Life Expectancy at Birth Worldwide"),
    em("Predicting the Influence of the Living Standard and Health Expenditure on Life Expectancy using Data from 2000 to 2017"),
    br(),
    br(),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("gdpRangeSlider",
                        "GDP per capita (USD)",
                        min = round(min(wg_df$gdp_c)),
                        max = round(max(wg_df$gdp_c)),
                        value = c(round(min(wg_df$gdp_c)), round(max(wg_df$gdp_c)))
                        ),
            sliderInput("hexpRangeSlider",
                        "Health Expenditure (USD)",
                        min = round(min(wh_df$health.exp)),
                        max = round(max(wh_df$health.exp)),
                        value = c(round(min(wh_df$health.exp)), round(max(wh_df$health.exp)))
            ),
            checkboxGroupInput("ContinentTypesCheckbox",
                               "Continent",
                               choices = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
                               selected = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
                               ),
            sliderInput("yearselectBox",
                        "Year",
                        min = 2000,
                        max = 2017,
                        value = 2000,
                        step = 1,
                        sep = "",
                        ticks = FALSE,
                        animate = TRUE,
            ),
            checkboxGroupInput("regressionMethodCheckBox",
                               "Regression methods",
                               choices = c("Linear", "Cubic", "Polynomial")
            ),
            selectizeInput("labeledCountriesInput",
                           "Label countries",
                           choices = unique(wg_df$Country.Name),
                           multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("gdp_c"), plotOutput("health_exp")),
            br(),
            p("1. The graph represents the relationship between the outcome variable life expectancy and independent variable gross domestic product (GDP) per capita. It needs to be kept in mind that the predictor is measured per capita and therefore the size of the population impacts the result. In this analysis living standards are operationalized as GDP per capita. The general trend is that higher values of GDP per capita lead to higher life expectancy at birth. GDP increases economic growth and development in a country and thus prolongs people’s lifespan. Lowest rates of life expectancy as well as GDP per capita are mainly recorded in African countries, especially in Sierra Leone, Eswatini and Zimbabwe. Highest values can be discerned mostly in Europe, Oceania and come countries of Asia. According to the analyzed World Bank dataset, countries with longest life expectancy are Hong-Kong, Japan, Macao and Switzerland. Overall, both GDP per capita and life expectancy increased over time in years 2002-2017. The tables below display the results obtained through the two following regression methods: cubic polynomial regression and natural splines."),
            br(),
            p("2. The graph represents the relationship between the outcome variable life expectancy and independent variable health expenditure per capita. This predictor indicates the amount of spending on the health sector relatively to the population size. There is generally a positive trend - more investment in the health care means higher life expectancy rates. However, after certain point, when population reaches the age of 80-85 years old the effect of health expenditure diminishes and the trade line flattens and even becomes negative. Lowest rates of both indicators are recorded in the countries of South America and African countries. According to the analyzed World Bank dataset, countries with longest life expectancy are Hong-Kong, Japan, Macao and Switzerland. Overall, both health expenditure and life expectancy increased over time in years 2002-2017. The tables below display the results obtained through the two following regression methods: cubic polynomial regression and natural splines."),
            splitLayout(cellWidths = c("50%", "50%"), tableOutput("stattable_g"), tableOutput("stattable_h"))
            )
        )
    )
))

