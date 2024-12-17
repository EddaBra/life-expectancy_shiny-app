#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    filtered_gdp <- reactive({
      result <- wg_df %>% #prepare filtered data set
        filter(gdp_c >= input$gdpRangeSlider [1], #filter for range on x-axis gdp_c
               gdp_c <= input$gdpRangeSlider [2],
               Continent %in% input$ContinentTypesCheckbox, #filter for Continent Check Box
               year == input$yearselectBox # filter for year out of a select box
        )
      updateSelectInput(session, "labeledCountriesInput",
                        choices = unique(result$Country.Name))
      return(result)
    })  
  
    filtered_hexp <- reactive({
      result <- wh_df %>% #prepare filtered data set
        filter(health.exp >= input$hexpRangeSlider [1], #filter for range on x-axis gdp_c
               health.exp <= input$hexpRangeSlider [2],
               Continent %in% input$ContinentTypesCheckbox, #filter for Continent Check Box
               year == input$yearselectBox # filter for year out of a select box
        )
      updateSelectInput(session, "labeledCountriesInput",
                        choices = unique(result$Country.Name))
      return(result)
    })  
    
    
    output$gdp_c <- renderPlot({
      data <- filtered_gdp()
      result <- ggplot(data, # filtered data set
                 aes(gdp_c, lifeexp, color= Continent))+
        geom_point(alpha=0.7)+
        labs(title="Life Expectancy and Gross Domestic Product per Capita",
             subtitle = input$yearselectBox, 
             x="Gross Domestic Product per Capita",
             y= "Life Expectancy",
             color= "Continent")+
        scale_color_viridis(discrete=TRUE) +
        theme_minimal()+
        scale_x_continuous(labels=scales::dollar) +
        geom_text(aes(label=ifelse(Country.Name %in% input$labeledCountriesInput, as.character(Country.Name),'')),hjust=0.2,vjust=0)
      if ("Linear" %in% input$regressionMethodCheckBox) {
        mg.lin <- lm(lifeexp ~ gdp_c, data= wg_df)
        x_g_pred <- seq(min(data$gdp_c), max(data$gdp_c), length.out = 20) #define x-values
        y_g_pred.lin <- predict(mg.lin, newdata = tibble(gdp_c = x_g_pred)) # predict for linear model
        result <- result +
          geom_line(data=tibble(gdp_c = x_g_pred, lifeexp = y_g_pred.lin), size = 1, col = "violetred4")
      }
      if ("Cubic" %in% input$regressionMethodCheckBox) {
        mg.cub <- lm(lifeexp ~ poly(gdp_c, 3, raw=TRUE), data=wg_df)
        x_g_pred <- seq(min(data$gdp_c), max(data$gdp_c), length.out = 20) #define x-values
        y_g_pred.lin <- predict(mg.cub, newdata = tibble(gdp_c = x_g_pred)) # predict for linear model
        result <- result +
          geom_line(data=tibble(gdp_c = x_g_pred, lifeexp = y_g_pred.lin), size = 1, col = "blue4")
      }
      if ("Polynomial" %in% input$regressionMethodCheckBox) {
        mg.ns <- lm(lifeexp ~ ns(gdp_c, 3), data = wg_df)
        x_g_pred <- seq(min(data$gdp_c), max(data$gdp_c), length.out = 20) #define x-values
        y_g_pred.lin <- predict(mg.ns, newdata = tibble(gdp_c = x_g_pred)) # predict for linear model
        result <- result +
          geom_line(data=tibble(gdp_c = x_g_pred, lifeexp = y_g_pred.lin), size = 1, col = "darkslategray3")
      }
      return(result)
    })
    
    output$health_exp <- renderPlot({
      data <- filtered_hexp()
        result <- ggplot(data, # filtered data set
               aes(health.exp, lifeexp, color= Continent))+
          geom_point(alpha=0.7)+
          labs(title="Health Expenditure",
               subtitle = input$yearselectBox,
               x="Health Expenditure",
               y= "Life Expectancy",
               color= "Continent") +
          scale_color_viridis(discrete=TRUE) +
          theme_minimal()+
          scale_x_continuous(labels=scales::dollar) +
          geom_text(aes(label=ifelse(Country.Name %in% input$labeledCountriesInput, as.character(Country.Name),'')),hjust=0.2,vjust=0)
        if ("Linear" %in% input$regressionMethodCheckBox) {
          mh.lin <- lm(lifeexp ~ health.exp, data= wh_df)
          x_g_pred <- seq(min(data$health.exp), max(data$health.exp), length.out = 100) #define x-values
          y_g_pred.lin <- predict(mh.lin, newdata = tibble(health.exp = x_g_pred)) # predict for linear model
          result <- result +
            geom_line(data=tibble(health.exp = x_g_pred, lifeexp = y_g_pred.lin), size = 1, col = "violetred4")
        }
        if ("Cubic" %in% input$regressionMethodCheckBox) {
          mh.cub <- lm(lifeexp ~ poly(health.exp, 3, raw=TRUE), data=wh_df)
          x_g_pred <- seq(min(data$health.exp), max(data$health.exp), length.out = 100) #define x-values
          y_g_pred.cub <- predict(mh.cub, newdata = tibble(health.exp = x_g_pred)) # predict for cubic model
          result <- result +
            geom_line(data=tibble(health.exp = x_g_pred, lifeexp = y_g_pred.cub), size = 1, col = "blue4")
        }
        if ("Polynomial" %in% input$regressionMethodCheckBox) {
          mh.ns <- lm(lifeexp ~ ns(health.exp, 3), data = wh_df)
          x_g_pred <- seq(min(data$health.exp), max(data$health.exp), length.out = 100) #define x-values
          y_g_pred.ns <- predict(mh.ns, newdata = tibble(health.exp = x_g_pred)) # predict for cubic model
          result <- result +
            geom_line(data=tibble(health.exp = x_g_pred, lifeexp = y_g_pred.ns), size = 1, col = "darkslategray3")
        }
        return(result)
    })
    
    
    output$stattable_g <- renderTable({
      if (!("Linear" %in% input$regressionMethodCheckBox) & !("Cubic" %in% input$regressionMethodCheckBox) & !("Polynomial" %in% input$regressionMethodCheckBox)) {
        return()
      }
      
      data <- filtered_gdp()
      result_table <- matrix(nrow=0, ncol=4)
      if ("Linear" %in% input$regressionMethodCheckBox) {
        mg.lin <- lm(lifeexp ~ gdp_c, data=data)
        model.lin.sum <- summary(mg.lin)
        result_table <- rbind(result_table, c("Linear Regression", round(model.lin.sum$r.squared, 3), round(model.lin.sum$adj.r.squared, 3), model.lin.sum$df[2]))
      }
      if ("Cubic" %in% input$regressionMethodCheckBox) {
        mg.cub <- lm(lifeexp ~ poly(gdp_c, 3, raw=TRUE), data=data)
        model.cub.sum <- summary(mg.cub)
        result_table <- rbind(result_table, c("Cubic Polynomial Regression", round(model.cub.sum$r.squared, 3), round(model.cub.sum$adj.r.squared, 3), model.cub.sum$df[2]))
      }
      if ("Polynomial" %in% input$regressionMethodCheckBox) {
        mg.ns <- lm(lifeexp ~ ns(gdp_c, 3), data=data)
        model.ns.sum <- summary(mg.ns)
        result_table <- rbind(result_table, c("Natural Splines", round(model.ns.sum$r.squared, 3), round(model.ns.sum$adj.r.squared, 3), model.ns.sum$df[2]))
      }
      
      colnames(result_table) <- c("Regression method", "R-squared", "Adj R-squared", "df")
      result_table
    })
    
    output$stattable_h <- renderTable({
      if (!("Linear" %in% input$regressionMethodCheckBox) & !("Cubic" %in% input$regressionMethodCheckBox) & !("Polynomial" %in% input$regressionMethodCheckBox)) {
        return()
      }
      
      data <- filtered_hexp()
      result_table <- matrix(nrow=0, ncol=4)
      if ("Linear" %in% input$regressionMethodCheckBox) {
        mh.lin <- lm(lifeexp ~ health.exp, data=data)
        model.lin.sum <- summary(mh.lin)
        result_table <- rbind(result_table, c("Linear Regression", round(model.lin.sum$r.squared, 3), round(model.lin.sum$adj.r.squared, 3), model.lin.sum$df[2]))
      }
      if ("Cubic" %in% input$regressionMethodCheckBox) {
        mh.cub <- lm(lifeexp ~ poly(health.exp, 3, raw=TRUE), data=data)
        model.cub.sum <- summary(mh.cub)
        result_table <- rbind(result_table, c("Cubic Polynomial Regression", round(model.cub.sum$r.squared, 3), round(model.cub.sum$adj.r.squared, 3), model.cub.sum$df[2]))
      }
      if ("Polynomial" %in% input$regressionMethodCheckBox) {
        mh.ns <- lm(lifeexp ~ ns(health.exp, 3), data=data)
        model.ns.sum <- summary(mh.ns)
        result_table <- rbind(result_table, c("Natural Splines", round(model.ns.sum$r.squared, 3), round(model.ns.sum$adj.r.squared, 3), model.ns.sum$df[2]))
      }
      
      colnames(result_table) <- c("Regression method", "R-squared", "Adj R-squared", "df")
      result_table
    })

})
