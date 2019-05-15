library(shiny)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(shinythemes)

matches = read.csv("matches.csv", stringsAsFactors = TRUE)
deliveries = read.csv("deliveries.csv", stringsAsFactors = TRUE)

df = merge(matches, deliveries, by.x = "id", by.y = "match_id")
df$season = as.factor(df$season)

batsman_list = as.list(levels(df$batsman))

functions_list = c("Runs by Seasons", "Dismissals by Seasons", "Strike Rate Plots", "Favorite Venue", "Favorite Bowlers")

ui <- fluidPage(
  
  theme = shinytheme("slate"),
  setBackgroundImage(src = "https://storage.googleapis.com/news-photo/1515093585_4GPndc_IPL_pic.JPG"),
  titlePanel("IPL Batsmen Analytics"),
  
  sidebarPanel(
    selectInput("batsman", "Batsman", choices = batsman_list),
    selectInput("function_name", "Function", choices = functions_list),
    actionButton("button", "Analyze!"),
    br(),br()),

  column(10, plotlyOutput("plot"))
)
