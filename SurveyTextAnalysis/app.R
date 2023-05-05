# Copyright (C) 2022 Md Ayubur Rahman Khan, The University of Toledo - All Rights Reserved
# 
# This program is free software: you can redistribute it and/or modify it under the terms of 
# the GNU General Public License as published by the Free Software Foundation, either version 3 of 
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without 
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with this program. 
# If not, see <https://www.gnu.org/licenses/>

# Copyright (C) 2023 Daniel R Getty, The University of Michigan - Flint - All Rights Reserved

# Updates:
# Custom color to maize & Blue - Done
# Add Institutional Analysis logo - Done
# import template link - Done
# convert template and upload data format to Pipe Separate Value (PSV) - Done
# Remove Bar chart for Most common words - Done

# To do:
# add sentiment analysis functionality
# add test data link - Done
# add directions

############################ Load Library#######################################

library(bslib)
library(shiny)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(wordcloud)
# library(sentimentr)
library(igraph)
library(ggraph)
library(textstem)
library(DT)

# Define the theme colors
primary_color <- "#CFC096"
secondary_color <- "#174992"
background_color <- "#00274C"
foreground_color <- "#FFCB05"
success_color <- "#A5A508"
info_color <- "#575294"
warning_color <- "#D86018"
white_color <- "#FFFFFF"

# Define the theme
my_theme <- bs_theme(
  primary = primary_color,
  secondary = secondary_color,
  bg = background_color,
  fg = white_color,
  font = "Arial",
  base_font_size = "14px",
  link_color = primary_color,
  link_hover_color = info_color,
  btn_primary_bg = primary_color,
  btn_primary_border = primary_color,
  btn_primary_hover_bg = info_color,
  btn_primary_hover_border = info_color,
  input_padding_y = ".375rem",
  input_padding_x = ".75rem",
  input_font_size = "inherit",
  input_border_radius = ".25rem",
  input_border_width = "1px",
  input_border_color = primary_color,
  input_border_color_focus = info_color,
  input_box_shadow_focus = "0 0 0 .2rem rgba(0, 123, 255, .25)",
  input_disabled_bg = primary_color,
  input_disabled_border_color = primary_color,
  input_disabled_text_color = warning_color,
  output_error_color = warning_color,
  output_success_color = success_color,
  output_warning_color = warning_color
)

# Define UI for data upload app ----
ui <- fluidPage(
theme = my_theme, # Apply the custom theme
title = "Open-Ended Survey Data Analysis Tool",
tags$style(HTML(".header { background-color: white; text-align: center; font-size: 24px; padding: 10px; }")),
tags$div(class = "header", tags$figure(align = "Left", tags$img(src = "logo_IA.jpg", height = 117, width = 685, alt = "IA Logo"))),
tags$h1("Open-Ended Survey Data Analysis Tool"),
tabsetPanel(
############################ Tab 1 : Input-1 ###################################
tabPanel("Upload and Preview Survey Results", fluid = TRUE,
# App title ----
titlePanel(h3("Upload Survey Results")),
# Sidebar layout with input and output definitions ----
sidebarLayout(
# Sidebar panel for inputs ----
sidebarPanel(width = 3,
# Download links ----
downloadLink("AboutSurveyAnalyisTool", "About"),
tags$div(style="margin-bottom:10px"),  #----- adding margin between buttons
tags$hr(), 
downloadLink("responseTemplate", "Response Template"),
tags$div(style="margin-bottom:10px"),  #----- adding margin between buttons
tags$hr(),
downloadLink("AI_On_AI", "Sample AI Data"),
tags$div(style="margin-bottom:10px"),  #----- adding margin between buttons
downloadLink("course_data", "Sample Courses Data"),
tags$div(style="margin-bottom:10px"),  #----- adding margin between buttons
tags$hr(),             
# Input: Select a file ----
fileInput("file1", "Choose Pipe Seperated Values (PSV) File",
multiple = FALSE,
# accept = c("text/csv","text/comma-separated-values,text/plain",".psv")),
accept = c(".psv")),
# Horizontal line ----
tags$hr(),
downloadLink("license", "License"),
tags$div(style="margin-bottom:10px"),  #----- adding margin between buttons
),
# Main panel for displaying outputs ----
mainPanel(width = 9,
# Output: Data file ----
tags$i(textOutput("out.1.2")),
tags$i(textOutput("out.1.3")),
dataTableOutput("out.1")
))),

######################## Tab 2 : Data Pre-Processing ###########################

tabPanel("Stemming and Stop Word Removal", fluid = TRUE,
# App title ----
titlePanel(h3("Process Uploaded Data")),
# Sidebar layout with input and output definitions ----
sidebarLayout(
# Sidebar panel for inputs ----
sidebarPanel(width = 3,
tags$b("Step-01:"),
tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons
selectInput("SL1",
label = "Select Question to Analyze",
multiple = FALSE,
choices = c("")
),
tags$div(style="margin-bottom:15px"),  #----- adding margin between buttons
# tags$h4(textOutput("out.2.1")),
tags$div(style="margin-bottom:15px"),
selectInput("SL2",
label = "Filter By",
multiple = FALSE,
choices = c("")
),
tags$div(style="margin-bottom:15px"),  #----- adding margin between buttons
selectInput("SL3",
label = "Select Filter Category",
multiple = TRUE,
choices = c("")
),
tags$div(style="margin-bottom:150px"),  #----- adding margin between buttons
tags$b("Step-02:"),
tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons
actionButton("BT2.1", "Run Tokenization"),
tags$div(style="margin-bottom:150px"),  #----- adding margin between buttons
tags$b("Step-03:"),
tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons
actionButton("BT2.2", "Stemming and Stop Word Removal"),
tags$div(style="margin-bottom:200px")  #----- adding margin between buttons
),
# Main panel for displaying outputs ----
mainPanel(width = 9,
# Output: Data file ----
fluidRow(align = "left",
column(6, 
# display selected question
tags$h4(tags$b("Selected Question: ")),
tags$h4(textOutput("out.2.1")),
tags$div(style="margin-bottom:50px"),  #----- adding margin between buttons
# display sample responses
tags$h4(tags$b("Sample Responses: ")),
dataTableOutput("out.2.2")
),
column(3, 
# tags$div(style="margin-bottom:100px"),
# display tokenized sample responses
tags$h4(tags$b("Tokenization: ")),
dataTableOutput("out.2.3"),
tags$div(style="margin-bottom:5px")  #----- adding margin between buttons
),
column(3, 
# display tokenized sample responses without stop words
tags$h4(tags$b("Stemming and Stop Word Removal: ")),
dataTableOutput("out.2.4"),
tags$div(style="margin-bottom:5px")  #----- adding margin between buttons
))))),

############################ Tab 3 : N-Grams ###################################

tabPanel("N-Grams", fluid = TRUE,
# App title ----
titlePanel(h3("N-Gram Analysis")),
# Sidebar layout with input and output definitions ----
sidebarLayout(
# Sidebar panel for inputs ----
sidebarPanel(width = 3,
tags$h4(tags$b("Unigram Analysis")),
tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons
tags$b("Step-01:"),
tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons
actionButton("BT3.1", "Find Most Common Words"),
tags$div(style="margin-bottom:250px"),  #----- adding margin between buttons
tags$b("Step-02:"),
tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons
actionButton("BT3.2", "Show Word Cloud"),
tags$div(style="margin-bottom:250px")  #----- adding margin between buttons
),
# Main panel for displaying outputs ----
mainPanel(width = 9,
# Output: Data file ----
fluidRow(align = "left",
column(3, 
# display most common words
tags$h4(tags$b("Most Common Words: ")),
dataTableOutput("out.3.1"),
tags$div(style="margin-bottom:25px")  #----- adding margin between buttons
),
column(1, 
tags$div(style="margin-bottom:25px")  #----- adding margin between buttons
),
# column(3,
# # display bar plot
# tags$h4(tags$b("Barplot of Most Common Words: ")),
# plotOutput("out.3.2", width = "120%", height = "600px"),
# tags$div(style="margin-bottom:25px")  #----- adding margin between buttons
# ),


# column(3,
# # display sentiment analysis
# tags$h4(tags$b("Sentiment Analysis: ")),
# plotOutput("out.3.2", width = "120%", height = "600px"),
# tags$div(style="margin-bottom:25px")  #----- adding margin between buttons
# ),


column(1, 
tags$div(style="margin-bottom:25px")  #----- adding margin between buttons
),
column(4, 
# display word cloud
tags$h4(tags$b("Word Cloud: ")),
plotOutput("out.3.3", width = "120%", height = "700px"),
tags$div(style="margin-bottom:5px")  #----- adding margin between buttons
)))),
# Sidebar layout with input and output definitions ----
sidebarLayout(
# Sidebar panel for inputs ----
sidebarPanel(width = 3,
tags$h4(tags$b("Bigram Analysis")),
tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons
tags$b("Step-01:"),
tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons
actionButton("BT3.3", "Find Most Common Bigrams"),
tags$div(style="margin-bottom:250px"),  #----- adding margin between buttons
tags$b("Step-02:"),
tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons
actionButton("BT3.4", "Show Network Graph"),
tags$div(style="margin-bottom:10px"),  #----- adding margin between buttons
numericInput("n_bigram", "Cut-off Frequency (n):", 2, min = 1, max = 3),
tags$div(style="margin-bottom:250px")  #----- adding margin between buttons
),
# Main panel for displaying outputs ----
mainPanel(width = 9,
# Output: Data file ----
fluidRow(align = "left",
column(3, 
# display most common words
tags$h4(tags$b("Most Common Bigrams: ")),
dataTableOutput("out.3.4"),
tags$div(style="margin-bottom:5px")  #----- adding margin between buttons
),
column(9, 
# display network graph
tags$h4(tags$b("Network Graph: ")),
plotOutput("out.3.5", width = "100%", height = "600px"),
tags$div(style="margin-bottom:5px")  #----- adding margin between buttons
)))),
# Sidebar layout with input and output definitions ----
sidebarLayout(
# Sidebar panel for inputs ----
sidebarPanel(width = 3,
tags$h4(tags$b("Text Exploration")),
tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons
tags$b("Step-01:"),
tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons
selectInput("SL3.1",
label = "Explore By:",
multiple = FALSE,
choices = c("Words", "Bigrams"),
selected = "Bigrams"
),
tags$div(style="margin-bottom:50px"),  #----- adding margin between buttons
tags$b("Step-02:"),
tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons
selectInput("SL3.2",
label = "Select Word/Bigram:",
multiple = FALSE,
choices = c("")
),
tags$div(style="margin-bottom:30px"),  #----- adding margin between buttons
actionButton("BT3.5", "Show Results"),
tags$div(style="margin-bottom:30px"),  #----- adding margin between buttons
downloadLink("downloadData", "Download Filtered Responses"),
tags$div(style="margin-bottom:350px")  #----- adding margin between buttons
),
# Main panel for displaying outputs ----
mainPanel(width = 9,
# display most common words
tags$h4(tags$b("Responses Consisting of Selected Word/Bigram: ")),
tableOutput("out.3.6"),
tags$div(style="margin-bottom:5px")  #----- adding margin between buttons
)))))

#--------------------------    SERVER    --------------------------------------#

# Define server logic to read selected file ----
server <- function(input, output, session) {
observe({

########################### Tab 1 : Input-1 ####################################

  output$AboutSurveyAnalyisTool <- downloadHandler(
    filename = function(){"AboutSurveyAnalyisTool.pdf"},
    content = function(file){
      myfile <- srcpath <-  "www/Documentation/AboutSurveyAnalyisTool.pdf"
      file.copy(myfile, file)
    })
  
  output$license <- downloadHandler(
    filename = function(){"LICENSE.TXT"},
    content = function(file){
      myfile <- srcpath <-  "www/Documentation/LICENSE.TXT"
      file.copy(myfile, file)
    })
  
  output$responseTemplate <- downloadHandler(
    filename = function(){"ResponseTemplate.psv"},
    content = function(file){
      myfile <- srcpath <-  "www/Sample_Data/ResponseTemplate.psv"
      file.copy(myfile, file)
    })
  
  output$AI_On_AI <- downloadHandler(
    filename = function(){"AI_On_AI.psv"},
    content = function(file){
      myfile <- srcpath <-  "www/Sample_Data/AI_On_AI.psv"
      file.copy(myfile, file)
    })
  
  output$course_data <- downloadHandler(
    filename = function(){"course_data.psv"},
    content = function(file){
      myfile <- srcpath <-  "www/Sample_Data/course_data.psv"
      file.copy(myfile, file)
    })
  
output$out.1 <- renderDataTable({ 

# input$file1 will be NULL initially. After the user selects
# and uploads a file, head of that data file by default,
# or all rows if selected, will be shown.
req(input$file1)
# when reading semicolon separated files,
# having a comma separator causes `read.csv` to error
tryCatch(
{
# df1 <- read.csv(input$file1$datapath)
df1 <- read.delim(input$file1$datapath, header= TRUE, sep = '|')
},
error = function(e) {
# return a safeError if a parsing error occurs
stop(safeError(e))
})
df1
})
getData1 <- reactive({
inFile <- input$file1
if (is.null(input$file1))
return(NULL)
# read.csv(inFile$datapath, header=input$header1, stringsAsFactors = T)
# read.csv(inFile$datapath, stringsAsFactors = FALSE)
read.delim(inFile$datapath, sep = '|', stringsAsFactors = FALSE)
})
# uploaded data set
dat = reactive({
dat = getData1()
dat
})

####################### Tab 2 - Data Pre-Processing ############################

# select survey question to analyze
observeEvent(input$file1,{
updateSelectInput(session, "SL1", label = NULL, choices = colnames(dat()), selected = "")
})
output$out.2.1 <- renderText({
dat = dat()
q = dat[c(1), c(input$SL1)]
paste(q)
})

# filter by
observeEvent(input$file1,{
updateSelectInput(session, "SL2", label = NULL, choices = colnames(dat()), selected = "")
})

# select category
observeEvent(input$SL2,{
dat = dat()
cat.filter = unique(dat[-c(1), c(input$SL2)])
updateSelectInput(session, "SL3", label = NULL, choices = cat.filter, selected = NULL)
})

# convert text response into data frame
text_df = reactive({
  if (is.null(input$SL1 == ""))
    return(NULL)
  if (input$SL2 == "") {
    dat = dat()
    dat.q = dat[-c(1, 2), c(input$SL1)]
    dat.q = dat.q[dat.q != ""]
    text_df <- tibble(line = 1:length(dat.q), text = dat.q)
    text_df
  } else {
    dat = dat()
    dat.q = dat[-c(1, 2),]
    dat.q = dat.q[dat.q[, c(input$SL2)] %in% input$SL3,]
    dat.q = dat.q[, c(input$SL1)]
    dat.q = dat.q[dat.q != ""]
    text_df <- tibble(line = 1:length(dat.q), text = dat.q)
    text_df
  }
})
output$out.2.2 <- renderDataTable({
  text_df()
})

# tokenization
text_tk = eventReactive(input$BT2.1,{
if(is.null(input$BT2.1)){
return()
}
text_df = text_df()
text_tk = text_df %>%
unnest_tokens(word, text)
text_tk
})
output$out.2.3 <- renderDataTable({
text_tk()
})

# remove stop words
text_rsw = eventReactive(input$BT2.2,{
if(is.null(input$BT2.2)){
return()
}
text_tk = text_tk()
data(stop_words)
text_rsw <- text_tk %>%
anti_join(stop_words)

# lemmatization
text_rsw$word = lemmatize_words(text_rsw$word)
text_rsw 
})
output$out.2.4 <- renderDataTable({
text_rsw()
})

######################### Tab 3 - N-Gram Analysis ##############################

# most common words
text_common = eventReactive(input$BT3.1,{
if(is.null(input$BT3.1)){
return()
}
text_rsw = text_rsw()
text_common = text_rsw %>%
count(word, sort = TRUE)
text_common
})
output$out.3.1 <- renderDataTable({
text_common()
})

# bar plot of most common words
# plot.common <- eventReactive(input$BT3.1,{
# if(is.null(input$BT3.1)){
# return()
# }
# text_rsw = text_rsw()
# set.seed(1)
# plot.common = text_rsw %>%
# count(word, sort = TRUE) %>%
# # filter(n > 100) %>%
# head(20) %>%
# mutate(word = reorder(word, n)) %>%
# ggplot(aes(word, n)) +
# geom_col() +
# xlab(NULL) +
# coord_flip() +
# theme(axis.text.y = element_text(size=16), axis.text.x = element_text(size=16))
# plot.common
# })
# output$out.3.2 <- renderPlot({ --This works
# plot.common()
# })

# Sentiment Analysis of most common words
# plot.common <- eventReactive(input$BT3.1,{
# if(is.null(input$BT3.1)){
# return()
# }
# text_rsw = text_rsw()
# set.seed(1)
# plot.common = text_rsw %>%
# count(word, sort = TRUE) %>%
# # filter(n > 100) %>%
# head(20) %>%
# mutate(word = reorder(word, n)) %>%
# ggplot(aes(word, n)) +
# geom_col() +
# xlab(NULL) +
# coord_flip() +
# theme(axis.text.y = element_text(size=16), axis.text.x = element_text(size=16))
# plot.common
# })
# output$out.3.2 <- renderPlot({ --This works
# plot.common()
# })

# word cloud plot
plot.word_cloud <- eventReactive(input$BT3.2,{
if(is.null(input$BT3.2)){
return()
}
text_rsw = text_rsw()
plot.word_cloud = text_rsw %>%
count(word) %>%
with(wordcloud(word, n, max.words = 50))
plot.word_cloud
})
output$out.3.3 <- renderPlot({
plot.word_cloud()
})
# most common bigrams
text_bigram = eventReactive(input$BT3.3,{
if(is.null(input$BT3.3)){
return()
}
text_df = text_df()
# tokenization
text_bigram = text_df %>%
unnest_tokens(bigram, text, token = "ngrams", n = 2)
# remove stop words
## split bigrams into words
text_bigram <- text_bigram %>%
separate(bigram, c("word1", "word2"), sep = " ")
## remove cases where either is a stop word
data(stop_words)
text_bigram <- text_bigram %>%
filter(!word1 %in% stop_words$word) %>%
filter(!word2 %in% stop_words$word)
# Lemmatization
text_bigram$word1 = lemmatize_words(text_bigram$word1)
text_bigram$word2 = lemmatize_words(text_bigram$word2)
# new bigram counts:
text_bigram <- text_bigram %>%
count(word1, word2, sort = TRUE)
text_bigram
})
# combine bigrams
bigram_recombined = eventReactive(input$BT3.3,{
if(is.null(input$BT3.3)){
return()
}
text_bigram = text_bigram()
bigram_recombined <- text_bigram %>%
unite(bigram, word1, word2, sep = " ")
bigram_recombined = bigram_recombined[bigram_recombined$bigram != "NA NA", ]
bigram_recombined
})
output$out.3.4 <- renderDataTable({
bigram_recombined()
})
# network graph of bigrams
plot.network <- eventReactive(input$BT3.4,{
if(is.null(input$BT3.4)){
return()
}
text_bigram = text_bigram()
# remove NA NA
text_bigram = text_bigram[-c(1),]
set.seed(1)
plot.network = text_bigram %>%
filter(n >= input$n_bigram) %>%
graph_from_data_frame() %>%
ggraph(layout = "fr") +
geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
geom_node_point(size = 5) +
geom_node_text(aes(label = name), repel = TRUE,
point.padding = unit(0.1, "lines")) +
theme_void()
plot.network
})
output$out.3.5 <- renderPlot({
plot.network()
})
# text exploration by words/bigrams
observe({
if(input$SL3.1 == "Words"){
text_choice = text_common()
} else if(input$SL3.1 == "Bigrams"){
text_choice = bigram_recombined()
}
updateSelectInput(session, "SL3.2", label = NULL, choices = text_choice, selected = NULL)
})
text_exp <- eventReactive(input$BT3.5,{
if(is.null(input$BT3.5)){
return()
}
if(input$SL2 == ""){
dat = dat()
text.q = dat[-c(1,2), ]
text.q = text.q[ , c(input$SL1)]
text.q = text.q[text.q != ""]
text.lem = lemmatize_strings(text.q) #--------lemmatize before text search
text_exp = text.q[grepl(input$SL3.2, text.lem, ignore.case = TRUE)]
text_exp = data.frame("Responses" = text_exp)
#colnames(text_exp) = c("filtered_responses")
text_exp
} else {
dat = dat()
text.q = dat[-c(1,2), ]
text.q = text.q[text.q[, c(input$SL2)] %in% input$SL3, ]
text.q = text.q[ , c(input$SL1)]
text.q = text.q[text.q != ""]
text.lem = lemmatize_strings(text.q) #--------lemmatize before text search
text_exp = text.q[grepl(input$SL3.2, text.lem, ignore.case = TRUE)]
text_exp = data.frame("Responses" = text_exp)
#colnames(text_exp) = c("filtered_responses")
text_exp
}})
output$out.3.6 <- renderTable({
text_exp()
})
output$downloadData <- downloadHandler(
filename = function(){"filtered-responses.csv"}, 
content = function(fname){
write.csv(text_exp(), fname)
})
})}
# Create Shiny app ----
shinyApp(ui, server)
