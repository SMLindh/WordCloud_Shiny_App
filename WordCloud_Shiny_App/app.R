library(shiny)
library(shinydashboard)
library(RedditExtractoR)
library(DT)
library(ggplot2)
library(dplyr)
library(tidytext)
library(wordcloud2)

ui <- dashboardPage(skin = 'black',
                    dashboardHeader(title = "Subreddit Analysis"),
                    #start of dashboard sidebar
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Intro", tabName = "intro", icon = icon("dashboard")),
                            menuItem("Load Data", tabName = "dataload", icon = icon("dashboard")),
                            menuItem("Frequency Charts", tabName = "freq", icon = icon("dashboard")),
                            menuItem("Word Cloud", tabName = "wordcloud", icon = icon("dashboard"))
                            
                        )),#dashboard sidebar end
                    #Dashboard body begin
                    dashboardBody(
                      #TabItems start
                      tabItems(
                        #Creating the introduction tab of the dashboard
                        tabItem(tabName = 'intro', 
                                fluidRow(
                                  column(12,box(h3("Welcome to RSA."),
                                      p("This is a simple app to help you see
                                        what your favorite subreddits are talking about. First, you
                                        will load in the data from the subreddit you want to talk about.
                                        This will take some time due to restrictions set by the reddit API.
                                        The Frequency Charts tab will give you a nice plot of the top words.
                                        The Word Cloud tab will give a nice visual for seeing these words.
                                        Go into the Load Data tab to start.")))
                                )
                                ),#Intro tab end
                        #Creating second (dataloader) tab of dashboard
                        tabItem(tabName = 'dataload',
                                #Fluidrow start
                                fluidRow(box(width = 900,
                                             #Input for entering a subreddit
                                             column(2,textInput('subreddit', 'Enter a Subreddit', 'Wallstreetbets')),
                                             #input for selecting sort type (new or by commments)
                                             column(2,selectInput('sort', 'Sort By',
                                                                  choices = c('comment', 'new'), selected = 'new')),
                                             #input for search terms
                                             column(2,textInput("st", "Enter a Search Term", "")),
                                             #input for page threshold. greater #'s significantly increase run time
                                             column(2,numericInput('pagethresh', "# of Pages Scraped", 2)),
                                             #input for what you want to scrape (text or comments)
                                             column(2,selectInput('textorcomment', 'Scrape By',
                                                                  choices = c('title', 'comment','post_text'), selected = 'title')),
                                             #input for action button to load data
                                             column(12, actionButton('getdata', 'Load Data'), align = 'center',
                                                    actionButton('savecsv', 'Download Data'))),
                                         #outputs the table
                                         dataTableOutput('mastertable')
                                )
                                ),
                        
                        #Starts frequency charts tab
                        tabItem(tabName = 'freq', fluidRow(
                          #Number of words in col plot input + start fluidrow
                          box(numericInput('numwords', '# of Top Words', 15, width = 100),
                          #Col plot    
                          box(plotOutput('freqplot'), width = 200)), 
                        )),#End tabitem
                        #Start of wordcloud tab
                        tabItem(tabName = 'wordcloud',
                                #Start fluidrow
                                fluidRow(
                                  #Output for wordcloud plot
                                  column(12,box(wordcloud2Output('wcplot'))),
                                  box(
                                    #Inputs for parameters to wordcloud function
                                    column(3,numericInput('numwords2','# of Words',50)),
                                    column(3,numericInput('gridsize', 'gridsize', 1)),
                                    column(3,numericInput('rotate', "rotate",2, min =0,max = 1, step =.05)),
                                    column(3,numericInput('ellipticity', "ellipticity",1)),
                                    column(3,numericInput('minrot', "minrot",1)),
                                    column(3,numericInput('maxrot', "maxrot",1)),
                                    column(3,selectInput('shape', 'Cloud Shape',
                                                         choices = c('circle', 'cardoid', 'diamond',
                                                                     'triangle-forward','star','pentagon'), 
                                                         selected = 'circle')),
                                    column(3,selectInput('bgcolor', 'bgcolor',
                                                         choices = c('black', 'white', 'grey',
                                                                     'blue'), 
                                                         selected = 'black')),
                                    column(3,selectInput('textcolor', 'textcolor',
                                                         choices = c('random-light','random-dark'), 
                                                         selected = 'random-light')),
                                    column(3,actionButton('save', "Save Cloud")),
                                    
                                  )
                                ))
                      )
))



server <- function(input, output) {
  
  #Observer for press of load data button
  #Want to eventually change the structure of this to get rid of everything being in the observe event
  #Also want to make it so there is a default dataset loaded. The csv is already in this WD called 
  #default.csv
  observeEvent(input$getdata,{
    #Using redditextractoR package to read in data from subreddit choseen
    reddit_urls = reddit_urls(subreddit =input$subreddit, page_threshold = input$pagethresh)
    #progress bar. probably get rid of this
    withProgress(message = 'Loading Data',{my_data = reddit_content(reddit_urls$URL)})
    #takes a subset of columns for the data table 
    cleaner_data = my_data[,c(3,6,13,14,15)]
    #outputs the data table
    output$mastertable = renderDataTable({
      cleaner_data
    })
    #unnesting tokens > remove stop words > filter custom words > count freqs > sort desc
    #I want to append my custom words to the stop_words list
    a = cleaner_data %>% 
      unnest_tokens(word, input$textorcomment) %>% 
      anti_join(stop_words) %>% 
      filter(word != '3', word!= '2', word != 'aggressive', word != 'amp', word!= 'û', word != 'ž',
             word != '11', word != '12', word != '13', word!= "lol", word !='don',
             word!='de', word != 'gt', word != 'https', word != '100', word != '20', word != '10',
             word != '5', word != '1', word !='http', word != 'comments', word != 'en.wikipedia.org',
             word != 'np.reddit.com', word != 'll', word != 've', word != '30', word != '2021') %>% 
      count(word) %>% 
      arrange(desc(n))
    #Outputs the flipped col plot from previous data
    output$freqplot = renderPlot({
      ggplot(a[1:input$numwords,],aes(x= reorder(word,n), y = n)) + geom_col(fill = 'green') + coord_flip()+ theme_classic()+
        theme(text = element_text(size=10)) 


    })
    #outputting the wordcloud plot
    output$wcplot = renderWordcloud2({
        wordcloud2(a[1:input$numwords2,],
                   size = 1, rotateRatio = input$rotate, shape = input$shape,
                   ellipticity = input$ellipticity, color = input$textcolor, backgroundColor = input$bgcolor,
                   gridSize = input$gridsize, minRotation = pi / input$minrot, maxRotation = -pi / input$maxrot)
      })

    })

}

# Run the application 
shinyApp(ui = ui, server = server)
