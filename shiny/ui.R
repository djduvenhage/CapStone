
library(shiny)

#IMPORTANT, set which model file being worked with
#source("backoffR1.R")
#source("backoffR2.R")
source("backoffR2SSBO.R")

########################################################################################################################################## 


# Use a fluid Bootstrap layout
fluidPage(  
        
        #Condition panel code that uses "uiOutput("predictor")" to check if system is busy
        #Displays a busy .gif (from .www file) when loading the data on initial startup of application
        #conditionalPanel("x != -1", tags$img(src="loading_circle.gif")), 
        
        #Give the page a title
        titlePanel("Stupid Backoff Next Word Prediction"),
        
        #Generate a row with a sidebar
        fluidRow(
                
                # Define the sidebar with one input
                column(4, wellPanel(
                        
                        h4("Predict next word from user input:"),
                        h5("A simple backoff 'next word' prediction model is used to predict the next likely word from a word or phrase. The model applies pre-sorted, 'Stupid Backoff Ranked' ordered words, from highest to lowest frequency, to predict the next likely word." , style="color:blue"),
                        h4("Procedure:"),
                        h5(" 1. First the provided phrase is reduced to the last four words, and the 'next likely word' search form a pentagram of words."),
                        h5(" 2. If no suitable match is found in the pentagram, the model search 'back's off' to a tetragram, i.e. using the last three words, from the user provided phrase, to predict the next best match."),
                        h5(" 3. If no tetragram match is found, the model 'back's off' to a trigram, i.e. using the last two words from the user provided phrase, to predict a suitable match."),
                        h5(" 4. If no bigram word is found, the model default 'back's off' to the highest frequency unigram word available."),
                        hr(),
                        textInput("testString", label = h4("Enter word or Phrase:"), value = ""),
                        hr(),
                        submitButton("Submit"),
                        hr(),
                        helpText("Owner: Dawid J. Duvenhage"),
                        helpText("Date: 12-Nov-2017")
                       
                         )
                ),
                
                #Create left main panel space for results
                column(4, wellPanel (
                        
                        h4("Your sentence 'cleaned':"),
                        verbatimTextOutput("sentence"),
                        hr(),
                        h4("Top 10  predicted responses:", style="color:blue"),
                        verbatimTextOutput("prediction"), 
                        h6("Notes:"),
                        h6("1. a single available Top 1 match is acommpanied by <NA> & NA"),
                        h6("2. if no match found the system returns 'NULL'"),
                        h6("3. the 'NULL' below the 10th entry, when a positive match is made, is a coding artifact"),
                        h6("4. 'modelUsed' indicates how far the model backed back from initial phrase input")
                        
                        )
                ),
                
                #Create right main panel space for examples
                column(4,
                       
                        h4("App 'test string' Evaluation:", style="color:blue"),
                        h5("Test String...................................Word to Predict................Actual Prediction"),
                        h5("----------------------------------------------------------------------------------------------"),
                        h5("'a bat chucked with a forceful'...............'throw'........................1st"),
                        h5("'a beautiful day in the'..........................'neighborhood'...........1st"),	
                        h5("'a phone call'........................................'returned'................< 10th"),
                        h5("'a personal trainer we'...........................'know'.........................?"),
                        h5("'persisting marriage till old'....................'age'...........................?"), 
                        h5("'playlist of every'...................................'song'.....................< 10th"),
                        h5("'johnny depp in donnie'.........................'brasco'......................1st"),
                        h5("'doing anything in the'...........................'evening'....................1st"),
                        h5("'i wasnt without hope this'.....................'year'..........................1st"),
                        h5("'including four football players we'.........'arrested'..................1st"),
                        h5("'interviews instead of'...........................'observation'...............?"),
                        h5("'learn about the world around'..............'them'.........................1st"),
                        h5("-----------------------------------------------------------------------------------------------"),
                        h5("Note: 1st:- in Top 10 / < 10th:- not in top 10 / ? : not predicted")
                      
                        )
                )
                
        )
        

           