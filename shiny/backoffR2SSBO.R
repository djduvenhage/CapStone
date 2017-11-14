##########################################################################################################################################
#Main Code Ver 2 [Multiple seek on Individual models]
##########################################################################################################################################
#A1. Loading libraries

#Hide information messages
options( warn = -1 )
#suppressMessages(library(readtext))      #reading text files 
suppressMessages(library(stringr))       #to clean up simple strings
suppressMessages(library(tidytext)) 
suppressMessages(library(tidyr)) 
suppressMessages(library(knitr))         #to resolve issue of R-Markdown File Not Knitting
suppressMessages(library(data.table))

#A2. Setup working Directory

#setwd("C:/Users/Dawid J Duvenhage/Desktop/Coursera Courses/Data Scientist Specialization/10_Cap Stone Project")

#A3. Loading ngram libraries [IMPORTANT: Enusre system is loading the correct file set]

tdm1 <- read.table("tdm1SSBO.txt", stringsAsFactors=FALSE)
tdm2 <- read.table("tdm2SSBO.txt", stringsAsFactors=FALSE)
tdm3 <- read.table("tdm3SSBO.txt", stringsAsFactors=FALSE)
tdm4 <- read.table("tdm4SSBO.txt", stringsAsFactors=FALSE)
tdm5 <- read.table("tdm5SSBO.txt", stringsAsFactors=FALSE)

##########################################################################################################################################
#B. FUNCTIONS
##########################################################################################################################################
#B1. Output Function:

output <- function(tdmWordTop10, modelUsed, tdmFreqTop10){
        
                         print(data.frame(tdmWordTop10= tdmWordTop10[1:10], 
                                                        modelUsed= modelUsed[1:10], 
                                                                tdmFreqTop10= tdmFreqTop10[1:10]), 
                                                                        print.gap=3, quote=F, right=F, sep="\n")
                                       
        
return("Stupid Backoff Model Result")}



          

##########################################################################################################################################
#B2.User Input String Cleaning Function:

stringCleaned <- function(testString) {
        
                                        #6.1 Removing non ASCI strings
                                        testString <- iconv(testString, "latin1", "ASCII", sub="")
        
                                        #6.2 Convert the text to lower case:
                                        testString <- tolower(testString)
        
                                        #6.3 Removing special characters & punctuations & numbers:
                                        testString <- stringr::str_replace_all(testString,"[^a-zA-Z\\s]", " ")
        
                                        #6.4 Eliminate extra white spaces:
                                        testString <- stringr::str_replace_all(testString,"[\\s]+", " ")
        
                                        #6.5 Cleaned Test String
                                        testString <- stringr::str_split(testString, " ")[[1]]
                                        testString_cleaned <- paste(testString, collapse=" ", sep="")
        
return(testString_cleaned)}


##########################################################################################################################################
#B3. MAIN INPUT DECISION CODE

back_off <- function(testString) {
                                        
                                        #If a large string is user input, truncate to last three words
                                        if (sapply(strsplit(testString, " "), length) > 4) {
                                                #keep last 4 words of string
                                                testString <- word(testString, -4, -1)
                                        }        
                                       
                                        #Call function to clean input string
                                        string_cleaned <- stringCleaned(testString)
                                        #REmove "" from cleaned string
                                        testString_cleaned <- paste(string_cleaned, collapse=" ", sep="")
                                        #Count number of words in string
                                        testStringWordCount <- sapply(strsplit(testString_cleaned, " "), length)
                                        #Split testString into 2, 3, 4, & 5 words ngrame equivalent combinations
                                        testString_words <- unlist(strsplit(testString_cleaned, split=" "))
        
                                        #Create place holders that clear between runs
                                        result <- as.character()
                                         rm(result)
        
                                        #Start of Stupid Backoff code decision making
                                         #######
                                         #Start here if a four word phrase is fed
                                            if  (testStringWordCount == 4) {
                                                 #Assign user inputs
                                                 tSWord1 <- testString_words[1]
                                                 tSWord2 <- testString_words[2]
                                                 tSWord3 <- testString_words[3]
                                                 tSWord4 <- testString_words[4]
                                                 
                                                 #When a four word pharse match (&, &, &) is made call the penta model to predict fifth word else proceed
                                                 if ((tSWord1 %in% tdm5[, 1] & tSWord2 %in% tdm5[, 2] & tSWord3 %in% tdm5[, 3] & tSWord4 %in% tdm5[, 4]) == TRUE) {
                                                         
                                                         #Call penta model
                                                        try(pentamodel(tSWord1, tSWord2, tSWord3, tSWord4), silent=TRUE)
                                                         
                                                        #withCallingHandlers(message = message_handler, { message("Stupid Backoff Model") })
                                                        
                                                }
                                                 else if 
                                                 #If four word pharse is not a match backoff first word and try to predict (&, &) three words 
                                                 #If match is made call the tetra model to predict fourth word else proceed
                                                 ((tSWord2 %in% tdm4[, 1] & tSWord3 %in% tdm4[, 2] & tSWord4 %in% tdm4[, 3]) == TRUE) {
                                                         #Assign user inputs with backoff
                                                         tSWord1 <- tSWord2
                                                         tSWord2 <- tSWord3
                                                         tSWord3 <- tSWord4  
                                                         
                                                         try(tetramodel(tSWord1, tSWord2, tSWord3), silent=TRUE)
                                                }
                                                 else if 
                                                 #If three word pharse is not a match backoff first word and try to predict (&) two words 
                                                 #If match is made call the tri model to predict third word else proceed
                                                 ((tSWord2 %in% tdm3[, 1] & tSWord3 %in% tdm3[, 2] ) == TRUE) {
                                                         #Assign user inputs with backoff
                                                         tSWord1 <- tSWord2
                                                         tSWord2 <- tSWord3
                                                         #tSWord3 <- "zznotvalidwordzz"   
                                                         
                                                         try(trimodel(tSWord1, tSWord2), silent=TRUE)                                               
                                                 }
                                                 else if 
                                                 #If two word pharse is not a match backoff second word and try to predict one word 
                                                 #If match is made call the bi model to predict second word else proceed
                                                 ((tSWord3 %in% tdm2[, 2] ) == TRUE) {
                                                         #Assign user inputs with backoff
                                                         tSWord1 <- tSWord3
                                                         #tSWord2  <- "zznotvalidwordzz"   
                                                         #tSWord3 <- "zznotvalidwordzz"   
                                                         
                                                         try(bimodel(tSWord1), silent=TRUE)
                                                 }
                                                 else
                                                         #If one word pharse is not a match backoff to unigram and obtain word 
                                                         if ((tSWord3 %in% tdm1[, 1] ) == TRUE) {
                                                                 tSWord1 <- tSWord3
                                                                 #tSWord2  <- "zznotvalidwordzz"   
                                                                 #tSWord3 <- "zznotvalidwordzz"  
                                                                 
                                                                 try(unimodel(tSWord1), silent=TRUE)
                                                         }  
                                                 else
                                                         #If one word pharse is not a match backoff to unigram and obtain word 
                                                         try(unimodel(tSWord1), silent=TRUE)  
                                        
                                        #######
                                        #Start here if a three word phrase is fed
                                        }
                                        else if  (testStringWordCount == 3) {
                                                #Assign user inputs
                                                tSWord1 <- testString_words[1]
                                                tSWord2 <- testString_words[2]
                                                tSWord3 <- testString_words[3]
                
                                                #When a three word pharse match (&, &) is made call the tetra model to predict fourth word else proceed
                                                if ((tSWord1 %in% tdm4[, 1] & tSWord2 %in% tdm4[, 2] & tSWord3 %in% tdm4[, 3] ) == TRUE) {
                                                        
                                                        #Call tetra model
                                                        try(tetramodel(tSWord1, tSWord2, tSWord3), silent=TRUE)
                                                }
                                                else if 
                                                        #If three word pharse is not a match backoff first word and try to predict (&) two words 
                                                        #If match is made call the tri model to predict third word else proceed
                                                        ((tSWord2 %in% tdm3[, 1] & tSWord3 %in% tdm3[, 2] ) == TRUE) {
                                                        #Assign user inputs with backoff
                                                        tSWord1 <- tSWord2
                                                        tSWord2 <- tSWord3
                                                        #tSWord3 <- "zznotvalidwordzz"   
                        
                                                        try(trimodel(tSWord1, tSWord2), silent=TRUE)                                               
                                                }
                                                else if 
                                                        #If two word pharse is not a match backoff second word and try to predict one word 
                                                        #If match is made call the bi model to predict second word else proceed
                                                        ((tSWord3 %in% tdm2[, 2] ) == TRUE) {
                                                        #Assign user inputs with backoff
                                                        tSWord1 <- tSWord3
                                                        #tSWord2  <- "zznotvalidwordzz"   
                                                        #tSWord3 <- "zznotvalidwordzz"   
                        
                                                        try(bimodel(tSWord1), silent=TRUE)
                                                }
                                                else
                                                        #If one word pharse is not a match backoff to unigram and obtain word 
                                                        if ((tSWord3 %in% tdm1[, 1] ) == TRUE) {
                                                                tSWord1 <- tSWord3
                                                                #tSWord2  <- "zznotvalidwordzz"   
                                                                #tSWord3 <- "zznotvalidwordzz"  
                        
                                                                try(unimodel(tSWord1), silent=TRUE)
                                                        }  
                                                        else
                                                                #If one word pharse is not a match backoff to unigram and obtain word 
                                                                try(unimodel(tSWord1), silent=TRUE)        
                                                        
                                        #######
                                        #Start here if a tWO word phrase is fed
                                        }        
                                        else if  
                                                (testStringWordCount== 2) { 
                                                        #Assign user inputs
                                                        tSWord1 <- testString_words[1]
                                                        tSWord2 <- testString_words[2]
                                                        #tSWord3 <- "zznotvalidwordzz"
                
                                                        #When a tWO word pharse match (&) is made call the tri model to predict third word else proceed
                                                        if ((tSWord1 %in% tdm3[, 1] & tSWord2 %in% tdm3[, 2]) == TRUE) {
                                                                
                                                                #Call tri model
                                                                try(trimodel(tSWord1, tSWord2), silent=TRUE)
                                                        }
                                                        else if
                                                                #If two word pharse is not a match backoff first word and try to predict one word 
                                                                #If match is made call the bi model to predict second word else proceed
                                                                ((tSWord2 %in% tdm3[, 2]) == TRUE) {
                                                                #Assign user inputs with backoff
                                                                tSWord1 <- tSWord2
                                                                #tSWord2  <- "zznotvalidwordzz"   
                                                                #tSWord3 <- "zznotvalidwordzz"   
                                
                                                                try(bimodel(tSWord1), silent=TRUE)
                                                        }       
                                                        else 
                                                                #If one word pharse is not a match backoff to unigram and obtain word 
                                                                if ((tSWord2 %in% tdm1[, 1] ) == TRUE) {
                                                                        #Assign user inputs with backoff       
                                                                        tSWord1 <- tSWord2
                                                                        #tSWord2  <- "zznotvalidwordzz"   
                                                                        #tSWord3 <- "zznotvalidwordzz"
                        
                                                                        try(unimodel(tSWord1), silent=TRUE)
                                                                }  
                                                                else
                                                                        #If one word pharse is not a match backoff to unigram and obtain word 
                                                                        try(unimodel(tSWord1), silent=TRUE)
                                                        
                                        #######
                                        #Start here if a one word phrase is fed             
                                        }                
                                        else if  
                                                (testStringWordCount== 1) {      
                                                        #Assign user input
                                                        tSWord1 <- testString_words[1]
                                                        #tSWord2 <- "zznotvalidwordzz"
                                                        #tSWord3 <- "zznotvalidwordzz"
                
                                                        #When a one word pharse match is made call the bi model to predict second word else proceed
                                                        if ((tSWord1 %in% tdm2[, 1]) == TRUE) {                
                                                                
                                                                #Call bi model
                                                                try(bimodel(tSWord1), silent=TRUE)
                                                                
                                                                assign("last.warning", NULL, envir = baseenv())
                                                        }  
                                                        else
                                                                #If one word pharse is not a match backoff to unigram and obtain word 
                                                                try(unimodel(tSWord1), silent=TRUE)
                                                        
                                                        
                                                }        
                                                           
return("Stupid Backoff Model Result")} 


##########################################################################################################################################
#B4. ngram Stupid BackoffModels

pentamodel <- function(tSWord1, tSWord2, tSWord3, tSWord4)  {        
        
                                        #Create empty place holder for model name
                                        modelUsed <- vector()
        
                                        #Extract top X fifth words associated with penta gram
                                        tdm5Word5 <- tdm5[(tdm5[, 1] == tSWord1 & tdm5[, 2] == tSWord2 & tdm5[, 3] == tSWord3 & tdm5[, 4] == tSWord4) == TRUE, 5]
                                        #tdm5Word5 <- tdm5Word5[1:100]
                                        #Extract top X fourth words associated with "first word input dropped (backed off)" in tetra gram
                                        tdm4Word4 <- tdm4[(tdm4[, 1] == tSWord2 & tdm4[, 2] == tSWord3 & tdm4[, 3] == tSWord4)  == TRUE, 4]
                                        #tdm4Word4 <- tdm4Word4[1:100]
        
                                        #if match are not exactly in one row call next model
                                        if (identical(tdm5Word5, character(0)) | identical(tdm4Word4, character(0))) {
                                                
                                                rm(tdm5word5)
                                                rm(tdm4word4)
                                                rm(tSWord1)
                
                                                tSWord1 <- tSWord2 
                                                tSWord2 <- tSWord3
                                                tSWord3 <- tSWord4
                
                                                tetramodel(tSWord1, tSWord2, tSWord3)
                                        }
                                        else
                                                
                                                #Sum fifth words total match count associated with words extracted from penta gram
                                                tdm5PhraseCountSum <- length(tdm5[(tdm5[, 1] == tSWord1 & tdm5[, 2] == tSWord2 & 
                                                                                                  tdm5[, 3] == tSWord3 & tdm5[, 4] == tSWord4) == TRUE, 5])
                                                #Sum fourth words total match count associated with words extracted from tetra gram
                                                tdm4PhraseCountSum <- length(tdm4[(tdm4[, 1] == tSWord2 & tdm4[, 2] == tSWord3 & tdm4[, 3] == tSWord4)  == TRUE, 4])
        
                                                #Extract top X fifth words frequency's associated with penta gram
                                                tdm5WordFreqCount <- tdm5[(tdm5[, 1] == tSWord1 & tdm5[, 2] == tSWord2 & tdm5[, 3] == tSWord3 & tdm5[, 4] == tSWord4) == TRUE, 6]
                                                #tdm5WordFreqCount <- tdm5WordFreqCount[1:100]
                                                #Extract top X fourth words frequency associated with "first word input dropped (backed off)" in tetra gram
                                                tdm4WordFreqCount <- tdm4[(tdm4[, 1] == tSWord2 & tdm4[, 2] == tSWord3 & tdm4[, 3] == tSWord4)  == TRUE, 5]
                                                #tdm4WordFreqCount <- tdm4WordFreqCount[1:100]
        
                                                #Tabulate and order the results for penta gram predictions
                                                word5Table <- data.frame(word5= tdm5Word5, freq= tdm5WordFreqCount, stringsAsFactors=FALSE)
                                                if (nrow(word5Table > 1)) { word5Table <- aggregate(word5Table['freq'], by= word5Table['word5'], sum) }
                                                if (nrow(word5Table > 1)) { word5Table <- word5Table[order(-word5Table$freq),] }
                                                #Tabulate and order the results for tetra gram predictions
                                                word5Table2 <- data.frame(word5= tdm4Word4, freq= tdm4WordFreqCount, stringsAsFactors=FALSE)
                                                if (nrow(word5Table2 > 1)) { word5Table2 <- aggregate(word5Table2['freq'], by= word5Table2['word5'], sum) }
                                                if (nrow(word5Table2 > 1)) { word5Table2 <- word5Table2[order(-word5Table2$freq),] }
                                                word5Table2count <- nrow(word5Table2)
        
                                                #Calculate Simple Backoff Score for the penta gram fifth word candidates from pentaand tetra gram
                                                #penta gram matched scores= (matched pentagram count / matched backed off tetra gram count)
                                                scorePentagram <- word5Table$freq / tdm4PhraseCountSum
                                                #tetra gram matched scores= (0.4* (matched tetragram count / matched backed off tetra gram count))
                                                scoreTetragram <- (0.4 * (word5Table2$freq / tdm4PhraseCountSum))
        
                                                #Tabulate and order the Simple Backoff Scores
                                                word5Table3 <- data.frame(word5= c(word5Table$word5, word5Table2$word5), 
                                                                   freq= c(scorePentagram, scoreTetragram), stringsAsFactors=FALSE)
                                                if (nrow(word5Table3 > 1)) { word5Table3 <- aggregate(word5Table3['freq'], by= word5Table3['word5'], sum) }
                                                if (nrow(word5Table3 > 1)) { word5Table3 <- word5Table3[order(-word5Table3$freq),] }
        
                                                #Extract the top ten frequent fifth word occurances based on highest Simple Backoff Score
                                                tdmWordTop10 <- word5Table3$word5[1:10]
                                                tdmFreqTop10 <- word5Table3$freq[1:10]
                                                modelUsed[1:10] <- "penta-"
        
                                                #Give result output
                                                output(tdmWordTop10, modelUsed, tdmFreqTop10)         
        
        
return("Stupid Backoff Model Result")}


tetramodel <- function(tSWord1, tSWord2, tSWord3) {        
                                        
                                        #Create empty place holder for model name
                                        modelUsed <- vector()
        
                                        #Extract top X fourth words associated with tetra gram
                                        tdm4Word4 <- tdm4[(tdm4[, 1] == tSWord1 & tdm4[, 2] == tSWord2 & tdm4[, 3] == tSWord3) == TRUE, 4]
                                        #tdm4Word4 <- tdm4Word4[1:100]
                                        #Extract top X third words associated with "first word input dropped (backed off)" in tri gram
                                        tdm3Word3 <- tdm3[(tdm3[, 1] == tSWord2 & tdm3[, 2] == tSWord3)  == TRUE, 3]
                                        #tdm3Word3 <- tdm3Word3[1:100]
                                        
                                        #if match are not exactly in one row call next model
                                        if (identical(tdm4Word4,character(0)) | identical(tdm3Word3, character(0))) {
                                                
                                                rm(tdm4word4)
                                                rm(tdm3word3)
                                                rm(tSWord1)
                                                
                                                tSWord1 <- tSWord2 
                                                tSWord2 <- tSWord3
                                        
                                                trimodel(tSWord1, tSWord2)
                                        }
                                        else
                                                
                                                #Sum fourth words total match count associated with words extracted from tetra gram
                                                tdm4PhraseCountSum <- length(tdm4[(tdm4[, 1] == tSWord1 & tdm4[, 2] == tSWord2 & tdm4[, 3] == tSWord3) == TRUE, 4])
                                                #Sum third words total match count associated with words extracted from tri gram
                                                tdm3PhraseCountSum <- length(tdm3[(tdm3[, 1] == tSWord2 & tdm3[, 2] == tSWord3)  == TRUE, 3])
                
                                                #Extract top X fourth words frequency's associated with tetra gram
                                                tdm4WordFreqCount <- tdm4[(tdm4[, 1] == tSWord1 & tdm4[, 2] == tSWord2 & tdm4[, 3] == tSWord3) == TRUE, 5]
                                                #tdm4WordFreqCount <- tdm4WordFreqCount[1:100]
                                                #Extract top X third words frequency associated with "first word input dropped (backed off)" in tri gram
                                                tdm3WordFreqCount <- tdm3[(tdm3[, 1] == tSWord2 & tdm3[, 2] == tSWord3)  == TRUE, 4]
                                                #tdm3WordFreqCount <- tdm3WordFreqCount[1:100]
                
                                                #Tabulate and order the results for tetra gram predictions
                                                word4Table <- data.frame(word4= tdm4Word4, freq= tdm4WordFreqCount, stringsAsFactors=FALSE)
                                                if (nrow(word4Table > 1)) { word4Table <- aggregate(word4Table['freq'], by= word4Table['word4'], sum) }
                                                if (nrow(word4Table > 1)) { word4Table <- word4Table[order(-word4Table$freq),] }
                                                #Tabulate and order the results for tri gram predictions
                                                word4Table2 <- data.frame(word4= tdm3Word3, freq= tdm3WordFreqCount, stringsAsFactors=FALSE)
                                                if (nrow(word4Table2 > 1)) { word4Table2 <- aggregate(word4Table2['freq'], by= word4Table2['word4'], sum) }
                                                if (nrow(word4Table2 > 1)) { word4Table2 <- word4Table2[order(-word4Table2$freq),] }
                                                word4Table2count <- nrow(word4Table2)
                
                                                #Calculate Simple Backoff Score for the tetra gram fourth word candidates from tetra and tri gram
                                                #tetra gram matched scores= (matched tetragram count / matched backed off tri gram count)
                                                scoreTetragram <- word4Table$freq / tdm3PhraseCountSum
                                                #tri gram matched scores= (0.4* (matched trigram count / matched backed off tri gram count))
                                                scoreTrigram <- (0.4 * (word4Table2$freq / tdm3PhraseCountSum))
                                        
                                                #Tabulate and order the Simple Backoff Scores
                                                word4Table3 <- data.frame(word4= c(word4Table$word4, word4Table2$word4), 
                                                                                freq= c(scoreTetragram, scoreTrigram), stringsAsFactors=FALSE)
                                                if (nrow(word4Table3 > 1)) { word4Table3 <- aggregate(word4Table3['freq'], by= word4Table3['word4'], sum) }
                                                if (nrow(word4Table3 > 1)) { word4Table3 <- word4Table3[order(-word4Table3$freq),] }
                
                                                #Extract the top ten frequent fourth word occurances based on highest Simple Backoff Score
                                                tdmWordTop10 <- word4Table3$word4[1:10]
                                                tdmFreqTop10 <- word4Table3$freq[1:10]
                                                modelUsed[1:10] <- "tetra-"
                
                                                #Give result output
                                                output(tdmWordTop10, modelUsed, tdmFreqTop10)
                                        
                                        
return("Stupid Backoff Model Result")}


trimodel <- function(tSWord1, tSWord2) {        
        
                                        #Create empty place holder for model name
                                        modelUsed <- vector()
        
                                        #Extract top X third words associated with tri gram
                                        tdm3Word3 <- tdm3[(tdm3[, 1] == tSWord1 & tdm3[, 2] == tSWord2) == TRUE, 3]
                                        #tdm3Word3 <- tdm3Word3[1:100]
                                        #Extract top X second words associated with "first word input dropped (backed off)" in bi gram
                                        tdm2Word2 <- tdm2[(tdm2[, 1] == tSWord2)  == TRUE, 2]
                                        #tdm2Word2 <- tdm2Word2[1:100]
                                        
                                        #if match are not exactly in one row call next model
                                        if (identical(tdm3Word3,character(0)) | identical(tdm2Word2, character(0))) {
                                                
                                                rm(tdm3word3)
                                                rm(tdm2word2)
                                                rm(tSWord1)
                                                
                                                tSWord1 <- tSWord2 
                                                
                                                bimodel(tSWord1)
                                       
                                        } 
                                        else                                        
        
                                                #Sum third words total match count associated with words extracted from tri gram
                                                tdm3PhraseCountSum <- length(tdm3[(tdm3[, 1] == tSWord1 & tdm3[, 2] == tSWord2) == TRUE, 3])
                                                #Sum second words total match count associated with words extracted from bi gram
                                                tdm2PhraseCountSum <- length(tdm2[(tdm2[, 1] == tSWord2)  == TRUE, 2])
        
                                                #Extract top X third words frequency's associated with tri gram
                                                tdm3WordFreqCount <- tdm3[(tdm3[, 1] == tSWord1 & tdm3[, 2] == tSWord2 ) == TRUE, 4]
                                                #tdm3WordFreqCount <- tdm3WordFreqCount[1:100]
                                                #Extract top X second words frequency associated with "first word input dropped (backed off)" in bi gram
                                                tdm2WordFreqCount <- tdm2[(tdm2[, 1] == tSWord2)  == TRUE, 3]
                                                #tdm2WordFreqCount <- tdm2WordFreqCount[1:100]
        
                                                #Tabulate and order the results for tri gram predictions
                                                word3Table <- data.frame(word3= tdm3Word3, freq= tdm3WordFreqCount, stringsAsFactors=FALSE)
                                                if (nrow(word3Table > 1)) {word3Table <- aggregate(word3Table['freq'], by= word3Table['word3'], sum) }
                                                if (nrow(word3Table > 1)) { word3Table <- word3Table[order(-word3Table$freq),] }
                                                #Tabulate and order the results for bi gram predictions
                                                word3Table2 <- data.frame(word3= tdm2Word2, freq= tdm2WordFreqCount, stringsAsFactors=FALSE)
                                                if (nrow(word3Table2 > 1)) { word3Table2 <- aggregate(word3Table2['freq'], by= word3Table2['word3'], sum) }
                                                if (nrow(word3Table2 > 1)) { word3Table2 <- word3Table2[order(-word3Table2$freq),] }
                                                word3Table2count <- nrow(word3Table2)
        
                                                #Calculate Simple Backoff Score for the tri gram third word candidates from tri and bi gram
                                                #tri gram matched scores= (matched trigram count / matched backed off bi gram count)
                                                scoreTrigram <- word3Table$freq / tdm2PhraseCountSum
                                                #bi gram matched scores= (0.4* (matched bigram count / matched backed off bi gram count))
                                                scoreBigram <- (0.4 * (word3Table2$freq / tdm2PhraseCountSum))
        
                                                #Tabulate and order the Simple Backoff Scores
                                                word3Table3 <- data.frame(word3= c(word3Table$word3, word3Table2$word3), 
                                                                                freq= c(scoreTrigram, scoreBigram), stringsAsFactors=FALSE)
                                                if (nrow(word3Table3 > 1)) { word3Table3 <- aggregate(word3Table3['freq'], by= word3Table3['word3'], sum) }
                                                if (nrow(word3Table3 > 1)) { word3Table3 <- word3Table3[order(-word3Table3$freq),] }
        
                                                #Extract the top ten frequent third word occurances based on highest Simple Backoff Score
                                                tdmWordTop10 <- word3Table3$word3[1:10]
                                                tdmFreqTop10 <- word3Table3$freq[1:10]
                                                modelUsed[1:10] <- "tri-"
        
                                                #Give result output
                                                output(tdmWordTop10, modelUsed, tdmFreqTop10)
                                        
return("Stupid Backoff Model Result")}

        
bimodel <- function(tSWord1) {        
        
                                        #Create empty place holder for model name
                                        modelUsed <- vector()
        
                                        #Extract top X second words associated with bi gram
                                        tdm2Word2 <- tdm2[(tdm2[, 1] == tSWord1) == TRUE, 2]
                                        #tdm2Word2 <- tdm2Word2[1:100]
                                        #Extract top X words associated with higest frequency in uni gram
                                        tdm1Word1 <- tdm1[, 1]
                                        #tdm1Word1 <- tdm1Word1[1:100]
                                        
                                        #if match are not exactly in one row call next model
                                        if (identical(tdm2Word2, character(0)) | identical(tdm1Word1, character(0))) {
                                                
                                                rm(tdm2word2)
                                                rm(tdm1word1)
                                                
                                                #Calling bimodel if one word available
                                                bimodel(tSWord1)
                                        }        
                                        else if
                                                (identical(tdm2Word2,character(0)) & identical(tdm1Word1, character(0))) {
                                                
                                                rm(tdm2word2)
                                                rm(tdm1word1)        
                                                        
                                                
                                                #calling unimodel if no word is available
                                                model(tSWord1)
                                                        
                                        }
                                        else
        
                                                #Sum second words total match count associated with words extracted from bi gram
                                                tdm2PhraseCountSum <- length(tdm2[(tdm2[, 1] == tSWord1) == TRUE, 2])
                                                #Sum one word count extracted from uni gram
                                                tdm1PhraseCountSum <- length(tdm1[, 1])
        
                                                #Extract top X second words frequency's associated with bi gram
                                                tdm2WordFreqCount <- tdm2[(tdm2[, 1] == tSWord1) == TRUE, 3]
                                                #tdm2WordFreqCount <- tdm2WordFreqCount[1:100]
                                                #Extract top X one word frequency's etracted from uni gram
                                                tdm1WordFreqCount <- tdm1[, 2]
                                                #tdm1WordFreqCount <- tdm1WordFreqCount[1:100]
        
                                                #Tabulate and order the results for bi gram predictions
                                                word2Table <- data.frame(word2= tdm2Word2, freq= tdm2WordFreqCount, stringsAsFactors=FALSE)
                                                if (nrow(word2Table > 1)) { word2Table <- aggregate(word2Table['freq'], by= word2Table['word2'], sum) }
                                                if (nrow(word2Table > 1)) { word2Table <- word2Table[order(-word2Table$freq),] }
                                                #Tabulate and order the results for uni gram predictions
                                                word2Table2 <- data.frame(word2= tdm1Word1, freq= tdm1WordFreqCount, stringsAsFactors=FALSE)
                                                if (nrow(word2Table2 > 1)) { word2Table2 <- aggregate(word2Table2['freq'], by= word2Table2['word2'], sum) }
                                                if (nrow(word2Table2 > 1)) { word2Table2 <- word2Table2[order(-word2Table2$freq),] }
                                                word2Table2count <- nrow(word2Table2)
        
                                                #Calculate Simple Backoff Score for the bi gram second word candidates from bi and uni gram
                                                #bi gram matched scores= (matched bigram count / matched backed off uni gram count)
                                                scoreBigram <- word2Table$freq / tdm1PhraseCountSum
                                                #uni gram matched scores= (0.4* (matched unigram count / matched backed off uni gram count))
                                                #x 1000 in denominator to greater emphysis to bi gram predictions
                                                scoreUnigram <- (0.4 * (word2Table2$freq / (tdm1PhraseCountSum*1000)))
        
                                                #Tabulate and order the Simple Backoff Scores
                                                word2Table3 <- data.frame(word2= c(word2Table$word2, word2Table2$word2), 
                                                                        freq= c(scoreBigram, scoreUnigram), stringsAsFactors=FALSE)
                                                if (nrow(word2Table3 > 1)) { word2Table3 <- aggregate(word2Table3['freq'], by= word2Table3['word2'], sum) }
                                                if (nrow(word2Table3 > 1)) { word2Table3 <- word2Table3[order(-word2Table3$freq),] }
        
                                                #Extract the top ten frequent second word occurances based on highest Simple Backoff Score
                                                tdmWordTop10 <- word2Table3$word2[1:10]
                                                tdmFreqTop10 <- word2Table3$freq[1:10]
                                                modelUsed[1:10] <- "bi-"
        
                                                #Give result output
                                                output(tdmWordTop10, modelUsed, tdmFreqTop10)
                                                
                                                
return("Stupid Backoff Model Result")}


unimodel <- function(tSWord1){        
        
                                        #Create empty place holder for model name
                                        modelUsed <- vector()
        
                                        #Extract top X words associated with higest frequency in uni gram
                                        tdm1Word1 <- tdm1[, 1]
                                        #tdm1Word1 <- tdm1Word1[1:100]
        
                                        #Sum one word count extracted from uni gram
                                        tdm1PhraseCountSum <- length(tdm1[, 1])
        
                                        #Extract top X one word frequency's etracted from uni gram
                                        tdm1WordFreqCount <- tdm1[, 2]
                                        #tdm1WordFreqCount <- tdm1WordFreqCount[1:100]
        
                                        #Tabulate and order the results for uni gram predictions
                                        word1Table <- data.frame(word1= tdm1Word1, freq= tdm1WordFreqCount, stringsAsFactors=FALSE)
                                        if (nrow(word1Table > 1)) { word1Table <- aggregate(word1Table['freq'], by= word1Table['word1'], sum) }
                                        if (nrow(word1Table > 1)) { word1Table <- word1Table[order(-word1Table$freq),] }
                                        word1Tablecount <- nrow(word1Table)
        
                                        #uni gram matched scores= (0.4* (matched unigram count / matched backed off uni gram count))
                                        scoreUnigram <- (0.4 * (word1Table$freq / tdm1PhraseCountSum))
        
                                        #Tabulate and order the Simple Backoff Scores
                                        word1Table2 <- data.frame(word1= c(word1Table$word1), freq= c(scoreUnigram), stringsAsFactors=FALSE)
                                        if (nrow(word1Table > 1)) {word1Table2 <- aggregate(word1Table2['freq'], by= word1Table2['word1'], sum) }
                                        if (nrow(word1Table2 > 1)) { word1Table2 <- word1Table2[order(-word1Table2$freq),] }
        
                                        #Extract the top ten frequent second word occurances based on highest Simple Backoff Score
                                        tdmWordTop10 <- word1Table2$word1[1:10]
                                        tdmFreqTop10 <- word1Table2$freq[1:10]
                                        modelUsed[1:10] <- "uni-"
        
                                        #Give result output
                                        output(tdmWordTop10, modelUsed, tdmFreqTop10)
        
return("Stupid Backoff Model Result")}

##########################################################################################################################################
#End COde
##########################################################################################################################################

#testString <- "when are we going to help other"
#back_off(testString) #, tdm1, tdm2, tdm3, tdm4)


