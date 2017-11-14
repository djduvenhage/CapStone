##########################################################################################################################################
###A. Basic Environment Setup
###Load and Read the Raw Data, Create Corpus, and Extract ngrams
##########################################################################################################################################
###IMPORTANT NOTE: check settings below for three areas with indicator on the right: #<---- check status removed ?
##################

###A1. Loading libraries
#####There is an issue reading libraries qdap, ANLP, & RWeka, not seeing the rJava information. 
#####Hence, set the path, and read the rJava library.
Sys.setenv(JAVA_HOME= "C:\\Program Files\\Java\\jdk1.8.0_112\\jre")
suppressMessages(library(rJava))
#####Load libraries.

suppressMessages(library(readtext))      # reading text files 
#suppressMessages(library(caret))        # generate training and testing data
suppressMessages(library(tm))            # for text mining
suppressMessages(library(SnowballC))     # for text stemming  
suppressMessages(library(wordcloud))     # word-cloud generator
suppressMessages(library(RColorBrewer))  # color palettes
suppressMessages(library(dplyr))         # use of pipes and count() 
suppressMessages(library(plyr))
suppressMessages(library(tidytext)) 
suppressMessages(library(tidyr)) 
suppressMessages(library(bigmemory))     # use to manipulate big matrix's
suppressMessages(library(ggplot2))
suppressMessages(library(reshape2))
suppressMessages(library(RWeka))         #tokenizer to evaluate bigrams and trigrams 
suppressMessages(library(ngram))         #tokenizer to evaluate bigrams and trigrams 
suppressMessages(library(stringr))       #to clean up simple strings
suppressMessages(library(knitr))         #to resolve issue of R-Markdown File Not Knitting,
suppressMessages(library(qdap))
suppressMessages(library(tibble))
suppressMessages(library(data.table))
suppressMessages(library(ANLP))         #predict_Backoff  ----> there is an issue with this model, does not works as yet


###A2. Setup Directories.
#####Set file path to working directory.
#####Important: working directory should point at the file where .Rmd is located
setwd("C:/Users/Dawid J Duvenhage/Desktop/Coursera Courses/Data Scientist Specialization/10_Cap Stone Project")

#####Download the raw data sets.
#####Set correct path for file download.
filepath <- "C:/Users/Dawid J Duvenhage/Desktop/Coursera Courses/Data Scientist Specialization/10_Cap Stone Project/Assignment"
if(!file.exists("./Assignment")){dir.create("./Assignment")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

if (!file.exists("Assignment/coursera-swiftkey.zip")){
        download.file(fileUrl, destfile="./Assignment/coursera-swiftkey.zip")    
        
        #Unzip raw dataSet to /Project1 directory
        unzip(zipfile="./Assignment/coursera-swiftkey.zip",exdir="./Assignment")
}
#Download and read list of profane/bad words.
fileURL <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"

if (!file.exists("/Assignment/profane.txt")){
        download.file(fileURL, destfile="./Assignment/profane.txt")
}


###B3. Read the Raw data from hard drive.
####3.1 Open and Read raw training and profane table data in binary mode.
news <- readLines("Assignment/final/en_US/en_US.news.txt", encoding="UTF-8", skipNul= TRUE)    #readTextFile
blogs <- readLines("Assignment/final/en_US/en_US.blogs.txt", encoding="UTF-8", skipNul= TRUE)
twitter <- readLines("Assignment/final/en_US/en_US.twitter.txt", encoding="UTF-8", skipNul= TRUE)
#myWords <- readLines("Assignment/final/en_US/myWords.txt", encoding="UTF-8", skipNul= TRUE)                                #<---- add additional files ?

####3.2Read the profane word library as downloaded to hard drive.
profane <- read.table("Assignment/profane.txt", header=FALSE, sep="\n", strip.white=TRUE)


##########################################################################################################################################
###B. Extract The Data - Create and clean Corpus, extract ngrams and save as text files
##########################################################################################################################################
###Step B1. Create single reduce raw data file and Remove non-ASCII characters
#####Combine the news, blogs, and twitter file text into one mega text file.    
#####Remove the non-ASCII characters. It can safely assume that the remaining words are largely of the English language. 
#####While the ASCII code primarily represents English characters and numbers, other European languages also have English 
#####letter characters. Hence, this data cleanup is not comprehensive in the sense of foreign language clean up.
comb_text <- c(news, blogs, twitter)                                                                                   
#comb_text <- myWords                                                                                                   #<---- add additional files ?
system.time(comb_text <- iconv(comb_text, "latin1", "ASCII", sub=""))

###Step B2:Randomly select 10% of the lines from complete data set.
#####Apply system.time() to several of the operations and loops to keep track of code execution efficiency.
#####Keep in mind:
#####user = CPU time in seconds
#####system = CPU time in seconds
#####elapsed = wall clock time in seconds
#####If the CPU time is larger than the elapsed time it may be because of running an a multiprocessor system, and the application run multithreaded.
#####So, if elapsed time > user time, this means that the CPU is waiting around for some other operations (may be external) to be done.
#####and, If elapsed time < user time, this means that your machine has multiple cores and is able to use them.
#####Also, "User CPU time" gives the CPU time spent by the current process (i.e., the current R session) and "system CPU time" gives the CPU time spent 
#####by the kernel (the operating system) on behalf of the current process. The operating system is used for things like opening files, doing input or 
#####output, starting other processes, and looking at the system clock: operations that involve resources that many processes must share.


#Get a Training and Testing Data set
# This approach runs into a "Error: C stack usage  387846287 is too close to the limit"
#set.seed(101562)
#system.time(inTrain <- createDataPartition(comb_text, p = 0.75, list = F))
#training <- train_subset[inTrain, ]
#testing <- train_subset[-inTrain, ]
#Clear and release memory
#rm(inTrain)
#gc()
#assign the training data object as "comb_txt"
#comb_text <- training

set.seed(101562)
system.time(comb_text <- comb_text[sample(1:length(comb_text), (length(comb_text)*.10))])    #.10 or #0.50 or #0.75     #<---- check status fraction ?

#####Calculate the proximate memory usage using number of rows and columns within the data:
#####Suppose you have a data frame with 2,000,000 rows and 250 columns, all of which are numeric data.
#####If the data frame is just character data, the ratio is going to be around 1.1, and reading it in as factor data might be even below this.
#####(2,000,000 × 250 × 8 bytes/numeric)
#####Roughly saying, how much memory is required to store this data?
#####bytes: 2000000*250*8
#####bytes/MB: 2000000*250*8/2^{20}
#####MB: round(2000000*250*8/2^{20},2)
#####GB: round(2000000*250*8/2^{20}/1024, 2)
#
#####object size       SI
#####1	              1  B
#####1000	      1 kB
#####1000^2	      1 MB
#####1000^3	      1 GB
#####1000^4	      1 TB
#####1000^5	      1 PB
#####1000^6	      1 EB
#####1000^7	      1 ZB
#####1000^8	      1 YB

rowNumberText <- length(comb_text)

#####Object memory size:
object.size(comb_text)             # B
object.size(comb_text)/(1000^2)    #MB
object.size(comb_text)/(1000^3)    #GB

#clear up and release memory
rm(news, blogs, twitter)
gc()

###Step B3:Extract Random Sampled Data into a Corpus and Clean the Data
#Setup loop parameters
j= 1
i= 1
endLoop= 0

#Loop for (rowNumberText/4000) number of random sample text lines.
system.time(while (endLoop <- rowNumberText/4000) {
        
        #Progress bar using the Windows operating system.
        #create progress bar
        pb1 <- winProgressBar(title= "progress bar j", min= 0, max= ceiling(rowNumberText/4000), width= 300)
        pb2 <- winProgressBar(title= "progress bar x", min= 0, max= rowNumberText, width= 300)      
        
        #Outer loop: from (1 to (rowNumberText/4000)) number of random sample text lines.
        for(j in 1:ceiling(rowNumberText/4000)) {
                
                #Inner loop: from 1 to (1 to (rowNumberText/4000)) lines, in chunks of 4000 lines, of random sample text lines.
                #for(i in (4000*(j-1)+1):(min(4000*j, rowNumberText))) {    
                
                #Set line parameters for each new Corpus cycle, based on line numbers in random sampled comb_text.
                x= (4000*(j-1)+1)
                y= 4000*j
                
                #Setup a Progress bar to watch progression through comb_text.
                Sys.sleep(0.1)
                setWinProgressBar(pb1, j, title= paste("Progress Bar - Chunk # ", j))
                setWinProgressBar(pb2, x, title= paste("Progress Bar - rowNumberText # ", x))
                
                ###Step B3.1: Create the Corpus using VCorpus per [x:y] lines in random sampled comb_text.
                system.time(Corpus <- VCorpus(VectorSource(comb_text[(x):(y)])))
                
                #####One can Inspect the corpus objects:
                #inspect(Corpus)
                #####To print a row wsie list of single line Corpus information.
                #strWrap(Corpus[[1]])
                
                ###Step B3.2: Text Transformation
                #####For this assignment "cleaning up the text data" follows an uncondensed "Stepwise Text Transformation",
                #####each time re-writing the Corpus object with new text transformation.
                
                ####3.2.1 Removing special characters:
                toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
                Corpus <- tm_map(Corpus, toSpace, "/")
                
                ####3.2.2 Convert the text to lower case:
                Corpus <- tm_map(Corpus, content_transformer(tolower))
                
                ####3.2.3 Remove numbers:
                Corpus <- tm_map(Corpus, content_transformer(removeNumbers))
                
                ####3.2.4a Remove English common stop words:
                #Corpus <- tm_map(Corpus, removeWords, stopwords("english"))                                     #<---- check status removed ?
                
                ####3.2.4b Remove additional stop words:
                #dropWords = c("is","and","then")
                #Corpus = tm_map(Corpus,removeWords,dropWords)                                                   #<---- check status removed ?
                
                ####3.2.5 Remove profane words (using the profane word file supplied):
                names(profane) <-"profane"
                Corpus <- tm_map(Corpus, removeWords, profane[,1]) 
                
                ####3.2.6 Remove punctuations:
                Corpus <- tm_map(Corpus, content_transformer(removePunctuation))
                
                ####3.2.7 Eliminate extra white spaces:
                Corpus  <- tm_map(Corpus , content_transformer(stripWhitespace))
                
                ####3.2.8 Text stemming:
                #####Removing suffixes from words to get the common origin.
                #####Stemming is a way to improve / increase word coverage, i.e. keep memory use lower.
                #Corpus <- tm_map(Corpus, content_transformer(stemDocument))                                     #<---- check status removed ?
                
                #####Obtain final Corpus number of lines for use
                rowNumberCorpus <- length(Corpus)
                
                
                ###Step B3.3: NGRAM TOKINAZATION:
                ###################################################################################################################################
                
                #####ngram Tokenizer setup:
                BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
                TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
                TetragramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
                PentagramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
                HexagramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 6, max = 6))
                HeptagramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 7, max = 7))
                
                #####Create Term Documents for one (uni-), two (bi-), three (tri-), four (tetra-), and five (penta-) word pairs.
                #####Final table setup with "words" in the 1st Column and word "frequency" in 2nd column.
                #####Three Steps per word pair: 
                #####(i) Create term document matrix (tdm_unigram)
                #####(ii) Transform to data frame (tdm.df1) to hold "words" & "freq" 
                #####(iii) Assign to a place holder (tdm1_accum) for word & freq accumulated per new Corpus cycle.
                #####
                #####Removed sparse term ... removeSparseTerms(<>, 0.5) ... from tdm_ code snippet as it give problems computing.
                tdm_unigram <- TermDocumentMatrix(Corpus, control = list(tokenize="words"))                
                tdm.df1 <- data.frame(word=tdm_unigram$dimnames[[1]],freq=rowSums(as.matrix(tdm_unigram)),row.names = NULL)
                #Capture the unigram for pass
                tdm1_accum <- tdm.df1
                #Appending the unigram on each new pass
                tdm1_accum <- c(tdm1_accum, tdm.df1)
                
                tdm_bigram <- TermDocumentMatrix(Corpus, control = list(tokenize = BigramTokenizer))
                tdm.df2 <- data.frame(word=tdm_bigram$dimnames[[1]],freq=rowSums(as.matrix(tdm_bigram)),row.names = NULL)
                #Capture the bigram for pass
                tdm2_accum <- tdm.df2
                #Appending the bigram on each new pass
                tdm2_accum <- c(tdm2_accum, tdm.df2)
                
                tdm_trigram <- TermDocumentMatrix(Corpus, control = list(tokenize = TrigramTokenizer))
                tdm.df3 <- data.frame(word=tdm_trigram$dimnames[[1]],freq=rowSums(as.matrix(tdm_trigram)),row.names = NULL)
                #Capture the trigram for pass
                tdm3_accum <- tdm.df3
                #Appending the trigram on each new pass
                tdm3_accum <- c(tdm3_accum, tdm.df3)
                
                tdm_tetragram <- TermDocumentMatrix(Corpus, control = list(tokenize = TetragramTokenizer))      
                tdm.df4 <- data.frame(word=tdm_tetragram$dimnames[[1]],freq=rowSums(as.matrix(tdm_tetragram)),row.names = NULL)
                #Capture the tetragram for pass
                tdm4_accum <- tdm.df4
                #Appending the tetragram on each new pass
                tdm4_accum <- c(tdm4_accum, tdm.df4)
                
                tdm_pentagram <- TermDocumentMatrix(Corpus, control = list(tokenize = PentagramTokenizer))      
                tdm.df5 <- data.frame(word=tdm_pentagram$dimnames[[1]],freq=rowSums(as.matrix(tdm_pentagram)),row.names = NULL)
                #Capture the tetragram for pass
                tdm5_accum <- tdm.df5
                #Appending the tetragram on each new pass
                tdm5_accum <- c(tdm5_accum, tdm.df5)
                
                tdm_hexagram <- TermDocumentMatrix(Corpus, control = list(tokenize = HexagramTokenizer))      
                tdm.df6 <- data.frame(word=tdm_hexagram$dimnames[[1]],freq=rowSums(as.matrix(tdm_hexagram)),row.names = NULL)
                #Capture the hexagram for pass
                tdm6_accum <- tdm.df6
                #Appending the hexagram on each new pass
                tdm6_accum <- c(tdm6_accum, tdm.df6)
                
                tdm_heptagram <- TermDocumentMatrix(Corpus, control = list(tokenize = HeptagramTokenizer))      
                tdm.df7 <- data.frame(word=tdm_heptagram$dimnames[[1]],freq=rowSums(as.matrix(tdm_heptagram)),row.names = NULL)
                #Capture the heptagram for pass
                tdm7_accum <- tdm.df7
                #Appending the heptagram on each new pass
                tdm7_accum <- c(tdm7_accum, tdm.df7)
                
                #inspect(tdm_heptagram) 
                
                #Clear up and release term document matrix object memory per ngram for next cycle
                rm(tdm_unigram, tdm_bigram, tdm_trigram, tdm_tetragram, tdm_pentagram, tdm_hexagram, tdm_heptagram,
                   tdm.df1, tdm.df2, tdm.df3, tdm.df4, tdm.df5, tdm.df6, tdm.df7)
                gc()
                
                #clear up and release the Corpus memory for next cycle
                rm(Corpus)
                gc()
                
                #}
                
        }     
        
        #endLoop = j
        
        #End Progress Bar
        close(pb1)
        close(pb2)
        
})  


#####clear up and release unused memory
rm(profane)
rm(comb_text)
gc()

###Step B4:  Order the ngrams and save to hard drive:  
#####Aggregate similar group words by summing frequency from each pass.
tdm1_accum <- aggregate(tdm1_accum['freq'], by=tdm1_accum['word'], sum)
#####Order the final ngrams in decreasing order based on word frequency.
tdm1 <- tdm1_accum[order(-tdm1_accum$freq),]

tdm2_accum <- aggregate(tdm2_accum['freq'], by=tdm2_accum['word'], sum)
tdm2 <- tdm2_accum[order(-tdm2_accum$freq),]

tdm3_accum <- aggregate(tdm3_accum['freq'], by=tdm3_accum['word'], sum)
tdm3 <- tdm3_accum[order(-tdm3_accum$freq),]

tdm4_accum <- aggregate(tdm4_accum['freq'], by=tdm4_accum['word'], sum)
tdm4 <- tdm4_accum[order(-tdm4_accum$freq),]

tdm5_accum <- aggregate(tdm5_accum['freq'], by=tdm5_accum['word'], sum)
tdm5 <- tdm5_accum[order(-tdm5_accum$freq),]

tdm6_accum <- aggregate(tdm6_accum['freq'], by=tdm6_accum['word'], sum)
tdm6 <- tdm6_accum[order(-tdm6_accum$freq),]

tdm7_accum <- aggregate(tdm7_accum['freq'], by=tdm7_accum['word'], sum)
tdm7 <- tdm7_accum[order(-tdm7_accum$freq),]

#Save unaggregated ngrams to hard drive
write.table(tdm1_accum, file= "tdm1_accum.txt", sep = "\t")
write.table(tdm2_accum, file= "tdm2_accum.txt", sep = "\t")
write.table(tdm3_accum, file= "tdm3_accum.txt", sep = "\t")
write.table(tdm4_accum, file= "tdm4_accum.txt", sep = "\t")
write.table(tdm5_accum, file= "tdm5_accum.txt", sep = "\t")
write.table(tdm6_accum, file= "tdm6_accum.txt", sep = "\t")
write.table(tdm7_accum, file= "tdm7_accum.txt", sep = "\t")

#Save the aggregated ngrams to hard drive.
write.table(tdm1, file= "tdm1.txt", sep = "\t")
write.table(tdm2, file= "tdm2.txt", sep = "\t")
write.table(tdm3, file= "tdm3.txt", sep = "\t")
write.table(tdm4, file= "tdm4.txt", sep = "\t")
write.table(tdm5, file= "tdm5.txt", sep = "\t")
write.table(tdm6, file= "tdm6.txt", sep = "\t")
write.table(tdm7, file= "tdm7.txt", sep = "\t")

#head(tdm7)

#clear up and release memory on ngram
rm(tdm7_accum, tdm6_accum, tdm5_accum, tdm4_accum, tdm3_accum, tdm2_accum, tdm1_accum)
rm(tdm7, tdm6, tdm5, tdm4, tdm3, tdm2, tdm1)
gc()


##########################################################################################################################################