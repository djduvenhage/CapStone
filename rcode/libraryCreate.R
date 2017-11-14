##########################################################################################################################################
###C. Create Library Files
###Reading the "freq count" ngram text files from hard drive and convert to "freq fraction" files
##########################################################################################################################################

###C1. Loading libraries
#####There is an issue reading libraries qdap, ANLP, & RWeka, not seeing the rJava information. 
#####Hence, set the path, and read the rJava library.
Sys.setenv(JAVA_HOME= "C:\\Program Files\\Java\\jdk1.8.0_112\\jre")
suppressMessages(library(rJava))

#####Load libraries.
suppressMessages(library(readtext))      # reading text files 
suppressMessages(library(dplyr))         # use of pipes and count() 
suppressMessages(library(plyr))
suppressMessages(library(tidytext)) 
suppressMessages(library(tidyr)) 
suppressMessages(library(stringr))       #to clean up simple strings
suppressMessages(library(knitr))         #to resolve issue of R-Markdown File Not Knitting,
suppressMessages(library(data.table))
suppressMessages(library(tm))
suppressMessages(library(ANLP))


###C2. Set file path to working directory.
#####Important: working directory should point at the file where .Rmd is located
setwd("C:/Users/Dawid J Duvenhage/Desktop/Coursera Courses/Data Scientist Specialization/10_Cap Stone Project")

#filePath <- "./Old ngram txt files/75 percent no sw/"

#files without stop words
#fileName1 = "tdm1.txt"
#fileName2 = "tdm2.txt"
#fileName3 = "tdm3.txt"
#fileName4 = "tdm4.txt"

#files with stop words
fileName1 = "tdm1sw.txt"
fileName2 = "tdm2sw.txt"
fileName3 = "tdm3sw.txt"
fileName4 = "tdm4sw.txt"

###C3.Reconstruct ngramDB from ngram text files loaded / read from hard drive.
#####Read the text files where "word" comes into object as "factor" and "freq" as integer
tdm1Read <- read.table(fileName1, stringsAsFactors=FALSE)
#####Delete rows where the freq count are less than four to remove some of the noise
tdm1Read <- tdm1Read[!rowSums(tdm1Read < 0),]
#####Calculate the actual fractions for each of the frequency counts
tdm1Frac <- mutate(tdm1Read, frac= round(tdm1Read$freq/sum(tdm1Read$freq), 5))
#####Remove the frequency count column
tdm1 <- within(tdm1Frac, rm("freq"))
#####Rename the clomuns as "word1" and "freq"
colnames(tdm1) <- c("word1", "freq")
#sapply(tdm1, class)
#head(tdm1)

#####Apply tdm1 procedure above on the bi-, tri-, tetra-, penta-, hexa-, and hepta- equivalents
#####First four lines read file, reduce data, and calculate fractions. 
#####The next five lines split the string into separate words and adjust column names
#####The final linechanges object name to simlper name
tdm2Read <- read.table(fileName2, stringsAsFactors=FALSE)
tdm2Read <- tdm2Read[!rowSums(tdm2Read < 0),]

tdm2Frac <- mutate(tdm2Read, frac= round(tdm2Read$freq/sum(tdm2Read$freq),5))
tdm2Frac <- within(tdm2Frac, rm("freq"))

tdm2wordsplit <- strsplit(tdm2Frac[, "word"], " ")
tdm2wordsplit <- t(as.data.frame(tdm2wordsplit))
tdm2wordsplit <- cbind.data.frame(tdm2wordsplit, tdm2Frac$frac, stringsAsFactors=FALSE)
rownames(tdm2wordsplit) <- c()
colnames(tdm2wordsplit) <- c("word1", "word2", "freq")

tdm2 <- tdm2wordsplit
#sapply(tdm2, class)
#head(tdm2)

tdm3Read <- read.table(fileName3, stringsAsFactors=FALSE)
tdm3Read <- tdm3Read[!rowSums(tdm3Read < 0),]

tdm3Frac <- mutate(tdm3Read, frac= round(tdm3Read$freq/sum(tdm3Read$freq), 5))
tdm3Frac <- within(tdm3Frac, rm("freq"))

tdm3wordsplit <- strsplit(tdm3Frac[, "word"], " ")
tdm3wordsplit <- t(as.data.frame(tdm3wordsplit))
tdm3wordsplit <- cbind.data.frame(tdm3wordsplit, tdm3Frac$frac, stringsAsFactors=FALSE)
rownames(tdm3wordsplit) <- c()
colnames(tdm3wordsplit) <- c("word1", "word2", "word3", "freq")

tdm3 <- tdm3wordsplit
#sapply(tdm3, class)
#head(tdm3)

tdm4Read <- read.table(fileName4, stringsAsFactors=FALSE)
tdm4Read <- tdm4Read[!rowSums(tdm4Read < 0),]

tdm4Frac <- mutate(tdm4Read, frac= round(tdm4Read$freq/sum(tdm4Read$freq), 5))
tdm4Frac <- within(tdm4Frac, rm("freq"))

tdm4wordsplit <- strsplit(tdm4Frac[, "word"], " ")
tdm4wordsplit <- t(as.data.frame(tdm4wordsplit))
tdm4wordsplit <- cbind.data.frame(tdm4wordsplit, tdm4Frac$frac, stringsAsFactors=FALSE)
rownames(tdm4wordsplit) <- c()
colnames(tdm4wordsplit) <- c("word1", "word2", "word3", "word4", "freq")

tdm4 <- tdm4wordsplit
#sapply(tdm4, class)
#head(tdm4)

#Create base files to read
write.table(tdm1, fileName1, sep = "\t")
write.table(tdm2, fileName2, sep = "\t")
write.table(tdm3, fileName3, sep = "\t")
write.table(tdm4, fileName4, sep = "\t")

#Delete unused objects and release the memory
rm(tdm1Read, tdm2Read, tdm3Read, tdm4Read)
rm(tdm1Frac, tdm2Frac, tdm3Frac, tdm4Frac)
rm(tdm2wordsplit, tdm3wordsplit, tdm4wordsplit)
gc()

##########################################################################################################################################