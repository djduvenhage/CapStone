CapStone - NLP App
========================================================
author: Dawid J. Duvenhage
date: November 11th, 2017
autosize: true

<style>
.footer {
    color: black;
    background: #E8E8E8;
    position: fixed;
    top: 95%;
    text-align:center;
    width:100%;
}
</style>

Background
========================================================
##### "NLP" based on "Stupid Backoff Prediction" [1].     

##### Publicly available blog, twitter, & news data used.

##### Project information available from Coursera web site.

##### "SwiftKey.zip" available from the link under References.    

##### There are three raw data files [2]:

##### en_US.blogs.txt    
##### en_US.news.txt    
##### en_US.twitter.txt    


Text Predictive Model Algorithm
========================================================
### Prediction Algorithm

#### Stupid Backoff Prediction Methodology (SBPM):
> ###### 1. Input text word or phrase truncate to four words.
> ###### 2. Possible matches determined using "SBPM".
>  <center>![alt text](Capture2.png)</center>
> ###### 3. Attempt to match input in pentagram, backing off to tetra-, tri-, and bigrams as required.
> ###### 4. If not found in bigram return the top 10 most frequently used unigram words.


Shiny App - NLP Prediction Model
========================================================
### App Screenshot:
</center>![alt text](Capture.png)</center>
###### Application available at:  https://rforreal.shinyapps.io/shiny_app/


Conclusion
========================================================

###### 1. Model shows acceptable predictive capability, but not great.        
###### 2. Good one and two word phrase predictions (found in top 5).       
###### 3. Three, and larger word phrases, predicts with poor accuracy.      
###### 4. Positively, the model response is quick.
###### 5. Model consistently "backs off" through ngrams.     
###### 6. "Stupid Backoff" approach, as implemented, needs attention.     


### References:    
###### [1] Milestone report and source code can be found at: https://github.com/2blam/DataSciCapstone
###### [2] Brants T. et al., Large Language Models in ..., http://www.aclweb.org/anthology/D07-1090.pdf
###### [3] https://www.coursera.org/learn/data-science-project/supplement/Iimbd/task-0-understanding-the-problem       


