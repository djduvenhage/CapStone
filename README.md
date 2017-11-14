

# Shiny App Assignment [Coursera CapStone Project]

The Shiny App presented for this assignment is based on Natural Language Processing (NLP). On a user entered word or phrase, the model predicts the most likely next word.

The notes below describe the background to the prediction model and how to operate the Shiny App.

# Background: Data & Task

Publicly available data (in the form of news, blog, and twitter repositories) is "cleaned" by removing foreign language, non-ASCII text, numbers, punctuation, profanity filtering, and white spaces. A basic n-gram model is developed using exploratory analysis and then expanded to improve accuracy.

Penta-, tetra-, tri-, bi-, and uni- "ngrams" are extracted from the cleaned training corpus. In our case 75% of the raw clean data was extracted to develop the model. The aim is to understand the basic relationships observed in the data before building the linguistic predictive model. An understanding of word distribution and relationship between the words, tokens, and phrases are used to develop the model. Finally the algorithms predictive capability is improved applying text smoothing, backoff prediction, and more specifically in our case "Stupid Backoff Prediction" methodology, as highlighted in a paper by Google, Inc. http://www.aclweb.org/anthology/D07-1090.

# Conclusion

The predictive model presented here shows:
1. Adequate predictive capability, but generally the predictions are not great.        
2. One and two word predictions typically make sense, with suitable predictions typically in the top 5 matches offered.       
3. Three, and larger word phrase predictions are unsatisfactory.      
4. On a positive note, the model response is quick.
5. Also, the model consistently appears to cycle, or "back off", through the ngrams.     
6. The "Stupid Backoff Prediction" approach, as implemented, needs optimization.     

# How to operate Application

The App layout and operation is simple. On the left panel are operational instructions, a user word or phrase input box, and an execution button. Blue text gives a brief description of the model and black text the procedure to run the App, as also highlighted below. To the bottom of the user input box is a submit button that will execute the operation. The user input will take anything from a single word to a long phrase. 

On execution any phrases longer than four words will be truncated, and only the last four words are kept and submitted to predict the next or fifth word from the pentagram library. On obtaining a result, the "original sentence entered" is printed to the top of the main panel, followed by the ten top word matches with their calculated "Stupid Backoff Prediction Frequencies", hence giving preference to the most likely match in priority from top to bottom.

If a word match is not found in the pentagram library, the model will remove the first word submitted from the input phrase and enter the last three words to the tetragram library to predict the fourth word. If no match is made, the model removes another word from the input phrase and submits the last two words to the trigram library to predict the third word. If no match is made, another word is removed from the input phrase and the final remaining word used to predict the next word using the bigram library. If no positive match is made from the bigram, the model, by default, extracts the top 10 highest ranked words from the unigram.

Stupid Backoff App Operation:
1. Input text word or phrase in the user input box.
2. Hit the "submit" button.	
3. The original sentence and best word predictions appear on the main panel.
