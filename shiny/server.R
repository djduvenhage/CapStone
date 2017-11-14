
library(shiny)


##########################################################################################################################################    
 

function(input, output, session) {
        
                #Main server code
                output$prediction <- renderPrint({
                
                                        back_off(input$testString);
                                        
                input$action;
                
                })
        
                output$sentence <- renderText({
                                
                                        paste(stringCleaned(input$testString));       
                        
                
                })
}

