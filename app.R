# =================================================
# : Coursera.org
# : Data Science Specialization - Capstone Project
# : January. 2016
# :
# : Shiny Application: Predicting Next Word
# :
# : Author  - Sergio Vicente
# : twitter - @svicente99
# =================================================

library(shiny)
source("functions.R")


server <- function(input, output, session) {

  # include the js code
  # includeScript("mycode.js")

  
  output$text <- renderText({
    paste("Input text is:", input$text)
  })

  observe({
    iniTime <- Sys.time()
    
    textCleansed <- clean(input$text)
    if(textCleansed != " ") 
    {
      output$cleaned <- renderText({
        paste0("Cleansed text: [",textCleansed,"]")
      })

      textCleansed <- gsub(" \\* "," ",textCleansed)    # not taking account of profanity terms
      predictWords <- predict_model(textCleansed)
      updateSelectInput(session = session, inputId = "predicts", choices = predictWords)

      endTime <- Sys.time()
      output$msg <- renderText({
        paste(msg, "\n", sprintf("- Total time processing = %6.3f msecs",1000*(endTime-iniTime)))
      })
    }  
  })
}

ui <- fluidPage(
	# Application title
  	titlePanel("Data Science Capstone Project - Sergio Vicente"),
  	fluidRow(HTML("<div style='margin-left:18px;margin-bottom:12px;color:navy;'><strong>Creation date: Jan.2016</strong></div>") ),
  	    
  	# User interface controls1
    sidebarLayout(
	    sidebarPanel(
			p("Input a word or text and press <ENTER> or click <Predict> to see the next word(s) suggestions:"),	
			textInput(inputId="text", label = ""),
			submitButton("Predict"),
			HTML('<script type="text/javascript"> 
        document.getElementById("text").focus();
        </script>')
	    ),

		mainPanel(
		   	tabsetPanel(

		   	  tabPanel("Result", 
		   	    conditionalPanel(condition = "input.text != ''",
              verbatimTextOutput("text"),
              verbatimTextOutput("cleaned"), verbatimTextOutput("msg"),
              selectInput("predicts","Word predictions:",choices=c(""))
      	    ),
		   	    conditionalPanel(condition = "input.text != '' && nGram==1",
		   	      HTML("<img src='wordcloud.png' style='width:90px;height:90px;border:solid 1pt #c0c0c0;'/>")
		   	    )
		   	  ),                 
			    tabPanel("Documentation", htmlOutput("help"),
			    	tags$div(id="help", 
			    	  HTML("<iframe id='ifrHelp' src='help.html' height='550' width='650'></iframe>")
			    	)
			    )
			)
		)
	)
)


shinyApp(ui = ui, server = server)


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
# References I've used to write this code:
# ---------------------------------------- (in order of development)

# Application layout guide
# http://shiny.rstudio.com/articles/layout-guide.html

# Customize your UI with HTML
# http://shiny.rstudio.com/articles/html-tags.html

# Natural Language Processing: A Model to Predict a Sequence of Words
# http://www.modsimworld.org/papers/2015/Natural_Language_Processing.pdf

# Bigrams and Trigrams
# http://english.boisestate.edu/johnfry/files/2013/04/bigram-2x2.pdf

# Shiny - Conditional Panels
# http://shiny.rstudio.com/gallery/conditionalpanel-demo.html

# R Presentation - knit tables
# http://cpsievert.github.io/slides/markdown/#/

# R Presentation - Top 5 CSS Customizations
# http://rstudio-pubs-static.s3.amazonaws.com/27777_55697c3a476640caa0ad2099fe914ae5.html#/
