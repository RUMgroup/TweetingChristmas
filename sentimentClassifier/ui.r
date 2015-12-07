library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
	
	# Application title
	titlePanel("Sentimenting tweets"),
	
	# Sidebar with a slider input for the number of bins
	sidebarLayout(
		sidebarPanel(
			textOutput("tweet")
		),
		
		# Show a plot of the generated distribution
		mainPanel(
			# actionButton("start", "Start", class = "btn-primary"),
			actionButton("submitPOS", "Positive", class = "btn-primary"),
			actionButton("submitNEU", "Neutral", class = "btn-primary"),
			actionButton("submitNEG", "Negative", class = "btn-primary"),
			p("Classify the tweet to display a new tweet."),
			actionButton("save", "Finish and save", class = "btn-primary")
		)
	)
))