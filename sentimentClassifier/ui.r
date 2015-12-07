library(shiny)

shinyUI(fluidPage(
	
	titlePanel("Sentimenting tweets"),
	
	sidebarLayout(
		sidebarPanel(
			textOutput("tweet")
		),
		
		mainPanel(
			# actionButton("start", "Start", class = "btn-primary"),
			actionButton("submitPOS", "Positive", class = "btn-primary"),
			actionButton("submitNEU", "Neutral", class = "btn-primary"),
			actionButton("submitNEG", "Negative", class = "btn-primary"),
			p("Classify the tweet to display a new tweet.")#,
			# actionButton("save", "Finish and save", class = "btn-primary")
		)
	)
))