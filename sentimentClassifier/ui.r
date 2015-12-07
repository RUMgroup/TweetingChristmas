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
			actionButton("submitPOS", "Positive", class = "btn-primary"),
			actionButton("submitNEU", "Neutral", class = "btn-primary"),
			actionButton("submitNEG", "Negative", class = "btn-primary"),
			p("Classify the tweet to display a new tweet.")
		)
	)
))
# 
# shinyApp(
# 	ui = fluidPage(
# 		
# 		fluidRow(
# 			column(6,
# 						 div(
# 						 	id = "form",
# 						 	
# 						 	textInput("name", labelMandatory("Name"), ""),
# 						 	textInput("favourite_pkg", labelMandatory("Favourite R package")),
# 						 	checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
# 						 	sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
# 						 	selectInput("os_type", "Operating system used most frequently",
# 						 							c("",  "Windows", "Mac", "Linux")),
# 						 	actionButton("submit", "Submit", class = "btn-primary"),
# 						 	
# 						 	shinyjs::hidden(
# 						 		span(id = "submit_msg", "Submitting..."),
# 						 		div(id = "error",
# 						 				div(br(), tags$b("Error: "), span(id = "error_msg"))
# 						 		)
# 						 	)
# 						 ),
# 						 
# 						 shinyjs::hidden(
# 						 	div(
# 						 		id = "thankyou_msg",
# 						 		h3("Thanks, your response was submitted successfully!"),
# 						 		actionLink("submit_another", "Submit another response")
# 						 	)
# 						 )
# 			),
# 			column(6,
# 						 uiOutput("adminPanelContainer")
# 			)
# 		)
# 	),
# 	server = function(input, output, session) {
# 		
# 		# Enable the Submit button when all mandatory fields are filled out
# 		observe({
# 			mandatoryFilled <-
# 				vapply(fieldsMandatory,
# 							 function(x) {
# 							 	!is.null(input[[x]]) && input[[x]] != ""
# 							 },
# 							 logical(1))
# 			mandatoryFilled <- all(mandatoryFilled)
# 			
# 			shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
# 		})
# 		
# 		# Gather all the form inputs (and add timestamp)
# 		formData <- reactive({
# 			data <- sapply(fieldsAll, function(x) input[[x]])
# 			data <- c(data, timestamp = epochTime())
# 			data <- t(data)
# 			data
# 		})    
# 		
# 		# When the Submit button is clicked, submit the response
# 		observeEvent(input$submit, {
# 			
# 			# User-experience stuff
# 			shinyjs::disable("submit")
# 			shinyjs::show("submit_msg")
# 			shinyjs::hide("error")
# 			
# 			# Save the data (show an error message in case of error)
# 			tryCatch({
# 				saveData(formData())
# 				shinyjs::reset("form")
# 				shinyjs::hide("form")
# 				shinyjs::show("thankyou_msg")
# 			},
# 			error = function(err) {
# 				shinyjs::text("error_msg", err$message)
# 				shinyjs::show(id = "error", anim = TRUE, animType = "fade")
# 			},
# 			finally = {
# 				shinyjs::enable("submit")
# 				shinyjs::hide("submit_msg")
# 			})
# 		})
# 		
# 		# submit another response
# 		observeEvent(input$submit_another, {
# 			shinyjs::show("form")
# 			shinyjs::hide("thankyou_msg")
# 		})
# 		
# 		# render the admin panel
# 		output$adminPanelContainer <- renderUI({
# 			if (!isAdmin()) return()
# 			
# 			div(
# 				id = "adminPanel",
# 				h2("Previous responses (only visible to admins)"),
# 				downloadButton("downloadBtn", "Download responses"), br(), br(),
# 				DT::dataTableOutput("responsesTable") 
# 			)
# 		})
# 		
# 		# determine if current user is admin
# 		isAdmin <- reactive({
# 			is.null(session$user) || session$user %in% adminUsers
# 		})    
# 		
# 		# Show the responses in the admin table
# 		output$responsesTable <- DT::renderDataTable(
# 			loadData(),
# 			rownames = FALSE,
# 			options = list(searching = FALSE, lengthChange = FALSE)
# 		)
# 		
# 		# Allow user to download responses
# 		output$downloadBtn <- downloadHandler(
# 			filename = function() { 
# 				sprintf("mimic-google-form_%s.csv", humanTime())
# 			},
# 			content = function(file) {
# 				write.csv(loadData(), file, row.names = FALSE)
# 			}
# 		)    
# 	}
# )