library(shiny)

load("tweets.Christmas.sampled.RData")
tmp <- tmp
if(!any(names(tmp)%in%c('sentiment'))){
	tmp$sentiment <- NA
	tmp$id <- 1:nrow(tmp)
	}

updateID <- function(tmp){
	sample(tmp[ is.na(tmp[,'sentiment']) ,'id'], size=1)
}

shinyServer(function(input, output) {
	
	values <- reactiveValues(id=NA)

	# observeEvent(input$start, {
	values[['id']] <- updateID(tmp)
	# })
	
		
	observeEvent(input$submitPOS, {
		tmp[ tmp[,'id']==values[['id']] ,'sentiment'] <<- 4
		save(tmp, file = "tweets.Christmas.sampled.RData")
		values[['id']] <<- updateID(tmp)
	})
	observeEvent(input$submitNEU, {
		tmp[ tmp[,'id']==values[['id']] ,'sentiment'] <<- 2
		save(tmp, file = "tweets.Christmas.sampled.RData")
		values[['id']] <<- updateID(tmp)
	})
	observeEvent(input$submitNEG, {
		tmp[ tmp[,'id']==values[['id']] ,'sentiment'] <<- 0
		save(tmp, file = "tweets.Christmas.sampled.RData")
		values[['id']] <<- updateID(tmp)
	})
	
	observeEvent(input$save, {
		save(tmp, file = "tweets.Christmas.sampled.RData")
		# stop()
	})
	
	output$tweet <- renderText({ 
		ifelse(is.na(values[['id']]), 'Press start', as.character(tmp[ values[['id']] ,'text']))
	})
	
})