library(shiny)

load("tweets.Christmas.sampled.RData")
# tmp <- tweets.Christmas.sampled
if(!any(names(tmp)%in%c('sentiment'))){
	tmp$sentiment <- NA
	tmp$id <- 1:nrow(tmp)
}
# counter <- 1
# ids <- tmp[ is.na(tmp[,'sentiment']) ,'id']
updateID <- function(tmp){
	sample(tmp[ is.na(tmp[,'sentiment']) ,'id'], size=1)
}

shinyServer(function(input, output) {
	
	values <- reactiveValues(id=NA, done=sum(!is.na(tmp[,'sentiment'])))
	# values[['id']] <- ids[counter]

	# observeEvent(input$start, {
		values[['id']] <- updateID(tmp)
	# })
	
		
	observeEvent(input$submitPOS, {
		tmp[ tmp[,'id']==values[['id']] ,'sentiment'] <<- 4
		save(tmp, file = "tweets.Christmas.sampled.RData")
		values[['id']] <<- updateID(tmp)
		values[['done']] <<- sum(!is.na(tmp[,'sentiment']))
		# counter <<- counter+1
		# values[['id']] <<- ids[counter]
	})
	observeEvent(input$submitNEU, {
		tmp[ tmp[,'id']==values[['id']] ,'sentiment'] <<- 2
		save(tmp, file = "tweets.Christmas.sampled.RData")
		values[['id']] <<- updateID(tmp)
		values[['done']] <<- sum(!is.na(tmp[,'sentiment']))
		# counter <<- counter+1
		# values[['id']] <<- ids[counter]
	})
	observeEvent(input$submitNEG, {
		tmp[ tmp[,'id']==values[['id']] ,'sentiment'] <<- 0
		save(tmp, file = "tweets.Christmas.sampled.RData")
		values[['id']] <<- updateID(tmp)
		values[['done']] <<- sum(!is.na(tmp[,'sentiment']))
		# counter <<- counter+1
		# values[['id']] <<- ids[counter]
	})
	
# 	observeEvent(input$save, {
# 		save(tmp, file = "tweets.Christmas.sampled.RData")
# 		# stop()
# 	})
	
	# Displays the current tweet
	output$tweet <- renderText({ 
		as.character(tmp[ values[['id']] ,'text'])
		# ifelse(is.na(values[['id']]), 'Press start', as.character(tmp[ values[['id']] ,'text']))
	})
	
	output$tweetCounter <- renderText({ 
		paste('You finished',values[['done']] ,'tweets!')
	})
	
})