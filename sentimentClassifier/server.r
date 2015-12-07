library(shiny)

shinyServer(function(input, output) {
	
	# tweet <- reactiveValues(data=NULL)
	
# 	ds <- reactive({
# 		tmp <- read.csv("~/Desktop/tst.txt")
# 		id <- sample(tmp[is.na(tmp[,'emotion']),'id'], size=1)
# 		as.character(tmp[id,'tweet'])
# 	})
	path <- "~/Desktop/tst.txt"
	values <- reactiveValues()
	
		updateData <- function(path){
			tmp <- read.csv(path, header=T, sep=',')
			if(length(is.na(tmp[,'emotion']))==0) stop('All done :)') else return(tmp)
		}
		tmp <- updateData(path)
		
		# updateID <- function(tmp){
			# values[['id']] <- sample(tmp[is.na(tmp[,'emotion']),'id'], size=1)
		# }
		# updateID(tmp)
	
# 		rate <- function(rating){
# 			tmp[ values[['id']] ,'emotion'] <- rating
# 			write.csv(x=tmp, file=path)
# 			updateData()	
# 		}
		
	observeEvent(input$submitPOS, {
		tmp <- updateData(path)
		values[['id']] <- sample(tmp[is.na(tmp[,'emotion']),'id'], size=1)
		tmp[ values[['id']] ,'emotion'] <- 4
		write.csv(x=tmp, file=path, row.names=F)
		tmp <- updateData(path)
		# updateID(tmp)
	})
	observeEvent(input$submitNEU, {
		tmp <- updateData(path)
		values[['id']] <- sample(tmp[is.na(tmp[,'emotion']),'id'], size=1)
		tmp[ values[['id']] ,'emotion'] <- 2
		write.csv(x=tmp, file=path, row.names=F)
		tmp <- updateData(path)
		# updateID(tmp)
	})
	observeEvent(input$submitNEG, {
		tmp <- updateData(path)
		values[['id']] <- sample(tmp[is.na(tmp[,'emotion']),'id'], size=1)
		tmp[ values[['id']] ,'emotion'] <- 0
		write.csv(x=tmp, file=path, row.names=F)
		tmp <- updateData(path)
		# updateID(tmp)
	})
	
	output$tweet <- renderText({ 
		# ds()
		as.character(tmp[ values[['id']] ,'tweet'])
		# ifelse(values[['id']]== ..., done, as.character(tmp[ values[['id']] ,'tweet']) )
	})
	
})