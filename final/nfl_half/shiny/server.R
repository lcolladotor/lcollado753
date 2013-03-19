## setup
library(shiny)
load("fits.Rdata")

## Inverse logit
ilogit <- function(x) { exp(x)/(1+exp(x))}

## Prediction function
getPred <- function(f, newdata) {
	logitA <- predict(f, newdata=newdata[1,])
	logitB <- predict(f, newdata=newdata[2,])
	p <- ilogit(logitA - logitB)
	return(c(p, 1-p))
}


## Main shiny function
shinyServer(function(input, output) {
	
	## Use the appropriate model
	modelInput <- reactive({
		switch(input$model,
			"2011" = fits[["2012"]],
			"2012" = fits[["2013"]]
		)
	})
	
	constructData <- reactive({
		fit <- modelInput()
		
		data <- data.frame(matrix(rep(NA, 2*12), nrow=2))
		colnames(data) <- c("teamAoPassYdsAtt", "teamAoRun", "teamAdPassYdsAtt", "teamAdRunAtt", "teamBoRun", "teamBoFumble", "teamBdRunAtt", "local", "halfdiff", "resumes", "gwrA", "gwrB")
		data$teamAoPassYdsAtt <- c(input$teamAoPassYdsAtt, input$teamBoPassYdsAtt)
		data$teamAoRun <- c(input$teamAoRun, input$teamBoRun)
		data$teamAdPassYdsAtt <- c(input$teamAdPassYdsAtt, input$teamBdPassYdsAtt)
		data$teamAdRunAtt <- c(input$teamAdRunAtt, input$teamBdRunAtt)
		data$teamBoRun <- rev(data$teamAoRun)
		data$teamBoFumble <- c(input$teamBoFumble, input$teamAoFumble)
		data$teamBdRunAtt <- rev(data$teamAdRunAtt)
		local <- as.logical(input$local)
		data$local <- c(local, !local)
		data$halfdiff <- c(input$halfdiff, (-1)*input$halfdiff)
		resumes <- as.logical(input$resumes)
		data$resumes <- c(resumes, !resumes)
		data$gwrA <- c(input$gwrA, input$gwrB)
		data$gwrB <- rev(data$gwrA)
		return(data)
	})
	
	toshow <- reactive({
		data <- constructData()
		toshow <- t(data)
		colnames(toshow) <- c("Team A", "Team B")
		toshow
	})
	
	output$summary <- renderPrint({
	    fit <- modelInput()
	    summary(fit)
	})
	
	output$yourval <- renderTable({
		toshow()
	})
	
	output$prediction <- renderText( {
		fit <- modelInput()
		data <- constructData()				
		predictions <- round(getPred(f=fit, newdata=data), 3)
		paste("Team A has", predictions[1], "probability of winning. Team B has", predictions[2], "probability of winning.")
		
	})
	
	output$plot1 <- renderPlot({
		fit <- modelInput()
		plot(fit, which=1)
	})
	
	output$plot2 <- renderPlot({
		fit <- modelInput()
		plot(fit, which=2)
	})
	
	output$plot3 <- renderPlot({
		fit <- modelInput()
		plot(fit, which=3)
	})
	
	output$plot4 <- renderPlot({
		fit <- modelInput()
		plot(fit, which=4)
	})
	
	output$plot5 <- renderPlot({
		fit <- modelInput()
		plot(fit, which=5)
	})
	
	output$plot6 <- renderPlot({
		fit <- modelInput()
		plot(fit, which=6)
	})
	
	output$downloadData <- downloadHandler(
	    filename = function() { paste("yourdata", sample(1:10000, 1), '.csv', sep='') },
	    content = function(file) {
	      write.csv(toshow(), file)
	    }
	  )
	
}) 