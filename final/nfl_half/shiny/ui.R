## Setup
source("server.R")

## Specify layout
shinyUI(pageWithSidebar(
	
	headerPanel("Should you keep watching a game of football?"),
	
	
	sidebarPanel(
		## Construct input options
		
		## Choose fitting model
		h4("Model"),
		selectInput("model", "Choose a model. Trained on NFL data from 2006 to", choices = c("2011", "2012")),
		
		h4("Input data for prediction."),
		
		sliderInput("teamAoPassYdsAtt", "Team A's offensive net passing yards per attempt:", min=0, max=10, value=1.0684, step=0.0001),
		sliderInput("teamBoPassYdsAtt", "Team B's offensive net passing yards per attempt:", min=0, max=10, value=1.0684, step=0.0001),
		sliderInput("teamAoRun", "Team A's running success rate:", min=0, max=1, value=0.4939),
		sliderInput("teamBoRun", "Team B's running success rate:", min=0, max=1, value=0.4939),
		sliderInput("teamAdPassYdsAtt", "Team A's defensive net passing yards per attempt:", min=0, max=10, value=1.0618, step=0.0001),
		sliderInput("teamBdPassYdsAtt", "Team B's defensive net passing yards per attempt:", min=0, max=10, value=1.0618, step=0.0001),
		sliderInput("teamAdRunAtt", "Team A's defensive net rushing yards per attempt:", min=0, max=15, value=6.754, step=0.001),
		sliderInput("teamBdRunAtt", "Team B's defensive net rushing yards per attempt:", min=0, max=15, value=6.754, step=0.001),
		#numericInput("teamBoRun", "Number of observations to view:", 10),
		sliderInput("teamAoFumble", "Team A's offensive fumbles per attempt:", min=0, max=0.05, value=0.011770),
		sliderInput("teamBoFumble", "Team B's offensive fumbles per attempt:", min=0, max=0.05, value=0.011770),
		#numericInput("teamBdRunAtt", "Number of observations to view:", 10),
		sliderInput("local", "Is Team A the local team? (Use 1 for yes):", min=0, max=1, step=1, value=1),
		sliderInput("halfdiff", "What is the current half-time score difference? (Positive means that it's in favor of team A)", min=-60, max=60, value=0, step=1),
		sliderInput("resumes", "Does Team A starts by receiving the ball on the second half? (Use 1 for yes):", min=0, max=1, step=1, value=1),
		sliderInput("gwrA", "Team A's current season percent of games won (Use 0 if it's week 1):", min=0, max=1, value=0.5),
		sliderInput("gwrB", "Team B's current season percent of games won (Use 0 if it's week 1):", min=0, max=1, value=0.5)

	
	),
	
	mainPanel(
		tabsetPanel(
			tabPanel("Prediction",
				h4("Game prediction"),
				textOutput("prediction"),
				h5(HTML("Full description at <a href='https://github.com/lcolladotor/lcollado753/tree/master/final/nfl_half'>GitHub</a>. Check the report for details or look at the R code. Post explaining the project at <a href='http://fellgernon.tumblr.com/post/46117939292/predicting-who-will-win-a-nfl-match-at-half-time#.UU5TDlvF2c4'>Fellgernon Bit</a>."))
			),
			
			tabPanel("Model information",
				h4("Trained model information"),
				verbatimTextOutput("summary")
			),
			
			tabPanel("Model diagnostic plots",
				h4("Basic diagnostic plots"),
				plotOutput("plot1"),
				plotOutput("plot2"),
				plotOutput("plot3"),
				plotOutput("plot4"),
				plotOutput("plot5"),
				plotOutput("plot6")
			),
			
			tabPanel("The values you specified", 
				h4("This is the data you specified"),
				tableOutput("yourval"),
				h4("Download your data"),
				downloadButton('downloadData', 'Download')
			)	
			
		)
	)
	
))
