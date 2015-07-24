library(shiny)
library(DT)
library(FNN)
	
GIIcalc <- function(MMR, ABR, PRf, SEf, SEm, LFPRf, LFPRm){
	PRm <- 1-PRf
	Gf <- (sqrt(10/MMR * 1/ABR) * sqrt(PRf*SEf)*LFPRf)^(1/3)
	Gm <- ( 1 * sqrt(PRm*SEm)*LFPRm )^(1/3)
	H <- ( (Gf^-1 + Gm^-1) / 2 ) ^ -1
	health.bar <- ( sqrt( 10/MMR*1/ABR) + 1) / 2
	empower.bar <- ( sqrt(PRf*SEf) + sqrt(PRm*SEm))/2
	LFPR.bar <- (LFPRf + LFPRm)/2
	Gfm <- ( health.bar * empower.bar * LFPR.bar  ) ^(1/3)
	return( 1 - H/ Gfm )
}

server <- function(input, output) {
  d <- read.csv('gii2013.csv', stringsAsFactors=FALSE)
  d <- na.omit(d)
  
  GII <- reactive({ 
	GIIcalc(input$MMR, input$ABR, input$PRf, input$SEf, input$SEm, input$LFPRf, input$LFPRm)
  })
  
  matchGII <- function(e){
	if(!is.null(e) ){
		knnx.index(d[, c('HDI', 'GII')], cbind(e$x,e$y), k = 1)
	}
  }
  
  
   output$GII <- renderText({
	 #paste0('x: ', k$x)
	 paste0( "CURRENT GII: ", GII())
	 
   })
  
  k <- reactiveValues( x = NULL, y = NULL, country = NULL, i = NULL)
  
  observeEvent(input$GIItable_row_last_clicked ,{
	 e <- input$GIItable_row_last_clicked
	 # k$x <- d[e,'HDI']
	 # k$y <- d[e, 'GII']
	 # k$country <- d[e, 'country']
	 k$i <- e
  })
  
  observeEvent( input$plot_click, {
	e <- input$plot_click
	k$i <-  matchGII(e)
	# k$x <- g[,2]
	# k$y <- g[,3]
	# k$country <- g[,1]
  })
  

  
  output$plot <- renderPlot({
	if(  ! is.null(input$plot_click) |  !is.null(input$GIItable_row_last_clicked)){}
	gii <- GII()
	plot(d$HDI , d$GII, xlim = c(0,1), ylim= c(0,1), xlab = 'HDI', ylab = 'GII')
	abline( h = gii , col = 'red')
	
		try( points(d[k$i,c('HDI','GII')], cex = 2, col = 'blue', lwd =2) )
	
  })
  
  output$country <- DT::renderDataTable(d[k$i,])
  
  output$GIItable <- DT::renderDataTable(d, select = 'single')
  
}

ui <- fluidPage(
	fluidRow(
		column(12,
		  sidebarLayout(
			sidebarPanel(
			  sliderInput("MMR", "Maternal Mortality Ratio", min = 10, max = 1000, value = 200),
			  sliderInput("ABR", "Adolescent Birth Rate", min = 10, max = 1000, value = 47),
			  sliderInput("PRf", "Female Parliamentary Representation", min = .001, max = 1, value = .007),
			  sliderInput("SEf", "Secondary Education Attainment (female)", min = .01, max = 1, value = .076),
			  sliderInput("SEm", "Secondary Education Attainment (male)", min = .01, max = 1, value = .244),
			  sliderInput("LFPRf", "labor participation rate (female)", min = .01, max = 1, value = .252),
			  sliderInput("LFPRm", "labor participation rate (male)", min = .01, max = 1, value = .718),
			  verbatimTextOutput('GII')
			),
			mainPanel(plotOutput("plot", click = "plot_click", height = "800px"), DT::dataTableOutput('country'))
		  )
		)
	),
	fluidRow(
		column(12,
			h2("2013 Data"),
			DT::dataTableOutput("GIItable")
		)
	)
)

shinyApp(ui = ui, server = server)