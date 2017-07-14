require("shiny")
require("reshape")
require("ggplot2")
require("plotly")

#1. use require instead of library
#2. four functions: name.tabPanel
#                   name.siderbarPanel
#                   name.prepareData
#                   name.preparePlot
#3. profile to pass parameters for specific data
#4. function fileLocate to locate the date in selected folder
#5. In prepareData, use <<- instead of <- or == to bring the reactive variable out
#6. name each variables with prefix to aviod name conflict
#7  global data frame sampleInfo as the sample profiles
#8. touch server.R to enforce the update
#9. use conditionalPanel in sidebarPanel
#10. global data frame all_parameters to get folder specific parameters
#11. reactive data is shareable
#12. To use the module for one data, put "tools	toolname" in main.profile in the folder
#13. To add parameter for a module for a folder, put "key	value" in toolname in the folder

boxplot.tabPanel<-function(profile){
	tabPanel("Box Plot",div(id = "boxplot.container"
          , div(class = "plotlybars-wrapper"
            , div( class="plotlybars"
              , div(class="plotlybars-bar b1")
              , div(class="plotlybars-bar b2")
              , div(class="plotlybars-bar b3")
              , div(class="plotlybars-bar b4")
              , div(class="plotlybars-bar b5")
              , div(class="plotlybars-bar b6")
              , div(class="plotlybars-bar b7")
            )
            , div(class="plotlybars-text"
              , p("loading")
            )
          )
          , plotlyOutput("boxplot.plot")),htmlOutput("boxplot.frame"),textOutput("boxplot_link"),includeScript("tmp.js"))
}
boxplot.sidebarPanel<-function(profile,input,session){
	title = profile$profile_value[profile$profile_key=="title"]
	subtitle = profile$profile_value[profile$profile_key=="subtitle"]
	query <- parseQueryString(session$clientData$url_search)
	conditionalPanel(condition = "input.tabs1 == 'Box Plot' || input.tabs1 == 'Statistic tests' || input.tabs1 == 'T-test'",
	                 #h4(paste(nrow(profile),"rows")),h5(title),h5(subtitle),
				           selectInput("boxplot.type","Select a test",levels(share.data()$variable),selected=query["col"])
#    			includeScript("tmp.js")   
	)
}

#boxplot.prepareData<-function(input){
	share.data <- reactive({ 
		mdata<-read.table(fileLocate("matrix.tsv"),header=T,sep="\t",check.names = F)
		mdata<-merge(sampleInfo,mdata,by="SampleID")      
 		melt(mdata,id=colnames(sampleInfo))
	})
	share.filteredData <- reactive({
		tmpdata=share.data()
		tmpdata[tmpdata$variable==input$boxplot.type,]
	})
	share.standard <- reactive({
		if(file.exists(fileLocate("standard.csv"))){
			data<-read.csv(fileLocate("standard.csv"))
		}
		else{
			data=NULL
		}
		 data
	})
	boxplot.loadframe<-reactive({
		searchterm=gsub("\\."," ",input$boxplot.type)
		paste0("https://en.wikipedia.org/wiki/Special:Search?search=",searchterm)

	})
#}

#boxplot.preparePlot<-function(input,output){
	output$boxplot.plot<- renderPlotly({
	  p<-ggplot(share.filteredData(),aes_string(x=input$group_by,y="value"))+
	     geom_boxplot(aes_string(fill=input$group_by))+
	     geom_jitter(aes(samid=SampleID),width=0.1,height=0) 
	     # use geom_jitter instead of geom_point so that points with the same value could be separated
		standards=share.standard()
		if(!is.null(standards)){
			frow=which(standards$feature==input$boxplot.type)
			if(length(frow)==1){
				fcols=which(!is.na(standards[frow,]))[-1]
				standard=unlist(standards[frow,fcols])
				stdref=colnames(standards)[fcols]
				tmpdata<-data.frame(standard=standard,stdref=stdref)
				p<-p+geom_hline(data=tmpdata,aes(yintercept=standard,stdref=stdref),linetype="dashed")
			}
		}
		ggplotly(p,source="boxplot.plot")
  }) 
	output$boxplot_link<- renderText({
		event.data <- event_data("plotly_click", source = "boxplot.plot")
	
    	if(is.null(event.data) == T) return("")
		if(length(event.data$pointNumber)>1) return("")
		paste("http://igenomed4.stanford.edu/shiny/shinyapp/individual/?sample=",share.filteredData()[event.data$pointNumber+1,3],sep="")
	}) 
	output$boxplot.frame <- renderUI({
    	tags$iframe(src=boxplot.loadframe(), height=600,width="100%")
   })
#}
