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

sigBoxs.tabPanel<-function(profile){
	tabPanel("Significant Boxes",uiOutput("sigBoxs.ui"))# plotlyOutput("sigBoxs.plot",width="100%",height=input$height))#,height = paste(30+40*length(indBoxSig.data()),"px",sep="") ))
#	tabPanel("Box Plot", plotlyOutput("sigBoxs.plot",width="100%",height ="3000px" ))
}
sigBoxs.sidebarPanel<-function(profile,input,session){
	conditionalPanel(condition = "input.tabs1 == 'Significant Boxes'",
        #h4(paste(nrow(profile),"rows")),h5(title),h5(subtitle),
#		h5(profile$profile_value[profile$profile_key=="desc"])
		sliderInput("sigheight", "Plot Height (px)", min = 0, max = 2000, value = 400),
		h5("selecte height first and then select group-by")
	)
}

#sigBoxs.prepareData<-function(input){
	indBoxSig.data  <- reactive({
		filename=paste(input$group_by,"sigs.csv",sep="_")
		if(file.exists(fileLocate(filename))){
			sigs<-read.csv(fileLocate(filename))
			sigs<-as.character(sigs$x)
		}
		else{
			share.data=share.data()
			sigs=levels(share.data$variable)
		}
		sigs

	})
#}

#sigBoxs.preparePlot<-function(input,output){
	output$sigBoxs.ui<-renderUI({
		div(id = "sigBoxs.container"
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
		,plotlyOutput("sigBoxs.plot",width="100%",height =input$sigheight))# paste(30+40*length(indBoxSig.data()),"px",sep="")))

	})
	output$sigBoxs.plot<- renderPlotly({
		Sys.sleep(1)
		data<-share.data()
		data=data[data$variable %in% indBoxSig.data(),]
		data$height=input$sigheight
		data$variable=as.factor(as.character(data$variable))
#		levels(data$variable)=swr(levels(data$variable))
	  p<-ggplot(data,aes_string(x="variable",group=input$group_by,y="value"))+
    geom_boxplot(aes_string(fill=input$group_by,colour=input$group_by))+coord_flip()
	     # use geom_jitter instead of geom_point so that points with the same value could be separated
		ggplotly(p)
		
  }) 

#}
