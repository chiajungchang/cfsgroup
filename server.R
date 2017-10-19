source("global.r")

page="group"


# Define server logic required to draw a histogram
function(input, output,session) {
	source("boxplot.r",  local = TRUE)$value
	source("PCA.r",  local = TRUE)$value
	source("Heatmap.r",  local = TRUE)$value
	source("export.r",  local = TRUE)$value
	source("sigBoxs.r",  local = TRUE)$value
	source("Tests.r",  local = TRUE)$value
	source("jquery.r",  local = TRUE)$value
    foldLocate <- reactive({
		file.path(datafolder,input$mainfolder,input$subfolder)
	})
	fileLocate<-function(filename){
		file.path(foldLocate(),filename)
	}
    userRole <- reactive({ input$userRole }) 

#	for(tool in unique(all_parameters$profile_value[all_parameters$profile==page & all_parameters$profile_key=="tools"])){
#		do.call(paste(tool,"prepareData",sep="."),list(input=input))
#		do.call(paste(tool,"preparePlot",sep="."),list(input=input,output=output))
#	}
#	do.call("prepareData",list(input=input))

  output$folder<-  renderUI({
	 if(userRole()=="subscriber"){
	  query <- parseQueryString(session$clientData$url_search)
	  selectInput("mainfolder","Category",choices = levels(all_parameters$mainfolder),selected=query["folder"])
		}
	else{
		h5("unauthorized user")
	}
  })
  output$subfolder<-  renderUI({
	 if(userRole()=="subscriber"){
	  query <- parseQueryString(session$clientData$url_search)
      selectInput("subfolder", "Subcategory", unique(all_parameters$subfolder[all_parameters$mainfold==input$mainfolder]),selected=query["subfolder"])
		}
	else{
		h5("unauthorized user")
	}
  })
  output$dynamicTabPanel=renderUI({
	tools=all_parameters$profile_value[all_parameters$mainfolder==input$mainfolder & all_parameters$subfolder==input$subfolder     & all_parameters$profile==page & all_parameters$profile_key=="tools"]
	myTabs=list()
	i=1
	for(tool in tools){
		profile=all_parameters[all_parameters$mainfolder==input$mainfolder & all_parameters$subfolder==input$subfolder & all_parameters$profile==tool ,]
		myTabs[[i]]=do.call(paste(tool,"tabPanel",sep="."),list(profile=profile))
		i=i+1

	}


	myTabs$id="tabs1"
	do.call(tabsetPanel, myTabs)

  })
  output$toolPanel=renderUI({
	tools=all_parameters$profile_value[all_parameters$mainfolder==input$mainfolder & all_parameters$subfolder==input$subfolder     & all_parameters$profile==page & all_parameters$profile_key=="tools"]
	myTabs=list()
	i=1
	for(tool in tools){
		profile=all_parameters[all_parameters$mainfolder==input$mainfolder & all_parameters$subfolder==input$subfolder & all_parameters$profile==tool ,]
		myTabs[[i]]=do.call(paste(tool,"sidebarPanel",sep="."),list(profile=profile,input=input,session=NULL))
		i=i+1

	}
	do.call(tagList, myTabs)
  })
}
