require("shiny")

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

export.tabPanel<-function(profile){
	selections<-as.character(profile$profile_key)
	names(selections)=profile$profile_value
	tabPanel("Export", selectInput("export.fileDesc","File Description",selections ),
		downloadButton("export.download", "Download"))
}

export.sidebarPanel<-function(profile,input,session){
	conditionalPanel(condition = "input.tabs1 == 'Export'",
	                 h5("Exporting Files")
	)
}

#export.prepareData<-function(input){
	export.fileName <- reactive({
		fileLocate(input$export.fileDesc)

	})
#}

#export.preparePlot<-function(input,output){
	output$export.download<- downloadHandler(
    filename = function() {
		paste("download_",basename(export.fileName()),sep="")
    },
    content = function(file) {
		file.copy(export.fileName(),file)
    }
  )
#}
