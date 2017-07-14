require("shiny")
#require("reshape")
#require("ggplot2")
#require("plotly")

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

wilcoxonTest.tabPanel<-function(profile){
	tabPanel("Wilcoxon Test",verbatimTextOutput("wilcoxonTest"))
}

wilcoxonTest.sidebarPanel<-function(profile,input,session){
  conditionalPanel(condition = "input.tabs1 == 'Wilcoxon Test'" 
                 #  selectInput("wilcoxonTest.type","Select a test",levels(wilcoxonTest.data()$variable))
	)
}

wilcoxonTest.prepareData<-function(input){
}

wilcoxonTest.preparePlot<-function(input,output){
  output$wilcoxonTest <- renderPrint({
	formula=paste("value ~ ",input$group_by,sep="")
	wilcox.test(as.formula(formula),share.filteredData(),exact=FALSE)

  })
}
