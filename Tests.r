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

Tests.tabPanel<-function(profile){
	tabPanel("Statistic tests",verbatimTextOutput("Tests.print"))
}

Tests.sidebarPanel<-function(profile,input,session){
  conditionalPanel(condition = "input.tabs1 == 'Statistic tests'",
                   selectInput("Tests.type","Select a statistic test",c("T-test","Wilcoxon Test","ANOVA"))
	)
}


#Tests.preparePlot<-function(input,output){
  output$Tests.print <- renderPrint({
	singleformula=paste("value ~",input$group_by,sep=" ")
	multformula=paste("value ~", paste(colnames(sampleInfo)[-1],collapse="*"),sep=" ")
	if(input$Tests.type=="T-test"){
		t.test(as.formula(singleformula),share.filteredData())
	}
	else if(input$Tests.type=="Wilcoxon Test"){
		wilcox.test(as.formula(singleformula),share.filteredData(),exact=FALSE)
	}
	else if(input$Tests.type=="ANOVA"){
		summary(aov(as.formula(multformula),data=share.filteredData()))
	}
  })
#}
