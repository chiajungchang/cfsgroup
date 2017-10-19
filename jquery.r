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

jquery.tabPanel<-function(profile){
	tabPanel("JBrowse",tags$iframe(src=paste("http://igenomed.stanford.edu/CFS_WGS/",profile$profile_value[profile$profile_key=="sublink"],sep=""), height=800,width="100%") )
}
jquery.sidebarPanel<-function(profile,input,session){
	conditionalPanel(condition = "input.tabs1 == 'JBrowse'",
					h4("see https://jbrowse.org/")
	)
}

#}
