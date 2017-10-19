require("shiny")
require("reshape")
require("ggplot2")
require("plotly")
require("heatmaply")

RBcol <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
"#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")))


Heatmap.tabPanel<-function(profile){
	#tabPanel("eatmap",verbatimTextOutput("Heatmap"))
  tabPanel("Heatmap",div(id = "Heatmap.container"
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
          ,plotlyOutput("Heatmap",height="768px")))
}

Heatmap.sidebarPanel<-function(profile,input,session){
  conditionalPanel(condition = "input.tabs1 == 'Heatmap'"
	)
	
}

#Heatmap.prepareData<-function(input){
  Heatmap.mergeData<-reactive({
			data<-read.table(fileLocate("matrix.tsv"),header=T,sep="\t",check.names = F)
			data<-merge(sampleInfo,data,by="SampleID")
			data
	})
  Heatmap.sampleData<-reactive({
	sdata=share.mergeData()[,c(1:ncol(sampleInfo))]
  })
  Heatmap.data <- reactive({ 
			data=share.mergeData()[,-c(1:ncol(sampleInfo))]
			data=data[,which(apply(data, 2, var, na.rm=TRUE)!=0)] #remove constant
			for(i in 1:ncol(data)){
				data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE) #set NA as average
			}
			data=t(scale(data))
			colnames(data)=Heatmap.sampleData()$SampleID
			data
  })
#}

#Heatmap.preparePlot<-function(input,output){

  output$Heatmap <- renderPlotly({
    heatmaply(Heatmap.data(),colors=RBcol,col_side_colors=t(Heatmap.sampleData()[,-1]),xlab="sampleID",ylab="tests",margins=c(100,100,30,0))
  })
#}
