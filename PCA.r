require("shiny")
require("reshape")
require("ggplot2")
require("plotly")
require("ggfortify")

PCA.tabPanel<-function(profile){
	#tabPanel("PCA",verbatimTextOutput("PCA"))
  tabPanel("PCA",div(id = "eggHead.container"
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
          ,plotlyOutput("PCA")),
			h5("variance"),plotOutput("PCA_variance_plot"))
}

PCA.sidebarPanel<-function(profile,input,session){
  
  conditionalPanel(condition = "input.tabs1 == 'PCA'",
				selectInput("pca_xc","Select x component",colnames(PCA.pcadata()$x)[1:min(5,ncol(PCA.pcadata()$x))],selected="PC1"),
				selectInput("pca_yc","Select y component",colnames(PCA.pcadata()$x)[1:min(5,ncol(PCA.pcadata()$x))],selected="PC2")
                #selectInput("PCA.type","Test",levels(PCA.data()$variable))
				
	)
	
}

#PCA.prepareData<-function(input){
  PCA.mergeData<-reactive({
			data<-read.table(fileLocate("matrix.tsv"),header=T,sep="\t",check.names = F)
			data<-merge(sampleInfo,data,by="SampleID")
			data
	})
  PCA.sampleData<-reactive({
	sdata=share.mergeData()[,c(1:ncol(sampleInfo))]
  })
  PCA.pcadata <- reactive({
			data=share.mergeData()[,-c(1:ncol(sampleInfo))]
			data=data[,which(apply(data, 2, var, na.rm=TRUE)!=0)] #remove constant
			for(i in 1:ncol(data)){
				data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE) #set NA as average
			}
			prcomp(data,scale=T)
     })
	PCA.variance <- reactive({
		vars <- apply(PCA.pcadata()$x, 2, var)
		props <- vars / sum(vars)
		

	})
	PCA.drawdata <- reactive({
		if(file.exists(fileLocate("pca.tsv"))){
			data<-read.delim(fileLocate("pca.tsv"),check.names=F)
			data<-merge(sampleInfo,data,by="SampleID")
		}
		else{
			data<-PCA.pcadata()$x[,1:min(5,ncol(PCA.pcadata()$x))]
			data<-cbind(PCA.sampleData(),data)
			rdata=data
			rdata$SampleID=sampleRIDs[rdata$SampleID]
			write.table(rdata[,-c(2:ncol(sampleInfo))],fileLocate("pca.tsv"),sep="\t",row.names=F)
			data<-cbind(PCA.sampleData(),data[,c(input$pca_xc,input$pca_yc)])
			
		}
		data$SampleID=sampleRIDs[data$SampleID]
		data
	})
#}

#PCA.preparePlot<-function(input,output){

  output$PCA <- renderPlotly({
   # p<-autoplot(PCA.pcadata(), data=sampleInfo,colour=input$group_by,samid="SampleID",frame = TRUE, frame.type = 'norm')
	p<-ggplot(PCA.drawdata(),aes_string(x=input$pca_xc,y=input$pca_yc,color=input$group_by))+
		geom_point(aes(samid=SampleID))+stat_ellipse()
	ggplotly(p)
  })
	output$PCA_variance_plot <- renderPlot({
      plot(PCA.pcadata(),type="l")
    })
#}
