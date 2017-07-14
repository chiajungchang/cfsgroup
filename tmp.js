$('#boxplot_link').on('shiny:value', function(event) {
  // append a character string to the output value
	if(event.value!=""){
		window.open(event.value,'_blank');
	}
});
