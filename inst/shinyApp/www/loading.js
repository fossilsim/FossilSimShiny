var loaditem = document.getElementById("loader-wrapper");

// We recycle the prototab class wich let's us smoothly turn objects visible/invisible
loaditem.classList.add("prototab");

Shiny.addCustomMessageHandler('loading', function(a) {
	if (a) {
		loaditem.classList.remove("prototabhidden");
	}

	if (!a) {
		loaditem.classList.add("prototabhidden");
	}
	
});
