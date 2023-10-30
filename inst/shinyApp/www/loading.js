var loaditem = document.getElementById("loader-wrapper");

// We recycle the prototab class which let's us smoothly turn objects visible/invisible
loaditem.classList.add("prototab");

// When you get server response, stop loading
Shiny.addCustomMessageHandler('loading', function(a) {

	if (!a) {
		loaditem.classList.add("prototabhidden");
	}
	
});

// Start loading is client side, whenever user presses a button (local)
var listOfButtons = ["simtree", "simtax", "simfossils"];

for (let i = 0; i < listOfButtons.length; i++) {

	document.getElementById("inputSidebar-" + listOfButtons[i]).onclick = function() {loaditem.classList.remove("prototabhidden")};

}
