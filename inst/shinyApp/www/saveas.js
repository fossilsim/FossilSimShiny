// Retrieving save button from webpage
var saveAsButton = document.getElementById("inputSidebar-saveas");
var currTab = "i"

// Function to save plot as image
function savePlotAs() {
	
	if (document.getElementById("outputSidebar-" + currTab + "tree")) {
		// Retrieving plot from webpage
		var img = document.getElementById("outputSidebar-" + currTab + "tree").getElementsByTagName('img')[0];

		download("plot.png", img.src)
	}

}

// Attributing saveas function to button onclick event
saveAsButton.onclick = function() {savePlotAs();};

// Function that creates a download button and clicks it so we can spontaneously download files
function download(file, source) {
  
	//creating an invisible element
	var element = document.createElement('a');
	element.href = source;
	element.setAttribute('download', file);
  
	// Above code is equivalent to
	// <a href="path of file" download="file name">
  
	document.body.appendChild(element);
  
	//onClick property
	element.click();
  
	document.body.removeChild(element);
}

Shiny.addCustomMessageHandler('tab-for-saveas', function(message) {currTab = message});
