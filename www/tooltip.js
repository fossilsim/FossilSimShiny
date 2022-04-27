var tooltip = document.getElementById("tooltip");

// Tooltip data :
// FORMAT : "key/id" : "description"
var tooltip_dict = {
	"lambda" :               "Here you can set the speciation rate, commonly referred as λ. A speciation event corresponds to the birth of a new species.",
	"mu" :                   "Here you can set the extinction rate, commonly referred as µ.",
	"tips" :                 "The number of tips is the number of unique non-exctinct species at the end of a tree.",
	"simtree" :              "Click here to generate a tree.",
	
	"usertree" :             "Check this box if you want to import your own tree using the text box below.",
	"newick" :               "Import your tree in this text box, make sure it's in the Newick format.",
	
	"taxonomybeta":          "#todo",
	"taxonomylambda":        "#todo",
	"simtax":                "#todo",
	
	
	"uniform":               "Select this if you want fossil sampling to follow a uniform distribution/poisson distribution.",
	"non-uniform":           "Select this if you want fossil sampling to follow a non-uniform distribution.",
	"psi" :                  "Here you can set the sampling rate, commonly referred as Ψ.",
	
	"newfossils":            "Click here to generate fossil data using <i>fossilsim</i>.",
	
	"showtree":              "Toggle show/hide tree.",
	"showtaxonomy":          "Toggle show/hide fossil taxonomy.",
	"showfossils":           "Toggle show/hide fossils.",
	"showranges":            "#todo",
	"showstrata":            "#todo",
	"showtips":				 "Toggle show/hide tips.",
	"reconstructed":         "Toggle show/hide reconstructed tree with current fossil data.",
	
	"saveas":                "Click here to save your tree as an image, you can also just simply right click on the tree select save as."
	
};

// Clear tooltip on page load
clearTooltip();

// Getting all of the elements that have a tooltip according to tooltip_dict
var tooltip_keys = Object.keys(tooltip_dict);
for (let i = 0; i < tooltip_keys.length; i++) {
	//todo -- fix : console.log(tooltip_keys[i])
	var tooltip_element = document.getElementById("inputSidebar-" + tooltip_keys[i]);
	
	// Adding a mousehover function that displays or clears the correct tooltip for each element
	tooltip_element.addEventListener('mouseenter', e => {
		setTooltip(tooltip_keys[i]);
	});
	
	tooltip_element.addEventListener('mouseleave', e => {
		clearTooltip();
	});
}

// Function that sets the tooltip with the right text, to be called when mouse enters element
function setTooltip(key) {
	tooltip.innerHTML = "&nbsp;&nbsp;&nbsp;&nbsp;ℹ : " + tooltip_dict[key];
}

// Function that clears the tooltip, to be called when the mouse leaves the element
function clearTooltip() {
	tooltip.innerHTML = "&nbsp;&nbsp;&nbsp;&nbsp;ℹ : ";
}

