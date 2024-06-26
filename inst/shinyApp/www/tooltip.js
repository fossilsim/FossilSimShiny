var tooltip = document.getElementById("tooltip");

// Tooltip data :
// FORMAT : "key/id" : "description"
var tooltip_dict = {
	"lambda" :               "Set the birth rate, commonly referred to as λ. A birth event corresponds to the appearance of a new lineage in the tree.",
	"mu" :                   "Set the death rate, commonly referred to as µ. A death event corresponds to the disappearance of a lineage from the tree.",
	"tips" :                 "Set the number of extant tips at the end of the tree.",
	"simtree" :              "Simulate a tree.",
	
	"newick" :               "Import your tree in this text box, make sure it's in the Newick format.",
	"usertree" :             "Import the user tree from the given Newick string.",
	
	"taxonomybeta":          "Set the probability that any birth event is a bifurcating speciation (as opposed to a budding speciation).",
	"taxonomylambda":        "Set the rate of anagenetic speciation.",
	"simtax":                "Simulate the taxonomy.",
	"cleartax":              "Removed the simulated taxonomy - this cannot be reversed!",
	
	"uniform":               "Uniform fossil sampling (with constant rate across the tree).",
	"non-uniform":           "Piecewise-constant (=skyline) time-dependent fossil sampling. Rates are drawn for each interval from a lognormal distribution.",
	"enviro-dep":			 "Depth-dependent fossil sampling (following Holland, 1995).",
	"lineage-dep":			 "Lineage-dependent fossil sampling. Rates are drawn for each species from a lognormal distribution.",
	
	"uniform-psi" :			 "Set the uniform fossil sampling rate, commonly to referred as Ψ.",
	
	"nonUniformType" :      "Select how to define time intervals (at random, spread evenly on the length of the tree, or defined by the user)",
	"non-uniform-int" :		 "Set the number of time intervals for the fossil sampling rate.",
	"non-uniform-bounds" :  "Set the interval bounds for the fossil sampling rate (delimited by /).",
	"non-uniform-meanrate" : "Set the mean fossil sampling rate across all intervals.",
	"non-uniform-variance" : "Set the variance of the fossil sampling rates across all intervals.",
	
	"enviro-dep-strata" :	 "Set the number of time intervals for the fossil sampling rate.",
	"enviro-dep-pd" :		 "Set the preferred depth (= mean of the sampling distribution) for all time intervals.",
	"enviro-dep-dt" :		 "Set the depth tolerance (= standard deviation of the sampling distribution) for all time intervals.",
	"enviro-dep-pa" :		 "Set the peak abundance (= maximum sampling probability per interval) for all time intervals.",
	
	"lineage-dep-LNrate" :	 "Set the mean fossil sampling rate across all lineages.",
	"lineage-dep-LNsd" : 	 "Set the standard deviation of the fossil sampling rates across all lineages.",
	
	"simfossils":            "Simulate fossil specimens.",
	"clearfossils":          "Remove the simulated fossil specimens - this cannot be reversed!",
	
	"showtree":              "Show/hide the tree.",
	"showtaxonomy":          "Show/hide the fossil taxonomy.",
	"showfossils":           "Show/hide the fossil samples.",
	"showranges":            "Show the fossil species as ranges.",
	"showstrata":            "Show the time intervals used for simulation.",
	"showtips":				 "Show/hide the tip labels.",
	"reconstructed":         "Show only the reconstructed tree (based on the current fossil data and full extant sampling).",
	"enviro-dep-showsamplingproxy":	 "Show the depth values used as environmental proxy (only for depth-dependent fossil sampling).",
	
	"saveas":                "Save the current tab as an image, you can also just simply right click on the image and select save as.",
	"imgformat":			 "Image format to save as.",
	"dldata":				 "Download the simulated tree, taxonomy and fossils as RData.",
	"outputTabset":   "Change between tabs (i), (ii), etc. or create a new tab with the (+) button"
};

// Clear tooltip on page load
clearTooltip();

// Getting all of the elements that have a tooltip according to tooltip_dict
var tooltip_keys = Object.keys(tooltip_dict);
for (let i = 0; i < tooltip_keys.length; i++) {
	var tooltip_element = document.getElementById("inputSidebar-" + tooltip_keys[i]);
	if(tooltip_element == null) tooltip_element = document.getElementById("outputSidebar-" + tooltip_keys[i]);
	
	// Adding a mousehover function that displays or clears the correct tooltip for each element
	tooltip_element.addEventListener('mouseenter', e => {
		setTooltip(tooltip_keys[i]);
	});
	
	tooltip_element.addEventListener('mouseleave', e => {
		clearTooltip();
	});
	
	var tooltip_element = document.getElementById("inputSidebar-" + tooltip_keys[i] + "-label");
	if(tooltip_element != null) {
		tooltip_element.addEventListener('mouseenter', e => {
			setTooltip(tooltip_keys[i]);
		});
		
		tooltip_element.addEventListener('mouseleave', e => {
			clearTooltip();
		});
	}
}

// Function that sets the tooltip with the right text, to be called when mouse enters element
function setTooltip(key) {
	tooltip.innerHTML = "&nbsp;&nbsp;&nbsp;&nbsp;ℹ : " + tooltip_dict[key];
}

// Function that clears the tooltip, to be called when the mouse leaves the element
function clearTooltip() {
	tooltip.innerHTML = "&nbsp;&nbsp;&nbsp;&nbsp;ℹ : hover over any option to see more details";
}

