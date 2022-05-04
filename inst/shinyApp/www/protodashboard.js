var keys = ["treetab", "taxtab", "fossiltab", "appearancetab"];

for (let i = 0; i < keys.length; i++) {
	document.getElementById("inputSidebar-" + keys[i] + "-button").onclick = function() {
		document.getElementById("inputSidebar-" + keys[i] + "-content").classList.toggle("prototabhidden");
	}
}

