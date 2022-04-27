document.getElementById("pressme").onclick = function() { changeTheme("green"); }

function changeTheme(theme) {
    var element = document.body;
    element.classList.toggle("dark");
	console.log("grrrr");
}