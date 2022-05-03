// Theme names
themes = ["classic", "git-dark", "git-light"];
// Theme icon colors
theme_colors = ["#4a3c4a;", "#28303b", "#ffffff"];

// Create a button for each theme
for (let i = 0; i < themes.length; i++) {
	let button = document.createElement("button");
	
	button.setAttribute("class", "themeButton");
	button.setAttribute("style", "background-color : " + theme_colors[i] + ";");
	button.setAttribute("onClick", "changeTheme(" + i.toString() + ");")
	
	document.getElementById("themebar-fixed").append(button);
	document.getElementById("themebar-fixed").append(" ");
}

// Function used to changed theme, assigned to each button
function changeTheme(idx) {
	document.documentElement.className = themes[idx];
}
