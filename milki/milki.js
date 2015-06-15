var showdown;
var input;
var preview;
var last = "";

window.onload = function () {
    showdown = new Showdown.converter();
    var post = document.getElementById("wiki-post");

    if (post) {
	post.innerHTML = showdown.makeHtml(post.innerHTML);
	return;
    }

    preview = document.getElementById("wiki-preview");
    if (preview) {
	input = document.getElementById("wiki-input");
	input.focus();
	window.setInterval(function(){
		if(input.value != last) {
		    last = input.value;
		    preview.innerHTML = showdown.makeHtml(last);
		}
	    },100);
    }
}

function open_markdown_cheatsheet() {
    window.open("/milki-static/markdown.html","Markdown cheatsheet","menubar=1,resizable=0,scrollbars=1,width=280,height=500");
}
