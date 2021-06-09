function hideButton() {
  var x = document.getElementById("start_exercise");
  if (x.style.display === "none") {
    x.style.display = "block";
  } else {
    x.style.display = "none";
  }
}