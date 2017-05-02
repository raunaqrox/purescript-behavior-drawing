exports.move = function(sub) {
  addEventListener("mousemove", function(e) {
    sub({x: e.clientX, y: e.clientY})
  })
}
window.test = function(sub) {
  return function(acsValue) {
    var i = 0;
    setInterval(function(){
      i ++;
      sub(acsValue + " " + i);
    }, 3000)
  }
}

exports.acsEvent = function(sub) {
  window.start = window.test(sub)("testing");
}


