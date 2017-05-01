exports.move = function(sub) {
  addEventListener("mousemove", function(e) {
    sub({x: e.clientX, y: e.clientY})
  })
}
