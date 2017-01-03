/* dynamic offset for affix nav */
$('nav').affix({
      offset: {
        top: $('#titleHeader').height()+$('#intro').outerHeight()
      }
});

var offset = 50;

$('.navbar li a').click(function(e) {
  e.preventDefault();
  // store hash
  var hash = this.hash;
  // animate
  $('html, body').animate({
      scrollTop: $(hash).offset().top-50
    }, 300, function(){
  });
});

/* parallax effect */
d3.select('#titleHeader').on('mousemove', function(){
  var width = d3.select(this).style('width').replace('px','');
  var height = d3.select(this).style('height').replace('px','');
  var coordinates = d3.mouse(this);

  var diffX = d3.round((coordinates[0] - width/2) * 50 / (width/2));
  var diffY = d3.round((coordinates[1] - height/2) * 50 / (height/2));
  d3.select(this).style("transform", "scale(1.005) translate("+ diffX/300 + "%, " + diffY/300 +"%)");
  d3.select("#title").style("transform", "scale(0.995) translate("+ (-1*diffX/300) + "%, " + (-1*diffY/300) +"%)");
  d3.select("#subtitle").style("transform", "scale(0.995) translate("+ (-1*diffX/300) + "%, " + (-1*diffY/300) +"%)");
})

d3.select('#titleHeader').on('mouseout', function(){
  d3.select(this).style("transform", "scale(1) translate(0%, 0%)")
  d3.select("#title").style("transform", "scale(1)translate(0%, 0%)")
  d3.select("#subtitle").style("transform", "scale(1)translate(0%, 0%)")

})

/* glitch*/
setInterval(function(){
  var random = Math.floor(Math.random()*2)+1;
  d3.select(".glitch")
    .classed("glitchDynamic_"+random, true)


  setTimeout(function(){
    d3.select(".glitch")
      .classed("glitchDynamic_"+random, false)
  },50)

}, 40000);
;
