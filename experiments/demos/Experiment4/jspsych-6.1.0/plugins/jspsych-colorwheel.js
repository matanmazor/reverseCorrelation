jsPsych.plugins["colorwheel"] = (function() {

	var plugin = {};

	plugin.info = {
		name: 'Flicker'
	}

	const Cstar = 49;
	const lab_origin = [54,21.5,11.5];

	function polar2lab(Csatr,hstar, origin) {
		astar = Cstar*Math.cos(hstar)+origin[1];
		bstar = Cstar*Math.sin(hstar)+origin[2];
		return([origin[0],astar,bstar])
	}

	function polar2rgb(Cstar, hstar,origin) {
		return(lab2rgb(polar2lab(Cstar,hstar,lab_origin)))
	}

	var segmentCount = 24,
    radius;


	plugin.trial = function(display_element, trial) {

		display_element.innerHTML = ''

		trial.bright_side =  Math.random()>0.5? 'right':'left';

		//open a p5 sketch
		let sketch = function(p) {

		//sketch setup
		p.setup = function() {
			p.createCanvas(p.windowWidth, p.windowHeight);
			p.background(128); //gray
			p.strokeWeight(0);
			p.frameRate(trial.frame_rate);
			p.noCursor();
			var angleStep = p.floor(360 / segmentCount);
			radius = 300;

			p.beginShape(p.TRIANGLE_FAN);
  p.vertex(p.width / 2, p.height / 2);
  for(var angle = 0; angle <= 360; angle += angleStep) {

    var vx = p.width / 2 + p.cos(p.radians(angle)) * radius;
    console.log(p.radians(angle));
    console.log(p.cos(p.radians(angle)));
    console.log(p.cos(p.radians(angle))*radius);

    var vy = p.width / 2 + p.sin(p.radians(angle)) * radius;
    console.log(p.radians(angle));
    console.log(p.sin(p.radians(angle)));
    console.log(p.sin(p.radians(angle))*radius);
    p.vertex(vx, vy);
    //fill(angle, mouseX, mouseY);
    p.fill(polar2rgb(Cstar,-p.radians(angle),lab_origin));

  } // end of for loop
  p.endShape();

	p.ellipseMode(p.CENTER)
	p.fill(128)
	p.circle(p.width/2, p.height/2, 30, 30)

		};



		//organize everything in one sequence
		p.draw = function() {

			}
		};

		// start sketch!
		let myp5 = new p5(sketch);

}

//Return the plugin object which contains the trial
return plugin;
})();
