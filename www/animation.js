$(function() {
    console.log('initialized');
    init();
});

function showAxes(ctx, axes) {
    var width = ctx.canvas.width;
    var height = ctx.canvas.height;
    var xMin = 0;

    ctx.beginPath();
    ctx.strokeStyle = "rgb(200,200,200)";

    // X-Axis
    ctx.moveTo(xMin, height / 2);
    ctx.lineTo(width, height / 2);

    // Y-Axis
    ctx.moveTo(width / 2, 0);
    ctx.lineTo(width / 2, height);

    // Starting line
    ctx.moveTo(0, 0);
    ctx.lineTo(0, height);

    ctx.stroke();
}

function plotSine(ctx, xOffset, yOffset, iter) {
    var width = ctx.canvas.width;
    var height = ctx.canvas.height;
    var scale = 20;

    ctx.beginPath();
    ctx.lineWidth = 2;
    ctx.strokeStyle = "rgb(200,200,200)";

    // console.log("Drawing point...");
    // drawPoint(ctx, yOffset+step);

    var x = 4;
    var y = 0;
    var amplitude = iter * 100;
    var frequency = 250;
    //ctx.moveTo(x, y);
    ctx.moveTo(x, 200);
    while (x < width) {
        y = height / 2 + amplitude * Math.sin((x + xOffset) / frequency);
        ctx.lineTo(x, y);
        x++;
        // console.log("x="+x+" y="+y);
    }
    ctx.stroke();
    ctx.save();

    //console.log("Drawing point at y=" + y);
    //drawPoint(ctx, y);
    ctx.stroke();
    ctx.restore();
}

function draw() {
    var canvas = document.getElementById("canvas");
    var context = canvas.getContext("2d");

    context.clearRect(0, 0, 2000, 2000);

    plotSine(context, step, 100, iter);
    
    if(higher){
        iter +=0.005;
        if(counter>=300){
            higher = false;
            counter = -300;
        }
    } else {
        iter -= 0.005;
        if(counter >= 300){
            higher = true;
            counter = -300;
        }
    }
    counter += 1;
    setTimeout(function() {window.requestAnimationFrame(draw)}, 20);
    

}

function init() {
    window.requestAnimationFrame(draw);
}
var step = -4;
var iter = 0.1;
var counter = 0;
var higher = true;