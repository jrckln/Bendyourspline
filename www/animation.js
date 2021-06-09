$(function() {
    init();
});

function plotSine(ctx, yOffset, iter) {
    var width = ctx.canvas.width;
    var height = ctx.canvas.height;
    var scale = 20;

    ctx.beginPath();
    ctx.lineWidth = 2;
    ctx.strokeStyle = "rgb(200,200,200)";

    var x = 0;
    var y = 0;
    var amplitude = iter * 100;
    var frequency = 270;
    ctx.moveTo(x, 200);
    while (x < width) {
        y = height / 2 + amplitude * Math.sin((x + -4) / frequency);
        ctx.lineTo(x, y);
        x+=20;
    }
    ctx.stroke();
    ctx.save();

    ctx.stroke();
    ctx.restore();
}

function draw() {
    var canvas = document.getElementById("canvas");
    var context = canvas.getContext("2d");

    context.clearRect(0, 0, 2000, 2000);

    plotSine(context, 100, iter);
    
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
    setTimeout(function() {
            window.requestAnimationFrame(draw);
    }, 20);
    

}

function init() {
    window.requestAnimationFrame(draw);
}

var iter = 0.1;
var counter = 0;
var higher = true;