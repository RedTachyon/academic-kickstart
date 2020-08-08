let canvas = document.getElementById("canvas");
let ctx = canvas.getContext("2d");
const width = canvas.width;
const height = canvas.height;

ctx.fillStyle = "#000000";

ctx.translate(300, 300);
ctx.rotate(-Math.PI/4);
ctx.translate(-300, -300);

let arm1 = {
  h:
  {
    l: 100, // left
    m: 160, // mid
    r: 301, // bottom
  },
  v: 
  {
    b: 330, // bottom
    m: 270, // mid
    t: 130,  // top
  }
};


const L = arm1.v.b - arm1.v.t;
const w = arm1.h.m - arm1.h.l;


function getArm2(t) {
  return {
    v: {
      b: 270,
      m: 330,
      t: (t < 0.5) ? 330 : (330 + 2*(L-w)*(t-0.5)),
    }, 
    h: {
      l: (t < 0.5) ? (width/2 + 2*t*(L - 60))       : (width/2 + L - 60),
      m: (t < 0.5) ? (width/2 + 2*t*(L - w - 60)) : (width/2 + L - w - 60),
      r: 300,
    }};
}

function drawArm(ctx, arm) { 
  ctx.beginPath();

  ctx.moveTo(arm.h.l, arm.v.t);
  ctx.lineTo(arm.h.l, arm.v.b);
  ctx.lineTo(arm.h.r, arm.v.b);
  ctx.lineTo(arm.h.r, arm.v.m);
  ctx.lineTo(arm.h.m, arm.v.m);
  ctx.lineTo(arm.h.m, arm.v.t);


  ctx.closePath();

  ctx.fill();
}

function crosshairs(ctx, x, y) {
  ctx.beginPath();
  ctx.moveTo(x-1, 0);
  ctx.lineTo(x-1, height);
  ctx.lineTo(x+1, height);
  ctx.lineTo(x+1, 0);
  ctx.closePath();
  ctx.fill();
  
  ctx.beginPath();
  ctx.moveTo(0, y-1);
  ctx.lineTo(width, y-1);
  ctx.lineTo(width, y+1);
  ctx.lineTo(0, y+1);
  ctx.closePath();
  ctx.fill();
}


function getSword(t) {
  return {
    x: t < 0.5 ? 220 + 2*20*t: 240,
    y: 130,
    w: t < 0.5 ? 40 + 2*20*t : 60,
    h: t < 0.5 ? 220 + 2*120*t : 340,
  };
}

function drawSword(ctx, sword) {
  ctx.fillRect(sword.x, sword.y, sword.w, sword.h);
}

function getTip(t) {
  return [
    {x: t < 0.5 ? 220 + 2*20*t : 240, y: 130},
    {x: t < 0.5 ? 260 + 2*40*t : 300, y: 130},
    {x: t < 0.5 ? 240 + 2*30*t : 270, y: t < 0.5 ? 110 + 2*20*t : 130},
  ]
}

function drawTip(ctx, tip) {
  ctx.beginPath();
  ctx.moveTo(tip[0].x, tip[0].y);
  ctx.lineTo(tip[1].x, tip[1].y);
  ctx.lineTo(tip[2].x, tip[2].y);
  ctx.closePath();
  ctx.fill();
}

function getHiltLeft(t) {
  return {
    x: t < 0.5 ? 220 + 2*20*t + 1: 240 + 1,
    y: t < 0.5 ? 250 + 2*220*t: 470,
    w: t < 0.5 ? -30 - 1 : -30 - 2*110*(t-0.5) - 1,
    h: t < 0.5 ? -40 - 2*20*t : -60,
  }
}

function getHiltRight(t) {
  return {
    x: t < 0.5 ? 260 + 2*40*t - 1 : 290 - 1,
    y: t < 0.5 ? 210 - 2*80*t : 130,
    w: t < 0.5 ? 30 + 1 : 30 + 2*120*(t-0.5) + 1,
    h: t < 0.5 ? 40 + 2*20*t : 60,
  }
}

console.log(getHiltRight(0));
console.log(getHiltRight(1));

function drawHilt(ctx, hilt) {
  ctx.fillRect(hilt.x, hilt.y, hilt.w, hilt.h);
}

function draw(t) { 
  // Function to draw the entire thing.
  
  drawArm(ctx, arm1);
  drawArm(ctx, getArm2(t))
  drawSword(ctx, getSword(t));
  drawTip(ctx, getTip(t));
  drawHilt(ctx, getHiltLeft(t));
  drawHilt(ctx, getHiltRight(t));
  //crosshairs(ctx, 300, 300);
  
}

let t = 0;
let dt = 0.01;
draw(t)

let globalTimer;

function animate(direction) {
    if (globalTimer) {
        clearInterval(globalTimer);
    }
    let timer = setInterval(() => {
        t += direction * dt;
        ctx.clearRect(0, 0, width, height);
        draw(t);
        if (t >= 1) clearInterval(timer);
        if (t <= 0) clearInterval(timer);

    }, 1000/60); // animation speed
    globalTimer = timer;
}

let direction = 1

canvas.onclick = () => {
    animate(direction);
    direction *= -1;
}
// setTimeout(() => {
//   let timer = setInterval(() => {
//     t += dt;
//     ctx.clearRect(0, 0, width, height);
//     draw(t);
//     if (t >= 1) {
//       clearInterval(timer);
//     }
//   }, 1000/60);

//   timer = setInterval(() => {
//     t -= dt;
//     ctx.clearRect(0, 0, width, height);
//     draw(t);
//     if (t >= 1) {
//       clearInterval(timer);
//     }
//   }, 1000/60);
// }, 50);
