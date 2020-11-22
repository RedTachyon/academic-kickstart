let colors = ["black", "blue", "brown", "cyan", "green", "lime", "orange", "pink", "purple", "red", "white", "yellow"];
let crewmates = {};
for (let color of colors) {
    let crewmate = new Image();
    crewmate.src = `img/crewmates/${color}.png`;
    crewmates[color] = crewmate;
}
let rooms = {
    upeng: [80, 80],
    cafe: [400, 40],
    medbay: [260, 150],
    reactor: [20, 200],
    security: [190, 190],
    storage: [260, 370],
    admin: [530, 260],
    o2: [570, 180],
    nav: [790, 180]
};
function drawMap(c, ctx, image) {
    ctx.drawImage(image, 0, 0, c.width, c.height);
}
function drawCrewmate(c, ctx, room, color, row, col) {
    if (room == "error")
        return;
    let xRatio = c.width / 885;
    let yRatio = c.height / 469;
    let [x, y] = rooms[room];
    let size = 0.03 * c.width;
    ctx.drawImage(crewmates[color], xRatio * x + col * (size + 1), yRatio * y + row * (size + 1), size, size);
}
function drawCrewmates(c, ctx, room, colors) {
    for (let i = 0; i < colors.length; i++) {
        drawCrewmate(c, ctx, room, colors[i], Math.floor(i / 3), i % 3);
    }
}
function drawAll(c, ctx, image, state) {
    ctx.clearRect(0, 0, c.width, c.height);
    drawMap(c, ctx, image);
    for (let room in state.positions) {
        drawCrewmates(c, ctx, room, state.positions[room]);
    }
}
export { drawCrewmate, drawCrewmates, drawMap, drawAll };
//# sourceMappingURL=vis.js.map