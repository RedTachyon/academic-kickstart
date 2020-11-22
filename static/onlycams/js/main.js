import { CamEvent, processEvent } from './game.js';
import { drawMap, drawAll } from "./vis.js";
const canvas = document.getElementById("cams-img");
const ctx = canvas.getContext("2d");
const roomForm = document.getElementById("room");
const fromForm = document.getElementById("from");
const toForm = document.getElementById("to");
const colorForm = document.getElementById("color");
const saveBtn = document.getElementById("save");
const fastInput = document.getElementById("fast-input");
const fastSubmit = document.getElementById("fast-submit");
const eventsListElem = document.getElementById("events-list");
const slider = document.getElementById("myRange");
let output = document.getElementById("slider-value");
const forms = [roomForm, fromForm, toForm, colorForm];
const widthRatio = 0.4;
let shortColors = {
    r: "red",
    blu: "blue",
    g: "green",
    pi: "pink",
    o: "orange",
    bla: "black",
    w: "white",
    pu: "purple",
    br: "brown",
    c: "cyan",
    l: "lime",
};
let eventsList = [];
let statesList = [];
function getFormInfo() {
    let room = roomForm.elements['room'].value;
    let from = fromForm.elements['from'].value;
    let to = toForm.elements['to'].value;
    let color = colorForm.elements['color'].value;
    return new CamEvent(room, from, to, color);
}
function getFastInfo() {
    let inp = fastInput.value;
    let [col, room, from, to] = inp.split(" ");
    let color;
    for (let short in shortColors) {
        if (col.startsWith(short)) {
            color = shortColors[short];
        }
    }
    return new CamEvent(room, from, to, color);
}
function resetForms() {
    for (let form of forms) {
        form.reset();
    }
}
function clearEvents() {
    eventsList = [];
}
let worldMap = new Image();
worldMap.src = 'img/skeld.png';
worldMap.onload = () => {
    canvas.width = widthRatio * window.innerWidth;
    canvas.height = canvas.width * worldMap.height / worldMap.width;
    drawMap(canvas, ctx, worldMap);
};
// Room selection
roomForm.onkeydown = (e) => {
    if (e.key.toLowerCase() == "q") {
        document.getElementById("room-q")["checked"] = true;
    }
    else if (e.key.toLowerCase() == "w") {
        document.getElementById("room-w")["checked"] = true;
    }
    else if (e.key.toLowerCase() == "a") {
        document.getElementById("room-a")["checked"] = true;
    }
    else if (e.key.toLowerCase() == "s") {
        document.getElementById("room-s")["checked"] = true;
    }
    console.log(roomForm.elements['room'].value);
};
// From selection
fromForm.onkeydown = (e) => {
    if (e.key.toLowerCase() == "q") {
        document.getElementById("from-unk")["checked"] = true;
    }
    else if (e.key.toLowerCase() == "w") {
        document.getElementById("from-w")["checked"] = true;
    }
    else if (e.key.toLowerCase() == "a") {
        document.getElementById("from-a")["checked"] = true;
    }
    else if (e.key.toLowerCase() == "s") {
        document.getElementById("from-s")["checked"] = true;
    }
    else if (e.key.toLowerCase() == "d") {
        document.getElementById("from-d")["checked"] = true;
    }
    console.log(fromForm.elements['from'].value);
};
// To selection
toForm.onkeydown = (e) => {
    if (e.key.toLowerCase() == "q") {
        document.getElementById("to-unk")["checked"] = true;
    }
    else if (e.key.toLowerCase() == "w") {
        document.getElementById("to-w")["checked"] = true;
    }
    else if (e.key.toLowerCase() == "a") {
        document.getElementById("to-a")["checked"] = true;
    }
    else if (e.key.toLowerCase() == "s") {
        document.getElementById("to-s")["checked"] = true;
    }
    else if (e.key.toLowerCase() == "d") {
        document.getElementById("to-d")["checked"] = true;
    }
    console.log(toForm.elements['to'].value);
};
window.onkeydown = (e) => {
    if (e.keyCode == 13) {
        fastSubmit.click();
    }
};
saveBtn.onclick = () => {
    // resetForms();
    document.getElementById("room-q").focus();
    let event = getFormInfo();
    console.log(event);
    eventsList.push(event);
    // console.log(eventsList);
    let vent;
    [statesList, vent] = processEvent(statesList, event);
    let node = document.createElement("LI");
    let textNode = document.createTextNode(`${vent ? "VENT" : ""} ${event.color} in ${event.camRoom.toUpperCase()} from ${event.from.toUpperCase()} to ${event.to.toUpperCase()}`);
    node.appendChild(textNode);
    eventsListElem.appendChild(node);
    console.log(statesList);
    if (vent) {
        console.log("SOMEONE VENTED");
    }
    slider.max = statesList.length;
    slider.value += 1;
    drawAll(canvas, ctx, worldMap, statesList[slider.value - 1]);
    output.innerHTML = slider.value;
};
fastSubmit.onclick = () => {
    // resetForms();
    document.getElementById("fast-input").focus();
    let event = getFastInfo();
    fastInput.value = "";
    console.log(event);
    eventsList.push(event);
    // console.log(eventsList);
    let vent;
    [statesList, vent] = processEvent(statesList, event);
    let node = document.createElement("LI");
    let textNode = document.createTextNode(`${vent ? "VENT" : ""} ${event.color} in ${event.camRoom.toUpperCase()} from ${event.from.toUpperCase()} to ${event.to.toUpperCase()}`);
    node.appendChild(textNode);
    eventsListElem.appendChild(node);
    console.log(statesList);
    if (vent) {
        console.log("SOMEONE VENTED");
    }
    slider.max = statesList.length;
    slider.value += 1;
    drawAll(canvas, ctx, worldMap, statesList[slider.value - 1]);
    output.innerHTML = slider.value;
};
// Update the current slider value (each time you drag the slider handle)
slider.oninput = () => {
    drawAll(canvas, ctx, worldMap, statesList[slider.value - 1]);
    output.innerHTML = slider.value;
};
window.onload = () => {
    document.getElementById("fast-input").focus();
};
//# sourceMappingURL=main.js.map