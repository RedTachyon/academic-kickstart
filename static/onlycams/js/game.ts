class CamEvent {
    constructor(public camRoom: string, public from: string, public to: string, public color: string) {}
}

class CameraRoom {

    constructor(public name: string,
                public w: Room,
                public a: Room,
                public s: Room,
                public d: Room) {}
}

// class Room {
//     constructor(public name: string) {}
// }

type Room = string;

let rooms = {
    reactor: "reactor",
    security: "security",
    upeng: "upeng",
    medbay: "medbay",
    cafe: "cafe",
    nav: "nav",
    admin: "admin",
    storage: "storage",
    o2: "o2",
    error: "error",
}

let cameras = {
    q: new CameraRoom("q", rooms.cafe, rooms.o2, rooms.storage, rooms.nav),
    w: new CameraRoom("w", rooms.cafe, rooms.error, rooms.storage, rooms.admin),
    a: new CameraRoom("a", rooms.error, rooms.upeng, rooms.medbay, rooms.cafe),
    s: new CameraRoom("s", rooms.upeng, rooms.reactor, rooms.storage, rooms.security)
}


function getRoomFrom(event: CamEvent): string {
    return event.from == "?" ? rooms.error : cameras[event.camRoom][event.from];
}

function getRoomTo(event: CamEvent): string {
    return cameras[event.camRoom][event.to];
}

class GameState {
    positions: { [key: string]: Array<string>; };

    constructor() {

        this.positions = {};
        for (let roomName in rooms) {
            this.positions[roomName] = [];
        }
    }
}

function findColor(state: GameState, color: string): string {
    for (let roomName in state.positions) {
        if (state.positions[roomName].indexOf(color) >= 0) {
            return roomName;
        }
    }
    return "none";
}

function processEvent(states: Array<GameState>, event: CamEvent): [Array<GameState>, boolean] {
    if (states.length == 0) {
        let oldState = new GameState();
        let newState = new GameState();

        if (event.from != "?") {
            oldState.positions[getRoomFrom(event)].push(event.color);
        }
        newState.positions[getRoomTo(event)].push(event.color);

        return [[oldState, newState], false];

    } else {

        let vent = false;
        let from = getRoomFrom(event);
        let to = getRoomTo(event);

        // Add the color to all previous records
        for (let state of states.slice().reverse()) {
            let colorRoom = findColor(state, event.color);

            if (colorRoom == "none") {
                // Appears from no previous information, fill and keep going
                console.log(state.positions);
                console.log(event);
                if (from !== "?") {state.positions[from].push(event.color);}
            } else if (colorRoom == from) {
                // Was here previously, everything's okay
                break;
            } else {
                // Uh-oh, someone vented!
                vent = true;
                break;
            }

        }

        // Add the new record
        let newState = new GameState();
        let lastState = states[states.length - 1];

        // Copy the previous state
        for (let roomName in lastState.positions) {
            newState.positions[roomName] = [...lastState.positions[roomName]];
        }


        if (!vent) {
            newState.positions[from].splice(newState.positions[from].indexOf(event.color), 1);
            newState.positions[to].push(event.color);
        } else {
            let colorRoom = findColor(newState, event.color);
            newState.positions[colorRoom].splice(newState.positions[colorRoom].indexOf(event.color), 1);

            console.log(newState.positions);

            newState.positions[to].push(event.color);
        }

        states.push(newState);
        return [states, vent];
    }

}





export {CamEvent, processEvent, GameState}