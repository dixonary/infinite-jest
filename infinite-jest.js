(function () {
    var gameWindow = document.querySelector("#__layout > * > .container");
    var logo = document.querySelector(".site-title");
    // Lookup existing id or generate a fresh one
    var id = localStorage.getItem("infinite-jest-id") || crypto.randomUUID();
    localStorage.setItem("infinite-jest-id", id);
    var overlaps = [];
    var conn = new WebSocket("ws://dixonary.co.uk:12024");
    conn.onopen = function () {
        console.log("Connected to Infinite Jest");
        conn.send(id);
    };
    conn.onmessage = function (e) {
        console.log("==> ".concat(e.data));
        if (e.data === "join") {
            summonJoinForm();
            return;
        }
        if (JSON.parse(e.data).clients) {
            // Room update!
            renderRoom(JSON.parse(e.data));
            return;
        }
    };
    var sendEvent = function (event, payload) {
        console.log("<== ".concat(payload));
        conn.send(JSON.stringify({ event: event, payload: payload }));
    };
    var renderRoom = function (room) {
        var _a, _b, _c;
        (_a = document.querySelector("#room")) === null || _a === void 0 ? void 0 : _a.remove();
        var $room = document.createElement("room");
        $room.id = "room";
        var $header = document.createElement("h2");
        $header.textContent = room.rid;
        $room.appendChild($header);
        if (room.leader === id) {
            var $forms = document.createElement("div");
            if (room.status === "Lobby") {
                var $playForm = document.createElement("form");
                $playForm.id = "playForm";
                $playForm.style.display = "flex";
                $playForm.style.flexDirection = "column";
                $playForm.style.gap = "0.5em";
                $playForm.innerHTML = "\n      <div><button type=\"submit\" style=\"margin-right:0.5em\">Start game</button><span>with</span><input type=\"number\" id=\"num-items\" min=\"1\" max=\"25\" value=\"9\" style=\"margin:0 0.5em; width:3em\"/><span> targets</span></div><div style=\"display:flex;gap:0.5em\">Include: <textarea id=\"include-targets\" rows=\"5\"></textarea></div>";
                $forms.appendChild($playForm);
                $playForm.onsubmit = function (e) {
                    e.preventDefault();
                    var itemsInput = document.querySelector("#num-items");
                    var items = itemsInput.value;
                    var includeInput = document.querySelector("#include-targets");
                    sendEvent("start", JSON.stringify({ numItems: parseInt(items), includeTargets: includeInput.value.split("\n") }));
                };
            }
            $room.appendChild($forms);
        }
        var $items = document.createElement("ul");
        $items.id = "items-list";
        var _loop_1 = function (item) {
            var $li = document.createElement("li");
            $li.classList.add("target");
            if (((_b = item.claim) === null || _b === void 0 ? void 0 : _b.by) === id) {
                $li.classList.add("yes");
            }
            else if (item.claim) {
                $li.classList.add("no");
            }
            $li.innerHTML += "".concat(item.name);
            if (item.claim) {
                var userName = room.clients[item.claim.by].name;
                $li.textContent += " (".concat(userName, " with ").concat(item.claim.with, ")");
            }
            // Add an unclaim trigger
            if (((_c = item.claim) === null || _c === void 0 ? void 0 : _c.by) === id) {
                $li.addEventListener("dblclick", function () {
                    sendEvent("unclaim", JSON.stringify({ unclaim: item.name }));
                });
            }
            $items.appendChild($li);
        };
        for (var _i = 0, _d = room.items; _i < _d.length; _i++) {
            var item = _d[_i];
            _loop_1(item);
        }
        $room.appendChild($items);
        var $spacer = document.createElement("div");
        $spacer.classList.add("spacer");
        $spacer.style.flexGrow = "1";
        $room.appendChild($spacer);
        var $clients = document.createElement("ul");
        $clients.id = "clients-list";
        for (var _e = 0, _f = Object.values(room.clients); _e < _f.length; _e++) {
            var client = _f[_e];
            var $li = document.createElement("li");
            $li.innerHTML = client.name;
            if (room.leader === client.id) {
                $li.innerHTML += "<span style='font-size:1.1em;'> &star;</span>";
            }
            $clients.appendChild($li);
        }
        $room.appendChild($clients);
        gameWindow.appendChild($room);
        // Move the "reset" button next to the other buttons
        var $reset = document.querySelector(".reset");
        var $sideControls = document.querySelector(".side-controls");
        $reset.style.position = "absolute";
        $reset.style.bottom = "40px";
        $reset.style.textAlign = "right";
        setInterval(function () {
            $reset.style.width = "".concat($sideControls.getBoundingClientRect().width, "px");
            $reset.style.left = "".concat($sideControls.getBoundingClientRect().left, "px");
        }, 100);
        // Add mousemove handlers to items
        var realItems = Array.from(document.querySelectorAll(".item.instance:not(.instance-hide"));
        var targets = Array.from(document.querySelectorAll("#items-list li"));
    };
    var overlap = function (e1, e2) {
        var r1 = e1.getBoundingClientRect();
        var r2 = e2.getBoundingClientRect();
        return !(r1.right < r2.left ||
            r1.left > r2.right ||
            r1.bottom < r2.top ||
            r1.top > r2.bottom);
    };
    var summonJoinForm = function () {
        var $joinForm = document.createElement("form");
        $joinForm.id = "joinForm";
        $joinForm.style.display = "flex";
        $joinForm.style.flexDirection = "column";
        // add 2px gap between elements
        $joinForm.style.gap = "2px";
        $joinForm.innerHTML = "\n    <input type=\"text\" id=\"room-code\" placeholder=\"Room Code\" autocomplete=\"off\" />\n    <input type=\"text\" id=\"player-name\" placeholder=\"Player Name\" autocomplete=\"off\" />\n    <button type=\"submit\">Join</button>\n    ";
        gameWindow.appendChild($joinForm);
        $joinForm.style.position = "absolute";
        $joinForm.style.top = "".concat(logo.getBoundingClientRect().bottom + 8, "px");
        $joinForm.style.left = "0.5em";
        $joinForm.onsubmit = function (e) {
            e.preventDefault();
            var roomInput = document.querySelector("#room-code");
            var room = roomInput.value;
            if (room.length === 0)
                return;
            var nameInput = document.querySelector("#player-name");
            var name = nameInput.value;
            if (name.length === 0)
                return;
            joinRoom(room, name);
        };
        // TESTING
        // joinRoom("UWCS", "Alex");
    };
    var joinRoom = function (room, name) {
        sendEvent("join", JSON.stringify({ room: room, name: name }));
        var joinForm = document.querySelector("#joinForm");
        joinForm.remove();
    };
    // Add new styles to the page
    var style = document.createElement("style");
    style.textContent = "\n  #room {\n    display:flex;\n    flex-direction:column;\n\n    justify-content:space-between;\n\n    position:absolute;\n    left:8px;\n    top:50px;\n    height:calc(100% - 58px);\n\n    overflow:hidden;\n\n    & h2 {\n      font-size: 20px;\n      margin: 0;    \n    }\n  }\n  #items-list {\n    list-style-type: none;\n    padding: 0;\n    overflow-y:auto;\n    direction:rtl;\n    \n    display: flex;\n    flex-direction: column;\n    align-items: end;\n\n    &>*{\n      direction:ltr;\n    }\n    & li {\n      padding: 9px 10px 8px;\n      border-radius:5px;\n      border:1px solid #c8c8c8;\n      font-size:16.4px;\n      display:block;\n      \n      &:not(:last-child) {\n        margin-bottom: 6px;\n      }\n\n      &::before {\n        content: \"\uD83C\uDFAF\";\n        margin-right: 6px;\n        display:inline-block;\n      }\n      &.yes {\n        background: linear-gradient(0deg,#d4ffd4,#fff 70%);\n        &::before {\n          content: \"\u2611\uFE0F\";\n        }\n      }\n      &.no {\n        background: linear-gradient(0deg,#ffd4d4,#fff 70%);\n\n        &::before {\n          content: \"\u274C\";\n        }\n      }\n\n      background: linear-gradient(0deg,#fffad4,#fff 70%);\n    }\n  }\n  #clients-list {\n    list-style-type: none;\n    padding: 0;\n    &::before {\n      content: \"Players:\";\n      margin-bottom: 6px;\n      font-weight: bold;\n      display: block;\n    }\n  }\n\n  ";
    document.body.appendChild(style);
    document.addEventListener("mousemove", function (e) {
        overlaps.length = 0;
        // If left mouse button is down
        if (e.buttons === 1) {
            var $room_1 = document.querySelector("#room");
            // First, get a list of all items that overlap the room
            var items = Array.from(document.querySelectorAll(".item.instance:not(.instance-hide)")).filter(function (item) { return overlap(item, $room_1); });
            var targets = Array.from(document.querySelectorAll("#items-list li"));
            var _loop_2 = function (item) {
                var _loop_3 = function (target) {
                    if (overlap(item, target)) {
                        if (!overlaps.some(function (o) { return o.item === item; }) && !overlaps.some(function (o) { return o.target === target; })) {
                            overlaps.push({ item: item, target: target });
                        }
                    }
                };
                for (var _d = 0, targets_2 = targets; _d < targets_2.length; _d++) {
                    var target = targets_2[_d];
                    _loop_3(target);
                }
            };
            for (var _i = 0, items_1 = items; _i < items_1.length; _i++) {
                var item = items_1[_i];
                _loop_2(item);
            }
            for (var _a = 0, targets_1 = targets; _a < targets_1.length; _a++) {
                var target = targets_1[_a];
                // Change target background color
                target.style.background = "";
            }
            for (var _b = 0, overlaps_1 = overlaps; _b < overlaps_1.length; _b++) {
                var _c = overlaps_1[_b], item = _c.item, target = _c.target;
                // Change target background color
                target.style.background = "linear-gradient(0deg,#d6fcff,#fff 90%)";
            }
        }
    });
    document.addEventListener("mouseup", function (e) {
        // If the overlaps array is not empty, claim the first item with the first target
        while (overlaps.length > 0) {
            var item = overlaps[0].item;
            var target = overlaps[0].target;
            overlaps.shift();
            sendEvent("claim", JSON.stringify({ item: item.textContent, target: target.textContent }));
        }
    });
})();
