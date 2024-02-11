(() => {
  const gameWindow = document.querySelector("#__layout > * > .container")!;
  const logo = document.querySelector(".site-title")!;

  // Lookup existing id or generate a fresh one
  let id = localStorage.getItem("infinite-jest-id") || crypto.randomUUID();
  localStorage.setItem("infinite-jest-id", id);

  const overlaps: { item: HTMLElement, target: HTMLElement }[] = [];

  const conn = new WebSocket("wss://infinite-jest.dixonary.co.uk:2024");
  conn.onopen = () => {
    console.log("Connected to Infinite Jest");
    conn.send(id);
  }
  conn.onmessage = (e) => {
    console.log(`==> ${e.data}`);
    if (e.data === "join") {
      summonJoinForm();
      return;
    }

    if (JSON.parse(e.data).clients) {
      // Room update!
      renderRoom(JSON.parse(e.data));
      return;
    }
  }

  const sendEvent = (event: string, payload: string) => {
    console.log(`<== ${payload}`);
    conn.send(JSON.stringify({ event, payload }));
  }

  const renderRoom = (room: Room) => {

    document.querySelector("#room")?.remove();

    const $room = document.createElement("room");
    $room.id = "room";

    const $header = document.createElement("h2");
    $header.textContent = room.rid;
    $room.appendChild($header);

    if (room.leader === id) {
      const $forms = document.createElement("div");

      if (room.status === "Lobby") {
        const $playForm = document.createElement("form");
        $playForm.id = "playForm";
        $playForm.style.display = "flex";
        $playForm.style.flexDirection = "column";
        $playForm.style.gap = "0.5em";
        $playForm.innerHTML = `
      <div><button type="submit" style="margin-right:0.5em">Start game</button><span>with</span><input type="number" id="num-items" min="1" max="25" value="9" style="margin:0 0.5em; width:3em"/><span> targets</span></div><div style="display:flex;gap:0.5em">Include: <textarea id="include-targets" rows="5"></textarea></div>`;

        $forms.appendChild($playForm);

        $playForm.onsubmit = (e) => {
          e.preventDefault();
          const itemsInput = document.querySelector("#num-items") as HTMLInputElement;
          const items = itemsInput.value;
          const includeInput = document.querySelector("#include-targets") as HTMLTextAreaElement;
          sendEvent("start", JSON.stringify({ numItems: parseInt(items), includeTargets: includeInput.value.split("\n") }));
        }
      }

      $room.appendChild($forms);
    }

    const $items = document.createElement("ul");
    $items.id = "items-list";
    for (const item of room.items) {
      const $li = document.createElement("li");
      $li.classList.add("target");
      if (item.claim?.by === id) {
        $li.classList.add("yes");
      }
      else if (item.claim) {
        $li.classList.add("no");
      }
      $li.innerHTML += `${item.name}`;
      if (item.claim) {
        const userName = room.clients[item.claim.by].name;
        $li.textContent += ` (${userName} with ${item.claim.with})`;
      }

      // Add an unclaim trigger
      if (item.claim?.by === id) {
        $li.addEventListener("dblclick", () => {
          sendEvent("unclaim", JSON.stringify({ unclaim: item.name }));
        });
      }

      $items.appendChild($li);
    }
    $room.appendChild($items);

    const $spacer = document.createElement("div");
    $spacer.classList.add("spacer");
    $spacer.style.flexGrow = "1";
    $room.appendChild($spacer);

    const $clients = document.createElement("ul");
    $clients.id = "clients-list";
    for (const client of Object.values(room.clients)) {
      const $li = document.createElement("li");
      $li.innerHTML = client.name;
      if (room.leader === client.id) {
        $li.innerHTML += "<span style='font-size:1.1em;'> &star;</span>";
      }
      $clients.appendChild($li);
    }
    $room.appendChild($clients);

    gameWindow.appendChild($room);

    // Move the "reset" button next to the other buttons
    const $reset = document.querySelector(".reset")! as HTMLElement;
    const $sideControls = document.querySelector(".side-controls")! as HTMLElement;
    $reset.style.position = "absolute";
    $reset.style.bottom = `40px`;
    $reset.style.textAlign = "right";
    setInterval(() => {
      $reset.style.width = `${$sideControls.getBoundingClientRect().width}px`;
      $reset.style.left = `${$sideControls.getBoundingClientRect().left}px`;
    }, 100);

    // Add mousemove handlers to items
    const realItems = Array.from(document.querySelectorAll(".item.instance:not(.instance-hide") as NodeListOf<HTMLElement>);
    const targets = Array.from(document.querySelectorAll("#items-list li") as NodeListOf<HTMLElement>);


  }

  const overlap = (e1: HTMLElement, e2: HTMLElement) => {
    const r1 = e1.getBoundingClientRect();
    const r2 = e2.getBoundingClientRect();

    return !(
      r1.right < r2.left ||
      r1.left > r2.right ||
      r1.bottom < r2.top ||
      r1.top > r2.bottom
    );
  }

  const summonJoinForm = () => {
    const $joinForm = document.createElement("form");
    $joinForm.id = "joinForm";
    $joinForm.style.display = "flex";
    $joinForm.style.flexDirection = "column";
    // add 2px gap between elements
    $joinForm.style.gap = "2px";
    $joinForm.innerHTML = `
    <input type="text" id="room-code" placeholder="Room Code" autocomplete="off" />
    <input type="text" id="player-name" placeholder="Player Name" autocomplete="off" />
    <button type="submit">Join</button>
    `;
    gameWindow.appendChild($joinForm);
    $joinForm.style.position = "absolute";
    $joinForm.style.top = `${logo.getBoundingClientRect().bottom + 8}px`;
    $joinForm.style.left = "0.5em";

    $joinForm.onsubmit = (e) => {
      e.preventDefault();
      const roomInput = document.querySelector("#room-code") as HTMLInputElement;
      const room = roomInput.value;
      if (room.length === 0) return;
      const nameInput = document.querySelector("#player-name") as HTMLInputElement;
      const name = nameInput.value;
      if (name.length === 0) return;

      joinRoom(room, name);
    };

    // TESTING
    // joinRoom("UWCS", "Alex");
  }

  const joinRoom = (room, name) => {
    sendEvent("join", JSON.stringify({ room, name }));
    const joinForm = document.querySelector("#joinForm")!;
    joinForm.remove();
  }

  // Add new styles to the page
  const style = document.createElement("style");
  style.textContent = `
  #room {
    display:flex;
    flex-direction:column;

    justify-content:space-between;

    position:absolute;
    left:8px;
    top:50px;
    height:calc(100% - 58px);

    overflow:hidden;

    & h2 {
      font-size: 20px;
      margin: 0;    
    }
  }
  #items-list {
    list-style-type: none;
    padding: 0;
    overflow-y:auto;
    direction:rtl;
    
    display: flex;
    flex-direction: column;
    align-items: end;

    &>*{
      direction:ltr;
    }
    & li {
      padding: 9px 10px 8px;
      border-radius:5px;
      border:1px solid #c8c8c8;
      font-size:16.4px;
      display:block;
      
      &:not(:last-child) {
        margin-bottom: 6px;
      }

      &::before {
        content: "🎯";
        margin-right: 6px;
        display:inline-block;
      }
      &.yes {
        background: linear-gradient(0deg,#d4ffd4,#fff 70%);
        &::before {
          content: "☑️";
        }
      }
      &.no {
        background: linear-gradient(0deg,#ffd4d4,#fff 70%);

        &::before {
          content: "❌";
        }
      }

      background: linear-gradient(0deg,#fffad4,#fff 70%);
    }
  }
  #clients-list {
    list-style-type: none;
    padding: 0;
    &::before {
      content: "Players:";
      margin-bottom: 6px;
      font-weight: bold;
      display: block;
    }
  }

  `;
  document.body.appendChild(style);

  document.addEventListener("mousemove", (e) => {

    overlaps.length = 0;

    // If left mouse button is down
    if (e.buttons === 1) {
      const $room = document.querySelector("#room") as HTMLElement;

      // First, get a list of all items that overlap the room
      const items = Array.from(document.querySelectorAll(".item.instance:not(.instance-hide)") as NodeListOf<HTMLElement>).filter(item => overlap(item, $room));

      const targets = Array.from(document.querySelectorAll("#items-list li") as NodeListOf<HTMLElement>);
      for (const item of items) {
        for (const target of targets) {
          if (overlap(item, target)) {
            if (!overlaps.some(o => o.item === item) && !overlaps.some(o => o.target === target)) {
              overlaps.push({ item, target });
            }

          }
        }
      }

      for (const target of targets) {
        // Change target background color
        target.style.background = "";
      }
      for (const { item, target } of overlaps) {
        // Change target background color
        target.style.background = "linear-gradient(0deg,#d6fcff,#fff 90%)"
      }
    }
  });

  document.addEventListener("mouseup", (e) => {
    // If the overlaps array is not empty, claim the first item with the first target
    while (overlaps.length > 0) {
      const item = overlaps[0].item;
      const target = overlaps[0].target;
      overlaps.shift();
      sendEvent("claim", JSON.stringify({ item: item.textContent, target: target.textContent }));

    }
  })

})();

type Room = {
  clients: Record<string, {
    id: string,
    name: string
  }>,
  connected: string[],
  items: { name: string, claim: { by: string, with: string } | null }[],
  leader: string,
  rid: string,
  started: Date | null,
  status: "Lobby" | "InProgress" | "Finished"
}
