const worker = new Worker("dist/worker.js");

const logOutput = document.getElementById("log-output") as HTMLDivElement;
worker.onmessage = (event: MessageEvent) => {
  if(event.data.kind == "RunningInfo") {
    const element = document.createElement("div");
    element.classList.add("log-debug");
    element.innerText = event.data.message;
    logOutput.appendChild(element);
  } else {
    const element = document.createElement("div");
    element.classList.add("log-unknown");
    element.innerText = JSON.stringify(event.data);
    logOutput.appendChild(element);
  }
};

