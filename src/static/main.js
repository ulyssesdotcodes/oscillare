var ws = new WebSocket("ws://" + window.location.hostname + (window.location.port ? ":" + window.location.port : ""));

nx.sendsTo(function(data, something, somethingElse){
  console.log(data);
  console.log(this.oscPath);
  ws.send(JSON.stringify({address : this.oscPath, args: Object.keys(data).map(key => data[key])}));
});
