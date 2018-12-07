var storageKey = 'store';
var flags = localStorage.getItem(storageKey);
var app = Elm.Main.init({flags: flags});

app.ports.storeCache.subscribe(function(val) {
 if (val === null) {
  localStorage.removeItem(storageKey);
 } else {
  localStorage.setItem(storageKey, JSON.stringify(val));
 }
 setTimeout(function() { app.ports.onStoreChange.send(val); }, 0);
});

window.addEventListener('storage', function(event) {
 if (event.storageArea === localStorage && event.key === storageKey) {
  app.ports.onStoreChange.send(event.newValue);
 }
}, false);

