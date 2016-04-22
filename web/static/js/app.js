import 'deps/phoenix_html/web/static/js/phoenix_html'
import Howler from 'howler';
import R from 'ramda';
import socket from './socket';

let elmApp = Elm.fullscreen(Elm.App, {rawWebEvent: {text: '', eventType: 'neutral'}});
window.sendElm = elmApp.ports.rawWebEvent.send;

elmApp.ports.sounds.subscribe(sound => {
  (new Howl({urls: [`/sounds/${sound}`]})).play()
});

socket.connect();
let channel = socket.channel('web_events:lobby', {});
channel.join()
  .receive("ok", resp => { console.log("Joined successfully", resp) })
  .receive("error", resp => { console.log("Unable to join", resp) })

channel.on("ping", x => console.log(x))
channel.on("web_event", window.sendElm)
