import 'deps/phoenix_html/web/static/js/phoenix_html'
import Howler from 'howler';
import R from 'ramda';
//import socket from './socket';

let elmApp = Elm.fullscreen(Elm.App, {rawWebEvent: {text: '', eventType: 'neutral'}});
elmApp.ports.sounds.subscribe(sound => {
  (new Howl({urls: [`/sounds/${sound}`]})).play()
});

window.sendElm = elmApp.ports.rawWebEvent.send
