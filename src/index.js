require('./styles/main.scss')

require('howler'); //Howl
var Elm = require('./Main.elm');
var keycode = require('keycode');
var scrollIntoViewIfNeeded = require('./scrollIntoViewIfNeeded');
var store = require('./store');

var root  = document.getElementById('root');
store.get(function(_model){
    var app = Elm.Main.embed(root, _model || null);
    var sound;

    function playUrl(playLoad){
        console.log('playLoad', playLoad);
        if(sound){
            sound.unload();
        }
        sound = new Howl({
            src:[playLoad.url],
            html5: true,
            onplay: function() {
                requestAnimationFrame(updateProgress);
            },
            onload: function(){
                app.ports.soundLoaded.send(true);
                if(playLoad.seek !== -1){
                    sound.seek(playLoad.seek);
                }
            },
            onloaderror: function(){
                if(sound){
                    sound.unload();
                    sound = undefined;
                }
                app.ports.playError.send(playLoad.url);
            },
            onend: function(){
                console.log( 'end, state = ', sound ? sound.state() : 'no sound' );
                if(sound && sound.state() == "loading"){
                    return;
                }
                if(sound){
                    console.log('playnext');
                    app.ports.playEnd.send(sound._src);
                    sound.unload();
                    sound = undefined;
                }
            }
        });
        if(playLoad.rate){
            sound.rate(playLoad.rate);
        }
        if(playLoad.vol){
            sound.volume(playLoad.vol);
        }
        if(playLoad.muted){
            sound.mute(true);
        }

        console.log("play file", playLoad.url);
        sound.play();
    }

    app.ports.play.subscribe(playUrl);

    var updateProgressTimer;
    function updateProgress(){
        if(!sound){
            return;
        }
        app.ports.updateProgress.send({
            progress: sound.seek(),
            duration: sound.duration()
        });

        if(sound.playing()){
            updateProgressTimer = setTimeout(updateProgress, 1000);
        }
    }


    app.ports.stop.subscribe(function() {
        if(sound){
            clearTimeout(updateProgressTimer);
            app.ports.updateProgress.send({
                progress: sound.seek(),
                duration: sound.duration()
            });
            sound.unload();
            sound = undefined;
        }
    });

    app.ports.pause.subscribe(function() {
        if(sound){
            sound.pause();
            clearTimeout(updateProgressTimer);
        }
    });

    app.ports.storeModel.subscribe(function(storeModel){
        // localStorage.setItem("model", JSON.stringify(storeModel));
        store.put(storeModel);
    });


    app.ports.seek.subscribe(function(value){
        if(sound){
            sound.seek(value);
            app.ports.updateProgress.send({
                progress: sound.seek(),
                duration: sound.duration()
            });
        }
    });

    app.ports.setRate.subscribe(function(rate) {
        if(sound){
            sound.rate(rate);
        }
    });

    app.ports.openNewLink.subscribe(function(url){
        window.open(url, '_blank');
    });

    app.ports.setVol.subscribe(function(vol) {
        if(sound){
            sound.volume(vol);
        }
    });

    document.onkeyup = function(ev){
        var target = document.activeElement || ev.target;
        if(target){
            var tagName = target.tagName.toLowerCase();
            if(tagName !== "input" && tagName !== "textarea"){
                var key = keycode(ev) || "";
                if(ev.ctrlKey){
                    key = "ctrl-"+key;
                }
                if(ev.shiftKey){
                    key = "shift-"+key;
                }
                app.ports.keyUp.send(key);
            }
        }
    };

    document.body.addEventListener('PlayPause', function(){
        app.ports.keyUp.send('p');
    });

    app.ports.scrollToElement.subscribe(function(id){
        var el = document.getElementById(id);
        if(el){
            scrollIntoViewIfNeeded(el);
        }
    });
});
