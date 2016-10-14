require('./main.css');

require('howler'); //Howl
var Elm = require('./Main.elm');

var root  = document.getElementById('root');

// var app = Elm.Main.embed(root, 'https://feeds.feedburner.com/JackysBlog');
var app = Elm.Main.embed(root,
    JSON.parse(localStorage.getItem("model"))
    // 'http://podcast.talkonly.net/feed'
);


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
        },
        onend: function(){
            console.log( 'end, state = ', sound ? sound.state() : 'no sound' );
            if(sound && sound.state() == "loading"){
                console.log( 'end called when loading' );
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
    if(playLoad.seek !== -1){
        sound.seek(playLoad.seek);
    }
    if(playLoad.rate){
        sound.rate(playLoad.rate);
    }
    console.log("play file", playLoad.url, sound);
    sound.play();
}

app.ports.play.subscribe(playUrl);

var updateProgressTimer;
function updateProgress(){
    if(!sound){
        return;
    }
    app.ports.updateProgress.send({
        current: sound.seek(),
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
            current: sound.seek(),
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
    localStorage.setItem("model", JSON.stringify(storeModel));
});


app.ports.seek.subscribe(function(value){
    if(sound){
        sound.seek(value);
        app.ports.updateProgress.send({
            current: sound.seek(),
            duration: sound.duration()
        });
    }
});

app.ports.setRate.subscribe(function(rate) {
    if(sound){
        sound.rate(rate);
    }
})


// http://www.memehk.com/podcast.php?id=8
// http://feeds.soundcloud.com/users/soundcloud:users:62921190/sounds.rss

// app.ports.loadFeed.subscribe(function(url){
//   load(url);
// });
//
