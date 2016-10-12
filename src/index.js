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
        }
    });
    if(playLoad.seek !== -1){
        sound.seek(playLoad.seek);
    }
    console.log("play file", playLoad.url, sound);
    sound.play();
}

app.ports.play.subscribe(playUrl);

// app.ports.resume.subscribe(function(url){
//     if(sound){
//         sound.play();
//     }
//     else {
//         playUrl(url)
//     }
// });
//

function updateProgress(){
    app.ports.updateProgress.send({
        current: sound.seek(),
        duration: sound.duration()
    });

    if(sound.playing()){
        setTimeout(updateProgress, 1000);
    }
}


app.ports.stop.subscribe(function() {
    if(sound){
        sound.stop();
        app.ports.updateProgress.send({
            current: sound.seek(),
            duration: sound.duration()
        });
    }
});

app.ports.pause.subscribe(function() {
    if(sound){
        sound.pause();
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


// http://www.memehk.com/podcast.php?id=8
// http://feeds.soundcloud.com/users/soundcloud:users:62921190/sounds.rss

// app.ports.loadFeed.subscribe(function(url){
//   load(url);
// });
//
