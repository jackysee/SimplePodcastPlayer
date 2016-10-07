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
app.ports.play.subscribe(function(url){
    if(sound){
        sound.unload();
    }
    sound = new Howl({
        src:[url],
        html5: true,
        onplay: function() {
            requestAnimationFrame(updateProgress);
        }
    });
    console.log("play file", url, sound);
    sound.play();
});


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
    }
});

app.ports.pause.subscribe(function() {
    if(sound){
        sound.pause();
    }
});

app.ports.storeModel.subscribe(function(storeModel){
    console.log(storeModel);
    localStorage.setItem("model", JSON.stringify(storeModel));
});



// http://www.memehk.com/podcast.php?id=8
// http://feeds.soundcloud.com/users/soundcloud:users:62921190/sounds.rss

// app.ports.loadFeed.subscribe(function(url){
//   load(url);
// });
//
