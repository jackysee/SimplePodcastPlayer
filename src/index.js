require('./main.css');

require('howler'); //Howl
var Elm = require('./Main.elm');

var root  = document.getElementById('root');

// var app = Elm.Main.embed(root, 'https://feeds.feedburner.com/JackysBlog');
var app = Elm.Main.embed(root, 'http://podcast.talkonly.net/feed');


var current;
app.ports.play.subscribe(function(url){
    if(current){
        current.unload();
    }
    console.log("play file", url);
    current = new Howl({
        src:[url],
        html5: true
    });
    current.play();
});


app.ports.stop.subscribe(function() {
    if(current){
        current.stop();
    }
})




// http://www.memehk.com/podcast.php?id=8
// http://feeds.soundcloud.com/users/soundcloud:users:62921190/sounds.rss

// app.ports.loadFeed.subscribe(function(url){
//   load(url);
// });
//
