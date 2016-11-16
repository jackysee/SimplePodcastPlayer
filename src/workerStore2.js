
var Dexie = require('dexie');
var db = new Dexie("Model");

db.version(1).stores({
    view: '++',
    setting:'++',
    feeds: '&url',
    items: '&url,feedUrl'
});

db.open();

onmessage = function(event){
    if(event.data.type === 'get'){
        var setting = db.setting.toCollection().first();
        var view = db.view.toCollection().first();
        var items = db.items.toArray();
        var feeds = db.feeds.toArray();

        Dexie.Promise.all([setting, view, items, feeds]).then(function(values){
            console.log('getting', values);
            postMessage({
                type: 'get',
                queryId:event.data.queryId,
                model: {
                    setting : values[0],
                    view : values[1],
                    items: values[2],
                    feeds: values[3]
                }
            });
        });
    }
    if(event.data.type === 'set'){
        if(event.data.name === 'setting' || event.data.name === 'view'){
            db[event.data.name].put(event.data.data, 1);
        }
        else{
            db[event.data.name].bulkPut(event.data.data);
        }
    }
    if(event.data.type === 'deleteFeed'){
        var feed = event.data.data;
        db.feeds.delete(feed.url);
        db.items.where('feedUrl').equals(feed.url).delete();
    }

    if(event.data.type == 'destroy'){
        db.delete();
    }

}
