var db;
function openDB(callback){
    var idb = window.indexedDB;
    var req = idb.open("ModelDB", 1);
    req.onsuccess = function(ev){
        db = this.result;
        callback(db);
    }
    req.onerror = function(ev){
        console.error('idb err : ' + ev.target.errorCode);
    }
    req.onupgradeneeded = function(ev){
        ev.currentTarget.result.createObjectStore('model', {keyPath:'id'});
    }
}

var getStore = function(callback){
    if(!db){
        openDB(function(db){
            getObjectStore(db, callback);
        });
        return;
    }
    getObjectStore(db, callback);
}

var getObjectStore = function(db, callback){
    var tx = db.transaction('model', 'readwrite');
    callback(tx.objectStore('model'));
}

module.exports = {
    put: function(model){
        getStore(function(store){
            model.id = 1;
            store.put(model);
        });
    },
    get: function(callback, errCallback){
        getStore(function(store){
            var m = store.get(1)
            m.onsuccess = function(ev){
                callback(ev.target.result);
            };
            m.onerror = function(ev){
                console.error('error get value', ev);
                if(errCallback){
                    errCallback(ev);
                }
            };

        })
    }
}
