var StoreWorker = require('worker!./workerStore2.js');

var worker = new StoreWorker();
var queryId = 11;
var callbacks = {};

window.worker = worker;

worker.onmessage = function(ev){
    if(ev.data.type === 'get'){
        var callback = callbacks[ev.data.queryId];
        if(callback){
            console.log('callback', ev.data.model)
            callback(ev.data.model)
            delete callbacks[ev.data.queryId];
        }
    }
};

module.exports = {
    set: function(data, name){
        worker.postMessage({type:'set', name:name, data:data})
    },
    deleteFeed: function(feed){
        worker.postMessage({type:'deleteFeed', data:feed});
    },
    get: function(callback){
        queryId = queryId + 1;
        var queryId = ""+queryId;
        worker.postMessage({type:'get', queryId:queryId});
        callbacks[queryId] = callback;
    }
};
