var StoreWorker = require('worker!./workerStore.js');

var worker = new StoreWorker();
var queryId = 11;
var callbacks = {};

worker.onmessage = function(ev){
    if(ev.data.type === 'get'){
        var callback = callbacks[ev.data.queryId];
        if(callback){
            callback(ev.data.model)
            delete callbacks[ev.data.queryId];
        }
    }
};

module.exports = {
    put: function(model){
        worker.postMessage({type:'put', model:model})
    },
    get: function(callback){
        queryId = queryId + 1;
        var queryId = ""+queryId;
        worker.postMessage({type:'get', queryId:queryId});
        callbacks[queryId] = callback;
    }
};
