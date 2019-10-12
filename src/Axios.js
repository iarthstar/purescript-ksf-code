"use strict";

function doSomethingAsync(config, cb) {

    var headers = {};
    config.headers.forEach(function(elem) {
        headers[elem[0]] = elem[1];
    });
    config.headers = headers;

    if(config.method == "GET" || config.method == "DELETE"){
        config.params = config.data;
        delete config.data;
    }

    axios(config).then(function(res) {
        cb(false, res.data);
    }).catch(function(err) {
        cb(true, err);
    });
}

exports._axios = function (config) {
    return function (onError, onSuccess) {
        var cancel = doSomethingAsync(config, function (err, res) {
            if (err) {
                onError(res);
            } else {
                onSuccess(res);
            }
        });
        return function (cancelError, onCancelerError, onCancelerSuccess) {
            cancel();
            onCancelerSuccess();
        }
    }
}