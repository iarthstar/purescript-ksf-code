"use strict";

function doSomethingAsync(config, cb) {

    // var data = config.data;
    // if(config.formData == true){
    //     var formData = new FormData();

    //     Object.keys(data).forEach(function(key) {
    //         formData.append(key, data[key]);
    //     });

    //     config.data = formData;
    // } else {
    //     config.data = data;
    // }

    // console.log("BODY", config.body);

    var headers = {};
    config.headers.forEach(function(elem) {
        headers[elem[0]] = elem[1];
    });
    config.headers = headers;

    if(config.method == "GET" || config.method == "DELETE"){
        config.params = config.data;
        delete config.data;
    }

    // config = {
    //     method: "POST",
    //     "headers": {
    //         "Content-Type": "application/json",
    //         "User-Agent": "PostmanRuntime/7.17.1",
    //         "Accept": "*/*",
    //         "Cache-Control": "no-cache",
    //         "Postman-Token": "4ef4b912-5140-485b-a5af-f02a2ace0c55,01ffbc39-3ea7-4abe-bce7-7a133b2e7953",
    //         "Host": "persona.api.ksfmedia.fi",
    //         "Accept-Encoding": "gzip, deflate",
    //         "Content-Length": "63",
    //         "Connection": "keep-alive",
    //         "cache-control": "no-cache",
    //         "origin": "http://localhost:1234",
    //         "Access-Control-Allow-Origin": "*"
    //     },
    //     body: JSON.stringify({
    //         username: "iarthstar@gmail.com",
    //         password: "12345678"
    //     })
    // }

    // fetch(url, config)
    //     .then(function (res) { return res.json() })
    //     .then(function (data) {
    //         console.log("SUCCESS ----->", JSON.stringify(data));
    //         cb(false, data);
    //     }).catch(function (err) {
    //         console.log("ERROR ----->", err);
    //         cb(true, err);
    //     });
    console.log("CONFIG --->", config);
    axios(config).then(function(res) {
        console.log("SUCCESS ----->", JSON.stringify(res.data));
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