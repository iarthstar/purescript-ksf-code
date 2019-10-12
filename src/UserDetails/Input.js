"use strict";

exports.updateDom = function() {
    if (typeof componentHandler != "undefined" ) {
        componentHandler.upgradeDom();
    };
    return {};
}