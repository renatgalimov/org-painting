//////////////////////////////////////////////////////////////////////
//
// AddXY.js
// An example PortFunnel port that adds its x & y args and returns the sum as sum.
// It also illustrates asynchronous sends through the Sub port.
// Copyright (c) 2018-2019 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////


(function(scope) {
    var moduleName = 'PictureUrl';
    var sub;

    function init() {
        var PortFunnel = scope.PortFunnel;
        if (!PortFunnel || !PortFunnel.sub || !PortFunnel.modules) {
            // Loop until PortFunnel.js has initialized itself.
            setTimeout(init, 10);
            return;
        }
        sub = PortFunnel.sub;
        PortFunnel.modules[moduleName] = { cmd: dispatcher };
    }
    init();

    function dispatcher(tag, args) {
        var searchParams = new URLSearchParams(window.location.search)
        searchParams.set("todo", args.todo);
        var newRelativePathQuery = window.location.pathname + '?' + searchParams.toString();
        history.pushState(null, '', newRelativePathQuery);

        return { module: moduleName,
                 tag: tag,
                 args: args
               }
    }
})(this);
