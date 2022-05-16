const { parallel, dest, series, watch, task, src } = require('gulp');
var elm = require('gulp-elm'),
    webserver = require('gulp-webserver');

function runElm() {
    return src('src/Projects.elm')
        .pipe(elm({ debug: true }))
        .pipe(dest('docs/js/'));
};


function runWebserver() {
    src('docs')
        .pipe(webserver({}));
}

function watchElm() {
    watch('src/**/*.elm', runElm);
}

exports.elm = runElm;
exports.default = series(runElm, parallel(runWebserver, watchElm));
