const { parallel, dest, series, watch, task, src } = require('gulp');
const sass = require('gulp-sass')(require('sass'));
const elm = require('gulp-elm'),
      webserver = require('gulp-webserver');

function buildStyles() {
    return src('./src/scss/*.sass', {base: "src/scss/"})
        .pipe(sass({outputStyle: 'compressed', includePaths: "node_modules"}).on('error', sass.logError))
    .pipe(dest('docs/css/'));
};

exports.buildStyles = buildStyles;

function runElm() {
    return src('src/*.elm')
        .pipe(elm({ debug: true }))
        .pipe(dest('docs/js/'));
};


function runWebserver() {
    src('docs')
        .pipe(webserver({}));
}

function watchCss() {
    watch('src/scss/*.sass', buildStyles);
}

function watchElm() {
    watch('src/**/*.elm', runElm);
}

exports.css = buildStyles;
exports.elm = runElm;
exports.default = series(runElm, parallel(runWebserver, watchElm, watchCss));
