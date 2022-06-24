const { parallel, dest, series, watch, task, src } = require('gulp');
const sass = require('gulp-sass')(require('sass'));
const elm = require('gulp-elm'),
      webserver = require('gulp-webserver');

function copyJs() {
    return src('./src/js/**/*.js', {base: "src/js/"})
        .pipe(dest('docs/js/'));
}

function copySvg() {
    return src('./src/img/**/*.svg', {base: "src/img/"})
        .pipe(dest('docs/img/'));
}

function buildStyles() {
    return src('./src/scss/*.sass', {base: "src/scss/"})
        .pipe(sass({outputStyle: 'expanded', includePaths: "node_modules"}).on('error', sass.logError))
    .pipe(dest('docs/css/'));
};

exports.buildStyles = buildStyles;

function runElm() {
    return src(['src/Painting.elm', 'src/Projects.elm'])
        .pipe(elm({ debug: true }))
        .pipe(dest('docs/js/'));
};


function runWebserver() {
    src('docs')
        .pipe(webserver({}));
}

function watchSvg() {
    watch('src/img/**/*.svg', copySvg);
}

function watchJs() {
    watch('src/js/**/*.js', copyJs);
}

function watchCss() {
    watch('src/scss/*.sass', buildStyles);
}

function watchElm() {
    watch('src/**/*.elm', runElm);
}

exports.js = copyJs;
exports.css = buildStyles;
exports.elm = runElm;
exports.default = series(runElm, parallel(runWebserver, watchElm, watchCss, watchJs, watchSvg));
