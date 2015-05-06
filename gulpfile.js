"use strict";

var gulp = require("gulp"),
    plumber = require("gulp-plumber"),
    purescript = require("gulp-purescript"),
    jsvalidate = require("gulp-jsvalidate");

gulp.task("make", function() {
  return gulp.src(["src/**/*.purs", "bower_components/purescript-*/src/**/*.purs"])
    .pipe(plumber())
    .pipe(purescript.pscMake());
});

gulp.task("make-test", function() {
  return gulp.src(["src/**/*.purs", "test/**/*.purs", "bower_components/purescript-*/src/**/*.purs"])
    .pipe(plumber())
    .pipe(purescript.psc({ main: "Test.Main", output: "test.js" }))
    .pipe(gulp.dest("tmp/"));
});

gulp.task("jsvalidate", ["make"], function () {
  return gulp.src("output/**/*.js")
    .pipe(plumber())
    .pipe(jsvalidate());
});

var docTasks = [];

var docTask = function(name) {
  var taskName = "docs-" + name.toLowerCase();
  gulp.task(taskName, function () {
    return gulp.src("src/" + name.replace(/\./g, "/") + ".purs")
      .pipe(plumber())
      .pipe(purescript.pscDocs())
      .pipe(gulp.dest("docs/" + name + ".md"));
  });
  docTasks.push(taskName);
};

["Database.Mongo.Mongo", "Database.Mongo.Options", "Database.Mongo.Results",
 "Database.Mongo.ConnectionInfo", "Database.Mongo.Bson.BsonValue"].forEach(docTask);

gulp.task("docs", docTasks);

gulp.task("default", ["jsvalidate", "docs", "make-test"]);