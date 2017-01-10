exports.traceAny = function (a) {
  return function () {
    console.log(a);
    return {};
  };
};

