function dummy() {
  return Promise.resolve();
}

module.exports = {
  paths: {
    "elm-format": require("./binary.js")(),
  },
  install: dummy,
  prepare: dummy,
  test: dummy,
};
