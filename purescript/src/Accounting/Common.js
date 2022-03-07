
exports.alert = function (msg) {
    return function () {
        alert(msg);
    }
}