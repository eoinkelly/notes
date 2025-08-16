(function () {
  'use strict';

  function gizmo(id) {
    return {
      id: id,
      toString: function () {
        return 'gizmo' + this.id;
      }
    };
  }

  function hoozit(id) {
    var that = gizmo(id);
    that.test = function (testid) {
      return testid === this.id;
    };
    return that;
  }

  var h = hoozit(23);
})();
