"use strict";

// module Episodes.Common


exports.ajax = function(settings) {
    return function() {
        return jQuery.ajax(settings);
    };
};

exports.first = function(jq) {
    return function() {
        return jq.first();
    }
};

exports.getAttr = function(name) {
    return function(jq) {
        return function() {
            return jq.attr(name);
        }
    }
};

exports.getJQueryEventPageX = function(e) {
    return function() {
        return e.pageX;
    };
};

exports.getJQueryEventPageY = function(e) {
    return function() {
        return e.pageY;
    };
};

exports.getValueText = function(ob) {
    return function() {
        return (ob.val() || '');
    };
};

exports.is = function(sel) {
    return function(ob) {
        return function() {
            return ob.is(sel);
        };
    };
};

exports.jqXhrDone = function(req) {
    return function(act) {
        return function() {
            return req.done(function(data, status, req) {
                act(data)(status)(req)();
            });
        };
    };
};

exports.jqXhrFail = function(req) {
    return function(act) {
        return function() {
            return req.fail(function(req, status, err) {
                act(req)(status)(err)();
            });
        };
    };
};

exports.jsonStringify = function(v) {
    return function() {
        return JSON.stringify(v, null, "    ");
    };
};

exports.jQueryFadeIn = function(duration) {
    return function (ob) {
        return function() {
            return ob.fadeIn(duration);
        }
    };
};

exports.jQueryFadeOut = function(duration) {
    return function (ob) {
        return function() {
            return ob.fadeOut(duration);
        }
    };
};

exports.jQueryFadeOut$prime = function(duration) {
    return function(act) {
        return function (ob) {
            return function() {
                return ob.fadeOut(duration, act);
            }
        };
    }
};

exports.jQueryFadeOut$prime = function(duration) {
    return function(act) {
        return function(ob) {
            return function() {
                return ob.fadeOut(duration, act);
            };
        };
    };
};

exports.last = function(jq) {
    return function() {
        return jq.last()
    };
};

exports.nextUntil = function(sel) {
    return function(jq) {
        return function() {
            return jq.nextUntil(sel);
        };
    };
};

exports.on = function(evt) {
    return function(act) {
        return function(ob) {
            return function() {
                return ob.on(evt, function(e) {
                    act(e)(jQuery(this))();
                });
            };
        };
    };
};

exports.prevAll = function(sel) {
    return function(jq) {
        return function() {
            return jq.prevAll(sel);
        };
    };
};

exports.redirect = function(h) {
    return function() {
        window.location.href = h;
    };
};

exports.size = function(jq) {
    return jq.size();
};

exports.split = function(r) {
    return function(s) {
        return s.split(r);
    };
};
