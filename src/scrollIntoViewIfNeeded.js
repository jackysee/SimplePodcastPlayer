var scrollparent = require('scrollparent');

module.exports = function scrollIntoViewIfNeeded(el, centerIfNeeded){
    if(Element.prototype.scrollIntoViewIfNeeded){
        el.scrollIntoViewIfNeeded(centerIfNeeded);
    }
    
    if(centerIfNeeded !== undefined){
        centerIfNeeded = true;
    }
    var parent = scrollparent(el),
        parentComputedStyle = window.getComputedStyle(parent, null),
        parentBorderTopWidth = parseInt(parentComputedStyle.getPropertyValue('border-top-width')),
        parentBorderLeftWidth = parseInt(parentComputedStyle.getPropertyValue('border-left-width')),
        overTop = el.offsetTop - parent.offsetTop < parent.scrollTop,
        overBottom = (el.offsetTop - parent.offsetTop + el.clientHeight - parentBorderTopWidth) > (parent.scrollTop + parent.clientHeight),
        overLeft = el.offsetLeft - parent.offsetLeft < parent.scrollLeft,
        overRight = (el.offsetLeft - parent.offsetLeft + el.clientWidth - parentBorderLeftWidth) > (parent.scrollLeft + parent.clientWidth),
        alignWithTop = overTop && !overBottom;

    if ((overTop || overBottom) && centerIfNeeded) {
      parent.scrollTop = el.offsetTop - parent.offsetTop - parent.clientHeight / 2 - parentBorderTopWidth + el.clientHeight / 2;
    }

    if ((overLeft || overRight) && centerIfNeeded) {
      parent.scrollLeft = el.offsetLeft - parent.offsetLeft - parent.clientWidth / 2 - parentBorderLeftWidth + el.clientWidth / 2;
    }

    if ((overTop || overBottom || overLeft || overRight) && !centerIfNeeded) {
      el.scrollIntoView(alignWithTop);
    }
    
}

