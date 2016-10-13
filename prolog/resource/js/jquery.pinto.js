/*!
  Pinto jQuery Plugin
  @name jquery.pinto.js
  @description Lightweight and customizable jQuery plugin for creating pinterest like responsive grid layout
  @author Max Lawrence 
  @version 1.4.0
  @category jQuery plugin
  @copyright (c) 2015 Max Lawrence (http://www.avirtum.com)
  @license Licensed under the MIT (http://www.opensource.org/licenses/mit-license.php) license.
*/
(function($) {
    "use strict";
    
    var ITEM_DATA_NAME = "pinto";
    
    function Pinto(config, el) {
        this.el = el;
        this.init(config);
    };
    
    Pinto.prototype = {
        //=============================================
        // Properties
        //=============================================
        defaults : {
            itemSelector: "> div", // a block identification key
            itemSkipClass: "pinto-skip", // a class of items that will be skipped and not layouted
            itemWidth: 220, // the width of one grid block in pixels
            gapX: 10, // the width spacing between blocks in pixels
            gapY: 10, // the height spacing between blocks in pixels
            align: "left", // a blocks alignment ("left", "right", "center")
            fitWidth: true, // adjust the block width to create optimal layout based on container size
            autoResize: true, // update layout after browser is resized
            resizeDelay: 50, // time in milliseconds between browser resize and layout update
            onItemLayout: function($item, column, position) {}, // fire after item layout complete
        },
        
        config: null,
        resizeEvent: null,
        resizeTimer: null,
        el: null, // a container element (should be passed into constructor)
        
        //=============================================
        // Methods
        //=============================================
        init: function(config) {
            this.destroyResize();
            this.config = config;
            this.build();
            this.layout();
        },
        
        destroyResize: function() {
            if(this.resizeEvent) {
                this.resizeEvent.unbind();
                this.resizeEvent = null;
            }
            clearTimeout(this.resizeTimer);
        },
        
        build: function() {
            if (this.el.length == 0) {
                return;
            }
            
            if (this.el.css("position") != "relative") {
                this.el.css("position", "relative");
            }
            
            if (this.config.autoResize) {
                this.resizeEvent =  $(window).on("resize", $.proxy(this.resize, this));
                this.el.on("remove", this.resizeEvent.unbind);
            }
        },
        
        layout: function () {
            if (this.el.length == 0 || !this.el.is(":visible")) { 
                return;
            }
            
            var self = this,
            items = this.el.find(this.config.itemSelector),
            width = this.el.innerWidth(),
            itemWidth = this.config.itemWidth,
            gapX = parseInt(this.config.gapX || 0),
            gapY = parseInt(this.config.gapY || 0),
            offset = 0,
            colsCount = 0;
            
            while(width > offset) {
                offset += itemWidth;
                if(width >= offset) {
                    colsCount++;
                } else {
                    break;
                }
                offset += gapX;
            };
            colsCount = Math.max(colsCount, 1);
            
            var cols = [], 
            colsH = [],
            i = colsCount;
            while(i--) { 
                cols.push(0);
                colsH.push(0);
            }
            
            offset = 0;
            var gap = (colsCount-1) * gapX;
            if (this.config.fitWidth) {
                itemWidth += Math.floor(0.5 + (width - gap - colsCount * itemWidth) / colsCount);
            } else {
                // calculate the offset based on the alignment of columns to the parent container
                if (this.config.align === "center") {
                    offset += Math.floor(0.5 + (width - gap - colsCount * itemWidth) >> 1);
                } else if (this.config.align === "right") {
                    offset += Math.floor(0.5 + (width - gap - colsCount * itemWidth));
                };
            };
            
            items.each(function(index, item) {
                var $item = $(item),
                i = self.getSmallestIndex(colsH);
                
                if (!$item.is(":visible") || $item.hasClass(self.itemSkipClass)) {
                    return;
                }
                
                $item.css({
                    position: "absolute",
                    top: colsH[i] + "px",
                    left: (itemWidth + gapX) * i + offset + "px",
                    width: itemWidth
                });
                
                colsH[i] += $item.outerHeight() + gapY;
                
                 if (typeof self.config.onItemLayout == "function") { // make sure the callback is a function
                    self.config.onItemLayout.call(self, $item, i, cols[i]); // brings the scope to the callback
                }
                
                cols[i]++;
            });
            
            var height=0;
            i = colsCount;
            while(i--) if(colsH[i]>height) height = colsH[i];
            this.el.css({height:height});
        },
        
        resize: function() {
            clearTimeout(this.resizeTimer);
            this.resizeTimer = setTimeout($.proxy(this.layout, this), this.config.resizeDelay);
        },
         
        destroy: function () {
            this.destroyResize();
            this.el.removeData(ITEM_DATA_NAME);
            
            // remove dynamic styles
            var items = this.el.find(this.config.itemSelector);
            items.each(function() {
                var $item = $(this);
                $item.css({
                    position: "",
                    top: "",
                    left: "",
                    width: "",
                });
            });
            
            this.el.css({
                position: "",
                height: ""
            });
        },
        
        getSmallestIndex: function (a) {
            var index = 0;
            for (var i = 1, len = a.length; i < len; i++) {
                if (a[i] < a[index]) index = i;
            }
            return index;
        },
    }
    
    //=============================================
    // Init jQuery Plugin
    //=============================================
    /**
     * @param CfgOrCmd - config object or command name
     *     you may set any public property (see above);
     *     you may use .pinto("layout") to call the layout function
     * @param CmdArgs - some commands may require an argument
     */
    $.fn.pinto = function(CfgOrCmd, CmdArgs) {
        var instance = this.data(ITEM_DATA_NAME);
        
        if (CfgOrCmd == "layout") {
            if (!instance) {
                throw Error("Calling 'layout' method on not initialized instance is forbidden");
            }
            
            return instance.layout();
        }
        
        if (CfgOrCmd == "destroy") {
            if (!instance) {
                throw Error("Calling 'destroy' method on not initialized instance is forbidden");
            }
            
            return instance.destroy();
        }
        
        return this.each(function() {
            var el = $(this),
            instance = el.data(ITEM_DATA_NAME),
            options = $.isPlainObject(CfgOrCmd) ? CfgOrCmd : {};

            if (instance) {
                var config = $.extend({}, instance.config, options);
                instance.init(config);
            } else {
                var config = $.extend({}, Pinto.prototype.defaults, options);
                instance = new Pinto(config, el);
                el.data(ITEM_DATA_NAME, instance);
            }
        });
    }
})(window.jQuery);