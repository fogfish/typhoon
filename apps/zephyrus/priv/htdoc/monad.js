//
// Copyright (C) 2016 Dmitry Kolesnikov
//
// This software may be modified and distributed under the terms
// of the MIT license.  See the LICENSE file for details.
//
(function(root, factory) {
   if (typeof exports === 'object') {
      module.exports = factory(root);
   } else if (typeof define === 'function' && define.amd) {
      define(factory);
   } else {
      root.curry = factory(root);
   }
}(this, function(root) {
   "use strict";

   //
   //
   Function.prototype.curry = function() 
   {
      var curry = function(fn, args)
      {
         return function()
         {
            var args1 = args.concat(Array.prototype.slice.call(arguments));
            return args1.length >= fn.length ? fn.apply(this, args1) : curry(fn, args1);
         };
      };
      return curry(this, [])
   };
   Function.prototype.$_ = Function.prototype.curry

   //
   // @see 
   // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise
   var monad = {};
   monad.curry = function(fn){return fn.curry()};
   monad.$_  = monad.curry;
   monad.do  = function(list)
   {
      var head = list.shift()
      return list.reduce(function(acc, x){return acc.bind(x)}, head)
   }

   //
   // 
   monad.IO = function(value)
   {
      if (!!(value && value.constructor && value.call && value.apply)) 
      {
         return new Promise(
            function(accept, reject)
            {
               value(function(x){return accept(x);}, function(x){return reject(x)})
            }
         )
      } else {
         return Promise.resolve(value)
      }
   };
   Promise.prototype.bind = Promise.prototype.then
   Promise.prototype.fail = Promise.prototype.catch

   //
   //
   monad.UI = function(value)
   {
      return new UI(value)
   }

   var UI = function(value)
   {
      this.plist = []
      this.flist = []
      var plist  = this.plist
      var flist  = this.flist

      // acceptor function is used by function/value to trigger action chain
      // promise is used to bind computation to IO monad in lazy manner
      var accept = function(x) {
         var head = Promise.resolve(x)
         var tail = plist.reduce(function(acc, x){return acc.bind(x)}, head)
         if (flist.length > 0)
            tail.fail(flist[0])
      }
      value(accept)
   }

   UI.prototype.bind = function(fun)
   {
      this.plist.push(fun)
      return this
   }

   UI.prototype.fail = function(fun)
   {
      this.flist.push(fun)
      return this
   }

   root.monad = monad
}));
