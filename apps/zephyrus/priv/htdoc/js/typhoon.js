var IO = (function()
{
   var self = {}

   self.json = function(path, accept, reject)
   {
      d3.json(path, 
         function(error, json)
         {
            if (error != null)
            {
               reject({code: error.status, text: error.statusText})
            } else {
               accept(json)
            }
         }
      )
   }.$_()

   return self
})()


var UI = (function()
{
   var self = {}

   self.log  = function(x)
   {
      console.log(x)
      return x
   }.$_()

   self.fail = function(error)
   {
      console.error(error)
   }.$_()

   return self
})()