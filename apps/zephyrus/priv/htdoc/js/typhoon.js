var IO = (function()
{
   var self = {}

   self.json = function(url, accept, reject)
   {
      d3.json(url, 
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

   self.post = function(url, type, data, accept, reject)
   {
      d3.xhr(url)
         .header('Content-Type', type)
         .post(data,
            function(error, req)
            {
               if (error != null)
               {
                  reject(JSON.parse(error.response))
               } else {
                  accept(req.response)
               }
            }
         )
   }.$_()

   self.text = function(url, accept, reject)
   {
      d3.xhr(url).get(
         function(error, req)
         {
            if (error != null)
            {
               reject({code: error.status, text: error.statusText})
            } else {
               accept(req.response)
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

   self.action = function(klass, accept)
   {
      d3.select(klass).on('click',
         function()
         {
            accept(d3.event.target)
         }
      )
   }.$_()

   return self
})()