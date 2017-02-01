var IO = {}

IO.json = function(url, accept, reject)
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

IO.scenario = {}
IO.scenario.get  = function(id, accept, reject)
{
console.log(id)
   d3.xhr('/scenario/' + id)
      .header('Accept', 'application/erlang')
      .get(
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

IO.scenario.lint = function(id, data, accept, reject)
{
   d3.xhr('/lint/' + id)
      .header('Content-Type', 'application/erlang')
      .post(data,
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

IO.scenario.put = function(id, data, accept, reject)
{
   d3.xhr('/scenario/' + id)
      .header('Content-Type', 'application/erlang')
      .send('PUT', data, 
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

IO.scenario.remove = function(id, accept, reject)
{
   d3.xhr('/scenario/' + id)
      .send('DELETE',
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

IO.scenario.metadata = function(id, accept, reject)
{
   d3.json('/scenario/' + id + '/attributes', 
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

IO.scenario.history = function(id, accept, reject)
{
   d3.json('/scenario/' + id + '/history', 
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

IO.scenario.series = function(id, urn, from, to, accept, reject)
{
   d3.json('/scenario/' + id + '/series/' + urn + '/' + from + '/' + to, 
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

IO.scenario.cubism = function(id, urn, from, to, accept, reject)
{
   d3.json('/scenario/' + id + '/cubism/' + urn + '/' + from + '/' + to + '?chronon=1', 
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