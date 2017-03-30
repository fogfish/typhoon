//
// 
var M      = monad
var action = {}
var ui     = {}

//
//
var model = {
   api: 'http://localhost:8080',
   user: 'root'
}


//-----------------------------------------------------------------------------
//
// action IO
//
//-----------------------------------------------------------------------------
action.IO = {}

// f: url -> IO json
action.IO.json = function(url, accept, reject)
{
   $.ajax(
      {
         url: url, 
         type: 'get', 
         dataType: 'json'
      }
   )
   .done(function(json){accept(json)})
   .fail(function(xhr){reject(action.IO.fail(xhr))})
}.$_()


action.IO.typhoon = {}
action.IO.typhoon.profile = function(uid)
{
   return action.IO.json([model.api, 'user', uid, 'scenario'].join('/'))
}

//-----------------------------------------------------------------------------
//
// action UI
//
//-----------------------------------------------------------------------------
action.UI = {}

action.UI.click = function(match, accept)
{
   $(document).on('click', match,
      function(e)
      {
         accept($(this).data())
         e.preventDefault()
      }
   )
}.$_()

action.UI.scenario = action.UI.click('.dc-table__tr--interactive')


//-----------------------------------------------------------------------------
//
// UI
//
//-----------------------------------------------------------------------------
ui.fail = function(x){console.error(x)}.$_()

ui.scenario = {}
ui.scenario.thumbnail = whiskers.compile($('#scenario-thumbnail').html())

ui.scenario.list = function(list)
{
   list.forEach(
      function(x)
      {
         $('.js-scenario-thumbnail').prepend( ui.scenario.thumbnail(x) )
      }
   )
   return list
}.$_()

//-----------------------------------------------------------------------------
//
// chain
//
//-----------------------------------------------------------------------------
var chain = {}

chain.request_user_profile = function()
{
   M.do([
      M.IO(action.IO.typhoon.profile(model.user)),
      ui.scenario.list
   ]).fail(ui.fail)
}

chain.show_scenario_details = function()
{
   M.do([
      M.UI(action.UI.scenario),
      function(e)
      {
         console.log(e)
      }
   ]).fail(ui.fail)
}


//
// entry point
$(document).ready(
   function()
   {
      chain.request_user_profile()
      chain.show_scenario_details()
   }
)





