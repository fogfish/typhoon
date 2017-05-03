//
// 
var M       = monad
var action  = {}
var ui      = {}
var present = {} 

var width   = 960;
var height  = 30;

//
//
var model = {
   api: 'http://localhost:8080',
   user: 'root',

   // profile
   profile: [],

   // scenario specification
   scenario: null,

   // analytics
   analytics: null,   

   // text editor (ace)
   editor: null,
   
   // cubism context
   cubism: null
}

String.prototype.hashCode = function() {
  var hash = 0, i, chr;
  if (this.length === 0) return hash;
  for (i = 0; i < this.length; i++) {
    chr   = this.charCodeAt(i);
    hash  = ((hash << 5) - hash) + chr;
    hash |= 0; // Convert to 32bit integer
  }
  return hash;
}

//-----------------------------------------------------------------------------
//
// present
//
//-----------------------------------------------------------------------------
present.scenario = {}
present.scenario.new   = whiskers.compile($('#scenario-new-file').html())

// create empty scenario from template
present.scenario.empty = function(_)
{
   var scenario =  {
      id: 'x' + Math.random().toString(36).substring(2),
      title: 'No title',
      spec: null,
      urls: [],
      hosts: [],
      //
      _new: true
   }
   scenario.spec  = present.scenario.new(scenario)
   model.scenario = scenario
   return scenario
}.$_()

// set a new specification for scenario
present.scenario.spec = function(spec)
{
   var id = spec.match(/-module\((.*)\)\./i)[1]
   if (!id) 
   {
      var err = 'module id is not defined.\nset a id of your scenario using following directive: -module(test).'
      model.editor.getSession().setAnnotations([
         {row: 1, column: 0, text: err, type: 'error'}
      ])
      throw err
   }
   if (!/^[a-z][a-z0-9]*/.test(id))
   {
      var err = 'module id is not invalid.\nthe module id should contain only lowercase letters and digits.'
      model.editor.getSession().setAnnotations([
         {row: 1, column: 0, text: err, type: 'error'}
      ])
      throw err
   }

   if ((model.scenario.id != null) && (model.scenario.id != id))
   {
      var err = 'scenario id is not changeable'
      model.editor.getSession().setAnnotations([
         {row: 1, column: 0, text: err, type: 'error'}
      ])
      throw err
   }
   model.scenario.id   = id
   model.scenario.spec = spec
   return model.scenario
}.$_()

// build a time series streams for url
// identity of time series streams is aligned with aura application
present.scenario.url2ts = function(sid, url)
{
   return {
      id: url,
      ts: [
         {id: sid, urn: "urn:c:2xx+" + url, title: '2xx'}   // HTTP 2xx per second
        ,{id: sid, urn: "urn:c:3xx+" + url, title: '3xx'}   // HTTP 3xx per second
        ,{id: sid, urn: "urn:c:4xx+" + url, title: '4xx'}   // HTTP 4xx per second
        ,{id: sid, urn: "urn:c:5xx+" + url, title: '5xx'}   // HTTP 5xx per second
        ,{id: sid, urn: "urn:g:ttfb+" + url, title: 'ttfb (μs)'}  // Time To First Byte
        ,{id: sid, urn: "urn:g:ttmr+" + url, title: 'ttmr (μs)'}  // Time To Meaningful Response
      ]
   }
}.$_()

present.scenario.host2ts = function(sid, host)
{
   return {
      id: host,
      ts: [
         {id: sid, urn: "urn:g:connect+" + host, title: 'tcp (μs)'}
        ,{id: sid, urn: "urn:g:handshake+" + host, title: 'ssl (μs)'}
        ,{id: sid, urn: "urn:c:packet+" + host, title: 'packet / sec'}
        ,{id: sid, urn: "urn:g:packet+" + host, title: 'packet (byte)'}
      ]
   }
}.$_()

present.scenario.user2ts = function(sid)
{
   return {
      id: sid,
      ts: [
         {id: sid, urn: "urn:c:scenario:" + sid, title: 'sessions / sec'}
        ,{id: sid, urn: "urn:g:scenario:" + sid, title: 'latency (μs)'}
      ]
   }
}.$_()

// builds cdf
present.cdf = function(title, series)
{
   var x = series.map(function(x){return x[1]}).sort(d3.ascending)
   return {
      title: title,
      min: d3.min(x),
      mean: d3.mean(x),
      std: d3.deviation(x),
      q25: d3.quantile(x, 0.25),
      q50: d3.quantile(x, 0.50),
      q75: d3.quantile(x, 0.75),
      q80: d3.quantile(x, 0.80),
      q90: d3.quantile(x, 0.90),
      q95: d3.quantile(x, 0.95),
      q99: d3.quantile(x, 0.99),
      max: d3.max(x)
   }
}


//-----------------------------------------------------------------------------
//
// action IO
//
//-----------------------------------------------------------------------------
action.IO = {}

action.IO.fail = function(xhr)
{
   return {code: xhr.status, text: xhr.statusText, error: xhr.responseJSON}
}

action.IO.after = function(t, json, accept, reject)
{
   setTimeout(function(){ accept(json) }, t)
}.$_()

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

// cubism data series
action.IO.series = function(sid, urn, title) 
{
   return model.context.metric(function(start, stop, step, callback) {
      d3.json(model.api + "/scenario/" + sid + "/cubism/" + encodeURIComponent(urn) 
         + "/" + start.getTime() / 1000
         + "/" + stop.getTime() / 1000 
         + "?chronon=" + step / 1000, 
         function(data) 
         {
            if (!data) return callback(new Error("unable to load data"));
            callback(null, data.map(function(x){return x[1]}))
         });
      }, title);
}

action.IO.typhoon = {}
action.IO.typhoon.profile = function(uid)
{
   return action.IO.json([model.api, 'user', uid, 'scenario'].join('/'))
}

action.IO.typhoon.get = function(sid)
{
   return action.IO.json([model.api, 'scenario', sid].join('/'))
}

action.IO.typhoon.lint = function(scenario, accept, reject)
{
   $.ajax(
      {
         url: [model.api, 'lint', scenario.id].join('/'),
         type: 'post',
         dataType: 'text',
         data: scenario.spec,
         headers: {'Content-Type': 'application/erlang'}
      }
   )
   .done(function(json){accept(json)})
   .fail(
      function(xhr)
      {
         var json = JSON.parse(xhr.responseText)
         var reason = Object.keys(json)         
         reject({code: xhr.status, text: xhr.statusText, lint: json[reason]})
      })
}.$_()

action.IO.typhoon.put = function(scenario, accept, reject)
{
   $.ajax(
      {
         url: [model.api, 'scenario', scenario.id].join('/'),
         type: 'put',
         dataType: 'json',
         data: scenario.spec,
         headers: {'Content-Type': 'application/erlang'}
      }
   )
   .done(function(json){accept(json)})
   .fail(function(xhr){reject(action.IO.fail(xhr))})
}.$_()

action.IO.typhoon.remove = function(scenario, accept, reject)
{
   $.ajax(
      {
         url: [model.api, 'scenario', scenario.id].join('/'),
         type: 'delete',
         dataType: 'json'
      }
   )
   .done(function(json){accept(json)})
   .fail(function(xhr){reject(action.IO.fail(xhr))})
}.$_()

action.IO.typhoon.spawn = function(sid)
{
   return action.IO.json([model.api, 'scenario', sid, 'spawn'].join('/'))
}
action.IO.typhoon.abort = function(sid)
{
   return action.IO.json([model.api, 'scenario', sid, 'abort'].join('/'))
}

action.IO.typhoon.history = function(sid)
{
   return action.IO.json([model.api, 'scenario', sid, 'history'].join('/'))
}

action.IO.typhoon.attr = function(sid)
{
   return action.IO.json([model.api, 'scenario', sid, 'attributes'].join('/'))
}

action.IO.typhoon.series = function(sid, urn, from, to)
{
   return action.IO.json([model.api, 'scenario', sid, 'series', encodeURIComponent(urn), from, to].join('/'))
}

action.IO.system = function()
{
   return action.IO.json([model.api, 'health', 'sys'].join('/'))
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

action.UI.scenario = {}
action.UI.scenario.show = action.UI.click('.js-action-scenario')
action.UI.scenario.init = action.UI.click('.js-action-scenario-new')
action.UI.scenario.spec = function(accept)
{
   $('.js-scenario-spec').change(
      function(e)
      {
         if (!this.checked) 
         {
            accept(model.editor.getValue())
            $('.js-scenario-spec').prop('checked', true)
            e.preventDefault()
         }
      }
   )
}.$_()

action.UI.scenario.hide  = action.UI.click('.js-action-scenario-hide')
action.UI.scenario.spawn = action.UI.click('.js-action-scenario-spawn')
action.UI.scenario.abort = action.UI.click('.js-action-scenario-abort')
action.UI.scenario.remove = action.UI.click('.js-action-scenario-remove')
action.UI.scenario.shortcut = function(accept)
{
   $('.js-scenario-thumbnail').delegate('label', 'click',
      function(e) {
        e.stopPropagation();
        accept($(this).data())    
      }
   )  
}.$_()

//
action.UI.history = {}
action.UI.history.show = action.UI.click('.js-action-history')

//-----------------------------------------------------------------------------
//
// UI
//
//-----------------------------------------------------------------------------
ui.fail = function(x)
{
   $('.dc-loading-bar').hide()
   console.error(x)
   chain.show_error(5000, x)
}.$_()

ui.debug = function(x)
{
   console.log(x)
   return x
}.$_()

ui.progressbar = function(show, x)
{
   if (show)
      $('.dc-loading-bar').show()
   else
      $('.dc-loading-bar').hide()
   return x
}.$_()


ui.scenario = {}
ui.scenario.thumbnail = whiskers.compile($('#scenario-thumbnail').html())
ui.scenario.cubism = whiskers.compile($('#scenario-cubism').html())

ui.scenario.append = function(scenario)
{
   if ($('.dc-table__tr[data-scenario="' + scenario.id + '"]').length == 0)
   {
      $('.js-scenario-thumbnail').prepend( ui.scenario.thumbnail(scenario) )
   }
   return scenario
}.$_()

ui.scenario.list = function(list)
{
   list.forEach(ui.scenario.append)
   return list
}.$_()

ui.scenario.show = function(scenario)
{
   model.scenario = scenario

   if ('spec' in scenario)
      model.editor.setValue(scenario.spec)
   else 
      model.editor.setValue('')
   model.editor.session.selection.clearSelection()
   model.editor.scrollToLine(1, true, true, function(){})
   model.editor.gotoLine(1)

   $('.js-scenario-title').text(scenario.title)
   if (scenario._new)
      $('.js-scenario-spec').prop('checked', true)

   $('.js-scenario-cubism-user').text('No Content')
   $('.js-scenario-cubism-url').text('No Content')
   $('.js-scenario-cubism-host').text('No Content')

   $('.js-scenario').show()
   $('html, body').animate({scrollTop: $(".dc-card__scenario").offset().top}, 1000);
   return scenario
}.$_()

ui.scenario.hide = function(_)
{
   $('.js-scenario').hide()
}.$_()

ui.scenario.action = function(flag, scenario)
{
   if (flag)
   {
      $('.js-action-scenario-spawn').removeClass('dc-btn--disabled')
      $('.js-action-scenario-abort').removeClass('dc-btn--disabled')
      $('.js-action-scenario-remove').removeClass('dc-btn--disabled')
   } else {
      $('.js-action-scenario-spawn').addClass('dc-btn--disabled')
      $('.js-action-scenario-abort').addClass('dc-btn--disabled')
      $('.js-action-scenario-remove').addClass('dc-btn--disabled')
   }
   return scenario
}

ui.scenario.editor = function(flag, x)
{
   $('.js-scenario-spec').prop('checked', flag)
   return x
}.$_()

ui.scenario.realtime = function(scenario)
{
   ui.cubism.init(scenario)

   $('.js-scenario-cubism-user').text('')
   ui.cubism.series('.js-scenario-cubism-user', [present.scenario.user2ts(scenario.id)])

   $('.js-scenario-cubism-url').text('')
   ui.cubism.series('.js-scenario-cubism-url', scenario.urls.map(present.scenario.url2ts(scenario.id)))

   $('.js-scenario-cubism-host').text('')
   ui.cubism.series('.js-scenario-cubism-host', scenario.hosts.map(present.scenario.host2ts(scenario.id)))

   return scenario
}.$_()

ui.scenario.attributes = function(attr)
{
   $('.dc-table__tr[data-scenario="' + attr.id + '"] .js-scenario-capacity').text(attr.capacity.toFixed(2))
   $('.dc-table__tr[data-scenario="' + attr.id + '"] .js-scenario-availability').text(attr.availability.toFixed(2))
   $('.dc-table__tr[data-scenario="' + attr.id + '"] .js-scenario-latency').text(attr.latency.toFixed(2))
   if (attr.latency > 0)
      $('.js-shortcut-' + attr.id).prop('checked', true)
   else
      $('.js-shortcut-' + attr.id).prop('checked', false)

   return attr
}.$_()

ui.scenario.error = function(error)
{
   model.editor.getSession().setAnnotations(
      error.map(
         function(x)
         {
            return {row: x.line - 1, column: 0, text: x.message, type: x.type}
         }
      )
   )
}.$_()


//
ui.history  = {}
ui.history.thumbnail = whiskers.compile($('#history-thumbnail').html())

ui.history.show = function(history)
{
   $('.js-history-thumbnail').empty()
   history.forEach(
      function(x)
      {
         var d = new Date(x.t * 1000)
         var h = Object.assign({}, x, {timestamp: d.toUTCString()})
         $('.js-history-thumbnail').prepend( ui.history.thumbnail(h) )
      }
   )
}.$_()

//
//
ui.cubism = {}
ui.cubism.orange = ['#ffefe0','#ffd1a6','#ffba7a','#ffa54e','#ff8e25','#cc711d','#995516','#66380e']
ui.cubism.gray   = ['#f7f7f7','#ededed','#d1d1d1','#b6b6b6','#9b9b9b','#808080','#646464','#4a4a4a']
ui.cubism.green  = ['#e7ffd6','#c6ff9e','#acff70','#8bfa3c','#72e620','#65cb1c','#4a9912','#30660a']
ui.cubism.blue   = ['#e6f4ff','#d1ebff','#a3d9ff','#75c6ff','#26aafe','#1e87cb','#186698','#124365']
ui.cubism.cyan   = ['#e8fcff','#b5f5ff','#8cf0ff','#61eaff','#24e0fe','#1fb3cb','#1a8899','#145b66']
ui.cubism.yellow = ['#fff9d9','#fff09e','#ffeb7a','#ffe347','#ffda0a','#ccb116','#99840e','#665705']
ui.cubism.red    = ['#ffeae6','#ffcabf','#ff9985','#ff6c4f','#ff4a25','#cc3a1d','#992b15','#661d0e']
ui.cubism.magenta= ['#f7e9f7','#f7c8f7','#f7b2f6','#f296f1','#eb74e9','#bb5cba','#8c458b','#5d2e5c']
ui.cubism.purple = ['#f4edff','#e1cfff','#c8a6ff','#ac7afd','#9757fd','#7845cb','#5a3498','#3c2365']

ui.cubism.init = function(scenario)
{
   model.context = cubism.context()
      .step(1 * 1000)          // 1 second per value
      .size(width)
      .serverDelay(30 * 1000)  // time to collect and process metrics by server
      .clientDelay( 5 * 1000)
      .start()
 
   d3.select('.js-scenario-cubism').append("div")
      .attr("class", "rule")
      .call(model.context.rule())
  
   model.context.on("focus", 
      function(i) 
      {
         d3.selectAll(".value")
            .style("right", i == null ? null : model.context.size() - i + "px");
      }
   )
}.$_()

//
ui.cubism.vmHorizon = function(el, list)
{
   var data = list.map(
      function(x)
      {
         return action.IO.series(x.id, x.urn, x.title, x.slo)
      }
   );

   d3.select(el).selectAll(".horizon")
      .data(data, function(d){return d.toString()})
      .enter()
         .insert("div", ".bottom")
            .attr("class", "horizon")
            .call(
               model.context.horizon()
                  .colors(ui.cubism.yellow.reverse().concat(ui.cubism.orange))
                  .height( height )
                  .format(d3.format("+,.2d"))
            );
}

//
ui.cubism.series = function(domel, sensors)
{
   d3.select(domel).selectAll('div')
      // .data(model.scenario.urls.concat(model.scenario.hosts))
      .data(sensors)
      .enter()
         .append('div')
         .attr('id', function(d){return "hash-" + d.id.hashCode()})
         .classed('chronolog', true)
   
   sensors.map(
      function(x)
      {
         var el = '#hash-' + x.id.hashCode()
         d3.select(el).selectAll("*").remove();
         d3.select(el).insert('h4').classed('dc-h4', true).text(x.id)

         d3.select(el).selectAll(".axis")
            .data(["top", "bottom"])
            .enter().append("div")
            .attr("class", function(d) {return d + " axis"; })
            .each(function(d) { d3.select(this).call(model.context.axis().ticks(12).orient(d)); });

         ui.cubism.vmHorizon(el, x.ts)
      }
   )

   return sensors;
}.$_();

//
ui.bullet = function(isA, cdfs)
{
   if (!cdfs)
   {
      $('.js-analytics__' + isA).hide()
      return
   } 
   $('.js-analytics__' + isA).show()
   var ui = '.dc-card__' + isA 


   var data = cdfs.map(
      function(stat)
      {
         return {
            title: stat.title,
            subtitle: '',
            ranges: [stat.min, stat.mean, stat.max],
            measures: [stat.q75, stat.q80, stat.q90, stat.q95, stat.q99],
            markers: [stat.q50]
         }
      }
   )

   var margin = {top: 25, right: 40, bottom: 20, left: 120};
   var width  = 800;
   var height = 75 - margin.top - margin.bottom;

   var chart = d3.bullet()
      .width(width)
      .height(height);
   
   d3.select(ui).selectAll('*').remove();
   var svg = d3.select(ui).selectAll("svg")
      .data(data)
      .enter().append("svg")
         .attr("class", "bullet")
         .attr("width", width + margin.left + margin.right)
         .attr("height", height + margin.top + margin.bottom)
         .append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
            .call(chart);

   var title = svg.append("g")
      .style("text-anchor", "end")
      .attr("transform", "translate(-6," + height / 2 + ")");

   title.append("text")
      .attr("class", "title")
      .text(function(d) { return d.title; });

   title.append("text")
      .attr("class", "subtitle")
      .attr("dy", "1em")
      .text(function(d) { return d.subtitle; });   
}.$_()


ui.system = function(json)
{
   var date = new Date(json.time * 1000)
   var time = ("0" + date.getHours()).slice(-2) + ':' + ("0" + date.getMinutes()).slice(-2)
   var diff = Math.abs( ~~(+new Date / 1000) - json.time )

   $('.js-system-time').text(time)
   if (diff > 300) {
      $('.js-system-time').addClass("dc-status dc-status--error")      
   } else {
      $('.js-system-time').removeClass("dc-status dc-status--error")
   }

   $('.js-system-peers').text(json.peers.length)
   $('.js-system-rps').text(json.rps)
   $('.js-system-failure').text(json.failure)
   $('.js-system-scenario').text(json.scenario)
}.$_()

//-----------------------------------------------------------------------------
//
// chain
//
//-----------------------------------------------------------------------------
var chain = {}

chain.show_info = function(dur, text)
{
   $('.js-msg-info-text').text(text)
   $('.dc-msg--info').show()
   M.do([
      M.IO(action.IO.after(dur, null)),
      function(_)
      {
         $('.dc-msg--info').hide()
      }
   ])
}

chain.show_error = function(dur, text)
{
   $('.js-msg-error-text').text(text)
   $('.dc-msg--error').show()
   M.do([
      M.IO(action.IO.after(dur, null)),
      function(_)
      {
         $('.dc-msg--error').hide()
      }
   ])
}

chain.init_ace_editor = function()
{
   var editor = ace.edit("js-ace-editor")
   editor.setTheme("ace/theme/github")
   editor.session.setMode("ace/mode/erlang")
   editor.session.setNewLineMode("unix")
   editor.setOptions({
      enableBasicAutocompletion: true,
      enableSnippets: true
   })
   model.editor = editor
}

chain.request_user_profile = function()
{
   M.do([
      M.IO(action.IO.typhoon.profile(model.user)),
      function(list)
      {
         model.profile = list
         return list
      },
      ui.scenario.list
   ]).fail(ui.fail)
}

chain.scenario_init = function()
{
   M.do([
      M.UI(action.UI.scenario.init),
      present.scenario.empty,
      ui.scenario.show
   ])
}

chain.scenario_show = function()
{
   M.do([
      M.UI(action.UI.scenario.show),
      function(req)
      {
         return M.IO(action.IO.typhoon.get(req.scenario))
      },
      ui.scenario.show,
      ui.scenario.realtime,
      ui.scenario.action,
      function(_)
      {
         return M.IO(action.IO.typhoon.history(model.scenario.id))
      },
      ui.history.show
   ]).fail(ui.fail)
}

chain.scenario_hide = function()
{
   M.do([
      M.UI(action.UI.scenario.hide),
      ui.scenario.hide
   ]).fail(ui.fail)
}

chain.scenario_lint_and_save = function()
{
   M.do([
      M.UI(action.UI.scenario.spec),
      ui.progressbar(true),
      present.scenario.spec,
      function(scenario)
      {
         return M.IO(action.IO.typhoon.lint(model.scenario))
      },
      function(lintlog)
      {
         chain.show_info(2000, lintlog)
         return M.IO(action.IO.typhoon.remove(model.scenario))
      },
      function(_)
      {
         return M.IO(action.IO.typhoon.put(model.scenario))
      },
      function(scenario)
      {
         return ui.scenario.append(scenario)
      },
      ui.scenario.editor(false),
      ui.scenario.show,
      ui.scenario.realtime,
      ui.scenario.action,
      ui.progressbar(false)
   ]).fail(
      function(err)
      {
         if ('lint' in err)
         {
            ui.scenario.error(err.lint)
         }
      }
   )
}

chain.scenario_shortcut = function()
{
   M.do([
      M.UI(action.UI.scenario.shortcut),
      function(json)
      {
         var klass = '.js-shortcut-' + json.scenario
         if ( $(klass).prop('checked') )
         {
            $(klass).prop('checked', false)
            return M.IO(action.IO.typhoon.abort(json.scenario))
         } else {
            $(klass).prop('checked', true)
            return M.IO(action.IO.typhoon.spawn(json.scenario))
         }
      }
   ]).fail(ui.fail)
}

chain.scenario_spawn = function()
{
   M.do([
      M.UI(action.UI.scenario.spawn),
      ui.progressbar(true),
      function(_)
      {
         return M.IO(action.IO.typhoon.spawn(model.scenario.id))
      },
      function(_)
      {
         chain.show_info(2000, "Launched " + model.scenario.n + " sessions.")
      },
      ui.progressbar(false)
   ]).fail(ui.fail)
}

chain.scenario_abort = function()
{
   M.do([
      M.UI(action.UI.scenario.abort),
      ui.progressbar(true),
      function(_)
      {
         return M.IO(action.IO.typhoon.abort(model.scenario.id))
      },
      function(_)
      {
         chain.show_info(2000, "Execution of scenario <" + model.scenario.id + "> is aborted.")
      },
      ui.progressbar(false)
   ]).fail(ui.fail)
}

chain.scenario_remove = function()
{
   M.do([
      M.UI(action.UI.scenario.remove),
      ui.progressbar(true),
      function(_)
      {
         return M.IO(action.IO.typhoon.remove(model.scenario))
      },
      function(_)
      {
         chain.show_info(2000, "Scenario <" + model.scenario.id + "> is removed.")
      },
      ui.progressbar(false)
   ]).fail(ui.fail)
}

chain.load_analytics = function(head, url, sensors, a, b)
{
   return sensors.ts.reduce(
      function(acc, x)
      {
         return M.do([acc,
            function(_)
            {
               return M.IO(action.IO.typhoon.series(x.id, x.urn, a, b))
            },
            function(series)
            {
               if (series.length > 0)
               {
                  var key  = x.urn.split('+')[0]
                  if (!(key in model.analytics))
                     model.analytics[key] = []
                  model.analytics[key].push(present.cdf(url, series))
               }
            }
         ])
      },
      head
   )
}

chain.history_show = function()
{
   M.do([
      M.UI(action.UI.history.show),
      function(history)
      {
         var ta = history.t 
         var tb = history.t + history.duration
         model.analytics = {}
         var io = model.scenario.urls.reduce(
            function(Mio, url)
            {
               var x = present.scenario.url2ts(model.scenario.id, url)
               return chain.load_analytics(Mio, url, x, ta, tb)
            },
            M.IO(true)
         )

         model.scenario.hosts.reduce(
            function(Mio, url)
            {
               var x = present.scenario.host2ts(model.scenario.id, url)
               return chain.load_analytics(Mio, url, x, ta, tb)
            },
            io
         ).bind(
            function(_)
            {
               ui.bullet('capacity', model.analytics['urn:c:2xx'])
               ui.bullet('ttfb', model.analytics['urn:g:ttfb'])
               ui.bullet('ttmr', model.analytics['urn:g:ttmr'])
               ui.bullet('connect', model.analytics['urn:g:connect'])
               ui.bullet('handshake', model.analytics['urn:g:handshake'])
               ui.bullet('pps', model.analytics['urn:c:packet'])
               ui.bullet('packet', model.analytics['urn:g:packet'])
            }
         ).fail(ui.fail)

      }
   ]).fail(ui.fail)
}


chain.system_status = function()
{
   M.do([
      M.IO(action.IO.after(5000, 1)),
      function(_)
      {
         return M.IO(action.IO.system())
      },   
      ui.system,
      function(_)
      {
         chain.system_status()
      }
   ]).fail(
      function(x)
      {
         console.error(x)
         chain.system_status()
      }
   )
}

chain.scenario_status = function(_)
{
   model.profile.reduce(
      function(acc, x)
      {
         return M.do([acc,
            function()
            {
               return M.IO(action.IO.typhoon.attr(x.id))
            },
            ui.scenario.attributes
         ])
      },
      M.IO(action.IO.after(5000, 1))
   ).bind(
      function(_){ chain.scenario_status() }
   ).fail(
      function(_){ chain.scenario_status() }
   )
}


//
// entry point
$(document).ready(
   function()
   {
      chain.init_ace_editor()
      chain.scenario_init()
      chain.scenario_show()
      chain.scenario_hide()
      chain.scenario_lint_and_save()
      chain.scenario_shortcut()
      chain.scenario_spawn()
      chain.scenario_abort()
      chain.scenario_remove()
      chain.history_show()

      chain.request_user_profile()

      chain.system_status()
      chain.scenario_status()
   }
)





