# onyx-sim

## What's this now

Try it out here: https://onyx-sim.herokuapp.com/

Onyx simimulator is an interactive implementation of the [onyx local runtime](https://github.com/onyx-platform/onyx-local-rt).

Clojars [onyx-sim "0.1.0"].

## Dat View

Onyx-sim is a temporary home for future [datview](https://github.com/metasoarous/datview) part of the [datsys](https://github.com/metasoarous/datsys) project.

What is a segment?

You can think of a segment as a plain clojure map or object or record. In onyx it is called a segment partially because it could be broken into peices and distributed to various tasks.


What is a task?

Basically a function with some extra meta-data that can be placed in the onyx compute graph.


Beginning of spec for dat.view here:

```clojure

dat-view-entity            = datascript entity containing some of the following keys:

:dat.view/route            = keyword used for identing datview entity data to be assoc'd into the segment


:dat.view.sub/handler      = keyword type of subscription e.g. :dat.view.subscribe/pull or :dat.view.subscribe/query

:dat.view.sub/pull-expr    = datascript pull-expr

:dat.view.sub/entity       = datascript entity to be pulled

:dat.view.sub/q-expr       = datascript expr to be fed to q

:dat.view.sub/inputs       = data to be fed into q

:dat.view.sub/alias        = dat-view-alias applied to both query and pull (should really be at end of subscription layer)
dat-view-alias             = map of form: {alias-target-key alias-source-path}
alias-target-key           = keyword which will be assoc'd into the current segment
alias-source-path          = get-in style vector which contains the value you wish to assoc

:dat.view.sub/row-base     = the base entity for each row of a query

:dat.view.sub/row-alias    = dat-view-alias applied to each row of a query


:dat.view.rep/handler      = keyword selecting which dat.view representation function should be applied to this segment

:dat.view.rep/component    = reagent vector intended to be displayed. This is not pure data, but an actual reagent component.


:dat.view.rep/style         = inline css in clojure map format

:dat.view.rep/label         = string used by some components

:dat.view.rep/direction     = direction for box layout component



:dat.view.event/handler    = keyword selecting the event handler to use for this segment

:dat.view.event/attr       = keyword attr event target

:dat.view.event/entity     = local eid for event target

:dat.view.event/context    = set of attr keys from this segment to include in the event for dispatch. #{:dat.view.event/handler :dat.view.event/attr :dat.view.event/attr} included by default


```


## Development

Open a terminal and type `lein repl` to start a Clojure REPL
(interactive prompt).

In the REPL, type

```clojure
(go)
(cljs-repl)
```

The call to `(go)` starts the Figwheel server at port 3449, which takes care of
live reloading ClojureScript code and CSS, and the app server at port 10555
which forwards requests to the http-handler you define.

Running `(cljs-repl)` starts the Figwheel ClojureScript REPL. Evaluating
expressions here will only work once you've loaded the page, so the browser can
connect to Figwheel.

When you see the line `Successfully compiled "resources/public/app.js" in 21.36
seconds.`, you're ready to go. Browse to `http://localhost:10555` and enjoy.

**Attention: It is not needed to run `lein figwheel` separately. Instead `(go)`
launches Figwheel directly from the REPL**

## Trying it out

If all is well you now have a browser window saying 'Hello Chestnut',
and a REPL prompt that looks like `cljs.user=>`.

Open `resources/public/css/style.css` and change some styling of the
H1 element. Notice how it's updated instantly in the browser.

Open `src/cljs/onyx-sim/core.cljs`, and change `dom/h1` to
`dom/h2`. As soon as you save the file, your browser is updated.

In the REPL, type

```
(ns onyx-sim.core)
(swap! app-state assoc :text "Interactivity FTW")
```

Notice again how the browser updates.

### Lighttable

Lighttable provides a tighter integration for live coding with an inline
browser-tab. Rather than evaluating cljs on the command line with the Figwheel
REPL, you can evaluate code and preview pages inside Lighttable.

Steps: After running `(go)`, open a browser tab in Lighttable. Open a cljs file
from within a project, go to the end of an s-expression and hit Cmd-ENT.
Lighttable will ask you which client to connect. Click 'Connect a client' and
select 'Browser'. Browse to [http://localhost:10555](http://localhost:10555)

View LT's console to see a Chrome js console.

Hereafter, you can save a file and see changes or evaluate cljs code (without
saving a file).

### Emacs/CIDER

CIDER is able to start both a Clojure and a ClojureScript REPL simultaneously,
so you can interact both with the browser, and with the server. The command to
do this is `M-x cider-jack-in-clojurescript`.

We need to tell CIDER how to start a browser-connected Figwheel REPL though,
otherwise it will use a JavaScript engine provided by the JVM, and you won't be
able to interact with your running app.

Put this in your Emacs configuration (`~/.emacs.d/init.el` or `~/.emacs`)

``` emacs-lisp
(setq cider-cljs-lein-repl
      "(do (user/go)
           (user/cljs-repl))")
```

Now `M-x cider-jack-in-clojurescript` (shortcut: `C-c M-J`, that's a capital
"J", so `Meta-Shift-j`), point your browser at `http://localhost:10555`, and
you're good to go.

## Testing

To run the Clojure tests, use

``` shell
lein test
```

To run the Clojurescript you use [doo](https://github.com/bensu/doo). This can
run your tests against a variety of JavaScript implementations, but in the
browser and "headless". For example, to test with PhantomJS, use

``` shell
lein doo phantom
```

## Deploying to Heroku

This assumes you have a
[Heroku account](https://signup.heroku.com/dc), have installed the
[Heroku toolbelt](https://toolbelt.heroku.com/), and have done a
`heroku login` before.

``` sh
git init
git add -A
git commit
heroku create
git push heroku master:master
heroku open
```

## Running with Foreman

Heroku uses [Foreman](http://ddollar.github.io/foreman/) to run your
app, which uses the `Procfile` in your repository to figure out which
server command to run. Heroku also compiles and runs your code with a
Leiningen "production" profile, instead of "dev". To locally simulate
what Heroku does you can do:

``` sh
lein with-profile -dev,+production uberjar && foreman start
```

Now your app is running at
[http://localhost:5000](http://localhost:5000) in production mode.

## License

Copyright © 2016 Brian Marco

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

## Chestnut

Created with [Chestnut](http://plexus.github.io/chestnut/) 0.15.2 (e2759aff).
