# onyx-sim

Try it out here: https://mademonkey.com/onyx-sim

Onyx simimulator is an interactive interface for the the [onyx local runtime](https://github.com/onyx-platform/onyx-local-rt).

## Dat View

Onyx-sim is a temporary home for future [datview](https://github.com/metasoarous/datview) part of the [datsys](https://github.com/metasoarous/datsys) project.

What is a segment?

You can think of a segment as a plain clojure map or object or record. In onyx it is called a segment partially because it could be broken into peices and distributed to various tasks.


What is a task?

Basically a function with some extra meta-data that can be placed in the onyx compute graph.

Beginning of spec for dat.view here:

```

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

``` shell
clj -M:fig:build
```

### Emacs/CIDER

CIDER is able to start both a Clojure and a ClojureScript REPL simultaneously,
so you can interact both with the browser, and with the server. The command to
do this is `M-x cider-jack-in-clojurescript`.

## Production

To build cljs use
``` shell
clj -M:fig:prod
clj -M:compile
clojure -M:uber
```

To run use
``` shell
java -jar target/project.jar
```

## License

Copyright © 2016 Brian Marco

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

## Chestnut

Originally created with [Chestnut](http://plexus.github.io/chestnut/) 0.15.2 (e2759aff). Heavily modified to use figwheel-main.
