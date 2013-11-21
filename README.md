![Development screenshot](https://raw.github.com/eudoxia0/clack-errors/master/screenshot-dev.png)
![Production screenshot](https://raw.github.com/eudoxia0/clack-errors/master/screenshot-prod.png)

A clone of [better_errors](https://github.com/charliesome/better_errors)
for [Clack](https://github.com/fukamachi/clack).

By default, when Clack throws an exception when rendering a page, the
server waits for the response until it times out while the exception waits
in the REPL. This isn't very useful. So now there's this.

# Usage

Simply add the `<clack-error-middleware>` to your application's clackup form.

```lisp
(clack:clackup
  (builder
    clack-errors:<clack-error-middleware>
    *app*))
```

By default, the middleware will show all information. In a production environment,
you'll want to initialize this with the `:debug` parameter set to NIL.

If you use [Envy](https://github.com/fukamachi/envy) to manage configuration,
you'd initialize it like this:

```lisp
(clack:clackup
  (builder
    (clack-errors:<clack-error-middleware>
      :debug (getf (envy:config :myapp) :debug))
    *app*))
```

# License

Copyright (c) 2013 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the LLGPL License.
