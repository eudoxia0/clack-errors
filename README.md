![Screenshot](https://raw.github.com/eudoxia0/clack-errors/master/screenshot.png)

A clone of [better_errors](https://github.com/charliesome/better_errors)
for [Clack](https://github.com/fukamachi/clack).

# Usage

Simply add the `<clack-error-middleware>` to your application's clackup form.

```lisp
(clack:clackup
  (builder
    clack-errors:<clack-error-middleware>
    *app*))
```

# License

Copyright (c) 2013 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the LLGPL License.
