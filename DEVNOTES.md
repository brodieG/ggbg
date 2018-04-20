# Developer Notes

## Alternate Docs

### On `gg_proto`

Things to know:

* Objects that contain member functions and data
* The members can be accessed like list objects `object$member`
* When member functions are invoked they are always automatically provided with
  a `self` object if `self` is part of the signature (you can probably still
  access `self` even if it isn't in the sig, need to test).

### Geom Basics

To implement a geom you need:

* A `geom_*` standard function
* A `Geom*` object, possibly re-used from an existing geom

### The `Geom*` Object

`Geom*` objects such as `GeomPoint` or `GeomRect` are responsible for
translating your data into graphical objects.  In `ggplot` these graphical
objects are called "grobs", short for Grid Graphical Objects, because they are
encoded in a format defined by the `grid` R package.  In order to create your
own geoms you will need to learn how to use `grid`, or how to re-use existing
`Geom*` objects to generate grobs suited for your purposes.

`Geom*` objects are implemented using `ggproto`, a `ggplot2` specific Object
Oriented framework.  `ggproto` is derived from the `proto` R OOP package.


creating
the Grid Objects (aka "grobs")  

## Doc Issues

### ?Geom

Seems like a paragraph is out of place in ?Geom:

> Each of the ‘Geom’ objects is a ‘ggproto’ object, descended from the
> top-level ‘Geom’, and each implements various methods and fields. To create a
> new type of Geom object, you typically will want to implement one or more of
> the following:

And then there is another paragraph before the bullet point list.  There is
also conflict between "implement" and "override" in the first bullet point.

## Gotcha's

You need to generate your first set of docs before you use any of the `ggproto`
calls because those are evaluated at package load time, and will break
documentation (and NAMESPACE) generation.

### Extending Ggplot 2 vignette

In "Creating a new Geom", `draw_panel` is described as having 3 parameters,
instead of the 4 + (in particular starting with `self`) in other docs and in the
source.  Additionally, the `panel_scales` param appears to actually be
`panel_params`, at least in the sample fun we used (but `?Geom` also references
`panel_scales`).

For `draw_panel` sigs, we have

* `GeomPoint`: `function(data, panel_params, coord, na.rm = FALSE)`
* `GeomRect`: `function(self, data, panel_params, coord)`
* `Geom`: `function(self, data, panel_params, coord, ...)`

From what I can infer from docs and source your function must accept at least
three arguments.  `draw_layer` from `Geom` will call `draw_panel` with three
unnamed parameters that in theory should match up to `data`, `panel_params`,
`coord`.

* `data`: A data.frame with all the aesthetic values as specified via `aes`.
  The column names corresponding to the aesthetic names, not the original data
  frame column names.
* `panel_params`: named list, the intersection of parameters provided to the
  `geom_*` function with the formals of the `draw_panel` method, although you
  can customize `Geom` objects to return a specific eligible parameter list.
* `coord`: coord transformation functions?

Additionally, named parameters that are the intersection of the parameters
provided to the `geom_*` function by the user and the parameters of `draw_panel`
are supplied.

Finally, since `draw_panel` is a `ggproto` method `self` will be provided if it
is part of the `draw_panel` signature.

### `setup_data` vs `reparametrise`

It seems like both those functions can be used for the same purpose.  For
example, in `GeomRect`, `setup_data` is explicitly used to convert width to
`xmax/xmin`.

Actually, looks like `reparameterise` doesn't exist anymore?


Need to check where `draw_panel` is called. AFAICT this is through the
`draw_layer` method of `Geom`, which invokes it as:

```
    args <- c(list(quote(data), quote(panel_params), quote(coord)), params)
    plyr::dlply(data, "PANEL", function(data) {
      if (empty(data)) return(zeroGrob())

      panel_params <- layout$panel_params[[data$PANEL[1]]]
      do.call(self$draw_panel, args)
    }, .drop = FALSE)
```

Since the arguments are not named, it is quite remarkable this works.  Not sure
how it is possible for this to work with the different signatures.

Actually, by the looks of it `params` in the composition of `args` can include a
named `self`.  This from a traceback of `geom_rect`:

```
[17]]
do.call(self$draw_panel, args)

[[18]]
(function (...) 
f(..., self = self))(data, panel_params, coord)

[[19]]
f(..., self = self)
```

Ah, and it looks that what's happening is that a `ggproto` method is not just
the function you define, but a processed version of it that looks like:

```
<ggproto method>
  <Wrapper function>
    function (...) 
f(..., self = self)

  <Inner function (f)>
    function (self, data, panel_params, coord) 
{
```

This should mean that `self` is provided if it isn't otherwise, but if you do
provide somehow in a direct call to `draw_panel` it then you must do so with a
`self=self` form as otherwise it gets captured by the dots.

Actually, for `geom_point` which doesn't have a `self` param, the call doesn't
include `self`:

```
 $ : language do.call(self$draw_panel, args)
 $ : language (function (...)  f(...))(data, panel_params, coord, na.rm = FALSE)
 $ : language f(...)
```

Alright, looks like there is automated parameter trimming going on, which is why
`GeomPoint` doesn't need `self`:

```
params <- params[intersect(names(params), self$parameters())]
```
