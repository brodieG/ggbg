
# Developer Notes

## Relevant docs

* ggplot2 docs `?Geom`, etc.
* ggplot2 vignette
* [B. Rudis in-progress Book](https://rud.is/books/creating-ggplot2-extensions/)

## Waterfall

### geom / stat / position

Some question of whether it should be a geom, or a position, or even a stat.
Unfortunately the ggplot2 semantics don't really fit fully with any of this.
Technically it's not really a geom since we're just making bars/rects here.
We're translating the points though, which maybe is more like a position, except
we're doing it across x values instead of within a value, so it's not a standard
position adjustment. It's also not a stat since we're not changing the number of
rows in the data.

Given stacking doesn't work, maybe we should do this via position:
waterfall-stack / waterfall-dodge.  Even though it doesn't naturally meet with
the ggplot2 semantics, the position adjustments appear to have all the data for
each layer.

### stacking

Need to figure out what the generic stacking mechanism is for various inputs:

* `y`
* `y` + `height`
* `ymin` + `ymax`
* `ymin` + `ymax` + `height`
* `y` + `ymin` + `ymax`
* `y` + `ymin` + `ymax` + `height`

Let's take the case with `y` + `height`.  What is the desired outcome?  We want
the graphical elements to align on top of each other.  So as soon as there is
height there is the potential for an element to float off of the x-axis.  But
then it becomes difficult to distinguish what it means for the waterfall to go
up or down.  If not floating then it is obvious based on the sign of y.  If it
is floating then it becomes a lot trickier as the semantics change completely.

Alternate: If the entire "value" of our element is dictated purely by the `y`
aesthetic this simplifies things a bit.  How do we handle conflicts with
pre-existing `ymin` and `ymax` values?  We're going to hijack them, so if they
already exist it is a bit of a problem.  And should we also force `height` to
conform?  Seemingly no, both stack and dodge appear to operate purely by
modifying `xmin/xmax/ymin/ymax`.

So basically, we require a `y` value.  If `ymax/min` values exist we over-write
them with the adjustments, so height values will probably stop working?

Actually, more important than anything is not hijacking existing xmin/xmax
values, so we just need to apply the stacking purely based on `y` values, and
whatever happens happens.

## Ggplot Doc Issues

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

## Random Ggplot Analysis Notes

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
