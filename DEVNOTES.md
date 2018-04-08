# Developer Notes

## Doc Issues

### ?Geom

Seems like a paragraph is out of place in ?Geom:

> Each of the ‘Geom’ objects is a ‘ggproto’ object, descended from the
> top-level ‘Geom’, and each implements various methods and fields. To create a
> new type of Geom object, you typically will want to implement one or more of
> the following:

And then there is another paragraph before the bullet point list.  There is
also conflict between "implement" and "override" in the first bullet point.

### Extending Ggplot 2 vignette

In "Creating a new Geom", `draw_panel` is described as having 3 parameters,
instead of the 4 + (in particular starting with `self`) in other docs and in the
source.


