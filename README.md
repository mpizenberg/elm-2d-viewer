# elm-2d-viewer

A 2D (image) viewer with zoom and pan in mind.
The core module is `Viewer`, which defines the type
and transformations available.

The module `Viewer.Svg` provides a function to transform
a group of SVG elements to visualize them as specified by a viewer.

I suppose additional modules for canvas or webgl could be useful
as well but are not available for the time being.
