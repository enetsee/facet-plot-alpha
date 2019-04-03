# Facet Plot

A plotting library inspired by the _Grammar of Graphics_ and various
implementations including [ggplot](http://ggplot2.tidyverse.org/index.html)
and [vega](https://vega.github.io/vega/).

The main idea behind the library is that a wide variety of different plots
can be created by composing a small set of primitive visual marks and that
data can be encoded as some visual attribute of those marks. Some [examples
created with the library can be found here.](https://enetsee.github.io/facet-plot-alpha/)

Once a plot is declared, it can be 'compiled' with some appropriate data to
generate a [`Scenegraph`](https://github.com/enetsee/facet-scenegraph-alpha).

The `Scenegraph` can then be rendered with any back-end. At the moment the only
available rendering is for [SVG](https:/github.com/enetsee/facet-render-svg-alpha)
but over time I may look to create back-ends for Canvas and WebGL.

`Facet` also supports [theming](https://github.com/enetsee/facet-theme-alpha)
i.e. creating a set of default styles to be applied to non-data attributes of a plot.

The key abstractions that support this are outlined below.

## NOTE

As indicated by the name, this library is very much in development. I have
open sourced it now since I want to use it in a work project and would like
help and feedback on the API.

## Plot

A `Plot` allows you to combine several layers of `Encodings` along with
the corresponding `Legends` and `Axis`.

In addition, you can specify how the plot should be facetted to create
[small multiples](https://en.wikipedia.org/wiki/Small_multiple).

### Axis

An `Axis` is a special type of `Legend` for `PositionalChannel`s which
shows the user-defined mapping between data and an on-screen position.


### Legend

A visualization of the user-defined mapping between data and some visual
aspect of a mark.

### Facet

Faceting a plot creates series of similar plots (or 'small multiples')
sharing the same scale and axes, allowing them to be easily compared.

A plot can be facetted by one `Field` to create either a row or column of
small multiples.

A plot can also be facetted by two `Field`s to create a grid of small multiples.


## Encoding

An `Encoding` is a means of encoding data as visual mark by combining
several `Channel`s to represent various attributes of that visual mark.

A description of each encoding along with the required and optional `Channels`
is given below.

### Arc

A circular arc.

#### Required channels

- x position (`PositionalChannel`)
- y position (`PositionalChannel`)
- start angle in Radians (`FloatChannel`)
- end angle in Radians (`FloatChannel`)
- outer radius in user-space pixels (`FloatChannel`)

#### Optional channels

- inner radius in user-space pixels (`FloatChannel`)
- corner radius in user-space pixels (`FloatChannel`)
- fill color (`ColorChannel`)
- fill opacity, between 0 and 1 (`FloatChannel`)
- stroke color (`ColorChannel`)
- stroke opacity, between 0 and 1 (`FloatChannel`)
- stroke width in user-space pixels (`FloatChannel`)
- stroke dash (`StrokeDashChannel`)
- tooltip (`TextChannel`)

### Area

Filled area with either vertical or horizontal orientation.

#### Required channels

- x positions (`PositionalChannel`)
- y positions (`PositionalChannel`)

You must also provide an interpolation method and the preferred behaviour
when missing values are encountered

#### Optional channels

- fill color (`ColorChannel`)
- fill opacity, between 0 and 1 (`FloatChannel`)
- stroke color (`ColorChannel`)
- stroke opacity, between 0 and 1 (`FloatChannel`)
- stroke width in user-space pixels (`FloatChannel`)
- stroke dash (`StrokeDashChannel`)
- tooltip (`TextChannel`)

### Line

Stroked lines.

#### Required channels

- x positions (`PositionalChannel`)
- y positions (`PositionalChannel`)

You must also provide an interpolation method and the preferred behaviour
when missing values are encountered

#### Optional channels

- stroke color (`ColorChannel`)
- stroke opacity, between 0 and 1 (`FloatChannel`)
- stroke width in user-space pixels (`FloatChannel`)
- stroke dash (`StrokeDashChannel`)
- tooltip (`TextChannel`)

### Polygon

Arbitrary filled polygons.

#### Required channels

- x positions (`PositionalChannel`)
- y positions (`PositionalChannel`)

You must also provide an interpolation method and the preferred behaviour
when missing values are encountered

#### Optional channels

- fill color (`ColorChannel`)
- fill opacity, between 0 and 1 (`FloatChannel`)
- stroke color (`ColorChannel`)
- stroke opacity, between 0 and 1 (`FloatChannel`)
- stroke width in user-space pixels (`FloatChannel`)
- stroke dash (`StrokeDashChannel`)
- tooltip (`TextChannel`)

### Rect

Filled rectangles.

#### Required channels

Either
- primary and secondary x and y positions
or
- primary x and y positions, width and height

#### Optional channels

- corner radius in user-space pixels (`FloatChannel`)
- fill color (`ColorChannel`)
- fill opacity, between 0 and 1 (`FloatChannel`)
- stroke color (`ColorChannel`)
- stroke opacity, between 0 and 1 (`FloatChannel`)
- stroke width in user-space pixels (`FloatChannel`)
- stroke dash (`StrokeDashChannel`)
- tooltip (`TextChannel`)

### Rule

Stroked line segments.

#### Required channels

- primary and secondary x positions (`PositionalChannel`)
- primary and secondary y positions (`PositionalChannel`)

#### Optional channels

- stroke color (`ColorChannel`)
- stroke opacity, between 0 and 1 (`FloatChannel`)
- stroke width in user-space pixels (`FloatChannel`)
- stroke dash (`StrokeDashChannel`)
- tooltip (`TextChannel`)

### Symbol

Plotting symbols, including circles, squares and other shapes.

#### Required channels

- shape (`ShapeChannel`)
- x position (`PositionalChannel`)
- y position (`PositionalChannel`)

#### Optional channels

- size in user-space pixels squared (`FloatChannel`)
- angle in Radians (`FloatChannel`)
- fill color (`ColorChannel`)
- fill opacity, between 0 and 1 (`FloatChannel`)
- stroke color (`ColorChannel`)
- stroke opacity, between 0 and 1 (`FloatChannel`)
- stroke width in user-space pixels (`FloatChannel`)
- stroke dash (`StrokeDashChannel`)
- tooltip (`TextChannel`)

### Text

Text labels with configurable fonts, alignment and angle.

#### Required Channels

- text (`TextChannel`)
- x position (`PositionalChannel`)
- y position (`PositionalChannel`)


#### Optional channels

- size in user-space pixels squared (`FloatChannel`)
- angle in Radians (`FloatChannel`)
- fill color (`ColorChannel`)
- fill opacity, between 0 and 1 (`FloatChannel`)
- stroke color (`ColorChannel`)
- stroke opacity, between 0 and 1 (`FloatChannel`)
- stroke width in user-space pixels (`FloatChannel`)
- stroke dash (`StrokeDashChannel`)
- tooltip (`TextChannel`)

### Trail

Filled lines with varying width.

#### Required Channels

- widths (`FloatChannel`)
- x positions (`PositionalChannel`)
- y positions (`PositionalChannel`)


#### Optional channels

- fill color (`ColorChannel`)
- fill opacity, between 0 and 1 (`FloatChannel`)
- tooltip (`TextChannel`)

## Channel

A `Channel` is a means representing data as some attribute of a visual mark by
specify a `Field` to extract data and a `Scale` to transform it to the type
required for that `Channel`

Available channels are summarized below. Each channel corresponds with the type
required  by some visual attribute of mark.

### Positional Channel

A `PositionalChannel` is used to associate a data value with a position on either
the x- or y-axis.

### Angle Channel

An `AngleChannel` is used to encode data as the rotation of a visual mark.

### Color Channel

A `ColorChannel` is used to encode data as either the fill color or stroke color
of a visual mark.

### Float Channel

A `FloatChannel` is used to encode data as some non-positional numeric attribute
of a visual mark e.g. stroke width, size, font size.

### Int Channel

A `IntChannel` is used to encode data as some non-positional numeric attribute
of a visual mark e.g. stroke width, size, font size.

### Shape Channel

A `ShapeChannel` is used to encode data as the shape used in a `Symbol` visual
mark.

### Text Channel

A `TextChannel` is used to encode data as the text of a `Text` mark or as the
tooltip of any visual mark.

### Stroke-dash Channel

A `StrokeDashChannel` is used to encode data as the stroke dash array
and (optional) stroke dash offset of a visual mark.

## Scale

A scale provides a means of mapping between values of type _domain_ to
values of type _range_.

Scales allow you to specify how data gets transformed after being extracted
by a `Field`.

## Field

A `Field` is a means of extracting a value from some data type.

There are different `Field`s allowing you to extract different 'shapes'
of data:
- A scalar `Field` extracts single item from a single piece of data;
- A vector `Field` extracts a list of items from a list of data;
- An aggregate `Field` summarizes a list of data as a single item.

In addition, each type of field supports situations where the item you
are extracting may be missing.
