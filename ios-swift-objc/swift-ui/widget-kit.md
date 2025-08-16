# Widget kit

Things you need for a widget

1. A `WidgetBundle`
    - The entry point for the built target
    - has similar shape to a SwiftUI view except the `body` property returns a `some Widget` rather
      than `some View`
1. A `TimelineProvider`
1. A `TimelineEntry`
1. A `View`
1. A `Widget`
