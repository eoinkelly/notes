## CSS Environment Variables

- https://developer.mozilla.org/en-US/docs/Web/CSS/env()
- Spec: https://drafts.csswg.org/css-env-1/#env-function
- Supported by all major evergreen browsers
    - But have seen reports of it being buggy on Android
- `env(var-name-set-by-user-agent, fallback-value)`
- Can be used in any part of a property value or any part of a descriptor
- originally provided on iOS to allow devs to place content with the "safe area"
  of the viewport i.e. on non-rectangular screens
- Property names are case sensitive (unlike normal property names)
- As of end 2020, only 4 values defined
    1. safe-area-inset-top
    1. safe-area-inset-right
    1. safe-area-inset-bottom
    1. safe-area-inset-left
