- parser directives
    - sort of a precompiler directive
    - not case sensitive, traditionally are lowercase
    - must be at top of Dockerfile (docker stops looking for them after empty
      line or any other directive is found
    - currently only one supported directive: `escape

```
# directive=value
# escape=`
# makes backtick be the escape character (handy on windows where \ is path separator)

FROM foo/bar
```
