
```
git push origin foo
#automatically expands to
git push origin refs/heads/foo:refs/heads/foo


git push origin foo:bar
# will expand to
git push origin refs/heads/foo:refs/heads/bar
```
