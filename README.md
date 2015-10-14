# magit-gh-issues.el
A small plugin for Magit which allows users to see, open, comment and label GitHub issues.

## Installing

Add this package to your load path and then turn on the minor mode globally
```
(require 'magit-gh-issues)
(magit-gh-issues-mode 1)
```

#### Creating Issues
Currently, this package uses [`ghi`](https://github.com/stephencelis/ghi) to open _new_ issues, you can install it by running
```
brew install ghi
ghi config --auth
```
You need to authorize `ghi` so that it can work with _all_ your repos
