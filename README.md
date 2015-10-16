# magit-gh-issues.el
A small plugin for Magit which allows users to see, open, comment and label GitHub issues.

## Installing

Add this package to your load path and then turn on the minor mode globally
```
(require 'magit-gh-issues)
(magit-gh-issues-mode 1)
```


#### Usage
Most interactions are context sensitive and only run when highlighting an issue _(currently this project doesn't support performing actions on multiple issues, partly because I didn't think this made sense)_. There are a couple of commands that can be run globally _(I'd like to be able to add these to the `magit-popup` in the future)_

##### Global bindings

Key Binding | Command | Effect 
--- | --- | ---
<kbd>I o</kbd> | `magit-gh-issues-open-issue` | Open a new issue and refresh the list of issues
<kbd>I g</kbd> | `magit-gh-issues-reload` | Purge the cache of issues and refetch them for the current project
<kbd>I z</kbd> | `magit-gh-issues-collapse-issues` | Collapse all of the currently opened issues

##### Context Sensitive bindings

Key Binding | Command | Effect 
--- | --- | ---
<kbd>a</kbd> | `magit-gh-issues-add-label` | Add a label to the currently highlighted issue from a popup
<kbd>r</kbd> | `magit-gh-issues-remove-label` | Remove a label from the currently highlighted issue
<kbd>c</kbd> | `magit-gh-issues-comment-issue` | Comment on the current issue
<kbd>k</kbd> | `magit-gh-issues-close-issue` | Close the current issue
<kbd>RET</kbd> | `magit-gh-issues-visit-issue` | Open the current issue in a browser

#### Creating Issues
Currently, this package uses [`ghi`](https://github.com/stephencelis/ghi) to open _new_ issues, you can install it by running
```
brew install ghi
ghi config --auth
```
You need to authorize `ghi` so that it can work with _all_ your repos

#### Planned Features
- [x] Ability to open Issues
- [x] Ability to close Issues
- [x] Ability to add/remove labels
- [x] Display comments on Issues
- [x] Ability to comment on Issues
- [ ] User Images next to comments
- [ ] Ability to see Assignees
- [ ] Assign people to issues
- [ ] Show events in the issue break down
    - Unsure whether this should be mixed in with the comments or a separate section
    - Whether we should display _all_ events or just the references
    - [ ] Link through to commit references?
- [ ] Auto completion for `git-commit-mode` which let's you refernce issues in commits
- [ ] Hook into `magit-popup` to display command prompts for issues
