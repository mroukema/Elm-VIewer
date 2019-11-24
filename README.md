# Elm-Viewer
Simple browser based offline image slideshow written in Elm.

Offline in the sense that app doesn't need server of any sort once it is distributed

Created to facilitate casting slideshow to chromecast.

Runs in browser tab so it can be casted in background. Displays images from local file system


### Todo List

  - [ ] Handle ImageKey conflicts when ImageUrl is different
  - [ ] Detect and remove images with duplicate image data
  - [ ] Improve on screen controls for removing images
  - [ ] Allow for image transformations (rotation, mirror, zoom)
  - [ ] Collections of images
  - [ ] Persistence between browser opening/closing
  - [ ] Secure any persisted data
  - [ ] Separate URLs for different views
  - [ ] Upgrade to Browser.application; handle URLs
  - [ ] Make keyboard controls configurable
  - [ ] More on screen controls
  - [ ] Design/style improvement
  - [ ] Explore use cases for an online version of application

## Controls

### Slideshow View
- Left/Right Arrow Keys : Cycle images forwards/backwards
- Space : Pause/Unpause Slideshow
- Escape : Return to preview view

### Preview View
- Space : Start slideshow

### PreferencesView
- Escape : Return to preview view
