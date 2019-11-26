# Elm-Viewer
Simple browser based offline image slideshow written in Elm.

Offline in the sense that app doesn't need server of any sort once it is distributed

Created to facilitate casting slideshow to chromecast.

Runs in browser tab so it can be casted in background. Displays images from local file system


### Todo List

  - [ ] Handle ImageKey conflicts when ImageUrl is different
  - [ ] Detect and remove images with duplicate image data
  - [x] Improve on screen controls for removing images
  - [ ] Allow for image transformations (rotation, mirror, zoom)
  - [ ] Collections of images
  - [x] Persistence via saveFile
  - [ ] Secure any persisted data
  - [ ] Separate URLs for different views
  - [ ] Upgrade to Browser.application; handle URLs
  - [ ] Make keyboard controls configurable
  - [x] More on screen controls
    - [ ] Still room for more
  - [ ] Design/style improvement
    - Some progress made
  - [ ] Explore use cases for an online version of application
  - [ ] Notification when images conversion fails
  - [ ] Grey-out / hide start slideshow button when no images

## Keyboard Controls

### Slideshow View
- Left/Right Arrow : Cycle images forwards/backwards
- Arrow Down : Return to Preview Catalog View
- Space : Pause/Unpause Slideshow
- Escape : Return to preview view

### Preview View

#### Catalog View
- Space : Start slideshow

#### Focused Image
- ArrowUp : Open slideshow (paused) at focused image
- Arrow Down : Return to catalog view
- Escape : Return to catalog view


### PreferencesView
- Escape : Return to preview view
