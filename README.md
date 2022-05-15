# OCaml-Edit

An experimental editor made using OCaml.

## To Do List:

### Navigation
- [x] Horizontal scrolling with cursor
- [x] Horizontal scrolling with mouse wheel
- [x] Cursor navigation with mouse
    - [x] Vertical navigation
    - [x] Horizontal navigation
- [ ] Vertical scrolling with cursor
    - Apply the idea of a scroll margin
- [ ] Page up/down (implemented but not working, could just be this laptop)

### Editing
- [x] Text insertion
- [x] Text deletion (backspace and delete)
- [x] Newlines
- [ ] Text selection (in progress)
  - [x] With mouse
  - [ ] With keyboard (in progress)
  - [x] Select-all

### UI
- [ ] Widgets (TODO: Expand on this)
- [ ] Line numbers
- [ ] Syntax highlighting?
- [x] Window resize event

### Performance
- [x] Cache the document texture if unchanged