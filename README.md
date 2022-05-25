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
- [ ] Ctrl+left/right should skip words

### Editing
- [x] Text insertion
- [x] Text deletion (backspace and delete)
- [x] Newlines
- [x] Text selection
  - [x] With mouse
  - [x] With keyboard
  - [x] Select-all

### UI
- [ ] Widgets (TODO: Expand on this)
- [ ] Line numbers
- [ ] Syntax highlighting?
- [x] Window resize event

### Performance
- [x] Cache the document texture if unchanged

### Tech Debt
- [x] Refactor document lines as own module (refactored as `Doctext`)
- [ ] Refactor entire rendering context (window, renderer, etc) as own module
- [x] Refactor cursor module to do operations on `cursor_pos` instead. May want to declare own module so it is testable?

### Bugs

- [x] Shift+left to select over newline crashes (do `cursor_pos` refactor first)
- [ ] There is a memory leak when the cursor is not moving around vertically. It is likely that the textures are not being disposed of properly.