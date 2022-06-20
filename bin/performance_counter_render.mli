val as_mean_text :
  int OEditor.Performance_counter.performance_counter ->
  Tsdl.Sdl.renderer ->
  Theme.t ->
  int ->
  unit

val as_max_text :
  int OEditor.Performance_counter.performance_counter ->
  Tsdl.Sdl.renderer ->
  Theme.t ->
  int ->
  unit

val as_bar_graph :
  int OEditor.Performance_counter.performance_counter ->
  Tsdl.Sdl.renderer ->
  Tsdl.Sdl.Pixel.format_enum ->
  int ->
  unit
