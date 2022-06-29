type key =
  | K_a
  | K_b
  | K_c
  | K_d
  | K_e
  | K_f
  | K_g
  | K_h
  | K_i
  | K_j
  | K_k
  | K_l
  | K_m
  | K_n
  | K_o
  | K_p
  | K_q
  | K_r
  | K_s
  | K_t
  | K_u
  | K_v
  | K_w
  | K_x
  | K_y
  | K_z
  | K_up
  | K_down
  | K_left
  | K_right
  | K_home
  | K_end
  | K_page_up
  | K_page_down
  | K_return
  | K_backspace
  | K_delete

type kmod = M_shift | M_ctrl | M_alt | M_meta
type mouse = Mouse_left | Mouse_right | Mouse_middle

type t =
  | KeyDown of key * kmod
  | KeyUp of key * kmod
  | MouseDown of mouse * Helpers.point
  | MouseUp of mouse * Helpers.point
  | MouseMove of Helpers.point
  | MouseWheel of int * int
  | TextInput of string
