type rsf_layer_config = {
  clip_min: f32,
  clip_max: f32,
  seed_offset: u64,
  grad_mean: bool
}

type rsf_config = {
  clip_min: f32,
  clip_max: f32,
  grad_mean: bool,
  max_dim: i64,
  max_layers: i64
}

def save_version : u32 = 4u32
def fractal_scale : f32 = 0.70710677f32
def gpu_enabled : bool = false

def default_layer_config : rsf_layer_config =
  { clip_min = -5.0f32, clip_max = 5.0f32, seed_offset = 0u64, grad_mean = true }

def default_config : rsf_config =
  { clip_min = -5.0f32, clip_max = 5.0f32, grad_mean = true,
    max_dim = 1i64 << 20, max_layers = 1i64 << 20 }

def is_finite_f32 (x: f32) : bool =
  ! (f32.isnan x) && ! (f32.isinf x)

def checked_mul_i64 (a: i64) (b: i64) : (bool, i64) =
  if a == 0 || b == 0 then (true, 0)
  else
    let r = a * b
    in (r / a == b && r >= 0, r)

def checked_mul_u64 (a: u64) (b: u64) : (bool, u64) =
  if a == 0u64 || b == 0u64 then (true, 0u64)
  else
    let r = a * b
    in (r / a == b, r)

def checked_add_u64 (a: u64) (b: u64) : (bool, u64) =
  let r = a + b
  in (r >= a, r)

def checked_cast_u64_to_i64 (v: u64) : (bool, i64) =
  if v > 0x7FFFFFFFFFFFFFFFu64 then (false, 0)
  else (true, i64.from_u64 v)

def validate_clip_range (clip_min: f32) (clip_max: f32) : bool =
  is_finite_f32 clip_min && is_finite_f32 clip_max
  && clip_min < clip_max
  && clip_max <= 20.0f32 && clip_min >= -20.0f32

def validate_comparison_tolerances (abs_tol: f32) (rel_tol: f32) : bool =
  is_finite_f32 abs_tol && is_finite_f32 rel_tol
  && abs_tol >= 0.0f32 && rel_tol >= 0.0f32

def validate_tensor_2d (rows: i64) (cols: i64) (data_len: i64) : bool =
  let (ok, expected) = checked_mul_i64 rows cols
  in ok && data_len == expected

def validate_tensor_2d_shape (rows_actual: i64) (cols_actual: i64) (data_len: i64)
                             (rows: i64) (cols: i64) : bool =
  rows_actual == rows && cols_actual == cols
  && validate_tensor_2d rows cols data_len

def tensor_has_shape (rows_actual: i64) (cols_actual: i64) (rows: i64) (cols: i64) : bool =
  rows_actual == rows && cols_actual == cols

def tensors_same_shape (r1: i64) (c1: i64) (r2: i64) (c2: i64) : bool =
  r1 == r2 && c1 == c2

def ensure_finite_slice [n] (data: [n]f32) : bool =
  reduce (&&) true (map is_finite_f32 data)

def zero_array (n: i64) : [n]f32 = replicate n 0.0f32

def tensor_all_close_eq [n] (a: [n]f32) (b: [n]f32) (abs_tol: f32) (rel_tol: f32) : bool =
  if ! (validate_comparison_tolerances abs_tol rel_tol) then false
  else
    let check (av: f32) (bv: f32) : bool =
      if ! (is_finite_f32 av) || ! (is_finite_f32 bv) then false
      else
        let diff = f32.abs (av - bv)
        let scale = f32.max (f32.abs av) (f32.abs bv)
        in diff <= abs_tol + rel_tol * scale
    in reduce (&&) true (map2 check a b)

def validate_model_config_values (dim: i64) (num_layers: i64) (cfg: rsf_config) : bool =
  dim > 0 && num_layers > 0
  && validate_clip_range cfg.clip_min cfg.clip_max
  && cfg.max_dim > 0 && cfg.max_layers > 0
  && dim <= cfg.max_dim && num_layers <= cfg.max_layers

def splitmix64 (state: u64) : u64 =
  let z = state + 0x9E3779B97F4A7C15u64
  let z1 = (z ^ (z >> 30u64)) * 0xBF58476D1CE4E5B9u64
  let z2 = (z1 ^ (z1 >> 27u64)) * 0x94D049BB133111EBu64
  in z2 ^ (z2 >> 31u64)

def u64_to_unit_f32 (x: u64) : f32 =
  let v = u32.from_u64 (x >> 32u64)
  in f32.from_u32 v / 4294967296.0f32

def random_uniform_array (n: i64) (lo: f32) (hi: f32) (seed: u64) : [n]f32 =
  map (\i ->
         let s = splitmix64 (seed + u64.from_i64 i)
         in lo + (hi - lo) * u64_to_unit_f32 s) (iota n)

type~ layer_core = {
  dim: i64,
  s_weight: []f32,
  t_weight: []f32,
  s_bias: []f32,
  t_bias: []f32,
  s_weight_grad: []f32,
  t_weight_grad: []f32,
  s_bias_grad: []f32,
  t_bias_grad: []f32,
  has_s_weight_grad: bool,
  has_t_weight_grad: bool,
  has_s_bias_grad: bool,
  has_t_bias_grad: bool,
  clip_min: f32,
  clip_max: f32,
  grad_mean: bool
}

def layer_init_owned (dim: i64) (config: rsf_layer_config) : layer_core =
  let fan_in = f32.from_i64 dim
  let fan_out = f32.from_i64 dim
  let fan_sum = fan_in + fan_out
  let xavier_bound = f32.sqrt (6.0f32 / fan_sum)
  let dim_sq = dim * dim
  let (_, seed1) = checked_add_u64 42u64 config.seed_offset
  let (_, seed2) = checked_add_u64 43u64 config.seed_offset
  let sw = random_uniform_array dim_sq (-xavier_bound) xavier_bound seed1
  let tw = random_uniform_array dim_sq (-xavier_bound) xavier_bound seed2
  let sb = zero_array dim
  let tb = zero_array dim
  in { dim = dim,
       s_weight = sw,
       t_weight = tw,
       s_bias = sb,
       t_bias = tb,
       s_weight_grad = zero_array dim_sq,
       t_weight_grad = zero_array dim_sq,
       s_bias_grad = zero_array dim,
       t_bias_grad = zero_array dim,
       has_s_weight_grad = false,
       has_t_weight_grad = false,
       has_s_bias_grad = false,
       has_t_bias_grad = false,
       clip_min = config.clip_min,
       clip_max = config.clip_max,
       grad_mean = config.grad_mean }

def layer_ensure_gradients (lc: layer_core) : layer_core =
  let dim_sq = lc.dim * lc.dim
  let lc1 =
    if lc.has_s_weight_grad then lc
    else lc with s_weight_grad = zero_array dim_sq
              with has_s_weight_grad = true
  let lc2 =
    if lc1.has_t_weight_grad then lc1
    else lc1 with t_weight_grad = zero_array dim_sq
               with has_t_weight_grad = true
  let lc3 =
    if lc2.has_s_bias_grad then lc2
    else lc2 with s_bias_grad = zero_array lc2.dim
               with has_s_bias_grad = true
  let lc4 =
    if lc3.has_t_bias_grad then lc3
    else lc3 with t_bias_grad = zero_array lc3.dim
               with has_t_bias_grad = true
  in lc4

def layer_zero_gradients (lc: layer_core) : layer_core =
  let dim_sq = lc.dim * lc.dim
  let lc1 =
    if lc.has_s_weight_grad then lc with s_weight_grad = zero_array dim_sq
    else lc
  let lc2 =
    if lc1.has_t_weight_grad then lc1 with t_weight_grad = zero_array dim_sq
    else lc1
  let lc3 =
    if lc2.has_s_bias_grad then lc2 with s_bias_grad = zero_array lc2.dim
    else lc2
  let lc4 =
    if lc3.has_t_bias_grad then lc3 with t_bias_grad = zero_array lc3.dim
    else lc3
  in lc4

def layer_validate_pair (lc: layer_core)
                        (a_rows: i64) (a_cols: i64) (a_len: i64)
                        (b_rows: i64) (b_cols: i64) (b_len: i64) : (bool, i64) =
  if ! (validate_tensor_2d a_rows a_cols a_len) then (false, 0)
  else if ! (validate_tensor_2d b_rows b_cols b_len) then (false, 0)
  else if a_cols != lc.dim || b_cols != lc.dim then (false, 0)
  else if a_rows != b_rows then (false, 0)
  else if a_rows == 0 then (false, 0)
  else
    let (ok, _) = checked_mul_i64 a_rows lc.dim
    in (ok, a_rows)

def compute_translation_row (lc: layer_core) (input_row: []f32) : []f32 =
  let dim = lc.dim
  in map (\d ->
            let base = d * dim
            in loop s = lc.t_bias[d] for j < dim do
                 s + lc.t_weight[base + j] * input_row[j]) (iota dim)

def compute_scale_row (lc: layer_core) (input_row: []f32) : []f32 =
  let dim = lc.dim
  in map (\d ->
            let base = d * dim
            let pre = loop s = lc.s_bias[d] for j < dim do
                       s + lc.s_weight[base + j] * input_row[j]
            let clipped =
              if pre < lc.clip_min then lc.clip_min
              else if pre > lc.clip_max then lc.clip_max
              else pre
            in f32.exp clipped) (iota dim)

def forward_row_pair (lc: layer_core) (x1_row: []f32) (x2_row: []f32) : ([]f32, []f32) =
  let scale = compute_scale_row lc x2_row
  let x1_new = map2 (*) x1_row scale
  let trans = compute_translation_row lc x1_new
  let x2_new = map2 (+) x2_row trans
  in (x1_new, x2_new)

def inverse_row_pair (lc: layer_core) (y1_row: []f32) (y2_row: []f32) : ([]f32, []f32) =
  let trans = compute_translation_row lc y1_row
  let y2_pre = map2 (-) y2_row trans
  let scale = compute_scale_row lc y2_pre
  let y1_new = map2 (/) y1_row scale
  in (y1_new, y2_pre)

def layer_forward_in_place (lc: layer_core) (x1: [][]f32) (x2: [][]f32) : ([][]f32, [][]f32) =
  let pairs = map2 (\r1 r2 -> forward_row_pair lc r1 r2) x1 x2
  in (map fst pairs, map snd pairs)

def layer_inverse_in_place (lc: layer_core) (y1: [][]f32) (y2: [][]f32) : ([][]f32, [][]f32) =
  let pairs = map2 (\r1 r2 -> inverse_row_pair lc r1 r2) y1 y2
  in (map fst pairs, map snd pairs)

def layer_verify_invertible (lc: layer_core) (x1: [][]f32) (x2: [][]f32) (abs_tol: f32) (rel_tol: f32) : bool =
  if ! (validate_comparison_tolerances abs_tol rel_tol) then false
  else
    let (f1, f2) = layer_forward_in_place lc x1 x2
    let (b1, b2) = layer_inverse_in_place lc f1 f2
    let fx1 = flatten x1
    let fx2 = flatten x2
    let fb1 = flatten b1
    let fb2 = flatten b2
    let n1 = length fx1
    let n2 = length fx2
    let m1 = length fb1
    let m2 = length fb2
    in n1 == m1 && n2 == m2
       && tensor_all_close_eq (fx1 :> [n1]f32) (fb1 :> [n1]f32) abs_tol rel_tol
       && tensor_all_close_eq (fx2 :> [n2]f32) (fb2 :> [n2]f32) abs_tol rel_tol

type~ backward_row_result = {
  x1_row: []f32,
  x2_row: []f32,
  dx1_row: []f32,
  dx2_row: []f32,
  s_w_delta: []f32,
  t_w_delta: []f32,
  s_b_delta: []f32,
  t_b_delta: []f32
}

def backward_from_outputs_row (lc: layer_core)
                              (y1_row: []f32) (y2_row: []f32)
                              (dy1_row: []f32) (dy2_row: []f32)
                              (grad_scale: f32) : backward_row_result =
  let dim = lc.dim
  let dy1_total =
    loop dt = dy1_row for d < dim do
      let dy2_val = dy2_row[d]
      let base = d * dim
      in loop acc = dt for j < dim do
           acc with [j] = acc[j] + lc.t_weight[base + j] * dy2_val
  let t_w_delta =
    flatten (map (\d ->
                    let dyv = dy2_row[d] * grad_scale
                    in map (\j -> dyv * y1_row[j]) (iota dim)) (iota dim))
  let t_b_delta = map (\d -> dy2_row[d] * grad_scale) (iota dim)
  let x2_row_out =
    map (\d ->
           let base = d * dim
           let trans_sum =
             loop s = lc.t_bias[d] for j < dim do
               s + lc.t_weight[base + j] * y1_row[j]
           in y2_row[d] - trans_sum) (iota dim)
  let pre_arr =
    map (\d ->
           let base = d * dim
           in loop s = lc.s_bias[d] for j < dim do
                s + lc.s_weight[base + j] * x2_row_out[j]) (iota dim)
  let clipped_arr =
    map (\pre ->
           if pre < lc.clip_min then lc.clip_min
           else if pre > lc.clip_max then lc.clip_max
           else pre) pre_arr
  let scale_arr = map f32.exp clipped_arr
  let x1_row_out = map2 (/) y1_row scale_arr
  let dx1_row_out = map2 (*) dy1_total scale_arr
  let ds =
    map2 (\pre d ->
            if pre < lc.clip_min || pre > lc.clip_max
            then 0.0f32
            else dy1_total[d] * y1_row[d]) pre_arr (iota dim)
  let s_w_delta =
    flatten (map (\d ->
                    let dsv = ds[d] * grad_scale
                    in map (\j -> dsv * x2_row_out[j]) (iota dim))
