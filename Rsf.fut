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
  else (true, i64.u64 v)

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
  let v = u32.u64 (x >> 32u64)
  in f32.u32 v / 4294967296.0f32

def random_uniform_array (n: i64) (lo: f32) (hi: f32) (seed: u64) : [n]f32 =
  map (\i ->
         let s = splitmix64 (seed + u64.i64 i)
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
  let fan_in = f32.i64 dim
  let fan_out = f32.i64 dim
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
  in (map (.0) pairs, map (.1) pairs)

def layer_inverse_in_place (lc: layer_core) (y1: [][]f32) (y2: [][]f32) : ([][]f32, [][]f32) =
  let pairs = map2 (\r1 r2 -> inverse_row_pair lc r1 r2) y1 y2
  in (map (.0) pairs, map (.1) pairs)

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
    loop dt = copy dy1_row for d < dim do
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
                    in map (\j -> dsv * x2_row_out[j]) (iota dim)) (iota dim))
  let s_b_delta = map (\d -> ds[d] * grad_scale) (iota dim)
  let dx2_row_out =
    loop dx = copy dy2_row for d < dim do
      let ds_val = ds[d]
      let base = d * dim
      in loop acc = dx for j < dim do
           acc with [j] = acc[j] + lc.s_weight[base + j] * ds_val
  in { x1_row = x1_row_out,
       x2_row = x2_row_out,
       dx1_row = dx1_row_out,
       dx2_row = dx2_row_out,
       s_w_delta = s_w_delta,
       t_w_delta = t_w_delta,
       s_b_delta = s_b_delta,
       t_b_delta = t_b_delta }

def accumulate_layer_grads (lc: layer_core)
                           (sw_d: []f32) (tw_d: []f32)
                           (sb_d: []f32) (tb_d: []f32) : layer_core =
  let dim_sq = lc.dim * lc.dim
  let dim = lc.dim
  let sw_typed = sw_d :> [dim_sq]f32
  let tw_typed = tw_d :> [dim_sq]f32
  let sb_typed = sb_d :> [dim]f32
  let tb_typed = tb_d :> [dim]f32
  let cur_sw = lc.s_weight_grad :> [dim_sq]f32
  let cur_tw = lc.t_weight_grad :> [dim_sq]f32
  let cur_sb = lc.s_bias_grad :> [dim]f32
  let cur_tb = lc.t_bias_grad :> [dim]f32
  in lc with s_weight_grad = map2 (+) cur_sw sw_typed
        with t_weight_grad = map2 (+) cur_tw tw_typed
        with s_bias_grad = map2 (+) cur_sb sb_typed
        with t_bias_grad = map2 (+) cur_tb tb_typed

type~ rsf_core = {
  dim: i64,
  num_layers: i64,
  layers: []layer_core,
  cfg: rsf_config,
  gpu_available: u8,
  gpu_weight_version: u64,
  cpu_weight_version: u64,
  f16_buf_present: bool,
  f16_buf: []f32,
  oftb_dim: i64
}

def rsf_init_with_config (dim: i64) (num_layers: i64) (cfg: rsf_config) : rsf_core =
  let layers =
    map (\l ->
           let (_, seed_base) = checked_mul_u64 (u64.i64 l) 10007u64
           let layer_cfg : rsf_layer_config =
             { clip_min = cfg.clip_min,
               clip_max = cfg.clip_max,
               seed_offset = seed_base,
               grad_mean = cfg.grad_mean }
           in layer_init_owned dim layer_cfg) (iota num_layers)
  in { dim = dim,
       num_layers = num_layers,
       layers = layers,
       cfg = cfg,
       gpu_available = 0u8,
       gpu_weight_version = 0u64,
       cpu_weight_version = 1u64,
       f16_buf_present = false,
       f16_buf = [],
       oftb_dim = dim }

def rsf_init (dim: i64) (num_layers: i64) : rsf_core =
  rsf_init_with_config dim num_layers default_config

def rsf_zero_gradients (core: rsf_core) : rsf_core =
  core with layers = map layer_zero_gradients core.layers

def validate_model_metadata (core: rsf_core) : bool =
  let layer_count = length core.layers
  in core.num_layers == layer_count
     && layer_count > 0
     && validate_model_config_values core.dim layer_count core.cfg
     && reduce (&&) true
          (map (\layer ->
                  layer.dim == core.dim
                  && layer.clip_min == core.cfg.clip_min
                  && layer.clip_max == core.cfg.clip_max
                  && layer.grad_mean == core.cfg.grad_mean
                  && validate_tensor_2d_shape core.dim core.dim (length layer.s_weight) core.dim core.dim
                  && validate_tensor_2d_shape core.dim core.dim (length layer.t_weight) core.dim core.dim
                  && validate_tensor_2d_shape 1 core.dim (length layer.s_bias) 1 core.dim
                  && validate_tensor_2d_shape 1 core.dim (length layer.t_bias) 1 core.dim) core.layers)

def oftb_mix (x1_row: []f32) (x2_row: []f32) : ([]f32, []f32) =
  let s = fractal_scale
  let n1 = map2 (\a b -> (a - b) * s) x1_row x2_row
  let n2 = map2 (\a b -> (a + b) * s) x1_row x2_row
  in (n1, n2)

def oftb_unmix (y1_row: []f32) (y2_row: []f32) : ([]f32, []f32) =
  let s = fractal_scale
  let n1 = map2 (\a b -> (a + b) * s) y1_row y2_row
  let n2 = map2 (\a b -> (b - a) * s) y1_row y2_row
  in (n1, n2)

def forward_on_core (core: rsf_core) (x: [][]f32) : [][]f32 =
  let dim = core.dim
  let dim2 = dim * 2
  let process_row (row: []f32) : [dim2]f32 =
    let r0 = row :> [dim2]f32
    let final =
      loop r = r0 for l < core.num_layers do
        let layer = core.layers[l]
        let x1_row = (take dim r) :> [dim]f32
        let x2_row = (drop dim r) :> [dim]f32
        let (x1_a, x2_a) = forward_row_pair layer x1_row x2_row
        let (x1_b, x2_b) = oftb_mix x1_a x2_a
        in (x1_b ++ x2_b) :> [dim2]f32
    in final
  in map process_row x

def inverse_on_core (core: rsf_core) (y: [][]f32) : [][]f32 =
  let dim = core.dim
  let dim2 = dim * 2
  let process_row (row: []f32) : [dim2]f32 =
    let r0 = row :> [dim2]f32
    let final =
      loop r = r0 for idx_rev < core.num_layers do
        let idx = core.num_layers - 1 - idx_rev
        let layer = core.layers[idx]
        let y1_row = (take dim r) :> [dim]f32
        let y2_row = (drop dim r) :> [dim]f32
        let (y1_a, y2_a) = oftb_unmix y1_row y2_row
        let (y1_b, y2_b) = inverse_row_pair layer y1_a y2_a
        in (y1_b ++ y2_b) :> [dim2]f32
    in final
  in map process_row y

def backward_on_core (core: rsf_core)
                     (grad_output: [][]f32)
                     (_input: [][]f32)
                     (output: [][]f32) : (rsf_core, [][]f32) =
  let dim = core.dim
  let dim2 = dim * 2
  let batch_size = length output
  let core_eg = core with layers = map layer_ensure_gradients core.layers
  let grad_scale =
    if ! core.cfg.grad_mean then 1.0f32
    else
      let s = 1.0f32 / f32.i64 batch_size
      in if is_finite_f32 s then s else 1.0f32

  let process_batch (b: i64) (acc_layers: []layer_core)
                    : ([]layer_core, [dim2]f32) =
    let out_row = output[b] :> [dim2]f32
    let grad_row = grad_output[b] :> [dim2]f32
    let y1_init = (take dim out_row) :> [dim]f32
    let y2_init = (drop dim out_row) :> [dim]f32
    let dy1_init = (take dim grad_row) :> [dim]f32
    let dy2_init = (drop dim grad_row) :> [dim]f32
    let (final_layers, final_dy1, final_dy2, _, _) =
      loop (lyrs, dy1, dy2, y1, y2) =
            (acc_layers, dy1_init, dy2_init, y1_init, y2_init)
        for idx_rev < core.num_layers do
          let idx = core.num_layers - 1 - idx_rev
          let (y1_u, y2_u) = oftb_unmix y1 y2
          let (dy1_u, dy2_u) = oftb_unmix dy1 dy2
          let layer = lyrs[idx]
          let res = backward_from_outputs_row layer y1_u y2_u dy1_u dy2_u grad_scale
          let updated_layer =
            accumulate_layer_grads layer res.s_w_delta res.t_w_delta
                                   res.s_b_delta res.t_b_delta
          let lyrs2 = lyrs with [idx] = updated_layer
          let nx1 = res.x1_row :> [dim]f32
          let nx2 = res.x2_row :> [dim]f32
          let ndx1 = res.dx1_row :> [dim]f32
          let ndx2 = res.dx2_row :> [dim]f32
          in (lyrs2, ndx1, ndx2, nx1, nx2)
    in (final_layers, (final_dy1 ++ final_dy2) :> [dim2]f32)

  let init_rows = replicate batch_size (replicate dim2 0.0f32)
  let (final_layers, grad_rows) =
    loop (lyrs, rows) = (core_eg.layers, init_rows)
      for b < batch_size do
        let (lyrs', row) = process_batch b lyrs
        in (lyrs', rows with [b] = row)
  in (core_eg with layers = final_layers, grad_rows)

def layer_gpu_compatible (lc: layer_core) (cfg: rsf_config) (dim: i64) : bool =
  lc.dim == dim
  && lc.clip_min == cfg.clip_min
  && lc.clip_max == cfg.clip_max
  && lc.grad_mean == cfg.grad_mean
  && lc.clip_min == -5.0f32
  && lc.clip_max == 5.0f32

def model_gpu_compatible (core: rsf_core) : bool =
  gpu_enabled
  && core.num_layers > 0
  && reduce (&&) true (map (\l -> layer_gpu_compatible l core.cfg core.dim) core.layers)

def disable_gpu (core: rsf_core) : rsf_core =
  core with gpu_available = 0u8
       with gpu_weight_version = 0u64
       with f16_buf_present = false
       with f16_buf = []

def validate_f16_convertible [n] (data: [n]f32) : bool =
  let max_f16 = 65504.0f32
  in reduce (&&) true
       (map (\v -> is_finite_f32 v && f32.abs v <= max_f16) data)

def sync_all_layers_gpu (core: rsf_core) : rsf_core =
  if ! gpu_enabled then disable_gpu core
  else if ! (validate_model_metadata core) then disable_gpu core
  else if ! (model_gpu_compatible core) then disable_gpu core
  else
    let all_ok =
      reduce (&&) true
        (map (\l ->
                ensure_finite_slice l.s_weight
                && ensure_finite_slice l.t_weight
                && ensure_finite_slice l.s_bias
                && ensure_finite_slice l.t_bias
                && validate_f16_convertible l.s_weight
                && validate_f16_convertible l.t_weight
                && validate_f16_convertible l.s_bias
                && validate_f16_convertible l.t_bias) core.layers)
    in if ! all_ok then disable_gpu core
       else
         let dim_sq = core.dim * core.dim
         in core with gpu_available = 1u8
                with gpu_weight_version = core.cpu_weight_version
                with f16_buf_present = true
                with f16_buf = zero_array dim_sq

def try_forward_gpu (core: rsf_core) (x: [][]f32) : (bool, rsf_core, [][]f32) =
  if ! gpu_enabled then (false, core, x)
  else if ! (model_gpu_compatible core) then (false, disable_gpu core, x)
  else if core.gpu_available == 0u8 then (false, core, x)
  else if core.gpu_weight_version != core.cpu_weight_version then (false, core, x)
  else (true, core, forward_on_core core x)

def is_gpu_available (core: rsf_core) : bool =
  model_gpu_compatible core
  && core.gpu_available != 0u8
  && core.gpu_weight_version == core.cpu_weight_version

def rsf_forward_cpu (core: rsf_core) (x: [][]f32) : [][]f32 =
  forward_on_core core x

def rsf_forward (core: rsf_core) (x: [][]f32) : (rsf_core, [][]f32) =
  let dim = core.dim
  let dim2 = dim * 2
  let valid =
    length x > 0 && length x[0] == dim2
  in if ! valid then (core, x)
     else if gpu_enabled then
       let needs_write = core.gpu_available != 0u8 || model_gpu_compatible core
       in if needs_write then
            if model_gpu_compatible core then
              let (ok1, c1, r1) = try_forward_gpu core x
              in if ok1 then (c1, r1)
                 else
                   let c2 = sync_all_layers_gpu c1
                   let (ok2, c3, r2) = try_forward_gpu c2 x
                   in if ok2 then (c3, r2)
                      else (c3, forward_on_core c3 x)
            else if core.gpu_available != 0u8 || core.gpu_weight_version != 0u64
            then let c = disable_gpu core in (c, forward_on_core c x)
            else (core, forward_on_core core x)
          else (core, forward_on_core core x)
     else (core, forward_on_core core x)

def rsf_inverse (core: rsf_core) (y: [][]f32) : [][]f32 =
  inverse_on_core core y

def rsf_backward (core: rsf_core)
                 (grad_output: [][]f32)
                 (input: [][]f32)
                 (output: [][]f32) : (rsf_core, [][]f32) =
  backward_on_core core grad_output input output

def rsf_notify_weights_changed (core: rsf_core) : rsf_core =
  core with cpu_weight_version = core.cpu_weight_version + 1u64

def rsf_sync_weights_to_gpu (core: rsf_core) : rsf_core =
  sync_all_layers_gpu core

def rsf_verify_invertible (core: rsf_core) (x: [][]f32) (abs_tol: f32) (rel_tol: f32) : bool =
  if ! (validate_comparison_tolerances abs_tol rel_tol) then false
  else
    let y = forward_on_core core x
    let z = inverse_on_core core y
    let fx = flatten x
    let fz = flatten z
    let n = length fx
    let m = length fz
    in n == m && tensor_all_close_eq (fx :> [n]f32) (fz :> [n]f32) abs_tol rel_tol

type~ saved_layer_snapshot = {
  clip_min: f32,
  clip_max: f32,
  grad_mean: bool,
  s_weight: []f32,
  t_weight: []f32,
  s_bias: []f32,
  t_bias: []f32,
  dim: i64
}

type~ saved_model_snapshot = {
  dim: i64,
  num_layers: i64,
  cfg: rsf_config,
  layers: []saved_layer_snapshot
}

def snapshot_model_for_save (core: rsf_core) : (bool, saved_model_snapshot) =
  if ! (validate_model_metadata core) then
    (false, { dim = 0, num_layers = 0, cfg = default_config, layers = [] })
  else
    let all_finite =
      reduce (&&) true
        (map (\l ->
                validate_clip_range l.clip_min l.clip_max
                && ensure_finite_slice l.s_weight
                && ensure_finite_slice l.t_weight
                && ensure_finite_slice l.s_bias
                && ensure_finite_slice l.t_bias) core.layers)
    in if ! all_finite then
         (false, { dim = 0, num_layers = 0, cfg = default_config, layers = [] })
       else
         let layers =
           map (\l ->
                  { clip_min = l.clip_min,
                    clip_max = l.clip_max,
                    grad_mean = l.grad_mean,
                    s_weight = copy l.s_weight,
                    t_weight = copy l.t_weight,
                    s_bias = copy l.s_bias,
                    t_bias = copy l.t_bias,
                    dim = l.dim }) core.layers
         in (true, { dim = core.dim,
                     num_layers = core.num_layers,
                     cfg = core.cfg,
                     layers = layers })

def crc32_table : [256]u32 =
  map (\i ->
         loop c = u32.i64 i for _k < 8 do
           if (c & 1u32) != 0u32
           then (c >> 1u32) ^ 0xEDB88320u32
           else c >> 1u32) (iota 256)

def crc32_init : u32 = 0xFFFFFFFFu32

def crc32_update_bytes [n] (crc: u32) (data: [n]u8) : u32 =
  loop c = crc for i < n do
    let idx = i64.u32 ((c ^ u32.u8 data[i]) & 0xFFu32)
    in (c >> 8u32) ^ crc32_table[idx]

def crc32_finalize (crc: u32) : u32 = crc ^ 0xFFFFFFFFu32

def u32_to_le_bytes (v: u32) : [4]u8 =
  [u8.u32 (v & 0xFFu32),
   u8.u32 ((v >> 8u32) & 0xFFu32),
   u8.u32 ((v >> 16u32) & 0xFFu32),
   u8.u32 ((v >> 24u32) & 0xFFu32)]

def u64_to_le_bytes (v: u64) : [8]u8 =
  [u8.u64 (v & 0xFFu64),
   u8.u64 ((v >> 8u64) & 0xFFu64),
   u8.u64 ((v >> 16u64) & 0xFFu64),
   u8.u64 ((v >> 24u64) & 0xFFu64),
   u8.u64 ((v >> 32u64) & 0xFFu64),
   u8.u64 ((v >> 40u64) & 0xFFu64),
   u8.u64 ((v >> 48u64) & 0xFFu64),
   u8.u64 ((v >> 56u64) & 0xFFu64)]

def f32_to_u32_bits (v: f32) : u32 = f32.to_bits v
def u32_bits_to_f32 (v: u32) : f32 = f32.from_bits v

def crc_update_u32_le (crc: u32) (v: u32) : u32 =
  crc32_update_bytes crc (u32_to_le_bytes v)

def crc_update_u64_le (crc: u32) (v: u64) : u32 =
  crc32_update_bytes crc (u64_to_le_bytes v)

def crc_update_u8 (crc: u32) (v: u8) : u32 =
  crc32_update_bytes crc [v]

def magic_bytes : [4]u8 = [82u8, 83u8, 70u8, 48u8]

def hash_tensor_data_version4 [n] (crc: u32) (rows: i64) (cols: i64) (data: [n]f32) : u32 =
  let c1 = crc_update_u64_le crc 2u64
  let c2 = crc_update_u64_le c1 (u64.i64 rows)
  let c3 = crc_update_u64_le c2 (u64.i64 cols)
  in loop c = c3 for i < n do
       crc_update_u32_le c (f32_to_u32_bits data[i])

def compute_save_checksum (snapshot: saved_model_snapshot) : u32 =
  let c0 = crc32_update_bytes crc32_init magic_bytes
  let c1 = crc_update_u32_le c0 save_version
  let c2 = crc_update_u64_le c1 (u64.i64 snapshot.num_layers)
  let c3 = crc_update_u64_le c2 (u64.i64 snapshot.dim)
  let c4 = crc_update_u32_le c3 (f32_to_u32_bits snapshot.cfg.clip_min)
  let c5 = crc_update_u32_le c4 (f32_to_u32_bits snapshot.cfg.clip_max)
  let c6 = crc_update_u8 c5 (if snapshot.cfg.grad_mean then 1u8 else 0u8)
  let c7 = crc_update_u64_le c6 (u64.i64 snapshot.cfg.max_dim)
  let c8 = crc_update_u64_le c7 (u64.i64 snapshot.cfg.max_layers)
  let cf =
    loop c = c8 for i < snapshot.num_layers do
      let layer = snapshot.layers[i]
      let ca = crc_update_u32_le c (f32_to_u32_bits layer.clip_min)
      let cb = crc_update_u32_le ca (f32_to_u32_bits layer.clip_max)
      let cc = crc_update_u8 cb (if layer.grad_mean then 1u8 else 0u8)
      let cd = hash_tensor_data_version4 cc layer.dim layer.dim layer.s_weight
      let ce = hash_tensor_data_version4 cd layer.dim layer.dim layer.t_weight
      let cg = hash_tensor_data_version4 ce 1 layer.dim layer.s_bias
      in hash_tensor_data_version4 cg 1 layer.dim layer.t_bias
  in crc32_finalize cf

def serialize_tensor_v4 (rows: i64) (cols: i64) (data: []f32) : []u8 =
  let header_dims = u64_to_le_bytes 2u64
  let header_r = u64_to_le_bytes (u64.i64 rows)
  let header_c = u64_to_le_bytes (u64.i64 cols)
  let body = flatten (map (\v -> u32_to_le_bytes (f32_to_u32_bits v)) data)
  in header_dims ++ header_r ++ header_c ++ body

def serialize_snapshot (snapshot: saved_model_snapshot) : []u8 =
  let version_b = u32_to_le_bytes save_version
  let nl_b = u64_to_le_bytes (u64.i64 snapshot.num_layers)
  let dim_b = u64_to_le_bytes (u64.i64 snapshot.dim)
  let cmin_b = u32_to_le_bytes (f32_to_u32_bits snapshot.cfg.clip_min)
  let cmax_b = u32_to_le_bytes (f32_to_u32_bits snapshot.cfg.clip_max)
  let gm_b = [if snapshot.cfg.grad_mean then 1u8 else 0u8]
  let md_b = u64_to_le_bytes (u64.i64 snapshot.cfg.max_dim)
  let ml_b = u64_to_le_bytes (u64.i64 snapshot.cfg.max_layers)
  let layers_b =
    flatten (map (\layer ->
                    let lmin = u32_to_le_bytes (f32_to_u32_bits layer.clip_min)
                    let lmax = u32_to_le_bytes (f32_to_u32_bits layer.clip_max)
                    let lgm = [if layer.grad_mean then 1u8 else 0u8]
                    let sw = serialize_tensor_v4 layer.dim layer.dim layer.s_weight
                    let tw = serialize_tensor_v4 layer.dim layer.dim layer.t_weight
                    let sb = serialize_tensor_v4 1 layer.dim layer.s_bias
                    let tb = serialize_tensor_v4 1 layer.dim layer.t_bias
                    in lmin ++ lmax ++ lgm ++ sw ++ tw ++ sb ++ tb) snapshot.layers)
  let checksum = compute_save_checksum snapshot
  let csum_b = u32_to_le_bytes checksum
  in magic_bytes ++ version_b ++ nl_b ++ dim_b
     ++ cmin_b ++ cmax_b ++ gm_b
     ++ md_b ++ ml_b
     ++ layers_b ++ csum_b

def rsf_save (core: rsf_core) : (bool, []u8) =
  let (ok, snapshot) = snapshot_model_for_save core
  in if ! ok then (false, [])
     else (true, serialize_snapshot snapshot)

def read_u8_at (data: []u8) (offset: i64) : (i64, u8) =
  (offset + 1, data[offset])

def read_u32_le (data: []u8) (offset: i64) : (i64, u32) =
  let v = u32.u8 data[offset]
          | (u32.u8 data[offset + 1] << 8u32)
          | (u32.u8 data[offset + 2] << 16u32)
          | (u32.u8 data[offset + 3] << 24u32)
  in (offset + 4, v)

def read_u64_le (data: []u8) (offset: i64) : (i64, u64) =
  let v = u64.u8 data[offset]
          | (u64.u8 data[offset + 1] << 8u64)
          | (u64.u8 data[offset + 2] << 16u64)
          | (u64.u8 data[offset + 3] << 24u64)
          | (u64.u8 data[offset + 4] << 32u64)
          | (u64.u8 data[offset + 5] << 40u64)
          | (u64.u8 data[offset + 6] << 48u64)
          | (u64.u8 data[offset + 7] << 56u64)
  in (offset + 8, v)

def read_tensor_v4 (data: []u8) (offset: i64) : (bool, i64, i64, i64, []f32) =
  let (o1, dims) = read_u64_le data offset
  in if dims != 2u64 then (false, offset, 0, 0, [])
     else
       let (o2, r) = read_u64_le data o1
       let (o3, c) = read_u64_le data o2
       let (ok_r, ri) = checked_cast_u64_to_i64 r
       let (ok_c, ci) = checked_cast_u64_to_i64 c
       in if ! ok_r || ! ok_c then (false, offset, 0, 0, [])
          else
            let (ok_m, total) = checked_mul_i64 ri ci
            in if ! ok_m then (false, offset, 0, 0, [])
               else
                 let values =
                   map (\i ->
                          let base = o3 + i * 4
                          let bits = u32.u8 data[base]
                                     | (u32.u8 data[base + 1] << 8u32)
                                     | (u32.u8 data[base + 2] << 16u32)
                                     | (u32.u8 data[base + 3] << 24u32)
                          in u32_bits_to_f32 bits) (iota total)
                 in (true, o3 + total * 4, ri, ci, values)

def deserialize_snapshot (data: []u8) (policy_max_dim: i64) (policy_max_layers: i64)
                         : (bool, saved_model_snapshot) =
  let empty : saved_model_snapshot =
    { dim = 0, num_layers = 0, cfg = default_config, layers = [] }
  in if length data < 4 then (false, empty)
     else if data[0] != magic_bytes[0] || data[1] != magic_bytes[1]
          || data[2] != magic_bytes[2] || data[3] != magic_bytes[3] then (false, empty)
     else
       let (o1, version) = read_u32_le data 4
       in if version != save_version then (false, empty)
          else
            let (o2, num_layers_u64) = read_u64_le data o1
            let (o3, dim_u64) = read_u64_le data o2
            in if num_layers_u64 == 0u64 || dim_u64 == 0u64 then (false, empty)
               else
                 let (ok_nl, num_layers) = checked_cast_u64_to_i64 num_layers_u64
                 let (ok_d, dim) = checked_cast_u64_to_i64 dim_u64
                 in if ! ok_nl || ! ok_d then (false, empty)
                    else if num_layers > policy_max_layers || dim > policy_max_dim then (false, empty)
                    else
                      let (o4, clip_min_bits) = read_u32_le data o3
                      let (o5, clip_max_bits) = read_u32_le data o4
                      let clip_min = u32_bits_to_f32 clip_min_bits
                      let clip_max = u32_bits_to_f32 clip_max_bits
                      let (o6, gm_byte) = read_u8_at data o5
                      in if gm_byte > 1u8 then (false, empty)
                         else
                           let grad_mean = gm_byte == 1u8
                           in if ! (validate_clip_range clip_min clip_max) then (false, empty)
                              else
                                let (o7, max_dim_u64) = read_u64_le data o6
                                let (o8, max_layers_u64) = read_u64_le data o7
                                in if max_dim_u64 == 0u64 || max_layers_u64 == 0u64 then (false, empty)
                                   else if max_dim_u64 < dim_u64 || max_layers_u64 < num_layers_u64 then (false, empty)
                                   else
                                     let (ok_md, max_dim) = checked_cast_u64_to_i64 max_dim_u64
                                     let (ok_ml, max_layers) = checked_cast_u64_to_i64 max_layers_u64
                                     in if ! ok_md || ! ok_ml then (false, empty)
                                        else
                                          let cfg : rsf_config =
                                            { clip_min = clip_min,
                                              clip_max = clip_max,
                                              grad_mean = grad_mean,
                                              max_dim = max_dim,
                                              max_layers = max_layers }
                                          in if ! (validate_model_config_values dim num_layers cfg) then (false, empty)
                                             else
                                               let init_layer : saved_layer_snapshot =
                                                 { clip_min = 0.0f32,
                                                   clip_max = 0.0f32,
                                                   grad_mean = false,
                                                   s_weight = [],
                                                   t_weight = [],
                                                   s_bias = [],
                                                   t_bias = [],
                                                   dim = 0 }
                                               let init_layers = replicate num_layers init_layer
                                               let (final_ok, final_offset, final_layers) =
                                                 loop (ok_acc, off_acc, lyrs_acc) = (true, o8, init_layers)
                                                   for i < num_layers do
                                                     if ! ok_acc then (false, off_acc, lyrs_acc)
                                                     else
                                                       let (oA, lmin_bits) = read_u32_le data off_acc
                                                       let (oB, lmax_bits) = read_u32_le data oA
                                                       let lmin = u32_bits_to_f32 lmin_bits
                                                       let lmax = u32_bits_to_f32 lmax_bits
                                                       let (oC, lgm_byte) = read_u8_at data oB
                                                       in if lgm_byte > 1u8 then (false, oC, lyrs_acc)
                                                          else
                                                            let lgm = lgm_byte == 1u8
                                                            in if ! (validate_clip_range lmin lmax) then (false, oC, lyrs_acc)
                                                               else if lmin != clip_min || lmax != clip_max || lgm != grad_mean then (false, oC, lyrs_acc)
                                                               else
                                                                 let (ok_sw, oD, rsw, csw, sw) = read_tensor_v4 data oC
                                                                 in if ! ok_sw || rsw != dim || csw != dim then (false, oD, lyrs_acc)
                                                                    else
                                                                      let (ok_tw, oE, rtw, ctw, tw) = read_tensor_v4 data oD
                                                                      in if ! ok_tw || rtw != dim || ctw != dim then (false, oE, lyrs_acc)
                                                                         else
                                                                           let (ok_sb, oF, rsb, csb, sb) = read_tensor_v4 data oE
                                                                           in if ! ok_sb || rsb != 1 || csb != dim then (false, oF, lyrs_acc)
                                                                              else
                                                                                let (ok_tb, oG, rtb, ctb, tb) = read_tensor_v4 data oF
                                                                                in if ! ok_tb || rtb != 1 || ctb != dim then (false, oG, lyrs_acc)
                                                                                   else if ! (ensure_finite_slice sw) || ! (ensure_finite_slice tw)
                                                                                           || ! (ensure_finite_slice sb) || ! (ensure_finite_slice tb) then (false, oG, lyrs_acc)
                                                                                   else
                                                                                     let layer : saved_layer_snapshot =
                                                                                       { clip_min = lmin,
                                                                                         clip_max = lmax,
                                                                                         grad_mean = lgm,
                                                                                         s_weight = sw,
                                                                                         t_weight = tw,
                                                                                         s_bias = sb,
                                                                                         t_bias = tb,
                                                                                         dim = dim }
                                                                                     in (true, oG, lyrs_acc with [i] = layer)
                                               in if ! final_ok then (false, empty)
                                                  else if final_offset + 4 > length data then (false, empty)
                                                  else
                                                    let (oFinal, expected_crc) = read_u32_le data final_offset
                                                    let snapshot : saved_model_snapshot =
                                                      { dim = dim,
                                                        num_layers = num_layers,
                                                        cfg = cfg,
                                                        layers = final_layers }
                                                    let actual_crc = compute_save_checksum snapshot
                                                    in if expected_crc != actual_crc then (false, empty)
                                                       else if oFinal != length data then (false, empty)
                                                       else (true, snapshot)

def core_from_snapshot (snapshot: saved_model_snapshot) : rsf_core =
  let layers =
    map (\sl ->
           let dim_sq = sl.dim * sl.dim
           in { dim = sl.dim,
                s_weight = sl.s_weight,
                t_weight = sl.t_weight,
                s_bias = sl.s_bias,
                t_bias = sl.t_bias,
                s_weight_grad = zero_array dim_sq,
                t_weight_grad = zero_array dim_sq,
                s_bias_grad = zero_array sl.dim,
                t_bias_grad = zero_array sl.dim,
                has_s_weight_grad = false,
                has_t_weight_grad = false,
                has_s_bias_grad = false,
                has_t_bias_grad = false,
                clip_min = sl.clip_min,
                clip_max = sl.clip_max,
                grad_mean = sl.grad_mean }) snapshot.layers
  in { dim = snapshot.dim,
       num_layers = snapshot.num_layers,
       layers = layers,
       cfg = snapshot.cfg,
       gpu_available = 0u8,
       gpu_weight_version = 0u64,
       cpu_weight_version = 1u64,
       f16_buf_present = false,
       f16_buf = [],
       oftb_dim = snapshot.dim }

def rsf_load (data: []u8) : (bool, rsf_core) =
  rsf_load_with_config data default_config.max_dim default_config.max_layers

def rsf_load_with_config (data: []u8) (max_dim: i64) (max_layers: i64) : (bool, rsf_core) =
  let (ok, snapshot) = deserialize_snapshot data max_dim max_layers
  in if ! ok then (false, rsf_init 1 1)
     else
       let core = core_from_snapshot snapshot
       in if ! (validate_model_metadata core) then (false, rsf_init 1 1)
          else
            let core' =
              if model_gpu_compatible core then sync_all_layers_gpu core
              else core
            in (true, core')

def rsf_save_load_roundtrip (core: rsf_core) (abs_tol: f32) (rel_tol: f32) : bool =
  if ! (validate_comparison_tolerances abs_tol rel_tol) then false
  else
    let (ok_save, bytes) = rsf_save core
    in if ! ok_save then false
       else
         let (ok_load, loaded) = rsf_load bytes
         in if ! ok_load then false
            else if core.dim != loaded.dim then false
            else if core.num_layers != loaded.num_layers then false
            else if core.cfg.clip_min != loaded.cfg.clip_min
                    || core.cfg.clip_max != loaded.cfg.clip_max
                    || core.cfg.grad_mean != loaded.cfg.grad_mean then false
            else if core.cfg.max_dim != loaded.cfg.max_dim
                    || core.cfg.max_layers != loaded.cfg.max_layers then false
            else
              reduce (&&) true
                (map (\i ->
                        let a = core.layers[i]
                        let b = loaded.layers[i]
                        let na = length a.s_weight
                        let nb = length b.s_weight
                        let ma = length a.s_bias
                        let mb = length b.s_bias
                        in a.clip_min == b.clip_min
                           && a.clip_max == b.clip_max
                           && a.grad_mean == b.grad_mean
                           && na == nb && ma == mb
                           && tensor_all_close_eq (a.s_weight :> [na]f32) (b.s_weight :> [na]f32) abs_tol rel_tol
                           && tensor_all_close_eq (a.t_weight :> [na]f32) (b.t_weight :> [na]f32) abs_tol rel_tol
                           && tensor_all_close_eq (a.s_bias :> [ma]f32) (b.s_bias :> [ma]f32) abs_tol rel_tol
                           && tensor_all_close_eq (a.t_bias :> [ma]f32) (b.t_bias :> [ma]f32) abs_tol rel_tol) (iota core.num_layers))

entry run_layer_forward_inverse_test : bool =
  let dim : i64 = 32
  let batch : i64 = 4
  let layer = layer_init_owned dim default_layer_config
  let flat1 = random_uniform_array (batch * dim) (-1.0f32) 1.0f32 99u64
  let flat2 = random_uniform_array (batch * dim) (-1.0f32) 1.0f32 100u64
  let x1 = unflatten (flat1 :> [batch * dim]f32) :> [batch][dim]f32
  let x2 = unflatten (flat2 :> [batch * dim]f32) :> [batch][dim]f32
  let c1 = copy x1
  let c2 = copy x2
  let (f1, f2) = layer_forward_in_place layer x1 x2
  let (b1, b2) = layer_inverse_in_place layer f1 f2
  let fc1 = flatten c1
  let fc2 = flatten c2
  let fb1 = flatten b1
  let fb2 = flatten b2
  let n = batch * dim
  in tensor_all_close_eq (fc1 :> [n]f32) (fb1 :> [n]f32) 1e-4f32 0.0f32
     && tensor_all_close_eq (fc2 :> [n]f32) (fb2 :> [n]f32) 1e-4f32 0.0f32

entry run_oftb_forward_inverse_test : bool =
  let dim : i64 = 16
  let num_layers : i64 = 4
  let batch : i64 = 2
  let core = rsf_init_with_config dim num_layers default_config
  let input =
    map (\b ->
           random_uniform_array (dim * 2) (-0.5f32) 0.5f32 (u64.i64 b + 7u64)) (iota batch)
  let original = map copy input
  let forward_out = forward_on_core core input
  let inverse_out = inverse_on_core core forward_out
  let fo = flatten original
  let fi = flatten inverse_out
  let n = length fo
  let m = length fi
  in n == m && tensor_all_close_eq (fo :> [n]f32) (fi :> [n]f32) 1e-4f32 0.0f32

entry forward (dim: i64) (num_layers: i64) (x: [][]f32) : [][]f32 =
  let core = rsf_init dim num_layers
  in forward_on_core core x

entry inverse (dim: i64) (num_layers: i64) (y: [][]f32) : [][]f32 =
  let core = rsf_init dim num_layers
  in inverse_on_core core y

entry forward_with_config (dim: i64) (num_layers: i64)
                          (clip_min: f32) (clip_max: f32) (grad_mean: bool)
                          (max_dim: i64) (max_layers: i64)
                          (x: [][]f32) : [][]f32 =
  let cfg : rsf_config =
    { clip_min = clip_min, clip_max = clip_max, grad_mean = grad_mean,
      max_dim = max_dim, max_layers = max_layers }
  let core = rsf_init_with_config dim num_layers cfg
  in forward_on_core core x

entry inverse_with_config (dim: i64) (num_layers: i64)
                          (clip_min: f32) (clip_max: f32) (grad_mean: bool)
                          (max_dim: i64) (max_layers: i64)
                          (y: [][]f32) : [][]f32 =
  let cfg : rsf_config =
    { clip_min = clip_min, clip_max = clip_max, grad_mean = grad_mean,
      max_dim = max_dim, max_layers = max_layers }
  let core = rsf_init_with_config dim num_layers cfg
  in inverse_on_core core y

entry verify_invertible (dim: i64) (num_layers: i64) (x: [][]f32)
                        (abs_tol: f32) (rel_tol: f32) : bool =
  let core = rsf_init dim num_layers
  in rsf_verify_invertible core x abs_tol rel_tol

entry save_model (dim: i64) (num_layers: i64) : (bool, []u8) =
  let core = rsf_init dim num_layers
  in rsf_save core

entry load_model (data: []u8) : (bool, i64, i64) =
  let (ok, core) = rsf_load data
  in (ok, core.dim, core.num_layers)

entry save_load_roundtrip (dim: i64) (num_layers: i64) (abs_tol: f32) (rel_tol: f32) : bool =
  let core = rsf_init dim num_layers
  in rsf_save_load_roundtrip core abs_tol rel_tol

entry backward (dim: i64) (num_layers: i64)
               (grad_output: [][]f32) (input: [][]f32) (output: [][]f32)
               : [][]f32 =
  let core = rsf_init dim num_layers
  let (_, grad_input) = backward_on_core core grad_output input output
  in grad_input

entry gpu_available (dim: i64) (num_layers: i64) : bool =
  let core = rsf_init dim num_layers
  in is_gpu_available core
