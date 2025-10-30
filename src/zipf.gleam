import gleam/int
import gleam/list

// Integer-power Zipf sampler (deterministic, seeded).
// s: integer exponent (>= 0). Uses inverse-transform sampling with floats but
// computes powers using integer multiplication to avoid missing float pow helpers.

fn int_pow(base: Int, exp: Int) -> Int {
  case exp <= 0 { True -> 1 False -> list.fold(list.range(1, exp), 1, fn(acc, _) { acc * base }) }
}

fn lcg_uniform(seed: Int) -> Float {
  // Constants from Numerical Recipes
  let a = 1664525
  let c = 1013904223
  let m = 4294967296 // 2^32
  let next = {a * seed + c} % m
  int.to_float(next) /. int.to_float(m)
}

fn pow_inverse_float(k: Int, s: Int) -> Float {
  let denom = int_pow(k, s)
  1.0 /. int.to_float(denom)
}

fn harmonic_norm(n: Int, s: Int) -> Float {
  list.fold(list.range(1, n), 0.0, fn(acc, i) { acc +. pow_inverse_float(i, s) })
}

pub fn sample_zipf_seed(s: Int, max: Int, seed: Int) -> Int {
  let norm = harmonic_norm(max, s)
  let u = lcg_uniform(seed)
  sample_zipf_rec(1, 0.0, max, norm, u, s)
}

fn sample_zipf_rec(i: Int, acc: Float, max: Int, norm: Float, u: Float, s: Int) -> Int {
  case i > max {
    True -> max
    False -> {
      let p = pow_inverse_float(i, s) /. norm
      let acc2 = acc +. p
      case acc2 >=. u {
        True -> i
        False -> sample_zipf_rec(i + 1, acc2, max, norm, u, s)
      }
    }
  }
}

pub fn sample_zipf(s: Int, max: Int) -> Int {
  sample_zipf_seed(s, max, 1)
}
