import gleam/int
import gleam/list

// Integer-power Zipf sampler (deterministic, seeded).
// FIXED VERSION: Better random distribution

fn int_pow(base: Int, exp: Int) -> Int {
  case exp <= 0 {
    True -> 1
    False -> base * int_pow(base, exp - 1)
  }
}

// Improved LCG with better mixing to avoid all users getting rank 1
fn lcg_uniform(seed: Int) -> Float {
  // Use a combination of transformations for better distribution
  let seed1 = { seed * 48271 } % 2147483647
  let seed2 = { seed * 69621 + 12345 } % 2147483647
  let mixed = { seed1 + seed2 } % 2147483647

  // Additional mixing to spread values
  let final_seed = { mixed * 1103515245 + 12345 } % 2147483647

  int.to_float(final_seed) /. 2147483647.0
}

fn pow_inverse_float(k: Int, s: Int) -> Float {
  let denom = int_pow(k, s)
  1.0 /. int.to_float(denom)
}

fn harmonic_norm(n: Int, s: Int) -> Float {
  list.fold(list.range(1, n), 0.0, fn(acc, i) { acc +. pow_inverse_float(i, s) })
}

pub fn sample_zipf_seed(s: Int, max: Int, seed: Int) -> Int {
  // Critical fix: Use seed directly without modification to ensure variety
  let norm = harmonic_norm(max, s)
  let u = lcg_uniform(seed)

  // Add debug trace for first few seeds
  // io.debug(#("seed", seed, "u", u, "max", max))

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

// Calculate the probability mass function for a given rank in Zipf distribution
pub fn zipf_probability(rank: Int, s: Int, max: Int) -> Float {
  let norm = harmonic_norm(max, s)
  pow_inverse_float(rank, s) /. norm
}

// Get the expected frequency for a rank (normalized to 0-1 range)
pub fn zipf_frequency(rank: Int, s: Int, max: Int) -> Float {
  zipf_probability(rank, s, max)
}