let fibonacci = fn(x) {
  if (x == 0) {
    0
  } else {
    if (x == 1) {
       1
    } else {
       fibonacci(x - 1) + fibonacci(x - 2)
    }
  }
};
puts("fibonacci(10)", fibonacci(10));
puts("fibonacci(15)", fibonacci(15));
