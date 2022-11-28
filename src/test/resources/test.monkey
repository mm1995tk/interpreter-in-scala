let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y
};

let twice = fn(f) {
  fn (x) {
    f(f(x))
  }
};

let addTwo = fn(x) {
  return x + 2;
};

let g = twice(addTwo);

let result = g(five);

if (result > 7) {
  return result;
} else {
  return null;
}
