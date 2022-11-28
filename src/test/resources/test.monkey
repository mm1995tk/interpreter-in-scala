let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y
};

let twice = fn(f) {
  return fn (x) {
    f(f(x))
  };
};

let addTwo = fn(x) {
  return x + 2;
};

let result = twice(addTwo)(five);

if (result > 7) {
  return result;
} else {
  return null;
}

