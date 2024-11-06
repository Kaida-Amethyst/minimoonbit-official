/// ------------------------------------------------
/// Deque
/// ------------------------------------------------
struct Deque[T] {
  data: Array[T]
  mut front: Int
  mut rear: Int
}

fn Deque::new[T]() -> Deque[T] {
  Deque::{
    data: Array::new(),
    front: 0,
    rear: 0
  }
}

fn Deque::is_empty[T](deque: Deque[T]) -> Bool {
  deque.front == deque.rear
}

fn Deque::clear[T](deque: Deque[T]) -> Deque[T] {
  deque.front = 0
  deque.rear = 0
  deque
}

fn Deque::push_back[T](deque: Deque[T], value: T) -> Unit {
  deque.data.push(value)
  deque.rear += 1
}

fn op_get[T](self: Deque[T], index: Int) -> T {
  self.data[self.front + index]
}

fn Deque::pop_front[T](deque: Deque[T]) -> T? {
  if Deque::is_empty(deque) {
    None
  } else {
    let value = deque.data[deque.front]
    deque.front += 1
    Some(value)
  }
}
