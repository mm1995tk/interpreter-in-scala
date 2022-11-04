package obj

enum Object:
  def get = this match
    case Int(value)     => Some(value)
    case Boolean(value) => Some(value)
    case Null           => None

  case Int(value: scala.Int)
  case Boolean(value: scala.Boolean)
  case Null
