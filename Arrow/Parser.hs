
module Alt.Arrow.Parser where


newtype ArrowParser e f a b = ArrowParser {
	arrowParser :: f a (ParseStatus e b)
}

data ParseStatus e r
  = OkConsumed r
  | FailSkip e
  | OkSkip r
  | FailConsumed e
