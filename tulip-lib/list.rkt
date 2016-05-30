#lang tulip

map f .nil         = .nil
map f (.cons x xs) = .cons (f x) (map f xs)

foldl f acc .nil         = acc
foldl f acc (.cons x xs) = foldl f (f acc x) xs

foldr f acc .nil         = acc
foldr f acc (.cons x xs) = f (foldr f acc xs) x

reverse lst = {
  go acc .nil         = acc
  go acc (.cons x xs) = go (.cons x acc) xs
  go .nil lst
}
