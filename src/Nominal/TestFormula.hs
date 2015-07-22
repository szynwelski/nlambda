module Nominal.TestFormula where

import Nominal.Formula
import Nominal.Type
import Nominal.Variable
import Prelude hiding (not)
import Test.HUnit

x = variable "x"
y = variable "y"
z = variable "z"
xy = eq x y
xz = eq x z
yz = eq y z
lt = lessThan
le = lessEquals
gt = greaterThan
ge = greaterEquals

x1 = variable "x1"
x2 = variable "x2"
x3 = variable "x3"
x4 = variable "x4"
x5 = variable "x5"
x6 = variable "x6"
f1 = existsVar x2 $ eq x1 x2 \/ eq x2 x3
f2 = existsVar x5 $ eq x4 x5 \/ eq x5 x6

tests = test [true                   ~=?    eq x x,
              eq x y                 ~=?    eq y x,
              false                  ~=?    neq x x,
              neq x y                ~=?    neq y x,

              false                  ~=?    lt x x,
              gt x y                 ~=?    lt y x,
              true                   ~=?    le x x,
              ge x y                 ~=?    le y x,

              false                  ~=?    gt x x,
              lt x y                 ~=?    gt y x,
              true                   ~=?    ge x x,
              le x y                 ~=?    ge y x,

              true                   ~=?    not false,
              false                  ~=?    not true,
              neq x y                ~=?    not (eq x y),
              eq x y                 ~=?    not (neq x y),
              ge x y                 ~=?    not (lt x y),
              gt x y                 ~=?    not (le x y),
              lt x y                 ~=?    not (ge x y),
              le x y                 ~=?    not (gt x y),

              true                   ~=?    true /\ true,
              false                  ~=?    true /\ false,
              false                  ~=?    false /\ true,
              false                  ~=?    false /\ false,

              true                   ~=?    true \/ true,
              true                   ~=?    true \/ false,
              true                   ~=?    false \/ true,
              false                  ~=?    false \/ false,

              xy                     ~=?    true /\ xy,
              xy                     ~=?    xy /\ true,
              false                  ~=?    false /\ xy,
              false                  ~=?    xy /\ false,

              true                   ~=?    true \/ true,
              true                   ~=?    true \/ false,
              true                   ~=?    false \/ true,
              false                  ~=?    false \/ false,

              true                   ~=?    true \/ xy,
              true                   ~=?    xy \/ true,
              xy                     ~=?    false \/ xy,
              xy                     ~=?    xy \/ false,

              xy /\ (xz /\ yz)       ~=?    (xy /\ xz) /\ yz,
              xy \/ (xz \/ yz)       ~=?    (xy \/ xz) \/ yz,
              xy                     ~=?    xy /\ (lt y z \/ ge x z),

              lt x y                 ~=?    lt x y /\ lt x y,
              lt x y                 ~=?    lt x y /\ le x y,
              false                  ~=?    lt x y /\ eq x y,
              lt x y                 ~=?    lt x y /\ neq x y,
              false                  ~=?    lt x y /\ ge x y,
              false                  ~=?    lt x y /\ gt x y,

              lt x y                 ~=?    le x y /\ lt x y,
              le x y                 ~=?    le x y /\ le x y,
              eq x y                 ~=?    le x y /\ eq x y,
              lt x y                 ~=?    le x y /\ neq x y,
              eq x y                 ~=?    le x y /\ ge x y,
              false                  ~=?    le x y /\ gt x y,

              false                  ~=?    eq x y /\ lt x y,
              eq x y                 ~=?    eq x y /\ le x y,
              eq x y                 ~=?    eq x y /\ eq x y,
              false                  ~=?    eq x y /\ neq x y,
              eq x y                 ~=?    eq x y /\ ge x y,
              false                  ~=?    eq x y /\ gt x y,

              lt x y                 ~=?    neq x y /\ lt x y,
              lt x y                 ~=?    neq x y /\ le x y,
              false                  ~=?    neq x y /\ eq x y,
              neq x y                ~=?    neq x y /\ neq x y,
              gt x y                 ~=?    neq x y /\ ge x y,
              gt x y                 ~=?    neq x y /\ gt x y,

              false                  ~=?    ge x y /\ lt x y,
              eq x y                 ~=?    ge x y /\ le x y,
              eq x y                 ~=?    ge x y /\ eq x y,
              gt x y                 ~=?    ge x y /\ neq x y,
              ge x y                 ~=?    ge x y /\ ge x y,
              gt x y                 ~=?    ge x y /\ gt x y,

              f1                     ~=?    f1 /\ f1,
              f1                     ~=?    true /\ f1,
              f1                     ~=?    f1 /\ true,
              false                  ~=?    false /\ f1,
              false                  ~=?    f1 /\ false,
              false                  ~=?    f1 /\ not f1 /\ f2,
              f1                     ~=?    f1 /\ (f1 \/ f2),
              f1 /\ f2               ~=?    f1 /\ (not f1 \/ f2),
              false                  ~=?    eq x y /\ neq y x,
              eq x y /\ eq x z       ~=?    eq z y /\ eq y x /\ eq z x,

              f1                     ~=?    f1 \/ f1,
              true                   ~=?    true \/ f1,
              true                   ~=?    f1 \/ true,
              f1                     ~=?    false \/ f1,
              f1                     ~=?    f1 \/ false,
              true                   ~=?    f1 \/ not f1 \/ f2,
              f1                     ~=?    f1 \/ (f1 /\ f2),
              f1 \/ f2               ~=?    f1 \/ (not f1 /\ f2),
              true                   ~=?    eq x y \/ neq y x,

              true                   ~=?    (∀) x true,
              false                  ~=?    (∀) x false,
              eq y z                 ~=?    (∀) x (eq y z),
              false                  ~=?    (∀) x (eq x y),

              true                   ~=?    (∃) x true,
              false                  ~=?    (∃) x false,
              eq y z                 ~=?    (∃) x (eq y z),
              true                   ~=?    (∃) x (eq x y)]

main = runTestTT tests
