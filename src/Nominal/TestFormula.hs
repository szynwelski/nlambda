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

tests = test [true                   ~=?    eq x x,                      --  0
              eq x y                 ~=?    eq y x,                      --  1
              false                  ~=?    neq x x,                     --  2
              neq x y                ~=?    neq y x,                     --  3

              false                  ~=?    lt x x,                      --  4
              gt x y                 ~=?    lt y x,                      --  5
              true                   ~=?    le x x,                      --  6
              ge x y                 ~=?    le y x,                      --  7

              false                  ~=?    gt x x,                      --  8
              lt x y                 ~=?    gt y x,                      --  9
              true                   ~=?    ge x x,                      -- 10
              le x y                 ~=?    ge y x,                      -- 11

              true                   ~=?    not false,                   -- 12
              false                  ~=?    not true,                    -- 13
              neq x y                ~=?    not (eq x y),                -- 14
              eq x y                 ~=?    not (neq x y),               -- 15
              ge x y                 ~=?    not (lt x y),                -- 16
              gt x y                 ~=?    not (le x y),                -- 17
              lt x y                 ~=?    not (ge x y),                -- 18
              le x y                 ~=?    not (gt x y),                -- 19

              true                   ~=?    true /\ true,                -- 20
              false                  ~=?    true /\ false,               -- 21
              false                  ~=?    false /\ true,               -- 22
              false                  ~=?    false /\ false,              -- 23

              true                   ~=?    true \/ true,                -- 24
              true                   ~=?    true \/ false,               -- 25
              true                   ~=?    false \/ true,               -- 26
              false                  ~=?    false \/ false,              -- 27

              xy                     ~=?    true /\ xy,                  -- 28
              xy                     ~=?    xy /\ true,                  -- 29
              false                  ~=?    false /\ xy,                 -- 30
              false                  ~=?    xy /\ false,                 -- 31

              true                   ~=?    true \/ true,                -- 32
              true                   ~=?    true \/ false,               -- 33
              true                   ~=?    false \/ true,               -- 34
              false                  ~=?    false \/ false,              -- 35

              true                   ~=?    true \/ xy,                  -- 36
              true                   ~=?    xy \/ true,                  -- 37
              xy                     ~=?    false \/ xy,                 -- 38
              xy                     ~=?    xy \/ false,                 -- 39

              xy /\ (xz /\ yz)       ~=?    (xy /\ xz) /\ yz,            -- 40
              xy \/ (xz \/ yz)       ~=?    (xy \/ xz) \/ yz,            -- 41
              xy                     ~=?    xy /\ (lt y z \/ ge x z),    -- 42

              lt x y                 ~=?    lt x y /\ lt x y,            -- 43
              lt x y                 ~=?    lt x y /\ le x y,            -- 44
              false                  ~=?    lt x y /\ eq x y,            -- 45
              lt x y                 ~=?    lt x y /\ neq x y,           -- 46
              false                  ~=?    lt x y /\ ge x y,            -- 47
              false                  ~=?    lt x y /\ gt x y,            -- 48

              lt x y                 ~=?    le x y /\ lt x y,            -- 49
              le x y                 ~=?    le x y /\ le x y,            -- 50
              eq x y                 ~=?    le x y /\ eq x y,            -- 51
              lt x y                 ~=?    le x y /\ neq x y,           -- 52
              eq x y                 ~=?    le x y /\ ge x y,            -- 53
              false                  ~=?    le x y /\ gt x y,            -- 54

              false                  ~=?    eq x y /\ lt x y,            -- 55
              eq x y                 ~=?    eq x y /\ le x y,            -- 56
              eq x y                 ~=?    eq x y /\ eq x y,            -- 57
              false                  ~=?    eq x y /\ neq x y,           -- 58
              eq x y                 ~=?    eq x y /\ ge x y,            -- 59
              false                  ~=?    eq x y /\ gt x y,            -- 60

              lt x y                 ~=?    neq x y /\ lt x y,           -- 61
              lt x y                 ~=?    neq x y /\ le x y,           -- 62
              false                  ~=?    neq x y /\ eq x y,           -- 63
              neq x y                ~=?    neq x y /\ neq x y,          -- 64
              gt x y                 ~=?    neq x y /\ ge x y,           -- 65
              gt x y                 ~=?    neq x y /\ gt x y,           -- 66

              false                  ~=?    ge x y /\ lt x y,            -- 67
              eq x y                 ~=?    ge x y /\ le x y,            -- 68
              eq x y                 ~=?    ge x y /\ eq x y,            -- 69
              gt x y                 ~=?    ge x y /\ neq x y,           -- 70
              ge x y                 ~=?    ge x y /\ ge x y,            -- 71
              gt x y                 ~=?    ge x y /\ gt x y,            -- 72

              f1                     ~=?    f1 /\ f1,                    -- 73
              f1                     ~=?    true /\ f1,                  -- 74
              f1                     ~=?    f1 /\ true,                  -- 75
              false                  ~=?    false /\ f1,                 -- 76
              false                  ~=?    f1 /\ false,                 -- 77
              false                  ~=?    f1 /\ not f1 /\ f2,          -- 78
              f1                     ~=?    f1 /\ (f1 \/ f2),            -- 79
              f1 /\ f2               ~=?    f1 /\ (not f1 \/ f2),        -- 80
              false                  ~=?    eq x y /\ neq y x,           -- 81
              eq x y /\ eq x z       ~=?    eq z y /\ eq y x /\ eq z x,  -- 82

              f1                     ~=?    f1 \/ f1,                    -- 83
              true                   ~=?    true \/ f1,                  -- 84
              true                   ~=?    f1 \/ true,                  -- 85
              f1                     ~=?    false \/ f1,                 -- 86
              f1                     ~=?    f1 \/ false,                 -- 87
              true                   ~=?    f1 \/ not f1 \/ f2,          -- 88
              f1                     ~=?    f1 \/ (f1 /\ f2),            -- 89
              f1 \/ f2               ~=?    f1 \/ (not f1 /\ f2),        -- 90
              true                   ~=?    eq x y \/ neq y x,           -- 91

              true                   ~=?    (∀) x true,                  -- 92
              false                  ~=?    (∀) x false,                 -- 93
              eq y z                 ~=?    (∀) x (eq y z),              -- 94
              false                  ~=?    (∀) x (eq x y),              -- 95

              true                   ~=?    (∃) x true,                  -- 96
              false                  ~=?    (∃) x false,                 -- 97
              eq y z                 ~=?    (∃) x (eq y z),              -- 98
              true                   ~=?    (∃) x (eq x y)]              -- 99

main = runTestTT tests
