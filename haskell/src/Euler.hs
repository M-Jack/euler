module Euler (
              solve
) where

import Euler.P00X
import Euler.P01X
import Euler.P02X
import Euler.P03X
import Euler.P04X
import Euler.P05X
import Euler.P06X



solutions :: [Integer]
solutions = [p001,p002,p003,p004,p005,p006,p007,p008,p009,p010,
             p011,p012,p013,p014,p015,p016,p017,p018,p019,p020,
             p021,p022,p023,p024,p025,p026,p027,p028,p029,p030,
             p031,p032,p033,p034,p035,p036,p037,p038,p039,p040,
             p041,p042,p043,p044,p045,p046,p047,p048,p049,p050,
             p051,p052,p053,p054,p055,p056,p057,p058,p059,p060,
             p061,p062,p063,p064,p065,p066,p067,p068,p069]

solve :: Int -> Integer
solve x = solutions !! (x - 1) 
