module Tests exposing (..)

import Test exposing (..)
import HarvestTests


suite : Test
suite =
    describe "Harvest API Tests"
        [ HarvestTests.suite
        ]
