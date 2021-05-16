module IntegrationApp.BasicTypesTests

open NUnit.Framework
open BaseTypes
open RiemanSum

let anyConstantFunction x = 1.0   // y = 1
let anyIncreasingFunction x = (x * 2.0) + 1.0 // y = 2x + 1
let anyDecreasingFunction x = -(x * 2.0) + 1.0 // y = -2x + 1

[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(2.23)>]
[<TestCase(-2.23)>]
let AnyIntervalOfLength0_Always_Returns0(bounds:double) =
    let anyNumericalMethod = RiemanSumGenerator(RHS, 1)
    Assert.AreEqual(0, evaulate ({a = bounds; b = bounds; f = anyConstantFunction}, anyNumericalMethod))
    Assert.AreEqual(0, evaulate ({a = bounds; b = bounds; f = anyIncreasingFunction}, anyNumericalMethod))
    Assert.AreEqual(0, evaulate ({a = bounds; b = bounds; f = anyDecreasingFunction}, anyNumericalMethod))
