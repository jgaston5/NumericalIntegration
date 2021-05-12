module IntegrationApp.Tests

open NUnit.Framework
open System.Diagnostics
open System

// TODO move this to business logic
let round (x:double) = Math.Round(x,16)

type BoundedIntegral = {
    a : double; 
    b : double;
    f : double -> double;
}

type NumericalIntegrationMethod = 
    BoundedIntegral
        -> double

let rhs (n:int): NumericalIntegrationMethod   =
    fun (boundedIntegral) -> 
        let mutable result = 0.0
        let width = boundedIntegral.b - boundedIntegral.a
        let delta = width / (double n)
        let mutable inputValue = delta + boundedIntegral.a
        while inputValue <= boundedIntegral.b do
            result <- result + ((boundedIntegral.f inputValue) * delta)
            inputValue <- inputValue + delta
        result

let lhs (n:int) : NumericalIntegrationMethod   =
    fun (boundedIntegral) -> 
        let mutable result = 0.0
        let width = boundedIntegral.b - boundedIntegral.a
        let delta = width / (double n)
        let mutable inputValue = boundedIntegral.a
        while inputValue < boundedIntegral.b do
            result <- result + ((boundedIntegral.f inputValue) * delta)
            inputValue <- inputValue + delta
        result

type RiemanSumType = 
    RHS | LHS 

let RiemanSumGenerator (method: RiemanSumType, n: int) = 
    match method with
    | RHS -> rhs n
    | LHS -> lhs n

let evaulate (boundedIntegral: BoundedIntegral, numericalIntegrationMethod: NumericalIntegrationMethod) = 
    if boundedIntegral.a = boundedIntegral.b then 0.0
    else Math.Round(numericalIntegrationMethod boundedIntegral,15)

// END move this to business logic

// TODO move these basic functions to a (test?) helper
let anyConstantFunction x = 1.0   // y = 1
let anyIncreasingLinearFunction x = (x ** 2.0) + 1.0 // y = 2x + 1
let anyDecreasingLinearFunction x = -(x ** 2.0) + 1.0 // y = -2x + 1

// END move these basic functions to a (test?) helper


let anyNumericalMethod = rhs 1

[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(2.23)>]
[<TestCase(-2.23)>]
let AnyIntervalOfLength0_Always_Returns0(bounds:double) =
    Assert.AreEqual(0, evaulate ({a = bounds; b = bounds; f = anyConstantFunction}, anyNumericalMethod))
    Assert.AreEqual(0, evaulate ({a = bounds; b = bounds; f = anyIncreasingLinearFunction}, anyNumericalMethod))

[<TestCase(1, 1, 2)>]
[<TestCase(2, 1, 3)>]
[<TestCase(3, 1, 4)>]
[<TestCase(4, 2, 5)>]
[<TestCase(5, 10, 20)>]
[<TestCase(6, 0, 3)>]
[<TestCase(6, 1, 3)>]
[<TestCase(8, 2, 3)>]
let RiemanSumOfAConstantFunction_Always_ReturnsTheExactAnswer(n: int, a: double, b: double) = 
    let rhsMethod = RiemanSumGenerator (RHS,  n) 
    let lhsMethod = RiemanSumGenerator (LHS,  n) 
    let constantValue = 1.0  // this anyConstantFunction has a value of 1
    let correctAnswer = (b - a) * constantValue
    Assert.AreEqual(correctAnswer, evaulate ({ a= a; b = b; f = anyConstantFunction}, rhsMethod);)
    Assert.AreEqual(correctAnswer, evaulate ({ a= a; b = b; f = anyConstantFunction}, lhsMethod);)

[<TestCase(1, 1, 2)>]
let RightHandRiemanSumOfAnIncreasingFunction_Always_ReturnsAnOverEstimateAnswer(n: int, a: double, b: double) = 
    let rhsMethod = RiemanSumGenerator (RHS,  n) 

    //Todo this
    Assert.True(false)
