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
        
type RiemanSumType = 
    RHS | LHS | MPS | TZS

//type RiemanEvaluator = double -> double -> (double -> double) -> double

let RightHandSum (left: double, delta: double, f: double -> double): double = 
    f (left + delta)

let LeftHandSum (left: double, delta: double, f: double -> double) : double = 
    f left

let MidpointSum (left: double, delta: double, f: double -> double) : double = 
    f (left + (delta/2.0))

let TrapezoidSum (left: double, delta: double, f: double -> double) : double = 
    (f (left)  + f (left + delta))/2.0

let RiemanSumGenerator (method: RiemanSumType, n: int): NumericalIntegrationMethod = 
    fun (boundedIntegral) -> 
        let width = boundedIntegral.b - boundedIntegral.a
        let delta = width / (double n)
        let mutable inputValue = boundedIntegral.a
        let nextValue = match method with
                        | RHS -> RightHandSum
                        | LHS -> LeftHandSum      
                        | MPS -> MidpointSum      
                        | TZS -> TrapezoidSum      
        let mutable result = (double)0.0
        for i in 1 .. n do
            result <- result + (nextValue(inputValue, delta, boundedIntegral.f) * delta)
            inputValue <- inputValue + delta
        result

let evaulate (boundedIntegral: BoundedIntegral, numericalIntegrationMethod: NumericalIntegrationMethod) = 
    if boundedIntegral.a = boundedIntegral.b then 0.0
    else Math.Round(numericalIntegrationMethod boundedIntegral,15)

// END move this to business logic

// TODO move these basic functions to a (test?) helper
let anyConstantFunction x = 1.0   // y = 1
let anyIncreasingFunction x = (x * 2.0) + 1.0 // y = 2x + 1
let anyDecreasingFunction x = -(x * 2.0) + 1.0 // y = -2x + 1
// END move these basic functions to a (test?) helper


[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(2.23)>]
[<TestCase(-2.23)>]
let AnyIntervalOfLength0_Always_Returns0(bounds:double) =
    let anyNumericalMethod = RiemanSumGenerator(RHS, 1)
    Assert.AreEqual(0, evaulate ({a = bounds; b = bounds; f = anyConstantFunction}, anyNumericalMethod))
    Assert.AreEqual(0, evaulate ({a = bounds; b = bounds; f = anyIncreasingFunction}, anyNumericalMethod))

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
    Assert.AreEqual(correctAnswer, evaulate ({ a= a; b = b; f = anyConstantFunction}, rhsMethod))
    Assert.AreEqual(correctAnswer, evaulate ({ a= a; b = b; f = anyConstantFunction}, lhsMethod))

[<TestCase(1, 1, 2)>]
[<TestCase(2, 1, 3)>]
[<TestCase(3, 1, 4)>]
[<TestCase(4, 2, 5)>]
[<TestCase(5, 10, 20)>]
[<TestCase(6, 0, 3)>]
[<TestCase(6, 1, 3)>]
[<TestCase(8, 2, 3)>]
let RightHandRiemanSumOfAnIncreasingFunction_Always_ReturnsAnOverEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (RHS,  n)
    let derivativeFunction = anyIncreasingFunction // 2x + 1 
    let integralFunction x: double = (x ** 2.0) + x // xx + x
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Greater(estimatedAnswer, correctAnswer)

[<TestCase(1, 1, 2)>]
[<TestCase(2, 1, 3)>]
[<TestCase(3, 1, 4)>]
[<TestCase(4, 2, 5)>]
[<TestCase(5, 10, 20)>]
[<TestCase(6, 0, 3)>]
[<TestCase(6, 1, 3)>]
[<TestCase(8, 2, 3)>]
let RightHandRiemanSumOfADecreasingFunction_Always_ReturnsAnUnderEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (RHS,  n)
    let derivativeFunction = anyDecreasingFunction // -2x + 1 
    let integralFunction x: double = -(x ** 2.0) + x // -xx + x
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Less(estimatedAnswer, correctAnswer)

[<TestCase(1, 1, 2)>]
[<TestCase(2, 1, 3)>]
[<TestCase(3, 1, 4)>]
[<TestCase(4, 2, 5)>]
[<TestCase(5, 10, 20)>]
[<TestCase(6, 0, 3)>]
[<TestCase(6, 1, 3)>]
[<TestCase(8, 2, 3)>]
let LeftHandRiemanSumOfAnIncreasingFunction_Always_ReturnsAnUnderEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (LHS,  n)
    let derivativeFunction = anyIncreasingFunction // 2x + 1 
    let integralFunction x: double = (x ** 2.0) + x // xx + x
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Less(estimatedAnswer, correctAnswer )


[<TestCase(1, 1, 2)>]
[<TestCase(2, 1, 3)>]
[<TestCase(3, 1, 4)>]
[<TestCase(4, 2, 5)>]
[<TestCase(5, 10, 20)>]
[<TestCase(6, 0, 3)>]
[<TestCase(6, 1, 3)>]
[<TestCase(8, 2, 3)>]
let LeftHandRiemanSumOfADecreasingFunction_Always_ReturnsAnOverEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (LHS,  n)
    let derivativeFunction = anyDecreasingFunction // -2x + 1 
    let integralFunction x: double = -(x ** 2.0) + x // -xx + x
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Greater(estimatedAnswer, correctAnswer)


[<TestCase(1, 1, 2)>]
[<TestCase(2, 1, 3)>]
[<TestCase(3, 1, 4)>]
[<TestCase(4, 2, 5)>]
[<TestCase(5, 10, 20)>]
[<TestCase(6, 0, 3)>]
[<TestCase(6, 1, 3)>]
[<TestCase(8, 2, 3)>]
let MidpointRiemanSumOfAConcaveUpFunction_Always_ReturnsAnUnderEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (MPS,  n)
    let anyConcaveUpFunctionOverGivenRange x = (x * x * 3.0) // y = 3xx 
    let derivativeFunction = anyConcaveUpFunctionOverGivenRange 
    let integralFunction x: double = (x ** 3.0)// xxx
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Less(estimatedAnswer, correctAnswer)

[<TestCase(1, 1, 2)>]
[<TestCase(2, 1, 3)>]
[<TestCase(3, 1, 4)>]
[<TestCase(4, 2, 5)>]
[<TestCase(5, 10, 20)>]
[<TestCase(6, 0, 3)>]
[<TestCase(6, 1, 3)>]
[<TestCase(8, 2, 3)>]
let MidpointRiemanSumOfAConcaveDownFunction_Always_ReturnsAnOverEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (MPS,  n)
    let anyConcaveDownFunctionOverGivenDomain x = -(x * x * 3.0) // y = -3xx 
    let derivativeFunction = anyConcaveDownFunctionOverGivenDomain 
    let integralFunction x: double = -(x ** 3.0)// -xxx
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Greater(estimatedAnswer, correctAnswer)

[<TestCase(1, 1, 2)>]
[<TestCase(2, 1, 3)>]
[<TestCase(3, 1, 4)>]
[<TestCase(4, 2, 5)>]
[<TestCase(5, 10, 20)>]
[<TestCase(6, 0, 3)>]
[<TestCase(6, 1, 3)>]
[<TestCase(8, 2, 3)>]
let TrapezoidalRiemanSumOfAConcaveUpFunction_Always_ReturnsAnOverEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (TZS,  n)
    let anyConcaveUpFunctionOverGivenDomain x = (x * x * 3.0) // y = 3xx 
    let derivativeFunction = anyConcaveUpFunctionOverGivenDomain 
    let integralFunction x: double = (x ** 3.0) // xxx
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Greater(estimatedAnswer, correctAnswer )


[<TestCase(1, 1, 2)>]
[<TestCase(2, 1, 3)>]
[<TestCase(3, 1, 4)>]
[<TestCase(4, 2, 5)>]
[<TestCase(5, 10, 20)>]
[<TestCase(6, 0, 3)>]
[<TestCase(6, 1, 3)>]
[<TestCase(8, 2, 3)>]
let TrapezoidalRiemanSumOfAConcaveDownFunction_Always_ReturnsAnUnderEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (TZS,  n)
    let anyConcaveDownFunctionOverGivenDomain x = -(x * x * 3.0) // y = -3xx 
    let derivativeFunction = anyConcaveDownFunctionOverGivenDomain
    let integralFunction x: double = -(x ** 3.0)// -xxx
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Less(estimatedAnswer, correctAnswer)