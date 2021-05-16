module NumericalIntegration.RiemanSumTests

open NUnit.Framework
open BaseTypes
open RiemanSum

let anyConstantFunction x = 1.0   // y = 1
let anyIncreasingFunction x = (x * 2.0) + 1.0 // y = 2x + 1
let anyDecreasingFunction x = -(x * 2.0) + 1.0 // y = -2x + 1

let RiemanSumCases =
    [
        1, 1, 2
        2, 1, 3
        3, 1, 4
        4, 2, 5
        6, 0, 3
        8, 2, 3
        6, 1, 3
        5, 10, 20
    ] |> List.map (fun (q, n, d) -> TestCaseData(q,n,d))

[<TestCaseSource("RiemanSumCases")>]
let RiemanSumOfAConstantFunction_Always_ReturnsTheExactAnswer(n: int, a: double, b: double) = 
    let rhsMethod = RiemanSumGenerator (RHS,  n) 
    let lhsMethod = RiemanSumGenerator (LHS,  n) 
    let constantValue = 1.0  // this anyConstantFunction has a value of 1
    let correctAnswer = (b - a) * constantValue
    Assert.AreEqual(correctAnswer, evaulate ({ a= a; b = b; f = anyConstantFunction}, rhsMethod))
    Assert.AreEqual(correctAnswer, evaulate ({ a= a; b = b; f = anyConstantFunction}, lhsMethod))

[<TestCaseSource("RiemanSumCases")>]
let RightHandRiemanSumOfAnIncreasingFunction_Always_ReturnsAnOverEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (RHS,  n)
    let derivativeFunction = anyIncreasingFunction // 2x + 1 
    let integralFunction x: double = (x ** 2.0) + x // xx + x
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Greater(estimatedAnswer, correctAnswer)

[<TestCaseSource("RiemanSumCases")>]
let RightHandRiemanSumOfADecreasingFunction_Always_ReturnsAnUnderEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (RHS,  n)
    let derivativeFunction = anyDecreasingFunction // -2x + 1 
    let integralFunction x: double = -(x ** 2.0) + x // -xx + x
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Less(estimatedAnswer, correctAnswer)

[<TestCaseSource("RiemanSumCases")>]
let LeftHandRiemanSumOfAnIncreasingFunction_Always_ReturnsAnUnderEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (LHS,  n)
    let derivativeFunction = anyIncreasingFunction // 2x + 1 
    let integralFunction x: double = (x ** 2.0) + x // xx + x
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Less(estimatedAnswer, correctAnswer )


[<TestCaseSource("RiemanSumCases")>]
let LeftHandRiemanSumOfADecreasingFunction_Always_ReturnsAnOverEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (LHS,  n)
    let derivativeFunction = anyDecreasingFunction // -2x + 1 
    let integralFunction x: double = -(x ** 2.0) + x // -xx + x
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Greater(estimatedAnswer, correctAnswer)


[<TestCaseSource("RiemanSumCases")>]
let MidpointRiemanSumOfAConcaveUpFunction_Always_ReturnsAnUnderEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (MPS,  n)
    let anyConcaveUpFunctionOverGivenRange x = (x * x * 3.0) // y = 3xx 
    let derivativeFunction = anyConcaveUpFunctionOverGivenRange 
    let integralFunction x: double = (x ** 3.0)// xxx
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Less(estimatedAnswer, correctAnswer)

[<TestCaseSource("RiemanSumCases")>]
let MidpointRiemanSumOfAConcaveDownFunction_Always_ReturnsAnOverEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (MPS,  n)
    let anyConcaveDownFunctionOverGivenDomain x = -(x * x * 3.0) // y = -3xx 
    let derivativeFunction = anyConcaveDownFunctionOverGivenDomain 
    let integralFunction x: double = -(x ** 3.0)// -xxx
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Greater(estimatedAnswer, correctAnswer)

[<TestCaseSource("RiemanSumCases")>]
let TrapezoidalRiemanSumOfAConcaveUpFunction_Always_ReturnsAnOverEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (TZS,  n)
    let anyConcaveUpFunctionOverGivenDomain x = (x * x * 3.0) // y = 3xx 
    let derivativeFunction = anyConcaveUpFunctionOverGivenDomain 
    let integralFunction x: double = (x ** 3.0) // xxx
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Greater(estimatedAnswer, correctAnswer )

[<TestCaseSource("RiemanSumCases")>]
let TrapezoidalRiemanSumOfAConcaveDownFunction_Always_ReturnsAnUnderEstimateAnswer(n: int, a: double, b: double) = 
    let rs = RiemanSumGenerator (TZS,  n)
    let anyConcaveDownFunctionOverGivenDomain x = -(x * x * 3.0) // y = -3xx 
    let derivativeFunction = anyConcaveDownFunctionOverGivenDomain
    let integralFunction x: double = -(x ** 3.0)// -xxx
    let correctAnswer = integralFunction(b) - integralFunction(a)
    let estimatedAnswer = evaulate ({ a= a; b = b; f = derivativeFunction}, rs)
    Assert.Less(estimatedAnswer, correctAnswer)
