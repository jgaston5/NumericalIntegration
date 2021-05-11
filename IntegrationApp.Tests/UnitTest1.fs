module IntegrationApp.Tests

open NUnit.Framework
open System

// TODO move this to business logic
type BoundedIntegral = {
    a : double; 
    b : double;
    f : double -> double;
}

let evaulate (boundedIntegral: BoundedIntegral) = 
    if boundedIntegral.a = boundedIntegral.b then 0
    else raise (NotImplementedException "Not ready")
// END move this to business logic

// TODO move these basic functions to a (test?) helper
let anyConstantFunction x = 1.0  
let anyLinearFunction x = (x ** 2.0) + 1.0
// END move these basic functions to a (test?) helper

[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(2.23)>]
[<TestCase(-2.23)>]
let AnyIntervalOfLength0_Always_Returns0(bounds:double) =
    Assert.AreEqual(0, evaulate {a= bounds;b =bounds; f = anyConstantFunction};)
    Assert.AreEqual(0, evaulate {a= bounds;b =bounds; f = anyLinearFunction};)
