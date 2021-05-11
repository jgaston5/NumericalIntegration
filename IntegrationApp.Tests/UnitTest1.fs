module IntegrationApp.Tests

open NUnit.Framework

type BoundedIntegral = {
a : double; 
b : double;
f : double -> double;
}

let integrate boundedIntegral = 0 


[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(2.23)>]
[<TestCase(-2.23)>]
let AnyIntervalOfLength0_Always_Returns0(bounds:double) =
    let integralOfLength0 = {a= bounds;b =bounds; f = fun x->x}
    let result = integrate integralOfLength0;
    Assert.AreEqual(0, result)
