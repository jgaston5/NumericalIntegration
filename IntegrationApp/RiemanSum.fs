module public RiemanSum
open BaseTypes

type RiemanSumType = 
    RHS | LHS | MPS | TZS

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
