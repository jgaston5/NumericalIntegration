module public BaseTypes

open System

type BoundedIntegral = {
    a : double; 
    b : double;
    f : double -> double;
}

type NumericalIntegrationMethod = 
    BoundedIntegral
        -> double

let evaulate (boundedIntegral: BoundedIntegral, numericalIntegrationMethod: NumericalIntegrationMethod) = 
    let roundPrecision =  15
    if boundedIntegral.a = boundedIntegral.b then 0.0
    else Math.Round(numericalIntegrationMethod boundedIntegral, roundPrecision)
