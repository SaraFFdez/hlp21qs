open System

//------------------------write your answer function(s) here---------------------//

// top-level subfunctions of polarToCartesianApprox (if any)

/// answer to Tick1
// the header given here is correct.
let fact n = if n = 0 then 1.0 else List.reduce (*) [1.0..float n]

let oddOrEven i = if (i%2) = 0 then [1.0] else [-1.0]

let coeff list = List.collect (oddOrEven) list


let addZeroSin list = 
    let addzero i =
        [0.0; i]
    List.collect addzero list


let addZeroCos list = 
    let addzero i =
        [i; 0.0]
    List.collect addzero list

let test1 = addZeroSin (coeff [0..5])

let sine (r,theta) n = 
    let coeffList = addZeroSin (coeff [0..n/2])
    let term i coeff= 
        (coeff * r * (theta ** (float i))) / (float (fact i)) 
    List.mapi term coeffList |> List.reduce (+) 


let cosine (r,theta) n =
    let coeffList = addZeroCos (coeff [0..n/2])
    let term i coeff = 
        (coeff * r * (theta ** (float i))) / (float (fact i)) 
    List.mapi term coeffList |> List.reduce (+) 

let sineTest = sine (1.0, 1.0) 2
let cosTest = cosine (2.0, 0.1) 2
    

let polarToCartesianApprox (r,theta) n = 
   (cosine (r,theta) n, sine(r,theta) n)

let test = polarToCartesianApprox (2.0, 0.1) 2 

//--------------------testbench code - DO NOT CHANGE-----------------------------//

/// used to make generate testbench data
let testInputs =
    let testPolarCoords = List.allPairs [1.;2.] [1.;2.]
    List.allPairs testPolarCoords [0;1;2;3;10]

/// data showing correct results generated with model answer and given here
let testBenchData =
    [
        ((1.0, 1.0), 0, (1.0, 0.0))       
        ((1.0, 2.0), 0, (1.0, 0.0))        
        ((2.0, 1.0), 0, (2.0, 0.0))        
        ((2.0, 2.0), 0, (2.0, 0.0))        
        ((1.0, 1.0), 1, (1.0, 1.0))        
        ((1.0, 2.0), 1, (1.0, 2.0))        
        ((2.0, 1.0), 1, (2.0, 2.0))        
        ((2.0, 2.0), 1, (2.0, 4.0))        
        ((1.0, 1.0), 2, (0.5, 1.0))        
        ((1.0, 2.0), 2, (-1.0, 2.0))        
        ((2.0, 1.0), 2, (1.0, 2.0))        
        ((2.0, 2.0), 2, (-2.0, 4.0))        
        ((1.0, 1.0), 3, (0.5, 0.8333333333))        
        ((1.0, 2.0), 3, (-1.0, 0.6666666667))        
        ((2.0, 1.0), 3, (1.0, 1.666666667))        
        ((2.0, 2.0), 3, (-2.0, 1.333333333))        
        ((1.0, 1.0), 10, (0.5403023038, 0.8414710097))        
        ((1.0, 2.0), 10, (-0.4161552028, 0.9093474427))        
        ((2.0, 1.0), 10, (1.080604608, 1.682942019))        
        ((2.0, 2.0), 10, (-0.8323104056, 1.818694885))
    ]
/// test testFun with testData to see whether actual results are the same as
/// expected results taken from testData
let testBench testData testFun =
    let closeTo f1 f2 = abs (f1 - f2) < 0.000001
    let testItem fn (coords, n, (expectedX,expectedY) as expected) =
        let actualX,actualY as actual = testFun coords n
        if not (closeTo actualX expectedX) || not (closeTo actualY expectedY) then
            printfn "Error: coords=%A, n=%d, expected result=%A, actual result=%A"coords n expected actual
            1
        else
            0
    printfn "Starting tests..."
    let numErrors = List.sumBy (testItem testFun) testData
    printfn "%d tests Passed %d tests failed." (testData.Length - numErrors) numErrors

[<EntryPoint>]
let main argv =
    testBench testBenchData polarToCartesianApprox
    0 // return an integer exit code
