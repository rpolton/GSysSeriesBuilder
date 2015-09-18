module GSeriesBuilder

open NUnit.Framework
open FsCheck
open FsUnit

module SeriesBuilder =
    let f x y = 
        let ``25 times a`` = ((0.5 * x * x) + (30.0 * x) + 10.0)  // which will be -ve if -50<x<-10, min value of a is -88/5 at x=-30
        try
            let b = (0.02 * y) / ``25 times a``
            Some(``25 times a`` / 25.0, b)
        with :? System.DivideByZeroException -> None // or we could check whether x is -50 or -10 (the roots of the quadratic)

    let series (firstNumber:float) growthRate length =
        if firstNumber >= 0.0 && growthRate <= 0.0 then failwith "Cannot generate a sequence of more than one element with +ve firstNumber and -ve growthRate"
        let s =
            let rec loop x = seq { yield (growthRate * (pown firstNumber x)) ; yield! loop (x+1) }
            seq { yield firstNumber ; yield! loop 1 }
        let roundedSeries = 
            let identity = fun x -> x
            s |> Seq.map (fun n -> (float (round (n*4.0))) / 4.0) |>
                (match firstNumber >= 0.0 && growthRate >= 0.0 with
                | true -> identity // we don't need to sort because the series defn is increasing for +ve firstNumber and growthRate
                | false -> Seq.filter (fun x -> x >= firstNumber)) |> // we have to make some decision about what to do with -ve nos. The spec says 'firstNumber' is the first no in the sequence and that the seq is monotonically increasing so we can infer that any more -ve nos should be excluded
                (match abs growthRate < 1.0 with // 0<|growthRate|<1 means the second element will be smaller than the first
                | true -> fun s' -> s' |>
                    // the no we take needs to be dynmaically determined in case we have very small growthRate
                    Seq.take (2*length) |> // because distinct removes items from the seq
                    Seq.sort 
                | false -> identity) |>
                Seq.distinct |>
                Seq.take length
        // no dupes
        // in order from lowest to highest - only need to sort if firstNumber is -ve or if growthRate<1
        // round to nearest 0.25
        roundedSeries |> List.ofSeq

    let takeNthFromEnd n l =
        // n>=1, 1 => end element
        if n<1 || n>Seq.length l then failwith "Not enough elements in the list"
        l |> List.ofSeq |> List.rev |> Seq.skip (n-1) |> (* Seq.take 1 |> *) Seq.head

    let closest v =
        // find the nearest, or next highest in the case of being exactly in the middle, neighbour of v in l
        // by minimising the distance from each element to v
        // Could possibly optimise this method by making use of the fact that the input list is sorted according to the problem spec
        Seq.fold (fun state elem -> 
            let distance = abs (v-elem)
            if distance <= (snd state) then (elem,distance) else state // <= gives the upper bound, < would give the lower in the case of v being midway
        ) (0.0,System.Double.MaxValue) // problem here if the series contains numbers that are further than MaxValue apart, eg -1 and MaxValue

module SeriesBuilderTester =
    open SeriesBuilder

    let generateTestSeries () =
        let rnd = System.Random()
        let length = rnd.Next(1, 50)
        seq { for a in 1 .. length -> rnd.NextDouble() } // all values will be between 0 and 1

// When I get FsCheck to generate sequences of floats *and* an integer then we can use this instead of the clunky
// manual tests below
//    let generateRandomDouble () = 
//        let rnd = System.Random()
//        gen { return rnd.NextDouble() }
//
//    let randomSeries n = 
//        Gen.listOfLength n (generateRandomDouble()) 

    // a property to test the correctness of the takeNthFromEnd function
    let ``nth from end is the same as skip length-n and take 1`` n l' =
        let l = l' |> List.ofSeq
        let num = l |> Seq.skip (Seq.length l - n) |> Seq.head
        num = SeriesBuilder.takeNthFromEnd n l

    let rec runTheTest testf (series:seq<'a>) howMany = 
        match howMany with
        | 0 -> true
        | _ -> NUnit.Framework.Assert.IsTrue (testf series) ; runTheTest testf series (howMany - 1)

    [<Test>]
    let ``Verify that the end is expected value`` () =
        let testSeq = seq [ 1.25; 5.5; 10.75; 21.25; 50.0 ]
        Assert.IsTrue (``nth from end is the same as skip length-n and take 1`` 1 testSeq)

    [<Test>]
    let ``Verify that the second from end is expected value`` () =
        let testSeq = seq [ 1.25; 5.5; 10.75; 21.25; 50.0 ]
        Assert.IsTrue (``nth from end is the same as skip length-n and take 1`` 2 testSeq)

    [<Test>]
    let ``Verify that the third from end is expected value`` () =
        let testSeq = seq [ 1.25; 5.5; 10.75; 21.25; 50.0 ]
        Assert.IsTrue (``nth from end is the same as skip length-n and take 1`` 3 testSeq)

    [<Test>]
    let ``Verify that the fourth from end is expected value`` () =
        let testSeq = seq [ 1.25; 5.5; 10.75; 21.25; 50.0 ]
        Assert.IsTrue (``nth from end is the same as skip length-n and take 1`` 4 testSeq)

    [<Test>]
    let ``Verify that the fifth from end is expected value`` () =
        let testSeq = seq [ 1.25; 5.5; 10.75; 21.25; 50.0 ]
        Assert.IsTrue (``nth from end is the same as skip length-n and take 1`` 5 testSeq)

    [<Test>]
    let ``Test that the nth from the end is the same as skip length-n`` () =
        let testseries = generateTestSeries()
        runTheTest (fun l -> 
            let rnd = System.Random()
            let which = rnd.Next(1,Seq.length l)
            ``nth from end is the same as skip length-n and take 1`` which l) testseries 10 |> ignore

//    [<Test>]
//    let ``Test nthFromEnd with FsCheck`` () =
//        let property num =
//            
//            ``nth from end is the same as skip length-n and take 1`` num
//        Check.Quick (property num)

    [<Test>]
    let ``Find the closest entry to very large -ve number`` () =
        let seriesForClosestCheck = [-100.0; 0.0; 1.0; 0.25; 50.0; -15.0; 7.5; -1.0]
        let nearest,distance = SeriesBuilder.closest -1000000.0 seriesForClosestCheck
        Assert.AreEqual ((seriesForClosestCheck|>Seq.head), nearest)

    [<Test>]
    let ``Find the closest entry to very large +ve number`` () =
        let seriesForClosestCheck = [-100.0; 0.0; 1.0; 0.25; 50.0; -15.0; 7.5; -1.0]
        let nearest,distance = SeriesBuilder.closest 1000000.0 seriesForClosestCheck
        Assert.AreEqual ((seriesForClosestCheck|>Seq.skip 4|>Seq.head), nearest)

    [<Test>]
    let ``Find the closest entry in middle of list`` () =
        let seriesForClosestCheck = [-100.0; 0.0; 1.0; 0.25; 50.0; -15.0; 7.5; -1.0]
        let nearest,distance = SeriesBuilder.closest 0.5 seriesForClosestCheck
        Assert.AreEqual ((seriesForClosestCheck|>Seq.skip 3|>Seq.head), nearest)

    [<Test>]
    let ``Find the closest entry when equidistant from two values`` () =
        let seriesForClosestCheck = [-100.0; 0.0; 1.0; 0.25; 50.0; -15.0; 7.5; -1.0]
        let nearest,distance = SeriesBuilder.closest 0.125 seriesForClosestCheck
        Assert.AreEqual ((seriesForClosestCheck|>Seq.skip 3|>Seq.head), nearest)

    [<Test>]
    let ``Ensure the zero-crossings are where we expect`` () =
        [-50.0; -10.0] |> List.map (fun x ->
            Assert.IsTrue ((SeriesBuilder.f x 1.0).IsSome)) |> ignore

    // Property satisfied by the quadratic
//    [<Test>]
//    let ``Negative first values occur for -50<x<-10`` () =
//        let testWithRange f num =
//            let isInRange i = (i > -50.0) && (i < -10.0)
//            isInRange num ==> lazy (f num)
//        let property num =
//            match SeriesBuilder.f num 1.0 with | Some (a,b) -> a < 0.0 | None -> false
//        Check.Quick (testWithRange property)
    
    [<Test>]
    let ``The series is sorted in strictly increasing order`` () =
        let firstNumber,rate = match SeriesBuilder.f 1.0 2500.0 with | Some (a,b) -> a,b
        let length = 15
        let series = SeriesBuilder.series firstNumber rate length    
        let isIncreasing l = l |> Seq.pairwise |> Seq.forall (fun (a,b) -> a < b)
        Assert.IsTrue (isIncreasing series)

    [<Test>]
    let ``The first element of the series is the one we supply, albeit rounded`` () =
        let firstNumber = 2.6
        Assert.IsTrue <| match SeriesBuilder.series firstNumber 1.0 10 with | firstNumber::_ -> true | _ -> false

    [<Test>]
    let ``The length of the series is what we supply`` () =
        let length = 11
        match SeriesBuilder.series 2.73 0.001 length with
        | [] -> Assert.Fail()
        | _ as l -> Assert.AreEqual ( length, Seq.length l )
    
    [<Test>]
    let ``Verify series against sample`` () =
        let firstNumber, growthRate = 
            match SeriesBuilder.f 1.0 5062.5 with
            | Some(a,b) -> a,b
        Assert.AreEqual (1.62, firstNumber)
        Assert.AreEqual (2.5, growthRate)

        let series = SeriesBuilder.series firstNumber growthRate 5

        [1.5; 4.0; 6.5; 10.75; 17.25] |> Seq.forall2 (fun x y -> x = y) series |> Assert.IsTrue
