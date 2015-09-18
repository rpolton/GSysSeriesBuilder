module Program

open GSeriesBuilder

    [<EntryPoint>]
    let main args = 
        let firstNumber, growthFactor = 
            match SeriesBuilder.f 2.5 1.25 with
            | Some (a,b) -> a,b
            | None -> failwith "Cannot determine firstNumber and growthFactor"
        let s = SeriesBuilder.series firstNumber growthFactor 25
        let number1 = SeriesBuilder.takeNthFromEnd 3 s
        let number2 z' = 
            let y = float 5 // some constant
            let approx z = y / z
            match SeriesBuilder.closest (approx z') s with | (a,_) -> a
        printf "%f %f" number1 (number2 2.6)
        0        

