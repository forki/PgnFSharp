namespace Lizard.Engine

type Pnt = 
    { X : float
      Y : float }

type QuadBzCv = 
    { Start : Pnt
      Gravity : Pnt
      End : Pnt }

module QuadraticBezier = 
    let XAtT (t : float) qbc = 
        (1.0 - t) * (1.0 - t) * qbc.Start.X + 2.0 * (1.0 - t) * t * qbc.Gravity.X + t * t * qbc.End.X
    let YAtT (t : float) qbc = 
        (1.0 - t) * (1.0 - t) * qbc.Start.Y + 2.0 * (1.0 - t) * t * qbc.Gravity.Y + t * t * qbc.End.Y
    
    let Select xCoords qbc = 
        let incr = 0.005
        
        let rec gett x t = 
            let xAtT = XAtT t qbc
            if xAtT >= x then t
            elif t > 1.0 then t
            else gett x (t + incr)
        seq { 
            for x in xCoords do
                let t = gett x 0.0
                let y = YAtT t qbc
                yield { X = x
                        Y = y }
        }
    
    let GetIntegerValues maxX first last ctrlX ctrlY = 
        let increase = last - first
        
        let start = 
            { X = 0.0
              Y = 0.0 }
        
        let nd = 
            { X = maxX
              Y = increase }
        
        let ctrl = 
            { X = float (maxX) * ctrlX
              Y = increase * ctrlY }
        
        let curve = 
            { Start = start
              Gravity = ctrl
              End = nd }
        
        let xCoordsFloat = [| 0..(int (maxX) + 1) |] |> Array.map (fun i -> float (i))
        
        let yCoords = 
            (Select xCoordsFloat curve)
            |> Seq.toArray
            |> Array.map (fun p -> p.Y)
        yCoords |> Array.map (fun y -> y + first)
