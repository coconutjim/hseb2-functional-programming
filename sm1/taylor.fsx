let rec fact n  =
    match n with
    | 0 -> 1
    | x when x > 0 -> n * fact(x - 1)

let rec power x n = 
    if n=0 then 1.
    else x*power x (n-1)

let nth x n = power -1. n * power x (2*n-1) / float(fact(2*int(n)+1))

let rec sinlob x n =
    if n=0 then x
    else nth x n + (sinlob x (n-1))

let rec sinnelob x n memb sum =
    if n = -1 then sum  
    else if n = 0 then sinnelob x (n-1) (memb / (- power x 2 / float(fact 3))) (sum + memb)
    else sinnelob x (n-1) (memb / (- power x 2 / float((2*n*(2*n+1))))) (sum + memb) 


for i = 0 to 10 do
    let x = float(i)/10.
    printfn "%10.2f %10.6f %10.6f %10.6f" x (sin x) (sinlob x 15) (sinnelob x 15 (nth x 15) 0.) 

 
 
   
 let rec iter f x0 n = 
    if n=0 then x0
    else f n <| iter f x0 (n-1)

let fact = iter (*) 1
let power x = iter (fun n acc -> acc*x) 1.


//-----------------------

//let sinnelob x n numb sum =
//    if n=0 then sum + x
//    else (sinnelob x (n-1)) / (- power x 2 / (4*n*(n+1))
 

//let rec comput_taylor_lob x n sum = 
 //   //if (taylor x n = 0.) then sum  // до машинного нуля
  //  //else comput_taylor_lob x (n+1.) (sum + taylor x n)
 //   forcycle (comput_taylor_lob x (n+1.) (sum+taylor x (n+1.) ) ) 1 1 10
    
//let rec comput_taylor_ne_lob x n sum = 
 //   if (nth x n = 0.) then sum
  //  else comput_taylor_ne_lob x (n+1.) (sum + taylor x n)
    
//let go x = 
    //comput_taylor_lob x 1. 0
 //   forcycle  

//sin 1. 
//sinlob 1. 15 




//let nth (x : float) (n : float) = -1.**n * x**(2.*n-1.) / float(fact(2*int(n)+1))

//let nth x n = -1**n * x**(2*n-1) / fact(2*int(n)+1)


let rec forcycle f A inc B = 
    if A >= B then f A
    else
    f A
    forcycle f (A + inc) inc B

//let fact (x : float) = forcycle (x *= x) 1 x 

