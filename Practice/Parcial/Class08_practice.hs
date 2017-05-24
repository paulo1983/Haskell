--insertarEn::[Integer]->Integer->Integer->[Integer]
insertarEn l n i | length l ==0 && i<=(length l+1) = n:[]
                 | i<=(length l+1) = (take (i-1) l)++(n:(drop (i-1) l))


--insertarEn [1,2] 5 2 => [1,5,2]
