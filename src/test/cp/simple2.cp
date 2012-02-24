var a: [int, bool] int

proc F (var n: int) : int begin
   var r: int

   if (n < 100) then
      r := a[n, true]
   else
      a := a[ n, false := 0 ];
      call r := F(n / 2)
   fi;

   return r
end

proc main (): void begin
	call F(10);
	return
end