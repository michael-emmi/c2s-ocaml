var a: [int, bool] int;

procedure F(n: int) returns (r: int)
   modifies a;
{
   if (n < 100) {
      r := a[n, true];
   } else {
      a := a[ n, false := 0 ];
      call r := F(n / 2);
   }
}