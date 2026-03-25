with Ada.Text_IO; use Ada.Text_IO;
procedure Hello is
   function Factorial (N : Natural) return Natural is
   begin
      if N <= 1 then return 1;
      else return N * Factorial (N - 1);
      end if;
   end Factorial;
begin
   Put_Line (Natural'Image (Factorial (10)));
end Hello;
