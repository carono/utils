unit TripCMP;

{$mode objfpc}{$H+}
interface


var
  NothingVariable: pointer;

function IsNothing(var AVar): boolean;
procedure SetNothing(var AVar);

implementation

function IsNothing(var AVar): boolean;
begin
  Result := pointer(AVar) = NothingVariable;
end;

procedure SetNothing(var AVar);
begin
  pointer(AVar) := NothingVariable;
end;

initialization
  NothingVariable := AllocMem(1024);

end.
