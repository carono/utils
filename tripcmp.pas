unit TripCMP;

{$mode objfpc}{$H+}
interface


var
  NothingVariable: pointer;

function IsNothing(var AVar): boolean;
procedure SetNothing(var AVar);
function Ternary(AIF: boolean; AThen: variant; AElse: variant): variant;

implementation

function IsNothing(var AVar): boolean;
begin
  Result := pointer(AVar) = NothingVariable;
end;

procedure SetNothing(var AVar);
begin
  pointer(AVar) := NothingVariable;
end;

function Ternary(AIF: boolean; AThen: variant; AElse: variant): variant;
begin
  if (AIF) then
    Result := AThen
  else
    Result := AElse;
end;

initialization
  NothingVariable := AllocMem(1024);

end.
