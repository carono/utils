unit Translit;

interface

uses SysUtils, LCLProc ;
//{$mode delphi}

function TranslitToLat(OldName: string): string;
function TranslitToRus(OldName: ansistring): ansistring;

implementation

function TranslitToLat(OldName: string): string;
const
  RArrayL = 'абвгдеёжзийклмнопрстуфхцчшщьыъэюя';
  RArrayU = 'АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯ';
  colChar = 33;
  arr: array[1..2, 1..ColChar] of string =
    (('a', 'b', 'v', 'g', 'd', 'e', 'yo', 'zh', 'z', 'i', 'y',
    'k', 'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'f',
    'kh', 'ts', 'ch', 'sh', 'shch', '''', 'y', '''', 'e', 'yu', 'ya'),
    ('A', 'B', 'V', 'G', 'D', 'E', 'Yo', 'Zh', 'Z', 'I', 'Y',
    'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U', 'F',
    'Kh', 'Ts', 'Ch', 'Sh', 'Shch', '''', 'Y', '''', 'E', 'Yu', 'Ya'));
var
  c:string;
  i: integer;
  LenS: integer;
  p: integer;
  d: byte;
begin
  Result := '';
  LenS :=  UTF8length(OldName);
  i:=1;
  for i:=1 to lenS do
  begin
    d := 1;
    c:= UTF8Copy(OldName,i,1);
    p := UTF8Pos(c, RArrayL);
    if p = 0 then
    begin
      p := UTF8Pos(c, RArrayU);
      d := 2;
    end;
    if p <> 0 then
      Result := Result + arr[d, p]
    else
      Result := Result + c; //если не русская буква, то берем исходную
  end;
end;

function TranslitToRus(OldName: ansistring): ansistring;
begin
  Result := OldName;
end;

end.
