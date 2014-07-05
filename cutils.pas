unit cUtils;

{$mode objfpc}{$H+}
interface

uses
  SysUtils, md5, variants, fpjson, Classes;

type
  TStringArray = array of string;

function Utf8ToCp1251(s: ansistring): ansistring;
function cp1251ToUrf8(s: ansistring): ansistring;
function IntVersionToStr(version: integer; section: shortint = 3; delimiter: char = '.'): string;
function StrToIntVersion(version: string; section: shortint = 3; delimiter: char = '.'): integer;
function MD5(str: string): string;
function MD5File(Path: string): string;
function PrepareMessage(Message: ansistring): ansistring;
function GetPreparedMessage(Message: ansistring): ansistring;
function SizeOfVariant(Value: variant): integer;
function FastStringReplace(const S: string; OldPattern: string; const NewPattern: string;
  Flags: TReplaceFlags = [rfReplaceAll]): string;
function JSONTypeToVariant(JSONData: TJSONData): variant;
procedure Explode(var a: TStringArray; Border, S: string);
function UrlEncode(url: string): string;
function IpToInt(IP: string): integer;
function IntToIp(IPint: integer): string;
function SortInputParams(AParamstr: string = ''): string;
function StringReplaceExt(const S: string; OldPattern, NewPattern: array of string; Flags: TReplaceFlags): string;
function toVisibleChars(AString:string):string;

implementation

procedure Explode(var a: TStringArray; Border, S: string);
var
  S2: string;
  i: integer;
begin
  i := 0;
  S2 := S + Border;
  setlength(a, 0);
  repeat
    setlength(a, i + 1);
    try
      a[i] := Copy(S2, 0, Pos(Border, S2) - 1);
    except
    end;
    Delete(S2, 1, Length(a[i] + Border));
    Inc(i);
  until S2 = '';
end;

function UrlEncode(url: string): string;
begin
  url := FastStringReplace(url, ' ', '%20', [rfReplaceAll]);
  Result := url;
end;

function IpToInt(IP: string): integer;
begin
  Result := 1;
end;

function IntToIp(IPint: integer): string;
begin
  Result := '0.0.0.0';
end;

function SortInputParams(AParamstr: string = ''): string;
var
  i: integer;
  SL,Params: TStringList;
begin
  SL := TStringList.Create;
  Params := TStringList.Create;
  i := 1;
  if AParamstr <> '' then
  begin
    SL.Delimiter := ' ';
    SL.DelimitedText := AParamstr;
  end
  else
    while ParamStr(i) <> '' do
    begin
      SL.Add(ParamStr(i));
      Inc(i);
    end;
  for i := 0 to SL.Count - 1 do
  begin
    if (SL[i][1] = '-') then
    begin
      Params.Add(SL[i] + '=');
    end
    else
    begin
      if Params.ValueFromIndex[Params.Count - 1] <> '' then
        Params.ValueFromIndex[Params.Count - 1] := Params.ValueFromIndex[Params.Count - 1] + ' ';
      Params.ValueFromIndex[Params.Count - 1] := Params.ValueFromIndex[Params.Count - 1] + SL[i];
    end;
  end;
  Result := Trim(Params.Text);
  Params.Free;
  SL.Free;
end;


function StringReplaceExt(const S: string; OldPattern, NewPattern: array of string; Flags: TReplaceFlags): string;
var
  i: integer;
begin
  Assert(Length(OldPattern) = (Length(NewPattern)));
  Result := S;
  for  i := Low(OldPattern) to High(OldPattern) do
    Result := FastStringReplace(Result, OldPattern[i], NewPattern[i], Flags);
end;

function toVisibleChars(AString: string): string;
begin
  Result := StringReplaceExt(AString, [#0,'\0',#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,#12,#13,#14,#15,#16,#17,#18,#19,#20,#21,#22,#23,#24,#25,#26,#27,#28,#29,#30,#31,#32], ['|','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░','░'], [rfReplaceAll]);
end;

function MD5File(Path: string): string;
begin
  Result := MD5Print(MDFile(Path, MD_VERSION_5, 1024));
end;

function PrepareMessage(Message: ansistring): ansistring;
begin
  Result := StringReplaceExt(Message, ['\', #0], ['\\', '\0'], [rfReplaceAll]);
end;

function GetPreparedMessage(Message: ansistring): ansistring;
begin
  Result := StringReplaceExt(Message, ['\\', '\0'], ['\', #0], [rfReplaceAll]);
end;

function SizeOfVariant(Value: variant): integer;
var
  vtype: TVarType;
begin
  vtype := VarType(Value);
  case vtype of
    varsmallint: Result := sizeOf(smallint);
    varinteger: Result := sizeOf(longint);
{$ifndef FPUNONE}
    varsingle: Result := sizeOf(single);
    vardouble: Result := sizeOf(double);
    vardate: Result := sizeOf(tdatetime);
{$endif}
    varcurrency: Result := sizeOf(currency);
    varolestr: Result := sizeOf(pwidechar);
    vardispatch: Result := sizeOf(pointer);
    varerror: Result := sizeOf(hresult);
    varboolean: Result := sizeOf(wordbool);
    varunknown: Result := sizeOf(pointer);
    // vardecimal : ( : );
    varshortint: Result := sizeOf(shortint);
    varbyte: Result := sizeOf(byte);
    varword: Result := sizeOf(word);
    varlongword: Result := sizeOf(dword);
    varint64: Result := sizeOf(int64);
    varqword: Result := sizeOf(qword);
    varstring: Result := length(Value){sizeOf(pointer)};
    varany: Result := sizeOf(pointer);
    vararray: Result := sizeOf(pvararray);
    varbyref: Result := sizeOf(pointer);
  end;
end;

function FastStringReplace(const S: string; OldPattern: string; const NewPattern: string;
  Flags: TReplaceFlags = [rfReplaceAll]): string;
var
  I, J, Idx: integer;
  IsEqual: boolean;
  UpperFindStr: string;
  pS: PChar;
  CanReplace: boolean;
begin
  {SOURCE: http://matrix.kladovka.net.ru/index.php?page=faststringreplace}
  if OldPattern = '' then
  begin
    Result := S;
    Exit;
  end;
  Result := '';
  if S = '' then
    Exit;
  if rfIgnoreCase in Flags then
  begin
    OldPattern := AnsiUpperCase(OldPattern);
    UpperFindStr := AnsiUpperCase(S);
    pS := PChar(UpperFindStr);
  end
  else
    pS := PChar(S);
  if Length(OldPattern) >= Length(NewPattern) then
  begin
    SetLength(Result, Length(S));
  end
  else
    SetLength(Result, (Length(S) + Length(OldPattern) + Length(NewPattern)) * 2);
  I := 1;
  Idx := 0;
  CanReplace := True;
  while I <= Length(S) do
  begin
    IsEqual := False;
    if CanReplace then
    begin
      if pS[I - 1] = OldPattern[1] then
      begin
        IsEqual := True;
        for J := 2 to Length(OldPattern) do
        begin
          if pS[I + J - 2] <> OldPattern[J] then
          begin
            IsEqual := False;
            Break;
          end;
        end;
        if IsEqual then
        begin
          for J := 1 to Length(NewPattern) do
          begin
            Inc(Idx);
            if Idx > Length(Result) then
              SetLength(Result, Length(Result) * 2);
            Result[Idx] := NewPattern[J];
          end;
          Inc(I, Length(OldPattern));
          if not (rfReplaceAll in Flags) then
            CanReplace := False;
        end;
      end;
    end;
    if not IsEqual then
    begin
      Inc(Idx);
      if Idx > Length(Result) then
        SetLength(Result, Length(Result) * 2);
      Result[Idx] := S[I];
      Inc(I);
    end;
  end;
  SetLength(Result, Idx);
end;

function JSONTypeToVariant(JSONData: TJSONData): variant;
begin
  case JSONData.JSONType of
    jtUnknown:
    begin
    end;
    jtNumber:
    begin
      Result := JSONData.AsInteger;
    end;
    jtString:
    begin
      Result := JSONData.AsString;
    end;
    jtBoolean:
    begin
      Result := JSONData.AsBoolean;
    end;
    jtNull:
    begin
    end;
    jtArray:
    begin
    end;
    jtObject:
    begin
    end;
  end;

end;

function Utf8ToCp1251(s: ansistring): ansistring;
var
  str: ansistring;
  i, j, n, x: integer;
  oem: ansistring;
  s1: ansistring;
  loc: ansistring;
  ex: boolean;

begin

  oem := oem + #208;
  oem := oem + #144;
  oem := oem + #208;
  oem := oem + #145;
  oem := oem + #208;
  oem := oem + #146;
  oem := oem + #208;
  oem := oem + #147;
  oem := oem + #208;
  oem := oem + #148;
  oem := oem + #208;
  oem := oem + #149;
  oem := oem + #208;
  oem := oem + #150;
  oem := oem + #208;
  oem := oem + #151;
  oem := oem + #208;
  oem := oem + #152;
  oem := oem + #208;
  oem := oem + #153;
  oem := oem + #208;
  oem := oem + #154;
  oem := oem + #208;
  oem := oem + #155;
  oem := oem + #208;
  oem := oem + #156;
  oem := oem + #208;
  oem := oem + #157;
  oem := oem + #208;
  oem := oem + #158;
  oem := oem + #208;
  oem := oem + #159;
  oem := oem + #208;
  oem := oem + #160;
  oem := oem + #208;
  oem := oem + #161;
  oem := oem + #208;
  oem := oem + #162;
  oem := oem + #208;
  oem := oem + #163;
  oem := oem + #208;
  oem := oem + #164;
  oem := oem + #208;
  oem := oem + #165;
  oem := oem + #208;
  oem := oem + #166;
  oem := oem + #208;
  oem := oem + #167;
  oem := oem + #208;
  oem := oem + #168;
  oem := oem + #208;
  oem := oem + #169;
  oem := oem + #208;
  oem := oem + #170;
  oem := oem + #208;
  oem := oem + #171;
  oem := oem + #208;
  oem := oem + #172;
  oem := oem + #208;
  oem := oem + #173;
  oem := oem + #208;
  oem := oem + #174;
  oem := oem + #208;
  oem := oem + #175;
  oem := oem + #208;
  oem := oem + #176;
  oem := oem + #208;
  oem := oem + #177;
  oem := oem + #208;
  oem := oem + #178;
  oem := oem + #208;
  oem := oem + #179;
  oem := oem + #208;
  oem := oem + #180;
  oem := oem + #208;
  oem := oem + #181;
  oem := oem + #208;
  oem := oem + #182;
  oem := oem + #208;
  oem := oem + #183;
  oem := oem + #208;
  oem := oem + #184;
  oem := oem + #208;
  oem := oem + #185;
  oem := oem + #208;
  oem := oem + #186;
  oem := oem + #208;
  oem := oem + #187;
  oem := oem + #208;
  oem := oem + #188;
  oem := oem + #208;
  oem := oem + #189;
  oem := oem + #208;
  oem := oem + #190;
  oem := oem + #208;
  oem := oem + #191;
  oem := oem + #209;
  oem := oem + #128;
  oem := oem + #209;
  oem := oem + #129;
  oem := oem + #209;
  oem := oem + #130;
  oem := oem + #209;
  oem := oem + #131;
  oem := oem + #209;
  oem := oem + #132;
  oem := oem + #209;
  oem := oem + #133;
  oem := oem + #209;
  oem := oem + #134;
  oem := oem + #209;
  oem := oem + #135;
  oem := oem + #209;
  oem := oem + #136;
  oem := oem + #209;
  oem := oem + #137;
  oem := oem + #209;
  oem := oem + #138;
  oem := oem + #209;
  oem := oem + #139;
  oem := oem + #209;
  oem := oem + #140;
  oem := oem + #209;
  oem := oem + #141;
  oem := oem + #209;
  oem := oem + #142;
  oem := oem + #209;
  oem := oem + #143;



  str := '';

  loc := '';
  ex := False;
  n := 192;

  for i := 0 to 63 do
    loc := loc + chr(n + i);


  for i := 1 to length(s) do
  begin
    if ex then
    begin
      ex := False;
      continue;
    end;
    j := 1;
    x := 1;
    while j < length(oem) do
    begin
      s1 := s[i] + s[i + 1];
      s1 := oem[j] + oem[j + 1];
      if s[i] + s[i + 1] = oem[j] + oem[j + 1] then
      begin
        str := str + loc[(j div 2) + 1];
        ex := True;
        Inc(x);
        break;
      end;
      Inc(j, 2);
    end;

    if not ex then
      str := str + s[i];
  end;
  Result := str;

end;

function cp1251ToUrf8(s: ansistring): ansistring;
var
  str: ansistring;
  i, j, n, x: integer;
  oem: ansistring;
  loc: ansistring;
  ex: boolean;

begin
  str := '';

  loc := 'АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдежзийклмнопрстуфхцчшщъыьэюя';

  oem := '';
  n := 192;
  for i := 0 to 63 do
    oem := oem + chr(n + i);

  for i := 1 to length(s) do
  begin
    ex := False;
    for j := 1 to length(oem) do
    begin
      if s[i] = oem[j] then
      begin
        str := str + loc[(j - 1) * 2 + 1] + loc[(j - 1) * 2 + 2]; //брать по 2 байта
        ex := True;
        break;
      end;
    end;
    if not ex then
      str := str + s[i];
  end;
  Result := str;
end;

function IntVersionToStr(version: integer; section: shortint; delimiter: char): string;
var
  s, Ver1: string;
  x, i: integer;
begin
  try
    Ver1 := IntToStr(version);
    x := (Trunc(length(ver1) / section) + 1) - (length(ver1) div section);
    if length(ver1) mod section <> 0 then
      for i := 0 to x do
        Ver1 := '0' + Ver1;

    Result := '';
    for i := 0 to section - 1 do
    begin
      s := copy(Ver1, 1, section);
      Result := Result + IntToStr(StrToInt(s));
      if i < section - 1 then
        Result := Result + delimiter;
      Delete(Ver1, 1, section);
    end;
  except
    Result := '';
  end;
end;

function StrToIntVersion(version: string; section: shortint; delimiter: char): integer;
var
  i, j: integer;
  t, s: string;
begin
  Result := 0;
  try
    for i := 0 to section - 1 do
    begin
      if pos(delimiter, version) <> 0 then
        s := copy(version, 1, pos(delimiter, version) - 1)
      else
        s := copy(version, 1, length(version));
      Delete(version, 1, length(s) + 1);
      while length(s) < section do
        s := '0' + s;
      t := t + s;
    end;
  finally
    Result := StrToInt(t);
  end;
end;

function MD5(str: string): string;
begin
  Result := MD5Print(MD5String(str));
end;

end.
