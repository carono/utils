unit CustomStringList;

interface
uses Classes{,Windows},SysUtils;

 type

 { TCustomStringList }

 TCustomStringList = class(TStringList)
 private
   function GetDelimitedText: string;
   procedure SetDelimitedText(const AValue: string);
 public
   property DelimitedText: string read GetDelimitedText write SetDelimitedText;
   constructor Create;
 end;
implementation
{$D+}
{$L+}

{ TCustomStringList }

Function QuoteString (Const S : String; Quote : String) : String;
Var
  I,J : Integer;
begin
  J:=0;
  Result:=S;
  for i:=1to length(s) do
   begin
     inc(j);
     if S[i]=Quote then
      begin
        System.Insert(Quote,Result,J);
        inc(j);
      end;
   end;
  Result:=Quote+Result+Quote;
end;

function TCustomStringList.GetDelimitedText: string;
begin
  inherited;
  TextLineBreakStyle:=tlbsCRLF;
end;

procedure TCustomStringList.SetDelimitedText(Const AValue: string);
var i,j:integer;
    aNotFirst:boolean;
begin
 CheckSpecialChars;
 BeginUpdate;

 i:=1;
 j:=1;
 aNotFirst:=false;

 try
  Clear;
  If StrictDelimiter then
    begin
    // Easier, faster loop.
    While I<=Length(AValue) do
      begin
      If (AValue[I] in [Delimiter,#0]) then
        begin
        Add(Copy(AValue,J,I-J));
        J:=I+1;
        end;
      Inc(i);
      end;
    If (Length(AValue)>0) then
      Add(Copy(AValue,J,I-J));
    end
  else
    begin
    while i<=length(AValue) do begin
     // skip delimiter
     if aNotFirst and (i<=length(AValue)) and (AValue[i]=Delimiter) then inc(i);

     // skip spaces
     while (i<=length(AValue)) and (Ord(AValue[i])<=Ord(' ')) do inc(i);

     // read next string
     if i<=length(AValue) then begin
      if AValue[i]=QuoteChar then begin
       // next string is quoted
       j:=i+1;
       while (j<=length(AValue)) and
             ( (AValue[j]<>QuoteChar) or
               ( (j+1<=length(AValue)) and (AValue[j+1]=QuoteChar) ) ) do begin
        if (j<=length(AValue)) and (AValue[j]=QuoteChar) then inc(j,2)
                                                          else inc(j);
       end;
       // j is position of closing quote
       Add( StringReplace (Copy(AValue,i+1,j-i-1),
                           QuoteChar+QuoteChar,QuoteChar, [rfReplaceAll]));
       i:=j+1;
      end else begin
       // next string is not quoted
       j:=i;
       while (j<=length(AValue)) and
            { (Ord(AValue[j])>Ord(' ')) and}
             (AValue[j]<>Delimiter) do inc(j);
       Add( Copy(AValue,i,j-i));
       i:=j;
      end;
     end else begin
      if aNotFirst then Add('');
     end;

     // skip spaces
     while (i<=length(AValue)) and (Ord(AValue[i])<=Ord(' ')) do inc(i);

     aNotFirst:=true;
    end;
    end;
 finally
   EndUpdate;
 end;
end;

constructor TCustomStringList.Create;
begin
  inherited Create;
  TextLineBreakStyle:=tlbsCRLF;
end;

end.