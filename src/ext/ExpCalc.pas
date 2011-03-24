unit ExpCalc;

{.$I compiler.inc}

{.$IFDEF DELPHI_NET}
{.$UNSAFECODE ON}
{.$ENDIF}

(*
  TITLE Calculator
  DESC  Simple calculator for standard expressions
  AUTOR Ivlev M.Dmitry (mailto:Dimon@Diogen.nstu.nsk.su)
  PATCHED Sergey Pedora (mailto:Sergey@mail.fact400.ru)

  Syntax:
  0xABCD, 0ABCDh, $ABCD - Hex number
  0b0101, 01010b,       - Binary number
  90`15`2               - Degree
   Operators by priorities:
    {  7} () (BRACES)
    {  6} ** (POWER),
    {  5} ~ (INVERSE), ! (NOT),
    {  4} * (MUL), / (DIV), % (MOD), %% (PERSENT),
    {  3} + (ADD), - (SUB),
    {  2} < (LT), <= (LE), == (EQ), <> != (NE), >= (GE), > (GT),
    {  1} | (OR), ^ (XOR), & (AND),
*)

interface

uses SysUtils, Classes, Contnrs;

type
  TToken = (
    { } tkEOF, tkERROR, tkASSIGN,
    {7} tkLBRACE, tkRBRACE, tkNUMBER, tkIDENT, tkSEMICOLON,
    {6} tkPOW,
    {5} tkINV, tkNOT,
    {4} tkMUL, tkDIV, tkMOD, tkPER,
    {3} tkADD, tkSUB,
    {2} tkLT, tkLE, tkEQ, tkNE, tkGE, tkGT,
    {1} tkOR, tkXOR, tkAND
  );

  ECalculate = class(Exception);
  TCalcCBType = (ctGetValue, ctSetValue, ctFunction);

  TQueryCallback = function (ctype: TCalcCBType; const Name: string; var Res: Double): Boolean of object;

  TNamedVar = class(TObject)
    Name: string;
    Value: Double;
  end;

  TCalculator = class(TPersistent)
  private
    fvalue: Double;
    svalue: string[32];

    FExpression: string;
    FLineNo: Integer;
    FPtr: PChar;
    FQueryCallback: TQueryCallback;
    FToken: TToken;
    FVars: TObjectList;

    function  GetResult(): Double;
    function  GetVar(const Name: string): Double;
    procedure SetVar(const Name: string; Value: Double);
    procedure lex();
    procedure term(var R: Double);
    procedure expr1(var R: Double);
    procedure expr2(var R: Double);
    procedure expr3(var R: Double);
    procedure expr4(var R: Double);
    procedure expr5(var R: Double);
    procedure expr6(var R: Double);
    procedure start(var R: Double);
  protected
    function Callback(ctype: TCalcCBType; const Name: string; var Res: Double): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearVars();
    function  NameOf(Index: Word): string;

    property Expression: string read FExpression write FExpression;
    property QueryCallback: TQueryCallback read FQueryCallback write FQueryCallback;
    property Result: Double read GetResult;
    property Vars[const Name: string]: Double read GetVar write SetVar;
  end;

resourcestring
  SSyntaxError = 'Syntax error.';
  SFunctionError = 'Unknown function or variable';
  SInvalidDegree = 'Invalid degree %s';

{ Degree convertation functions }
function DegreeToStr(Angle: Extended): string;
function StrToDegree(const S: AnsiString): Extended;

implementation

function DefCalcProc(ctype: TCalcCBType; const S: string; var V: Double): Boolean;
begin
  Result := True;

  case ctype of
    ctGetValue: begin
      if (S = 'pi') then V := Pi
      else
      if (S = 'e') then V := 2.718281828
      else
        Result := False;
    end;

    ctSetValue: begin
      Result := False;
    end;

    ctFunction: begin
      if (S = 'round') then V := Round(V)
      else
      if (S = 'trunc') then V := Trunc(V)
      else
      if (S = 'int') then V := Int(V)
      else
      if (S = 'frac') then V := Frac(V)
      else
      if (S = 'sin') then V := Sin(V)
      else
      if (S = 'cos') then V := Cos(V)
      else
      if (S = 'tan') then V := Sin(V)/Cos(V)
      else
      if (S = 'atan') then V := ArcTan(V)
      else
      if (S = 'ln') then V := Ln(V)
      else
      if (S = 'exp') then V := Exp(V)
      else
      if (S = 'sign') then begin
        if (V > 0) then V := 1
        else
        if (V < 0) then V := -1;
      end
      else
      if (S = 'sgn') then begin
        if (V > 0) then V := 1
        else
        if (V < 0) then V := 0;
      end
      else
      if (S = 'xsgn') then begin
        if (V < 0) then V := 0;
      end
      else Result := False;
    end;
  end;
end;

function fmod(x, y: Extended): Extended;
begin
  Result := x - Int(x / y) * y;
end;

function power(x, y: Double): Double;
begin
  if (x = 0)
  then Result := 1.0
  else Result := Exp(Ln(x) * y);
end;

function DegreeToStr(Angle: Extended): string;
var
  ang, min, sec: Longint;
begin
  Result := '';
  if (Abs(Angle) < 1E-20) then Angle := 0.0;
  if (Angle < 0) then begin
    Result := '-';
    Angle := -Angle;
  end;
  Angle := Angle * 180.0 / Pi; ang := Trunc(Angle+5E-10);
  Angle := (Angle - ang) * 60; min := Trunc(Angle+5E-10);
  Angle := (Angle - min) * 60; sec := Trunc(Angle+5E-10);
  Result := Result + IntToStr(ang)+'`';
  if (min <> 0) then Result := Result + IntToStr(min);
  if (sec <> 0) then Result := Result + '`' + IntToStr(sec);
end;

function StrToDegree(const S: AnsiString): Extended; 
var
  ptr: PChar;
  fvalue, frac, sign: Extended;
begin
  ptr := PChar(S);
  sign := 1;
  if (ptr^ in ['-', '+']) then begin
    if (ptr^ = '-') then sign := -sign;
    Inc(ptr);
  end;
  if not(ptr^ in ['0', '9'])
  then EConvertERROR.CreateFmt(SInvalidDegree, [S]);
  frac := 0;
  while (ptr^ in ['0'..'9']) do begin
    frac := frac * 10 + (Ord(ptr^) - Ord('0'));
    Inc(ptr);
  end;
  fvalue := frac;
  if (ptr^ = '`') then begin
    fvalue := fvalue * Pi / 180.0;
    Inc(ptr); frac := 0;
    while (ptr^ in ['0'..'9']) do begin
      frac := frac * 10 + (Ord(ptr^) - Ord('0'));
      Inc(ptr);
    end;
    fvalue := fvalue + (frac * Pi / 180.0 / 60);
    if (ptr^ = '`') then begin
      Inc(ptr); frac := 0;
      while (ptr^ in ['0'..'9']) do begin
        frac := frac * 10 + (Ord(ptr^) - Ord('0'));
        Inc(ptr);
      end;
      fvalue := fvalue + (frac * Pi / 180.0 / 60 / 60);
    end;
  end;
  fvalue := fmod(fvalue, 2*Pi);
end;

{ TCalculator }

procedure RaiseError(const Msg: string);
begin
  raise ECalculate.Create(Msg);
end;

function tofloat(B: Boolean): Double;
begin
  if (B)
  then Result := 1.0
  else Result := 0.0;
end;

{ yylex like function }

procedure TCalculator.lex();
label
  Error;
var
  c, sign: Char;
  frac: Double;
  exp: Longint;
  s_pos: PChar;

  function ConvertNumber(first, last: PChar; base: Word): Boolean;
  var
    c: Byte;
  begin
    fvalue := 0;
    while first < last do begin
      c := Ord(first^) - Ord('0');
      if (c > 9) then begin
        Dec(c, Ord('A') - Ord('9') - 1);
        if (c > 15) then Dec(c, Ord('a') - Ord('A'));
      end;
      if (c >= base) then break;
      fvalue := fvalue * base + c;
      Inc(first);
    end;
    Result := (first = last);
  end;

begin
  { skip blanks }
  while (FPtr^ <> #0) do begin
    if (FPtr^ = #13) then Inc(FLineNo)
    else if (FPtr^ > ' ') then break;
    Inc(FPtr);
  end;

  { check EOF }
  FToken := tkEOF;
  if (FPtr^ = #0) then Exit;

  s_pos := FPtr;
  FToken := tkNUMBER;

  { match pascal like hex number }
  if (FPtr^ = '$') then begin
    Inc(FPtr);
    while (FPtr^ in ['0'..'9', 'A'..'H', 'a'..'h']) do Inc(FPtr);
    if not ConvertNumber(s_pos, FPtr, 16) then goto Error;
    Exit;
  end;

  { match numbers }
  if (FPtr^ in ['0'..'9']) then begin

    { C like mathing }
    if (FPtr^ = '0') then begin
      Inc(FPtr);

      { match C like hex number }
      if (FPtr^ in ['x', 'X']) then begin
        Inc(FPtr);
        s_pos := FPtr;
        while (FPtr^ in ['0'..'9', 'A'..'H', 'a'..'h']) do Inc(FPtr);
        if not ConvertNumber(s_pos, FPtr, 16) then goto Error;
        Exit;
      end;

      { match C like binary number }
      if (FPtr^ in ['b', 'B']) then begin
        Inc(FPtr);
        s_pos := FPtr;
        while (FPtr^ in ['0'..'1']) do Inc(FPtr);
        if not ConvertNumber(s_pos, FPtr, 2) then goto Error;
        Exit;
      end;
    end;

    while (FPtr^ in ['0'..'9', 'A'..'F', 'a'..'f']) do Inc(FPtr);

    { match assembler like hex number }
    if (FPtr^ in ['H', 'h']) then begin
      if not ConvertNumber(s_pos, FPtr, 16) then goto Error;
      Inc(FPtr);
      Exit;
    end;

    { match assembler like binary number }
    if (FPtr^ in ['B', 'b']) then begin
      if not ConvertNumber(s_pos, FPtr, 2) then goto Error;
      Inc(FPtr);
      Exit;
    end;

    { match simple decimal number }
    if not ConvertNumber(s_pos, FPtr, 10) then goto Error;

    { match degree number }
    if (FPtr^ = '`') then begin
      fvalue := fvalue * Pi / 180.0;
      Inc(FPtr); frac := 0;
      while (FPtr^ in ['0'..'9']) do begin
        frac := frac * 10 + (Ord(FPtr^) - Ord('0'));
        Inc(FPtr);
      end;
      fvalue := fvalue + (frac * Pi / 180.0 / 60);
      if (FPtr^ = '`') then begin
      Inc(FPtr); frac := 0;
      while (FPtr^ in ['0'..'9']) do begin
        frac := frac * 10 + (Ord(FPtr^) - Ord('0'));
        Inc(FPtr);
      end;
      fvalue := fvalue + (frac * Pi / 180.0 / 60 / 60);
      end;
      fvalue := fmod(fvalue, 2*Pi);
      Exit;
    end;

    { match float numbers }
    if (FPtr^ = '.') then begin
      Inc(FPtr);
      frac := 1;
      while (FPtr^ in ['0'..'9']) do begin
        frac := frac / 10;
        fvalue := fvalue + frac * (Ord(FPtr^) - Ord('0'));
        Inc(FPtr);
      end;
    end;

    if (FPtr^ in ['E', 'e']) then begin
      Inc(FPtr);
      exp := 0;
      sign := FPtr^;
      if (FPtr^ in ['+', '-']) then Inc(FPtr);
      if not (FPtr^ in ['0'..'9']) then goto Error;
      while (FPtr^ in ['0'..'9']) do begin
        exp := exp * 10 + Ord(FPtr^) - Ord('0');
        Inc(FPtr);
      end;
      if (exp = 0)
      then fvalue := 1.0
      else if (sign = '-')
      then while exp > 0 do begin fvalue := fvalue * 10; Dec(exp); end
      else while exp > 0 do begin fvalue := fvalue / 10; Dec(exp); end
    end;
    Exit;
  end;

  { match identifiers }
  if (FPtr^ in ['A'..'Z','a'..'z','_']) then begin
    svalue := FPtr^;
    Inc(FPtr);
    while (FPtr^ in ['A'..'Z','a'..'z','0'..'9','_']) and
          (Length(svalue) < sizeof(svalue)-1) do
    begin
      svalue := svalue + FPtr^;
      Inc(FPtr);
    end;
    FToken := tkIDENT;
    Exit;
  end;

  { match operators }
  c := FPtr^;
  Inc(FPtr);
  
  case c of
    '=': begin
      FToken := tkASSIGN;
      if (FPtr^ = '=') then begin Inc(FPtr); FToken := tkEQ; end;
    end;
    '+': begin FToken := tkADD; end;
    '-': begin FToken := tkSUB; end;
    '*': begin FToken := tkMUL;
      if (FPtr^ = '*') then begin Inc(FPtr); FToken := tkPOW; end;
    end;
    '/': begin FToken := tkDIV; end;
    '%': begin FToken := tkMOD;
      if (FPtr^ = '%') then begin Inc(FPtr); FToken := tkPER; end;
    end;
    '~': begin FToken := tkINV; end;
    '^': begin FToken := tkXOR; end;
    '&': begin FToken := tkAND; end;
    '|': begin FToken := tkOR; end;
    '<': begin FToken := tkLT;
      if (FPtr^ = '=') then begin Inc(FPtr); FToken := tkLE; end else
      if (FPtr^ = '>') then begin Inc(FPtr); FToken := tkNE; end;
    end;
    '>': begin FToken := tkGT;
      if (FPtr^ = '=') then begin Inc(FPtr); FToken := tkGE; end else
      if (FPtr^ = '<') then begin Inc(FPtr); FToken := tkNE; end;
    end;
    '!': begin FToken := tkNOT;
      if (FPtr^ = '=') then begin Inc(FPtr); FToken := tkNE; end;
    end;
    '(': begin FToken := tkLBRACE; end;
    ')': begin FToken := tkRBRACE; end;
    ';': begin FToken := tkSEMICOLON end;
    else begin FToken := tkERROR; Dec(FPtr); end;
  end;
  Exit;

Error:
  FToken := tkERROR;
end;

(*
// LL grammatic for calculator, priorities from down to up
//
// start: expr6;
// expr6: expr5 { & expr5 | ^ expr5 | & expr5 }*;
// expr5: expr4 { < expr4 | > expr4 | <= expr4 | >= expr4 | != expr4 | == expr4 }*;
// expr4: expr3 { + expr3 | - expr3 }*;
// expr3: expr2 { * expr2 | / expr2 | % expr2 | %% expr2 }*;
// expr2: expr1 { ! expr1 | ~ expr1 | - expr1 | + expr1 };
// expr1: term ** term
// term: tkNUMBER | tkIDENT | (start) | tkIDENT(start) | tkIDENT = start;
//
*)

procedure TCalculator.term(var R: Double);
var
  S: string[32];
begin
  case FToken of
    tkNUMBER: begin
      R := fvalue;
      lex();
    end;
    tkLBRACE: begin
      lex();
      expr6(R);
      if (FToken = tkRBRACE)
      then lex()
      else RaiseError(SSyntaxError);
    end;
    tkIDENT: begin
      S := LowerCase(svalue);
      lex();
      if (FToken = tkLBRACE) then begin
        lex();
        expr6(R);

        if (FToken = tkRBRACE)
        then lex()
        else RaiseError(SSyntaxError);

        if not Callback(ctFunction, s, R)
        then RaiseError(SFunctionError+' "'+s+'".');
      end
      else
      if (FToken = tkASSIGN) then begin
        lex();
        expr6(R);
        if not Callback(ctSetValue, s, R)
        then RaiseError(SFunctionError+' "'+s+'".');
      end
      else
      if not Callback(ctGetValue, s, R)
      then RaiseError(SFunctionError+' "'+s+'".');
    end;
    else RaiseError('Syntax error.');
  end;
end;

procedure TCalculator.expr1(var R: Double);
var
  V: Double;
begin
  term(R);
  if (FToken = tkPOW) then begin
    lex();
    term(V);
    R := power(R, V);
  end;
end;

procedure TCalculator.expr2(var R: Double);
var
  oldt: TToken;
begin
  if (FToken in [tkNOT, tkINV, tkADD, tkSUB]) then begin
    oldt := FToken;
    lex();
    expr2(R);
    case oldt of
      tkNOT: R := tofloat(not(Boolean(Trunc(R))));
      tkINV: R := (not Trunc(R));
      tkADD: ;
      tkSUB: R := -R;
    end;
  end
  else expr1(R);
end;

procedure TCalculator.expr3(var R: Double);
var
  V: Double;
  oldt: TToken;
begin
  expr2(R);
  while (FToken in [tkMUL, tkDIV, tkMOD, tkPER]) do begin
    oldt := FToken;
    lex();
    expr2(V);
    case oldt of
      tkMUL: R := R * V;
      tkDIV: R := R / V;
      tkMOD: R := Trunc(R) mod Trunc(V);
      tkPER: R := R * V / 100.0;
    end;
  end;
end;

procedure TCalculator.expr4(var R: Double);
var
  V: Double;
  oldt: TToken;
begin
  expr3(R);
  while (FToken in [tkADD, tkSUB]) do begin
    oldt := FToken;
    lex();
    expr3(V);
    case oldt of
      tkADD: R := R + V;
      tkSUB: R := R - V;
    end;
  end;
end;

procedure TCalculator.expr5(var R: Double);
var
  V: Double;
  oldt: TToken;
begin
  expr4(R);
  while (FToken in [tkLT, tkLE, tkEQ, tkNE, tkGE, tkGT]) do begin
    oldt := FToken;
    lex();
    expr4(V);
    case oldt of
      tkLT: R := tofloat(R < V);
      tkLE: R := tofloat(R <= V);
      tkEQ: R := tofloat(R = V);
      tkNE: R := tofloat(R <> V);
      tkGE: R := tofloat(R >= V);
      tkGT: R := tofloat(R > V);
    end;
  end;
end;

procedure TCalculator.expr6(var R: Double);
var
  V: Double;
  oldt: TToken;
begin
  expr5(R);
  while (FToken in [tkOR, tkXOR, tkAND]) do begin
    oldt := FToken;
    lex();
    expr5(V);
    case oldt of
      tkOR : R := Trunc(R) or  Trunc(V);
      tkAND: R := Trunc(R) and Trunc(V);
      tkXOR: R := Trunc(R) xor Trunc(V);
    end;
  end;
end;

procedure TCalculator.start(var R: Double);
begin
  expr6(R);

  while (FToken = tkSEMICOLON) do begin
    lex();
    expr6(R);
  end;

  if not(FToken = tkEOF)
  then RaiseError(SSyntaxError);
end;

constructor TCalculator.Create;
begin
  inherited Create;
  FVars := TObjectList.Create(True);
end;

destructor TCalculator.Destroy;
begin
  ClearVars;
  FVars.Free;
  inherited Destroy;
end;

procedure TCalculator.SetVar(const Name: string; Value: Double);
var
  i: Integer;
  V: TNamedVar;
begin
  V := nil;
  for i := 0 to FVars.Count - 1 do begin
    if (CompareStr(TNamedVar(FVars[i]).Name, Name) = 0) then begin
      V := TNamedVar(FVars[i]);
      Break;
    end;
  end;

  if (V = nil) then begin
    V := TNamedVar.Create;
    V.Name := Name;
    FVars.Add(V);
  end;

  V.Value := Value;
end;

function TCalculator.GetVar(const Name: string): Double;
var
  i: Integer;
  V: TNamedVar;
begin
  for i := 0 to FVars.Count - 1 do begin
    V := TNamedVar(FVars[i]);
    if (CompareStr(V.Name, Name) = 0) then begin
      Result := V.Value;
      Exit;
    end;
  end;
  raise Ecalculate.Create(SFunctionError+' "'+name+'".');
end;

function TCalculator.GetResult(): Double;
begin
  FPtr := PChar(FExpression);
  FLineNo := 1;
  lex();
  start(Result);
end;

function TCalculator.Callback(ctype: TCalcCBType;
  const Name: string; var Res: Double): Boolean;
begin
  Result := DefCalcProc(ctype, Name, Res);
  if Result then Exit;

  if Assigned(FQueryCallback) then Result := FQueryCallback(ctype, Name, Res);
  if Result then Exit;

  Result := True;

  case ctype of
    ctGetValue: Res := Vars[Name];
    ctSetValue: Vars[Name] := Res;
    ctFunction: Result := False;
  end;
end;

function TCalculator.NameOf(Index: Word): string;
begin
  Result := TNamedVar(FVars[Index]).Name;
end;

procedure TCalculator.ClearVars();
begin
  FVars.Clear;
end;

const
  C_NOERROR             = 0;
  C_ERR_BRACKETS        = 1;
  C_ERR_EMPTY_EXPR      = 2;
  C_ERR_UNKNOWN_FUNC    = 3;
  C_ERR_FUNCTION_ERR    = 4;
  C_ERR_DEV_BY_ZERO     = 5;
  C_ERR_CANNOT_SOLVE    = 6;
  C_ERR_BAD_NUMBER      = 7;

function CalcErrorToStr(Error: Word): String;
begin
  case Error of
    C_NOERROR:          Result := 'No error';
    C_ERR_BRACKETS:     Result := 'Could not parse brackets';
    C_ERR_EMPTY_EXPR:   Result := 'Empty expression';
    C_ERR_UNKNOWN_FUNC: Result := 'Unknown function';
    C_ERR_FUNCTION_ERR: Result := 'Error while evaluating function';
    C_ERR_DEV_BY_ZERO:  Result := 'Division by sezo';
    C_ERR_CANNOT_SOLVE: Result := 'Error while evaluating expression';
    C_ERR_BAD_NUMBER:   Result := 'Incorrect number';
    else Result := 'Unknown error';
  end;
end;

end.
