unit StStrms;

interface

uses
  SysUtils, Classes;

type
  EStException = class(Exception)
  protected
  public
  end;

type
  EStExceptionClass = class of EStException;
  EStBufStreamError = class(EStException);

  TStMemSize = Integer;

  TStBufferedStream = class(TStream)
  private
    FBufCount: TStMemSize;   {count of valid bytes in buffer}
    FBuffer: PAnsiChar;    {buffer into underlying stream}
    FBufOfs: Longint;      {offset of buffer in underlying stream}
    FBufPos: TStMemSize;   {current position in buffer}
    FBufSize: TStMemSize;   {size of buffer}
    FDirty: Boolean;      {has data in buffer been changed?}
    FSize: Int64;      {size of underlying stream}
    FStream: TStream;      {underlying stream}
  protected
    procedure bsInitForNewStream(); virtual;
    function bsReadChar(var aCh: AnsiChar): Boolean;
    procedure bsReadFromStream();
    procedure bsWriteToStream();
  public
    constructor Create(aStream: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

type
  TStLineTerminator = (ltLF {#10}, ltCRLF {#13#10});

  TStAnsiTextStream = class(TStBufferedStream)
  private
    FLineTerm: TStLineTerminator;
  public
    constructor Create(aStream: TStream);
    destructor Destroy; override;

    function AtEndOfStream(): Boolean;
    function ReadLine(): string;
    procedure WriteLine(const aSt: string);
  end;

implementation

const
  LineTerm: array [TStLineTerminator] of array [0..1] of AnsiChar = (#10, #13#10);

const
  stscBadOrigin        = 263;
  stscNilStream        = 270;
  stscNoSeekForRead    = 271;
  stscNoSeekForWrite   = 272;
  stscCannotWrite      = 273;

  stscBadOriginS       = 'Bad origin parameter for call to Seek';

{--- Helper routines ---------------------------------------------------------}

function MinLong(A, B: Longint): Longint;
begin
  if (A < B)
  then Result := A
  else Result := B;
end;

procedure RaiseStError(ExceptionClass: EStExceptionClass; Code: LongInt);
var
  E: EStException;
begin
  E := ExceptionClass.Create('Code');
  raise E;
end;

{ TStBufferedStream }

constructor TStBufferedStream.Create(aStream: TStream);
begin
  inherited Create;

  {allocate the buffer}
  FBufSize := {4096} 32 * 1024;
  GetMem(FBuffer, FBufSize);

  {save the stream}
  if (aStream = nil) then RaiseStError(EStBufStreamError, stscNilStream);
  FStream := aStream;

  bsInitForNewStream;
end;

destructor TStBufferedStream.Destroy;
begin
  if (FBuffer <> nil) then begin
    if FDirty then bsWriteToStream;
    FreeMem(FBuffer, FBufSize);
  end;

  inherited Destroy;
end;

procedure TStBufferedStream.bsInitForNewStream();
begin
  FSize := FStream.Size;
  FBufCount := 0;
  FBufOfs := 0;
  FBufPos := 0;
  FDirty := false;
end;

function TStBufferedStream.bsReadChar(var aCh: AnsiChar): Boolean;
begin
  {is there anything to read?}
  if (FSize = (FBufOfs + FBufPos)) then begin
    Result := False;
    Exit;
  end;

  {if we get here, we'll definitely read a character}
  Result := True;

  {make sure that the buffer has some data in it}
  if (FBufCount = 0)
  then bsReadFromStream()
  else
  if (FBufPos = FBufCount) then begin
    if FDirty
    then bsWriteToStream();

    FBufPos := 0;
    Inc(FBufOfs, FBufSize);
    bsReadFromStream();
  end;

  {get the next character}
  aCh := AnsiChar(FBuffer[FBufPos]);
  Inc(FBufPos);
end;

procedure TStBufferedStream.bsReadFromStream();
var
  NewPos: Longint;
begin
  {assumptions: FBufOfs is where to read the buffer
                FBufSize is the number of bytes to read
                FBufCount will be the number of bytes read}
  NewPos := FStream.Seek(FBufOfs, soFromBeginning);

  if (NewPos <> FBufOfs)
  then RaiseStError(EStBufStreamError, stscNoSeekForRead);

  FBufCount := FStream.Read(FBuffer^, FBufSize);
end;

procedure TStBufferedStream.bsWriteToStream();
var
  NewPos, BytesWritten: Longint;
begin
  {assumptions: FDirty is true
                FBufOfs is where to write the buffer
                FBufCount is the number of bytes to write
                FDirty will be set false afterwards}
  NewPos := FStream.Seek(FBufOfs, soFromBeginning);

  if (NewPos <> FBufOfs)
  then RaiseStError(EStBufStreamError, stscNoSeekForWrite);

  BytesWritten := FStream.Write(FBuffer^, FBufCount);

  if (BytesWritten <> FBufCount)
  then RaiseStError(EStBufStreamError, stscCannotWrite);

  FDirty := false;
end;

function TStBufferedStream.Read(var Buffer; Count: Longint): Longint;
var
  BytesToGo, BytesToRead, DestPos: Longint;
  BufAsBytes: TByteArray absolute Buffer;
begin
  {calculate the number of bytes we could read if possible}
  BytesToGo := MinLong(Count, FSize - (FBufOfs + FBufPos));
  {we will return this number of bytes or raise an exception}
  Result := BytesToGo;
  {are we going to read some data after all?}
  if (BytesToGo > 0) then begin
    {make sure that the buffer has some data in it}
    if (FBufCount = 0) then bsReadFromStream;
    {read as much as we can from the current buffer}
    BytesToRead := MinLong(BytesToGo, FBufCount - FBufPos);
    {transfer that number of bytes}
    Move(FBuffer[FBufPos], BufAsBytes[0], BytesToRead);
    {update our counters}
    Inc(FBufPos, BytesToRead);
    Dec(BytesToGo, BytesToRead);
    {if we have more bytes to read then we've reached the end of the
     buffer and so we need to read another, and another, etc}
    DestPos := 0;
    while BytesToGo > 0 do begin
      {if the current buffer is dirty, write it out}
      if FDirty then bsWriteToStream;
      {position and read the next buffer}
      FBufPos := 0;
      Inc(FBufOfs, FBufSize);
      bsReadFromStream;
      {calculate the new destination position, and the number of bytes
       to read from this buffer}
      Inc(DestPos, BytesToRead);
      BytesToRead := MinLong(BytesToGo, FBufCount - FBufPos);
      {transfer that number of bytes}
      Move(FBuffer[FBufPos], BufAsBytes[DestPos], BytesToRead);
      {update our counters}
      Inc(FBufPos, BytesToRead);
      Dec(BytesToGo, BytesToRead);
    end;
  end;
end;

function TStBufferedStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  NewPos, NewOfs: Longint;
begin
  {optimization: to help code that just wants the current stream
   position (ie, reading the Position property), check for this as a
   special case}
  if (Offset = 0) and (Origin = soFromCurrent) then begin
    Result := FBufOfs + FBufPos;
    Exit;
  end;
  {calculate the desired position}
  case Origin of
    soFromBeginning: NewPos := Offset;
    soFromCurrent: NewPos := (FBufOfs + FBufPos) + Offset;
    soFromEnd: NewPos := FSize + Offset;
  else
    RaiseStError(EStBufStreamError, stscBadOrigin);
    NewPos := 0; {to fool the compiler's warning--we never get here}
  end;
  {force the new position to be valid}
  if (NewPos < 0) then
    NewPos := 0
  else if (NewPos > FSize) then
    NewPos := FSize;
  {calculate the offset for the buffer}
  NewOfs := (NewPos div FBufSize) * FBufSize;
  {if the offset differs, we have to move the buffer window}
  if (NewOfs <> FBufOfs) then begin
    {check to see whether we have to write the current buffer to the
     original stream first}
    if FDirty then
      bsWriteToStream;
    {mark the buffer as empty}
    FBufOfs := NewOfs;
    FBufCount := 0;
  end;
  {set the position within the buffer}
  FBufPos := NewPos - FBufOfs;
  Result := NewPos;
end;

function TStBufferedStream.Write(const Buffer; Count: Longint): Longint;
var
  BytesToGo: Longint;
  BytesToWrite: Longint;
  BufAsBytes: TByteArray absolute Buffer;
  DestPos: Longint;
begin
  {calculate the number of bytes we should be able to write}
  BytesToGo := Count;
  {we will return this number of bytes or raise an exception}
  Result := BytesToGo;
  {are we going to write some data?}
  if (BytesToGo > 0) then begin
    {try and make sure that the buffer has some data in it}
    if (FBufCount = 0) and ((FBufOfs + FBufPos) < FSize) then
      bsReadFromStream;
    {write as much as we can to the current buffer}
    BytesToWrite := MinLong(BytesToGo, FBufSize - FBufPos);
    {transfer that number of bytes}
    Move(BufAsBytes[0], FBuffer[FBufPos], BytesToWrite);
    FDirty := true;
    {update our counters}
    inc(FBufPos, BytesToWrite);
    if (FBufCount < FBufPos) then begin
      FBufCount := FBufPos;
      FSize := FBufOfs + FBufPos;
    end;
    dec(BytesToGo, BytesToWrite);
    {if we have more bytes to write then we've reached the end of the
     buffer and so we need to write another, and another, etc}
    DestPos := 0;
    while BytesToGo > 0 do begin
      {as the current buffer is dirty, write it out}
      bsWriteToStream;
      {position and read the next buffer, if required}
      FBufPos := 0;
      inc(FBufOfs, FBufSize);
      if (FBufOfs < FSize) then
        bsReadFromStream
      else
        FBufCount := 0;
      {calculate the new destination position, and the number of bytes
       to write to this buffer}
      inc(DestPos, BytesToWrite);
      BytesToWrite := MinLong(BytesToGo, FBufSize - FBufPos);
      {transfer that number of bytes}
      Move(BufAsBytes[DestPos], FBuffer[0], BytesToWrite);
      FDirty := true;
      {update our counters}
      inc(FBufPos, BytesToWrite);
      if (FBufCount < FBufPos) then begin
        FBufCount := FBufPos;
        FSize := FBufOfs + FBufPos;
      end;
      dec(BytesToGo, BytesToWrite);
    end;
  end;
end;

{ TStAnsiTextStream }

constructor TStAnsiTextStream.Create(aStream: TStream);
begin
  inherited Create(aStream);
  FLineTerm := ltCRLF;
end;

destructor TStAnsiTextStream.Destroy;
begin
  inherited Destroy;
end;

function TStAnsiTextStream.AtEndOfStream(): Boolean;
begin
  Result := FSize = (FBufOfs + FBufPos);
end;

function TStAnsiTextStream.ReadLine(): string;
var
  CurPos, EndPos, Len: Longint;
  Done: Boolean;
  Ch, PrevCh: AnsiChar;
begin
  CurPos := FBufOfs + FBufPos;
  Ch := #0;
  Done := False;
  while not Done do begin
    PrevCh := Ch;
    if not bsReadChar(Ch) then begin
      Done := True;
      EndPos := FBufOfs + FBufPos;
      Len := EndPos - CurPos;
    end else begin
      if (Ch = #10) then begin
        Done := True;
        EndPos := FBufOfs + FBufPos;
        if (PrevCh = #13)
        then Len := EndPos - CurPos - 2
        else Len := EndPos - CurPos - 1;
      end;
    end;
  end;

  ///

  SetLength(Result, Len);
  Seek(CurPos, soFromBeginning);
  Read(Result[1], Len);
  Seek(EndPos, soFromBeginning);
end;

procedure TStAnsiTextStream.WriteLine(const aSt: string);
var
  aLen: TStMemSize;
begin
  aLen := Length(aSt);

  if (aLen > 0)
  then Write(aSt[1], aLen);

  case FLineTerm of
      ltLF: Write(LineTerm[ltLF], 1);
    ltCRLF: Write(LineTerm[ltCRLF], 2);
  end;
end;

end.
