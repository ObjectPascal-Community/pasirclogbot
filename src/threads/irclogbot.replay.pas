unit IRCLogBot.Replay;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, SyncObjs
, Contnrs
, IdIRC

;

type
{ TReplayThread }
  TReplayThread = class(TThread)
  private
    FCriticalSection: TCriticalSection;
    FIRC: TIdIRC;
    FQueue: TFPObjectList;
  protected
    procedure Execute; override;
  public
    constructor Create(AIRC: TIdIRC);
    destructor Destroy; override;

    procedure Add(const ANick: String; const AList: TStringList);
  published
  end;

implementation

uses
  IRCLogBot.Common
;
type
{ TReplayBundle }
  TReplayBundle = class(TObject)
  private
    FNick: String;
    FLines: TStringList;
  protected
  public
    constructor Create(const ANick: String; const ALines: TStringList);
    destructor Destroy; override;

    property Nick: String
      read FNick;
    property Lines: TStringList
      read FLines;
  published
  end;

{ TReplayBundle }

constructor TReplayBundle.Create(const ANick: String;
  const ALines: TStringList);
begin
  FNick:= ANick;
  FLines:= TStringList.Create;
  FLines.Text:= ALines.Text;
end;

destructor TReplayBundle.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

{ TReplayThread }

procedure TReplayThread.Execute;
var
  bundle: TReplayBundle;
  line: String;
  index: Integer = 0;
begin
  while not Terminated do
  begin
    FCriticalSection.Acquire;
    try
      bundle:= nil;
      if FQueue.Count > 0 then
      begin
        bundle:= TReplayBundle.Create(
          (FQueue[0] as TReplayBundle).Nick,
          (FQueue[0] as TReplayBundle).Lines
        );
        FQueue.Delete(0);
      end;
    finally
      FCriticalSection.Release;
    end;
    if Assigned(bundle) then
    begin
      FIRC.Say(bundle.Nick, '!! --> To avoid triggering flooding, for each 5 lines, I will pause for 5 seconds <-- !!');
      FIRC.Say(bundle.Nick, Format('*** Here are the last %d lines ***', [bundle.Lines.Count]));
      index:= 1;
      for line in bundle.Lines do
      begin
        debug('Sending #%d: "%s".', [index, line]);
        Inc(index);
        FIRC.Say(bundle.Nick, line);
        if (index mod 5) = 0 then
        begin
          debug('Pausing...');
          Sleep(5000);
        end;
      end;
      FIRC.Say(bundle.Nick, Format('*** End of the last %d lines ***', [bundle.Lines.Count]));
    end
    else
    begin
      //debug('Nothing to do, sleeping...');
      Sleep(500);
    end;
  end;
end;

procedure TReplayThread.Add(const ANick: String; const AList: TStringList);
var
  bundle: TReplayBundle;
begin
  FCriticalSection.Acquire;
  try
    bundle:= TReplayBundle.Create(ANick, AList);
    debug(Format('Adding %d lines for "%s".', [
      AList.Count,
      ANick
    ]));
    FQueue.Add(bundle);
  finally
    FCriticalSection.Release;
  end;
end;

constructor TReplayThread.Create(AIRC: TIdIRC);
begin
  inherited Create(True);
  FCriticalSection:= TCriticalSection.Create;
  FQueue:= TFPObjectList.Create(True);
  FIRC:= AIRC;
  FreeOnTerminate:= True;
  Start;
end;

destructor TReplayThread.Destroy;
begin
  FQueue.Free;
  FCriticalSection.Free;
  inherited Destroy;
end;

end.

