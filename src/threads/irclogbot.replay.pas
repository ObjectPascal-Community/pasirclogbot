unit IRCLogBot.Replay;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, SyncObjs
, IdIRC

;

type
{ TReplayThread }
  TReplayThread = class(TThread)
  private
    FCriticalSection: TCriticalSection;
    FIRC: TIdIRC;
    FTarget: String;
    FLines: TStringList;
  protected
    procedure Execute; override;
  public
    constructor Create(AIRC: TIdIRC; const ATarget: String;
      const ALines: TStringList);
    destructor Destroy; override;
  published
  end;

implementation

uses
  IRCLogBot.Common
;

{ TReplayThread }

procedure TReplayThread.Execute;
var
  line: String;
  index: Integer = 0;
begin
  if not FIRC.Connected then
  begin
    debug('Exiting replay thread due not being connected.');
    exit;
  end;
  try
    FIRC.Say(FTarget, '!! --> To avoid triggering flooding, for each 5 lines, I will pause for 5 seconds <-- !!');
    FIRC.Say(FTarget, Format('*** Here are the last %d lines ***', [FLines.Count]));
    for line in FLines do
    begin
      if (Terminated) or (not FIRC.Connected) then
      begin
        debug('Exiting replay thread due to termination or not being connected.');
        exit;
      end;
      debug('Sending #%d: "%s"', [index, line]);
      Inc(index);
      FIRC.Say(FTarget, line);
      if (index mod 5) = 0 then
      begin
        debug('Pausing');
        Sleep(5000);
      end;
    end;
    FIRC.Say(FTarget, Format('*** End of the last %d lines ***', [FLines.Count]));
  finally
    FLines.Free;
  end;
end;

constructor TReplayThread.Create(AIRC: TIdIRC; const ATarget: String;
      const ALines: TStringList);
begin
  inherited Create(True);
  FCriticalSection:= TCriticalSection.Create;
  FIRC:= AIRC;
  FTarget:= ATarget;
  FLines:= TStringList.Create;
  FLines.Text := ALines.Text;
  FreeOnTerminate:= True;
  Start;
end;

destructor TReplayThread.Destroy;
begin
  FCriticalSection.Free;
  inherited Destroy;
end;

end.

