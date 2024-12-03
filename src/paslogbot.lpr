program paslogbot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  BaseUnix,
  {$ENDIF}
  Classes, SysUtils, CustApp, Bot
  { you can add units after this };

type

{ TPasLogBot }
  TPasLogBot = class(TCustomApplication)
  private
    FIRCLogBot: TIRCLogBot;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

var
  Application: TPasLogBot;

{ Signal Handling }
{$IFDEF UNIX}
procedure SignalHandler(signal: longint; info: psiginfo; context: psigcontext); cdecl;
begin
  case signal of
    SIGTERM, SIGINT:
      begin
        WriteLn;
        WriteLn('Received termination signal');
        if Assigned(Application) then
          Application.Terminate;
      end;
    SIGHUP:
      begin
        //WriteLn('Received SIGHUP - could implement config reload here');
        // Could implement configuration reload here
      end;
  end;
end;

procedure SetupSignalHandlers;
var
  act: SigActionRec;
begin
  FillChar(act, SizeOf(act), 0);
  act.sa_handler:= @SignalHandler;
  act.sa_flags:= 0;

  // Set up signal handlers
  fpSigAction(SIGTERM, @act, nil);
  fpSigAction(SIGINT, @act, nil);
  fpSigAction(SIGHUP, @act, nil);
end;
{$ENDIF}

{$IFDEF WINDOWS}
function ConsoleCtrlHandler(CtrlType: DWORD): BOOL; stdcall;
begin
  case CtrlType of
    CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT:
      begin
        WriteLn('Received termination signal');
        if Assigned(Application) then
          Application.Terminate;
        Result := True;
        Exit;
      end;
  end;
  Result := False;
end;

procedure SetupSignalHandlers;
begin
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, True);
end;
{$ENDIF}

{ TPasLogBot }

procedure TPasLogBot.DoRun;
var
  ErrorMsg: String;
begin
  // Signal Handling
  SetupSignalHandlers;
  // quick check parameters
  ErrorMsg:= CheckOptions('h', 'help');
  if ErrorMsg<>'' then
  begin
    //ShowException(Exception.Create(ErrorMsg));
    Terminate;
    exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    exit;
  end;

  WriteLn('Starting...');
  FIRCLogBot.Run;
  while not Terminated do
  begin
    Sleep(50);
  end;
  FIRCLogBot.Shutdown;
  WriteLn('Exiting.');
  // stop program loop
  //Terminate;
end;

constructor TPasLogBot.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:= True;
  FIRCLogBot:= TIRCLogBot.Create(
    '[PasLogBot]',
    'paslogbot',
    'Pascal channel IRC log bot',
    'localhost',
    6667,
    '#test'
  );
end;

destructor TPasLogBot.Destroy;
begin
  FIRCLogBot.Free;
  inherited Destroy;
end;

procedure TPasLogBot.WriteHelp;
begin
  { add your help code here }
  WriteLn('Usage: ', ExtractFileName(ExeName), ' -h');
end;

begin
  Application:= TPasLogBot.Create(nil);
  Application.Title:= 'Pascal Log Bot';
  Application.Run;
  Application.Free;
end.

