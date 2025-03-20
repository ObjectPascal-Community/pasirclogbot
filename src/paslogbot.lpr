program paslogbot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cThreads,
  BaseUnix,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, CustApp, IRCLogBot.Common, IRCLogBot.Bot, IRCLogBot.Config
  { you can add units after this };

type

{ TPasLogBot }
  TPasLogBot = class(TCustomApplication)
  private
    FIRCLogBot: TIRCLogBot;
    FConfigFile: String;
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
        if signal = SIGINT then WriteLn;
        debug('Received termination signal.');
        if Assigned(Application) then
          Application.Terminate;
      end;
    SIGHUP:
      begin
        //debug('Received SIGHUP - could implement config reload here');
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
  //fpSignal(SIGTERM, @SignalHandler); // Need to investigate the params for this
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
        debug('Received termination signal.');
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
  config: TBotConfig;
  ErrorMsg: String;
begin
  // Signal Handling
  SetupSignalHandlers;
  // quick check parameters
  ErrorMsg:= CheckOptions('hc:d', ['help', 'config:', 'debug']);
  if ErrorMsg<>'' then
  begin
    error('Error: "%s"', [ErrorMsg]);
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

  // Config
  if HasOption('c', 'config') then
  begin
    FConfigFile:= GetOptionValue('c', 'config');
  end
  else
  begin
    FConfigFile:= ConcatPaths([GetUserDir, '.pasirclogbot']);
  end;

  // Debug
  DebugOn:= HasOption('d', 'debug');

  //debug('Long Date: ' + DefaultFormatSettings.LongDateFormat);
  //debug('Long Time: ' + DefaultFormatSettings.LongTimeFormat);
  //debug('Short Date: ' + DefaultFormatSettings.ShortDateFormat);
  //debug('Short Time: ' + DefaultFormatSettings.ShortTimeFormat);
  //debug('Date Sep: ' + DefaultFormatSettings.DateSeparator);
  debug('Setting Date and Time formats.');
  DefaultFormatSettings.ShortDateFormat:= 'yyyy/mm/dd';
  DefaultFormatSettings.DateSeparator:= '/';
  debug(Format('Attempting to read config from: "%s"...', [FConfigFile]));

  config:= TBotConfig.Create(FConfigFile);
  try
    config.LoadValues;
  except
    on e:Exception do
    begin
      error('Error: %s', [e.Message]);
      Terminate;
      exit;
    end;
  end;

  debug('Creating IRC client...');
  try
    FIRCLogBot:= TIRCLogBot.Create(config);
  except
    on e:Exception do
    begin
      error('Error: "%s".', [e.Message]);
      Terminate;
      exit;
    end;
  end;
  debug('Successfully created IRC client.');
  info('Starting...');
  FIRCLogBot.Run;
  while not Terminated do
  begin
    Sleep(1);
  end;
  FIRCLogBot.Shutdown;
  FIRCLogBot.Free;
  config.Free;
  info('Exiting.');
  // stop program loop
  //Terminate;
end;

constructor TPasLogBot.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:= True;
end;

destructor TPasLogBot.Destroy;
begin
  inherited Destroy;
end;

procedure TPasLogBot.WriteHelp;
begin
  { add your help code here }
  WriteLn('Usage:');
  WriteLn('  ', ExtractFileName(ExeName), ' [PARAMS]');
  WriteLn;
  WriteLn('PARAMS:');
  WriteLn('    -h/--help         This help message.');
  {$IFDEF UNIX}
  WriteLn('    -c/--config=FILE  Use provided FILE as config. ( default: ~/.config/pasirclogbot.conf )');
  {$ENDIF}
  {$IFDEF WINDOWS}
  WriteLn('    -c/--config=FILE  Use provided FILE as config. ( default: %APPDATA%/pasirclogbot )');
  {$ENDIF}
  WriteLn('    -d/--debug        Turn debug On. (default: Off)');
end;

begin
  Application:= TPasLogBot.Create(nil);
  Application.Title:= 'Pascal IRC Log Bot';
  Application.Run;
  Application.Free;
end.

