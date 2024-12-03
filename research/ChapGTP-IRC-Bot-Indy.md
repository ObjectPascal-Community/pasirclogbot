# Prompt

Can you please give me a simple example of an `IRC` logging bot using Free Pascal with the `IRC` client from `Indy`?

# Answer

Here's an example of an IRC logging bot using Free Pascal with the Indy `IRC` client component (`TIdIRC`). This bot connects to an IRC server, joins a channel, and logs messages to a file.

### Example Code

```pas
program IRCLoggerBot;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, IdIRC, IdGlobal, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;

type
  TIRCLoggerBot = class
  private
    FIRC: TIdIRC;
    FLogFile: TextFile;
    procedure IRCConnect(AThread: TObject);
    procedure IRCDisconnect(AThread: TObject);
    procedure IRCMessage(ASender: TIdCommand; const AText: string);
    procedure OpenLogFile(const FileName: string);
    procedure CloseLogFile;
    procedure WriteLog(const Msg: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(const Server, Nickname, Channel: string; Port: Integer);
  end;

{ TIRCLoggerBot }

constructor TIRCLoggerBot.Create;
begin
  FIRC := TIdIRC.Create(nil);
  FIRC.OnConnect := @IRCConnect;
  FIRC.OnDisconnect := @IRCDisconnect;
  FIRC.OnPrivMessage := @IRCMessage;
end;

destructor TIRCLoggerBot.Destroy;
begin
  FIRC.Disconnect;
  CloseLogFile;
  FIRC.Free;
  inherited Destroy;
end;

procedure TIRCLoggerBot.IRCConnect(AThread: TObject);
begin
  WriteLog('Connected to IRC server.');
end;

procedure TIRCLoggerBot.IRCDisconnect(AThread: TObject);
begin
  WriteLog('Disconnected from IRC server.');
end;

procedure TIRCLoggerBot.IRCMessage(ASender: TIdCommand; const AText: string);
var
  LogMessage: string;
begin
  LogMessage := Format('[%s] <%s> %s', [FormatDateTime('hh:nn:ss', Now), ASender.Params[0], AText]);
  WriteLog(LogMessage);
end;

procedure TIRCLoggerBot.OpenLogFile(const FileName: string);
begin
  AssignFile(FLogFile, FileName);
  if FileExists(FileName) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
end;

procedure TIRCLoggerBot.CloseLogFile;
begin
  CloseFile(FLogFile);
end;

procedure TIRCLoggerBot.WriteLog(const Msg: string);
begin
  WriteLn(FLogFile, Msg);
  Flush(FLogFile);
end;

procedure TIRCLoggerBot.Start(const Server, Nickname, Channel: string; Port: Integer);
begin
  OpenLogFile('IRCLog.txt');
  WriteLog('Starting IRC bot.');

  FIRC.Host := Server;
  FIRC.Port := Port;
  FIRC.Nickname := Nickname;

  FIRC.Connect;
  if FIRC.Connected then
  begin
    WriteLog('Joining channel ' + Channel);
    FIRC.Join(Channel);
  end;
end;

{ Main program }
var
  Bot: TIRCLoggerBot;
begin
  Bot := TIRCLoggerBot.Create;
  try
    Bot.Start('irc.example.com', 'PascalBot', '#examplechannel', 6667);
    WriteLn('Press Enter to exit...');
    ReadLn;
  finally
    Bot.Free;
  end;
end.
```

### Explanation

1. **Setup**:
   - The bot uses `TIdIRC` from Indy components for IRC communication.
   - Logging is done via a text file (`IRCLog.txt`).

2. **Event Handlers**:
   - `IRCConnect`: Logs a message when connected to the server.
   - `IRCDisconnect`: Logs a message when disconnected.
   - `IRCMessage`: Logs messages received in the IRC channel.

3. **File Handling**:
   - `OpenLogFile` opens (or creates) the log file for writing.
   - `CloseLogFile` ensures the file is properly closed when the bot exits.
   - `WriteLog` writes log messages to the file with timestamps.

4. **Connection**:
   - `Start` sets the IRC server, nickname, and channel. It also opens the log file and connects to the IRC server.

### Running the Code
- Ensure Indy components are installed and added to your project.
- Replace `irc.example.com` and `#examplechannel` with the actual IRC server and channel you want to connect to.

### Notes
- This bot logs messages only. You can extend it to handle more IRC events (e.g., notices, kicks, etc.).
- Ensure you have permissions to write to the directory where the bot runs.