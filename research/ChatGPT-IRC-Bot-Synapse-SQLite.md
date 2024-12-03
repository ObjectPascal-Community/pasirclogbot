# Prompt

Can you please give me an example of an `IRC` logger bot in Free Pascal that has the following specifications:
- Uses the `Synapse` library
- The bot is a self contained class
- The main body has hooks to system signals
- Logs to `SQLite`
- Uses `TLS 1.3` to connect
- Verifies the remote certificate
- When it detects that the server has disconnected it will retry to reconnect with an increasing amount of time on every failure to reconnect

# Answer

Here is an example implementation of an IRC logger bot in Free Pascal using the `Synapse` library. This example includes all your specifications:

- A self-contained bot class.
- Signal handling for graceful shutdown.
- SQLite logging.
- `TLS 1.3` connection with certificate verification.
- Automatic reconnect with exponential backoff.

### IRCLoggerBot Class Implementation

```pascal
program IRCLoggerBot;

{$mode objfpc}{$H+}

uses
  cthreads, sysutils, classes, blcksock, synsock, ssl_openssl, sqlite3conn, sqldb, db, signal;

type
  TIRCLoggerBot = class
  private
    FSocket: TTCPBlockSocket;
    FSQLiteConnection: TSQLite3Connection;
    FSQLTransaction: TSQLTransaction;
    FReconnectDelay: Integer;
    FRunning: Boolean;
    FHost: String;
    FPort: String;
    FChannel: String;
    FNick: String;
    FPassword: String;
    procedure SetupDatabase;
    procedure LogMessage(const Message: String);
    procedure HandleSignal(Signal: Integer);
  public
    constructor Create(const Host, Port, Channel, Nick, Password: String);
    destructor Destroy; override;
    procedure ConnectAndRun;
    procedure Stop;
  end;

{ TIRCLoggerBot }

constructor TIRCLoggerBot.Create(const Host, Port, Channel, Nick, Password: String);
begin
  FHost := Host;
  FPort := Port;
  FChannel := Channel;
  FNick := Nick;
  FPassword := Password;
  FReconnectDelay := 5; // Initial delay in seconds
  FRunning := True;

  // Initialize SQLite
  FSQLiteConnection := TSQLite3Connection.Create(nil);
  FSQLTransaction := TSQLTransaction.Create(FSQLiteConnection);
  FSQLiteConnection.Transaction := FSQLTransaction;
  FSQLiteConnection.DatabaseName := 'irc_logs.db';
  SetupDatabase;

  // Initialize Synapse Socket
  FSocket := TTCPBlockSocket.Create;
  FSocket.SSL.SSLType := LT_TLSv1_3;
end;

destructor TIRCLoggerBot.Destroy;
begin
  FSocket.Free;
  FSQLTransaction.Free;
  FSQLiteConnection.Free;
  inherited Destroy;
end;

procedure TIRCLoggerBot.SetupDatabase;
begin
  FSQLiteConnection.Open;
  FSQLTransaction.StartTransaction;
  FSQLiteConnection.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS logs (' +
    'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'timestamp DATETIME DEFAULT CURRENT_TIMESTAMP, ' +
    'message TEXT)');
  FSQLTransaction.Commit;
end;

procedure TIRCLoggerBot.LogMessage(const Message: String);
begin
  FSQLTransaction.StartTransaction;
  try
    FSQLiteConnection.ExecuteDirect(
      'INSERT INTO logs (message) VALUES (' + QuotedStr(Message) + ')');
    FSQLTransaction.Commit;
  except
    FSQLTransaction.Rollback;
    Writeln('Failed to log message: ', Message);
  end;
end;

procedure TIRCLoggerBot.HandleSignal(Signal: Integer);
begin
  Writeln('Signal received: ', Signal);
  Stop;
end;

procedure TIRCLoggerBot.ConnectAndRun;
var
  Line: String;
begin
  while FRunning do
  begin
    try
      Writeln('Connecting to ', FHost, ':', FPort);
      FSocket.Connect(FHost, FPort);
      FSocket.SSLDoConnect;

      if not FSocket.LastError = 0 then
        raise Exception.Create(FSocket.LastErrorDesc);

      // Verifying certificate
      if not FSocket.SSL.SSLVerifyPeer then
        raise Exception.Create('Certificate verification failed!');

      // Send IRC login details
      FSocket.SendString('PASS ' + FPassword + #13#10);
      FSocket.SendString('NICK ' + FNick + #13#10);
      FSocket.SendString('USER ' + FNick + ' 0 * :' + FNick + #13#10);
      FSocket.SendString('JOIN ' + FChannel + #13#10);

      Writeln('Connected to IRC server.');

      while FRunning and FSocket.CanRead(1000) do
      begin
        Line := FSocket.RecvString(1000);
        if Line <> '' then
        begin
          Writeln('Received: ', Line);
          LogMessage(Line);

          // Respond to PING messages
          if Pos('PING', Line) = 1 then
            FSocket.SendString('PONG ' + Copy(Line, 6, Length(Line)) + #13#10);
        end;
      end;

    except
      on E: Exception do
      begin
        Writeln('Error: ', E.Message);
        FSocket.CloseSocket;
        Writeln('Reconnecting in ', FReconnectDelay, ' seconds...');
        Sleep(FReconnectDelay * 1000);
        FReconnectDelay := Min(FReconnectDelay * 2, 60); // Max delay 60 seconds
      end;
    end;
  end;
end;

procedure TIRCLoggerBot.Stop;
begin
  FRunning := False;
  FSocket.CloseSocket;
end;

var
  Bot: TIRCLoggerBot;

procedure SignalHandler(Signal: cint); cdecl;
begin
  if Assigned(Bot) then
    Bot.HandleSignal(Signal);
end;

begin
  // Initialize bot
  Bot := TIRCLoggerBot.Create(
    'irc.example.com',  // Replace with IRC server
    '6697',             // TLS port
    '#example',         // Replace with channel
    'LoggerBot',        // Replace with bot nickname
    'password'          // Replace with bot password
  );

  // Hook signals
  Signal(SIGINT, @SignalHandler);
  Signal(SIGTERM, @SignalHandler);

  try
    Bot.ConnectAndRun;
  finally
    Bot.Free;
  end;
end.
```

### Key Features

1. **Self-Contained Class**: All logic is encapsulated in `TIRCLoggerBot`.
2. **Signal Handling**: Uses `Signal()` to handle SIGINT and SIGTERM for graceful shutdown.
3. **SQLite Logging**: Logs IRC messages to an SQLite database.
4. **TLS 1.3 with Certificate Verification**: Ensures secure communication.
5. **Reconnection Logic**: Implements exponential backoff for reconnecting after disconnections.

### Setup Instructions

1. Install the Synapse library and ensure OpenSSL is available.
2. Compile the program with `fpc`.
3. Run the bot and observe its behavior as it connects, logs messages, and handles disconnections.

Let me know if you need further refinements!