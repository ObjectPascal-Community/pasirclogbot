# Prompt

Can you please give me an example of an `IRC` logger bot in Free Pascal that has the following specifications:
- Uses the `Indy` library
- The bot is a self contained class
- The main body has hooks to system signals
- Logs to `SQLite`
- Uses `TLS 1.3` to connect
- Verifies the remote certificate

# Answer

Here's an example of an IRC logger bot in Free Pascal using the `Indy` library. It includes the requested specifications:

1. **Self-contained class**: The bot is encapsulated in a class.
2. **Signal handling**: The main program handles system signals to allow graceful shutdown.
3. **SQLite logging**: Logs IRC messages to an SQLite database.
4. **TLS 1.3**: Uses TLS 1.3 to connect securely.
5. **Certificate verification**: Verifies the remote server's certificate.

### Code Implementation

#### Prerequisites
- Install `Indy` and `SQLite` units for Free Pascal.
- Configure OpenSSL libraries compatible with Indy for TLS.

```pascal
program IRCLoggerBot;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, IdIRC, IdSSL, IdSSLOpenSSL, IdTCPClient, SQLite3Conn, SQLDB, BaseUnix;

type
  TIRCLoggerBot = class
  private
    FIRC: TIdIRC;
    FSSL: TIdSSLIOHandlerSocketOpenSSL;
    FDBConn: TSQLite3Connection;
    FSQLTransaction: TSQLTransaction;
    FStop: Boolean;
    procedure OnConnect(Sender: TObject);
    procedure OnDisconnect(Sender: TObject);
    procedure OnMessage(Sender: TIdCommand; const AChannel, ANick, AMessage: string);
    procedure SetupDatabase;
  public
    constructor Create(const AHost, APort, AChannel, ANick: string);
    destructor Destroy; override;
    procedure Run;
    procedure Stop;
  end;

{ TIRCLoggerBot }

constructor TIRCLoggerBot.Create(const AHost, APort, AChannel, ANick: string);
begin
  FStop := False;

  // Setup SQLite Database
  FDBConn := TSQLite3Connection.Create(nil);
  FSQLTransaction := TSQLTransaction.Create(FDBConn);
  FDBConn.Transaction := FSQLTransaction;
  FDBConn.DatabaseName := 'irc_log.db';
  SetupDatabase;

  // Setup Indy IRC client
  FIRC := TIdIRC.Create(nil);
  FIRC.Host := AHost;
  FIRC.Port := StrToInt(APort);
  FIRC.Nick := ANick;
  FIRC.Username := ANick;
  FIRC.RealName := ANick;
  FIRC.OnConnect := @OnConnect;
  FIRC.OnDisconnect := @OnDisconnect;
  FIRC.OnPrivMessage := @OnMessage;

  // Setup SSL/TLS handler
  FSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FSSL.SSLOptions.Method := sslvTLSv1_3;
  FSSL.SSLOptions.Mode := sslmClient;
  FSSL.SSLOptions.VerifyMode := [sslvrfPeer];
  FSSL.SSLOptions.VerifyDepth := 5;
  FSSL.OnVerifyPeer := function(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean
  begin
    Result := AOk; // Ensure peer certificate is valid
  end;
  FIRC.IOHandler := FSSL;

  // Join channel after connecting
  FIRC.AutoJoinChannels.Add(AChannel);
end;

destructor TIRCLoggerBot.Destroy;
begin
  FIRC.Disconnect;
  FIRC.Free;
  FSSL.Free;
  FSQLTransaction.Free;
  FDBConn.Free;
  inherited Destroy;
end;

procedure TIRCLoggerBot.SetupDatabase;
begin
  FDBConn.Open;
  FSQLTransaction.StartTransaction;
  try
    FDBConn.ExecuteDirect(
      'CREATE TABLE IF NOT EXISTS logs (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
      'timestamp DATETIME DEFAULT CURRENT_TIMESTAMP, ' +
      'channel TEXT, ' +
      'nick TEXT, ' +
      'message TEXT)');
    FSQLTransaction.Commit;
  except
    FSQLTransaction.Rollback;
    raise;
  end;
end;

procedure TIRCLoggerBot.OnConnect(Sender: TObject);
begin
  WriteLn('Connected to IRC server.');
end;

procedure TIRCLoggerBot.OnDisconnect(Sender: TObject);
begin
  WriteLn('Disconnected from IRC server.');
end;

procedure TIRCLoggerBot.OnMessage(Sender: TIdCommand; const AChannel, ANick, AMessage: string);
begin
  WriteLn(Format('[%s] <%s>: %s', [AChannel, ANick, AMessage]));

  // Log message to SQLite
  FSQLTransaction.StartTransaction;
  try
    with FDBConn do
      ExecuteDirect(Format(
        'INSERT INTO logs (channel, nick, message) VALUES (%s, %s, %s)',
        [QuotedStr(AChannel), QuotedStr(ANick), QuotedStr(AMessage)]));
    FSQLTransaction.Commit;
  except
    FSQLTransaction.Rollback;
  end;
end;

procedure TIRCLoggerBot.Run;
begin
  while not FStop do
  begin
    try
      FIRC.Connect;
      FIRC.Socket.InputBuffer.Clear;
      FIRC.Socket.CheckForDataOnSource(10); // Run a simple event loop
    except
      on E: Exception do
        WriteLn('Error: ', E.Message);
    end;
  end;
end;

procedure TIRCLoggerBot.Stop;
begin
  FStop := True;
  FIRC.Disconnect;
end;

{ Signal Handling }
var
  LoggerBot: TIRCLoggerBot;

procedure SignalHandler(Signal: LongInt); cdecl;
begin
  WriteLn('Received signal: ', Signal);
  if Assigned(LoggerBot) then
    LoggerBot.Stop;
end;

begin
  Signal(SIGINT, @SignalHandler);
  Signal(SIGTERM, @SignalHandler);

  LoggerBot := TIRCLoggerBot.Create('irc.example.com', '6697', '#channel', 'LoggerBot');
  try
    LoggerBot.Run;
  finally
    LoggerBot.Free;
  end;
end.
```

### Key Features
1. **Graceful shutdown**: Handles system signals (e.g., `SIGINT` and `SIGTERM`) to cleanly disconnect and close the database.
2. **Certificate validation**: Verifies the server certificate to ensure secure communication.
3. **Logging**: Logs messages in the SQLite database with fields for channel, nickname, and message.
4. **Encapsulation**: The bot's functionality is encapsulated in the `TIRCLoggerBot` class for modularity and reusability.

### Dependencies
Ensure you have:
- OpenSSL libraries (`libssl.so` and `libcrypto.so`).
- SQLite library for database interaction.

This bot demonstrates secure and reliable IRC logging while adhering to modern best practices.

# Prompt

Can you please modify the code so that when it detects that the server has disconnected it will retry to reconnect with an increasing amount of time on every failure to reconnect?

# Answer

Here’s the modified version of the IRC logger bot. It implements a retry mechanism with an increasing delay when the server disconnects or fails to reconnect.

### Updated Code

```pascal
program IRCLoggerBot;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, IdIRC, IdSSL, IdSSLOpenSSL, IdTCPClient, SQLite3Conn, SQLDB, BaseUnix, DateUtils;

type
  TIRCLoggerBot = class
  private
    FIRC: TIdIRC;
    FSSL: TIdSSLIOHandlerSocketOpenSSL;
    FDBConn: TSQLite3Connection;
    FSQLTransaction: TSQLTransaction;
    FStop: Boolean;
    FReconnectDelay: Integer;
    procedure OnConnect(Sender: TObject);
    procedure OnDisconnect(Sender: TObject);
    procedure OnMessage(Sender: TIdCommand; const AChannel, ANick, AMessage: string);
    procedure SetupDatabase;
    procedure HandleReconnect;
  public
    constructor Create(const AHost, APort, AChannel, ANick: string);
    destructor Destroy; override;
    procedure Run;
    procedure Stop;
  end;

{ TIRCLoggerBot }

constructor TIRCLoggerBot.Create(const AHost, APort, AChannel, ANick: string);
begin
  FStop := False;
  FReconnectDelay := 1; // Initial reconnect delay in seconds

  // Setup SQLite Database
  FDBConn := TSQLite3Connection.Create(nil);
  FSQLTransaction := TSQLTransaction.Create(FDBConn);
  FDBConn.Transaction := FSQLTransaction;
  FDBConn.DatabaseName := 'irc_log.db';
  SetupDatabase;

  // Setup Indy IRC client
  FIRC := TIdIRC.Create(nil);
  FIRC.Host := AHost;
  FIRC.Port := StrToInt(APort);
  FIRC.Nick := ANick;
  FIRC.Username := ANick;
  FIRC.RealName := ANick;
  FIRC.OnConnect := @OnConnect;
  FIRC.OnDisconnect := @OnDisconnect;
  FIRC.OnPrivMessage := @OnMessage;

  // Setup SSL/TLS handler
  FSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FSSL.SSLOptions.Method := sslvTLSv1_3;
  FSSL.SSLOptions.Mode := sslmClient;
  FSSL.SSLOptions.VerifyMode := [sslvrfPeer];
  FSSL.SSLOptions.VerifyDepth := 5;
  FSSL.OnVerifyPeer := function(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean
  begin
    Result := AOk; // Ensure peer certificate is valid
  end;
  FIRC.IOHandler := FSSL;

  // Join channel after connecting
  FIRC.AutoJoinChannels.Add(AChannel);
end;

destructor TIRCLoggerBot.Destroy;
begin
  FIRC.Disconnect;
  FIRC.Free;
  FSSL.Free;
  FSQLTransaction.Free;
  FDBConn.Free;
  inherited Destroy;
end;

procedure TIRCLoggerBot.SetupDatabase;
begin
  FDBConn.Open;
  FSQLTransaction.StartTransaction;
  try
    FDBConn.ExecuteDirect(
      'CREATE TABLE IF NOT EXISTS logs (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
      'timestamp DATETIME DEFAULT CURRENT_TIMESTAMP, ' +
      'channel TEXT, ' +
      'nick TEXT, ' +
      'message TEXT)');
    FSQLTransaction.Commit;
  except
    FSQLTransaction.Rollback;
    raise;
  end;
end;

procedure TIRCLoggerBot.OnConnect(Sender: TObject);
begin
  WriteLn('Connected to IRC server.');
  FReconnectDelay := 1; // Reset reconnect delay on successful connection
end;

procedure TIRCLoggerBot.OnDisconnect(Sender: TObject);
begin
  WriteLn('Disconnected from IRC server.');
  if not FStop then
    HandleReconnect;
end;

procedure TIRCLoggerBot.OnMessage(Sender: TIdCommand; const AChannel, ANick, AMessage: string);
begin
  WriteLn(Format('[%s] <%s>: %s', [AChannel, ANick, AMessage]));

  // Log message to SQLite
  FSQLTransaction.StartTransaction;
  try
    with FDBConn do
      ExecuteDirect(Format(
        'INSERT INTO logs (channel, nick, message) VALUES (%s, %s, %s)',
        [QuotedStr(AChannel), QuotedStr(ANick), QuotedStr(AMessage)]));
    FSQLTransaction.Commit;
  except
    FSQLTransaction.Rollback;
  end;
end;

procedure TIRCLoggerBot.HandleReconnect;
begin
  while not FStop do
  begin
    try
      WriteLn(Format('Attempting to reconnect in %d seconds...', [FReconnectDelay]));
      Sleep(FReconnectDelay * 1000); // Wait before retrying
      FIRC.Connect;
      Exit; // Exit the reconnect loop on successful connection
    except
      on E: Exception do
      begin
        WriteLn('Reconnection failed: ', E.Message);
        FReconnectDelay := Min(FReconnectDelay * 2, 600); // Exponential backoff, max 10 minutes
      end;
    end;
  end;
end;

procedure TIRCLoggerBot.Run;
begin
  while not FStop do
  begin
    try
      WriteLn('Connecting to IRC server...');
      FIRC.Connect;
      FIRC.Socket.InputBuffer.Clear;
      FIRC.Socket.CheckForDataOnSource(10); // Run a simple event loop
    except
      on E: Exception do
      begin
        WriteLn('Connection error: ', E.Message);
        HandleReconnect;
      end;
    end;
  end;
end;

procedure TIRCLoggerBot.Stop;
begin
  FStop := True;
  FIRC.Disconnect;
end;

{ Signal Handling }
var
  LoggerBot: TIRCLoggerBot;

procedure SignalHandler(Signal: LongInt); cdecl;
begin
  WriteLn('Received signal: ', Signal);
  if Assigned(LoggerBot) then
    LoggerBot.Stop;
end;

begin
  Signal(SIGINT, @SignalHandler);
  Signal(SIGTERM, @SignalHandler);

  LoggerBot := TIRCLoggerBot.Create('irc.example.com', '6697', '#channel', 'LoggerBot');
  try
    LoggerBot.Run;
  finally
    LoggerBot.Free;
  end;
end.
```

### Key Modifications
1. **Reconnect Logic**: Added a `HandleReconnect` procedure that:
   - Waits with a delay (`Sleep`).
   - Retries connecting to the IRC server.
   - Uses exponential backoff for the delay (`FReconnectDelay` doubles after each failure, capped at 10 minutes).
   - Exits on successful connection.
   
2. **Reset Delay**: `FReconnectDelay` is reset to `1` second upon successful connection.

3. **Error Handling**: Ensures that the bot retries indefinitely unless stopped (`FStop` set to `True`).

This modification ensures the bot gracefully handles disconnections and intelligently retries with increasing delays, avoiding rapid reconnection attempts.