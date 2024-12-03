unit IRCLogBot.Database;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, SQLite3Conn
, SQLDB
, IRCLogBot.Common
;

type
{ TDatabase }
  TDatabase = class(TObject)
  private
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;

    procedure SetupTables;
  protected
  public
    constructor Create(ADatabaseFile: String);
    destructor Destroy; override;

    procedure Insert(ANickName, AChannel, AMessage: String);
    function Get(ACount: Integer): TStringList;
  published
  end;

implementation

{ TDatabase }

procedure TDatabase.SetupTables;
begin
  debug('Setting up tables...');
  FConnection.Open;
  FTransaction.StartTransaction;
  try
    FConnection.ExecuteDirect(
      'CREATE TABLE IF NOT EXISTS logs (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
      'timestamp DATETIME DEFAULT CURRENT_TIMESTAMP, ' +
      'channel TEXT, ' +
      'nick TEXT, ' +
      'message TEXT)');
    FTransaction.Commit;
  except
    on e:Exception do
    begin
      FTransaction.Rollback;
      debug('Error setting tables: %s', [e.Message]);
    end;
  end;
end;

procedure TDatabase.Insert(ANickName, AChannel, AMessage: String);
begin
  try
    debug('Starting Transaction...');
    FTransaction.StartTransaction;
    debug('Inserting: <%s> [%s] "%s"', [ANickName, AChannel, AMessage]);
    FConnection.ExecuteDirect(Format(
      'INSERT INTO logs (nick, channel, message) VALUES (%s, %s, %s)',
      [QuotedStr(ANickName), QuotedStr(AChannel), QuotedStr(AMessage)]));
    FTransaction.Commit;
    debug('Transaction committed.');
  except
    on e:Exception do
    begin
      FTransaction.Rollback;
      debug('Error inserting message: %s', [e.Message]);
    end;
  end;
end;

function TDatabase.Get(ACount: Integer): TStringList;
begin
  Result:= TStringList.Create;
  try
    debug('Starting transaction.');
    FTransaction.StartTransaction;
    FQuery.SQL.Text:= Format(
      'SELECT timestamp, nick, channel, message FROM logs ORDER BY id DESC LIMIT %d',
      [ACount]
    );
    FQuery.Open;
    if FQuery.RecordCount > 0 then
    begin
      FQuery.First;
      repeat
        debug('Retrieving: %s [%s] %s: %s', [
          FQuery.FieldByName('timestamp').AsString,
          FQuery.FieldByName('channel').AsString,
          FQuery.FieldByName('nick').AsString,
          FQuery.FieldByName('message').AsString
        ]);
        Result.Insert(0, Format('%s [%s] %s: %s',[
          FQuery.FieldByName('timestamp').AsString,
          FQuery.FieldByName('channel').AsString,
          FQuery.FieldByName('nick').AsString,
          FQuery.FieldByName('message').AsString
        ]));
        FQuery.Next;
      until FQuery.EOF;
      FQuery.Close;
      FTransaction.EndTransaction;
      debug('Transaction ended.');
    end;
  except
    on e:Exception do
    begin
      debug('Error retrieving lines: %s', [e.Message]);
    end;
  end;
end;

constructor TDatabase.Create(ADatabaseFile: String);
begin
  FConnection:= TSQLite3Connection.Create(nil);
  FTransaction:= TSQLTransaction.Create(FConnection);
  FConnection.Transaction:= FTransaction;
  FConnection.DatabaseName:= ADatabaseFile;
  FQuery:= TSQLQuery.Create(FConnection);
  FQuery.DataBase:= FConnection;

  SetupTables;
end;

destructor TDatabase.Destroy;
begin
  FQuery.Free;
  FTransaction.Free;
  FConnection.Free;
  inherited Destroy;
end;

end.

