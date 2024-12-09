unit IRCLogBot.Database;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, SyncObjs
, SQLite3Conn
, SQLDB
, IRCLogBot.Common
;

type
{ TDatabase }
  TDatabase = class(TObject)
  private
    FCriticalSection: TCriticalSection;
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;

    procedure SetupTables;
  protected
  public
    constructor Create(const ADatabaseFile: String);
    destructor Destroy; override;

    procedure Insert(const ANickName, AChannel, AMessage: String);
    function Get(const ACount: Integer): TStringList;
    function Search(const AQuery: String): TStringList;
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
      debug('Error setting tables: "%s".', [e.Message]);
    end;
  end;
end;

procedure TDatabase.Insert(const ANickName, AChannel, AMessage: String);
begin
  FCriticalSection.Acquire;
  try
    try
      debug('Starting Transaction...');
      FTransaction.StartTransaction;
      try
        debug('Inserting: <%s> [%s] "%s".', [ANickName, AChannel, AMessage]);
        FConnection.ExecuteDirect(Format(
          'INSERT INTO logs (nick, channel, message) VALUES (%s, %s, %s)',
          [QuotedStr(ANickName), QuotedStr(AChannel), QuotedStr(AMessage)]));
      finally
        FTransaction.Commit;
        debug('Transaction committed.');
      end;
    except
      on e:Exception do
      begin
        FTransaction.Rollback;
        debug('Error inserting message: "%s".', [e.Message]);
      end;
    end;
  finally
    FCriticalSection.Release;
  end;
end;

function TDatabase.Get(const ACount: Integer): TStringList;
var
  date, channel, nick, message: String;
begin
  Result:= TStringList.Create;
  FCriticalSection.Acquire;
  try
    debug('Starting GET transaction.');
    FTransaction.StartTransaction;
    try
      try
        FQuery.SQL.Text:= Format(
          'SELECT timestamp, nick, channel, message FROM logs ORDER BY id DESC LIMIT %d',
          [ACount]
        );
        FQuery.Open;
        if FQuery.RecordCount > 0 then
        begin
          FQuery.First;
          repeat
            date:= FQuery.FieldByName('timestamp').AsString;
            channel:= FQuery.FieldByName('channel').AsString;
            nick:= FQuery.FieldByName('nick').AsString;
            message:= FQuery.FieldByName('message').AsString;
            debug('Retrieving: %s [%s] %s: %s.', [
              date,
              channel,
              nick,
              message
            ]);
            Result.Insert(0, Format('%s [%s] %s: %s',[
              date,
              channel,
              nick,
              message
            ]));
            FQuery.Next;
          until FQuery.EOF;
          FQuery.Close;
        end;
      finally
        FTransaction.EndTransaction;
        debug('Transaction GET ended.');
      end;
    except
      on e:Exception do
      begin
        FTransaction.Rollback;
        debug('Error retrieving lines: "%s".', [e.Message]);
      end;
    end;
  finally
    FCriticalSection.Release;
  end;
end;

function TDatabase.Search(const AQuery: String): TStringList;
var
  date, channel, nick, message: String;
begin
  Result:= TStringList.Create;
  FCriticalSection.Acquire;
  try
    debug('Starting SEARCH transaction.');
    FTransaction.StartTransaction;
    try
      try
        FQuery.SQL.Text:= Format(
          'SELECT timestamp, nick, channel, message ' +
          'FROM logs ' +
          'WHERE message LIKE %s ' +
          'ORDER BY id DESC ' +
          'LIMIT %d',
          [QuotedStr('%' + AQuery + '%'), 10]
        );
        FQuery.Open;
        if FQuery.RecordCount > 0 then
        begin
          FQuery.First;
          repeat
            date:= FQuery.FieldByName('timestamp').AsString;
            channel:= FQuery.FieldByName('channel').AsString;
            nick:= FQuery.FieldByName('nick').AsString;
            message:= FQuery.FieldByName('message').AsString;
            debug('Retrieving: %s [%s] %s: %s.', [
              date,
              channel,
              nick,
              message
            ]);
            Result.Insert(0, Format('%s [%s] %s: %s',[
              date,
              channel,
              nick,
              message
            ]));
            FQuery.Next;
          until FQuery.EOF;
          FQuery.Close;
        end;
      finally
        FTransaction.EndTransaction;
        debug('Transaction SEARCH ended.');
      end;
    except
      on e:Exception do
      begin
        FTransaction.Rollback;
        debug('Error retrieving lines: "%s".', [e.Message]);
      end;
    end;
  finally
    FCriticalSection.Release;
  end;
end;

constructor TDatabase.Create(const ADatabaseFile: String);
begin
  FCriticalSection:= TCriticalSection.Create;
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
  FCriticalSection.Free;
  FQuery.Free;
  FTransaction.Free;
  FConnection.Free;
  inherited Destroy;
end;

end.

