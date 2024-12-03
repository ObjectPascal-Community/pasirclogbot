unit IRCLogBot.Bot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, IdContext
, IdIRC
;

type
{ TIRCLogBot }
  TIRCLogBot = class(TObject)
  private
    FIRC: TIdIRC;
    FNickName: String;
    FUserName: String;
    FRealName: String;
    FHost: String;
    FPort: Word;
    FChannel: String;

    FJoinedChannel: Boolean;

    procedure OnConnected(Sender: TObject);
    procedure OnDisconnected(Sender: TObject);
    procedure OnNotice(ASender: TIdContext; const ANickname, AHost,
      ATarget, ANotice: String);
    procedure OnJoin(ASender: TIdContext; const ANickname, AHost,
      AChannel: String);
    procedure OnPrivateMessage(ASender: TIdContext; const ANickname, AHost,
      ATarget, AMessage: String);

    procedure Help(const ATarget: String);
    procedure Replay(const ATarget: String; Count: Integer);
  protected
  public
    constructor Create(AHost: String; APort: Word;
      ANickName, AUserName, ARealName, AChannel: String);
    destructor Destroy; override;

    procedure Run;
    procedure Shutdown;
  published
  end;

implementation

uses
  IRCLogBot.Common
;

{ TIRCLogBot }

procedure TIRCLogBot.OnConnected(Sender: TObject);
begin
  debug('Connected to server');
end;

procedure TIRCLogBot.OnDisconnected(Sender: TObject);
begin
  debug('Disconnected from server');
end;

procedure TIRCLogBot.OnNotice(ASender: TIdContext; const ANickname, AHost,
  ATarget, ANotice: String);
begin
  debug('>> NOTICE: <%s> "%s"', [
    ANickname,
    ANotice
  ]);
end;

procedure TIRCLogBot.OnJoin(ASender: TIdContext; const ANickname, AHost,
  AChannel: String);
begin
  debug('>> JOIN: <%s@%s> %s', [
    ANickname,
    AHost,
    AChannel
  ]);
  if (ANickname = FNickName) and (AChannel = FChannel) then
  begin
    debug('Successfully joined my channel');
    FJoinedChannel:= True;
    FIRC.Say(AChannel, 'I have arrived!!! TADAAAAA!!! Send me a private message with ".help" to see what I can do for you.');
  end;
end;

procedure TIRCLogBot.OnPrivateMessage(ASender: TIdContext; const ANickname,
  AHost, ATarget, AMessage: String);
var
  strings: TStringArray;
  count: Integer;
begin
  debug('>> PRIVMSG: <%s@%s>(%s) "%s"', [
    ANickname,
    AHost,
    ATarget,
    AMessage
  ]);
  if ATarget = FNickName then
  begin
    if Pos('.', AMessage) = 1 then
    begin
      // Parse commands
      if Pos('.help', Trim(AMessage)) = 1 then
      begin
        Help(ANickname);
        exit;
      end;
      if Pos('.replay', Trim(AMessage)) = 1 then
      begin
        strings:= AMessage.Split([' ']);
        try
          if Length(strings) > 1 then
          begin
            count:= StrToInt(strings[1]);
          end
          else
          begin
            count:= 0;
          end;
          Replay(ANickname, count);
        except
          FIRC.Say(ANickname, 'That <count> is not a number.');
        end;
        exit;
      end;
    end
    else
    begin
      debug('No command.');
      FIRC.Say(ANickname, 'Not a command. Please use ".help" to see a list of commands.');
    end;
  end;
end;

procedure TIRCLogBot.Help(const ATarget: String);
begin
  debug('Help command.');
  FIRC.Say(ATarget, 'Commands:');
  FIRC.Say(ATarget, '.help - This help information.');
  FIRC.Say(ATarget, '.replay [count] - Raplays last <count> lines. Default is last 10 lines.');
end;

procedure TIRCLogBot.Replay(const ATarget: String; Count: Integer);
begin
  debug('Replay command.');
  FIRC.Say(ATarget, Format('Not fully implemented yet: %d',[Count]));
end;

procedure TIRCLogBot.Run;
begin
  debug('Connecting...');
  try
    FIRC.Connect;
  except
    on e:Exception do
    begin
      debug('Error connecting: %s', [e.Message]);
    end;
  end;
  debug('Joining channel: "%s"...', [FChannel]);
  try
    FIRC.Join(FChannel);
  except
    on e:Exception do
    begin
      debug('Error joining: %s', [e.Message]);
    end;
  end;
end;

procedure TIRCLogBot.Shutdown;
begin
  if FIRC.Connected then
  begin
    debug('Disconnecting...');
    try
      FIRC.Disconnect('Need to go and have a wee nap.');
    except
      on e:Exception do
      begin
        debug('Error: %s', [e.Message]);
      end;
    end;
  end;
end;

constructor TIRCLogBot.Create(AHost: String; APort: Word; ANickName, AUserName,
  ARealName, AChannel: String);
begin
  FNickname:= ANickname;
  FUsername:= AUserName;
  FRealName:= ARealName;
  FHost:= AHost;
  FPort:= APort;
  FChannel:= AChannel;

  FIRC:= TIdIRC.Create;
  FIRC.Nickname:= FNickName;
  FIRC.Username:= FUserName;
  FIRC.RealName:= FRealName;
  FIRC.Host:= FHost;
  FIRC.Port:= FPort;
  FIRC.OnConnected:= @OnConnected;
  FIRC.OnDisconnected:= @OnDisconnected;
  FIRC.OnJoin:= @OnJoin;
  FIRC.OnNotice:= @OnNotice;
  FIRC.OnPrivateMessage:= @OnPrivateMessage;

  FJoinedChannel:= False;
end;

destructor TIRCLogBot.Destroy;
begin
  FIRC.Free;
  inherited Destroy;
end;

end.

