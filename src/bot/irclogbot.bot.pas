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

{ TIRCLogBot }

procedure TIRCLogBot.OnConnected(Sender: TObject);
begin
  WriteLn('Connected to server');
end;

procedure TIRCLogBot.OnDisconnected(Sender: TObject);
begin
  WriteLn('Disconnected from server');
end;

procedure TIRCLogBot.OnNotice(ASender: TIdContext; const ANickname, AHost,
  ATarget, ANotice: String);
begin
  WriteLn(Format('>> NOTICE: <%s> "%s"', [
    ANickname,
    ANotice
  ]));
end;

procedure TIRCLogBot.OnJoin(ASender: TIdContext; const ANickname, AHost,
  AChannel: String);
begin
  WriteLn(Format('>> JOIN: <%s@%s> %s', [
    ANickname,
    AHost,
    AChannel
  ]));
  if (ANickname = FNickName) and (AChannel = FChannel) then
  begin
    WriteLn('Successfully joined my channel');
    FJoinedChannel:= True;
  end;
end;

procedure TIRCLogBot.OnPrivateMessage(ASender: TIdContext; const ANickname,
  AHost, ATarget, AMessage: String);
begin
  WriteLn(Format('>> PRIVMSG: <%s@%s>(%s) "%s"', [
    ANickname,
    AHost,
    ATarget,
    AMessage
  ]));
  if ATarget = FNickName then
  begin
    if Pos('.', AMessage) = 1 then
    begin
      // Parse commands
      if Pos('.help', Trim(AMessage)) = 1 then
      begin
        WriteLn('Help command.');
        FIRC.Say(ANickname, 'Commands:');
        FIRC.Say(ANickname, '.help - This help information');
      end;
    end
    else
    begin
      WriteLn('No command.');
      FIRC.Say(ANickname, 'Not a command. Please use ".help" to see a list of commands.');
    end;
  end;
end;

procedure TIRCLogBot.Run;
begin
  WriteLn('Connecting...');
  FIRC.Connect;
  WriteLn('Joining channel: "', FChannel, '"...');
  FIRC.Join(FChannel);
end;

procedure TIRCLogBot.Shutdown;
begin
  if FIRC.Connected then
  begin
    WriteLn('Disconnecting...');
    FIRC.Disconnect('Need to go and have a wee nap.');
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

