unit IRCLogBot.Config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, IniFiles
;

type
{ TBotConfig }
  TBotConfig = class(TObject)
  private
    FINIFile: String;
    FINI: TIniFile;
    FHost: String;
    FPort: Word;
    FNickName: String;
    FUserName: String;
    FRealName: String;
    FChannel: String;
  protected
  public
    constructor Create(AConfigFile: String);
    destructor Destroy; override;

    procedure LoadValues;

    property Host: String
      read FHost
      write FHost;
    property Port: Word
      read FPort
      write FPort;
    property NickName: String
      read FNickName
      write FNickName;
    property UserName: String
      read FUserName
      write FUserName;
    property RealName: String
      read FRealName
      write FRealName;
    property Channel: String
      read FChannel
      write FChannel;
  published
  end;

implementation

{ TBotConfig }

constructor TBotConfig.Create(AConfigFile: String);
begin
  FINIFile:= AConfigFile;
  FHost:= 'localhost';
  FPort:= 6667;
  FNickName:= '';
  FUserName:= '';
  FRealName:= '';
  FChannel:= '';
end;

destructor TBotConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TBotConfig.LoadValues;
begin
  if FileExists(FINIFile) then
  begin
    FINI:= TIniFile.Create(FINIFile);
    FHost:= FINI.ReadString('Main', 'Host', 'localhost');
    FPort:= FINI.ReadInteger('Main', 'Port', 6667);
    FNickName:= FINI.ReadString('Main', 'NickName', '');
    FUserName:= FINI.ReadString('Main', 'UserName', '');
    FRealName:= FINI.ReadString('Main', 'RealName', '');
    FChannel:= FINI.ReadString('Main', 'Channel', '');
  end
  else
  begin
    raise EFileNotFoundException.Create(Format('Cannot find file "%s".', [FINIFile]));
  end;
end;

end.

