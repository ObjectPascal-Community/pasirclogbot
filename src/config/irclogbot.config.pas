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

    FIRCHost: String;
    FIRCPort: Word;
    FIRCNickName: String;
    FIRCUserName: String;
    FIRCRealName: String;
    FIRCChannel: String;

    FDBFile: String;
  protected
  public
    constructor Create(AConfigFile: String);
    destructor Destroy; override;

    procedure LoadValues;

    property Host: String
      read FIRCHost
      write FIRCHost;
    property Port: Word
      read FIRCPort
      write FIRCPort;
    property NickName: String
      read FIRCNickName
      write FIRCNickName;
    property UserName: String
      read FIRCUserName
      write FIRCUserName;
    property RealName: String
      read FIRCRealName
      write FIRCRealName;
    property Channel: String
      read FIRCChannel
      write FIRCChannel;

    property Database: String
      read FDBFile
      write FDBFile;
  published
  end;

implementation

{ TBotConfig }

constructor TBotConfig.Create(AConfigFile: String);
begin
  FINIFile:= AConfigFile;
  FIRCHost:= 'localhost';
  FIRCPort:= 6667;
  FIRCNickName:= '';
  FIRCUserName:= '';
  FIRCRealName:= '';
  FIRCChannel:= '';
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
    try
      FIRCHost:= FINI.ReadString('IRC', 'Host', 'localhost');
      FIRCPort:= FINI.ReadInteger('IRC', 'Port', 6667);
      FIRCNickName:= FINI.ReadString('IRC', 'NickName', '');
      FIRCUserName:= FINI.ReadString('IRC', 'UserName', '');
      FIRCRealName:= FINI.ReadString('IRC', 'RealName', '');
      FIRCChannel:= FINI.ReadString('IRC', 'Channel', '');

      FDBFile:= FINI.ReadString('DB', 'File', 'paslogbot.db');

    finally
      FINI.Free;
    end;
  end
  else
  begin
    raise EFileNotFoundException.Create(Format('Cannot find file "%s".', [FINIFile]));
  end;
end;

end.

