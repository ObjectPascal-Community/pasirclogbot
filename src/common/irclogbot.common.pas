unit IRCLogBot.Common;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils
;

const
  cVersion = 'v0.0.3';
  cRepoURL = 'https://github.com/ObjectPascal-Community/pasirclogbot';

var
  DebugOn: Boolean;

procedure info(const AMessage: String); overload;
procedure info(const AFormat: String; AValues: array of const);overload;
procedure error(const AMessage: String); overload;
procedure error(const AFormat: String; AValues: array of const);overload;
procedure debug(const AMessage: String); overload;
procedure debug(const AFormat: String; AValues: array of const);overload;

implementation

type
  TLogLevel = (llInfo, llError, llDebug);

var
  dateTimeStr: String;

procedure log(const ALevel: TLogLevel; const AMessage: String);
begin
  dateTimeStr:= FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz: ', Now);
  case ALevel of
    llInfo:begin
      WriteLn(dateTimeStr, '[INF]: ', AMessage);
    end;
    llError:begin
      WriteLn(dateTimeStr, '[ERR]: ', AMessage);
    end;
    llDebug:begin
      WriteLn(dateTimeStr, '[DBG]: ', AMessage);
    end;
  end;

end;

procedure info(const AMessage: String);
begin
  Log(llInfo, AMessage);
end;

procedure info(const AFormat: String; AValues: array of const);
begin
  Log(llInfo, Format(AFormat, AValues));
end;

procedure error(const AMessage: String);
begin
  Log(llError, AMessage);
end;

procedure error(const AFormat: String; AValues: array of const);
begin
  Log(llError, Format(AFormat, AValues));
end;

procedure debug(const AMessage: String);
begin
  if DebugOn then
  begin
    Log(llDebug, AMessage);
  end;
end;

procedure debug(const AFormat: String; AValues: array of const);
begin
  if DebugOn then
  begin
    Log(llDebug, Format(AFormat, AValues));
  end;
end;

end.

