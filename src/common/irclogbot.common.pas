unit IRCLogBot.Common;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils
;

var
  DebugOn: Boolean;

procedure debug(const AMessage: String); overload;
procedure debug(const AFormat: String; AValues: array of const);overload;

implementation

var
  dateTimeStr: String;

procedure debug(const AMessage: String);
begin
  if DebugOn then
  begin
    dateTimeStr:= FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz:', Now);
    WriteLn(Format('%s %s', [dateTimeStr, AMessage]));
  end;
end;

procedure debug(const AFormat: String; AValues: array of const);
begin
  if DebugOn then
  begin
    dateTimeStr:= FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz: ', Now);
    WriteLn(Format(dateTimeStr+AFormat, AValues));
  end;
end;

end.

