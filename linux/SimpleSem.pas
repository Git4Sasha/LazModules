unit SimpleSem;

{$mode objfpc}{$H+}

interface

uses
  Classes, dynlibs;

type

  { TSimpleSem }

  TSimpleSem=class
  private
    FSem:Pointer;
    FSemName:string;
  public
    constructor Create(sname:string; inival:Integer);
    destructor Destroy; override;

    function Value:Integer;
    function Wait:Integer;
    function TryWait:Integer;
    function TimedWait(tlim:Double):Integer;
    procedure Post;
  end;

implementation

type
  TSimpleSemOpen = function(const name:PChar; initval:Cardinal):Pointer; cdecl;
  TSimpleSemClose = function(sem:Pointer):Integer; cdecl;
  TSimpleSemGetValue = function(sem:Pointer; val:Pointer):Integer; cdecl;
  TSimpleSemWait = function(sem:Pointer):Integer; cdecl;
  TSimpleSemTryWait = function(sem:Pointer):Integer; cdecl;
  TSimpleSemTimedWait = function(sem:Pointer; tlim:Double):Integer; cdecl;
  TSimpleSemPost = function(sem:Pointer):Integer; cdecl;
  TSimpleSemUnlink = function(const name:PChar):Integer; cdecl;

const
//libSimpleSem='./SimpleSem.so';
  libSimpleSem='/home/sasha/projects/c_proj/SemLib/SimpleSem.so';

var
  libHandle:TLibHandle;

  SimpleSemOpen: TSimpleSemOpen;
  SimpleSemClose: TSimpleSemClose;
  SimpleSemGetValue: TSimpleSemGetValue;
  SimpleSemWait: TSimpleSemWait;
  SimpleSemTryWait: TSimpleSemTryWait;
  SimpleSemTimedWait: TSimpleSemTimedWait;
  SimpleSemPost: TSimpleSemPost;
  SimpleSemUnlink: TSimpleSemUnlink;


procedure InitLib;
begin
  libHandle:=LoadLibrary(PChar(libSimpleSem));

  SimpleSemOpen:=TSimpleSemOpen(GetProcAddress(libHandle, 'SimpleSemOpen'));
  SimpleSemClose:=TSimpleSemClose(GetProcAddress(libHandle, 'SimpleSemClose'));
  SimpleSemGetValue:=TSimpleSemGetValue(GetProcAddress(libHandle, 'SimpleSemGetValue'));
  SimpleSemWait:=TSimpleSemWait(GetProcAddress(libHandle, 'SimpleSemWait'));
  SimpleSemTryWait:=TSimpleSemTryWait(GetProcAddress(libHandle, 'SimpleSemTryWait'));
  SimpleSemTimedWait:=TSimpleSemTimedWait(GetProcAddress(libHandle, 'SimpleSemTimedWait'));
  SimpleSemPost:=TSimpleSemPost(GetProcAddress(libHandle, 'SimpleSemPost'));
  SimpleSemUnlink:=TSimpleSemUnlink(GetProcAddress(libHandle, 'SimpleSemUnlink'));
end;

{ TSimpleSem }

constructor TSimpleSem.Create(sname: string; inival: Integer);
begin
  FSemName:=sname;
  FSem:=SimpleSemOpen(PChar(sname), inival);
end;

destructor TSimpleSem.Destroy;
begin
  if FSem<>nil then
    begin
      SimpleSemUnlink(PChar(FSemName)); // Отвязываем программу от семафора
      SimpleSemClose(FSem); // Закрываем семафор
    end;
  inherited Destroy;
end;

function TSimpleSem.Value: Integer;
begin
  SimpleSemGetValue(FSem, @Result);
end;

function TSimpleSem.Wait: Integer;
begin
  Result:=SimpleSemWait(FSem);
end;

function TSimpleSem.TryWait: Integer;
begin
  Result:=SimpleSemTryWait(FSem);
end;

function TSimpleSem.TimedWait(tlim: Double): Integer;
begin
  Result:=SimpleSemTimedWait(FSem, tlim);
end;

procedure TSimpleSem.Post;
begin
  SimpleSemPost(FSem);
end;

initialization
  InitLib;

finalization
  FreeLibrary(libHandle);

end.

