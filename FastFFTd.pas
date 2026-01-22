unit FastFFTd;

{$mode objfpc}{$H+}

interface

// Это модуль-интерфейс для библиотеки fftw3. Эта библиотека выполняет быстрое преобразование фурье изпользуя аппаратные возможности процессора
//

uses
  dynlibs, sysutils, ArrayOfTypes;

type
  fftw_sign=(fftw_forward=-1,fftw_backward=1);
  fftw_flag=(fftw_measure,            {generated optimized algorithm}
             fftw_destroy_input,      {default}
             fftw_unaligned,          {data is unaligned}
             fftw_conserve_memory,    {needs no explanation}
             fftw_exhaustive,         {search optimal algorithm}
             fftw_preserve_input,     {don't overwrite input}
             fftw_patient,            {generate highly optimized alg.}
             fftw_estimate);          {don't optimize, just use an alg.}
  fftw_flagset=set of fftw_flag;

  Tfftwd_plan_dft_1d    =function(n:cardinal;i,o:PValue2d; sign:fftw_sign; flags:fftw_flagset):Pointer;  cdecl;  // Перед тем как выполнять разсчёт БПФа необходимо создать "план"
  Tfftwd_execute        =procedure(plan:Pointer);       cdecl;   // Если "план" создан, то можно выполнять операцию БПФ
  Tfftwd_destroy_plan   =procedure(plan:Pointer);       cdecl;   // По окончании работы с "планом" его необходимо удалить


var
  fftwd_plan_dft_1d           :Tfftwd_plan_dft_1d;
  fftwd_execute               :Tfftwd_execute;
  fftwd_destroy_plan          :Tfftwd_destroy_plan;



function InitFFTW3d:Integer;
procedure DeinitFFTW3d;
function GetTwoPowerHigth(val:Integer):Integer; // Получение числа, являющего степенью 2-ки ближайшего с большей стороны
function GetTwoPowerLow(val:Integer):Integer;   // Получение числа, являющего степенью 2-ки ближайшего с меньшей стороны

implementation

{$IFDEF Unix}
  const
      fftwlib = 'libfftw3.so.3';
{$ELSE}
  const
      fftwlib = 'libfftw3.dll';
{$ENDIF}


var
  libhandlefftwd: TLibHandle=NilHandle;


function InitFFTW3d:Integer; // Инициализация библиотеки быстрого преобразования фурье
begin
  if libhandlefftwd<>NilHandle then Exit(0); // Если инициализация уже выполнялась, то выходим
  libhandlefftwd:=LoadLibrary(PChar(fftwlib));

  if libhandlefftwd=NilHandle then begin
    WriteLn(Format('Библиотека fftw3 не загружается',[]));
    WriteLn(Format('Ошибка:  %s', [dynlibs.GetLoadErrorStr]));
    Exit(-1);
  end;

  fftwd_plan_dft_1d  :=Tfftwd_plan_dft_1d ( GetProcAddress(libhandlefftwd, 'fftw_plan_dft_1d'    ));
  fftwd_execute      :=Tfftwd_execute     ( GetProcAddress(libhandlefftwd, 'fftw_execute'        ));
  fftwd_destroy_plan :=Tfftwd_destroy_plan( GetProcAddress(libhandlefftwd, 'fftw_destroy_plan'   ));

  Result:=0;
end;


procedure DeinitFFTW3d;
begin
  if libhandlefftwd=NilHandle then Exit;
  FreeLibrary(libhandlefftwd);
  libhandlefftwd:=NilHandle;
end;

function GetTwoPowerHigth(val: Integer): Integer; // Получение числа, являющего степенью 2-ки ближайшего с большей стороны
begin
  Result:=1;
  while (Result<val) do Result:=Result shl 1;
end;

function GetTwoPowerLow(val: Integer): Integer; // Получение числа, являющего степенью 2-ки ближайшего с меньшей стороны
begin
  Result:=1;
  while (Result<val) do Result:=Result shl 1;
  if Result<>val then Result:=Result shr 1;
end;

finalization
  DeinitFFTW3d;

end.

