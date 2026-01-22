unit FastFFT;

{$mode objfpc}{$H+}

interface

// Это модуль-интерфейс для библиотеки fftw3f. Эта библиотека выполняет быстрое преобразование фурье (на массивом с одинарной точностью)  изпользуя аппаратные возможности процессора

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

  {$IFDEF Unix}
    {$linklib fftw3f}

    function fftwf_plan_dft_1d(n:cardinal;i,o:PValue2f; sign:fftw_sign; flags:fftw_flagset):Pointer;  cdecl; external;  // Перед тем как выполнять разсчёт БПФа необходимо создать "план"
    procedure fftwf_execute(plan:Pointer);                                                           cdecl; external;  // Если "план" создан, то можно выполнять операцию БПФ
    procedure fftwf_destroy_plan(plan:Pointer);                                                      cdecl; external;  // По окончании работы с "планом" его необходимо удалить

  {$ELSE}
    {$linklib libfftw3f}

    function fftwf_plan_dft_1d(n:cardinal;i,o:PValue2f; sign:fftw_sign;flags:fftw_flagset):Pointer;  stdcall; external;  // Перед тем как выполнять разсчёт БПФа необходимо создать "план"
    procedure fftwf_execute(plan:Pointer);                                                           stdcall; external;  // Если "план" создан, то можно выполнять операцию БПФ
    procedure fftwf_destroy_plan(plan:Pointer);                                                      stdcall; external;  // По окончании работы с "планом" его необходимо удалить
  {$ENDIF}

  function GetTwoPowerHigth(val: Integer): Integer; // Получение числа, являющего степенью 2-ки ближайшего с большей стороны
  function GetTwoPowerLow(val: Integer): Integer; // Получение числа, являющего степенью 2-ки ближайшего с меньшей стороны


implementation

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


end.

