unit GaussKrugCalc;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , ArrayOfTypes
  , Math
  ;


type

  { TGausKrugCalc }

  TGausKrugCalc=class
  private
    ro:Double; // Число угловых секунд в радиане

    // Эллипсоид Красовского
    aP  :Double;
    alP :Double;
    e2P :Double;

    // Эллипсоид WGS84 (GRS80, эти два эллипсоида сходны по большинству параметров)
    aW  :Double;
    alW :Double;
    e2W :Double;

    // Вспомогательные значения для преобразования эллипсоидов
    a   :Double;
    e2  :Double;
    da  :Double;
    de2 :Double;

    // Линейные элементы трансформирования, в метрах
    dx :Double;
    dy :Double;
    dz :Double;

    // Угловые элементы трансформирования, в секундах
    wx :Double;
    wy :Double;
    wz :Double;

    // Дифференциальное различие масштабов
    ms :Double;

  public
    constructor Create;

    function ZoneNumber(lym:Double):Integer;              // Разсчёт номера зоны для заданной долготы (долгота передаётся в РАДИАНАХ)

    function WGS84ToSK42(flhwgs84:TValue3d):TValue3d;     // Перезсчёт географических координат wgs84 (гугл карты) в географические координаты СК42 (яндекс карты)
    function SK42ToWGS84(flhSK42:TValue3d):TValue3d;      // Перезсчёт географических координат СК42 (яндекс карты) в географические координаты wgs84 (гугл карты)

    function SK42ToGsKr(CK42:TValue3d):TValue3d;          // Перезсчёт географических координат СК42 (яндекс карты) в прямоугольные координаты в проекции Гаусса-Крюгера
    function GsKrToCK42(xyz:TValue3d):TValue3d;           // Перезсчёт прямоугольных координат в проекции Гаусса-Крюгера в географические координаты СК42 (яндекс карты)

    function WGS84ToGsKr(wgs84:TValue3d):TValue3d;
    function GsKrToWGS84(xyz:TValue3d):TValue3d;
  end;

implementation

const
  KRad:Double=pi/180;
  KDeg:Double=180/pi;


{ TGausKrugCalc }


function TGausKrugCalc.SK42ToGsKr(CK42: TValue3d): TValue3d;
var
  zn:Integer;
  Lo,lo2:Double;
  sinfi, sinb02, sinb04,sinb06:Double;

  Xa:Double;
  Xb:Double;
  Xc:Double;
  Xd:Double;

  Ya:Double;
  Yb:Double;
  Yc:Double;
begin
  zn:=ZoneNumber(ck42.lym);

  Lo := (ck42.lym*KDeg - (3 + 6 * (zn - 1))) * KRad; lo2:=lo*lo;
  sinfi:=Sin(ck42.fi); sinb02:=sinfi*sinfi; sinb04:=sinb02*sinb02; sinb06:=sinb04*sinb02;

  // SK42ToX
  Xa := Lo2 * (109500 - 574700 * sinb02 + 863700 * sinb04 - 398600 * sinb06);
  Xb := Lo2 * (278194 - 830174 * sinb02 + 572434 * sinb04 - 16010 * sinb06 + Xa);
  Xc := Lo2 * (672483.4 - 811219.9 * sinb02 + 5420 * sinb04 - 10.6 * sinb06 + Xb);
  Xd := Lo2 * (1594561.25 + 5336.535 * sinb02 + 26.79 * sinb04 + 0.149 * sinb06 + Xc);
  Result.X:= 6367558.4968 * ck42.fi - Sin(ck42.fi * 2) * (16002.89 + 66.9607 * sinb02 + 0.3515 * sinb04 - Xd);

  Ya := lo2 * (79690 - 866190 * sinb02 + 1730360 * sinb04 - 945460 * sinb06);
  Yb := lo2 * (270806 - 1523417 * sinb02 + 1327645 * sinb04 - 21701 * sinb06 + Ya);
  Yc := lo2 * (1070204.16 - 2136826.66 * sinb02 + 17.98 * sinb04 - 11.99 * sinb06 + Yb);
  Result.Y := (5 + 10 * zn) * 100000 + Lo * Cos(ck42.fi) * (6378245 + 21346.1415 * sinb02 + 107.159 * sinb04 + 0.5977 * sinb06 + Yc);

  Result.Z:=ck42.h;
end;

function TGausKrugCalc.WGS84ToSK42(flhwgs84: TValue3d): TValue3d;    // Перезсчёт из WGS84 в СК42
var
  M,N, delFi, delLym:Double;
  sinfi,sinfi2,sinlym:Double;
  cosfi,cos2fi,coslym:Double;
begin
  sinfi:=Sin(flhwgs84.fi);
  sinfi2:=sinfi*sinfi;
  sinlym:=Sin(flhwgs84.lym);
  cosfi:=Cos(flhwgs84.fi);
  cos2fi:=Cos(2*flhwgs84.fi);
  coslym:=Cos(flhwgs84.lym);

  M := a * Power((1 - e2) / (1 - e2 * sinfi2), 1.5);
  N := a * Power((1 - e2 * sinfi2), -0.5);

  delFi := ro / (M + flhwgs84.h) * (N / a * e2 * sinfi * cosfi * da  +
                              ((N*N) / (a*a) + 1) * N * sinfi * cosfi * de2 / 2  -
                              (dx * coslym + dy * sinlym) * sinfi + dz * cosfi) -
                              wx * sinlym * (1 + e2 * cos2fi) +
                              wy * coslym * (1 + e2 * cos2fi) -
                              ro * ms * e2 * sinfi * cosfi;

  dellym := ro / ((N + flhwgs84.h) * cosfi) * (-dx * sinlym + dy * coslym) + Tan(flhwgs84.fi) * (1 - e2) * (wx * coslym + wy * sinlym) - wz;

  Result.fi  :=  flhwgs84.fi - (delFi / 3600.0)*KRad;
  Result.lym :=  flhwgs84.lym - (dellym / 3600.0)*KRad;
  Result.h   :=  flhwgs84.h;
end;

function TGausKrugCalc.GsKrToCK42(xyz: TValue3d): TValue3d;  // Преобразование из проекции Гаус-Крюггер в СК42
var
  B0,delB,l,n,Betta,z0,sin2,sin4,sin6,z02:Double;
  sinbt:Double;
begin
  // На входе X,Y в метрах в проекции Гаусса-Крюгера
  // На выходе географические координаты (широта, долгота) в радианах в СК42

  Betta := xyz.X/6367558.4968;
  n := Trunc(xyz.Y*1e-6);

  sinbt:=Sin(Betta);

  //B0 - геодезическая широта точки, абсцисса которой равна абсциссе х определяемой точки, а ордината равна 0
  B0 := Betta+Sin(2.0*Betta)*(0.00252588685-0.00001491860*Power(sinbt, 2.0)+0.00000011904*Power(sinbt,4.0));

  z0 := (xyz.Y-(10.0*n+5.0)*1e5)/(6378245.0*Cos(B0));
  sin2 := Power(Sin(B0), 2);
  sin4 := sin2*sin2;
  sin6 := sin4*sin2;
  z02 := z0*z0;

  delB := (-z02)*sin(2.0*B0)*(0.251684631-0.003369263*sin2+0.000011276*sin4-
           z02*(0.10500614-0.04559916*sin2+0.00228901*sin4-0.00002987*sin6-
           z02*(0.042858-0.025318*sin2+0.014346*sin4-0.001264*sin6-
           z02*(0.01672-0.00630*sin2+0.01188*sin4-0.00328*sin6))));
  l   := z0*(1.-0.0033467108*sin2-0.0000056002*sin4-0.0000000187*sin6-
          z02*(0.16778975+0.16273586*sin2-0.00052490*sin4-0.00000846*sin6-
          z02*(0.0420025+0.1487407*sin2+0.0059420*sin4-0.0000150*sin6-
          z02*(0.01225+0.09477*sin2+0.03282*sin4-0.00034*sin6-
          z02*(0.0038+0.0524*sin2+0.0482*sin4+0.0032*sin6)))));

  Result.fi:= B0 + delB;
  Result.lym:= 6*(n-0.5)*KRad+l;
  Result.h:=xyz.Z
end;

function TGausKrugCalc.SK42ToWGS84(flhSK42: TValue3d): TValue3d;  // Перезсчёт из СК42 в WGS84 (в полярной ситсеме координат)
var
  M,N, delFi, delLym:Double;
  sinfi,sinfi2,sinlym:Double;
  cosfi,cos2fi,coslym:Double;
begin
  // latitude   - Широта   (  flhwgs84.fi      )
  // longitude  - Долгота  (  flhwgs84.lym     )
  // altitude   - Высота   (  flhwgs84.h       )

  sinfi:=Sin(flhSK42.fi); sinfi2:=sinfi*sinfi;
  sinlym:=Sin(flhSK42.lym);

  cosfi:=Cos(flhSK42.fi);
  cos2fi:=Cos(2*flhSK42.fi);
  coslym:=Cos(flhSK42.lym);

  M := a * Power((1 - e2) / (1 - e2 * sinfi2), 1.5);
  N := a * Power((1 - e2 * sinfi2), -0.5);

  delFi := ro / (M + flhSK42.h) * (N / a * e2 * sinfi * cosfi * da  +
                              ((N*N) / (a*a) + 1) * N * sinfi * cosfi * de2 / 2  -
                              (dx * coslym + dy * sinlym) * sinfi + dz * cosfi) -
                              wx * sinlym * (1 + e2 * cos2fi) +
                              wy * coslym * (1 + e2 * cos2fi) -
                              ro * ms * e2 * sinfi * cosfi;

  delLym := ro / ((N + flhSK42.h) * cosfi) * (-dx * sinlym +
                          dy * coslym) + Tan(flhSK42.fi) * (1 - e2) * (wx * coslym +
                          wy * sinlym) - wz;

  Result.fi   := flhSK42.fi + (delFi / 3600.0)*KRad;
  Result.lym  := flhSK42.lym + (delLym / 3600.0)*KRad;
  Result.h    := flhSK42.h;
end;

function TGausKrugCalc.WGS84ToGsKr(wgs84: TValue3d): TValue3d; // Перезсчёт из WGS84 в Гаусс-Крюгер
begin
  // на входе wgs84 - географические координаты (Широта, Долгота, Высота), углы в радианах
  // на выходе X,Y,Высота - прямоугольные координаты в проекции Гаусса-Крюгера в метрах

  // Для перезсчёта сначала необходимо перезсчитать wgs84 в СК42 (функция WGS84ToSK42) (т.е. перезчитать географические координаты с одного эллипсоида на другой)
  // Перезсчёт географических координат необходим т.к. разсчёт прямоугольных координат выполняется правильно, если географические координаты заданы в СК42
  // После получения географических координат в СК42 выполняется перезсчёт в прямоугольные координаты (функция SK42ToGsKr)
  Result:=SK42ToGsKr(WGS84ToSK42(wgs84));
end;

function TGausKrugCalc.GsKrToWGS84(xyz: TValue3d): TValue3d;
begin
  Result:=SK42ToWGS84(GsKrToCK42(xyz));
end;

constructor TGausKrugCalc.Create;
begin
  ro:= 206264.8062; // Число угловых секунд в радиане

  // Эллипсоид Красовского
  aP  := 6378245;                // Большая полуось
  alP := 1 / 298.3;              // Сжатие
  e2P := 2 * alP - alP * alP;    // Квадрат эксцентриситета

  // Эллипсоид WGS84 (GRS80, эти два эллипсоида сходны по большинству параметров)
  aW  := 6378137;                // Большая полуось
  alW := 1 / 298.257223563;      // Сжатие
  e2W := 2 * alW - alW * alW;    // Квадрат эксцентриситета

  // Вспомогательные значения для преобразования эллипсоидов
  a   := (aP + aW) * 0.5;
  e2  := (e2P + e2W) * 0.5;
  da  := aW - aP;
  de2 := e2W - e2P;


  // ВНИМАНИЕ! Используется ГОСТ Р51794-2008
  // Линейные элементы трансформирования, в метрах
  dx := 23.92;
  dy := -141.27;
  dz := -80.9;

  // Угловые элементы трансформирования, в секундах
  wx := 0;
  wy := -0.35;
  wz := -0.79;

  // Дифференциальное различие масштабов
  ms := -0.22*0.000001;
end;

function TGausKrugCalc.ZoneNumber(lym: Double): Integer;  // Разсчёт номера зоны для заданной долготы (долгота передаётся в РАДИАНАХ)
begin
  Result := Trunc((6 + lym*KDeg) / 6);  // Зоны изменяются через каждые 6 градусов по долготе (начиная с 1)
end;

end.

