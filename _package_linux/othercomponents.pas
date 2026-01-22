{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit OtherComponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  icons, EditNum, ValueListEditorNum, PanelUpDown, BitFieldsEditor, OpenGLContext, GLChart, GLChartF, GLPlace3D, OGLAxisRotate, FastGLWork, FastGLWorkV2, GLShaderProg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('EditNum', @EditNum.Register);
  RegisterUnit('ValueListEditorNum', @ValueListEditorNum.Register);
  RegisterUnit('PanelUpDown', @PanelUpDown.Register);
  RegisterUnit('BitFieldsEditor', @BitFieldsEditor.Register);
  RegisterUnit('OpenGLContext', @OpenGLContext.Register);
  RegisterUnit('GLChart', @GLChart.Register);
  RegisterUnit('GLChartF', @GLChartF.Register);
  RegisterUnit('GLPlace3D', @GLPlace3D.Register);
  RegisterUnit('OGLAxisRotate', @OGLAxisRotate.Register);
  RegisterUnit('FastGLWork', @FastGLWork.Register);
  RegisterUnit('FastGLWorkV2', @FastGLWorkV2.Register);
  RegisterUnit('GLShaderProg', @GLShaderProg.Register);
end;

initialization
  RegisterPackage('OtherComponents', @Register);
end.
