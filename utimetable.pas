unit UTimeTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls;

type

  { TTimetableForm }

  TTimetableForm = class(TForm)
    ComboBoxHor: TComboBox;
    ComboBoxVer: TComboBox;
    DrawGrid: TDrawGrid;
    GroupBoxDimensions: TGroupBox;
    LabelHor: TLabel;
    LabelVer: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  TimetableForm: TTimetableForm;

implementation

{$R *.lfm}

{ TTimetableForm }

procedure TTimetableForm.FormCreate(Sender: TObject);
begin

end;

end.

