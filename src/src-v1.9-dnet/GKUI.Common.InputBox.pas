unit GKUI.Common.InputBox;

interface

uses
  System.Drawing, System.Windows.Forms;

type
  InputBox = class(Form)
  private
    lbl: System.Windows.Forms.Label;
    textValue: System.Windows.Forms.TextBox;
    buttonOK: System.Windows.Forms.Button;
    buttonCancel: System.Windows.Forms.Button;
  public
    constructor Create(Caption, Text: string);

    class function Query(Caption, Text: string; var s_val: string): Boolean;
  end;

implementation

constructor InputBox.Create(Caption, Text: string);
begin
  inherited Create;

  Self.lbl := System.Windows.Forms.Label.Create;
  Self.textValue := System.Windows.Forms.TextBox.Create;
  Self.buttonOK := System.Windows.Forms.Button.Create;
  Self.buttonCancel := System.Windows.Forms.Button.Create;
  Self.SuspendLayout();
  Self.lbl.AutoSize := true;
  Self.lbl.Location := System.Drawing.Point.Create(9, 13);
  Self.lbl.Name := 'label';
  Self.lbl.Size := System.Drawing.Size.Create(31, 13);
  Self.lbl.TabIndex := 1;
  Self.lbl.Text := Text;
  Self.textValue.Location := System.Drawing.Point.Create(12, 31);
  Self.textValue.Name := 'textValue';
  Self.textValue.Size := System.Drawing.Size.Create(245, 20);
  Self.textValue.TabIndex := 2;
  Self.textValue.WordWrap := false;
  Self.buttonOK.DialogResult := System.Windows.Forms.DialogResult.OK;
  Self.buttonOK.Location := System.Drawing.Point.Create(57, 67);
  Self.buttonOK.Name := 'buttonOK';
  Self.buttonOK.Size := System.Drawing.Size.Create(75, 23);
  Self.buttonOK.TabIndex := 3;
  Self.buttonOK.Text := 'OK';
  Self.buttonCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.buttonCancel.Location := System.Drawing.Point.Create(138, 67);
  Self.buttonCancel.Name := 'buttonCancel';
  Self.buttonCancel.Size := System.Drawing.Size.Create(75, 23);
  Self.buttonCancel.TabIndex := 4;
  Self.buttonCancel.Text := 'Cancel';
  Self.AcceptButton := Self.buttonOK;
  //Self.buttonOK.UseVisualStyleBackColor := true;
  //Self.buttonCancel.UseVisualStyleBackColor := true;
  //Self.AutoScaleDimensions := System.Drawing.SizeF.Create(6F, 13F);
  //Self.AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font;
  Self.CancelButton := Self.buttonCancel;
  Self.ClientSize := System.Drawing.Size.Create(270, 103);
  Self.Controls.Add(Self.buttonCancel);
  Self.Controls.Add(Self.buttonOK);
  Self.Controls.Add(Self.textValue);
  Self.Controls.Add(Self.lbl);
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedSingle;
  Self.MaximizeBox := false;
  Self.MinimizeBox := false;
  Self.Name := 'InputBox';
  //Self.ShowIcon := false;
  Self.ShowInTaskbar := false;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := Caption;
  Self.ResumeLayout(false);
  Self.PerformLayout();
end;

class function InputBox.Query(Caption, Text: string; var s_val: string): Boolean;
var
  ib: InputBox;
begin
   ib := InputBox.Create(Caption, Text);
   ib.textValue.Text := s_val;

   if (ib.ShowDialog() <> System.Windows.Forms.DialogResult.OK)
   then Result := false
   else begin
     s_val := ib.textValue.Text;
     Result := true;
   end;
end;

{public static bool InputValue(string Caption, string Text, string prefix, string format, ref int value, int min, int max)
begin
   int val = value;

   string s_val = prefix + value.ToString(format);
   bool OKVal;
   do
   begin
      OKVal = true;
      if (!Query(Caption, Text, ref s_val)) return false;

      try
      begin
         string sTr = s_val.Trim();

         if ((sTr.Length > 0) && (sTr[0] == '#'))
         begin
               sTr = sTr.Remove(0, 1);
               val = Convert.ToInt32(sTr, 16);
         end
         else if ((sTr.Length > 1) && ((sTr[1] == 'x') && (sTr[0] == '0')))
         begin
            sTr = sTr.Remove(0, 2);
            val = Convert.ToInt32(sTr, 16);
         end
         else
            val = Convert.ToInt32(sTr, 10);
      end
      catch begin MessageBox.Show('Требуется ввести число!'); OKVal = false; end
      if ((val < min) || (val > max)) begin MessageBox.Show('Требуется число в диапазоне ' + min.ToString() + '...' + max.ToString() + ' !'); OKVal = false; end
   end while (!OKVal);
   value = val;
   return true;
end}

end.
