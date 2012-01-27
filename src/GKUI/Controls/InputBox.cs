using System;
using System.Drawing;
using System.Windows.Forms;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Controls
{
	public class InputBox : Form
	{
		private Label lbl;
		private TextBox textValue;
		private Button buttonOK;
		private Button buttonCancel;

		private InputBox(string Caption, string Text)
		{
			this.lbl = new Label();
			this.textValue = new TextBox();
			this.buttonOK = new Button();
			this.buttonCancel = new Button();
			base.SuspendLayout();
			this.lbl.AutoSize = true;
			this.lbl.Location = new Point(9, 13);
			this.lbl.Name = "label";
			this.lbl.Size = new Size(31, 13);
			this.lbl.TabIndex = 1;
			this.lbl.Text = Text;
			this.textValue.Location = new Point(12, 31);
			this.textValue.Name = "textValue";
			this.textValue.Size = new Size(245, 20);
			this.textValue.TabIndex = 2;
			this.textValue.WordWrap = false;
			this.buttonOK.DialogResult = DialogResult.OK;
			this.buttonOK.Location = new Point(57, 67);
			this.buttonOK.Name = "buttonOK";
			this.buttonOK.Size = new Size(75, 23);
			this.buttonOK.TabIndex = 3;
			this.buttonOK.Text = "OK";
			this.buttonCancel.DialogResult = DialogResult.Cancel;
			this.buttonCancel.Location = new Point(138, 67);
			this.buttonCancel.Name = "buttonCancel";
			this.buttonCancel.Size = new Size(75, 23);
			this.buttonCancel.TabIndex = 4;
			this.buttonCancel.Text = "Cancel";
			base.AcceptButton = this.buttonOK;
			base.CancelButton = this.buttonCancel;
			base.ClientSize = new Size(270, 103);
			base.Controls.Add(this.buttonCancel);
			base.Controls.Add(this.buttonOK);
			base.Controls.Add(this.textValue);
			base.Controls.Add(this.lbl);
			base.FormBorderStyle = FormBorderStyle.FixedSingle;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "InputBox";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = Caption;
			base.ResumeLayout(false);
			base.PerformLayout();
		}


		public static bool Query(string Caption, string Text, ref string s_val)
		{
			InputBox ib = new InputBox(Caption, Text);
			ib.textValue.Text = s_val;
			bool Result;
			if (ib.ShowDialog() != DialogResult.OK)
			{
				Result = false;
			}
			else
			{
				s_val = ib.textValue.Text;
				Result = true;
			}
			return Result;
		}
	}
}
