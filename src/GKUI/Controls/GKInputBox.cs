using System;
using System.Windows.Forms;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Controls
{
	public partial class GKInputBox : Form
	{
		private bool NumbersMode;

		public string Value
		{
			get { return this.textBox1.Text; }
			set { this.textBox1.Text = value; }
		}

		public GKInputBox(string Caption, string Prompt, string Value, bool NumbersMode)
		{
			this.InitializeComponent();

			this.Text = Caption;
			this.label1.Text = Prompt;
			this.Value = Value;
			this.NumbersMode = NumbersMode;

			base.AcceptButton = this.btnAccept;
			base.CancelButton = this.btnCancel;
		}

		/*void OnKeyDown(object sender, KeyEventArgs e)
		{
			if (this.mMode == 1)
			{
				this.NoneNumberEntered = false;
				if ((e.KeyCode < Keys.D0 || e.KeyCode > Keys.D9) && (e.KeyCode < Keys.NumPad0 || e.KeyCode > Keys.NumPad9) && e.KeyCode != Keys.Back && e.KeyCode != Keys.Decimal && e.KeyCode != Keys.Oemcomma && e.KeyCode != Keys.OemMinus && e.KeyCode != Keys.Subtract)
				{
					this.NoneNumberEntered = true;
				}
			}
		}

		void OnKeyPress(object sender, KeyPressEventArgs e)
		{
			if (this.mMode == 1 && this.NoneNumberEntered)
			{
				e.Handled = true;
			}
		}*/

		void BtnCancelClick(object sender, EventArgs e)
		{
			base.DialogResult = DialogResult.Cancel;
		}

		void BtnAcceptClick(object sender, EventArgs e)
		{
			base.DialogResult = DialogResult.OK;
		}



		public static bool QueryDouble(string Caption, string Prompt, ref double Value)
		{
			GKInputBox inputBox = new GKInputBox(Caption, Prompt, Value.ToString(), true);
			if (inputBox.ShowDialog() == DialogResult.OK)
			{
				return double.TryParse(inputBox.Value, out Value);
			}
			Value = 0.0;
			return false;
		}

		public static bool QueryInt(string Caption, string Prompt, ref int Value)
		{
			GKInputBox inputBox = new GKInputBox(Caption, Prompt, Value.ToString(), true);
			if (inputBox.ShowDialog() == DialogResult.OK)
			{
				return int.TryParse(inputBox.Value, out Value);
			}
			Value = 0;
			return false;
		}

		public static bool QueryText(string Caption, string Prompt, ref string Value)
		{
			GKInputBox inputBox = new GKInputBox(Caption, Prompt, Value, false);
			if (inputBox.ShowDialog() == DialogResult.OK)
			{
				Value = inputBox.Value;
				return true;
			}
			Value = "";
			return false;
		}

	}
}
