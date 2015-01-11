using System;
using System.Windows.Forms;

/// <summary>
/// 
/// </summary>

namespace GKUI.Controls
{
	public partial class GKInputBox : Form
	{
		private bool fNumbersMode;

		public string Value
		{
			get { return this.textBox1.Text; }
			set { this.textBox1.Text = value; }
		}

		public GKInputBox(string caption, string prompt, string value, bool fNumbersMode)
		{
			this.InitializeComponent();

			this.Text = caption;
			this.label1.Text = prompt;
			this.Value = value;
			this.fNumbersMode = fNumbersMode;

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


		public static bool QueryDouble(string caption, string prompt, ref double value)
		{
            bool result = false;
            value = 0.0;

            using (GKInputBox inputBox = new GKInputBox(caption, prompt, value.ToString(), true))
            {
                if (inputBox.ShowDialog() == DialogResult.OK)
                {
                    result = double.TryParse(inputBox.Value, out value);
                }
            }

            return result;
        }

		public static bool QueryInt(string caption, string prompt, ref int value)
		{
            bool result = false;
            value = 0;

            using (GKInputBox inputBox = new GKInputBox(caption, prompt, value.ToString(), true))
            {
                if (inputBox.ShowDialog() == DialogResult.OK)
                {
                    result = int.TryParse(inputBox.Value, out value);
                }
            }

            return result;
        }

		public static bool QueryText(string caption, string prompt, ref string value)
		{
            bool result = false;

            using (GKInputBox inputBox = new GKInputBox(caption, prompt, value, false))
            {
                if (inputBox.ShowDialog() == DialogResult.OK)
                {
                    value = inputBox.Value.Trim();
                    result = true;
                }
            }

            return result;
		}

	}
}
