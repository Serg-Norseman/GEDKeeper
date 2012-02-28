using System;
using System.Windows.Forms;

/// <summary>
/// Localization: unknown
/// CodeTransformation: need
/// </summary>

namespace GKSandbox
{
	public partial class frmReportsSetup : Form
	{
		private ReportProperties FRepProps;

		public frmReportsSetup(ReportProperties repProps)
		{
			this.InitializeComponent();
			base.StartPosition = FormStartPosition.CenterParent;
			this.FRepProps = repProps;
		}

		void OnShown(object sender, EventArgs e)
		{
			this.tbMgnLeft.Text = this.FRepProps.Margins.Left.ToString();
			this.tbMgnTop.Text = this.FRepProps.Margins.Top.ToString();
			this.tbMgnRight.Text = this.FRepProps.Margins.Right.ToString();
			this.tbMgnBottom.Text = this.FRepProps.Margins.Bottom.ToString();
			this.label14.BackColor = this.FRepProps.papercolor;
			this.label5.BackColor = this.FRepProps.headingbackcolor;
			this.label12.BackColor = this.FRepProps.headingforecolor;
			this.radioButton1.Checked = true;
			this.radioButton2.Checked = this.FRepProps.Landscape;
		}

		void button1_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label14.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label14.BackColor = this.colorDialog1.Color;
			}
		}

		void button5_Click(object sender, EventArgs e)
		{
			int num;
			if (int.TryParse(this.tbMgnLeft.Text, out num))
			{
				this.FRepProps.Margins.Left = num;
			}
			if (int.TryParse(this.tbMgnTop.Text, out num))
			{
				this.FRepProps.Margins.Top = num;
			}
			if (int.TryParse(this.tbMgnRight.Text, out num))
			{
				this.FRepProps.Margins.Right = num;
			}
			if (int.TryParse(this.tbMgnBottom.Text, out num))
			{
				this.FRepProps.Margins.Bottom = num;
			}
			this.FRepProps.papercolor = this.label14.BackColor;
			this.FRepProps.headingbackcolor = this.label5.BackColor;
			this.FRepProps.headingforecolor = this.label12.BackColor;
			this.FRepProps.Landscape = this.radioButton2.Checked;

			base.DialogResult = DialogResult.OK;
		}

		void button6_Click(object sender, EventArgs e)
		{
			base.DialogResult = DialogResult.Cancel;
		}

		void button2_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label5.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label5.BackColor = this.colorDialog1.Color;
			}
		}

		void button3_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label12.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label12.BackColor = this.colorDialog1.Color;
			}
		}

		void button4_Click(object sender, EventArgs e)
		{
		}

		void button7_Click(object sender, EventArgs e)
		{
			//frmTableSetup frmTableSetup = new frmTableSetup(this.doc);
			//frmTableSetup.ShowDialog(this);
		}

	}
}
