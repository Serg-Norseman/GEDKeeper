using System;
using System.Windows.Forms;

using GKCore.Interfaces;

namespace GKAncestorsCirclePlugin
{
	public partial class ACOptionsDialog : Form, ILocalization
	{
		private AncestorsCircleOptions fOptions;
		private ILangMan fLangMan;
		private IPlugin fPlugin;

		public AncestorsCircleOptions Options
		{
			get { return this.fOptions; }
			set { 
				this.fOptions = value;
				this.UpdateControls();
			}
		}

		private void UpdateControls()
		{
			//this.PanDefFont.Text = this.FOptions.ChartOptions.DefFont_Name + ", " + this.FOptions.ChartOptions.DefFont_Size.ToString();

			this.acb0.BackColor = this.fOptions.BrushColor[0];
			this.acb1.BackColor = this.fOptions.BrushColor[1];
			this.acb2.BackColor = this.fOptions.BrushColor[2];
			this.acb3.BackColor = this.fOptions.BrushColor[3];
			this.acb4.BackColor = this.fOptions.BrushColor[4];
			this.acb5.BackColor = this.fOptions.BrushColor[5];
			this.acb6.BackColor = this.fOptions.BrushColor[6];
			this.acb7.BackColor = this.fOptions.BrushColor[7];
			this.acbText.BackColor = this.fOptions.BrushColor[8];
			this.acbBack.BackColor = this.fOptions.BrushColor[9];
			this.acbLine.BackColor = this.fOptions.BrushColor[10];
			this.chkShowCircLines.Checked = this.fOptions.CircularLines;
		}

		public void AcceptChanges()
		{
			this.fOptions.BrushColor[ 0] = this.acb0.BackColor;
			this.fOptions.BrushColor[ 1] = this.acb1.BackColor;
			this.fOptions.BrushColor[ 2] = this.acb2.BackColor;
			this.fOptions.BrushColor[ 3] = this.acb3.BackColor;
			this.fOptions.BrushColor[ 4] = this.acb4.BackColor;
			this.fOptions.BrushColor[ 5] = this.acb5.BackColor;
			this.fOptions.BrushColor[ 6] = this.acb6.BackColor;
			this.fOptions.BrushColor[ 7] = this.acb7.BackColor;
			this.fOptions.BrushColor[ 8] = this.acbText.BackColor;
			this.fOptions.BrushColor[ 9] = this.acbBack.BackColor;
			this.fOptions.BrushColor[10] = this.acbLine.BackColor;
			this.fOptions.CreateBrushes();
			this.fOptions.CircularLines = this.chkShowCircLines.Checked;
		}

		private void PanColor_Click(object sender, EventArgs e)
		{
            Panel pan = (sender as Panel);

            this.ColorDialog1.Color = pan.BackColor;
            if (this.ColorDialog1.ShowDialog() == DialogResult.OK)
            {
                pan.BackColor = this.ColorDialog1.Color;
            }
		}

		private void PanDefFont_Click(object sender, EventArgs e)
		{
			if (this.FontDialog1.ShowDialog() == DialogResult.OK)
			{
				//this.FOptions.ChartOptions.DefFont_Name = this.FontDialog1.Font.Name;
				//this.FOptions.ChartOptions.DefFont_Size = (int)checked((long)Math.Round((double)this.FontDialog1.Font.Size));
			}
			this.UpdateControls();
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.AcceptChanges();
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				//SysUtils.LogWrite("TfmOptions.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public ACOptionsDialog(IPlugin plugin)
		{
			this.InitializeComponent();

			this.fPlugin = plugin;
			this.fLangMan = plugin.LangMan;
			
			(this as ILocalization).SetLang();
		}

		void acbMouseClick(object sender, MouseEventArgs e)
		{
			Label lbl = sender as Label;
			this.ColorDialog1.Color = lbl.BackColor;
			if (this.ColorDialog1.ShowDialog() == DialogResult.OK) lbl.BackColor = this.ColorDialog1.Color;
		}

		void ILocalization.SetLang()
		{
			//this.btnAccept.Text = LangMan.LSList[97];
			//this.btnCancel.Text = LangMan.LSList[98];
			//this.Text = LangMan.LSList[39];

			this.SheetAncCircle.Text = fLangMan.LS(ACLS.LSID_AncestorsCircle);
			this.acb0.Text = fLangMan.LS(ACLS.LSID_Circle) + " 0";
			this.acb1.Text = fLangMan.LS(ACLS.LSID_Circle) + " 1";
			this.acb2.Text = fLangMan.LS(ACLS.LSID_Circle) + " 2";
			this.acb3.Text = fLangMan.LS(ACLS.LSID_Circle) + " 3";
			this.acb4.Text = fLangMan.LS(ACLS.LSID_Circle) + " 4";
			this.acb5.Text = fLangMan.LS(ACLS.LSID_Circle) + " 5";
			this.acb6.Text = fLangMan.LS(ACLS.LSID_Circle) + " 6";
			this.acb7.Text = fLangMan.LS(ACLS.LSID_Circle) + " 7";
			this.acbText.Text = fLangMan.LS(ACLS.LSID_TextColor);
			this.acbBack.Text = fLangMan.LS(ACLS.LSID_BackColor);
			this.acbLine.Text = fLangMan.LS(ACLS.LSID_LinesColor);
			this.chkShowCircLines.Text = fLangMan.LS(ACLS.LSID_ShowCircLines);
		}
	}
}
