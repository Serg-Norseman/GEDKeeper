using System;
using System.Drawing;
using System.Windows.Forms;

using GKCommon;

/// <summary>
/// 
/// </summary>

namespace GKCalculatorPlugin
{
	public partial class TfmCalcWidget : Form
	{
    	private readonly Plugin fPlugin;
		private readonly ExpCalculator fCalc;

		public TfmCalcWidget(Plugin plugin) : base()
		{
			this.InitializeComponent();

			this.fPlugin = plugin;
			
			Screen scr = Screen.PrimaryScreen;
			this.Location = new Point(scr.WorkingArea.Width - this.Width - 10, scr.WorkingArea.Height - this.Height - 10);

			this.Text = this.fPlugin.LangMan.LS(PLS.LSID_MICalc);
			this.chkPutToClipboard.Text = this.fPlugin.LangMan.LS(PLS.LSID_CopyResultToClipboard);

			this.fCalc = new ExpCalculator();
			this.lbOutput.Items.Clear();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fCalc.Dispose();
			}
			base.Dispose(disposing);
		}

        private void TfmCalcWidget_Load(object sender, EventArgs e)
        {
        	this.fPlugin.Host.WidgetShow(this.fPlugin);
        }

		private void TfmCalcWidget_Closed(object sender, EventArgs e)
		{
			this.fPlugin.Host.WidgetClose(this.fPlugin);
		}

		private void edExpression_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Return)
			{
				string res;
				try
				{
					res = this.fCalc.Calc(this.edExpression.Text).ToString();
					if (this.chkPutToClipboard.Checked)
					{
						Clipboard.SetDataObject(res);
					}
				}
				catch (Exception)
				{
					res = "[ ??? ]";
				}
				this.lbOutput.Items.Add("> " + this.edExpression.Text);
				this.lbOutput.Items.Add("= " + res);
				this.lbOutput.SelectedIndex = this.lbOutput.Items.Count - 1;
				this.edCalcResult.Text = res;
			}
		}

		private void edCalcResult_MouseMove(object sender, MouseEventArgs e)
		{
			if (e.Button == MouseButtons.Left)
			{
				this.edCalcResult.DoDragDrop(this.edCalcResult.Text, DragDropEffects.Move);
			}
		}

		private void edCalcResult_DragOver(object sender, DragEventArgs e)
		{
			e.Effect = DragDropEffects.None;
		}

	}
}
