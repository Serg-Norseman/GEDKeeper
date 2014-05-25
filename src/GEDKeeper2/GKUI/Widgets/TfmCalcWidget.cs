using System;
using System.Drawing;
using System.Windows.Forms;

using ExtUtils;
using GKCore;
using GKCore.Interfaces;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Widgets
{
	public partial class TfmCalcWidget : Form, IWidget
	{
    	#region IWidget common

    	private IHost fHost;
    	private MenuItem fMenuItem;

    	IHost IWidget.Host
    	{
    		get { return this.fHost; }
    	}

    	MenuItem IWidget.MenuItem
    	{
    		get { return this.fMenuItem; }
    	}

    	void IWidget.WidgetInit(IHost host, MenuItem menuItem)
    	{
    		this.fHost = host;
    		this.fMenuItem = menuItem;
    	}

        void IWidget.BaseChanged(IBase aBase) {}

    	#endregion

		private readonly ExtCalculator fCalc;

		public TfmCalcWidget() : base()
		{
			this.InitializeComponent();

			Screen scr = Screen.PrimaryScreen;
			this.Location = new Point(scr.WorkingArea.Width - this.Width - 10, scr.WorkingArea.Height - this.Height - 10);

			this.Text = LangMan.LS(LSID.LSID_MICalc);
			this.chkPutToClipboard.Text = LangMan.LS(LSID.LSID_CopyResultToClipboard);

			this.fCalc = new ExtCalculator();
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
        	this.fHost.WidgetShow(this);
        }

		private void TfmCalcWidget_Closed(object sender, EventArgs e)
		{
			this.fHost.WidgetClose(this);
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
