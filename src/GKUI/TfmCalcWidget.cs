using System;
using System.Windows.Forms;

using GKCore;
using GKSys;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKUI
{
	public partial class TfmCalcWidget : Form
	{
		private TCalculator calc;

		private void OnModalBegin(object Sender)
		{
			SysUtils.PostMessage((uint)GKUI.TfmGEDKeeper.Instance.Handle.ToInt32(), 1135u, 0, 0);
		}

		private void edExpression_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Return)
			{
				string res;
				try
				{
					res = this.calc.Calc(this.edExpression.Text).ToString();
					if (this.chkPutToClipboard.Checked)
					{
						Clipboard.SetDataObject(res);
					}
				}
				catch (Exception E)
				{
					res = "[ошибка]: " + E.Message;
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

		private void TfmCalcWidget_Closed(object sender, EventArgs e)
		{
			GKUI.TfmGEDKeeper.Instance.miCalc.Checked = false;
			GKUI.TfmGEDKeeper.Instance.fmCalcWidget = null;
		}

		private void edCalcResult_DragOver(object sender, DragEventArgs e)
		{
			e.Effect = DragDropEffects.None;
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.calc.Dispose();
			}
			base.Dispose(Disposing);
		}

		public TfmCalcWidget()
		{
			this.InitializeComponent();
			this.calc = new TCalculator();
			this.lbOutput.Items.Clear();
			this.Text = LangMan.LSList[30];
			this.chkPutToClipboard.Text = LangMan.LSList[166];
		}
	}
}
