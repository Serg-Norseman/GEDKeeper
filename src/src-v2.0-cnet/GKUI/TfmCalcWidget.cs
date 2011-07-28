using GKCore;
using GKSys;
using System;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmCalcWidget : Form
	{
		private ListBox lbOutput;
		private TextBox edExpression;
		private CheckBox chkPutToClipboard;
		private TextBox edCalcResult;
		private TCalculator calc;

		private void OnModalBegin(object Sender)
		{
			VCLUtils.PostMessage((uint)GKL.fmGEDKeeper.Handle.ToInt32(), 1135u, 0, 0);
		}

		private void InitializeComponent()
		{
			this.lbOutput = new ListBox();
			this.edExpression = new TextBox();
			this.chkPutToClipboard = new CheckBox();
			this.edCalcResult = new TextBox();
			base.SuspendLayout();
			this.lbOutput.Location = new Point(8, 8);
			this.lbOutput.Name = "lbOutput";
			this.lbOutput.Size = new Size(257, 95);
			this.lbOutput.TabIndex = 0;
			this.edExpression.Location = new Point(8, 120);
			this.edExpression.Name = "edExpression";
			this.edExpression.Size = new Size(257, 21);
			this.edExpression.TabIndex = 1;
			this.edExpression.Text = "";
			this.edExpression.KeyDown += new KeyEventHandler(this.edExpression_KeyDown);
			this.chkPutToClipboard.Location = new Point(8, 176);
			this.chkPutToClipboard.Name = "chkPutToClipboard";
			this.chkPutToClipboard.Size = new Size(257, 17);
			this.chkPutToClipboard.TabIndex = 3;
			this.chkPutToClipboard.Text = "PutToClipboard";
			this.edCalcResult.Location = new Point(8, 144);
			this.edCalcResult.Name = "edCalcResult";
			this.edCalcResult.ReadOnly = true;
			this.edCalcResult.Size = new Size(257, 21);
			this.edCalcResult.TabIndex = 2;
			this.edCalcResult.TabStop = false;
			this.edCalcResult.Text = "1979";
			this.edCalcResult.DragOver += new DragEventHandler(this.edCalcResult_DragOver);
			this.edCalcResult.MouseMove += new MouseEventHandler(this.edCalcResult_MouseMove);
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(274, 201);
			base.Controls.Add(this.lbOutput);
			base.Controls.Add(this.edExpression);
			base.Controls.Add(this.chkPutToClipboard);
			base.Controls.Add(this.edCalcResult);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedToolWindow;
			base.Name = "TfmCalcWidget";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.Manual;
			this.Text = "ExpCalc";
			base.TopMost = true;
			base.Closed += new EventHandler(this.TfmCalcWidget_Closed);
			base.ResumeLayout(false);
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
			GKL.fmGEDKeeper.miCalc.Checked = false;
			GKL.fmGEDKeeper.fmCalcWidget = null;
		}
		private void edCalcResult_DragOver(object sender, DragEventArgs e)
		{
			e.Effect = DragDropEffects.None;
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.calc.Free();
			}
			base.Dispose(Disposing);
		}

		public TfmCalcWidget()
		{
			this.InitializeComponent();
			this.calc = new TCalculator();
			this.lbOutput.Items.Clear();
			this.Text = GKL.LSList[30];
			this.chkPutToClipboard.Text = GKL.LSList[166];
		}
	}
}
