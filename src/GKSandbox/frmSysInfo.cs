using System;
using System.Windows.Forms;

using GKSandbox;

namespace Test
{
	public partial class frmSysInfo : Form
	{
		private Cursor cur;

		public frmSysInfo()
		{
			this.InitializeComponent();
		}

		void OnShown(object sender, EventArgs e)
		{
			this.cur = this.Cursor;
			this.Cursor = Cursors.WaitCursor;
			this.richTextBox1.Text = "";

			SystemInfo SystemInfo = new SystemInfo();

			string text = "";
			text += "\n\n" + SystemInfo.GetComputerInfo();
			text += "\n\n" + SystemInfo.GetProcessorInfo();
			text += "\n\n" + SystemInfo.GetRAMInfo();
			
			this.richTextBox1.Text = text;
			this.Cursor = this.cur;
		}

	}
}
