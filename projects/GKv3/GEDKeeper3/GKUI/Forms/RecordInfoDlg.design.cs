using Eto.Drawing;
using GKUI.Components;

namespace GKUI.Forms
{
	partial class RecordInfoDlg
	{
        private HyperView hyperView1;

        private void InitializeComponent()
		{
		    SuspendLayout();

            hyperView1 = new HyperView();
		    hyperView1.BorderWidth = 0;
		    hyperView1.LinkColor = Colors.Blue;
		    hyperView1.WordWrap = true;

            Content = hyperView1;

            SetPredefProperties(630, 405);
            ResumeLayout();
		}
	}
}
