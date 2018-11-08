using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class QuickSearchDlg
    {
        private TextBox txtSearchPattern;
        private Button btnPrev;
        private Button btnNext;

        private void InitializeComponent()
        {
            SuspendLayout();

            txtSearchPattern = new TextBox();
            //txtSearchPattern.Width = 150;
            //txtSearchPattern.Height = 24;
            txtSearchPattern.TextChanged += SearchPattern_TextChanged;

            btnPrev = new Button();
            btnPrev.Size = new Size(26, 26);
            btnPrev.Click += FindPrev_Click;

            btnNext = new Button();
            btnNext.Size = new Size(26, 26);
            btnNext.Click += FindNext_Click;

            Content = TableLayout.Horizontal(10, new TableCell(txtSearchPattern, true), btnPrev, btnNext);

            KeyDown += SearchPanel_KeyDown;
            Maximizable = false;
            Minimizable = false;
            Resizable = false;
            ShowInTaskbar = false;
            Topmost = true;

            UIHelper.SetPredefProperties(this, 210, 30);
            ResumeLayout();
        }
    }
}
