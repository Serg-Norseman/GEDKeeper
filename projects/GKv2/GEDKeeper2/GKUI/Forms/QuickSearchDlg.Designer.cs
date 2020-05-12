using System.Windows.Forms;

namespace GKUI.Forms
{
    partial class QuickSearchDlg
    {
        private TextBox txtSearchPattern;
        private Button btnPrev;
        private Button btnNext;

        private void InitializeComponent()
        {
            txtSearchPattern = new TextBox();
            btnPrev = new Button();
            btnNext = new Button();
            SuspendLayout();

            txtSearchPattern.Location = new System.Drawing.Point(3, 3);
            txtSearchPattern.Width = 150;
            txtSearchPattern.Height = 24;
            txtSearchPattern.Margin = new Padding(3, 3, 3, 0);
            txtSearchPattern.TextChanged += SearchPattern_TextChanged;
            txtSearchPattern.Name = "txtSearchPattern";

            btnPrev.Location = new System.Drawing.Point(156, 3);
            btnPrev.Margin = new Padding(3);
            btnPrev.Height = 24;
            btnPrev.Width = 24;
            btnPrev.Click += FindPrev_Click;
            btnPrev.Name = "btnPrev";

            btnNext.Location = new System.Drawing.Point(156 + 27, 3);
            btnNext.Margin = new Padding(3);
            btnNext.Height = 24;
            btnNext.Width = 24;
            btnNext.Click += FindNext_Click;
            btnNext.Name = "btnNext";

            AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            ClientSize = new System.Drawing.Size(210, 30);
            Controls.Add(txtSearchPattern);
            Controls.Add(btnPrev);
            Controls.Add(btnNext);
            Font = new System.Drawing.Font("Tahoma", 8.25f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 204);
            FormBorderStyle = FormBorderStyle.FixedToolWindow;
            KeyPreview = true;
            KeyDown += SearchPanel_KeyDown;
            Name = "QuickSearchDlg";
            ShowInTaskbar = false;
            StartPosition = FormStartPosition.Manual;
            TopMost = true;

            ResumeLayout(false);
        }
    }
}
