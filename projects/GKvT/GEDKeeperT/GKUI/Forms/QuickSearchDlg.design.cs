#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class QuickSearchDlg
    {
        private TextField txtSearchPattern;
        private Button btnPrev;
        private Button btnNext;

        private void InitializeComponent()
        {
            txtSearchPattern = new TextField();
            btnPrev = new Button();
            btnNext = new Button();

            txtSearchPattern.Location = new Point(0, 0);
            txtSearchPattern.Size = new Size(15, 1);
            txtSearchPattern.TextChanged += SearchPattern_TextChanged;

            btnPrev.Text = "" + Application.Driver.LeftArrow;
            btnPrev.Location = new Point(16, 0);
            btnPrev.Size = new Size(3, 1);
            btnPrev.Clicked += FindPrev_Click;

            btnNext.Text = "" + Application.Driver.RightArrow;
            btnNext.Location = new Point(21, 0);
            btnNext.Size = new Size(3, 1);
            btnNext.Clicked += FindNext_Click;

            Size = new Size(28, 3);
            Add(txtSearchPattern);
            Add(btnPrev);
            Add(btnNext);
            KeyDown += SearchPanel_KeyDown;
        }
    }
}
