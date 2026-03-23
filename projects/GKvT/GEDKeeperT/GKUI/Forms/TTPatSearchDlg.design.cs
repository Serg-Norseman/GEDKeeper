#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TTPatSearchDlg
    {
        private Button btnPatSearch;
        private FrameView Panel3;
        private Label lblMinGenerations;
        private NumericStepper edMinGens;
        private Button btnSetPatriarch;
        private CheckBox chkWithoutDates;

        private void InitializeComponent()
        {
            chkWithoutDates = new CheckBox();
            lblMinGenerations = new Label();
            btnPatSearch = new Button();
            Panel3 = new FrameView();
            edMinGens = new NumericStepper();
            btnSetPatriarch = new Button();

            Panel3.Location = new Point(0, 0);
            Panel3.Size = new Size(122, 44);
            Panel3.TabIndex = 1;

            lblMinGenerations.Location = new Point(1, 45);
            lblMinGenerations.TabIndex = 0;

            edMinGens.Location = new Point(30, 45);
            edMinGens.Size = new Size(10, 1);
            edMinGens.TabIndex = 2;
            edMinGens.Value = 2;

            chkWithoutDates.Location = new Point(46, 45);
            chkWithoutDates.TabIndex = 5;

            btnSetPatriarch.Location = new Point(70, 45);
            btnSetPatriarch.Size = new Size(16, 1);
            btnSetPatriarch.TabIndex = 4;
            btnSetPatriarch.Clicked += btnSetPatriarch_Click;

            btnPatSearch.Location = new Point(92, 45);
            btnPatSearch.Size = new Size(16, 1);
            btnPatSearch.TabIndex = 0;
            btnPatSearch.Clicked += btnPatSearch_Click;

            Add(chkWithoutDates);
            Add(lblMinGenerations);
            Add(btnPatSearch);
            Add(Panel3);
            Add(edMinGens);
            Add(btnSetPatriarch);

            Size = new Size(124, 49);
        }
    }
}
